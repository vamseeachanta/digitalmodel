# ABOUTME: Engine-routed low-speed manoeuvring & station-keeping envelope sweep.
# ABOUTME: Reuses maneuvering_envelope physics; writes deterministic CSV+JSON.
"""Low-speed manoeuvring & station-keeping envelope sweep workflow.

Thin cfg adapter that reads ship / rudder / propulsion / environment / loading
conditions and a current-speed sweep from a YAML config, calls the EXISTING
physics in :mod:`digitalmodel.naval_architecture.maneuvering_envelope` (no new
physics here), and writes a deterministic CSV + JSON sweep under
``results/<dir>/``. Mirrors the yaw_moment / rudder_stock_torque workflow shape.

One row is emitted per (loading condition x current speed). Loading-invariant
scalars (Clarke derivatives, course-stability discriminant, rudder lift slope,
turning circle + IMO verdict) are reported in ``metadata``.
"""
from __future__ import annotations

import csv
import dataclasses
import json
import math
from dataclasses import dataclass
from importlib import resources
from pathlib import Path
from typing import Any, Iterable

import yaml

from digitalmodel.hydrodynamics.propeller_rudder import RudderGeometry, VesselPropulsion
from digitalmodel.naval_architecture import maneuvering_envelope as me

KNOT_TO_M_PER_S = me.KNOT_TO_M_PER_S
M_PER_S_TO_KNOT = me.M_PER_S_TO_KNOT

CSV_HEADERS = [
    "case_id",
    "loading_name",
    "draft_m",
    "Cb",
    "current_speed_kn",
    "current_speed_m_s",
    "threshold_speed_engine_off_kn",
    "threshold_speed_engine_on_kn",
    "current_yaw_moment_MNm",
    "rudder_yaw_authority_MNm",
    "required_rudder_angle_deg",
    "can_hold_heading",
    "utilisation",
    "critical_current_speed_kn",
    "engine_off_rudder_yaw_moment_MNm",
]

SUPPORTED_TABLE_FORMATS = {"csv", "json"}


def _finite(name: str, value: Any) -> float:
    value = float(value)
    if not math.isfinite(value):
        raise ValueError(f"{name} must be finite, got {value}")
    return value


def _positive(name: str, value: Any) -> float:
    value = _finite(name, value)
    if value <= 0:
        raise ValueError(f"{name} must be > 0, got {value}")
    return value


def _nonnegative(name: str, value: Any) -> float:
    value = _finite(name, value)
    if value < 0:
        raise ValueError(f"{name} must be >= 0, got {value}")
    return value


@dataclass(frozen=True)
class _Loading:
    name: str
    draft_m: float
    Cb: float
    rudder_effective_area_m2: float
    lateral_underwater_area_m2: float
    windage_area_m2: float
    inflow_factor_engine_off: float
    inflow_factor_engine_on: float


@dataclass(frozen=True)
class ManeuveringEnvelopeInput:
    """Validated manoeuvring-envelope sweep input."""

    case_id: str
    case_description: str
    lbp_m: float
    beam_m: float
    K_prime: float
    rudder_area_m2: float
    rudder_span_m: float
    rudder_aspect_ratio: float
    x_rudder_from_cg_m: float
    behind_hull: bool
    prop_diameter_m: float
    kt_coeffs: tuple
    thrust_deduction: float
    wake_fraction: float
    C_rp: float
    ship_speed_m_s: float
    shaft_speed_rev_s: float
    wind_speed_m_s: float
    wind_force_coeff: float
    CXYc: float
    imo_rudder_angle_deg: float
    imo_advance_m: float
    loadings: tuple[_Loading, ...]
    current_speeds_kn: tuple[float, ...]
    output_directory: str
    table_formats: tuple[str, ...]
    raw: dict[str, Any]


def validate_maneuvering_envelope_input(payload: dict[str, Any]) -> ManeuveringEnvelopeInput:
    """Validate the documented manoeuvring-envelope input contract."""

    if not isinstance(payload, dict):
        raise ValueError("maneuvering-envelope input must be a YAML mapping")
    required = {"case", "ship", "rudder", "propulsion", "environment",
                "loading_conditions", "sweep", "outputs"}
    missing = sorted(required - set(payload))
    if missing:
        raise ValueError(f"missing required top-level sections: {missing}")

    case = payload["case"]
    ship = payload["ship"]
    rudder = payload["rudder"]
    prop = payload["propulsion"]
    env = payload["environment"]
    imo = payload.get("imo", {})
    outputs = payload["outputs"]

    kt_coeffs = tuple(_finite("propulsion.kt_coeffs", c) for c in prop["kt_coeffs"])
    if not kt_coeffs:
        raise ValueError("propulsion.kt_coeffs must contain at least one coefficient")

    loadings_cfg = payload["loading_conditions"]
    if not loadings_cfg:
        raise ValueError("loading_conditions must contain at least one entry")
    loadings = tuple(
        _Loading(
            name=str(lc["name"]),
            draft_m=_positive("loading.draft_m", lc["draft_m"]),
            Cb=_positive("loading.Cb", lc["Cb"]),
            rudder_effective_area_m2=_positive(
                "loading.rudder_effective_area_m2", lc["rudder_effective_area_m2"]
            ),
            lateral_underwater_area_m2=_positive(
                "loading.lateral_underwater_area_m2", lc["lateral_underwater_area_m2"]
            ),
            windage_area_m2=_positive("loading.windage_area_m2", lc["windage_area_m2"]),
            inflow_factor_engine_off=_positive(
                "loading.inflow_factor_engine_off", lc.get("inflow_factor_engine_off", 1.0)
            ),
            inflow_factor_engine_on=_positive(
                "loading.inflow_factor_engine_on", lc.get("inflow_factor_engine_on", 1.5)
            ),
        )
        for lc in loadings_cfg
    )

    speeds = payload["sweep"]["current_speeds"]
    values = [_nonnegative("current_speed", v) for v in speeds["values"]]
    if not values:
        raise ValueError("sweep.current_speeds.values must contain at least one value")
    units = speeds.get("units")
    if units == "kn":
        current_speeds_kn = tuple(values)
    elif units == "m/s":
        current_speeds_kn = tuple(v * M_PER_S_TO_KNOT for v in values)
    else:
        raise ValueError("sweep.current_speeds.units must be 'kn' or 'm/s'")

    table_formats = tuple(outputs.get("tables", ("csv", "json")))
    unsupported = sorted(set(table_formats) - SUPPORTED_TABLE_FORMATS)
    if unsupported:
        raise ValueError(f"unsupported output table formats: {unsupported}")

    return ManeuveringEnvelopeInput(
        case_id=str(case["id"]),
        case_description=str(case.get("description", "")),
        lbp_m=_positive("ship.lbp_m", ship["lbp_m"]),
        beam_m=_positive("ship.beam_m", ship["beam_m"]),
        K_prime=_positive("ship.K_prime", ship["K_prime"]),
        rudder_area_m2=_positive("rudder.area_m2", rudder["area_m2"]),
        rudder_span_m=_positive("rudder.span_m", rudder["span_m"]),
        rudder_aspect_ratio=_positive("rudder.aspect_ratio", rudder["aspect_ratio"]),
        x_rudder_from_cg_m=_positive("rudder.x_from_cg_m", rudder["x_from_cg_m"]),
        behind_hull=bool(rudder.get("behind_hull", True)),
        prop_diameter_m=_positive("propulsion.diameter_m", prop["diameter_m"]),
        kt_coeffs=kt_coeffs,
        thrust_deduction=_finite("propulsion.thrust_deduction", prop["thrust_deduction"]),
        wake_fraction=_finite("propulsion.wake_fraction", prop["wake_fraction"]),
        C_rp=_finite("propulsion.C_rp", prop["C_rp"]),
        ship_speed_m_s=_positive("propulsion.ship_speed_m_s", prop["ship_speed_m_s"]),
        shaft_speed_rev_s=_positive("propulsion.shaft_speed_rev_s", prop["shaft_speed_rev_s"]),
        wind_speed_m_s=_nonnegative("environment.wind_speed_m_s", env["wind_speed_m_s"]),
        wind_force_coeff=_positive("environment.wind_force_coeff", env.get("wind_force_coeff", 0.8)),
        CXYc=_finite("environment.CXYc", env["CXYc"]),
        imo_rudder_angle_deg=_positive("imo.rudder_angle_deg", imo.get("rudder_angle_deg", 35.0)),
        imo_advance_m=_positive("imo.advance_m", imo.get("advance_m", 720.0)),
        loadings=loadings,
        current_speeds_kn=current_speeds_kn,
        output_directory=str(outputs.get("directory", "results/maneuvering_envelope")),
        table_formats=table_formats,
        raw=payload,
    )


def _provenance_metadata() -> dict[str, str]:
    return {
        "calculation": "low-speed manoeuvring & station-keeping envelope screen",
        "physics_source_module": "digitalmodel.naval_architecture.maneuvering_envelope",
        "composed_from": (
            "Clarke/Gedling/Hine (1983) derivatives; Nomoto (1957) R/L; "
            "Whicker & Fehlner (1958) lift slope; Soeding/Brix engine-on rudder; "
            "OCIMF (1994) current load; IMO Res. MSC.137(76) turning criteria"
        ),
        "scope_limitations": (
            "Screening-level. Not a class/IMO compliance proof and not a full "
            "MMG/6-DOF manoeuvring model. No new physics in this adapter."
        ),
    }


def run_maneuvering_envelope_sweep(config: ManeuveringEnvelopeInput) -> dict[str, Any]:
    """Run each (loading condition, current speed) combination and return rows."""

    lift_slope = me.rudder_lift_slope_per_rad(
        config.rudder_area_m2, config.rudder_span_m, config.behind_hull
    )
    ref = config.loadings[0]  # reference (typically laden) for turning circle + stability
    clarke = me.clarke_derivatives(config.lbp_m, config.beam_m, ref.draft_m, ref.Cb)
    stability_C = me.course_stability_discriminant(clarke)
    r_over_l = me.steady_turning_radius_over_L(config.K_prime, config.imo_rudder_angle_deg)
    td_over_l = me.tactical_diameter_over_L(config.K_prime, config.imo_rudder_angle_deg)
    imo_verdict = me.imo_turning_compliance(
        config.imo_advance_m, td_over_l * config.lbp_m, config.lbp_m
    )

    vessel = VesselPropulsion(
        D=config.prop_diameter_m,
        kt_coeffs=config.kt_coeffs,
        t=config.thrust_deduction,
        w=config.wake_fraction,
        C_rp=config.C_rp,
    )
    rudder = RudderGeometry(
        area=config.rudder_area_m2,
        aspect_ratio=config.rudder_aspect_ratio,
        x_R=config.x_rudder_from_cg_m,
    )

    rows: list[dict[str, Any]] = []
    for lc in config.loadings:
        for v_kn in config.current_speeds_kn:
            v_c = v_kn * KNOT_TO_M_PER_S
            u_off = me.threshold_speed_for_steerage(
                wind_speed_m_s=config.wind_speed_m_s,
                windage_area_m2=lc.windage_area_m2,
                rudder_effective_area_m2=lc.rudder_effective_area_m2,
                rudder_span_m=config.rudder_span_m,
                wind_force_coeff=config.wind_force_coeff,
                inflow_factor=lc.inflow_factor_engine_off,
                behind_hull=config.behind_hull,
            )
            u_on = me.threshold_speed_for_steerage(
                wind_speed_m_s=config.wind_speed_m_s,
                windage_area_m2=lc.windage_area_m2,
                rudder_effective_area_m2=lc.rudder_effective_area_m2,
                rudder_span_m=config.rudder_span_m,
                wind_force_coeff=config.wind_force_coeff,
                inflow_factor=lc.inflow_factor_engine_on,
                behind_hull=config.behind_hull,
            )
            n_current = me.current_yaw_moment_ocimf(
                CXYc=config.CXYc, current_speed_m_s=v_c,
                lbp_m=config.lbp_m, draft_m=lc.draft_m,
            )
            hold = me.rudder_angle_to_hold_heading(
                current_yaw_moment_Nm=n_current,
                ship_speed_m_s=config.ship_speed_m_s,
                shaft_speed_rev_s=config.shaft_speed_rev_s,
                vessel=vessel, rudder=rudder,
            )
            v_crit = me.critical_current_speed_for_heading(
                CXYc=config.CXYc, lbp_m=config.lbp_m, draft_m=lc.draft_m,
                ship_speed_m_s=config.ship_speed_m_s,
                shaft_speed_rev_s=config.shaft_speed_rev_s,
                vessel=vessel, rudder=rudder,
            )
            n_off = me.engine_off_rudder_yaw_moment(
                current_speed_m_s=v_c,
                rudder_area_m2=lc.rudder_effective_area_m2,
                rudder_span_m=config.rudder_span_m,
                rudder_angle_deg=config.imo_rudder_angle_deg,
                lever_arm_m=config.x_rudder_from_cg_m,
                behind_hull=config.behind_hull,
            )
            rows.append({
                "case_id": config.case_id,
                "loading_name": lc.name,
                "draft_m": lc.draft_m,
                "Cb": lc.Cb,
                "current_speed_kn": v_kn,
                "current_speed_m_s": v_c,
                "threshold_speed_engine_off_kn": u_off * M_PER_S_TO_KNOT,
                "threshold_speed_engine_on_kn": u_on * M_PER_S_TO_KNOT,
                "current_yaw_moment_MNm": n_current / 1e6,
                "rudder_yaw_authority_MNm": hold.rudder_yaw_authority_Nm / 1e6,
                "required_rudder_angle_deg": hold.required_rudder_angle_deg,
                "can_hold_heading": hold.can_hold_heading,
                "utilisation": hold.utilisation,
                "critical_current_speed_kn": v_crit * M_PER_S_TO_KNOT,
                "engine_off_rudder_yaw_moment_MNm": n_off / 1e6,
            })

    return {
        "metadata": {
            "case_id": config.case_id,
            "case_description": config.case_description,
            "lbp_m": config.lbp_m,
            "rudder_lift_slope_per_rad": lift_slope,
            "clarke_derivatives": clarke,
            "course_stability_discriminant": stability_C,
            "steady_turning_radius_over_L": r_over_l,
            "tactical_diameter_over_L": td_over_l,
            "imo_turning": dataclasses.asdict(imo_verdict),
            "units": {
                "current_speed_kn": "kn",
                "threshold_speed_*_kn": "kn",
                "*_MNm": "MN*m",
                "required_rudder_angle_deg": "deg (null if beyond authority)",
            },
        },
        "provenance": _provenance_metadata(),
        "rows": rows,
    }


def write_maneuvering_envelope_results(
    result: dict[str, Any],
    output_dir: str | Path,
    table_formats: Iterable[str] = ("csv", "json"),
) -> dict[str, Any]:
    """Write deterministic CSV/JSON sweep tables and return artifact paths."""

    output_path = Path(output_dir)
    output_path.mkdir(parents=True, exist_ok=True)
    table_formats = tuple(table_formats)
    manifest: dict[str, str] = {}

    if "csv" in table_formats:
        csv_path = output_path / "maneuvering_envelope_sweep.csv"
        with csv_path.open("w", newline="", encoding="utf-8") as stream:
            writer = csv.DictWriter(stream, fieldnames=CSV_HEADERS)
            writer.writeheader()
            for row in result["rows"]:
                # CSV cannot hold None cleanly -> emit empty string.
                writer.writerow({
                    h: ("" if row.get(h) is None else row[h]) for h in CSV_HEADERS
                })
        manifest["csv"] = str(csv_path)

    if "json" in table_formats:
        json_path = output_path / "maneuvering_envelope_sweep.json"
        payload = {
            "metadata": result["metadata"],
            "provenance": result["provenance"],
            "rows": result["rows"],
        }
        json_path.write_text(
            json.dumps(payload, indent=2, sort_keys=True) + "\n", encoding="utf-8"
        )
        manifest["json"] = str(json_path)

    return manifest


def load_packaged_maneuvering_envelope_yaml() -> ManeuveringEnvelopeInput:
    """Load the packaged typical-ship manoeuvring-envelope YAML."""

    resource = resources.files("digitalmodel.naval_architecture.data").joinpath(
        "maneuvering_envelope_typical_ship.yml"
    )
    with resource.open("r", encoding="utf-8") as stream:
        return validate_maneuvering_envelope_input(yaml.safe_load(stream))
