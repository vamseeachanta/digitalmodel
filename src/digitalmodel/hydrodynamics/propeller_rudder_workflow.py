# ABOUTME: Engine-routed propeller-rudder interaction force sweeps.
# ABOUTME: Reuses soding_forces / ad_flat_plate_forces; writes deterministic CSV+JSON.
"""Propeller-rudder interaction sweep workflow.

Thin cfg adapter that reads vessel/propulsion + rudder geometry + an operating
sweep (over rudder deflection ``delta_deg``, and optionally ship speed / shaft
speed) from a YAML config, calls the EXISTING propeller-rudder physics in
:mod:`digitalmodel.hydrodynamics.propeller_rudder` (no new physics here), and
writes a deterministic CSV + JSON sweep summary under ``results/<dir>/``.

Two methods are supported, mirroring the underlying module:
    - ``soding``  -> :func:`soding_forces` (Söding/Brix, primary)
    - ``actuator_disk_flat_plate`` -> :func:`ad_flat_plate_forces` (fallback)
"""

from __future__ import annotations

import csv
import json
import math
from pathlib import Path
from typing import Any, Iterable

from digitalmodel.hydrodynamics.propeller_rudder import (
    RudderForces,
    RudderGeometry,
    VesselPropulsion,
    ad_flat_plate_forces,
    soding_forces,
)

KNOT_TO_M_PER_S = 0.514444
M_PER_S_TO_KNOT = 1.0 / KNOT_TO_M_PER_S

CSV_HEADERS = [
    "case_id",
    "method",
    "ship_speed_m_s",
    "ship_speed_kn",
    "shaft_speed_rev_s",
    "rudder_angle_deg",
    "F_surge_N",
    "F_sway_N",
    "F_yaw_Nm",
]

SUPPORTED_METHODS = {"soding", "actuator_disk_flat_plate"}
SUPPORTED_TABLE_FORMATS = {"csv", "json"}

_METHOD_FUNCS = {
    "soding": soding_forces,
    "actuator_disk_flat_plate": ad_flat_plate_forces,
}


def _validate_finite(name: str, value: Any) -> float:
    value = float(value)
    if not math.isfinite(value):
        raise ValueError(f"{name} must be finite, got {value}")
    return value


def _validate_positive(name: str, value: Any) -> float:
    value = _validate_finite(name, value)
    if value <= 0:
        raise ValueError(f"{name} must be > 0, got {value}")
    return value


def _validate_nonnegative(name: str, value: Any) -> float:
    value = _validate_finite(name, value)
    if value < 0:
        raise ValueError(f"{name} must be >= 0, got {value}")
    return value


def _units_metadata() -> dict[str, str]:
    return {
        "ship_speed_m_s": "m/s",
        "ship_speed_kn": "kn",
        "shaft_speed_rev_s": "rev/s",
        "rudder_angle_deg": "deg",
        "F_surge_N": "N (positive forward)",
        "F_sway_N": "N (positive to port)",
        "F_yaw_Nm": "N*m (positive bow to port)",
    }


def _provenance_metadata() -> dict[str, str]:
    return {
        "calculation": "propeller-rudder interaction forces in ship-fixed axes",
        "physics_source_module": "digitalmodel.hydrodynamics.propeller_rudder",
        "methods": (
            "soding -> soding_forces (Soding/Brix, primary); "
            "actuator_disk_flat_plate -> ad_flat_plate_forces (fallback)"
        ),
        "references": (
            "McTaggart (2005) DRDC Atlantic TM 2005-071; Carlton (2007); "
            "Molland & Turnock (2007)"
        ),
        "scope_limitations": (
            "Preliminary screen of rudder forces in propeller slipstream; no new "
            "physics introduced by this workflow adapter."
        ),
    }


class PropellerRudderInput:
    """Validated propeller-rudder interaction sweep input."""

    def __init__(
        self,
        *,
        case_id: str,
        case_description: str,
        method: str,
        vessel: VesselPropulsion,
        rudder: RudderGeometry,
        ship_speeds_m_s: list[float],
        ship_speeds_kn: list[float],
        shaft_speeds_rev_s: list[float],
        rudder_angles_deg: list[float],
        output_directory: str,
        table_formats: tuple[str, ...],
        raw: dict[str, Any],
    ) -> None:
        self.case_id = case_id
        self.case_description = case_description
        self.method = method
        self.vessel = vessel
        self.rudder = rudder
        self.ship_speeds_m_s = ship_speeds_m_s
        self.ship_speeds_kn = ship_speeds_kn
        self.shaft_speeds_rev_s = shaft_speeds_rev_s
        self.rudder_angles_deg = rudder_angles_deg
        self.output_directory = output_directory
        self.table_formats = table_formats
        self.raw = raw


def validate_propeller_rudder_input(payload: dict[str, Any]) -> PropellerRudderInput:
    """Validate the documented propeller-rudder input contract."""

    if not isinstance(payload, dict):
        raise ValueError("propeller-rudder input must be a YAML mapping")
    required = {"case", "propulsion", "rudder", "sweep", "outputs"}
    missing = sorted(required - set(payload))
    if missing:
        raise ValueError(f"missing required top-level sections: {missing}")

    case = payload["case"]
    propulsion = payload["propulsion"]
    rudder_cfg = payload["rudder"]
    sweep = payload["sweep"]
    outputs = payload["outputs"]

    method = str(payload.get("method", "soding"))
    if method not in SUPPORTED_METHODS:
        raise ValueError(
            f"unsupported method '{method}'; expected one of {sorted(SUPPORTED_METHODS)}"
        )

    kt_coeffs = tuple(
        _validate_finite("propulsion.kt_coeffs", c)
        for c in propulsion["kt_coeffs"]
    )
    if not kt_coeffs:
        raise ValueError("propulsion.kt_coeffs must contain at least one coefficient")

    vessel = VesselPropulsion(
        D=_validate_positive("propulsion.D_m", propulsion["D_m"]),
        kt_coeffs=kt_coeffs,
        t=_validate_finite("propulsion.thrust_deduction_t", propulsion["thrust_deduction_t"]),
        w=_validate_finite("propulsion.wake_fraction_w", propulsion["wake_fraction_w"]),
        C_rp=_validate_finite(
            "propulsion.rudder_propeller_coeff_C_rp",
            propulsion["rudder_propeller_coeff_C_rp"],
        ),
        rho=_validate_positive("propulsion.rho_kg_m3", propulsion.get("rho_kg_m3", 1025.0)),
    )

    rudder = RudderGeometry(
        area=_validate_positive("rudder.area_m2", rudder_cfg["area_m2"]),
        aspect_ratio=_validate_positive(
            "rudder.aspect_ratio", rudder_cfg["aspect_ratio"]
        ),
        x_R=_validate_nonnegative("rudder.x_from_cg_m", rudder_cfg["x_from_cg_m"]),
        gamma_deg=_validate_finite("rudder.dihedral_deg", rudder_cfg.get("dihedral_deg", 0.0)),
    )

    speeds = sweep["ship_speeds"]
    speed_units = speeds.get("units")
    speed_values = [_validate_nonnegative("ship_speed", v) for v in speeds["values"]]
    if not speed_values:
        raise ValueError("sweep.ship_speeds.values must contain at least one value")
    if speed_units == "kn":
        ship_speeds_kn = speed_values
        ship_speeds_m_s = [v * KNOT_TO_M_PER_S for v in speed_values]
    elif speed_units == "m/s":
        ship_speeds_m_s = speed_values
        ship_speeds_kn = [v * M_PER_S_TO_KNOT for v in speed_values]
    else:
        raise ValueError("sweep.ship_speeds.units must be 'kn' or 'm/s'")

    shaft_speeds_rev_s = [
        _validate_nonnegative("shaft_speed_rev_s", v)
        for v in sweep["shaft_speeds_rev_s"]
    ]
    if not shaft_speeds_rev_s:
        raise ValueError("sweep.shaft_speeds_rev_s must contain at least one value")

    rudder_angles_deg = [
        _validate_finite("rudder_angle_deg", v) for v in sweep["rudder_angles_deg"]
    ]
    if not rudder_angles_deg:
        raise ValueError("sweep.rudder_angles_deg must contain at least one value")

    table_formats = tuple(outputs.get("tables", ("csv", "json")))
    unsupported = sorted(set(table_formats) - SUPPORTED_TABLE_FORMATS)
    if unsupported:
        raise ValueError(f"unsupported output table formats: {unsupported}")

    return PropellerRudderInput(
        case_id=str(case["id"]),
        case_description=str(case.get("description", "")),
        method=method,
        vessel=vessel,
        rudder=rudder,
        ship_speeds_m_s=ship_speeds_m_s,
        ship_speeds_kn=ship_speeds_kn,
        shaft_speeds_rev_s=shaft_speeds_rev_s,
        rudder_angles_deg=rudder_angles_deg,
        output_directory=str(
            outputs.get("directory", "results/propeller_rudder")
        ),
        table_formats=table_formats,
        raw=payload,
    )


def run_propeller_rudder_sweep(config: PropellerRudderInput) -> dict[str, Any]:
    """Run every (speed, shaft-speed, rudder-angle) combination and return rows."""

    force_fn = _METHOD_FUNCS[config.method]
    rows: list[dict[str, Any]] = []
    for V_s, V_kn in zip(config.ship_speeds_m_s, config.ship_speeds_kn):
        for n in config.shaft_speeds_rev_s:
            for delta_deg in config.rudder_angles_deg:
                forces: RudderForces = force_fn(
                    V_s, n, delta_deg, config.vessel, config.rudder
                )
                rows.append(
                    {
                        "case_id": config.case_id,
                        "method": forces.method,
                        "ship_speed_m_s": V_s,
                        "ship_speed_kn": V_kn,
                        "shaft_speed_rev_s": n,
                        "rudder_angle_deg": delta_deg,
                        "F_surge_N": forces.F_surge,
                        "F_sway_N": forces.F_sway,
                        "F_yaw_Nm": forces.F_yaw,
                    }
                )

    return {
        "metadata": {
            "case_id": config.case_id,
            "case_description": config.case_description,
            "method": config.method,
            "units": _units_metadata(),
        },
        "provenance": _provenance_metadata(),
        "rows": rows,
    }


def write_propeller_rudder_results(
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
        csv_path = output_path / "propeller_rudder_sweep.csv"
        with csv_path.open("w", newline="", encoding="utf-8") as stream:
            writer = csv.DictWriter(stream, fieldnames=CSV_HEADERS)
            writer.writeheader()
            for row in result["rows"]:
                writer.writerow({header: row[header] for header in CSV_HEADERS})
        manifest["csv"] = str(csv_path)

    if "json" in table_formats:
        json_path = output_path / "propeller_rudder_sweep.json"
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


def _resolve_dir(cfg: dict[str, Any], value: str | Path) -> Path:
    path = Path(value)
    if path.is_absolute():
        return path
    return Path(cfg.get("_config_dir_path", Path.cwd())) / path


class PropellerRudderWorkflow:
    """Route propeller-rudder interaction sweeps through the existing physics."""

    def router(self, cfg: dict[str, Any]) -> dict[str, Any]:
        workflow_cfg = cfg.get("propeller_rudder", {})
        config = validate_propeller_rudder_input(workflow_cfg)
        result = run_propeller_rudder_sweep(config)
        output_dir = _resolve_dir(cfg, config.output_directory)
        manifest = write_propeller_rudder_results(
            result, output_dir, table_formats=config.table_formats
        )
        cfg["propeller_rudder"] = {
            **workflow_cfg,
            "result": result,
            "artifacts": manifest,
        }
        cfg.setdefault("outputs", {})["directory"] = str(output_dir)
        return cfg
