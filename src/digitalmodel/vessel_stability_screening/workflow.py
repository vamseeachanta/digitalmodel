"""Durable workflow: intact vessel stability screening.

Wires :mod:`digitalmodel.naval_architecture.vessel_stability_screening`
into a registry workflow: hydrostatic-table lookup (draft-indexed, CSV or
inline YAML) -> loading condition with per-tank free-surface correction ->
draft/trim/GM equilibrium -> GZ curve from a GZ table or KN cross-curves ->
IMO IS Code 2008 Part A intact criteria + 46 CFR 170.170 weather criterion
-> optional lifting/crane-heel load case (46 CFR 173.005-series style) ->
optional max-KG (KG-limit) screening.

SCREENING TIER ONLY: the PE-stamped stability booklet / class or USCG
submittal governs. GHS remains the licensed cross-check; criteria
thresholds are cited config inputs (foam_system Citation pattern), with
the IMO IS Code 2008 defaults baked in and overridable.

Config schema (YAML basename ``vessel_stability_screening``)::

    basename: vessel_stability_screening
    vessel_stability_screening:
      vessel:
        name: synthetic box barge        # public/synthetic label only
        lbp_m: 100.0
        deck_edge_immersion_deg: 30.96   # optional
        downflooding_angle_deg: 35.0     # optional
      hydrostatics:
        table:                           # draft-indexed rows, or csv: path
          - {draft_m: 2.0, displacement_t: 4100.0, km_m: 17.67,
             lcb_m: 50.0, lcf_m: 50.0, mct_t_m_per_cm: 170.83,
             tpc_t_per_cm: 20.5}
        # csv: hydrostatics.csv          # same column names as above
      loading_condition:
        name: departure
        items:
          - {name: lightship, weight_t: 4000.0, vcg_m: 5.0, lcg_m: 50.0}
          - {name: fuel, weight_t: 1000.0, vcg_m: 2.0, lcg_m: 40.0,
             fsm_t_m: 410.0}             # or free_surface: {length_m,
                                         #   breadth_m, density_t_m3}
      gz:                                # optional; enables GZ criteria
        heel_deg: [0, 10, 20, 30, 40]
        kn_m: [0.0, 1.87, 3.72, 5.51, 7.18]        # at the condition
        # or gz_m: [...]                 # pre-corrected righting arms
        # or cross_curves:               # KN grid, interpolated in W
        #   displacements_t: [4100.0, 8200.0]
        #   kn_m: [[...], [...]]
      criteria:
        imo_intact:                      # optional value/citation overrides
          enabled: true
          # area_0_30_min_m_rad: 0.055 ... gm0_min_m: 0.15
          # citation: {standard: ..., edition: ..., clause: ...}
        weather_cfr_170_170:             # optional; ALL cited
          wind_pressure_t_m2: 0.055     # P per service/edition (config)
          windage_area_m2: 600.0
          windage_lever_m: 5.0
          max_heel_deg: 14.0             # rule value; deck edge caps it
          citation: {standard: "46 CFR 170.170", edition: "2024",
                     clause: "170.170(a)"}
      lifting:                           # optional crane-heel load case
        name: main crane lift
        hook_load_t: 100.0
        transverse_outreach_m: 15.0
        # or heeling_moment_t_m: 1500.0
        criteria:                        # thresholds cited (46 CFR 173)
          max_equilibrium_heel_deg: 5.0
          residual_area_ratio_min: 1.4   # optional
          residual_range_limit_deg: 40.0 # optional
          citation: {standard: "46 CFR Part 173 Subpart B", edition: "2024",
                     clause: "173.005-series"}
      max_kg:                            # optional KG-limit screening
        enabled: true
        tolerance_m: 0.0001
      output_dir: results

Outputs: loading-condition, equilibrium, GZ-curve, criteria (with citation
column), optional lifting and KG-limit CSVs plus a one-row summary CSV —
all report_pack-ready — and the same content in the returned config under
``vessel_stability_screening``; ``screening_status`` is stamped pass/fail.

Units: SI marine practice — m, t, deg, m*rad; MTC in t*m/cm; longitudinal
positions from the aft perpendicular; trim positive by the stern.
"""

from __future__ import annotations

import csv
from dataclasses import asdict
from pathlib import Path
from typing import Any

from digitalmodel.naval_architecture.vessel_stability_screening import (
    Citation,
    CraneHeelingCase,
    Equilibrium,
    GZCurve,
    HydroRow,
    HydrostaticTable,
    ImoIntactCriteria,
    LiftingCriteria,
    LiftingResult,
    LoadingCondition,
    WeatherCriterion,
    WeightItem,
    build_loading_condition,
    governing_kg_limit,
    gz_from_kn,
    imo_intact_criteria,
    kn_at_displacement,
    lifting_check,
    max_kg_screening,
    rectangular_fsm_t_m,
    solve_equilibrium,
    weather_criterion_cfr_170_170,
)

REPO_ROOT = Path(__file__).resolve().parents[3]

GOVERNANCE_NOTE = (
    "Screening tier only: ranks and gates intact loading conditions. The "
    "PE-stamped stability booklet / class or USCG submittal governs; "
    "criteria thresholds are cited config inputs and GHS remains the "
    "licensed cross-check."
)

_HYDRO_FIELDS = (
    "draft_m",
    "displacement_t",
    "km_m",
    "lcb_m",
    "lcf_m",
    "mct_t_m_per_cm",
    "tpc_t_per_cm",
)


def router(cfg: dict) -> dict:
    settings = cfg.get("vessel_stability_screening") or {}
    vessel = settings.get("vessel") or {}
    lbp_m = _opt_float(vessel, "lbp_m")
    downflooding = _opt_float(vessel, "downflooding_angle_deg")
    deck_edge = _opt_float(vessel, "deck_edge_immersion_deg")

    table = _hydrostatics(cfg, settings)
    condition = _loading_condition(settings)
    equilibrium = solve_equilibrium(condition, table, lbp_m)

    curve, kn_pairs = _gz_curve(settings, condition)

    imo_cfg = (settings.get("criteria") or {}).get("imo_intact") or {}
    imo = _imo_criteria(imo_cfg)
    weather = _weather(settings)
    lifting_case, lifting_criteria = _lifting(settings)

    criteria_results = []
    lifting_result: LiftingResult | None = None
    if curve is not None and _enabled(imo_cfg, default=True):
        criteria_results.extend(
            imo_intact_criteria(curve, equilibrium.gm_fluid_m, imo, downflooding)
        )
    if weather is not None:
        criteria_results.append(
            weather_criterion_cfr_170_170(
                weather, condition.displacement_t, equilibrium.gm_fluid_m, deck_edge
            )
        )
    if lifting_case is not None:
        if curve is None:
            raise ValueError(
                "vessel_stability_screening lifting requires gz input "
                "(gz table or KN cross-curves)"
            )
        lifting_result = lifting_check(
            curve,
            lifting_case,
            condition.displacement_t,
            lifting_criteria,
            downflooding,
            deck_edge,
        )
        criteria_results.extend(lifting_result.criteria)

    if not criteria_results:
        raise ValueError(
            "vessel_stability_screening evaluated no criteria — supply gz "
            "input and/or criteria.weather_cfr_170_170 and/or lifting"
        )

    kg_limits, governing = _max_kg(
        settings,
        condition,
        equilibrium,
        kn_pairs,
        imo,
        weather,
        lifting_case,
        lifting_criteria,
        downflooding,
        deck_edge,
    )

    status = "pass" if all(c.passed for c in criteria_results) else "fail"

    # -- rows / CSVs ---------------------------------------------------------
    condition_rows = _condition_rows(condition)
    equilibrium_row = _equilibrium_row(condition, equilibrium)
    criteria_rows = [_criterion_row(c) for c in criteria_results]
    gz_rows = _gz_rows(curve, lifting_case, condition.displacement_t)
    kg_rows = [asdict(limit) for limit in kg_limits]

    outputs: dict[str, str] = {}
    condition_csv = _output_path(cfg, settings, "loading_condition.csv")
    _write_csv(condition_csv, condition_rows)
    outputs["loading_condition_csv"] = _display_path(condition_csv)
    equilibrium_csv = _output_path(cfg, settings, "equilibrium.csv")
    _write_csv(equilibrium_csv, [equilibrium_row])
    outputs["equilibrium_csv"] = _display_path(equilibrium_csv)
    criteria_csv = _output_path(cfg, settings, "criteria.csv")
    _write_csv(criteria_csv, criteria_rows)
    outputs["criteria_csv"] = _display_path(criteria_csv)
    if gz_rows:
        gz_csv = _output_path(cfg, settings, "gz_curve.csv")
        _write_csv(gz_csv, gz_rows)
        outputs["gz_curve_csv"] = _display_path(gz_csv)
    if kg_rows:
        kg_csv = _output_path(cfg, settings, "kg_limits.csv")
        _write_csv(kg_csv, kg_rows)
        outputs["kg_limits_csv"] = _display_path(kg_csv)

    summary = {
        "method": "vessel_stability_screening_intact_v1",
        "vessel": str(vessel.get("name", "unnamed")),
        "condition": condition.name,
        "displacement_t": condition.displacement_t,
        "draft_m": equilibrium.draft_m,
        "trim_m": equilibrium.trim_m,
        "kg_m": condition.kg_m,
        "fsc_m": condition.fsc_m,
        "kg_fluid_m": condition.kg_fluid_m,
        "km_m": equilibrium.km_m,
        "gm_solid_m": equilibrium.gm_solid_m,
        "gm_fluid_m": equilibrium.gm_fluid_m,
        "criteria_evaluated": len(criteria_results),
        "criteria_failed": sum(1 for c in criteria_results if not c.passed),
        "lifting_equilibrium_heel_deg": (
            None if lifting_result is None else lifting_result.equilibrium_heel_deg
        ),
        "governing_kg_limit_m": (
            None if governing is None else governing.kg_limit_m
        ),
        "governing_kg_criterion": (None if governing is None else governing.key),
        "screening_status": status,
    }
    summary_csv = _output_path(cfg, settings, "summary.csv")
    _write_csv(summary_csv, [summary])
    outputs["summary_csv"] = _display_path(summary_csv)

    cfg["vessel_stability_screening"] = {
        **settings,
        **summary,
        "loading_condition_result": condition_rows,
        "equilibrium_result": equilibrium_row,
        "criteria_results": criteria_rows,
        "gz_curve_result": gz_rows,
        "lifting_result": None if lifting_result is None else _lifting_row(lifting_result),
        "kg_limits": kg_rows,
        **outputs,
        "governance": GOVERNANCE_NOTE,
    }
    cfg["screening_status"] = status
    return cfg


# --------------------------------------------------------------------------- #
# Input parsing
# --------------------------------------------------------------------------- #
def _hydrostatics(cfg: dict, settings: dict) -> HydrostaticTable:
    raw = settings.get("hydrostatics")
    if not isinstance(raw, dict):
        raise ValueError("vessel_stability_screening hydrostatics mapping is required")
    rows_raw: list[dict[str, Any]]
    if raw.get("csv"):
        csv_path = Path(str(raw["csv"]))
        if not csv_path.is_absolute():
            csv_path = _config_dir(cfg) / csv_path
        with csv_path.open(newline="") as stream:
            rows_raw = list(csv.DictReader(stream))
        if not rows_raw:
            raise ValueError(f"hydrostatics csv {csv_path} is empty")
    else:
        rows_raw = raw.get("table") or []
        if not isinstance(rows_raw, list) or not rows_raw:
            raise ValueError(
                "vessel_stability_screening hydrostatics needs table rows or csv"
            )
    rows = []
    for i, item in enumerate(rows_raw):
        unknown = set(item) - set(_HYDRO_FIELDS)
        if unknown and not raw.get("csv"):
            raise ValueError(
                f"hydrostatics.table[{i}] unknown fields: {sorted(unknown)}"
            )
        kwargs: dict[str, float] = {}
        for name in _HYDRO_FIELDS:
            value = item.get(name)
            if value is None or value == "":
                continue
            kwargs[name] = float(value)
        for name in ("draft_m", "displacement_t", "km_m"):
            if name not in kwargs:
                raise ValueError(f"hydrostatics row {i}: {name} is required")
        rows.append(HydroRow(**kwargs))
    return HydrostaticTable(rows=tuple(rows))


def _loading_condition(settings: dict) -> LoadingCondition:
    raw = settings.get("loading_condition")
    if not isinstance(raw, dict):
        raise ValueError(
            "vessel_stability_screening loading_condition mapping is required"
        )
    items_raw = raw.get("items")
    if not isinstance(items_raw, list) or not items_raw:
        raise ValueError(
            "vessel_stability_screening loading_condition.items must be a "
            "non-empty list"
        )
    items = []
    for i, item in enumerate(items_raw):
        label = f"loading_condition.items[{i}]"
        fsm = float(item.get("fsm_t_m", 0.0))
        fs = item.get("free_surface")
        if fs is not None:
            if fsm:
                raise ValueError(f"{label}: give fsm_t_m or free_surface, not both")
            fsm = rectangular_fsm_t_m(
                length_m=_req_float(fs, "length_m", f"{label}.free_surface.length_m"),
                breadth_m=_req_float(
                    fs, "breadth_m", f"{label}.free_surface.breadth_m"
                ),
                density_t_m3=_req_float(
                    fs, "density_t_m3", f"{label}.free_surface.density_t_m3"
                ),
            )
        items.append(
            WeightItem(
                name=str(item.get("name", f"item_{i + 1}")),
                weight_t=_req_float(item, "weight_t", f"{label}.weight_t"),
                vcg_m=_req_float(item, "vcg_m", f"{label}.vcg_m"),
                lcg_m=_req_float(item, "lcg_m", f"{label}.lcg_m"),
                fsm_t_m=fsm,
            )
        )
    return build_loading_condition(str(raw.get("name", "condition")), items)


def _gz_curve(
    settings: dict, condition: LoadingCondition
) -> tuple[GZCurve | None, tuple[list[float], list[float]] | None]:
    """Returns (curve, (heel_deg, kn_m)) — KN pairs only when KN was input
    (max-KG screening needs KN, not a pre-corrected GZ table)."""
    raw = settings.get("gz")
    if raw is None:
        return None, None
    if not isinstance(raw, dict):
        raise ValueError("vessel_stability_screening gz must be a mapping")
    heels = [float(v) for v in raw.get("heel_deg") or []]
    if not heels:
        raise ValueError("vessel_stability_screening gz.heel_deg is required")
    sources = [k for k in ("kn_m", "gz_m", "cross_curves") if raw.get(k) is not None]
    if len(sources) != 1:
        raise ValueError(
            "vessel_stability_screening gz needs exactly one of kn_m, gz_m "
            "or cross_curves"
        )
    if raw.get("gz_m") is not None:
        gz = [float(v) for v in raw["gz_m"]]
        return GZCurve(heel_deg=tuple(heels), gz_m=tuple(gz)), None
    if raw.get("kn_m") is not None:
        kn = [float(v) for v in raw["kn_m"]]
    else:
        grid = raw["cross_curves"]
        kn = kn_at_displacement(
            heels,
            [float(v) for v in grid.get("displacements_t") or []],
            [[float(v) for v in row] for row in grid.get("kn_m") or []],
            condition.displacement_t,
        )
    return gz_from_kn(heels, kn, condition.kg_fluid_m), (heels, kn)


def _enabled(raw: dict, default: bool) -> bool:
    return bool(raw.get("enabled", default))


def _imo_criteria(raw: dict) -> ImoIntactCriteria:
    kwargs: dict[str, Any] = {}
    for name in (
        "area_0_30_min_m_rad",
        "area_0_40_min_m_rad",
        "area_30_40_min_m_rad",
        "gz_30_min_m",
        "angle_max_gz_min_deg",
        "gm0_min_m",
    ):
        if raw.get(name) is not None:
            kwargs[name] = float(raw[name])
    if raw.get("citation") is not None:
        kwargs["citation"] = _citation(raw.get("citation"), "criteria.imo_intact")
    return ImoIntactCriteria(**kwargs)


def _weather(settings: dict) -> WeatherCriterion | None:
    raw = (settings.get("criteria") or {}).get("weather_cfr_170_170")
    if raw is None:
        return None
    if not isinstance(raw, dict):
        raise ValueError(
            "vessel_stability_screening criteria.weather_cfr_170_170 must be "
            "a mapping"
        )
    label = "criteria.weather_cfr_170_170"
    return WeatherCriterion(
        wind_pressure_t_m2=_req_float(
            raw, "wind_pressure_t_m2", f"{label}.wind_pressure_t_m2"
        ),
        windage_area_m2=_req_float(raw, "windage_area_m2", f"{label}.windage_area_m2"),
        windage_lever_m=_req_float(
            raw, "windage_lever_m", f"{label}.windage_lever_m"
        ),
        max_heel_deg=float(raw.get("max_heel_deg", 14.0)),
        citation=_citation(raw.get("citation"), label),
    )


def _lifting(settings: dict) -> tuple[CraneHeelingCase | None, LiftingCriteria | None]:
    raw = settings.get("lifting")
    if raw is None:
        return None, None
    if not isinstance(raw, dict):
        raise ValueError("vessel_stability_screening lifting must be a mapping")
    name = str(raw.get("name", "lift"))
    if raw.get("heeling_moment_t_m") is not None:
        case = CraneHeelingCase(
            name=name, heeling_moment_t_m=float(raw["heeling_moment_t_m"])
        )
    else:
        case = CraneHeelingCase.from_hook_load(
            name,
            _req_float(raw, "hook_load_t", "lifting.hook_load_t"),
            _req_float(
                raw, "transverse_outreach_m", "lifting.transverse_outreach_m"
            ),
        )
    crit_raw = raw.get("criteria")
    if not isinstance(crit_raw, dict):
        raise ValueError(
            "vessel_stability_screening lifting.criteria mapping is required "
            "(cited thresholds)"
        )
    ratio = crit_raw.get("residual_area_ratio_min")
    criteria = LiftingCriteria(
        max_equilibrium_heel_deg=_req_float(
            crit_raw,
            "max_equilibrium_heel_deg",
            "lifting.criteria.max_equilibrium_heel_deg",
        ),
        residual_area_ratio_min=None if ratio is None else float(ratio),
        residual_range_limit_deg=float(
            crit_raw.get("residual_range_limit_deg", 40.0)
        ),
        citation=_citation(crit_raw.get("citation"), "lifting.criteria"),
    )
    return case, criteria


def _max_kg(
    settings: dict,
    condition: LoadingCondition,
    equilibrium: Equilibrium,
    kn_pairs: tuple[list[float], list[float]] | None,
    imo: ImoIntactCriteria,
    weather: WeatherCriterion | None,
    lifting_case: CraneHeelingCase | None,
    lifting_criteria: LiftingCriteria | None,
    downflooding: float | None,
    deck_edge: float | None,
):
    raw = settings.get("max_kg")
    if raw is None or not _enabled(raw if isinstance(raw, dict) else {}, default=True):
        return [], None
    if not isinstance(raw, dict):
        raise ValueError("vessel_stability_screening max_kg must be a mapping")
    if kn_pairs is None:
        raise ValueError(
            "vessel_stability_screening max_kg requires KN input (gz.kn_m or "
            "gz.cross_curves) — a pre-corrected gz_m table cannot be re-based"
        )
    heels, kn = kn_pairs
    limits = max_kg_screening(
        heels,
        kn,
        equilibrium.km_m,
        condition.displacement_t,
        imo=imo,
        weather=weather,
        lifting_case=lifting_case,
        lifting=lifting_criteria,
        downflooding_angle_deg=downflooding,
        deck_edge_immersion_deg=deck_edge,
        tolerance_m=float(raw.get("tolerance_m", 1e-4)),
    )
    return limits, governing_kg_limit(limits)


def _citation(item: Any, label: str) -> Citation:
    if not isinstance(item, dict):
        raise ValueError(
            f"vessel_stability_screening {label}.citation is required "
            "(mapping with standard, edition, clause)"
        )
    try:
        return Citation(
            standard=str(item.get("standard", "")),
            edition=str(item.get("edition", "")),
            clause=str(item.get("clause", "")),
            note=str(item.get("note", "")),
        )
    except ValueError as exc:
        raise ValueError(f"vessel_stability_screening {label}.{exc}") from exc


# --------------------------------------------------------------------------- #
# Output rows
# --------------------------------------------------------------------------- #
def _condition_rows(condition: LoadingCondition) -> list[dict[str, Any]]:
    rows = [
        {
            "name": item.name,
            "weight_t": item.weight_t,
            "vcg_m": item.vcg_m,
            "lcg_m": item.lcg_m,
            "fsm_t_m": item.fsm_t_m,
        }
        for item in condition.items
    ]
    rows.append(
        {
            "name": f"TOTAL ({condition.name})",
            "weight_t": condition.displacement_t,
            "vcg_m": condition.kg_m,
            "lcg_m": condition.lcg_m,
            "fsm_t_m": condition.fsm_total_t_m,
        }
    )
    return rows


def _equilibrium_row(
    condition: LoadingCondition, equilibrium: Equilibrium
) -> dict[str, Any]:
    return {
        "condition": condition.name,
        "displacement_t": condition.displacement_t,
        "draft_m": equilibrium.draft_m,
        "km_m": equilibrium.km_m,
        "kg_m": condition.kg_m,
        "fsc_m": condition.fsc_m,
        "kg_fluid_m": condition.kg_fluid_m,
        "gm_solid_m": equilibrium.gm_solid_m,
        "gm_fluid_m": equilibrium.gm_fluid_m,
        "lcg_m": condition.lcg_m,
        "lcb_m": equilibrium.lcb_m,
        "lcf_m": equilibrium.lcf_m,
        "mct_t_m_per_cm": equilibrium.mct_t_m_per_cm,
        "trim_m": equilibrium.trim_m,
        "draft_aft_m": equilibrium.draft_aft_m,
        "draft_fwd_m": equilibrium.draft_fwd_m,
    }


def _criterion_row(criterion) -> dict[str, Any]:
    return {
        "key": criterion.key,
        "description": criterion.description,
        "value": criterion.value,
        "required": criterion.required,
        "unit": criterion.unit,
        "comparison": criterion.comparison,
        "margin": criterion.margin,
        "status": "pass" if criterion.passed else "fail",
        "citation": criterion.citation,
    }


def _gz_rows(
    curve: GZCurve | None,
    lifting_case: CraneHeelingCase | None,
    displacement_t: float,
) -> list[dict[str, Any]]:
    if curve is None:
        return []
    rows = []
    for phi, gz in zip(curve.heel_deg, curve.gz_m):
        row: dict[str, Any] = {"heel_deg": phi, "gz_m": gz}
        if lifting_case is not None:
            ha = lifting_case.heeling_arm_m(displacement_t, phi)
            row["heeling_arm_m"] = ha
            row["net_arm_m"] = gz - ha
        rows.append(row)
    return rows


def _lifting_row(result: LiftingResult) -> dict[str, Any]:
    return {
        "case_name": result.case_name,
        "heeling_moment_t_m": result.heeling_moment_t_m,
        "heeling_arm_0_m": result.heeling_arm_0_m,
        "equilibrium_heel_deg": result.equilibrium_heel_deg,
        "residual_range_to_deg": result.residual_range_to_deg,
        "residual_area_m_rad": result.residual_area_m_rad,
        "heeling_area_m_rad": result.heeling_area_m_rad,
        "residual_area_ratio": result.residual_area_ratio,
        "status": "pass" if result.passed else "fail",
    }


# --------------------------------------------------------------------------- #
# Shared plumbing (sibling-module pattern)
# --------------------------------------------------------------------------- #
def _req_float(source: dict, name: str, label: str) -> float:
    value = source.get(name)
    if value is None:
        raise ValueError(f"vessel_stability_screening {label} is required")
    return float(value)


def _opt_float(source: dict, name: str) -> float | None:
    value = source.get(name)
    return None if value is None else float(value)


def _output_path(cfg: dict, settings: dict, suffix: str) -> Path:
    output_dir = Path(settings.get("output_dir", "results"))
    if not output_dir.is_absolute():
        output_dir = _config_dir(cfg) / output_dir
    return output_dir / f"{_input_stem(cfg)}_{suffix}"


def _config_dir(cfg: dict) -> Path:
    if cfg.get("_config_dir_path"):
        return Path(cfg["_config_dir_path"])
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).parent
    return Path.cwd()


def _input_stem(cfg: dict) -> str:
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).stem
    return str(cfg.get("basename", "vessel_stability_screening"))


def _write_csv(path: Path, rows: list[dict[str, Any]]) -> None:
    if not rows:
        raise ValueError("vessel_stability_screening cannot write empty results")
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=list(rows[0].keys()))
        writer.writeheader()
        writer.writerows(rows)


def _display_path(path: Path) -> str:
    resolved = path.resolve()
    try:
        return str(resolved.relative_to(REPO_ROOT.resolve()))
    except ValueError:
        return str(resolved)
