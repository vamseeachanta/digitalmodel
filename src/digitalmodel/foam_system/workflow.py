"""Durable workflow: foam/fire-suppression system sizing screening.

Wires ``digitalmodel.foam_system.sizing`` into a registry workflow:
protected-area foam demand (criteria table cited to standard/edition/
clause) -> concentrate quantity with reserve policy -> proportioner duty
check -> tree-network hydraulic screening (Hazen-Williams or
Darcy-Weisbach) with pump head margin.

SCREENING calc only — final designs are governed by the authority having
jurisdiction / classification society review (see the module docstring in
``sizing.py`` and ``docs/domains/fire_safety/foam-system-sizing.md``).

Config schema (YAML basename ``foam_system_sizing``)::

    basename: foam_system_sizing
    foam_system_sizing:
      criteria:                      # application-rate table, all cited
        deck_foam_monitor:
          application_rate_lpm_per_m2: 6.5
          discharge_time_min: 15.0
          citation:                  # ALL of standard/edition/clause required
            standard: "NFPA 11"
            edition: "2021"
            clause: "example clause — verify against the purchased edition"
            note: "optional free text"
      protected_areas:
        - {name: helideck, area_m2: 300.0, criterion: deck_foam_monitor}
      demand_policy: max             # max (one fire at a time) | sum
      hose_streams:                  # optional supplementary allowance
        - {name: foam hose stations, flow_lpm: 400.0, count: 2,
           duration_min: 20.0,
           citation: {standard: ..., edition: ..., clause: ...}}
      concentrate:
        concentration_percent: 3.0
        reserve_percent: 100.0       # reserve policy from the governing code
      proportioner:                  # optional duty check (vendor data)
        min_solution_flow_lpm: 500.0
        max_solution_flow_lpm: 4000.0
        max_concentrate_flow_lpm: 150.0     # optional
        rated_concentration_percent: 3.0    # optional
      hydraulics:                    # optional tree-network screening
        method: hazen_williams       # hazen_williams | darcy_weisbach
        hazen_williams_c: 120.0
        roughness_mm: 0.045          # darcy_weisbach only
        kinematic_viscosity_m2_s: 1.0e-6
        density_kg_m3: 1000.0
        pump: {node: PUMP, rated_flow_lpm: 3600.0, rated_head_m: 120.0,
               elevation_m: 0.0}
        runs:
          - {from: PUMP, to: J1, length_m: 100.0, diameter_mm: 200.0,
             equivalent_length_m: 0.0}
          - {from: J1, to: HELIDECK, length_m: 50.0, diameter_mm: 150.0}
        terminals:                   # the discharge scenario to screen
          - {node: HELIDECK, flow_lpm: 1950.0, required_pressure_bar: 7.0,
             elevation_m: 25.0}
      output_dir: results

Units: SI (m2, L/min, min, m, mm, bar).
"""

from __future__ import annotations

import csv
from pathlib import Path
from typing import Any

from digitalmodel.foam_system.sizing import (
    Citation,
    FoamCriterion,
    HoseStream,
    HydraulicsResult,
    PipeRun,
    Proportioner,
    ProtectedArea,
    Pump,
    Terminal,
    concentrate_quantity,
    foam_demand,
    proportioner_check,
    solve_tree_network,
)

REPO_ROOT = Path(__file__).resolve().parents[3]

SCREENING_NOTICE = (
    "Screening calculation only. Application rates, discharge times and "
    "reserve policy are config-supplied and cited to the standard edition "
    "selected by the user; final foam-system designs are governed by the "
    "authority having jurisdiction / classification society review."
)


def router(cfg: dict) -> dict:
    settings = cfg.get("foam_system_sizing") or {}

    criteria = _criteria(settings)
    areas = [
        _protected_area(item, i, criteria)
        for i, item in enumerate(_req_list(settings, "protected_areas"))
    ]
    hoses = [_hose_stream(item, i) for i, item in enumerate(settings.get("hose_streams") or [])]
    policy = str(settings.get("demand_policy", "max")).strip().lower()

    demand = foam_demand(areas, hoses, policy)

    conc_cfg = settings.get("concentrate") or {}
    concentrate = concentrate_quantity(
        demand,
        concentration_percent=_req_float(conc_cfg, "concentration_percent", "concentrate.concentration_percent"),
        reserve_percent=float(conc_cfg.get("reserve_percent", 0.0)),
        hose_streams=hoses,
    )

    prop_result = None
    prop_cfg = settings.get("proportioner")
    if prop_cfg:
        proportioner = Proportioner(
            min_solution_flow_lpm=float(prop_cfg.get("min_solution_flow_lpm", 0.0)),
            max_solution_flow_lpm=_req_float(
                prop_cfg, "max_solution_flow_lpm", "proportioner.max_solution_flow_lpm"
            ),
            max_concentrate_flow_lpm=_opt_float(prop_cfg, "max_concentrate_flow_lpm"),
            rated_concentration_percent=_opt_float(prop_cfg, "rated_concentration_percent"),
        )
        prop_result = proportioner_check(
            proportioner,
            demand.design_solution_flow_lpm,
            concentrate.injection_rate_lpm,
            concentrate.concentration_percent,
        )

    hyd_result: HydraulicsResult | None = None
    hyd_cfg = settings.get("hydraulics")
    if hyd_cfg:
        hyd_result = _hydraulics(hyd_cfg)

    # -- outputs -------------------------------------------------------------
    demand_rows = [
        {
            "name": area.name,
            "area_m2": area.area_m2,
            "criterion": area.criterion.key,
            "application_rate_lpm_per_m2": area.criterion.application_rate_lpm_per_m2,
            "discharge_time_min": area.criterion.discharge_time_min,
            "solution_flow_lpm": area.solution_flow_lpm,
            "solution_volume_l": area.solution_volume_l,
            "citation": area.criterion.citation.label(),
        }
        for area in areas
    ]
    for hose in hoses:
        demand_rows.append(
            {
                "name": hose.name,
                "area_m2": "",
                "criterion": "hose_stream",
                "application_rate_lpm_per_m2": "",
                "discharge_time_min": hose.duration_min,
                "solution_flow_lpm": hose.total_flow_lpm,
                "solution_volume_l": hose.total_flow_lpm * hose.duration_min,
                "citation": hose.citation.label() if hose.citation else "",
            }
        )

    outputs: dict[str, str] = {}
    demand_csv = _output_path(cfg, settings, "foam_demand.csv")
    _write_csv(demand_csv, demand_rows)
    outputs["foam_demand_csv"] = _display_path(demand_csv)

    hydraulics_summary: dict[str, Any] | None = None
    if hyd_result is not None:
        run_rows = [
            {
                "from_node": r.from_node,
                "to_node": r.to_node,
                "flow_lpm": r.flow_lpm,
                "velocity_m_s": r.velocity_m_s,
                "headloss_m": r.headloss_m,
            }
            for r in hyd_result.runs
        ]
        terminal_rows = [
            {
                "terminal": t.node,
                "flow_lpm": t.flow_lpm,
                "path": " -> ".join(t.path),
                "friction_headloss_m": t.friction_headloss_m,
                "static_head_m": t.static_head_m,
                "pressure_head_m": t.pressure_head_m,
                "required_pump_head_m": t.required_pump_head_m,
            }
            for t in hyd_result.terminals
        ]
        runs_csv = _output_path(cfg, settings, "foam_hydraulic_runs.csv")
        terminals_csv = _output_path(cfg, settings, "foam_hydraulic_terminals.csv")
        _write_csv(runs_csv, run_rows)
        _write_csv(terminals_csv, terminal_rows)
        outputs["hydraulic_runs_csv"] = _display_path(runs_csv)
        outputs["hydraulic_terminals_csv"] = _display_path(terminals_csv)
        hydraulics_summary = {
            "method": hyd_result.method,
            "total_flow_lpm": hyd_result.total_flow_lpm,
            "governing_terminal": hyd_result.governing_terminal,
            "required_pump_head_m": hyd_result.required_pump_head_m,
            "pump_head_margin_m": hyd_result.pump_head_margin_m,
            "pump_head_margin_percent": hyd_result.pump_head_margin_percent,
            "pump_flow_margin_lpm": hyd_result.pump_flow_margin_lpm,
            "pump_head_ok": hyd_result.pump_head_ok,
            "pump_flow_ok": hyd_result.pump_flow_ok,
            "max_velocity_m_s": hyd_result.max_velocity_m_s,
        }

    cfg["foam_system_sizing"] = {
        **settings,
        "method": "foam_system_sizing_screening_v1",
        "screening_notice": SCREENING_NOTICE,
        "demand": {
            "demand_policy": demand.demand_policy,
            "governing_area": demand.governing_area,
            "governing_area_flow_lpm": demand.governing_area_flow_lpm,
            "hose_flow_lpm": demand.hose_flow_lpm,
            "design_solution_flow_lpm": demand.design_solution_flow_lpm,
            "per_area": demand_rows,
        },
        "concentrate_result": {
            "concentration_percent": concentrate.concentration_percent,
            "reserve_percent": concentrate.reserve_percent,
            "area_concentrate_l": concentrate.area_concentrate_l,
            "hose_concentrate_l": concentrate.hose_concentrate_l,
            "base_concentrate_l": concentrate.base_concentrate_l,
            "reserve_concentrate_l": concentrate.reserve_concentrate_l,
            "total_concentrate_l": concentrate.total_concentrate_l,
            "injection_rate_lpm": concentrate.injection_rate_lpm,
        },
        "proportioner_result": None
        if prop_result is None
        else {
            "design_solution_flow_lpm": prop_result.design_solution_flow_lpm,
            "injection_rate_lpm": prop_result.injection_rate_lpm,
            "utilization_percent": prop_result.utilization_percent,
            "solution_flow_ok": prop_result.solution_flow_ok,
            "concentrate_flow_ok": prop_result.concentrate_flow_ok,
            "concentration_match_ok": prop_result.concentration_match_ok,
            "ok": prop_result.ok,
            "messages": list(prop_result.messages),
        },
        "hydraulics_result": hydraulics_summary,
        **outputs,
    }
    return cfg


# -- config parsing ----------------------------------------------------------


def _criteria(settings: dict) -> dict[str, FoamCriterion]:
    table = settings.get("criteria")
    if not isinstance(table, dict) or not table:
        raise ValueError(
            "foam_system_sizing criteria must be a non-empty mapping of "
            "criterion key -> {application_rate_lpm_per_m2, discharge_time_min, citation}"
        )
    criteria: dict[str, FoamCriterion] = {}
    for key, item in table.items():
        if not isinstance(item, dict):
            raise ValueError(f"foam_system_sizing criteria['{key}'] must be a mapping")
        criteria[key] = FoamCriterion(
            key=str(key),
            application_rate_lpm_per_m2=_req_float(
                item, "application_rate_lpm_per_m2",
                f"criteria['{key}'].application_rate_lpm_per_m2",
            ),
            discharge_time_min=_req_float(
                item, "discharge_time_min", f"criteria['{key}'].discharge_time_min"
            ),
            citation=_citation(item.get("citation"), f"criteria['{key}']"),
        )
    return criteria


def _citation(item: Any, label: str) -> Citation:
    if not isinstance(item, dict):
        raise ValueError(
            f"foam_system_sizing {label}.citation is required "
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
        raise ValueError(f"foam_system_sizing {label}.{exc}") from exc


def _protected_area(
    item: dict[str, Any], index: int, criteria: dict[str, FoamCriterion]
) -> ProtectedArea:
    label = f"protected_areas[{index}]"
    key = str(item.get("criterion", ""))
    if key not in criteria:
        raise ValueError(
            f"foam_system_sizing {label}.criterion '{key}' not found in criteria table "
            f"(known: {sorted(criteria)})"
        )
    return ProtectedArea(
        name=str(item.get("name", f"area_{index + 1}")),
        area_m2=_req_float(item, "area_m2", f"{label}.area_m2"),
        criterion=criteria[key],
    )


def _hose_stream(item: dict[str, Any], index: int) -> HoseStream:
    label = f"hose_streams[{index}]"
    citation = None
    if item.get("citation") is not None:
        citation = _citation(item.get("citation"), label)
    return HoseStream(
        name=str(item.get("name", f"hose_{index + 1}")),
        flow_lpm=_req_float(item, "flow_lpm", f"{label}.flow_lpm"),
        count=int(item.get("count", 1)),
        duration_min=float(item.get("duration_min", 0.0)),
        citation=citation,
    )


def _hydraulics(hyd_cfg: dict[str, Any]) -> HydraulicsResult:
    pump_cfg = hyd_cfg.get("pump")
    if not isinstance(pump_cfg, dict):
        raise ValueError("foam_system_sizing hydraulics.pump is required")
    pump = Pump(
        node=str(pump_cfg.get("node", "PUMP")),
        rated_flow_lpm=_req_float(pump_cfg, "rated_flow_lpm", "hydraulics.pump.rated_flow_lpm"),
        rated_head_m=_req_float(pump_cfg, "rated_head_m", "hydraulics.pump.rated_head_m"),
        elevation_m=float(pump_cfg.get("elevation_m", 0.0)),
    )
    runs = [
        PipeRun(
            from_node=str(item.get("from", "")),
            to_node=str(item.get("to", "")),
            length_m=_req_float(item, "length_m", f"hydraulics.runs[{i}].length_m"),
            diameter_mm=_req_float(item, "diameter_mm", f"hydraulics.runs[{i}].diameter_mm"),
            equivalent_length_m=float(item.get("equivalent_length_m", 0.0)),
        )
        for i, item in enumerate(_req_list(hyd_cfg, "runs", "hydraulics.runs"))
    ]
    terminals = [
        Terminal(
            node=str(item.get("node", "")),
            flow_lpm=_req_float(item, "flow_lpm", f"hydraulics.terminals[{i}].flow_lpm"),
            required_pressure_bar=_req_float(
                item, "required_pressure_bar",
                f"hydraulics.terminals[{i}].required_pressure_bar",
            ),
            elevation_m=float(item.get("elevation_m", 0.0)),
        )
        for i, item in enumerate(_req_list(hyd_cfg, "terminals", "hydraulics.terminals"))
    ]
    return solve_tree_network(
        pump=pump,
        runs=runs,
        terminals=terminals,
        method=str(hyd_cfg.get("method", "hazen_williams")).strip().lower(),
        hazen_williams_c=float(hyd_cfg.get("hazen_williams_c", 120.0)),
        roughness_mm=float(hyd_cfg.get("roughness_mm", 0.045)),
        kinematic_viscosity_m2_s=float(hyd_cfg.get("kinematic_viscosity_m2_s", 1.0e-6)),
        density_kg_m3=float(hyd_cfg.get("density_kg_m3", 1000.0)),
    )


def _req_list(settings: dict, name: str, label: str | None = None) -> list:
    value = settings.get(name)
    if not isinstance(value, list) or not value:
        raise ValueError(
            f"foam_system_sizing {label or name} must be a non-empty list"
        )
    return value


def _req_float(source: dict, name: str, label: str) -> float:
    value = source.get(name)
    if value is None:
        raise ValueError(f"foam_system_sizing {label} is required")
    return float(value)


def _opt_float(source: dict, name: str) -> float | None:
    value = source.get(name)
    return None if value is None else float(value)


# -- output helpers (FATG workflow pattern) ----------------------------------


def _output_path(cfg: dict, settings: dict[str, Any], suffix: str) -> Path:
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
    return str(cfg.get("basename", "foam_system_sizing"))


def _write_csv(path: Path, rows: list[dict[str, Any]]) -> None:
    if not rows:
        raise ValueError("foam_system_sizing cannot write empty results")
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
