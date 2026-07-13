"""Durable workflow: legacy DYNMOOR static mooring + passing-ship forces.

Wires the ported legacy DYNMOOR chain
(``digitalmodel.subsea.mooring_analysis.dynmoor``) into a registry
workflow: line-type load-excursion tables -> pretension/anchor placement
-> static restoring forces at vessel offsets (+ optional fenders, static
wind and passing-ship force histories).

The legacy time-domain simulation (random sea, Morison hull loads, wave
drift, gusting, excursion/tension statistics) is NOT yet ported; a
``time_domain`` key in the config raises with a pointer to the follow-up
(see TODO(dynmoor-time-domain) in the physics module).

Config schema (YAML basename ``dynmoor_mooring``)::

    basename: dynmoor_mooring
    dynmoor_mooring:
      vessel:
        lbp_ft: 600.0
        beam_ft: 100.0
        draft_ft: 30.0
        displacement_kips: 30000.0
        water_depth_ft: 50.0
      line_types:
        - segments:              # 1-3 segments, anchor end first
            - {material: chain, length_ft: 1000.0, weight_air_lb_ft: 60.0,
               area_in2: 10.0, modulus_ksi: 3000.0,
               breaking_strength_kips: 500.0, size_in: 2.5}
          anchor: {weight_kips: 10.0, mud_efficiency: 1.0, sand_efficiency: 1.5}
          vertical_distance_anchor_to_fairlead_ft: 50.0
          bottom_friction_percent: 2.0
          # optional: pile_height_ft, bottom_slope_deg, buoy_buoyancy_kips,
          #           clump_weight_kips, nonlinear_spring {length_ft,
          #           breaking_strength_kips, p1, ep1, p2, ep2}
      lines:
        - {type: 1, fairlead_x_ft: 300.0, fairlead_y_ft: -50.0,
           angle_deg: 300.0, pretension_percent: 15.0}
      offsets:                   # optional static offsets to evaluate
        - {surge_ft: 5.0, sway_ft: 2.0, yaw_deg: 0.5}
      fenders:                   # optional
        side: 1
        curves:
          - {station_x_ft: 30.0, initial_gap_ft: 0.2,
             deflections_ft: [0.7, 2.8, 3.5, 4.2],
             forces_kips: [43.9, 175.7, 219.7, 263.6]}
      wind:                      # optional static wind force
        speed_knots: 30.0
        direction_deg: 60.0      # clockwise from bow
        frontal_areas:
          - {width_ft: 100.0, height_ft: 25.0, vca_above_keel_ft: 40.0,
             shape_coefficient: 1.0}
        lateral_areas:
          - {width_ft: 600.0, height_ft: 25.0, vca_above_keel_ft: 40.0,
             shape_coefficient: 1.0, lever_arm_ft: 0.0}
      passing_ship:              # optional force history sweep
        mode: parallel           # parallel | perpendicular
        nsteps: 300
        start_step: 10
        direction: -1.0
        speed_knots: 6.0
        separation_ft: 300.0
        passing_length_ft: 650.0
        passing_beam_ft: 100.0
        current_knots: 0.5
        current_direction_deg: 0.0
        # parallel: moored_midship_area_ft2, passing_midship_area_ft2
        # perpendicular: passing_draft_ft, passing_displacement_kips
      output_dir: results

Units: legacy English set (ft, kips, knots, degrees clockwise from bow).
"""

from __future__ import annotations

import csv
from pathlib import Path
from typing import Any

from digitalmodel.subsea.mooring_analysis.dynmoor import (
    DynmoorLine,
    DynmoorLineType,
    DynmoorSegment,
    Fender,
    NonlinearSpring,
    WindArea,
    anchor_holding_power,
    build_line_table,
    build_wind_model,
    fender_forces,
    passing_ship_sweep,
    restoring_force,
    solve_pretension,
    static_wind_force,
)

import math

REPO_ROOT = Path(__file__).resolve().parents[3]


def router(cfg: dict) -> dict:
    settings = cfg.get("dynmoor_mooring") or {}
    if settings.get("time_domain") is not None:
        raise NotImplementedError(
            "dynmoor_mooring time_domain is not ported yet - the legacy "
            "OSCILL integrator (random sea, Morison hull loads, wave drift, "
            "gusting, excursion/tension statistics) is a documented gap; "
            "see TODO(dynmoor-time-domain) in "
            "digitalmodel/subsea/mooring_analysis/dynmoor.py"
        )
    vessel = settings.get("vessel") or {}
    draft_ft = float(vessel.get("draft_ft", 0.0))
    line_types = [_line_type(item, i) for i, item in enumerate(_req_list(settings, "line_types"))]
    lines = [_line(item, i, len(line_types)) for i, item in enumerate(_req_list(settings, "lines"))]

    tables = [build_line_table(lt) for lt in line_types]
    solutions = [
        solve_pretension(line, line_types[line.line_type - 1], tables[line.line_type - 1])
        for line in lines
    ]
    anchors = [(sol.anchor_x_ft, sol.anchor_y_ft) for sol in solutions]

    table_rows = []
    for type_no, (lt, table) in enumerate(zip(line_types, tables), start=1):
        for row in range(len(table.h)):
            table_rows.append(
                {
                    "line_type": type_no,
                    "row": row + 1,
                    "model": table.model,
                    "distance_to_anchor_ft": table.u[row],
                    "horizontal_tension_kips": table.h[row] / 1000.0,
                    "total_tension_kips": table.t[row] / 1000.0,
                    "suspended_length_ft": table.total_length_ft - table.z[row],
                    "vertical_anchor_pull_kips": table.p[row] / 1000.0,
                }
            )

    line_rows = []
    for no, (line, sol) in enumerate(zip(lines, solutions), start=1):
        hmud, hsand = anchor_holding_power(line_types[line.line_type - 1])
        line_rows.append(
            {
                "line": no,
                "line_type": line.line_type,
                "pretension_kips": sol.pretension_lb / 1000.0,
                "pretension_percent_bs": line.pretension_percent,
                "horizontal_distance_to_anchor_ft": sol.horizontal_distance_ft,
                "anchor_x_ft": sol.anchor_x_ft,
                "anchor_y_ft": sol.anchor_y_ft,
                "fairlead_angle_deg": sol.fairlead_angle_deg,
                "anchor_holding_mud_kips": hmud,
                "anchor_holding_sand_kips": hsand,
            }
        )

    offset_rows = []
    fender_cfg = settings.get("fenders") or {}
    fenders = [_fender(item) for item in fender_cfg.get("curves", [])]
    side = int(fender_cfg.get("side", 1))
    wind_cfg = settings.get("wind")
    wind_model = None
    if wind_cfg:
        wind_model = build_wind_model(
            [_wind_area(a) for a in wind_cfg.get("frontal_areas", [])],
            [_wind_area(a) for a in wind_cfg.get("lateral_areas", [])],
            draft_ft,
        )
    for item in settings.get("offsets", []) or []:
        surge = float(item.get("surge_ft", 0.0))
        sway = float(item.get("sway_ft", 0.0))
        yaw_rad = float(item.get("yaw_deg", 0.0)) * math.pi / 180.0
        result = restoring_force(lines, tables, anchors, surge, sway, yaw_rad)
        row = {
            "surge_ft": surge,
            "sway_ft": sway,
            "yaw_deg": float(item.get("yaw_deg", 0.0)),
            "restoring_surge_kips": result.surge_lb / 1000.0,
            "restoring_sway_kips": result.sway_lb / 1000.0,
            "restoring_yaw_kip_ft": result.yaw_lb_ft / 1000.0,
            "max_tension_line": result.max_tension_line,
        }
        for no, (tension, horizontal) in enumerate(
            zip(result.total_tensions_lb, result.horizontal_tensions_lb), start=1
        ):
            row[f"line_{no}_tension_kips"] = tension / 1000.0
            row[f"line_{no}_horizontal_kips"] = horizontal / 1000.0
        if fenders:
            fl, fmz, _ = fender_forces(fenders, sway, yaw_rad, side)
            row["fender_sway_kips"] = fl / 1000.0
            row["fender_yaw_kip_ft"] = fmz / 1000.0
        if wind_model is not None:
            fwd = static_wind_force(
                wind_model,
                float(wind_cfg["speed_knots"]),
                float(wind_cfg["direction_deg"]),
                yaw_rad,
            )
            row["wind_surge_kips"] = fwd[0] / 1000.0
            row["wind_sway_kips"] = fwd[1] / 1000.0
            row["wind_yaw_kip_ft"] = fwd[2] / 1000.0
        offset_rows.append(row)

    passing_rows = []
    passing_cfg = settings.get("passing_ship")
    if passing_cfg:
        samples = passing_ship_sweep(
            mode=str(passing_cfg.get("mode", "parallel")),
            nsteps=int(passing_cfg.get("nsteps", 300)),
            start_step=int(passing_cfg.get("start_step", 1)),
            direction=float(passing_cfg.get("direction", 1.0)),
            speed_knots=_req_float(passing_cfg, "speed_knots", "passing_ship.speed_knots"),
            moored_length_ft=_req_float(vessel, "lbp_ft", "vessel.lbp_ft"),
            moored_beam_ft=float(vessel.get("beam_ft", 0.0)),
            moored_displacement_kips=float(vessel.get("displacement_kips", 0.0)),
            passing_length_ft=_req_float(
                passing_cfg, "passing_length_ft", "passing_ship.passing_length_ft"
            ),
            passing_beam_ft=float(passing_cfg.get("passing_beam_ft", 0.0)),
            separation_ft=_req_float(passing_cfg, "separation_ft", "passing_ship.separation_ft"),
            water_depth_ft=_req_float(vessel, "water_depth_ft", "vessel.water_depth_ft"),
            current_knots=float(passing_cfg.get("current_knots", 0.0)),
            current_direction_deg=float(passing_cfg.get("current_direction_deg", 0.0)),
            moored_midship_area_ft2=float(passing_cfg.get("moored_midship_area_ft2", 0.0)),
            passing_midship_area_ft2=float(passing_cfg.get("passing_midship_area_ft2", 0.0)),
            passing_draft_ft=float(passing_cfg.get("passing_draft_ft", 0.0)),
            passing_displacement_kips=float(passing_cfg.get("passing_displacement_kips", 0.0)),
        )
        passing_rows = [
            {
                "step": s.step,
                "x_over_l": s.x_over_l,
                "surge_kips": s.surge_lb / 1000.0,
                "sway_kips": s.sway_lb / 1000.0,
                "yaw_kip_ft": s.yaw_lb_ft / 1000.0,
            }
            for s in samples
        ]

    outputs: dict[str, str] = {}
    tables_csv = _output_path(cfg, settings, "dynmoor_line_tables.csv")
    lines_csv = _output_path(cfg, settings, "dynmoor_lines.csv")
    _write_csv(tables_csv, table_rows)
    _write_csv(lines_csv, line_rows)
    outputs["line_tables_csv"] = _display_path(tables_csv)
    outputs["lines_csv"] = _display_path(lines_csv)
    if offset_rows:
        offsets_csv = _output_path(cfg, settings, "dynmoor_offsets.csv")
        _write_csv(offsets_csv, offset_rows)
        outputs["offsets_csv"] = _display_path(offsets_csv)
    if passing_rows:
        passing_csv = _output_path(cfg, settings, "dynmoor_passing_ship.csv")
        _write_csv(passing_csv, passing_rows)
        outputs["passing_ship_csv"] = _display_path(passing_csv)

    peak = None
    if passing_rows:
        peak = {
            "surge_kips": max(abs(r["surge_kips"]) for r in passing_rows),
            "sway_kips": max(abs(r["sway_kips"]) for r in passing_rows),
            "yaw_kip_ft": max(abs(r["yaw_kip_ft"]) for r in passing_rows),
        }
    cfg["dynmoor_mooring"] = {
        **settings,
        "method": "dynmoor_legacy_static_v610",
        "lines": line_rows,
        "offsets_evaluated": offset_rows,
        "passing_ship_peak_abs": peak,
        "time_domain_ported": False,
        **outputs,
    }
    return cfg


# -- config parsing ----------------------------------------------------------


def _req_list(settings: dict, name: str) -> list:
    value = settings.get(name)
    if not isinstance(value, list) or not value:
        raise ValueError(f"dynmoor_mooring {name} must be a non-empty list")
    return value


def _req_float(source: dict, name: str, label: str) -> float:
    value = source.get(name)
    if value is None:
        raise ValueError(f"dynmoor_mooring {label} is required")
    return float(value)


def _segment(item: dict[str, Any]) -> DynmoorSegment:
    return DynmoorSegment(
        material=str(item.get("material", "none")).lower(),
        length_ft=float(item.get("length_ft", 0.0)),
        weight_air_lb_ft=float(item.get("weight_air_lb_ft", 0.0)),
        area_in2=float(item.get("area_in2", 0.0)),
        modulus_ksi=float(item.get("modulus_ksi", 0.0)),
        breaking_strength_kips=float(item.get("breaking_strength_kips", 0.0)),
        size_in=float(item.get("size_in", 0.0)),
    )


def _line_type(item: dict[str, Any], index: int) -> DynmoorLineType:
    label = f"line_types[{index}]"
    segments = item.get("segments")
    if not isinstance(segments, list) or not 1 <= len(segments) <= 3:
        raise ValueError(f"dynmoor_mooring {label}.segments needs 1-3 entries")
    segs = [_segment(s) for s in segments]
    while len(segs) < 3:
        segs.append(DynmoorSegment())
    spring = None
    if item.get("nonlinear_spring"):
        sp = item["nonlinear_spring"]
        spring = NonlinearSpring(
            length_ft=float(sp["length_ft"]),
            breaking_strength_kips=float(sp["breaking_strength_kips"]),
            ep1=float(sp["ep1"]),
            p1=float(sp["p1"]),
            ep2=float(sp["ep2"]),
            p2=float(sp["p2"]),
        )
    anchor = item.get("anchor") or {}
    depth = item.get("vertical_distance_anchor_to_fairlead_ft")
    if depth is None:
        raise ValueError(
            f"dynmoor_mooring {label}.vertical_distance_anchor_to_fairlead_ft is required"
        )
    return DynmoorLineType(
        segment1=segs[0],
        segment2=segs[1],
        segment3=segs[2],
        spring=spring,
        water_depth_ft=float(depth),
        pile_height_ft=float(item.get("pile_height_ft", 0.0)),
        bottom_slope_deg=float(item.get("bottom_slope_deg", 0.0)),
        bottom_friction_percent=float(item.get("bottom_friction_percent", 0.0)),
        anchor_weight_kips=float(anchor.get("weight_kips", 0.0)),
        anchor_mud_efficiency=float(anchor.get("mud_efficiency", 0.0)),
        anchor_sand_efficiency=float(anchor.get("sand_efficiency", 0.0)),
        buoy_buoyancy_kips=float(item.get("buoy_buoyancy_kips", 0.0)),
        clump_weight_kips=float(item.get("clump_weight_kips", 0.0)),
    )


def _line(item: dict[str, Any], index: int, n_types: int) -> DynmoorLine:
    label = f"lines[{index}]"
    type_no = int(item.get("type", 1))
    if not 1 <= type_no <= n_types:
        raise ValueError(f"dynmoor_mooring {label}.type must be 1..{n_types}")
    return DynmoorLine(
        line_type=type_no,
        fairlead_x_ft=_req_float(item, "fairlead_x_ft", f"{label}.fairlead_x_ft"),
        fairlead_y_ft=_req_float(item, "fairlead_y_ft", f"{label}.fairlead_y_ft"),
        angle_deg=_req_float(item, "angle_deg", f"{label}.angle_deg"),
        pretension_percent=_req_float(
            item, "pretension_percent", f"{label}.pretension_percent"
        ),
    )


def _fender(item: dict[str, Any]) -> Fender:
    deflections = tuple(float(v) for v in item["deflections_ft"])
    forces = tuple(float(v) for v in item["forces_kips"])
    if len(deflections) != 4 or len(forces) != 4:
        raise ValueError("dynmoor_mooring fender curves need 4 deflection/force points")
    return Fender(
        station_x_ft=float(item.get("station_x_ft", 0.0)),
        initial_gap_ft=float(item.get("initial_gap_ft", 0.0)),
        deflections_ft=deflections,
        forces_kips=forces,
    )


def _wind_area(item: dict[str, Any]) -> WindArea:
    return WindArea(
        width_ft=float(item["width_ft"]),
        height_ft=float(item["height_ft"]),
        vca_above_keel_ft=float(item["vca_above_keel_ft"]),
        shape_coefficient=float(item.get("shape_coefficient", 1.0)),
        lever_arm_ft=float(item.get("lever_arm_ft", 0.0)),
    )


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
    return str(cfg.get("basename", "dynmoor_mooring"))


def _write_csv(path: Path, rows: list[dict[str, Any]]) -> None:
    if not rows:
        raise ValueError("dynmoor_mooring cannot write empty results")
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
