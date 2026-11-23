# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Sweep static configurations and retain every constraint disposition.

The objective is the maximum excursion curvature range at touchdown and
sag bend. It is a stress-range proxy, not a fatigue calculation. Static
von Mises utilisation is a sampled thin-wall stress screen, not a standard
compliance check. Dynamic bending, contact stiffness and ovalisation are
outside its qualification.
"""

import csv
import hashlib
import json
import math
import time
from pathlib import Path

from digitalmodel.parametric.atlas import Axis
from digitalmodel.parametric.generate import _grid_points
from digitalmodel.riser_configuration_screen.static import lazy_wave_offsets, scr_offsets

FATIGUE_STATEMENT = "Fatigue damage and life are not computed here."
AXES = {"hangoff_angle_deg", "buoyancy_factor", "sag_fraction", "hog_fraction"}


def _validate(settings: dict) -> None:
    for name in ("water_depth_m", "submerged_weight_n_m", "grounded_length_m"):
        value = float(settings[name])
        if not math.isfinite(value) or value <= 0:
            raise ValueError(f"{name} must be finite and positive")
    if settings["riser_type"] not in ("SCR", "SLWR"):
        raise ValueError("riser_type must be SCR or SLWR")
    if not settings["offsets_m"]:
        raise ValueError("offsets_m must contain finite excursions")
    for offset in settings["offsets_m"]:
        if isinstance(offset, dict) and set(offset) != {"x_m", "y_m"}:
            raise ValueError("horizontal excursions require x_m and y_m")
        values = offset.values() if isinstance(offset, dict) else [offset]
        if any(not math.isfinite(float(v)) for v in values):
            raise ValueError("offsets_m must contain finite excursions")
    required = AXES if settings["riser_type"] == "SLWR" else {"hangoff_angle_deg"}
    if set(settings["axes"]) != required:
        raise ValueError("axes do not match the riser type")
    for values in settings["axes"].values():
        if not values or any(not math.isfinite(float(v)) for v in values):
            raise ValueError("axes must contain finite grid values")
    pipe = settings["pipe"]
    for name in ("outer_diameter_m", "wall_thickness_m", "youngs_modulus_pa", "yield_strength_pa"):
        if not math.isfinite(float(pipe[name])) or float(pipe[name]) <= 0:
            raise ValueError(f"pipe {name} must be finite and positive")
    if 2 * pipe["wall_thickness_m"] >= pipe["outer_diameter_m"]:
        raise ValueError("pipe wall must leave a positive bore")
    for name in ("internal_pressure_pa", "external_pressure_pa"):
        if not math.isfinite(float(pipe[name])) or float(pipe[name]) < 0:
            raise ValueError(f"pipe {name} must be finite and nonnegative")
    for name in ("max_top_tension_n", "min_bend_radius_m", "min_hog_clearance_m", "max_static_utilisation"):
        if not math.isfinite(float(settings["constraints"][name])) or float(settings["constraints"][name]) <= 0:
            raise ValueError(f"constraint {name} must be finite and positive")


def _utilisation(pipe: dict, configuration: dict, row: dict) -> float:
    outer = float(pipe["outer_diameter_m"])
    inner = outer - 2 * float(pipe["wall_thickness_m"])
    area = math.pi * (outer**2 - inner**2) / 4
    inside, outside = float(pipe["internal_pressure_pa"]), float(pipe["external_pressure_pa"])
    pressure_force = math.pi * (inside * inner**2 - outside * outer**2) / 4
    hoop = (inside - outside) * outer / (2 * float(pipe["wall_thickness_m"]))
    horizontal = row["horizontal_tension_n"]
    vertical = math.sqrt(max(0.0, row["top_tension_n"]**2 - horizontal**2))
    weight = float(configuration["submerged_weight_n_m"])
    start = row["buoyed_start_m"]
    length = row["buoyed_length_m"]
    maximum = 0.0
    for arc, _, _ in row["shape"]:
        buoyed_arc = 0.0 if start is None else min(length, max(0.0, arc - start))
        factor = float(configuration.get("buoyancy_factor", 0.0))
        force = vertical - weight * arc + weight * factor * buoyed_arc
        tension = math.hypot(horizontal, force)
        local_weight = weight * (1 - factor) if start is not None and start < arc < start + length else weight
        # d(theta)/ds = |w| H / (H**2 + V**2), with units 1/m.
        curvature = abs(local_weight) * horizontal / tension**2
        bending = float(pipe["youngs_modulus_pa"]) * outer * curvature / 2
        for sign in (-1, 1):
            axial = (tension + pressure_force) / area + sign * bending
            vm = math.sqrt(axial**2 + hoop**2 - axial * hoop)
            maximum = max(maximum, vm / float(pipe["yield_strength_pa"]))
    return maximum


def _constraint(margin: float | None, reason: str = "", unit: str = "1") -> dict:
    return {"status": "unavailable" if margin is None else ("pass" if margin >= 0 else "violation"),
            "margin": margin, "unit": unit, "reason": reason}


def _checks(configuration: dict, rows: list[dict], limits: dict, reason: str) -> dict:
    checks = {}
    if "buoyancy_factor" in configuration:
        checks["net_buoyancy"] = _constraint(float(configuration["buoyancy_factor"]) - 1.0)
        if float(configuration["buoyancy_factor"]) <= 1.0:
            checks["net_buoyancy"]["status"] = "violation"
        checks["hog_above_sag"] = _constraint(float(configuration["hog_fraction"]) - float(configuration["sag_fraction"]))
        if float(configuration["hog_fraction"]) <= float(configuration["sag_fraction"]):
            checks["hog_above_sag"]["status"] = "violation"
    if not rows:
        checks["shape_closure"] = _constraint(None, reason, "m")
        for name in ("top_tension_n", "bend_radius_m", "hog_clearance_m", "static_von_mises_utilisation"):
            unit = "N" if name == "top_tension_n" else ("1" if name == "static_von_mises_utilisation" else "m")
            checks[name] = _constraint(None, reason, unit)
        return checks
    checks["shape_closure"] = _constraint(1e-6 - max(max(abs(r["vertical_closure_error_m"]), abs(r["horizontal_closure_error_m"])) for r in rows), unit="m")
    checks["top_tension_n"] = _constraint(float(limits["max_top_tension_n"]) - max(r["top_tension_n"] for r in rows), unit="N")
    radii = [1 / r["touchdown_curvature_per_m"] for r in rows]
    radii += [r[name] for r in rows for name in ("sag_radius_m", "hog_radius_m") if r[name] is not None]
    checks["bend_radius_m"] = _constraint(min(radii) - float(limits["min_bend_radius_m"]), unit="m")
    clearance = [r["hog_clearance_m"] for r in rows if r["hog_clearance_m"] is not None]
    checks["hog_clearance_m"] = (_constraint(min(clearance) - float(limits["min_hog_clearance_m"]), unit="m")
                                 if clearance else {"status": "not_applicable", "margin": None, "unit": "m", "reason": "SCR has no hog bend"})
    checks["static_von_mises_utilisation"] = _constraint(float(limits["max_static_utilisation"]) - max(r["static_von_mises_utilisation"] for r in rows))
    return checks


def _candidate(settings: dict, point: dict) -> dict:
    configuration = {**settings, **point}
    reason = ""
    try:
        solver = scr_offsets if settings["riser_type"] == "SCR" else lazy_wave_offsets
        rows = solver(configuration, settings["offsets_m"])
        for row in rows:
            row["tier"] = "SCREEN"
            row["fatigue_statement"] = FATIGUE_STATEMENT
            row["static_von_mises_utilisation"] = _utilisation(settings["pipe"], configuration, row)
    except ValueError as exc:
        rows, reason = [], str(exc)
    checks = _checks(configuration, rows, settings["constraints"], reason)
    ranges = {}
    for name in ("touchdown_curvature_per_m", "sag_curvature_per_m"):
        values = [row[name] for row in rows if row[name] is not None]
        ranges[name] = max(values) - min(values) if values else None
    objective = max(value for value in ranges.values() if value is not None) if rows else None
    return {"tier": "SCREEN", "fatigue_statement": FATIGUE_STATEMENT, "configuration": point,
            "offset_results": rows, "constraints": checks, "curvature_ranges": ranges,
            "curvature_range_per_m": objective,
            "feasible": bool(rows) and all(c["status"] in ("pass", "not_applicable") for c in checks.values())}


def _flat_axes(rows: list[dict], axes: dict) -> list[str]:
    flat = []
    valid = [row for row in rows if row["curvature_range_per_m"] is not None]
    for axis, values in axes.items():
        if len(set(values)) < 2:
            continue
        groups: dict[tuple, list[dict]] = {}
        for row in valid:
            key = tuple((name, value) for name, value in row["configuration"].items() if name != axis)
            groups.setdefault(key, []).append(row)
        complete = [group for group in groups.values() if len(group) == len(set(values))]
        if complete and all(math.isclose(group[0]["curvature_range_per_m"], row["curvature_range_per_m"], rel_tol=1e-8, abs_tol=1e-14) for group in complete for row in group):
            flat.append(axis)
    return flat


def screen(settings: dict) -> dict:
    """Rank the existing parametric grid without excluding violated candidates.

    Args:
        settings: Pipe, excursions, criteria and explicit grid in SI units.

    Returns:
        Ranked rows, selection rule and provisional solver candidate list.

    Raises:
        ValueError: If inputs are invalid or a swept objective is flat.
    """
    _validate(settings)
    started = time.perf_counter()
    axes = [Axis(name=name, grid=[float(v) for v in values]) for name, values in settings["axes"].items()]
    rows = [_candidate(settings, point) for point in _grid_points(axes)]
    flat = _flat_axes(rows, settings["axes"])
    if flat:
        raise ValueError(f"flat curvature-range objective across: {', '.join(flat)}")
    rows.sort(key=lambda row: (not row["feasible"], row["curvature_range_per_m"] is None,
                              row["curvature_range_per_m"] or 0.0, tuple(row["configuration"].values())))
    for rank, row in enumerate(rows, 1):
        row["rank"] = rank
    count = 3 if settings["riser_type"] == "SCR" else 10
    candidates = [row["configuration"] for row in rows if row["feasible"]][:count]
    return {"tier": "SCREEN", "fatigue_statement": FATIGUE_STATEMENT,
            "data_basis": settings.get("data_basis", "unverified input basis"),
            "criteria_revision": settings.get("criteria_revision", "unverified"),
            "input_sha256": hashlib.sha256(json.dumps(settings, sort_keys=True, allow_nan=False).encode()).hexdigest(),
            "provisional": bool(settings.get("provisional", True)),
            "riser_type": settings["riser_type"], "grid_size": len(rows),
            "runtime_s": time.perf_counter() - started,
            "selection_rule": f"First {count} feasible configurations by ascending maximum touchdown/sag excursion curvature range; ties use ascending grid coordinates. Violated configurations remain in the ranked table.",
            "limitations": "Inextensible, frictionless mobile touchdown; horizontal near/far/cross offsets resolved into a radial static plane. Heave and rotations are unassessed. Static von Mises is sampled thin-wall stress with elastic curvature bending. No dynamic or code-compliance qualification.",
            "ranked_candidates": rows, "solver_candidates": candidates}


def router(cfg: dict) -> dict:
    """Write SCREEN tables to the explicitly selected output folder.

    Args:
        cfg: Workflow settings and Analysis.result_folder.

    Returns:
        Configuration with the screening result.
    """
    result = screen(cfg["riser_configuration_screen"])
    folder = Path(cfg["Analysis"]["result_folder"])
    folder.mkdir(parents=True, exist_ok=True)
    payload = json.dumps(result, indent=2, allow_nan=False)
    target = folder / "ranked_candidates.json"
    target.write_text(payload + "\n", encoding="utf-8")
    if json.loads(target.read_text(encoding="utf-8")) != result:
        raise ValueError("saved screening output differs from computed output")
    with (folder / "ranked_candidates.csv").open("w", newline="", encoding="utf-8") as stream:
        fields = ["rank", "tier", "provisional", "data_basis", "criteria_revision", "input_sha256", "limitations", "fatigue_statement", "configuration", "feasible", "curvature_range_per_m", "constraints", "offset_results", "curvature_ranges"]
        writer = csv.DictWriter(stream, fieldnames=fields)
        writer.writeheader()
        for row in result["ranked_candidates"]:
            record = {key: row[key] if key in row else result[key] for key in fields}
            writer.writerow({key: json.dumps(value, allow_nan=False) if isinstance(value, (dict, list)) else value for key, value in record.items()})
    cfg["riser_configuration_screen_result"] = result
    return cfg
