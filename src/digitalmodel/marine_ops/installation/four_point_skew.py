# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Four-point single-hook lift: linear axial springs and tension-only skew.

SI units throughout. Nominal padeyes must be horizontal and in convex perimeter
order. A rigid rotation aligns hook-to-CoG with world vertical in each case.
See the workflow README for derivation, tolerance definitions and limits.
This module has a standalone CLI to avoid loading unrelated solver adapters.
"""

from __future__ import annotations

import argparse
import ast
from itertools import combinations, product
import json
from pathlib import Path

import numpy as np


def _screen_basis():
    # Read only literal assignments: importing the OrcaFlex package initializer
    # would load unrelated adapters. PR 2678 remains the single value source.
    path = Path(__file__).resolve().parents[2] / "orcaflex" / "installation_analysis.py"
    names = {"_SKL_BY_LEG_COUNT", "_SKL_REFERENCE"}
    values = {}
    try:
        for node in ast.parse(path.read_text(encoding="utf-8")).body:
            if isinstance(node, ast.Assign) and len(node.targets) == 1:
                target = node.targets[0]
            elif isinstance(node, ast.AnnAssign):
                target = node.target
            else:
                continue
            if isinstance(target, ast.Name) and target.id in names:
                values[target.id] = ast.literal_eval(node.value)
        return values["_SKL_BY_LEG_COUNT"][4], values["_SKL_REFERENCE"]
    except (OSError, SyntaxError, ValueError, TypeError, KeyError) as exc:
        raise ValueError("Cannot read four-leg minimum basis from installation_analysis.py") from exc


FOUR_LEG_MINIMUM_SKL, FOUR_LEG_MINIMUM_SOURCE = _screen_basis()


def _array(value, shape, name):
    try:
        array = np.asarray(value, dtype=float)
    except (TypeError, ValueError) as exc:
        raise ValueError(f"{name} must contain finite numbers") from exc
    if array.shape != shape or not np.isfinite(array).all():
        raise ValueError(f"{name} must have shape {shape} and finite values")
    return array


def _scalar(value, name, minimum=0.0, inclusive=False):
    number = float(_array(value, (), name))
    if number < minimum or (number == minimum and not inclusive):
        raise ValueError(f"{name} must be {'>=' if inclusive else '>'} {minimum}")
    return number


def _bounds(value, name):
    if np.isscalar(value):
        value = [value] * 4
    values = _array(value, (4,), name)
    if np.any(values < 0):
        raise ValueError(f"{name} must be nonnegative")
    return values


def _validate(settings):
    allowed = {
        "lift_points_m", "hook_height_m", "weight_N", "sling_EA_N",
        "sling_length_m", "cog_envelope_xy_m", "code_minimum_skl",
        "code_minimum_source", "payload_warp_stiffness_N_per_m", "tolerances",
    }
    if set(settings) - allowed:
        raise ValueError(f"Unknown settings: {sorted(set(settings) - allowed)}")
    required = allowed - {"payload_warp_stiffness_N_per_m", "tolerances",
                          "code_minimum_skl", "code_minimum_source"}
    if required - set(settings):
        raise ValueError(f"Missing settings: {sorted(required - set(settings))}")
    points = _array(settings["lift_points_m"], (4, 3), "lift_points_m")
    if np.ptp(points[:, 2]) > 1e-9:
        raise ValueError("Nominal lift points must be horizontal and coplanar")
    edges = np.roll(points[:, :2], -1, axis=0) - points[:, :2]
    following = np.roll(edges, -1, axis=0)
    turns = edges[:, 0] * following[:, 1] - edges[:, 1] * following[:, 0]
    if not (np.all(turns > 1e-12) or np.all(turns < -1e-12)):
        raise ValueError("Lift points must form a convex quadrilateral in perimeter order")
    hook_z = float(_array(settings["hook_height_m"], (), "hook_height_m"))
    height = _scalar(hook_z - points[0, 2], "hook clearance")
    weight = _scalar(settings["weight_N"], "weight_N")
    ea = _array(settings["sling_EA_N"], (4,), "sling_EA_N")
    lengths = _array(settings["sling_length_m"], (4,), "sling_length_m")
    if np.any(ea <= 0) or np.any(lengths <= 0):
        raise ValueError("All sling EA and reference lengths must be positive")
    minimum = _scalar(settings.get("code_minimum_skl", FOUR_LEG_MINIMUM_SKL),
                      "code_minimum_skl", FOUR_LEG_MINIMUM_SKL, inclusive=True)
    if "code_minimum_source" in settings and "code_minimum_skl" not in settings:
        raise ValueError("A supplied code_minimum_source requires code_minimum_skl")
    if ("code_minimum_skl" in settings
            and minimum != FOUR_LEG_MINIMUM_SKL
            and "code_minimum_source" not in settings):
        raise ValueError("A custom code_minimum_skl requires code_minimum_source")
    source = settings.get("code_minimum_source", FOUR_LEG_MINIMUM_SOURCE)
    if not isinstance(source, str) or not source.strip():
        raise ValueError("A code_minimum_source is required")
    cogs = np.asarray(settings["cog_envelope_xy_m"], dtype=float)
    if cogs.ndim != 2 or cogs.shape[1] != 2 or not len(cogs) or not np.isfinite(cogs).all():
        raise ValueError("cog_envelope_xy_m must be a nonempty list of finite xy corners")
    kw = settings.get("payload_warp_stiffness_N_per_m")
    if kw is not None:
        kw = _scalar(kw, "payload_warp_stiffness_N_per_m")
        centre = points[:, :2].mean(axis=0)
        radii = np.linalg.norm(points[:, :2] - centre, axis=1)
        if not (np.allclose(points[0, :2] + points[2, :2], 2 * centre, atol=1e-9, rtol=0)
                and np.allclose(radii, radii[0], atol=1e-9, rtol=0)
                and np.allclose(cogs, centre, atol=1e-9, rtol=0)):
            raise ValueError("Finite payload warp stiffness requires a centred rectangular lift")
    tolerances = settings.get("tolerances", {})
    names = {"general_length_m", "matched_pair_difference_m", "padeye_elevation_m"}
    if set(tolerances) - names:
        raise ValueError("Unknown tolerance setting")
    general = _bounds(tolerances.get("general_length_m", 0.01), "general_length_m")
    if np.any(general <= 0):
        raise ValueError("general_length_m must be > 0 for every leg")
    elevation = _bounds(tolerances.get("padeye_elevation_m", 0.001), "padeye_elevation_m")
    matched = tolerances.get("matched_pair_difference_m")
    if matched is not None:
        matched = _scalar(matched, "matched_pair_difference_m", inclusive=True)
        if "general_length_m" not in tolerances:
            raise ValueError("Matched pairs also require general_length_m absolute bounds")
    return points, height, weight, ea, lengths, cogs, kw, general, elevation, matched


def _reference_misfit(points, height, lengths):
    """Fit one hook in the payload frame, independent of the CoG corners."""
    hook = np.r_[points[:, :2].mean(axis=0), points[0, 2] + height]
    for _ in range(50):
        arms = hook - points
        distances = np.linalg.norm(arms, axis=1)
        if np.any(distances <= 0):
            raise ValueError("Reference hook coincides with a lift point")
        step = np.linalg.lstsq(arms / distances[:, None], lengths - distances, rcond=None)[0]
        hook += step
        if np.linalg.norm(step) < 1e-11 * np.min(lengths):
            break
    else:
        raise ValueError("Reference hook fit did not converge")
    if abs(points[0, 2] + height - hook[2]) > 0.01 * np.min(lengths):
        raise ValueError("Reference hook height mismatch exceeds 0.01 L")
    return lengths - np.linalg.norm(hook - points, axis=1), hook


def _tilt(cog, hook, eye_z):
    tilt = float(np.degrees(np.arctan2(np.linalg.norm(cog - hook[:2]), hook[2] - eye_z)))
    return {"tilt_deg": tilt, "tilt_exceeds_3_deg": tilt > 3.0}


def _system(points, height, weight, ea, lengths, cog, kw, misfit, hook):
    # Work in rigid payload axes. Rotating hook-to-CoG onto world vertical
    # preserves every hook-to-eye distance, including at diagonal CoG offsets.
    arms = hook - points
    distances = np.linalg.norm(arms, axis=1)
    directions = arms / distances[:, None]
    vertical = hook - np.r_[cog, points[0, 2]]
    vertical /= np.linalg.norm(vertical)
    cosine = directions @ vertical
    if np.any(cosine <= 0):
        raise ValueError("All slings must rise towards the hook")
    a = directions.T
    b = weight * vertical
    _, _, vh = np.linalg.svd(a, full_matrices=True)
    mode = vh[-1] / np.max(np.abs(vh[-1]))
    particular = np.linalg.lstsq(a, b, rcond=None)[0]
    lower, upper = -np.inf, np.inf
    for p, n in zip(particular, mode):
        if abs(n) < 1e-12:
            if p < -1e-8 * weight:
                raise ValueError("CoG has no tension-only equilibrium")
        elif n > 0:
            lower = max(lower, -p/n)
        else:
            upper = min(upper, -p/n)
    if lower > upper + 1e-9 * weight:
        raise ValueError("CoG has no tension-only equilibrium inside the lift polygon")
    # Roundoff can make the feasible interval infinitesimally inverted at a corner.
    if lower > upper:
        lower = upper = (lower + upper) / 2
    compliance = np.diag(lengths / ea)
    if kw is not None:
        vertical_mode = cosine * mode
        compliance += np.outer(vertical_mode, vertical_mode) / (mode @ mode * kw)
    return a, b, cosine, mode, particular, lower, upper, compliance, misfit


def _solve(system, length_errors, elevation_errors):
    a, b, cosine, mode, particular, lower, upper, compliance, misfit = system
    error = misfit + length_errors + cosine * elevation_errors
    # All equilibrated states are p + n*alpha. The minimum complementary
    # energy in this one-dimensional feasible interval enforces compatibility.
    alpha = -(mode @ (compliance @ particular + error)) / (mode @ compliance @ mode)
    tension = particular + mode * np.clip(alpha, lower, upper)
    tension = np.maximum(tension, 0.0)
    if not np.allclose(a @ tension, b, rtol=1e-9, atol=1e-8 * np.linalg.norm(b)):
        raise ValueError("Tension-only equilibrium residual exceeds tolerance")
    return {
        "tensions_N": tension.tolist(),
        "vertical_load_share": (tension * cosine / np.linalg.norm(b)).tolist(),
        "slack": (tension <= 1e-9 * np.linalg.norm(b)).tolist(),
        "length_errors_m": length_errors.tolist(),
        "elevation_errors_m": elevation_errors.tolist(),
    }


def solve_case(settings, *, length_errors_m=(0, 0, 0, 0),
               elevation_errors_m=(0, 0, 0, 0), cog_xy_m=None):
    """Solve one specified misfit case (not bounded by the envelope tolerances).

    Positive length error means a longer sling. Positive elevation error raises
    the padeye. Both reduce extension at fixed body position. Reference length
    minus fitted body-frame hook distance is included as initial axial misfit.
    """
    points, height, weight, ea, lengths, cogs, kw, *_ = _validate(settings)
    cog = cogs[0] if cog_xy_m is None else _array(cog_xy_m, (2,), "cog_xy_m")
    if kw is not None and not np.allclose(cog, points[:, :2].mean(axis=0), atol=1e-9, rtol=0):
        raise ValueError("Finite payload warp stiffness requires centred CoG")
    misfit, hook = _reference_misfit(points, height, lengths)
    system = _system(points, height, weight, ea, lengths, cog, kw, misfit, hook)
    result = _solve(system, _array(length_errors_m, (4,), "length_errors_m"),
                    _array(elevation_errors_m, (4,), "elevation_errors_m"))
    result["cog_xy_m"] = cog.tolist()
    result.update(_tilt(cog, hook, points[0, 2]))
    return result


def _pair_vertices(first, second, matched):
    # Vertices of a rectangle intersected with |e_first-e_second| <= matched.
    matrix = [[1, 0], [-1, 0], [0, 1], [0, -1]]
    limits = [first, first, second, second]
    if matched is not None:
        matrix += [[1, -1], [-1, 1]]
        limits += [matched, matched]
    matrix, limits = np.array(matrix, dtype=float), np.array(limits)
    vertices = set()
    for i, j in combinations(range(len(limits)), 2):
        rows = matrix[[i, j]]
        if abs(np.linalg.det(rows)) < 1e-12:
            continue
        vertex = np.linalg.solve(rows, limits[[i, j]])
        if np.all(matrix @ vertex <= limits + 1e-12):
            vertices.add(tuple(np.round(vertex, 15)))
    return sorted(vertices)


def _corners(general, elevation, matched):
    pair_a = _pair_vertices(general[0], general[2], matched)
    pair_b = _pair_vertices(general[1], general[3], matched)
    levels = [sorted({-float(value), float(value)}) for value in elevation]
    return [([a[0], b[0], a[1], b[1]], list(z))
            for a, b, z in product(pair_a, pair_b, product(*levels))]


def tolerance_corners(settings):
    """Return every vertex of the bounded length/elevation tolerance polytope.

    Opposite pairs are indices (0,2) and (1,3). The matched-pair value is the
    maximum absolute length difference within each pair, not an added error.
    """
    *_, general, elevation, matched = _validate(settings)
    return _corners(general, elevation, matched)


def calculate_skew(settings):
    """Evaluate all supplied CoG corners crossed with all tolerance vertices.

    SKL is the maximum per-leg tension / that leg's zero-tolerance nominal
    tension at the SAME CoG. A slack nominal leg makes this ratio undefined
    and the envelope is rejected instead of silently omitting that leg.
    """
    points, height, weight, ea, lengths, cogs, kw, general, elevation, matched = _validate(settings)
    corners = _corners(general, elevation, matched)
    cases = []
    nominals = []
    misfit, hook = _reference_misfit(points, height, lengths)
    for cog in cogs:
        system = _system(points, height, weight, ea, lengths, cog, kw, misfit, hook)
        nominal = _solve(system, np.zeros(4), np.zeros(4))
        if any(nominal["slack"]):
            raise ValueError("A nominal sling is slack; a multiplicative SKL is undefined")
        nominals.append(nominal)
        for length_error, elevation_error in corners:
            case = _solve(system, np.array(length_error), np.array(elevation_error))
            case["cog_xy_m"] = cog.tolist()
            case.update(_tilt(cog, hook, points[0, 2]))
            case["nominal_tensions_N"] = nominal["tensions_N"]
            case["effective_skl"] = max(1.0, float(np.max(
                np.array(case["tensions_N"]) / nominal["tensions_N"])))
            cases.append(case)
    governing = max(range(len(cases)), key=lambda i: cases[i]["effective_skl"])
    effective = cases[governing]["effective_skl"]
    minimum = float(settings.get("code_minimum_skl", FOUR_LEG_MINIMUM_SKL))
    design = max(minimum, effective)
    shares = [max(row["vertical_load_share"]) for row in nominals]
    cog_governing = int(np.argmax(shares))
    return {
        "schema_version": 2,
        "method": "finite_rotation_pose_linear_axial_springs_single_self_stress_mode",
        "qa_status": "pending_family_clearance",
        "engineering_use_allowed": False,
        "inputs": settings,
        "effective_skl": effective,
        "code_minimum_skl": minimum,
        "code_minimum_source": settings.get("code_minimum_source", FOUR_LEG_MINIMUM_SOURCE),
        "design_skl": design,
        "max_nominal_vertical_share": max(shares),
        "governing_cog_xy_m": cogs[cog_governing].tolist(),
        "design_leg_tensions_N": (np.max([row["tensions_N"] for row in nominals], axis=0) * design).tolist(),
        "tilt_exceeds_3_deg": any(case["tilt_exceeds_3_deg"] for case in cases),
        "governing_case_index": governing,
        "cases": cases,
    }


def main(argv=None):
    """Write results/skew_summary.json beside an input YAML file."""
    import yaml

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("input", type=Path)
    args = parser.parse_args(argv)
    settings = yaml.safe_load(args.input.read_text(encoding="utf-8"))
    result = calculate_skew(settings["four_point_skew"])
    output = args.input.parent / "results" / "skew_summary.json"
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(json.dumps(result, indent=2, allow_nan=False) + "\n", encoding="utf-8")
    print(f"Effective SKL {result['effective_skl']:.6f}; design SKL {result['design_skl']:.6f}; "
          "family QA pending")


if __name__ == "__main__":
    main()
