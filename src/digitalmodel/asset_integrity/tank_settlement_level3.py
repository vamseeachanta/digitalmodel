# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline tank Level 3 preparation, with no solver or code acceptance claims.

SI throughout. Shell radius is the mid-surface radius; z=0 is the bottom.
Displacements are positive upward. Harmonics include rigid mean and tilt;
this module does not reduce a survey or remove its planar component.
"""
from __future__ import annotations

import argparse
import copy
import hashlib
import json
import math
from pathlib import Path

SOURCES = {
    "fourier": "https://dlmf.nist.gov/1.8#E1",
    "hydrostatic": (
        "https://openstax.org/books/university-physics-volume-1/"
        "pages/14-1-fluids-density-and-pressure"
    ),
}

# Clause pointers come from the issue/program note, not licensed clause text.
# No code coefficient or acceptance limit is supplied by this module.
REQUIREMENTS = {
    "strain_limit": ("B1.3", "primary", [
        "Converged local equivalent plastic strain and stress triaxiality history",
        "Qualified material limiting triaxial strain and sourced acceptance rule",
    ]),
    "buckling": ("B1.4.1", "primary", [
        "Selected Type 1/2 bifurcation or Type 3 collapse method",
        "Eigenvalues with sourced capacity reduction factors, or explicit imperfections",
        "Mesh convergence, load path and applicable buckling acceptance criteria",
    ]),
    "plastic_collapse": ("B1.2.4", "secondary", [
        "Converged collapse load path and termination evidence",
        "Sourced Table B1.4 load combinations and load factors",
        "Displacement-controlled settlement interpretation",
    ]),
    "weld_strain": ("B1.2.4.3 b", "service", [
        "Shell-to-bottom weld geometry, strain extraction and mesh convergence",
        "Sourced weld service strain limit and strain measure",
    ]),
}


def _fields(block, required, optional=()):
    if not isinstance(block, dict):
        raise ValueError("Expected an input object")
    missing = set(required) - block.keys()
    unknown = block.keys() - set(required) - set(optional)
    if missing or unknown:
        raise ValueError(f"Missing keys {sorted(missing)}; unknown keys {sorted(unknown)}")


def _number(value, name, *, minimum=None, strict=False):
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(f"{name} must be a finite number")
    try:
        result = float(value)
    except OverflowError:
        raise ValueError(f"{name} must be finite") from None
    if not math.isfinite(result):
        raise ValueError(f"{name} must be finite")
    if minimum is not None and (result <= minimum if strict else result < minimum):
        raise ValueError(f"{name} is outside its permitted range")
    return result


def _positive(value, name):
    return _number(value, name, minimum=0, strict=True)


def _text(value, name):
    if not isinstance(value, str) or not value.strip():
        raise ValueError(f"{name} must be a nonempty reference")


def _sequence(value, name, *, nonempty=False):
    if not isinstance(value, list) or (nonempty and not value):
        raise ValueError(f"{name} must be {'a nonempty' if nonempty else 'a'} list")


def _plate(block, required, optional=()):
    _fields(block, (*required, "material_ref"), optional)
    for key in required:
        _positive(block[key], key)
    _text(block["material_ref"], "material_ref")


def _geometry(g):
    _fields(g, ("radius_m", "courses", "bottom", "annular_ring", "top_angle",
                "wind_girders"), ("roof",))
    radius = _positive(g["radius_m"], "radius_m")
    _sequence(g["courses"], "courses", nonempty=True)
    height = 0.0
    for course in g["courses"]:
        _plate(course, ("height_m", "thickness_m"))
        height += course["height_m"]
        if course["thickness_m"] >= radius:
            raise ValueError("Course thickness must be below the shell radius")
    _plate(g["bottom"], ("thickness_m",))
    _plate(g["annular_ring"], ("width_m", "thickness_m"))
    if g["annular_ring"]["width_m"] >= radius:
        raise ValueError("Annular width must be smaller than the radius")
    _plate(g["top_angle"], ("area_m2",))
    _sequence(g["wind_girders"], "wind_girders")
    for girder in g["wind_girders"]:
        _plate(girder, ("area_m2",), ("elevation_m",))
        elevation = _number(girder.get("elevation_m"), "elevation_m", minimum=0)
        if elevation > height:
            raise ValueError("Girder elevation exceeds shell height")
    if "roof" in g:
        _plate(g["roof"], ("thickness_m",), ("type",))
        if g["roof"].get("type") != "flat":
            raise ValueError("Only explicit flat roof preparation is supported")
    return radius, height


def _harmonics(harmonics):
    _sequence(harmonics, "harmonics")
    orders = set()
    for harmonic in harmonics:
        _fields(harmonic, ("order", "cos_m", "sin_m"))
        order = harmonic["order"]
        if type(order) is not int or order < 1 or order in orders:
            raise ValueError("Harmonic orders must be distinct positive integers")
        _positive(order, "harmonic order")
        orders.add(order)
        _number(harmonic["cos_m"], "cos_m")
        _number(harmonic["sin_m"], "sin_m")


def settlement_displacement(angle_deg, mean_m, harmonics):
    """Synthesize full imposed uz; NIST DLMF (2010), equation 1.8.1.

    mean_m is a0/2, not a0. Degree-to-radian conversion is exact geometry.
    """
    angle = _number(angle_deg, "angle_deg")
    if not 0 <= angle < 360:  # Exact definition of a full turn in degrees.
        raise ValueError("angle_deg must be in [0, 360)")
    mean = _number(mean_m, "mean_m")
    _harmonics(harmonics)
    theta = math.radians(angle)
    try:
        displacement = mean + math.fsum(
            h["cos_m"] * math.cos(h["order"] * theta)
            + h["sin_m"] * math.sin(h["order"] * theta) for h in harmonics)
    except OverflowError:
        raise ValueError("Displacement exceeds the finite arithmetic range") from None
    return _number(displacement, "uz_m")


def pressure_at_elevation(z_m, fill_height_m, density_kg_m3, gravity_m_s2):
    """Gauge pressure rho*g*depth; OpenStax (2016), 14.1, equation 14.4.

    Constant density and vented tank; gravity and density are explicit inputs.
    """
    z = _number(z_m, "z_m", minimum=0)
    fill = _number(fill_height_m, "fill_height_m", minimum=0)
    rho = _positive(density_kg_m3, "density_kg_m3")
    gravity = _positive(gravity_m_s2, "gravity_m_s2")
    return _number(rho * gravity * max(0, fill - z), "pressure_pa", minimum=0)


def _validate(settings):
    _fields(settings, ("basis", "geometry", "loads", "settlement", "mesh"))
    _text(settings["basis"], "basis")
    radius, height = _geometry(settings["geometry"])
    loads = settings["loads"]
    _fields(loads, ("product_density_kg_m3", "steel_density_kg_m3", "gravity_m_s2",
                    "fill_height_m"), ("wind",))
    for key in ("product_density_kg_m3", "steel_density_kg_m3", "gravity_m_s2"):
        _positive(loads[key], key)
    fill = _number(loads["fill_height_m"], "fill_height_m", minimum=0)
    if fill > height:
        raise ValueError("Fill height exceeds shell height")
    if "wind" in loads:
        _fields(loads["wind"], ("pressure_pa", "source_ref"))
        _number(loads["wind"]["pressure_pa"], "pressure_pa", minimum=0)
        _text(loads["wind"]["source_ref"], "source_ref")
    mesh = settings["mesh"]
    _fields(mesh, ("target_size_m", "junction_size_m"))
    target = _positive(mesh["target_size_m"], "target_size_m")
    if _positive(mesh["junction_size_m"], "junction_size_m") > target:
        raise ValueError("Junction mesh size must not exceed target size")
    survey = settings["settlement"]
    _fields(survey, ("source_ref", "mean_m", "harmonics", "angles_deg"))
    _text(survey["source_ref"], "source_ref")
    _number(survey["mean_m"], "mean_m")
    _harmonics(survey["harmonics"])
    _sequence(survey["angles_deg"], "angles_deg", nonempty=True)
    angles = [_number(a, "angle_deg") for a in survey["angles_deg"]]
    if len(set(angles)) != len(angles) or any(not 0 <= a < 360 for a in angles):
        raise ValueError("Angles must be distinct and in [0, 360)")
    return radius, height


def _model_and_weight(settings, radius):
    g, loads = settings["geometry"], settings["loads"]
    z, volume, courses = 0.0, 0.0, []
    for course in g["courses"]:
        top = z + course["height_m"]
        def pressure(elevation):
            return pressure_at_elevation(
                elevation, loads["fill_height_m"], loads["product_density_kg_m3"],
                loads["gravity_m_s2"])
        courses.append({**course, "z_bottom_m": z, "z_top_m": top,
                        "pressure_bottom_pa": pressure(z), "pressure_top_pa": pressure(top)})
        # Derived mid-surface geometry: circumference*height*thickness.
        volume += math.tau * radius * course["height_m"] * course["thickness_m"]
        z = top
    inner = radius - g["annular_ring"]["width_m"]
    # Inner disk and annulus partition the bottom; no overlapping plate weight.
    volume += math.pi * inner * inner * g["bottom"]["thickness_m"]
    width = g["annular_ring"]["width_m"]
    volume += math.pi * width * (2 * radius - width) * g["annular_ring"]["thickness_m"]
    area = g["top_angle"]["area_m2"] + sum(x["area_m2"] for x in g["wind_girders"])
    volume += math.tau * radius * area
    if "roof" in g:
        volume += math.pi * radius * radius * g["roof"]["thickness_m"]
    weight = _positive(volume * loads["steel_density_kg_m3"] * loads["gravity_m_s2"],
                       "self_weight_n")
    model = {**copy.deepcopy(g), "courses": courses,
             "bottom_inner_radius_m": inner,
             "mesh": {**settings["mesh"], "status": "NOT_EVALUATED",
                      "reason": "Gmsh mesh, junction topology and convergence are not generated"}}
    return model, weight


def _checks():
    return {name: {"status": "NOT_EVALUATED", "role": role,
                   "reference_2007": f"API 579-1/ASME FFS-1 (2007), {clause}",
                   "reference_current": "API 579-1/ASME FFS-1 (2016/2021), Annex 2D",
                   "crosswalk_status": "subclause_not_verified",
                   "required_evidence": evidence}
            for name, (clause, role, evidence) in REQUIREMENTS.items()}


def _decision():
    from digitalmodel.asset_integrity.assessment.ffs_decision import decide

    # Non-finite margin selects ESCALATE before any pressure-specific bands.
    # No numerical acceptance limit or remaining life is inferred.
    decision = decide(float("nan"), float("nan"), float("nan"), "tank").to_dict()
    for key in ("margin", "margin_allowable", "remaining_life_yr", "rsf", "rsf_a"):
        decision[key] = None
    decision["governing_criterion"] = (
        "Level 3 tank settlement: all failure checks NOT_EVALUATED; "
        "mesh, material criteria, converged solver results and published-case qualification "
        "are required before a fitness conclusion. Settlement is displacement-controlled; "
        "plastic collapse is secondary and cannot replace strain or buckling checks.")
    return decision


def prepare_assessment(settings):
    """Return a solver-independent handoff; never issue a fitness acceptance."""
    radius, _height = _validate(settings)
    model, weight = _model_and_weight(settings, radius)
    loads, survey = settings["loads"], settings["settlement"]
    result = {
        "workflow_id": "api579-tank-settlement-level3", "status": "handoff_required",
        "qualification": "preparation_hand_calculations_only",
        "input_sha256": hashlib.sha256(json.dumps(
            settings, sort_keys=True, separators=(",", ":"), allow_nan=False).encode()).hexdigest(),
        "input": copy.deepcopy(settings), "model_recipe": model,
        "loads": {**copy.deepcopy(loads), "self_weight_n": weight,
                  "bottom_pressure_pa": pressure_at_elevation(
                      0, loads["fill_height_m"], loads["product_density_kg_m3"],
                      loads["gravity_m_s2"]),
                  "bottom_edge_samples": [
                      {"angle_deg": angle, "uz_m": settlement_displacement(
                          angle, survey["mean_m"], survey["harmonics"])}
                      for angle in survey["angles_deg"]]},
        "checks": _checks(), "decision": _decision(), "sources": SOURCES.copy(),
        "handoff_required": [
            "Gmsh connected shell/bottom/annulus mesh with junction refinement",
            "Qualified true stress-strain material data and local failure criteria",
            "Nonlinear shell solver adapter, supports/contact and load sequence",
            "Published tank-settlement FEA benchmark with stated tolerance",
            "Verified current-edition subclause crosswalk and code factors",
        ],
    }
    if "wind" in result["loads"]:
        result["loads"]["wind"]["status"] = "NOT_EVALUATED"
    result["report_html"] = _report(result)
    return result


def _report(result):
    from digitalmodel.asset_integrity.assessment.ffs_report import FFSReport

    sections = {
        "Revision history": {"1, 2026-10-20": "Initial preparation and explicit missing evidence"},
        "Input basis": {"basis": result["input"]["basis"],
                        "input_sha256": result["input_sha256"]},
        "Component inventory (SI)": result["model_recipe"],
        "Preparation (SI)": {"self_weight_n": result["loads"]["self_weight_n"],
                              "bottom_pressure_pa": result["loads"]["bottom_pressure_pa"]},
        "Settlement samples (degrees, metres)": {
            str(sample["angle_deg"]): sample["uz_m"]
            for sample in result["loads"]["bottom_edge_samples"]},
        **result["checks"],
        "Handoff requirements": {"required": result["handoff_required"]},
        "Sources": SOURCES,
    }
    return FFSReport.generate_screening_html(
        "TANK", "Tank settlement Level 3 preparation", result["decision"], sections, [
            "No solver is discovered or executed, even when available; this is a preparation package.",
            "Mesh sizes are intent only; sampled displacements are not FE boundary conditions.",
            "Full harmonics retain mean and planar tilt; survey reduction is not performed.",
            "Constant-density hydrostatic gauge pressure assumes a vented tank.",
            "Weight uses thin mid-surfaces, a flat optional roof and uniform supplied steel density.",
            "Wind, support contact, load sequence and junction compatibility remain NOT_EVALUATED.",
            "Material references are unverified identifiers, not a material-library qualification.",
            "2007 clause pointers are issue requirements; current subclause equivalence is unverified.",
        ])


def main(argv=None):
    """Write an offline JSON handoff and HTML report to an explicit directory."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("input", type=Path)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args(argv)
    result = prepare_assessment(json.loads(args.input.read_text(encoding="utf-8")))
    args.output_dir.mkdir(parents=True, exist_ok=True)
    (args.output_dir / "handoff.json").write_text(
        json.dumps(result, indent=2, allow_nan=False) + "\n", encoding="utf-8")
    (args.output_dir / "report.html").write_text(result["report_html"], encoding="utf-8")
    print(result["status"])
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
