# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""FAD assessment-limit growth, inspection planning and LBB evidence screen.

This is a screening interface, not a complete API 579 Part 9 LBB procedure.
No licensed-standard material tables or acceptance factors are reproduced.
"""

from __future__ import annotations

from collections.abc import Callable, Mapping
from dataclasses import dataclass

from digitalmodel.fatigue.threshold_growth import (
    GrowthLaw,
    GrowthPoint,
    GrowthResult,
    integrate_growth,
    validate_scalar,
)

from .assessment.crack_fad import critical_flaw_depth_mm
from .inspection_planning import (
    CrackInspectionPlan,
    plan_crack_inspection,
    validate_crack_policy_inputs,
)


@dataclass(frozen=True)
class CrackLifeAssessment:
    limiting_depth_mm: float
    critical_depth_basis: str
    governing_limit: str
    growth: GrowthResult
    inspection: CrackInspectionPlan


def assess_crack_life(
    *,
    law: GrowthLaw,
    delta_k: float | Callable[[float], float],
    initial_depth_mm: float,
    critical_depth_mm: float,
    wall_thickness_mm: float,
    critical_depth_basis: str,
    steps: int,
    cycles_per_year: float,
    code_max_interval_years: float,
    life_divisor: float,
    policy_basis: str,
) -> CrackLifeAssessment:
    """Stop at the first depth limit: assessment envelope or through-wall.

    The supplied critical depth may be an FAD validity cap, rather than a
    physical fracture boundary. Growth never extrapolates beyond that limit.
    ``delta_k`` receives depth in metres; all depth arguments here are mm.
    The conversion of mm to metres is the SI prefix definition, not a
    material constant. A zero critical depth means no remaining margin.
    """
    if not isinstance(law, GrowthLaw):
        raise ValueError("law must be a GrowthLaw")
    if not callable(delta_k):
        delta_k = validate_scalar(delta_k, "DeltaK")
        if delta_k < 0:
            raise ValueError("DeltaK must be non-negative")
    initial_depth_mm = validate_scalar(initial_depth_mm, "initial_depth_mm")
    wall_thickness_mm = validate_scalar(wall_thickness_mm, "wall_thickness_mm")
    critical_depth_mm = validate_scalar(critical_depth_mm, "critical_depth_mm")
    steps = validate_scalar(steps, "steps", integer=True)
    if steps < 1:
        raise ValueError("steps must be a positive integer")
    if min(initial_depth_mm, wall_thickness_mm) <= 0:
        raise ValueError("initial depth and wall thickness must be finite positive")
    if initial_depth_mm > wall_thickness_mm:
        raise ValueError("initial depth cannot exceed wall thickness")
    if critical_depth_mm < 0:
        raise ValueError("critical depth must be finite non-negative")
    if not isinstance(critical_depth_basis, str) or not critical_depth_basis.strip():
        raise ValueError("critical_depth_basis is required")
    cycles_per_year, code_max_interval_years, life_divisor = validate_crack_policy_inputs(
        cycles_per_year=cycles_per_year, code_max_interval_years=code_max_interval_years,
        life_divisor=life_divisor, policy_basis=policy_basis,
    )
    limit = min(critical_depth_mm, wall_thickness_mm)
    governing = (
        "WALL_PENETRATION" if wall_thickness_mm < critical_depth_mm else
        "ASSESSMENT_LIMIT" if critical_depth_mm < wall_thickness_mm else
        "COINCIDENT_LIMITS"
    )
    if limit == 0:
        growth = GrowthResult(
            "LIMIT_REACHED",
            0.0,
            (GrowthPoint(initial_depth_mm / 1000, 0.0),),
            law.basis,
            0.0,
        )
    else:
        growth = integrate_growth(
            law,
            delta_k,
            initial_m=initial_depth_mm / 1000,
            limit_m=limit / 1000,
            steps=steps,
        )
    plan = plan_crack_inspection(
        growth,
        cycles_per_year=cycles_per_year,
        code_max_interval_years=code_max_interval_years,
        life_divisor=life_divisor,
        policy_basis=policy_basis,
    )
    plan.notes.append(
        f"Governing depth: {governing}; assessment limit {critical_depth_mm:.3g} mm, "
        f"wall {wall_thickness_mm:.3g} mm; first-event depth {limit:.3g} mm."
    )
    return CrackLifeAssessment(limit, critical_depth_basis, governing, growth, plan)


def plan_crack_from_fad(*, fad_inputs: dict, **growth_inputs) -> CrackLifeAssessment:
    """Use the existing critical_flaw_depth_mm envelope without reimplementing it.

    Supply a compatible DeltaK(depth) solution and fixed total surface length
    ``length_mm = 2c`` in fad_inputs; the FAD routine converts this to the
    half-length ``c = length_mm / 2`` and does not model aspect-ratio evolution.
    Its depth/geometry validity cap is retained as an assessment limit.
    Through-wall stability cannot be inferred from this surface-flaw limit.
    Membrane and bending stresses are non-negative tensile components; either
    may be zero but at least one must be positive. Compression and an unloaded
    FAD are outside this adapter's supported loading domain.
    """
    if {"critical_depth_mm", "wall_thickness_mm", "critical_depth_basis"} & growth_inputs.keys():
        raise ValueError("FAD inputs and explicit depth limits are mutually exclusive")
    required = {"length_mm", "t_mm", "sigma_m_mpa", "sigma_y_mpa", "sigma_u_mpa"}
    nullable = {"k_mat", "cvn_joules"}
    allowed = required | nullable | {"sigma_b_mpa", "e_mpa", "tol_mm", "max_depth_fraction"}
    if not isinstance(fad_inputs, Mapping):
        raise ValueError("fad_inputs must be a mapping")
    if required - fad_inputs.keys() or fad_inputs.keys() - allowed:
        raise ValueError("FAD inputs require geometry, stress and strengths; unknown keys are invalid")
    if any(value is None and name not in nullable for name, value in fad_inputs.items()):
        raise ValueError("only k_mat and cvn_joules may be null FAD inputs")
    fad_inputs = {
        name: None if value is None else validate_scalar(value, f"FAD input {name}")
        for name, value in fad_inputs.items()
    }
    membrane = fad_inputs["sigma_m_mpa"]
    bending = fad_inputs.get("sigma_b_mpa", 0.0)
    if membrane < 0 or bending < 0 or membrane + bending <= 0:
        raise ValueError("FAD adapter requires tensile stress components and a positive applied load")
    positive = (
        "length_mm",
        "t_mm",
        "sigma_y_mpa",
        "sigma_u_mpa",
        "k_mat",
        "cvn_joules",
        "e_mpa",
        "tol_mm",
    )
    for name in positive:
        if fad_inputs.get(name) is not None and fad_inputs[name] <= 0:
            raise ValueError(f"FAD input {name} must be positive")
    if fad_inputs["sigma_u_mpa"] <= fad_inputs["sigma_y_mpa"]:
        raise ValueError("FAD ultimate strength must exceed yield strength")
    if (
        "max_depth_fraction" in fad_inputs
        and not 0 < fad_inputs["max_depth_fraction"] < 1
    ):
        raise ValueError("FAD max_depth_fraction must lie strictly between 0 and 1")
    depth = critical_flaw_depth_mm(**fad_inputs)
    return assess_crack_life(
        critical_depth_mm=depth,
        wall_thickness_mm=fad_inputs["t_mm"],
        critical_depth_basis=(
            "critical_flaw_depth_mm surface FAD envelope, including its validity cap; "
            "not a demonstrated through-wall fracture length"
        ),
        **growth_inputs,
    )


@dataclass(frozen=True)
class LeakBeforeBreakScreen:
    status: str
    geometry_screen: str
    missing_evidence: tuple[str, ...]
    basis: str | None


def screen_leak_before_break(
    *,
    through_wall: bool,
    detectable_length_mm: float | None = None,
    critical_length_mm: float | None = None,
    length_margin: float | None = None,
    basis: str | None = None,
) -> LeakBeforeBreakScreen:
    """Check caller-specified full-length ordering, without an LBB pass claim.

    ``length_margin * detectable_length < critical_length`` is only a
    user-supplied geometric prerequisite. No code value is assumed. Lengths
    must both be full through-wall lengths evaluated at consistent loads.
    A surface FAD depth is not a valid critical_length input.
    API 579-1/ASME FFS-1 (2016), Part 9, LBB procedure is the intended
    qualification route; the procedure has not been established here.
    """
    if not isinstance(through_wall, bool):
        raise ValueError("through_wall must be boolean")
    if basis is not None and not isinstance(basis, str):
        raise ValueError("LBB basis must be text when supplied")
    supplied = tuple(
        None if value is None else validate_scalar(value, name)
        for name, value in (
            ("detectable_length_mm", detectable_length_mm),
            ("critical_length_mm", critical_length_mm),
            ("length_margin", length_margin),
        )
    )
    detectable_length_mm, critical_length_mm, length_margin = supplied
    for value in supplied:
        if value is not None and value <= 0:
            raise ValueError("LBB lengths and margin must be finite positive")
    if length_margin is not None and length_margin < 1:
        raise ValueError("length_margin must be >= 1")
    missing = [
        "leak-rate and detection qualification",
        "through-wall stability under governing loads and transients",
        "applicable LBB procedure and margins",
    ]
    if not through_wall:
        missing.append("through-wall flaw characterization")
    if any(v is None for v in supplied) or not basis or not basis.strip():
        missing.append("consistent through-wall lengths, margin and basis")
        geometry = "NOT_EVALUATED"
    elif not through_wall:
        geometry = "NOT_EVALUATED"
    else:
        geometry = (
            "pass"
            if detectable_length_mm * length_margin < critical_length_mm
            else "fail"
        )
    return LeakBeforeBreakScreen(
        "fail" if geometry == "fail" else "NOT_EVALUATED",
        geometry,
        tuple(missing),
        basis,
    )
