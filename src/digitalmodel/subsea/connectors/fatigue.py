# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Elastic signed-radius fatigue hook for connector and jumper load histories.

Stress conversion uses the existing bending model at one fixed hot spot.
This homogeneous-section approximation does not qualify flexible-pipe layers.
No API 17R/17J/17B fatigue coefficients or geometry-derived SCFs are assumed.
"""

from __future__ import annotations

import math
from dataclasses import dataclass

from digitalmodel.fatigue.assessment import (
    AssessmentInput, AssessmentResult, CurveName, assess_fatigue,
)

from .bending_analysis import bending_from_radius
from .jumper_catalog import JumperSpec
from .section_properties import PipeSection


@dataclass(frozen=True)
class JumperFatigueResult:
    """Converted stress history in MPa and native fatigue assessment."""

    stress_history_mpa: list[float]
    assessment: AssessmentResult


def _stress_history(
    section: PipeSection,
    radii: list[float],
    youngs_modulus: float,
    scf: float,
) -> list[float]:
    """Use existing E-times-curvature bending with explicit sign and SCF.

    The bending equation and its reference are owned by ``bending_analysis``.
    Pa-to-MPa conversion uses the SI prefix, not a standard-derived constant.
    """
    if len(radii) < 2:
        raise ValueError("radius history requires at least two samples")
    if any(not math.isfinite(radius) or radius == 0 for radius in radii):
        raise ValueError("signed bend radii must be finite and nonzero")
    if not math.isfinite(youngs_modulus) or youngs_modulus <= 0:
        raise ValueError("Young's modulus must be finite and positive")
    if not math.isfinite(scf) or scf < 1:
        raise ValueError("stress concentration factor must be finite and >= 1")
    if not all(math.isfinite(value) for value in (
        section.outer_diameter, section.wall_thickness,
    )):
        raise ValueError("section dimensions must be finite")
    return [math.copysign(
        bending_from_radius(section, abs(radius), youngs_modulus).bending_stress,
        radius,
    ) * scf / 1e6 for radius in radii]


def assess_jumper_fatigue(
    spec: JumperSpec,
    section: PipeSection,
    radius_history_m: list[float],
    *,
    youngs_modulus_pa: float,
    block_years: float,
    design_years: float,
    design_fatigue_factor: float = 1.0,
    stress_concentration_factor: float = 1.0,
    sn_curve_name: CurveName = "D",
    environment: str = "air",
) -> JumperFatigueResult:
    """Connect a catalogue fatigue trigger to native rainflow and Miner damage.

    Args:
        spec: Dynamic entry or entry declaring a fatigue governing check.
        section: Homogeneous circular section in metres.
        radius_history_m: Ordered, signed bend radii at one fixed hot spot.
        youngs_modulus_pa: Explicit elastic modulus in Pa.
        block_years: Duration represented by the history.
        design_years: Required design exposure.
        design_fatigue_factor: Supplied design factor, at least one.
        stress_concentration_factor: Supplied hot-spot SCF, at least one.
        sn_curve_name: Qualified library curve class.
        environment: Library environment matching the load basis.

    Returns:
        Converted stresses and the native criterion-bound assessment.

    Raises:
        ValueError: If catalogue, geometry, material or history inputs are invalid.
    """
    if not spec.dynamic and "fatigue" not in spec.governing_checks:
        raise ValueError("fatigue requires a dynamic entry or a fatigue governing check")
    stresses = _stress_history(section, radius_history_m, youngs_modulus_pa,
                               stress_concentration_factor)
    inp = AssessmentInput.model_validate({
        "sn_curve_name": sn_curve_name, "environment": environment,
        "block_years": block_years, "design_years": design_years,
        "design_fatigue_factor": design_fatigue_factor,
        "load_cases": [{"name": "jumper-bending", "kind": "history",
                        "stress_history_mpa": stresses}],
    })
    return JumperFatigueResult(stresses, assess_fatigue(inp))
