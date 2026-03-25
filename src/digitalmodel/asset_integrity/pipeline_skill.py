"""Agent-callable pipeline integrity skill for digitalmodel.

Provides a zero-config, chained assessment:
  Step 1 — Wall thickness / hoop stress check (ASME B31.4 / B31.8 style)
  Step 2 — Fitness-For-Service (FFS) corrosion assessment per API 579-1
            using the standalone rsf_calculations helpers.

Units are internally SI (MPa, mm) converted to inches/psi at the FFS boundary.
"""
from __future__ import annotations

from dataclasses import dataclass, field

from .rsf_calculations import check_ffs_level1

SKILL_NAME = "pipeline_integrity"

# ---------------------------------------------------------------------------
# Material grade table — SMYS in MPa (API 5L PSL2 / ISO 3183)
# ---------------------------------------------------------------------------
SMYS_TABLE: dict[str, float] = {
    "X52": 359.0,
    "X60": 414.0,
    "X65": 448.0,
    "X70": 483.0,
    "X80": 552.0,
    "X100": 690.0,
}

# Hoop stress design factor (ASME B31.4 / B31.8 onshore buried pipelines)
HOOP_DESIGN_FACTOR = 0.72

# Unit conversion constants
_MPA_TO_PSI = 145.0378
_MM_TO_IN = 1.0 / 25.4


# ---------------------------------------------------------------------------
# Public dataclasses
# ---------------------------------------------------------------------------


@dataclass
class PipelineInput:
    """Input parameters for a pipeline integrity assessment."""

    outer_diameter_mm: float
    wall_thickness_mm: float
    design_pressure_mpa: float
    material_grade: str = "X65"
    corrosion_depth_mm: float = 0.0
    corrosion_length_mm: float = 0.0

    def __post_init__(self) -> None:
        _validate_positive("outer_diameter_mm", self.outer_diameter_mm)
        _validate_positive("wall_thickness_mm", self.wall_thickness_mm)
        _validate_positive("design_pressure_mpa", self.design_pressure_mpa)
        _validate_non_negative("corrosion_depth_mm", self.corrosion_depth_mm)
        _validate_non_negative("corrosion_length_mm", self.corrosion_length_mm)
        if self.material_grade not in SMYS_TABLE:
            valid = ", ".join(sorted(SMYS_TABLE))
            raise ValueError(
                f"Unknown material_grade {self.material_grade!r}. "
                f"Valid options: {valid}"
            )


@dataclass
class PipelineIntegrityResult:
    """Result from a pipeline integrity assessment."""

    wall_thickness_ok: bool
    utilisation_ratio: float
    ffs_verdict: str  # "accept" | "repair" | "replace" | "not_applicable"
    ffs_safe_pressure_mpa: float
    summary: dict = field(default_factory=dict)
    source: str = f"skill:{SKILL_NAME}"


# ---------------------------------------------------------------------------
# Main entry point
# ---------------------------------------------------------------------------


def pipeline_integrity(inp: PipelineInput) -> PipelineIntegrityResult:
    """Run a chained pipeline integrity assessment.

    Step 1: Hoop stress utilisation check.
    Step 2: API 579-1 Level 1 FFS assessment if corrosion is present.

    Args:
        inp: PipelineInput with pipe geometry, material, and corrosion data.

    Returns:
        PipelineIntegrityResult with full assessment outputs.
    """
    smys_mpa = SMYS_TABLE[inp.material_grade]

    # ------------------------------------------------------------------
    # Step 1 — Wall thickness / hoop stress check
    # ------------------------------------------------------------------
    hoop_stress_mpa = _hoop_stress_mpa(
        inp.design_pressure_mpa, inp.outer_diameter_mm, inp.wall_thickness_mm
    )
    allowable_stress_mpa = smys_mpa * HOOP_DESIGN_FACTOR
    utilisation_ratio = hoop_stress_mpa / allowable_stress_mpa
    wall_thickness_ok = utilisation_ratio <= 1.0

    # ------------------------------------------------------------------
    # Step 2 — FFS / corrosion assessment
    # ------------------------------------------------------------------
    has_corrosion = inp.corrosion_depth_mm > 0.0

    if not has_corrosion:
        ffs_verdict = "not_applicable"
        ffs_safe_pressure_mpa = inp.design_pressure_mpa
    else:
        ffs_verdict, ffs_safe_pressure_mpa = _ffs_assessment(inp, smys_mpa)

    summary = {
        "material_grade": inp.material_grade,
        "smys_mpa": smys_mpa,
        "hoop_stress_mpa": round(hoop_stress_mpa, 3),
        "allowable_stress_mpa": round(allowable_stress_mpa, 3),
        "utilisation_ratio": round(utilisation_ratio, 4),
        "wall_thickness_ok": wall_thickness_ok,
        "ffs_verdict": ffs_verdict,
        "ffs_safe_pressure_mpa": round(ffs_safe_pressure_mpa, 3),
        "has_corrosion": has_corrosion,
    }

    return PipelineIntegrityResult(
        wall_thickness_ok=wall_thickness_ok,
        utilisation_ratio=round(utilisation_ratio, 6),
        ffs_verdict=ffs_verdict,
        ffs_safe_pressure_mpa=round(ffs_safe_pressure_mpa, 3),
        summary=summary,
    )


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------


def _hoop_stress_mpa(pressure_mpa: float, od_mm: float, t_mm: float) -> float:
    """Thin-wall hoop stress: sigma = P * D / (2 * t)."""
    return pressure_mpa * od_mm / (2.0 * t_mm)


def _ffs_assessment(
    inp: PipelineInput, smys_mpa: float
) -> tuple[str, float]:
    """API 579-1 Level 1 FFS check adapted from rsf_calculations.check_ffs_level1.

    Converts SI inputs to imperial for the existing helper, then converts
    the reduced MAWP back to MPa.

    Returns:
        (verdict, safe_pressure_mpa) where verdict is one of:
        "accept", "repair", "replace".
    """
    t_nominal_mm = inp.wall_thickness_mm
    t_measured_mm = inp.wall_thickness_mm - inp.corrosion_depth_mm

    # Required minimum thickness for design pressure (Barlow rearranged):
    # t_req = P * D / (2 * SMYS * F)
    t_required_mm = (
        inp.design_pressure_mpa
        * inp.outer_diameter_mm
        / (2.0 * smys_mpa * HOOP_DESIGN_FACTOR)
    )

    # Convert to imperial for rsf_calculations helper
    t_measured_in = t_measured_mm * _MM_TO_IN
    t_required_in = t_required_mm * _MM_TO_IN
    t_nominal_in = t_nominal_mm * _MM_TO_IN
    od_in = inp.outer_diameter_mm * _MM_TO_IN
    design_pressure_psi = inp.design_pressure_mpa * _MPA_TO_PSI
    smys_psi = smys_mpa * _MPA_TO_PSI

    # Guard against corrosion eating through wall
    if t_measured_mm <= 0.0:
        return "replace", 0.0

    result = check_ffs_level1(
        t_measured=t_measured_in,
        t_required=t_required_in,
        t_nominal=t_nominal_in,
        design_pressure_psi=design_pressure_psi,
        smys_psi=smys_psi,
        outer_diameter_in=od_in,
    )

    # Reduced MAWP in MPa
    mawp_psi = result["mawp_psi"]
    mawp_mpa = mawp_psi / _MPA_TO_PSI

    # Determine FFS verdict
    if result["acceptable"]:
        verdict = "accept"
        safe_pressure_mpa = inp.design_pressure_mpa
    else:
        # Pipe can operate at reduced pressure only
        metal_loss_fraction = result["metal_loss_fraction"]
        if metal_loss_fraction >= 0.80:
            verdict = "replace"
        else:
            verdict = "repair"
        safe_pressure_mpa = mawp_mpa

    return verdict, safe_pressure_mpa


def _validate_positive(name: str, value: float) -> None:
    if value <= 0.0:
        raise ValueError(f"{name} must be positive, got {value}")


def _validate_non_negative(name: str, value: float) -> None:
    if value < 0.0:
        raise ValueError(f"{name} must be non-negative, got {value}")
