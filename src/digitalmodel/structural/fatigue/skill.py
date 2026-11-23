"""
Fatigue Assessment Skill
========================

Agent-callable skill wrapper for fatigue assessment using the Digital Model
fatigue analysis engine. Accepts stress ranges and cycle counts (histogram
format) and returns damage, life, and utilisation metrics.

Zero-config: works fully offline with no API keys or data files.

Usage::

    from digitalmodel.structural.fatigue.skill import (
        fatigue_assessment, FatigueAssessmentInput, SKILL_NAME
    )

    inp = FatigueAssessmentInput(
        stress_ranges=[50.0, 80.0, 120.0],
        cycle_counts=[1e6, 5e4, 1e3],
    )
    result = fatigue_assessment(inp)
    print(result.utilisation_ratio)  # Miner's sum D
    print(result.life_years)         # 1 / D years (block-normalised)

Author: Digital Model Team
Version: 1.0.0
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from typing import List

import numpy as np
import pandas as pd

from .sn_curves import get_dnv_curve, get_api_curve, get_bs_curve
from .damage_accumulation import LinearDamageAccumulation

# ---------------------------------------------------------------------------
# Skill identity
# ---------------------------------------------------------------------------

SKILL_NAME = "fatigue_assessment"

# Valid curve classes per standard (used for validation)
_DNV_CLASSES = {"B1", "B2", "C", "C1", "C2", "D", "E", "F", "F1", "F3", "G", "W1", "W2", "W3"}
_API_CLASSES = {"X", "X_prime", "Y", "S-N1", "S-N2"}
_BS_CLASSES = {"B", "C", "D", "E", "F", "F2", "G", "W"}

_VALID_CLASSES: dict[str, set] = {
    "DNV": _DNV_CLASSES,
    "API": _API_CLASSES,
    "BS": _BS_CLASSES,
}

_VALID_STANDARDS = set(_VALID_CLASSES.keys())


# ---------------------------------------------------------------------------
# Input / output dataclasses
# ---------------------------------------------------------------------------

@dataclass
class FatigueAssessmentInput:
    """Input to the fatigue assessment skill.

    Parameters
    ----------
    stress_ranges : list of float
        Stress ranges in MPa — one entry per histogram bin.
    cycle_counts : list of float
        Number of applied cycles for each stress range bin.
        Must have the same length as *stress_ranges*.
    sn_curve_name : str, default "D"
        S-N curve class identifier.  DNV classes: B1, B2, C, C1, C2, D, E,
        F, F1, F3, G, W1, W2, W3.  API: B, C, D, E, F, F2, G, W.
        BS: B, C, C1, D, E, E1.
    design_code : str, default "DNV"
        Standard to use: "DNV", "API", or "BS".
    output_formats : list of str, default ["summary"]
        Requested output sections.  Currently only ``"summary"`` is used;
        reserved for future extension.
    """

    stress_ranges: List[float]
    cycle_counts: List[float]
    sn_curve_name: str = "D"
    design_code: str = "DNV"
    output_formats: List[str] = field(default_factory=lambda: ["summary"])


@dataclass
class FatigueAssessmentResult:
    """Result returned by the fatigue assessment skill.

    Parameters
    ----------
    utilisation_ratio : float
        Miner's cumulative damage sum D = Σ(nᵢ/Nᵢ).  Failure when D ≥ 1.0.
    life_years : float
        Estimated fatigue life in years, defined as 1 / D (block-normalised).
        Returns ``inf`` when D = 0.
    governing_load_case : str
        Stress range [MPa] that contributes the most damage, as a label string.
    sn_curve_used : str
        Description of the S-N curve applied (e.g. "DNV-D").
    damage_per_block : list of float
        Damage increment for each histogram bin (same order as input).
    summary : dict
        Summary statistics: total_damage, life_years, utilisation_ratio,
        sn_curve, total_cycles, max_stress_range_mpa, governing_load_case.
    source : str
        Always ``"skill:fatigue_assessment"``.
    """

    utilisation_ratio: float
    life_years: float
    governing_load_case: str
    sn_curve_used: str
    damage_per_block: List[float]
    summary: dict
    source: str = "skill:fatigue_assessment"


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _validate_input(inp: FatigueAssessmentInput) -> None:
    """Raise ValueError for invalid input before any computation."""
    if not inp.stress_ranges:
        raise ValueError("stress_ranges must not be empty")

    if len(inp.stress_ranges) != len(inp.cycle_counts):
        raise ValueError(
            f"stress_ranges and cycle_counts must have the same length; "
            f"got {len(inp.stress_ranges)} and {len(inp.cycle_counts)}"
        )

    standard = inp.design_code.upper()
    if standard not in _VALID_STANDARDS:
        raise ValueError(
            f"Unknown design_code '{inp.design_code}'. "
            f"Supported: {sorted(_VALID_STANDARDS)}"
        )

    curve_class = inp.sn_curve_name.upper()
    valid_for_standard = _VALID_CLASSES[standard]
    if curve_class not in valid_for_standard:
        raise ValueError(
            f"Unknown sn_curve_name '{inp.sn_curve_name}' for standard '{standard}'. "
            f"Valid classes: {sorted(valid_for_standard)}"
        )


def _get_sn_curve(design_code: str, curve_class: str):
    """Retrieve the appropriate S-N curve object."""
    standard = design_code.upper()
    cls = curve_class.upper()
    if standard == "DNV":
        return get_dnv_curve(cls)
    elif standard == "API":
        return get_api_curve(cls)
    elif standard == "BS":
        return get_bs_curve(cls)
    # Should never reach here after validation
    raise ValueError(f"Unsupported standard: {standard}")


def _build_cycles_dataframe(
    stress_ranges: List[float],
    cycle_counts: List[float],
) -> pd.DataFrame:
    """Build a cycles DataFrame suitable for LinearDamageAccumulation."""
    return pd.DataFrame({
        "range": [float(s) for s in stress_ranges],
        "mean": [0.0] * len(stress_ranges),
        "count": [float(n) for n in cycle_counts],
    })


# ---------------------------------------------------------------------------
# Main skill entry point
# ---------------------------------------------------------------------------

def fatigue_assessment(inp: FatigueAssessmentInput) -> FatigueAssessmentResult:
    """Run a fatigue assessment from a stress-range histogram.

    This is the single entry point for agent-callable fatigue evaluation.
    It applies Palmgren-Miner linear damage accumulation against the requested
    S-N curve and returns a structured result.

    Parameters
    ----------
    inp : FatigueAssessmentInput
        Validated input describing the stress histogram and curve selection.

    Returns
    -------
    FatigueAssessmentResult
        Damage, life, and summary statistics.

    Raises
    ------
    ValueError
        If *stress_ranges* is empty, lengths mismatch, or the design code /
        curve class is unrecognised.
    """
    _validate_input(inp)

    sn_curve = _get_sn_curve(inp.design_code, inp.sn_curve_name)
    sn_curve_label = f"{inp.design_code.upper()}-{inp.sn_curve_name.upper()}"

    cycles_df = _build_cycles_dataframe(inp.stress_ranges, inp.cycle_counts)

    calc = LinearDamageAccumulation()
    damage_result = calc.calculate_damage(cycles_df, sn_curve)

    total_damage: float = damage_result["total_damage"]
    life_years: float = (1.0 / total_damage) if total_damage > 0.0 else math.inf

    # Damage per histogram bin (preserving original order)
    contributions = damage_result.get("damage_contributions", [])
    damage_per_block: List[float] = [c["damage_increment"] for c in contributions]

    # Pad with zeros for bins that contributed no damage (below fatigue limit)
    # Contributions only include bins with finite N_allowable; we need to map
    # them back to the original bins.
    full_damage_per_block: List[float] = []
    contrib_iter = iter(contributions)
    next_contrib = next(contrib_iter, None)

    for s, n in zip(inp.stress_ranges, inp.cycle_counts):
        if (
            next_contrib is not None
            and abs(next_contrib["stress_range"] - s) < 1e-9
            and abs(next_contrib["cycles_applied"] - n) < 1e-6
        ):
            full_damage_per_block.append(next_contrib["damage_increment"])
            next_contrib = next(contrib_iter, None)
        else:
            full_damage_per_block.append(0.0)

    # Governing load case: bin with highest damage contribution
    if full_damage_per_block and max(full_damage_per_block) > 0.0:
        idx_max = int(np.argmax(full_damage_per_block))
        governing_load_case = f"{inp.stress_ranges[idx_max]:.1f} MPa"
    elif inp.stress_ranges:
        governing_load_case = f"{max(inp.stress_ranges):.1f} MPa"
    else:
        governing_load_case = "N/A"

    summary = {
        "total_damage": total_damage,
        "life_years": life_years,
        "utilisation_ratio": total_damage,
        "sn_curve": sn_curve_label,
        "total_cycles": float(sum(inp.cycle_counts)),
        "max_stress_range_mpa": float(max(inp.stress_ranges)),
        "governing_load_case": governing_load_case,
    }

    return FatigueAssessmentResult(
        utilisation_ratio=total_damage,
        life_years=life_years,
        governing_load_case=governing_load_case,
        sn_curve_used=sn_curve_label,
        damage_per_block=full_damage_per_block,
        summary=summary,
        source="skill:fatigue_assessment",
    )
