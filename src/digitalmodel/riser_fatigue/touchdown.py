"""
SCR/SLWR Touchdown-Zone Fatigue Workflow (DNV-RP-C203)

Self-contained, license-free fatigue assessment for the touchdown zone (TDZ)
of a Steel Catenary Riser (SCR) or Steel Lazy-Wave Riser (SLWR).

The workflow consumes a **stress-range histogram** (cycles per stress bin)
that characterises the long-term TDZ stress response.  This histogram is the
output of an upstream solver (e.g. OrcaFlex time-domain + rainflow), but this
module does NOT depend on any solver or licence: the histogram is supplied as
input (real or synthetic).  The OrcaFlex stress-extraction front-end is a
deferred follow-on (see issue #810 acceptance criteria).

Method (per DNV-RP-C203, 2021):
  1. Apply a Stress Concentration Factor (SCF) to each nominal stress range.
  2. Apply the thickness correction  S_corr = S * (t/t_ref)^k  (welded joints).
  3. Read allowable cycles N_i from the selected bilinear S-N curve.
  4. Accumulate Palmgren-Miner damage  D = sum(n_i / N_i).
  5. Extrapolate one "histogram period" of damage to the design life and
     apply a Design Fatigue Factor (DFF) for the PASS/FAIL verdict.

Reuses the core ``digitalmodel.fatigue`` engine (get_sn_curve / miner_damage /
thickness_correction) — no S-N math is reimplemented here.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Sequence

import numpy as np
import pandas as pd

from digitalmodel.fatigue import (
    get_sn_curve,
    miner_damage,
    thickness_correction,
)

__all__ = [
    "RiserSection",
    "TouchdownFatigueInput",
    "BinResult",
    "TouchdownFatigueResult",
    "assess_touchdown_fatigue",
    "report_markdown",
]


@dataclass(frozen=True)
class RiserSection:
    """Riser pipe section properties at the touchdown zone.

    Parameters
    ----------
    outer_diameter_mm : float
        Pipe outer diameter (mm).
    wall_thickness_mm : float
        Pipe wall thickness (mm) — used for the DNV thickness correction.
    material : str
        Material designation (informational, e.g. 'API 5L X65').
    """

    outer_diameter_mm: float
    wall_thickness_mm: float
    material: str = "API 5L X65"


@dataclass(frozen=True)
class TouchdownFatigueInput:
    """Inputs for a touchdown-zone fatigue assessment.

    Parameters
    ----------
    section : RiserSection
        Riser pipe section properties.
    stress_ranges_mpa : Sequence[float]
        Nominal hot-spot stress-range bin centres (MPa).
    cycles : Sequence[float]
        Applied cycle count for each stress bin over ``histogram_period_years``.
    sn_class : str
        DNV-RP-C203 detail category (e.g. 'F1', 'F3', 'D').
    environment : str
        S-N environment: 'air', 'seawater_cp', or 'free_corrosion'.
    scf : float
        Stress concentration factor applied to every nominal stress range
        (girth-weld misalignment etc.).  Default 1.0 (already in histogram).
    design_life_years : float
        Required service design life (years).
    dff : float
        Design Fatigue Factor (DNV-OS-F201).  Allowable damage = 1 / DFF.
    histogram_period_years : float
        Real-time duration represented by the supplied histogram (default 1.0,
        i.e. an annual histogram).
    thickness_ref_mm : float
        Reference thickness for the thickness correction (default 25 mm).
    thickness_exponent : float
        Thickness exponent k (default 0.25 for welded tubular joints).
    """

    section: RiserSection
    stress_ranges_mpa: Sequence[float]
    cycles: Sequence[float]
    sn_class: str = "F1"
    environment: str = "seawater_cp"
    scf: float = 1.0
    design_life_years: float = 25.0
    dff: float = 10.0
    histogram_period_years: float = 1.0
    thickness_ref_mm: float = 25.0
    thickness_exponent: float = 0.25


@dataclass(frozen=True)
class BinResult:
    """Per-bin fatigue result row."""

    stress_range_mpa: float
    stress_corrected_mpa: float
    cycles: float
    allowable_cycles: float
    damage: float


@dataclass
class TouchdownFatigueResult:
    """Result of a touchdown-zone fatigue assessment."""

    bins: list[BinResult] = field(default_factory=list)
    period_damage: float = 0.0           # damage per histogram period
    annual_damage: float = 0.0           # damage per year
    design_life_damage: float = 0.0      # damage over the full design life
    fatigue_life_years: float = 0.0      # D=1 life (no DFF)
    allowable_damage: float = 0.0        # 1 / DFF
    usage_factor: float = 0.0            # design_life_damage / allowable_damage
    pass_fail: str = "FAIL"
    standard: str = "DNV-RP-C203 (2021) + DNV-OS-F201 (DFF)"
    sn_class: str = ""
    environment: str = ""
    scf: float = 1.0
    dff: float = 10.0
    design_life_years: float = 25.0


def assess_touchdown_fatigue(
    inp: TouchdownFatigueInput,
) -> TouchdownFatigueResult:
    """Assess SCR/SLWR touchdown-zone fatigue per DNV-RP-C203.

    Returns a :class:`TouchdownFatigueResult` with per-bin damage, the
    design-life damage, the D=1 fatigue life, and a DFF-based PASS/FAIL.
    """
    stress = np.asarray(inp.stress_ranges_mpa, dtype=float)
    cycles = np.asarray(inp.cycles, dtype=float)
    if stress.shape != cycles.shape:
        raise ValueError(
            "stress_ranges_mpa and cycles must have the same length "
            f"({stress.shape} vs {cycles.shape})"
        )
    if inp.histogram_period_years <= 0:
        raise ValueError("histogram_period_years must be > 0")
    if inp.dff <= 0:
        raise ValueError("dff must be > 0")

    # 1) SCF on nominal stress ranges, then 2) DNV thickness correction.
    stress_scf = stress * float(inp.scf)
    stress_corr = thickness_correction(
        stress_scf,
        t_actual=inp.section.wall_thickness_mm,
        t_ref=inp.thickness_ref_mm,
        k=inp.thickness_exponent,
    )

    # 3) + 4) Allowable cycles and Palmgren-Miner damage via the core engine.
    sn_curve = get_sn_curve(inp.sn_class, inp.environment)
    histogram = pd.DataFrame(
        {"stress_range": stress_corr, "cycles": cycles}
    )
    damaged = miner_damage(histogram, sn_curve)
    period_damage = float(damaged.attrs["total_damage"])

    bins: list[BinResult] = []
    for s_nom, s_corr, row in zip(
        stress, stress_corr, damaged.itertuples(index=False)
    ):
        bins.append(
            BinResult(
                stress_range_mpa=float(s_nom),
                stress_corrected_mpa=float(s_corr),
                cycles=float(row.cycles),
                allowable_cycles=float(row.allowable_cycles),
                damage=float(row.damage),
            )
        )

    annual_damage = period_damage / inp.histogram_period_years
    design_life_damage = annual_damage * inp.design_life_years
    fatigue_life_years = (
        float("inf") if annual_damage <= 0 else 1.0 / annual_damage
    )

    # 5) DFF check: design-life damage must not exceed the allowable 1/DFF.
    allowable_damage = 1.0 / inp.dff
    usage_factor = (
        float("inf")
        if allowable_damage <= 0
        else design_life_damage / allowable_damage
    )
    pass_fail = "PASS" if usage_factor <= 1.0 else "FAIL"

    return TouchdownFatigueResult(
        bins=bins,
        period_damage=period_damage,
        annual_damage=annual_damage,
        design_life_damage=design_life_damage,
        fatigue_life_years=fatigue_life_years,
        allowable_damage=allowable_damage,
        usage_factor=usage_factor,
        pass_fail=pass_fail,
        sn_class=inp.sn_class,
        environment=inp.environment,
        scf=float(inp.scf),
        dff=float(inp.dff),
        design_life_years=float(inp.design_life_years),
    )


def report_markdown(
    inp: TouchdownFatigueInput,
    result: TouchdownFatigueResult,
    location: str = "Touchdown Zone (TDZ)",
) -> str:
    """Render a Markdown fatigue report for the assessment."""
    sec = inp.section
    lines: list[str] = []
    lines.append("# SCR/SLWR Touchdown-Zone Fatigue Report")
    lines.append("")
    lines.append(f"**Standard:** {result.standard}")
    lines.append(f"**Critical location:** {location}")
    lines.append("")
    lines.append("## Inputs")
    lines.append("")
    lines.append("| Parameter | Value |")
    lines.append("|---|---|")
    lines.append(f"| Outer diameter | {sec.outer_diameter_mm:.1f} mm |")
    lines.append(f"| Wall thickness | {sec.wall_thickness_mm:.1f} mm |")
    lines.append(f"| Material | {sec.material} |")
    lines.append(f"| S-N detail category | {result.sn_class} ({result.environment}) |")
    lines.append(f"| SCF | {result.scf:.2f} |")
    lines.append(
        f"| Thickness correction | t_ref={inp.thickness_ref_mm:.0f} mm, "
        f"k={inp.thickness_exponent:.2f} |"
    )
    lines.append(f"| Histogram period | {inp.histogram_period_years:.2f} yr |")
    lines.append(f"| Design life | {result.design_life_years:.0f} yr |")
    lines.append(f"| Design Fatigue Factor (DFF) | {result.dff:.1f} |")
    lines.append("")
    lines.append("## Stress-range histogram and per-bin damage")
    lines.append("")
    lines.append(
        "| Nominal ΔS (MPa) | Corrected ΔS (MPa) | Cycles n | Allowable N | "
        "Damage n/N |"
    )
    lines.append("|---:|---:|---:|---:|---:|")
    for b in result.bins:
        lines.append(
            f"| {b.stress_range_mpa:.1f} | {b.stress_corrected_mpa:.1f} | "
            f"{b.cycles:,.0f} | {b.allowable_cycles:,.3g} | {b.damage:.4e} |"
        )
    lines.append("")
    lines.append("## Results")
    lines.append("")
    lines.append("| Quantity | Value |")
    lines.append("|---|---|")
    lines.append(f"| Damage per histogram period | {result.period_damage:.4e} |")
    lines.append(f"| Annual damage | {result.annual_damage:.4e} |")
    lines.append(
        f"| Damage over design life ({result.design_life_years:.0f} yr) | "
        f"{result.design_life_damage:.4e} |"
    )
    life = result.fatigue_life_years
    life_str = "inf" if np.isinf(life) else f"{life:,.1f}"
    lines.append(f"| Unfactored fatigue life (D=1) | {life_str} yr |")
    lines.append(f"| Allowable damage (1/DFF) | {result.allowable_damage:.4e} |")
    lines.append(f"| Usage factor (D_life · DFF) | {result.usage_factor:.3f} |")
    lines.append(f"| **Verdict** | **{result.pass_fail}** |")
    lines.append("")
    lines.append("## Deferred (follow-on)")
    lines.append("")
    lines.append(
        "- OrcaFlex time-domain stress extraction + rainflow at the TDZ to "
        "generate the histogram consumed by this workflow (issue #810)."
    )
    lines.append("")
    return "\n".join(lines)
