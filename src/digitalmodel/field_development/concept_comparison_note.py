# ABOUTME: W6 concept comparison note — weighted multi-criteria + sensitivity sweep + markdown.
# ABOUTME: Renders the deterministic screening result into a sourced FDP comparison note.
"""
digitalmodel.field_development.concept_comparison_note
======================================================

Turns a :class:`concept_screening.ScreeningResult` into:

  * a **weighted multi-criteria** ranking table,
  * a one-way **weight-sensitivity sweep** (re-rank under each axis dominant),
  * the **governing trade-off** call,
  * a markdown comparison note suitable for an FDP / wiki-PR artifact.

Everything is deterministic: same inputs -> byte-identical markdown. No client
names, no $/day rig economics (days only), and a scope banner stating the
screening-grade limits.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Optional

from .concept_screening import (
    DEFAULT_WEIGHTS,
    ProspectSpec,
    ScreeningResult,
    screen_concepts,
)

_SCOPE_BANNER = (
    "> **Scope (screening-grade).** Rule/parameter-based concept screening only. "
    "No reservoir, flow-assurance, or structural simulation. Production rate and "
    "well count are inputs. CAPEX is driver-level (not audited cashflow); rig demand "
    "is in days (live $/day rig rates are a paid feed, deliberately excluded). "
    "Dry-tree-unit (FrPS-class) access factors are engineering assumptions, not "
    "operating results. Figures carry wide bands — use to rank, not to commit."
)


@dataclass
class SensitivityRow:
    """One sensitivity scenario: which weighting, and the resulting winner."""

    scenario: str
    weights: dict[str, float]
    winner: str
    runner_up: str


# ---------------------------------------------------------------------------
# Sensitivity sweep
# ---------------------------------------------------------------------------

def weight_sensitivity_sweep(
    spec: ProspectSpec,
    params_path: Optional[str] = None,
    bsee_benchmark: Optional[float] = None,
    dominant_weight: float = 0.55,
) -> list[SensitivityRow]:
    """Re-rank the prospect under each axis made dominant, one at a time.

    Deterministic: the base case plus one scenario per axis. ``dominant_weight``
    is assigned to the dominant axis; the remainder is split evenly across the
    other axes.
    """
    axes = list(DEFAULT_WEIGHTS.keys())
    rows: list[SensitivityRow] = []

    # Base case first.
    base = screen_concepts(spec, params_path=params_path, bsee_benchmark=bsee_benchmark)
    rows.append(
        SensitivityRow(
            scenario="base weights",
            weights=dict(base.weights),
            winner=base.ranked[0].host_type.value,
            runner_up=base.ranked[1].host_type.value if len(base.ranked) > 1 else "-",
        )
    )

    others_share = (1.0 - dominant_weight) / max(1, len(axes) - 1)
    for dom in axes:
        w = {a: (dominant_weight if a == dom else others_share) for a in axes}
        res = screen_concepts(
            spec, weights=w, params_path=params_path, bsee_benchmark=bsee_benchmark
        )
        rows.append(
            SensitivityRow(
                scenario=f"{dom}-dominant",
                weights=w,
                winner=res.ranked[0].host_type.value,
                runner_up=res.ranked[1].host_type.value if len(res.ranked) > 1 else "-",
            )
        )
    return rows


# ---------------------------------------------------------------------------
# Markdown rendering
# ---------------------------------------------------------------------------

def _fmt_weights(weights: dict[str, float]) -> str:
    return ", ".join(f"{k} {v:.2f}" for k, v in weights.items())


def render_comparison_note(
    result: ScreeningResult,
    sensitivity: Optional[list[SensitivityRow]] = None,
) -> str:
    """Render a deterministic markdown comparison note from a screening result."""
    s = result.prospect
    lines: list[str] = []
    lines.append(f"# Concept screening — {s.name}")
    lines.append("")
    lines.append(_SCOPE_BANNER)
    lines.append("")

    # Prospect inputs
    dist = f"{s.distance_to_infra_km:.0f} km" if s.distance_to_infra_km is not None else "n/a"
    lines.append("## Prospect (inputs)")
    lines.append("")
    lines.append("| Parameter | Value |")
    lines.append("|---|---|")
    lines.append(f"| Water depth | {s.water_depth_m:.0f} m |")
    lines.append(f"| Development wells | {s.well_count} |")
    lines.append(f"| Recoverable reserves | {s.reservoir_size_mmbbl:.0f} MMbbl |")
    lines.append(f"| Production capacity | {s.production_capacity_bopd:,.0f} bopd |")
    lines.append(f"| Fluid | {s.fluid_type} |")
    lines.append(f"| Distance to infrastructure | {dist} |")
    lines.append("")
    lines.append(
        f"Parameter basis: `{result.params_basis}`. "
        f"Per-well rig-days source: {result.rig_day_source}. "
        f"Decision-axis weights: {_fmt_weights(result.weights)}."
    )
    lines.append("")

    # Ranking table
    lines.append("## Ranked concepts")
    lines.append("")
    lines.append(
        "| Rank | Concept | Composite | Suitability | Schedule (wk) | Rig-days | "
        "CAPEX ($bn) | Interv. index | Gated |"
    )
    lines.append("|---|---|---|---|---|---|---|---|---|")
    for i, r in enumerate(result.ranked, start=1):
        a = r.axes
        lines.append(
            f"| {i} | {r.host_type.value} | {r.composite:.2f} | {r.suitability:.1f} | "
            f"{a.schedule_weeks:.0f} | {a.rig_days:.0f} | {a.capex_base_usd_bn:.2f} | "
            f"{a.intervention_index:.2f} | {'yes' if r.gated else 'no'} |"
        )
    lines.append("")

    # Normalised axis scores (higher = better)
    lines.append("## Normalised axis scores (0-100, higher is better)")
    lines.append("")
    lines.append("| Concept | Schedule | Rig-days | CAPEX | Intervention | Weighted |")
    lines.append("|---|---|---|---|---|---|")
    for r in result.ranked:
        n = r.axes.norm
        lines.append(
            f"| {r.host_type.value} | {n['schedule']:.1f} | {n['rig_days']:.1f} | "
            f"{n['capex']:.1f} | {n['intervention']:.1f} | {r.weighted_axis_score:.1f} |"
        )
    lines.append("")

    # CAPEX line items for the top concept
    top = result.ranked[0]
    lines.append(f"## CAPEX driver breakdown — {top.host_type.value} (recommended)")
    lines.append("")
    lines.append("| Line item | USD bn |")
    lines.append("|---|---|")
    for k, v in top.axes.capex_line_items_usd_bn.items():
        lines.append(f"| {k} | {v:.3f} |")
    lines.append("")

    # Governing trade-off
    lines.append("## Governing trade-off")
    lines.append("")
    runner = result.ranked[1].host_type.value if len(result.ranked) > 1 else "-"
    lines.append(
        f"Recommended concept: **{top.host_type.value}** "
        f"(composite {top.composite:.2f}/100). The axis that most separates it from "
        f"the runner-up ({runner}) is **{result.governing_axis}** — this is the "
        f"decision's governing trade-off."
    )
    lines.append("")

    # Sensitivity
    if sensitivity:
        lines.append("## Weight-sensitivity sweep")
        lines.append("")
        lines.append("| Scenario | Weights | Winner | Runner-up |")
        lines.append("|---|---|---|---|")
        for row in sensitivity:
            lines.append(
                f"| {row.scenario} | {_fmt_weights(row.weights)} | "
                f"**{row.winner}** | {row.runner_up} |"
            )
        lines.append("")
        winners = {row.winner for row in sensitivity}
        if len(winners) == 1:
            lines.append(
                f"The recommendation (**{top.host_type.value}**) is **robust** — it wins "
                "under every single-axis-dominant weighting tested."
            )
        else:
            lines.append(
                "The recommendation is **weighting-sensitive** — the winner changes across "
                f"the sweep ({', '.join(sorted(winners))}). Confirm the weights with the "
                "decision owner before relying on the ranking."
            )
        lines.append("")

    lines.append("---")
    lines.append(
        "_Deterministic concept screening (digitalmodel field_development). "
        "Generic GoM parameter basis; client-specific calibration is a private overlay._"
    )
    lines.append("")
    return "\n".join(lines)


def build_comparison_note(
    spec: ProspectSpec,
    weights: Optional[dict[str, float]] = None,
    params_path: Optional[str] = None,
    bsee_benchmark: Optional[float] = None,
    with_sensitivity: bool = True,
) -> tuple[ScreeningResult, str]:
    """Convenience: screen a prospect and render the markdown note in one call."""
    result = screen_concepts(
        spec, weights=weights, params_path=params_path, bsee_benchmark=bsee_benchmark
    )
    sweep = (
        weight_sensitivity_sweep(spec, params_path=params_path, bsee_benchmark=bsee_benchmark)
        if with_sensitivity
        else None
    )
    return result, render_comparison_note(result, sweep)
