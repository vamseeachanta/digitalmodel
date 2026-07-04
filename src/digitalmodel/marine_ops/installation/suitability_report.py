"""
ABOUTME: Render a confidence-badged vessel suitability report (markdown + HTML).

Turns the suitability API (vessel_suitability.rank_fleet) into a consumable
artifact for GTM review (#591/#592): a ranked fleet table with a confidence
badge and a `defensible` flag per vessel, plus a summary (how many are
defensible, the confidence-tier mix, and the data-source mix). The report header
states the provenance honestly — public-sourced fleet, estimated/headline-only
values flagged — so it is never read as vessel-specific certified engineering.
"""

from __future__ import annotations

from collections import Counter
from dataclasses import dataclass, field
from typing import Optional

from digitalmodel.marine_ops.installation.vessel_suitability import (
    LiftRequirement,
    SuitabilityResult,
    rank_fleet,
)
from digitalmodel.marine_ops.vessel_db.confidence import ConfidenceTier

# Confidence tier -> (label, hex colour) for the HTML badge.
_TIER_STYLE = {
    ConfidenceTier.MEASURED: ("measured", "#1a7f37"),
    ConfidenceTier.CITED: ("cited", "#2da44e"),
    ConfidenceTier.BROCHURE: ("brochure", "#9a6700"),
    ConfidenceTier.ESTIMATED: ("estimated", "#bf8700"),
    ConfidenceTier.GENERIC: ("generic", "#cf222e"),
    ConfidenceTier.GAP: ("gap", "#82071e"),
}

_PROVENANCE_NOTE = (
    "Fleet data is public-sourced (worldenergydata vessel_fleet + curated brochure "
    "records). Crane SWL marked 'estimated' is headline-only (no published radius "
    "curve); 'generic' is class-typical. This screening ranks data-backed suitability "
    "and is NOT a substitute for vessel-specific lift engineering."
)


@dataclass
class SuitabilityReport:
    title: str
    requirement: LiftRequirement
    rows: list[SuitabilityResult]
    n_defensible: int
    confidence_mix: dict[str, int] = field(default_factory=dict)
    source_mix: dict[str, int] = field(default_factory=dict)


def build_report(
    req: LiftRequirement,
    title: str = "Vessel suitability screening",
    base=None,
) -> SuitabilityReport:
    rows = rank_fleet(req, base=base)
    return SuitabilityReport(
        title=title,
        requirement=req,
        rows=rows,
        n_defensible=sum(1 for r in rows if r.defensible),
        confidence_mix=dict(Counter(r.confidence.value for r in rows)),
        source_mix=dict(Counter((r.source or "unknown") for r in rows)),
    )


def _req_line(req: LiftRequirement) -> str:
    parts = [
        f"{req.weight_te:.0f} te @ {req.radius_m:.0f} m radius",
        f"DAF {req.daf:.2f}",
    ]
    if req.min_dp_class is not None:
        parts.append(f"min DP{req.min_dp_class}")
    if req.deck_area_m2 is not None:
        parts.append(f"deck ≥ {req.deck_area_m2:.0f} m²")
    return ", ".join(parts)


def to_markdown(report: SuitabilityReport, limit: Optional[int] = None) -> str:
    rows = report.rows[:limit] if limit else report.rows
    out = [f"# {report.title}", ""]
    out.append(f"**Lift requirement:** {_req_line(report.requirement)}")
    out.append("")
    out.append(
        f"**{report.n_defensible} of {len(report.rows)}** vessels meet the lift "
        "with defensible (≥ estimated) data."
    )
    out.append("")
    out.append(
        "_Confidence mix:_ "
        + ", ".join(f"{k} {v}" for k, v in report.confidence_mix.items())
    )
    out.append("")
    out.append(
        "| # | Vessel | Score | Defensible | Confidence | SWL@R (te) | Margin | Source |"
    )
    out.append(
        "|--:|--------|------:|:----------:|:----------:|----------:|------:|--------|"
    )
    for i, r in enumerate(rows, 1):
        out.append(
            f"| {i} | {r.vessel} | {r.score:.0f} | "
            f"{'✓' if r.defensible else '—'} | {r.confidence.value} | "
            f"{r.swl_at_radius_te:.0f} | {r.margin:.2f} | {r.source} |"
        )
    out.append("")
    out.append(f"> {_PROVENANCE_NOTE}")
    return "\n".join(out) + "\n"


def _badge(tier: ConfidenceTier) -> str:
    label, colour = _TIER_STYLE.get(tier, (tier.value, "#57606a"))
    return (
        f'<span style="background:{colour};color:#fff;padding:1px 6px;'
        f'border-radius:6px;font-size:0.85em">{label}</span>'
    )


def to_html(report: SuitabilityReport, limit: Optional[int] = None) -> str:
    rows = report.rows[:limit] if limit else report.rows
    trs = []
    for i, r in enumerate(rows, 1):
        flag = "✓" if r.defensible else "—"
        flag_col = "#1a7f37" if r.defensible else "#cf222e"
        lf = "; ".join(r.limiting_factors[:3])
        trs.append(
            f"<tr><td>{i}</td><td>{r.vessel}</td><td style='text-align:right'>{r.score:.0f}</td>"
            f"<td style='text-align:center;color:{flag_col};font-weight:600'>{flag}</td>"
            f"<td style='text-align:center'>{_badge(r.confidence)}</td>"
            f"<td style='text-align:right'>{r.swl_at_radius_te:.0f}</td>"
            f"<td style='text-align:right'>{r.margin:.2f}</td>"
            f"<td>{r.source}</td><td style='font-size:0.85em;color:#57606a'>{lf}</td></tr>"
        )
    mix = ", ".join(f"{k}&nbsp;{v}" for k, v in report.confidence_mix.items())
    css = (
        "body{font-family:-apple-system,Segoe UI,Roboto,sans-serif;margin:2rem;color:#1f2328}"
        "table{border-collapse:collapse;width:100%;font-size:0.92em}"
        "th,td{border-bottom:1px solid #d0d7de;padding:6px 10px}"
        "th{text-align:left;background:#f6f8fa}"
        ".summary{background:#f6f8fa;border:1px solid #d0d7de;border-radius:8px;"
        "padding:10px 14px;margin:1rem 0}"
        ".note{color:#57606a;font-size:0.85em;margin-top:1rem;"
        "border-top:1px solid #d0d7de;padding-top:0.6rem}"
    )
    headers = (
        "<th>#</th><th>Vessel</th><th>Score</th><th>Defensible</th>"
        "<th>Confidence</th><th>SWL@R (te)</th><th>Margin</th>"
        "<th>Source</th><th>Limiting factors</th>"
    )
    summary = (
        f"<b>Lift requirement:</b> {_req_line(report.requirement)}<br>"
        f"<b>{report.n_defensible} of {len(report.rows)}</b> vessels meet the lift "
        f"with defensible (&ge; estimated) data.<br>"
        f"<b>Confidence mix:</b> {mix}"
    )
    return (
        '<!doctype html><html><head><meta charset="utf-8">'
        f"<title>{report.title}</title><style>{css}</style></head><body>"
        f"<h1>{report.title}</h1>"
        f'<div class="summary">{summary}</div>'
        f"<table><thead><tr>{headers}</tr></thead>"
        f"<tbody>{''.join(trs)}</tbody></table>"
        f'<p class="note">{_PROVENANCE_NOTE}</p></body></html>'
    )
