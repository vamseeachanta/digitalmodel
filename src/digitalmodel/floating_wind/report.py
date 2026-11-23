"""Concept Data Sheet renderer for floating-wind concept screening (issue #1027).

The closing piece of the floating-wind concept-screening epic (#1022): a
standardized, **provenance-stamped one-page data sheet** per shortlisted floater
concept, built *on the shared reporting backbone* (:mod:`digitalmodel.reporting`,
issue #1018) rather than as a parallel renderer.

For one :class:`~digitalmodel.floating_wind.screening.VariantScreening` the sheet
presents, in a fixed causal order:

* **Identity** -- archetype, case id and the screening verdict badge.
* **Parameter vector** -- the swept/base parameters that define the variant
  (issue #1023 archetype inputs).
* **Derived properties** -- displacement, the steel/ballast/topside mass split,
  the hydrostatic stack (``KB``, ``BM``, ``KG``, ``GM``) and the rigid-body
  natural periods (issue #1023).
* **Screening verdict** -- the stability / motion / modal / mooring check table
  with values, limits, signed margins and the governing check (issue #1024),
  evaluated across the site load cases.
* **Trade-space position** -- Pareto rank and key parameter→response
  correlations, when supplied from the trade-space study (issue #1026).
* **Provenance** -- the inputs, the solver tier used (closed-form vs the
  OrcaWave/OrcaFlex high-fidelity tier, issue #1025) and tool versions
  (mandatory provenance intent, issues #1018-#1019).

Reporting-backbone reuse
------------------------
This module is a *consumer* of the single-source-of-truth report block library.
It declares a module-level :class:`~digitalmodel.reporting.ReportBackbone` of
keyed :class:`~digitalmodel.reporting.ReportSection` builders, renders the
ordered section HTML with :meth:`ReportBackbone.render`, and wraps the document
with :class:`~digitalmodel.reporting.ReportRenderer` -- mirroring the diffraction
report-generator adoption (#1018) and the fatigue tracer-bullet migration
(#1020). It does **not** re-implement any document/section assembly.

References
----------
* digitalmodel reporting block library (#1018) -- shared report backbone.
* DNV-OS-C301 / DNV-RP-0286 -- floating-wind stability & design practice.
"""

from __future__ import annotations

import html
import math
from pathlib import Path
from typing import Any

from pydantic import BaseModel, Field

from digitalmodel.floating_wind.screening import VariantScreening
from digitalmodel.reporting import (
    ReportBackbone,
    ReportDataModel,
    ReportRenderer,
    ReportSection,
    SectionMode,
)

__all__ = [
    "ConceptProvenance",
    "ConceptDataSheetData",
    "concept_data_sheet",
    "render_concept_data_sheet_html",
    "write_concept_data_sheet",
    "CONCEPT_DATA_SHEET_BACKBONE",
]


# --- provenance / envelope models -------------------------------------------


def _digitalmodel_version() -> str:
    """Best-effort installed ``digitalmodel`` version (``"unknown"`` if absent)."""
    try:  # pragma: no cover - trivial metadata lookup
        from importlib.metadata import PackageNotFoundError, version

        try:
            return version("digitalmodel")
        except PackageNotFoundError:
            return "unknown"
    except Exception:  # pragma: no cover - defensive
        return "unknown"


class ConceptProvenance(BaseModel):
    """Mandatory provenance stamp for a Concept Data Sheet (#1018-#1019).

    Records *where the numbers came from*: the inputs that defined the variant,
    the solver tier that produced the responses (the licence-free closed-form
    screen of #1024, or the high-fidelity OrcaWave/OrcaFlex tier of #1025), and
    the tool versions for reproducibility.
    """

    solver_tier: str = Field(
        "closed-form (licence-free screen)",
        description="Tier that produced the responses: closed-form vs "
        "OrcaWave/OrcaFlex high-fidelity (#1025)",
    )
    high_fidelity: bool = Field(
        False,
        description="True when any response was replaced by a solved "
        "OrcaWave/OrcaFlex value (#1025)",
    )
    load_cases: list[str] = Field(
        default_factory=list,
        description="Site load cases the variant was screened against (#1024)",
    )
    inputs: dict[str, Any] = Field(
        default_factory=dict,
        description="Free-form input record (criteria, topside, environment)",
    )
    digitalmodel_version: str = Field(default_factory=_digitalmodel_version)
    tool_versions: dict[str, str] = Field(
        default_factory=dict,
        description="Solver/tool versions (e.g. OrcaWave, OrcaFlex) when used",
    )
    references: tuple[str, ...] = (
        "DNV-OS-C301 / DNV-RP-0286 (floating-wind stability)",
        "digitalmodel reporting block library (#1018)",
    )


class ConceptDataSheetData(ReportDataModel):
    """Report envelope for one Concept Data Sheet.

    A thin :class:`~digitalmodel.reporting.ReportDataModel` carrying the screened
    variant plus the optional trade-space position and the provenance stamp. The
    backbone section builders read only from this envelope.
    """

    variant: VariantScreening
    tradespace_rank: int | None = Field(
        None, description="Pareto rank (1 = on the non-dominated front), if known"
    )
    pareto_front_size: int | None = Field(
        None, description="Size of the Pareto front the rank is taken within"
    )
    correlations: dict[str, float] = Field(
        default_factory=dict,
        description="Key parameter→response Pearson correlations (#1026)",
    )
    provenance: ConceptProvenance = Field(default_factory=ConceptProvenance)
    title: str = "Concept Data Sheet"


# --- formatting helpers ------------------------------------------------------


def _num(x: float, fmt: str = ".2f") -> str:
    """Format a float for the sheet, rendering non-finite periods as a glyph."""
    if x is None:
        return "—"
    if not math.isfinite(x):
        return "∞" if x > 0 or math.isinf(x) else "—"
    return format(x, fmt)


def _verdict_badge(passed: bool, feasible: bool) -> str:
    if not feasible:
        text, color = "INFEASIBLE", "#7f8c8d"
    elif passed:
        text, color = "PASS", "#27ae60"
    else:
        text, color = "FAIL", "#c0392b"
    return (
        '<span style="display:inline-block;padding:3px 12px;border-radius:4px;'
        f'color:#fff;font-weight:700;background:{color}">{text}</span>'
    )


def _section(title: str, body: str, *, anchor: str) -> str:
    return f'<div class="section" id="{anchor}"><h2>{title}</h2>{body}</div>'


# --- section builders: (data, **kwargs) -> str over the envelope -------------


def _sec_header(data: ConceptDataSheetData, **_: Any) -> str:
    v = data.variant
    badge = _verdict_badge(v.passed, v.feasible)
    rank = ""
    if data.tradespace_rank is not None:
        rank = f' &nbsp;·&nbsp; Pareto rank {data.tradespace_rank}'
    return (
        '<div class="report-header">'
        f"<h1>{html.escape(data.title)}</h1>"
        f'<div class="subtitle">{html.escape(v.archetype.value.upper())} '
        f"— {html.escape(v.case_id)}</div>"
        f'<div class="meta">Verdict: {badge}'
        f" &nbsp;·&nbsp; governing check: "
        f"<strong>{html.escape(v.governing_check)}</strong> "
        f"(margin {_num(v.governing_margin, '+.3f')}){rank}</div>"
        "</div>"
    )


def _sec_parameters(data: ConceptDataSheetData, **_: Any) -> str:
    v = data.variant
    rows = []
    for k in sorted(v.params):
        val = v.params[k]
        val_str = _num(val, ".4g") if isinstance(val, (int, float)) else str(val)
        rows.append(
            f"<tr><td>{html.escape(str(k))}</td>"
            f"<td>{html.escape(val_str)}</td></tr>"
        )
    body = (
        "<table><thead><tr><th>Parameter</th><th>Value</th></tr></thead>"
        f"<tbody>{''.join(rows)}</tbody></table>"
    )
    return _section(
        f"Parameter Vector — {html.escape(v.archetype.value)} archetype",
        body,
        anchor="parameters",
    )


def _sec_derived(data: ConceptDataSheetData, **_: Any) -> str:
    p = data.variant.properties
    rows = [
        ("Draft", _num(p.draft_m), "m"),
        ("Displacement", _num(p.displacement_t, ".0f"), "t"),
        ("Steel mass", _num(p.steel_mass_t, ".0f"), "t"),
        ("Ballast mass", _num(p.ballast_mass_t, ".0f"), "t"),
        ("Topside mass", _num(p.topside_mass_t, ".0f"), "t"),
        ("Total mass", _num(p.total_mass_t, ".0f"), "t"),
        ("Waterplane area", _num(p.waterplane_area_m2, ".1f"), "m²"),
        ("KB (centre of buoyancy)", _num(p.KB_m), "m"),
        ("BM (metacentric radius)", _num(p.BM_m), "m"),
        ("KG (centre of gravity)", _num(p.KG_m), "m"),
        ("GM (metacentric height)", _num(p.GM_m), "m"),
        ("Heave natural period", _num(p.heave_natural_period_s), "s"),
        ("Pitch natural period", _num(p.pitch_natural_period_s), "s"),
        ("Pitch radius of gyration", _num(p.pitch_radius_gyration_m), "m"),
    ]
    body_rows = "".join(
        f"<tr><td>{html.escape(name)}</td><td>{val}</td>"
        f"<td>{html.escape(unit)}</td></tr>"
        for name, val, unit in rows
    )
    note = ""
    if p.tendon_stabilised:
        note = (
            '<p class="skipped-note">Tendon-stabilised (TLP): heave/pitch '
            "periods are set by tendon axial stiffness; hydrostatic GM may be "
            "small or negative by design.</p>"
        )
    if p.notes:
        items = "".join(f"<li>{html.escape(n)}</li>" for n in p.notes)
        note += f"<ul>{items}</ul>"
    body = (
        "<table><thead><tr><th>Property</th><th>Value</th><th>Unit</th></tr>"
        f"</thead><tbody>{body_rows}</tbody></table>{note}"
    )
    return _section("Derived Naval-Architecture Properties", body, anchor="derived")


def _sec_verdict(data: ConceptDataSheetData, **_: Any) -> str:
    v = data.variant
    rows = []
    for chk in v.checks:
        status = (
            '<span style="color:#27ae60;font-weight:700">PASS</span>'
            if chk.passed
            else '<span style="color:#c0392b;font-weight:700">FAIL</span>'
        )
        governing = " ★" if chk.name == v.governing_check else ""
        rows.append(
            f"<tr><td>{html.escape(chk.name)}{governing}</td>"
            f"<td>{_num(chk.value, '.3g')}</td>"
            f"<td>{_num(chk.limit, '.3g')}</td>"
            f"<td>{_num(chk.margin, '+.3f')}</td>"
            f"<td>{status}</td>"
            f"<td>{html.escape(chk.basis)}</td></tr>"
        )
    lcs = ", ".join(data.provenance.load_cases) or "—"
    body = (
        f"<p>Load cases screened: <strong>{html.escape(lcs)}</strong>. "
        "The governing check (★) has the smallest signed margin.</p>"
        "<table><thead><tr><th>Check</th><th>Value</th><th>Limit</th>"
        "<th>Margin</th><th>Status</th><th>Basis</th></tr></thead>"
        f"<tbody>{''.join(rows)}</tbody></table>"
    )
    return _section("Screening Verdict", body, anchor="verdict")


def _sec_tradespace(data: ConceptDataSheetData, **_: Any) -> str:
    if data.tradespace_rank is None and not data.correlations:
        return ""  # nothing to show -> section omitted (backbone drops "")
    parts: list[str] = []
    if data.tradespace_rank is not None:
        within = (
            f" of {data.pareto_front_size}"
            if data.pareto_front_size is not None
            else ""
        )
        on_front = (
            "on the non-dominated Pareto front"
            if data.tradespace_rank == 1
            else "dominated by lower-rank concepts"
        )
        parts.append(
            f"<p>Trade-space rank: <strong>{data.tradespace_rank}{within}"
            f"</strong> ({on_front}).</p>"
        )
    if data.correlations:
        rows = "".join(
            f"<tr><td>{html.escape(str(k))}</td><td>{_num(r, '+.3f')}</td></tr>"
            for k, r in data.correlations.items()
        )
        parts.append(
            "<table><thead><tr><th>Driver</th><th>Pearson r</th></tr></thead>"
            f"<tbody>{rows}</tbody></table>"
        )
    return _section("Trade-space Position", "".join(parts), anchor="tradespace")


def _sec_provenance(data: ConceptDataSheetData, **_: Any) -> str:
    prov = data.provenance
    rows = [
        ("Solver tier", html.escape(prov.solver_tier)),
        ("High-fidelity solve", "yes" if prov.high_fidelity else "no"),
        ("digitalmodel version", html.escape(prov.digitalmodel_version)),
    ]
    for name, ver in prov.tool_versions.items():
        rows.append((f"{html.escape(str(name))} version", html.escape(str(ver))))
    for k, val in prov.inputs.items():
        rows.append((f"input · {html.escape(str(k))}", html.escape(str(val))))
    table = (
        "<table><thead><tr><th>Provenance</th><th>Value</th></tr></thead>"
        "<tbody>"
        + "".join(f"<tr><td>{name}</td><td>{val}</td></tr>" for name, val in rows)
        + "</tbody></table>"
    )
    refs = ""
    if prov.references:
        items = "".join(f"<li>{html.escape(r)}</li>" for r in prov.references)
        refs = f"<h3>References</h3><ul>{items}</ul>"
    return _section("Provenance", table + refs, anchor="provenance")


# --- backbone declaration ----------------------------------------------------

CONCEPT_DATA_SHEET_BACKBONE = ReportBackbone(
    title="Concept Data Sheet",
    sections=[
        ReportSection("header", "Identity & Verdict", SectionMode.ALWAYS, _sec_header),
        ReportSection(
            "parameters", "Parameter Vector", SectionMode.ALWAYS, _sec_parameters
        ),
        ReportSection(
            "derived", "Derived Properties", SectionMode.ALWAYS, _sec_derived
        ),
        ReportSection(
            "verdict", "Screening Verdict", SectionMode.ALWAYS, _sec_verdict
        ),
        ReportSection(
            "tradespace",
            "Trade-space Position",
            SectionMode.COMPACT_SKIP,
            _sec_tradespace,
        ),
        ReportSection(
            "provenance", "Provenance", SectionMode.ALWAYS, _sec_provenance
        ),
    ],
)


# Self-contained CSS (mirrors the diffraction report look-and-feel; #1018).
_CONCEPT_CSS = """\
  * { box-sizing: border-box; }
  body {
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Arial,
                 sans-serif;
    margin: 0; padding: 0; color: #333; background: #f8f9fa;
    font-size: 14px; line-height: 1.5;
  }
  .container { max-width: 1000px; margin: 0 auto; padding: 1.5em 2em; }
  .report-header {
    background: #2c3e50; color: #fff; padding: 1.2em 2em;
    margin-bottom: 1.5em; border-radius: 6px;
  }
  .report-header h1 { margin: 0 0 0.3em; font-size: 1.6em; }
  .report-header .subtitle {
    font-size: 1.1em; opacity: 0.9; margin-bottom: 0.5em; font-weight: 300;
  }
  .report-header .meta { font-size: 0.9em; opacity: 0.9; }
  .section {
    background: #fff; border-radius: 6px;
    box-shadow: 0 1px 3px rgba(0,0,0,0.08);
    margin-bottom: 1.5em; padding: 1.2em 1.5em;
  }
  .section h2 {
    margin: 0 0 0.8em; font-size: 1.2em; color: #2c3e50;
    border-bottom: 2px solid #3498db; padding-bottom: 0.3em;
  }
  .section h3 { font-size: 1.0em; color: #2c3e50; margin: 0.8em 0 0.3em; }
  table { border-collapse: collapse; margin: 0.5em 0; font-size: 0.9em; width: 100%; }
  th, td { border: 1px solid #ddd; padding: 0.45em 0.7em; text-align: left; }
  th {
    background: #34495e; color: #fff; font-weight: 600; font-size: 0.85em;
    text-transform: uppercase; letter-spacing: 0.3px;
  }
  tbody tr:nth-child(even) { background: #f8f9fa; }
  .skipped-note {
    font-size: 0.85em; color: #8a6d00; margin: 0.5em 0;
    padding: 0.3em 0.6em; background: #fef9e7;
    border-left: 3px solid #f0c674; border-radius: 2px;
  }
"""


# --- public API --------------------------------------------------------------


def concept_data_sheet(
    variant: VariantScreening,
    *,
    tradespace_rank: int | None = None,
    pareto_front_size: int | None = None,
    correlations: dict[str, float] | None = None,
    provenance: ConceptProvenance | None = None,
    load_cases: list[str] | None = None,
    inputs: dict[str, Any] | None = None,
    high_fidelity: bool = False,
    solver_tier: str | None = None,
    title: str | None = None,
    mode: str = "full",
) -> list[str]:
    """Render a Concept Data Sheet to the backbone's ordered section HTML.

    Returns exactly what the reporting backbone returns: the ordered list of
    non-empty section HTML strings (the trade-space section is omitted when no
    trade-space position is supplied). Use :func:`render_concept_data_sheet_html`
    or :func:`write_concept_data_sheet` to wrap it in a standalone document.

    Parameters
    ----------
    variant
        The screened variant to summarise (issue #1024).
    tradespace_rank, pareto_front_size, correlations
        Optional trade-space position from issue #1026.
    provenance
        A fully-built :class:`ConceptProvenance`. If omitted, one is assembled
        from ``load_cases`` / ``inputs`` / ``high_fidelity`` / ``solver_tier``.
    mode
        ``"full"`` (default) or ``"compact"`` -- compact drops the trade-space
        section per the backbone's ``COMPACT_SKIP`` rule.
    """
    if provenance is None:
        kwargs: dict[str, Any] = {
            "high_fidelity": high_fidelity,
            "load_cases": load_cases or [],
            "inputs": inputs or {},
        }
        if solver_tier is not None:
            kwargs["solver_tier"] = solver_tier
        provenance = ConceptProvenance(**kwargs)

    data = ConceptDataSheetData(
        variant=variant,
        tradespace_rank=tradespace_rank,
        pareto_front_size=pareto_front_size,
        correlations=correlations or {},
        provenance=provenance,
        title=title or "Concept Data Sheet",
    )
    return CONCEPT_DATA_SHEET_BACKBONE.render(data, mode=mode)


def render_concept_data_sheet_html(
    variant: VariantScreening, *, mode: str = "full", **kwargs: Any
) -> str:
    """Render a complete standalone HTML Concept Data Sheet document.

    Thin wrapper: builds the section HTML via :func:`concept_data_sheet` and
    wraps it with the shared :class:`~digitalmodel.reporting.ReportRenderer`
    (no parallel document assembly). Extra keyword arguments are forwarded to
    :func:`concept_data_sheet`.
    """
    sections = concept_data_sheet(variant, mode=mode, **kwargs)
    title = kwargs.get("title") or "Concept Data Sheet"
    archetype = variant.archetype.value
    renderer = ReportRenderer()
    return renderer.render_html(
        sections,
        title=f"{title} — {archetype} {variant.case_id}",
        css=_CONCEPT_CSS,
        plotlyjs_src="",
        container_class="container",
    )


def write_concept_data_sheet(
    variant: VariantScreening,
    output_path: str | Path,
    *,
    mode: str = "full",
    **kwargs: Any,
) -> Path:
    """Render and write a Concept Data Sheet HTML file; return its path."""
    html_doc = render_concept_data_sheet_html(variant, mode=mode, **kwargs)
    return ReportRenderer().write_html(html_doc, Path(output_path))
