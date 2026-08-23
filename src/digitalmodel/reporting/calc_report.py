#!/usr/bin/env python3
"""Standard AceEngineer engineering calculation report.

ABOUTME: The canonical section order and typed section content for every
engineering calculation report, so analyses fill **structured slots** rather
than hand-writing HTML and drifting apart.

Builds on the skeleton-first machinery (#1021): this module supplies the house
:data:`CALC_REPORT_SKELETON` plus typed content models for each section, so a
calculation supplies *data* and the report renders itself.

The house rules this encodes:

1. **Fixed section order.** Objective, design data, analysis methodology,
   results, validation status, way forward, references. Objective and design
   data come first so a reviewer sees the basis before the numbers.
2. **Formulas are shown, not hidden.** Every governing relation renders as a
   numbered equation card with a variable-definition table under it, so the
   arithmetic can be checked by hand. No MathJax — CSP blocks CDNs.
3. **Provenance is colour.** Every headline claim carries a
   :class:`Confidence` level: validated (teal), analytical with assumptions
   carried (amber), or pending data (muted). A projection must never read as
   proven. This is the honesty guardrail, and it is why
   :class:`ResultBlock` requires a confidence rather than defaulting one.
4. **Assumptions are declared, never silent.** Anything not supplied by the
   client goes in the assumption ledger with its basis.

Example::

    report = CalcReport(
        report_id="AE-AL-2026-001",
        title="Rod-pump surface-card analysis",
        discipline="Artificial Lift",
        objective=Objective(purpose="...", scope=["..."]),
        design_data=[DesignDataGroup(caption="Pumping unit", rows=[...])],
        ...
    )
    report.write(Path("report.html"))
"""

from __future__ import annotations

import html
from enum import Enum
from pathlib import Path
from typing import List, Optional, Sequence

from pydantic import Field

from digitalmodel.reporting._base import ReportDataModel
from digitalmodel.reporting.skeleton import BlockSpec, ReportSkeleton, SectionSpec


class Confidence(str, Enum):
    """How much weight a claim can carry. Rendered as colour.

    The three levels are deliberately coarse. A finer scale invites splitting
    hairs; what a reader needs to know is whether a number is measured,
    inferred, or not yet available.
    """

    VALIDATED = "validated"    # teal — checked against reference or measured directly
    ANALYTICAL = "analytical"  # amber — sound method, but carries an assumption
    PENDING = "pending"        # muted — cannot be stated until data arrives

    @property
    def css_class(self) -> str:
        return {"validated": "cfd", "analytical": "ro", "pending": "proj"}[self.value]

    @property
    def label(self) -> str:
        return {
            "validated": "Validated",
            "analytical": "Analytical",
            "pending": "Pending",
        }[self.value]

    @property
    def status_class(self) -> str:
        return {"validated": "ok", "analytical": "proj", "pending": "pend"}[self.value]

    @property
    def status_icon(self) -> str:
        return {"validated": "&#10003;", "analytical": "&asymp;", "pending": "&#8230;"}[
            self.value
        ]


def _esc(text: str) -> str:
    """Escape text, but leave intentional entities and inline markup alone."""
    return text if ("<" in text or "&" in text) else html.escape(text)


#: The house organisation, and the wordmark it is set in on the masthead.
#: Only the house name gets the two-tone treatment. Where to split another
#: organisation's name into stem and stress is a typographic decision about
#: someone else's identity, and guessing it wrong is worse than not trying:
#: any other name is set as plain text.
_HOUSE_ORGANISATION = "AceEngineer"
_HOUSE_WORDMARK = "Ace<b>Engineer</b>"


def _wordmark(organisation: str) -> str:
    """The masthead form of an organisation name."""
    if organisation == _HOUSE_ORGANISATION:
        return _HOUSE_WORDMARK
    return _esc(organisation)


def _logo_img(data_uri: str, alt: str) -> str:
    """The masthead logo, or nothing when no logo is set.

    Only a ``data:`` image URI is accepted. A report is a single
    self-contained file that has to render from an email attachment with no
    network, so a remote logo would leave a broken image on the client's
    screen; and an arbitrary URI in an ``src`` attribute is the same
    injection surface the reference links are guarded against.
    """
    if not data_uri:
        return ""
    if not data_uri.startswith("data:image/"):
        raise ValueError(
            "organisation_logo must be a self-contained data:image/ URI, "
            f"not {data_uri[:32]!r}")
    return (f'<img class="brand-logo" src="{html.escape(data_uri, quote=True)}" '
            f'alt="{html.escape(alt, quote=True)}" '
            'style="height:30px;width:auto;display:inline-block;'
            'vertical-align:middle;margin-right:10px">')


# ---------------------------------------------------------------------------
# Typed section content
# ---------------------------------------------------------------------------
class Objective(ReportDataModel):
    """Why the calculation exists and what bounds it."""

    purpose: str
    scope: List[str] = Field(default_factory=list)
    preliminary_note: str = ""

    def render(self) -> str:
        chips = "".join(f'<span class="chip">{_esc(s)}</span>' for s in self.scope)
        note = (
            f'<p class="prose">{_esc(self.preliminary_note)}</p>'
            if self.preliminary_note
            else ""
        )
        return (
            '<div class="objective" style="margin-top:18px">'
            '<span class="oh">Objective</span>'
            f"<p>{_esc(self.purpose)}</p>"
            f'<div class="scope">{chips}</div></div>{note}'
        )


class DataRow(ReportDataModel):
    """One parameter: label, value, unit."""

    label: str
    value: str
    unit: str = ""


class DesignDataGroup(ReportDataModel):
    """A captioned key-value table of inputs.

    Design data is standardised as groups of labelled rows rather than free
    prose, so two reports on the same discipline present their inputs the same
    way and a reviewer knows where to look.
    """

    caption: str
    rows: List[DataRow] = Field(default_factory=list)

    def render(self) -> str:
        body = "".join(
            f"<tr><td>{_esc(r.label)}</td><td>{_esc(r.value)}"
            + (f" <span class='un'>{_esc(r.unit)}</span>" if r.unit else "")
            + "</td></tr>"
            for r in self.rows
        )
        return (
            f'<div class="kv"><div class="cap">{_esc(self.caption)}</div>'
            f"<table><tbody>{body}</tbody></table></div>"
        )


class VariableDef(ReportDataModel):
    """One symbol appearing in an equation."""

    symbol: str
    description: str
    unit: str = ""


class Equation(ReportDataModel):
    """A numbered governing relation with its variable definitions.

    ``markup`` is house equation markup, not LaTeX: fractions via
    ``.frac > .n/.d``, roots via ``.rad > .rc``. MathJax is unavailable because
    the report must render with no external fetch.
    """

    markup: str
    variables: List[VariableDef] = Field(default_factory=list)
    note: str = ""

    def render(self, number: int) -> str:
        defs = "".join(
            f'<div class="d"><var>{v.symbol}</var><span>{_esc(v.description)}</span>'
            f'<span class="u">{_esc(v.unit)}</span></div>'
            for v in self.variables
        )
        note = f'<p class="prose">{_esc(self.note)}</p>' if self.note else ""
        return (
            f'<div class="eq"><div class="m">{self.markup}</div>'
            f'<div class="eno">({number})</div></div>'
            + (f'<div class="defs">{defs}</div>' if defs else "")
            + note
        )


class MethodBlock(ReportDataModel):
    """One methodology subsection: prose plus its governing relations."""

    heading: str
    prose: str = ""
    equations: List[Equation] = Field(default_factory=list)
    figure_svg: str = ""

    def render(self, index: int, eq_start: int) -> tuple:
        parts = [
            f'<div class="l2" id="s3-{index}"><div class="l2head">'
            f'<span class="n"></span><h3>{_esc(self.heading)}</h3></div>'
        ]
        if self.prose:
            parts.append(f'<p class="prose">{_esc(self.prose)}</p>')
        number = eq_start
        for equation in self.equations:
            parts.append(equation.render(number))
            number += 1
        if self.figure_svg:
            parts.append(f'<div class="flow">{self.figure_svg}</div>')
        parts.append("</div>")
        return "".join(parts), number


class ResultBlock(ReportDataModel):
    """One result, with the confidence it carries.

    ``confidence`` is required. A result whose standing is unstated is exactly
    the failure mode the colour system exists to prevent.
    """

    heading: str
    confidence: Confidence
    groups: List[DesignDataGroup] = Field(default_factory=list)
    prose: str = ""
    caption: str = ""

    def render(self, index: int) -> str:
        parts = [
            f'<div class="l2" id="s4-{index}"><div class="l2head">'
            f'<span class="n"></span><h3>{_esc(self.heading)}</h3></div>'
        ]
        if self.prose:
            parts.append(f'<p class="prose">{_esc(self.prose)}</p>')
        if self.groups:
            parts.append(
                '<div class="datagrid">'
                + "".join(g.render() for g in self.groups)
                + "</div>"
            )
        if self.caption:
            parts.append(
                f'<div class="fig-cap"><span class="tag {self.confidence.css_class}">'
                f"{self.confidence.label}</span><span>{_esc(self.caption)}</span></div>"
            )
        parts.append("</div>")
        return "".join(parts)


class ValidationItem(ReportDataModel):
    """One entry on the provenance ladder."""

    claim: str
    basis: str
    confidence: Confidence

    def render(self) -> str:
        return (
            f'<div class="st {self.confidence.status_class}">'
            f'<div class="icon">{self.confidence.status_icon}</div><div>'
            f'<div class="lab">{_esc(self.claim)}</div>'
            f'<div class="sub">{_esc(self.basis)}</div></div>'
            f'<div class="pill">{self.confidence.label}</div></div>'
        )


class WayForwardStage(ReportDataModel):
    """A staged next step: what is needed, and what it unlocks."""

    heading: str
    prose: str = ""
    required: List[DataRow] = Field(default_factory=list)
    returns: List[DataRow] = Field(default_factory=list)

    def render(self, index: int) -> str:
        parts = [
            f'<div class="l2" id="s6-{index}"><div class="l2head">'
            f'<span class="n"></span><h3>{_esc(self.heading)}</h3></div>'
        ]
        if self.prose:
            parts.append(f'<p class="prose">{_esc(self.prose)}</p>')
        if self.required or self.returns:
            groups = []
            if self.required:
                groups.append(DesignDataGroup(caption="Required", rows=self.required))
            if self.returns:
                groups.append(DesignDataGroup(caption="Returns", rows=self.returns))
            parts.append(
                '<div class="datagrid">'
                + "".join(g.render() for g in groups)
                + "</div>"
            )
        parts.append("</div>")
        return "".join(parts)


class Reference(ReportDataModel):
    """One cited standard, paper or data source.

    ``url`` is optional and rendered as a link on the citation text when
    present. A reference a reader cannot follow is a claim of provenance
    rather than provenance itself -- but many legitimate sources have no
    public URL (a purchased standard, a client-supplied model), so this is
    where-applicable and never required.
    """

    text: str
    url: Optional[str] = None


class RevisionEntry(ReportDataModel):
    """One row of the revision history: one issue to the client.

    The header already carries the CURRENT revision. This is the trail behind
    it, which is what a reviewer holding an earlier copy actually needs: not
    which revision this is, but what changed since theirs.

    ``revision`` is a letter -- A, B, C -- and it moves only when the report is
    issued to the client. It is never derived and never auto-incremented: the
    person issuing the report sets it. An internal edit is not an issue, and a
    letter that moved without one would tell a client they are holding a
    superseded document when they are not.

    ``incorporates`` names the internal range that issue folded in, e.g.
    ``"A.1-A.9"``. It is the link between the two tiers and is optional: a
    report that keeps no internal log has nothing to point at.
    """

    revision: str
    date: str
    description: str
    by: Optional[str] = None
    incorporates: str = ""


class InternalRevision(ReportDataModel):
    """One staff change to the document between issues.

    Numbered under the main letter that is currently pending -- ``A.1``,
    ``A.2``, ``A.3`` -- so the number says at a glance which issue the work
    belongs to, and the internal log resets naturally when the letter moves.

    These rows are client-visible in the delivered PDF. They record what
    changed in the document, not why: "waterline lengths corrected across the
    condition matrix" is a revision entry, an account of how the error got
    there is not.

    ``by`` should name the function that made the change rather than an
    individual, unless the individual is genuinely known.
    """

    revision: str
    date: str
    description: str
    by: Optional[str] = None


class KPI(ReportDataModel):
    """A headline number for the hero band."""

    value: str
    unit: str = ""
    caption: str = ""
    confidence: Optional[Confidence] = None

    def render(self) -> str:
        cls = {
            Confidence.VALIDATED: " teal",
            Confidence.ANALYTICAL: " amber",
        }.get(self.confidence, "")
        unit = f'<span class="u">{_esc(self.unit)}</span>' if self.unit else ""
        return (
            f'<div class="kpi{cls}"><div class="v num">{_esc(self.value)}{unit}</div>'
            f'<div class="k">{_esc(self.caption)}</div></div>'
        )


# ---------------------------------------------------------------------------
# The canonical house skeleton
# ---------------------------------------------------------------------------
CALC_REPORT_SKELETON = ReportSkeleton(
    title="AceEngineer standard engineering calculation report",
    sections=[
        SectionSpec(key="s1", label="Objective", blocks=[
            BlockSpec(key="objective", label="Purpose and scope",
                      description="what the calculation answers and what bounds it"),
        ]),
        SectionSpec(key="s2", label="Design data", blocks=[
            BlockSpec(key="design_data", label="Inputs",
                      description="inputs, constants, geometry as keyed tables"),
            BlockSpec(key="assumptions", label="Assumptions carried", required=False,
                      description="anything not client-supplied, with its basis"),
        ]),
        SectionSpec(key="s3", label="Analysis methodology", blocks=[
            BlockSpec(key="methodology", label="Governing relations",
                      description="numbered equation cards with variable definitions"),
        ]),
        SectionSpec(key="s4", label="Results", blocks=[
            BlockSpec(key="results", label="Detailed results",
                      description="one subsection per result, each with a confidence"),
        ]),
        SectionSpec(key="s5", label="Validation status", blocks=[
            BlockSpec(key="validation", label="Provenance ladder",
                      description="honest standing of every headline claim"),
        ]),
        SectionSpec(key="s6", label="Way forward", blocks=[
            BlockSpec(key="way_forward", label="Staged next steps",
                      description="what each additional input unlocks"),
        ]),
        SectionSpec(key="s7", label="References & provenance", blocks=[
            BlockSpec(key="references", label="References",
                      description="standards, methods, data sources"),
        ]),
    ],
)

# The presentation layer is vendored into the package rather than referenced
# from a developer path, so the standard is portable and a report renders
# identically wherever digitalmodel is installed.
_DEFAULT_TEMPLATE = Path(__file__).parent / "assets" / "calc_report_format.html"


class CalcReport(ReportDataModel):
    """A complete engineering calculation report in the house format."""

    report_id: str
    title: str
    discipline: str
    lede: str
    revision: str = "A"
    preliminary: bool = False

    #: Who the report is issued by. Carried in three places that have to
    #: agree -- the masthead, the footer byline and the foot-bar -- so they
    #: are set from one field rather than three strings that can drift apart.
    #: Defaults to the house mark, so an existing report is unchanged.
    organisation: str = _HOUSE_ORGANISATION
    #: Optional masthead logo as a self-contained ``data:`` image URI. Empty
    #: by default: the house masthead is a wordmark, not an image.
    organisation_logo: str = ""

    objective: Objective
    design_data: List[DesignDataGroup] = Field(default_factory=list)
    assumptions: List[DesignDataGroup] = Field(default_factory=list)
    methodology: List[MethodBlock] = Field(default_factory=list)
    results: List[ResultBlock] = Field(default_factory=list)
    validation: List[ValidationItem] = Field(default_factory=list)
    way_forward: List[WayForwardStage] = Field(default_factory=list)
    references: List[Reference] = Field(default_factory=list)
    kpis: List[KPI] = Field(default_factory=list)
    #: The client-facing trail: one row per issue, keyed by revision letter.
    revision_history: List[RevisionEntry] = Field(default_factory=list)
    #: The staff change log between issues, keyed A.1, A.2, ... under the
    #: pending letter. Rendered beneath the issued table and labelled apart
    #: from it. Empty by default, so no report that predates it changes.
    #: Rendered in the order given -- descending is the house convention, but
    #: sorting "A.10" against "A.9" as strings would invert them, and the
    #: caller already knows the order it means.
    internal_revisions: List[InternalRevision] = Field(default_factory=list)

    def completeness(self):
        """Which required house sections are still empty."""
        content = {
            "objective": "x" if self.objective else "",
            "design_data": "x" if self.design_data else "",
            "methodology": "x" if self.methodology else "",
            "results": "x" if self.results else "",
            "validation": "x" if self.validation else "",
            "way_forward": "x" if self.way_forward else "",
            "references": "x" if self.references else "",
        }
        skeleton = CALC_REPORT_SKELETON.model_copy(deep=True)
        skeleton.require_provenance = False
        return skeleton.completeness(content)

    # -- rendering ----------------------------------------------------------

    def _section(self, key: str, title: str, subtitle: str, body: str) -> str:
        return (
            f'<section id="{key}"><div class="l1head"><span class="secnum"></span>'
            f"<h2>{title}</h2></div>"
            f'<p class="l1sub">{_esc(subtitle)}</p>{body}'
            f'<a class="backtop" href="#top">↑ contents</a></section>'
        )

    @staticmethod
    def _revhist_table(rows: str, incorporates: bool = False) -> str:
        """The house revision table. Both tiers use it, so they read alike."""
        inc = "<th>Incorporates</th>" if incorporates else ""
        return ('<table class="revhist"><thead><tr><th>Rev</th><th>Date</th>'
                f'<th>Description</th>{inc}<th>By</th></tr></thead>'
                f"<tbody>{rows}</tbody></table>")

    def _revision_history(self) -> str:
        """The revision-history body: the issued table, then the internal log.

        The issued table is first and is the client-facing record. When there
        is an internal log beneath it, both tiers are given a labelled
        subsection so a client reading the PDF cannot mistake our change log
        for the list of documents they have been sent. When there is no
        internal log there is nothing to distinguish, so the labels are not
        rendered -- which is also what keeps every report that predates this
        feature byte-identical.
        """
        # The Incorporates column earns its place only when a row uses it. A
        # permanently-empty column in a client-facing table is noise, and
        # rendering it unconditionally would move every existing report.
        show_inc = any(h.incorporates for h in self.revision_history)
        issued_rows = "".join(
            f'<tr><td>{_esc(h.revision)}</td><td>{_esc(h.date)}</td>'
            f"<td>{_esc(h.description)}</td>"
            + (f"<td>{_esc(h.incorporates)}</td>" if show_inc else "")
            + f'<td>{_esc(h.by or "")}</td></tr>'
            for h in self.revision_history)
        issued = self._revhist_table(issued_rows, incorporates=show_inc)

        if not self.internal_revisions:
            return issued

        internal_rows = "".join(
            f'<tr><td>{_esc(r.revision)}</td><td>{_esc(r.date)}</td>'
            f"<td>{_esc(r.description)}</td>"
            f'<td>{_esc(r.by or "")}</td></tr>'
            for r in self.internal_revisions)
        return (
            '<div class="l2" id="s8-1"><div class="l2head"><span class="n"></span>'
            "<h3>Issued revisions</h3></div>"
            '<p class="prose">Each row is one issue of this document to the '
            "client. The revision letter moves only on issue; it is set when "
            "the document is sent, not when it is edited. This is the record "
            "of what has been received.</p>"
            f"{issued}</div>"
            '<div class="l2" id="s8-2"><div class="l2head"><span class="n"></span>'
            "<h3>Internal revisions</h3></div>"
            '<p class="prose">Changes made within the issuing organisation '
            "between issues, numbered under the revision letter they will be "
            "issued as. These have not been separately issued to the client: "
            "they are folded into the next lettered revision above.</p>"
            f"{self._revhist_table(internal_rows)}</div>"
        )

    def _toc(self) -> str:
        entries = [
            ("s1", "Objective"),
            ("s2", "Design data"),
            ("s3", "Analysis methodology"),
            ("s4", "Results" + (" &mdash; preliminary" if self.preliminary else "")),
            ("s5", "Validation status"),
            ("s6", "Way forward"),
            ("s7", "References &amp; provenance"),
        ]
        items = "".join(f'<li><a href="#{k}">{v}</a></li>' for k, v in entries)
        return (
            '<nav class="toc" aria-label="Contents"><p class="tt">Contents</p>'
            f"<ol>{items}</ol></nav>"
        )

    def render_html(self, template: Optional[Path] = None) -> str:
        """Render the complete self-contained report."""
        style, script = _template_parts(template or _DEFAULT_TEMPLATE)

        s1 = self._section("s1", "Objective", "Purpose and scope of this calculation.",
                           self.objective.render())

        design = '<div class="datagrid">' + "".join(
            g.render() for g in self.design_data) + "</div>"
        if self.assumptions:
            design += (
                '<div class="l2" id="s2-2"><div class="l2head"><span class="n"></span>'
                "<h3>Assumptions carried</h3></div>"
                '<p class="prose">Each of the following was assumed, not measured. '
                "Results depending on them are marked accordingly.</p>"
                '<div class="datagrid">'
                + "".join(g.render() for g in self.assumptions)
                + "</div></div>"
            )
        s2 = self._section("s2", "Design data",
                           "Inputs as supplied, and assumptions where a value was not.",
                           design)

        method_html, number = [], 1
        for i, block in enumerate(self.methodology, start=1):
            rendered, number = block.render(i, number)
            method_html.append(rendered)
        s3 = self._section(
            "s3", "Analysis methodology",
            "Governing relations, shown explicitly so a reviewer can check the "
            "arithmetic by hand.",
            "".join(method_html))

        results_title = "Results" + (" &mdash; preliminary" if self.preliminary else "")
        s4 = self._section(
            "s4", results_title,
            "Each result carries the confidence it can support.",
            "".join(r.render(i) for i, r in enumerate(self.results, start=1)))

        s5 = self._section(
            "s5", "Validation status", "Honest provenance for every headline claim.",
            '<div class="status">'
            + "".join(v.render() for v in self.validation)
            + "</div>")

        s6 = self._section(
            "s6", "Way forward",
            "What each additional input unlocks, ordered by how much it changes "
            "the answer.",
            "".join(s.render(i) for i, s in enumerate(self.way_forward, start=1)))

        def _ref_body(r: "Reference") -> str:
            # Only http(s) is linkified: a javascript: or data: URL in a
            # citation field would be an injection vector, and no legitimate
            # reference needs one.
            if r.url and r.url.startswith(("http://", "https://")):
                return (f'<a href="{_esc(r.url)}" rel="noopener noreferrer" '
                        f'target="_blank">{_esc(r.text)}</a>')
            return _esc(r.text)

        refs = "".join(
            f'<li><span class="rn">R{i}</span><span>{_ref_body(r)}</span></li>'
            for i, r in enumerate(self.references, start=1))
        s7 = self._section("s7", "References &amp; provenance",
                           "Methods, data sources, traceability.",
                           f'<ol class="refs">{refs}</ol>')

        if self.revision_history or self.internal_revisions:
            s7 += self._section(
                "s8", "Revision history",
                "What changed, and when.",
                self._revision_history())

        prelim = " &middot; Preliminary" if self.preliminary else ""
        kpis = "".join(k.render() for k in self.kpis)
        logo = _logo_img(self.organisation_logo, self.organisation)
        brand = _wordmark(self.organisation)
        org = html.escape(self.organisation)

        return f"""<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>{html.escape(self.title)} &middot; {html.escape(self.report_id)}</title>
{style}
</head><body>
<div class="doc" id="top">
  <header class="masthead"><div class="wrap" style="max-width:1240px">
    <div class="brand">{logo}{brand} &middot; {html.escape(self.discipline)}</div>
    <div class="mh-legend">
      <span class="lg"><span class="sw" style="background:var(--cfd)"></span>Validated</span>
      <span class="lg"><span class="sw" style="background:var(--ro)"></span>Analytical</span>
      <span class="lg"><span class="sw" style="background:var(--proj)"></span>Pending</span>
    </div>
  </div></header>
  <div class="hero"><div class="wrap" style="max-width:1240px">
    <div class="kicker"><span class="dot"></span><span>Engineering Calculation Report
      &middot; {html.escape(self.report_id)} &middot; Rev {html.escape(self.revision)}{prelim}</span></div>
    <h1>{html.escape(self.title)}</h1>
    <p class="lede">{self.lede}</p>
    <div class="kpis">{kpis}</div>
  </div></div>
  <div class="shell">
    {self._toc()}
    <main class="main">{s1}{s2}{s3}{s4}{s5}{s6}{s7}</main>
  </div>
  <footer><div class="wrap" style="max-width:1240px">
    <div class="foot-grid">
      <div><h3>{html.escape(self.report_id)} &middot; Rev {html.escape(self.revision)}</h3>
        <p>Prepared by {org} {html.escape(self.discipline)}.</p></div>
      <div><h3>Provenance</h3><ul>
        <li><span class="num">Teal</span> &mdash; validated</li>
        <li><span class="num">Amber</span> &mdash; analytical, assumptions carried</li>
        <li><span class="num">Muted</span> &mdash; pending data</li></ul></div>
    </div>
    <div class="foot-bar"><span>{org} &middot; {html.escape(self.discipline)}</span>
      <span>Standard calculation report format &middot; Rev A</span></div>
  </div></footer>
</div>
<div id="tip" role="status" aria-live="polite"></div>
{script}
</body></html>"""

    def write(self, path: Path, template: Optional[Path] = None) -> Path:
        """Render and write the report to disk."""
        path = Path(path)
        path.write_text(self.render_html(template), encoding="utf-8")
        return path


def _template_parts(template: Path) -> tuple:
    """Lift the house <style> and <script> blocks from the format template.

    Reusing them verbatim is what keeps every report visually identical; a
    reimplementation would drift on the first edit.
    """
    import re

    if not template.exists():
        raise FileNotFoundError(
            f"house report format asset not found: {template}. It supplies the "
            "shared CSS and scrollspy; without it reports would diverge visually. "
            "If this is an installed package, the asset may be missing from "
            "package data."
        )
    src = template.read_text(encoding="utf-8")
    style = re.search(r"<style>.*?</style>", src, re.S)
    script = re.search(r"<script>.*?</script>", src, re.S)
    if not style or not script:
        raise ValueError(f"{template} is missing its <style> or <script> block")
    return style.group(0) + _PRINT_CSS, script.group(0)


# The house template targets a 1240px screen. Printed to Letter/A4 the
# two-column data grid overflows the right margin and silently truncates
# values -- a client-facing report losing the right-hand column of its inputs.
# Collapse to one column for print and let long tables break across pages.
_PRINT_CSS = """
<style>
@media print {
  @page { size: Letter; margin: 14mm 12mm; }
  html, body { background: #fff !important; }
  .doc, .wrap, .shell, .main { max-width: 100% !important; width: 100% !important;
                               margin: 0 !important; padding-left: 0 !important;
                               padding-right: 0 !important; }
  .shell { display: block !important; }
  .toc { display: none !important; }          /* anchors are dead on paper */
  .backtop { display: none !important; }
  .datagrid { display: block !important; }
  .datagrid > .kv { width: 100% !important; margin: 0 0 10px 0 !important;
                    break-inside: avoid; page-break-inside: avoid; }
  /* auto, not fixed: `fixed` split every row 50/50, spending half the page
     measure on a short label while the value wrapped beside it. */
  .kv table { width: 100% !important; table-layout: auto !important; }
  .kv td:first-child { white-space: nowrap !important; width: 1% !important; }
  .kv td:last-child { white-space: normal !important; word-break: break-word !important;
                      width: 99% !important; text-align: right; }
  .kpis { display: grid !important; grid-template-columns: 1fr 1fr !important; }
  section { break-inside: auto; }
  .l1head, .l2head { break-after: avoid; page-break-after: avoid; }
  .eq, .defs, .st, .flow, .fig-cap { break-inside: avoid; page-break-inside: avoid; }
  .hero { padding-bottom: 12px !important; }
}
</style>"""


__all__ = [
    "CALC_REPORT_SKELETON",
    "CalcReport",
    "Confidence",
    "DataRow",
    "DesignDataGroup",
    "Equation",
    "InternalRevision",
    "KPI",
    "MethodBlock",
    "Objective",
    "Reference",
    "ResultBlock",
    "RevisionEntry",
    "ValidationItem",
    "VariableDef",
    "WayForwardStage",
]
