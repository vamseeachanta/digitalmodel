#!/usr/bin/env python3
"""Standard AceEngineer client brief — the short-form companion to a calc report.

ABOUTME: A one-purpose client-facing document (data request, scope note,
findings summary) sharing the house visual identity but not the calculation
report's seven-section skeleton.

A calculation report answers a question. A brief *asks* for something, or
states a position. Forcing an ask into the calc-report skeleton would leave
methodology and results sections empty and misrepresent the document, so briefs
get their own small standard:

* the same masthead, typography and provenance colours, so it is recognisably
  the same firm's document;
* free section order, because an ask is not a calculation;
* the same :class:`Confidence` colouring available on callouts, so a brief
  cannot quietly overstate what is known either.

Keep briefs to what is being asked and why. Analysis belongs in the report,
and a brief should link to it rather than restate it — duplicated numbers drift
apart the moment one document is revised.
"""

from __future__ import annotations

import html
from pathlib import Path
from typing import List, Optional

from pydantic import Field

from digitalmodel.reporting._base import ReportDataModel
from digitalmodel.reporting.calc_report import (
    Confidence,
    DesignDataGroup,
    _esc,
    _template_parts,
)

_DEFAULT_TEMPLATE = Path(__file__).parent / "assets" / "calc_report_format.html"


class Callout(ReportDataModel):
    """A highlighted statement, optionally carrying a confidence colour."""

    text: str
    confidence: Optional[Confidence] = None

    def render(self) -> str:
        if self.confidence is None:
            return (
                '<div class="objective" style="margin-top:14px">'
                f"<p>{_esc(self.text)}</p></div>"
            )
        return (
            f'<div class="st {self.confidence.status_class}">'
            f'<div class="icon">{self.confidence.status_icon}</div>'
            f'<div><div class="lab">{_esc(self.text)}</div></div>'
            f'<div class="pill">{self.confidence.label}</div></div>'
        )


class BriefSection(ReportDataModel):
    """One section of a brief: prose, tables and callouts in that order."""

    heading: str
    prose: List[str] = Field(default_factory=list)
    groups: List[DesignDataGroup] = Field(default_factory=list)
    callouts: List[Callout] = Field(default_factory=list)
    subtitle: str = ""

    def render(self, index: int) -> str:
        parts = [
            f'<section id="b{index}"><div class="l1head"><span class="secnum"></span>'
            f"<h2>{_esc(self.heading)}</h2></div>"
        ]
        if self.subtitle:
            parts.append(f'<p class="l1sub">{_esc(self.subtitle)}</p>')
        for paragraph in self.prose:
            parts.append(f'<p class="prose">{_esc(paragraph)}</p>')
        if self.groups:
            parts.append(
                '<div class="datagrid">'
                + "".join(g.render() for g in self.groups)
                + "</div>"
            )
        if self.callouts:
            parts.append(
                '<div class="status">'
                + "".join(c.render() for c in self.callouts)
                + "</div>"
            )
        parts.append("</section>")
        return "".join(parts)


class Brief(ReportDataModel):
    """A short client-facing document in the house identity."""

    brief_id: str
    title: str
    discipline: str
    lede: str
    prepared_for: str = ""
    revision: str = "A"
    sections: List[BriefSection] = Field(default_factory=list)
    footer_note: str = ""

    def render_html(self, template: Optional[Path] = None) -> str:
        style, script = _template_parts(template or _DEFAULT_TEMPLATE)
        body = "".join(s.render(i) for i, s in enumerate(self.sections, start=1))
        prepared = (
            f" &middot; prepared for {html.escape(self.prepared_for)}"
            if self.prepared_for
            else ""
        )
        note = (
            f"<p>{self.footer_note}</p>"
            if self.footer_note
            else ""
        )
        return f"""<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>{html.escape(self.title)} &middot; {html.escape(self.brief_id)}</title>
{style}
</head><body>
<div class="doc" id="top">
  <header class="masthead"><div class="wrap" style="max-width:1240px">
    <div class="brand">Ace<b>Engineer</b> &middot; {html.escape(self.discipline)}</div>
    <div class="mh-legend">
      <span class="lg"><span class="sw" style="background:var(--cfd)"></span>Have</span>
      <span class="lg"><span class="sw" style="background:var(--ro)"></span>Partial</span>
      <span class="lg"><span class="sw" style="background:var(--proj)"></span>Needed</span>
    </div>
  </div></header>
  <div class="hero"><div class="wrap" style="max-width:1240px">
    <div class="kicker"><span class="dot"></span><span>{html.escape(self.brief_id)}
      &middot; Rev {html.escape(self.revision)}{prepared}</span></div>
    <h1>{html.escape(self.title)}</h1>
    <p class="lede">{self.lede}</p>
  </div></div>
  <div class="shell"><main class="main" style="width:100%">{body}</main></div>
  <footer><div class="wrap" style="max-width:1240px">
    <div class="foot-grid">
      <div><h3>{html.escape(self.brief_id)} &middot; Rev {html.escape(self.revision)}</h3>
        {note}</div>
    </div>
    <div class="foot-bar"><span>AceEngineer &middot; {html.escape(self.discipline)}</span>
      <span>Standard client brief &middot; Rev A</span></div>
  </div></footer>
</div>
{script}
</body></html>"""

    def write(self, path: Path, template: Optional[Path] = None) -> Path:
        path = Path(path)
        path.write_text(self.render_html(template), encoding="utf-8")
        return path


__all__ = ["Brief", "BriefSection", "Callout"]
