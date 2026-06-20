#!/usr/bin/env python3
# ABOUTME: Riser report-template generator (deliverable of record).
# ABOUTME: Renders a branded riser-analysis report (HTML + markdown) from a
# ABOUTME: structured riser results object/dict. No live analysis / solver.
"""
Riser Report-Template Generator (deliverable of record)
=======================================================

Turns the structured output of a riser workflow into the standard riser
analysis report -- the deliverable of record (digitalmodel#814).

The generator is deliberately *data-driven*: it consumes a provided/sample
:class:`RiserReportData` structure (or a plain ``dict`` of the same shape)
and renders a branded HTML report plus a markdown twin. It does **not** run
any OrcaFlex / live analysis; the results structure is supplied by the
upstream workflow (A3-A6).

Report outline (mirrors the de-identified historical results-summary report):

    1. Executive summary
    2. Inputs & assumptions
    3. Method & standards
    4. Strength / utilisation summary  (results tables)
    5. Fatigue summary
    6. Code checks
    7. Critical-location findings
    8. Conclusions
    + Provenance stamp (workflow version, standard revision, atlas-vs-custom)

Branding reuses the repo's existing ``GTMReportBuilder`` when importable; if
the example builder is not on the path the generator falls back to a small,
self-contained HTML renderer that emits the same sections. Either way the
public API and the rendered section content are identical and deterministic.

Usage::

    from digitalmodel.subsea.catenary_riser.report import (
        generate_riser_report, sample_riser_report_data,
    )

    data = sample_riser_report_data()
    generate_riser_report(data, "output/riser_report.html")
"""

from __future__ import annotations

import html as _html
import sys
from dataclasses import dataclass, field, asdict, is_dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Optional, Union


# ---------------------------------------------------------------------------
# Provenance / labelling
# ---------------------------------------------------------------------------

#: Maps the screening (atlas) vs certified (custom) flag to a display label.
PROVENANCE_LABELS = {
    "atlas": "SCREENING (atlas)",
    "custom": "CERTIFIED (custom)",
}


def _status_of(util: Optional[float], allowable: float = 1.0,
               marginal_frac: float = 0.9) -> str:
    """Classify a utilisation ratio as PASS / MARGINAL / FAIL."""
    if util is None:
        return "N/A"
    if util > allowable:
        return "FAIL"
    if util > marginal_frac * allowable:
        return "MARGINAL"
    return "PASS"


# ---------------------------------------------------------------------------
# Input schema (the structured riser results consumed by the report)
# ---------------------------------------------------------------------------

@dataclass
class CodeCheck:
    """A single design-code check (e.g. DNV-OS-F201 LRFD combined load)."""
    name: str
    standard: str          # e.g. "DNV-OS-F201:2010"
    location: str          # e.g. "Touchdown point"
    utilisation: float     # fraction of allowable (1.0 == at limit)
    allowable: float = 1.0
    method: str = ""       # e.g. "LRFD combined loading, sec. 5.2"

    @property
    def status(self) -> str:
        return _status_of(self.utilisation, self.allowable)


@dataclass
class StrengthResult:
    """Strength / utilisation result at a named location along the riser."""
    location: str
    arc_length_m: float
    effective_tension_kN: float
    bending_moment_kNm: float
    von_mises_stress_MPa: float
    utilisation: float
    allowable: float = 1.0

    @property
    def status(self) -> str:
        return _status_of(self.utilisation, self.allowable)


@dataclass
class FatigueResult:
    """Fatigue summary at a named location (S-N based)."""
    location: str
    arc_length_m: float
    damage_per_year: float
    design_life_years: float
    sn_curve: str = "DNV-RP-C203 F1"
    scf: float = 1.0

    @property
    def fatigue_life_years(self) -> Optional[float]:
        if self.damage_per_year <= 0:
            return None
        return 1.0 / self.damage_per_year

    @property
    def status(self) -> str:
        life = self.fatigue_life_years
        if life is None:
            return "PASS"
        # design life must be covered with the design fatigue factor headroom
        if life < self.design_life_years:
            return "FAIL"
        if life < 2.0 * self.design_life_years:
            return "MARGINAL"
        return "PASS"


@dataclass
class Provenance:
    """Provenance stamp for the deliverable of record."""
    workflow: str = "riser-analysis"
    workflow_version: str = "0.0.0"
    standard_revision: str = ""
    #: "atlas" == screening (pre-computed atlas), "custom" == certified run.
    mode: str = "atlas"
    run_id: str = ""
    generated_utc: str = ""

    @property
    def mode_label(self) -> str:
        return PROVENANCE_LABELS.get(self.mode, self.mode.upper())


@dataclass
class RiserReportData:
    """Structured riser results -- the input to the report generator.

    This is the contract between any riser workflow (A3-A6) and the
    deliverable-of-record report. Every field is plain data; no behaviour.
    """
    project: str
    riser_name: str
    riser_type: str                       # e.g. "Steel Catenary Riser (SCR)"
    water_depth_m: float

    executive_summary: str = ""
    inputs: dict[str, Any] = field(default_factory=dict)
    assumptions: list[str] = field(default_factory=list)
    standards: list[str] = field(default_factory=list)
    method_summary: str = ""

    strength_results: list[StrengthResult] = field(default_factory=list)
    fatigue_results: list[FatigueResult] = field(default_factory=list)
    code_checks: list[CodeCheck] = field(default_factory=list)

    critical_findings: list[str] = field(default_factory=list)
    conclusions: list[str] = field(default_factory=list)

    provenance: Provenance = field(default_factory=Provenance)

    # ---- derived roll-ups ------------------------------------------------

    @property
    def max_strength_utilisation(self) -> Optional[float]:
        if not self.strength_results:
            return None
        return max(r.utilisation for r in self.strength_results)

    @property
    def min_fatigue_life_years(self) -> Optional[float]:
        lives = [r.fatigue_life_years for r in self.fatigue_results
                 if r.fatigue_life_years is not None]
        return min(lives) if lives else None

    @property
    def overall_status(self) -> str:
        """Worst-case status across strength, fatigue and code checks."""
        statuses = (
            [r.status for r in self.strength_results]
            + [r.status for r in self.fatigue_results]
            + [c.status for c in self.code_checks]
        )
        if any(s == "FAIL" for s in statuses):
            return "FAIL"
        if any(s == "MARGINAL" for s in statuses):
            return "MARGINAL"
        if statuses:
            return "PASS"
        return "N/A"


def _coerce(data: Union["RiserReportData", dict[str, Any]]) -> "RiserReportData":
    """Accept a RiserReportData or a plain dict of the same shape."""
    if isinstance(data, RiserReportData):
        return data
    if not isinstance(data, dict):
        raise TypeError(
            f"riser report data must be RiserReportData or dict, got {type(data)!r}"
        )
    d = dict(data)
    d["strength_results"] = [
        s if isinstance(s, StrengthResult) else StrengthResult(**s)
        for s in d.get("strength_results", [])
    ]
    d["fatigue_results"] = [
        f if isinstance(f, FatigueResult) else FatigueResult(**f)
        for f in d.get("fatigue_results", [])
    ]
    d["code_checks"] = [
        c if isinstance(c, CodeCheck) else CodeCheck(**c)
        for c in d.get("code_checks", [])
    ]
    prov = d.get("provenance")
    if prov is not None and not isinstance(prov, Provenance):
        d["provenance"] = Provenance(**prov)
    return RiserReportData(**d)


# ---------------------------------------------------------------------------
# Sample / synthetic results (so the generator runs with no live analysis)
# ---------------------------------------------------------------------------

def sample_riser_report_data() -> RiserReportData:
    """A deterministic, synthetic riser results structure for demos/tests.

    Represents a 12" steel catenary riser in 1500 m water depth, screened
    against the atlas (so labelled SCREENING). All numbers are illustrative.
    """
    return RiserReportData(
        project="Deepwater SCR Concept Screening",
        riser_name="SCR-12in-WI",
        riser_type="Steel Catenary Riser (SCR)",
        water_depth_m=1500.0,
        executive_summary=(
            "The 12-inch water-injection steel catenary riser was screened for "
            "strength and fatigue in 1500 m water depth. The governing strength "
            "utilisation of 0.82 occurs at the hang-off, and the minimum fatigue "
            "life of 41 years at the touchdown point exceeds the 25-year design "
            "life with a design fatigue factor of 1.6. The configuration is "
            "feasible at screening level and is recommended for certified analysis."
        ),
        inputs={
            "Outer diameter": "12.75 in (323.9 mm)",
            "Wall thickness": "22.2 mm",
            "Material": "API 5L X65",
            "Content": "Water injection (1025 kg/m^3)",
            "Top angle": "12 deg from vertical",
            "Design pressure": "345 bar",
            "Vessel offset (far)": "8% of water depth",
        },
        assumptions=[
            "Quasi-static screening using atlas response surfaces; no irregular-sea dynamics.",
            "Soil modelled as linear elastic seabed, stiffness 50 kN/m/m.",
            "Marine growth and current profiles per metocean basis of design rev B.",
            "Corrosion allowance of 3 mm included in the strength wall thickness.",
        ],
        standards=[
            "DNV-OS-F201 (2010) - Dynamic Risers",
            "DNV-RP-C203 - Fatigue Design of Offshore Steel Structures",
            "API RP 1111 - Design of Offshore Hydrocarbon Pipelines",
        ],
        method_summary=(
            "Effective tension and bending moment envelopes were extracted from "
            "the pre-computed response atlas for the screened metocean and vessel "
            "offset cases. Combined-loading utilisation was evaluated per "
            "DNV-OS-F201 LRFD. Fatigue damage was accumulated from the atlas "
            "stress-range histograms using DNV-RP-C203 S-N curves with the "
            "stated stress concentration factors."
        ),
        strength_results=[
            StrengthResult("Hang-off", 0.0, 1840.0, 95.0, 412.0, 0.82),
            StrengthResult("Sag bend", 920.0, 760.0, 188.0, 388.0, 0.77),
            StrengthResult("Touchdown point", 1610.0, 540.0, 156.0, 301.0, 0.60),
        ],
        fatigue_results=[
            FatigueResult("Hang-off", 0.0, 1.0 / 120.0, 25.0, "DNV-RP-C203 F1", 1.15),
            FatigueResult("Touchdown point", 1610.0, 1.0 / 41.0, 25.0, "DNV-RP-C203 F3", 1.30),
        ],
        code_checks=[
            CodeCheck("Combined loading (LRFD)", "DNV-OS-F201:2010",
                      "Hang-off", 0.82, 1.0, "LRFD combined load, sec. 5.2"),
            CodeCheck("Burst (pressure containment)", "API RP 1111",
                      "Hang-off", 0.71, 1.0, "Internal pressure, sec. 4.3.1"),
            CodeCheck("Local buckling (collapse)", "DNV-OS-F201:2010",
                      "Touchdown point", 0.64, 1.0, "External pressure, sec. 5.3"),
        ],
        critical_findings=[
            "Governing strength location is the hang-off (utilisation 0.82) under "
            "far vessel offset.",
            "Minimum fatigue life of 41 years is at the touchdown point, governed "
            "by the F3 weld detail and seabed interaction.",
        ],
        conclusions=[
            "All screened code checks pass with margin; no check exceeds 0.82 of allowable.",
            "The configuration is feasible at screening level (atlas).",
            "Certified custom analysis with vessel-specific RAOs is recommended "
            "before the deliverable is issued for design.",
        ],
        provenance=Provenance(
            workflow="riser-strength-fatigue-screening",
            workflow_version="0.3.0",
            standard_revision="DNV-OS-F201:2010 / DNV-RP-C203:2021",
            mode="atlas",
            run_id="screen-0001",
            generated_utc="",  # filled at render time if blank
        ),
    )


# ---------------------------------------------------------------------------
# HTML section assembly (builder-agnostic)
# ---------------------------------------------------------------------------

def _try_import_builder():
    """Return the repo's GTMReportBuilder if importable, else None."""
    try:
        from report_template import GTMReportBuilder  # type: ignore
        return GTMReportBuilder
    except Exception:
        pass
    # Try the examples path relative to the repo root.
    here = Path(__file__).resolve()
    for parent in here.parents:
        cand = parent / "examples" / "demos" / "gtm"
        if (cand / "report_template.py").exists():
            sys.path.insert(0, str(cand))
            try:
                from report_template import GTMReportBuilder  # type: ignore
                return GTMReportBuilder
            except Exception:
                return None
    return None


def _esc(text: Any) -> str:
    return _html.escape(str(text))


def _kv_table_html(mapping: dict[str, Any]) -> str:
    rows = "".join(
        f"<tr><td><strong>{_esc(k)}</strong></td><td>{_esc(v)}</td></tr>"
        for k, v in mapping.items()
    )
    return f'<table class="data-table"><tbody>{rows}</tbody></table>'


def _list_html(items: list[str]) -> str:
    if not items:
        return "<p><em>None recorded.</em></p>"
    return "<ul>" + "".join(f"<li>{_esc(i)}</li>" for i in items) + "</ul>"


def _strength_table_html(rows: list[StrengthResult]) -> str:
    if not rows:
        return "<p><em>No strength results provided.</em></p>"
    head = (
        "<tr><th>Location</th><th>Arc length (m)</th>"
        "<th>Eff. tension (kN)</th><th>Bending (kN&middot;m)</th>"
        "<th>von Mises (MPa)</th><th>Utilisation</th><th>Status</th></tr>"
    )
    body = ""
    for r in rows:
        body += (
            f"<tr><td>{_esc(r.location)}</td><td>{r.arc_length_m:.1f}</td>"
            f"<td>{r.effective_tension_kN:.1f}</td><td>{r.bending_moment_kNm:.1f}</td>"
            f"<td>{r.von_mises_stress_MPa:.1f}</td><td>{r.utilisation:.2f}</td>"
            f"<td>{_status_span(r.status)}</td></tr>"
        )
    return f'<table class="data-table"><thead>{head}</thead><tbody>{body}</tbody></table>'


def _fatigue_table_html(rows: list[FatigueResult]) -> str:
    if not rows:
        return "<p><em>No fatigue results provided.</em></p>"
    head = (
        "<tr><th>Location</th><th>Arc length (m)</th><th>S-N curve</th>"
        "<th>SCF</th><th>Damage/yr</th><th>Fatigue life (yr)</th>"
        "<th>Design life (yr)</th><th>Status</th></tr>"
    )
    body = ""
    for r in rows:
        life = r.fatigue_life_years
        life_s = "&infin;" if life is None else f"{life:.1f}"
        body += (
            f"<tr><td>{_esc(r.location)}</td><td>{r.arc_length_m:.1f}</td>"
            f"<td>{_esc(r.sn_curve)}</td><td>{r.scf:.2f}</td>"
            f"<td>{r.damage_per_year:.4f}</td><td>{life_s}</td>"
            f"<td>{r.design_life_years:.0f}</td>"
            f"<td>{_status_span(r.status)}</td></tr>"
        )
    return f'<table class="data-table"><thead>{head}</thead><tbody>{body}</tbody></table>'


def _code_check_table_html(rows: list[CodeCheck]) -> str:
    if not rows:
        return "<p><em>No code checks provided.</em></p>"
    head = (
        "<tr><th>Check</th><th>Standard</th><th>Method</th>"
        "<th>Location</th><th>Utilisation</th><th>Status</th></tr>"
    )
    body = ""
    for c in rows:
        body += (
            f"<tr><td>{_esc(c.name)}</td><td>{_esc(c.standard)}</td>"
            f"<td>{_esc(c.method)}</td><td>{_esc(c.location)}</td>"
            f"<td>{c.utilisation:.2f}</td><td>{_status_span(c.status)}</td></tr>"
        )
    return f'<table class="data-table"><thead>{head}</thead><tbody>{body}</tbody></table>'


def _status_span(status: str) -> str:
    cls = {
        "PASS": "status-pass",
        "FAIL": "status-fail",
        "MARGINAL": "status-marginal",
    }.get(status, "")
    return f'<span class="{cls}">{status}</span>' if cls else status


def _provenance_html(p: Provenance) -> str:
    mapping = {
        "Workflow": f"{p.workflow} v{p.workflow_version}",
        "Standard revision": p.standard_revision or "—",
        "Mode": p.mode_label,
        "Run ID": p.run_id or "—",
        "Generated (UTC)": p.generated_utc or "—",
    }
    return _kv_table_html(mapping)


# Section order constant -- the standard riser report outline.
SECTION_ORDER = [
    "Executive Summary",
    "Inputs & Assumptions",
    "Method & Standards",
    "Strength / Utilisation Summary",
    "Fatigue Summary",
    "Code Checks",
    "Critical-Location Findings",
    "Conclusions",
    "Provenance",
]


def _build_sections(d: RiserReportData) -> list[tuple[str, str]]:
    """Return ordered (title, html) section pairs."""
    overview = _kv_table_html({
        "Project": d.project,
        "Riser": d.riser_name,
        "Type": d.riser_type,
        "Water depth": f"{d.water_depth_m:.0f} m",
        "Overall status": _status_span(d.overall_status),
        "Max strength utilisation": (
            f"{d.max_strength_utilisation:.2f}"
            if d.max_strength_utilisation is not None else "—"
        ),
        "Min fatigue life": (
            f"{d.min_fatigue_life_years:.1f} yr"
            if d.min_fatigue_life_years is not None else "—"
        ),
        "Classification": d.provenance.mode_label,
    })
    exec_html = (
        overview
        + (f"<p>{_esc(d.executive_summary)}</p>" if d.executive_summary else "")
    )

    inputs_html = (
        "<h3>Inputs</h3>"
        + (_kv_table_html(d.inputs) if d.inputs else "<p><em>No inputs provided.</em></p>")
        + "<h3>Assumptions</h3>" + _list_html(d.assumptions)
    )

    method_html = (
        (f"<p>{_esc(d.method_summary)}</p>" if d.method_summary else "")
        + "<h3>Applicable Standards</h3>" + _list_html(d.standards)
    )

    return [
        ("Executive Summary", exec_html),
        ("Inputs & Assumptions", inputs_html),
        ("Method & Standards", method_html),
        ("Strength / Utilisation Summary", _strength_table_html(d.strength_results)),
        ("Fatigue Summary", _fatigue_table_html(d.fatigue_results)),
        ("Code Checks", _code_check_table_html(d.code_checks)),
        ("Critical-Location Findings", _list_html(d.critical_findings)),
        ("Conclusions", _list_html(d.conclusions)),
        ("Provenance", _provenance_html(d.provenance)),
    ]


# ---------------------------------------------------------------------------
# Renderers
# ---------------------------------------------------------------------------

_FALLBACK_CSS = """
body{font-family:Inter,-apple-system,Segoe UI,Roboto,sans-serif;color:#2d3748;
background:#f7fafc;line-height:1.6;font-size:15px;margin:0;}
.report-header{background:linear-gradient(135deg,#1a365d,#2c5282);color:#fff;
padding:2.5rem 3rem;}
.report-header .brand{font-size:.85rem;letter-spacing:2px;text-transform:uppercase;
opacity:.8;}
.report-header h1{font-size:1.8rem;margin:.3rem 0;}
.report-body{max-width:1100px;margin:0 auto;padding:2rem 3rem;}
.section{margin-bottom:2rem;}
.section h2{font-size:1.3rem;color:#1a365d;border-bottom:2px solid #ed8936;
padding-bottom:.4rem;margin-bottom:1rem;}
.section h3{font-size:1.05rem;color:#2c5282;margin:1rem 0 .5rem;}
.data-table{width:100%;border-collapse:collapse;font-size:.88rem;margin-bottom:1rem;}
.data-table th{background:#1a365d;color:#fff;padding:.5rem .8rem;text-align:left;}
.data-table td{padding:.5rem .8rem;border-bottom:1px solid #e2e8f0;}
.status-pass{color:#38a169;font-weight:700;}
.status-fail{color:#e53e3e;font-weight:700;}
.status-marginal{color:#d69e2e;font-weight:700;}
.report-footer{border-top:2px solid #e2e8f0;padding:1.5rem 3rem;max-width:1100px;
margin:0 auto;font-size:.8rem;color:#718096;}
@media print{.chart-container,.section{break-inside:avoid;}}
"""


def _render_fallback_html(d: RiserReportData, sections: list[tuple[str, str]],
                          timestamp: str) -> str:
    body = ""
    for title, content in sections:
        body += f'<div class="section"><h2>{_esc(title)}</h2>{content}</div>\n'
    return f"""<!DOCTYPE html>
<html lang="en"><head><meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>{_esc(d.riser_name)} Riser Analysis Report — digitalmodel</title>
<style>{_FALLBACK_CSS}</style></head>
<body>
<div class="report-header">
  <div class="brand">digitalmodel — Engineering Intelligence</div>
  <h1>{_esc(d.riser_name)} — Riser Analysis Report</h1>
  <div class="subtitle">{_esc(d.riser_type)} · {_esc(d.project)}</div>
  <div class="meta">Generated {timestamp} · {_esc(d.provenance.mode_label)}</div>
</div>
<div class="report-body">
{body}</div>
<div class="report-footer">
  <p class="disclaimer"><em>Preliminary engineering estimates. All outputs require
  review by a qualified engineer before use in design or operational decisions.</em></p>
  <p>References: {_esc(' · '.join(d.standards))}</p>
  <p>&copy; {datetime.now().year} digitalmodel · aceengineer.com</p>
</div>
</body></html>"""


def render_html(data: Union[RiserReportData, dict[str, Any]]) -> str:
    """Render the riser report as a complete HTML string (no file write)."""
    d = _coerce(data)
    timestamp = datetime.now(timezone.utc).strftime("%Y-%m-%d %H:%M UTC")
    if not d.provenance.generated_utc:
        d.provenance.generated_utc = timestamp
    sections = _build_sections(d)

    Builder = _try_import_builder()
    if Builder is not None:
        try:
            report = Builder(
                title=f"{d.riser_name} — Riser Analysis Report",
                subtitle=f"{d.riser_type} · {d.project}",
                demo_id=d.provenance.run_id or "riser-report",
                case_count=len(d.strength_results) + len(d.fatigue_results),
                code_refs=list(d.standards),
            )
            for title, content in sections:
                report.add_section(title, content)
            # build() needs a path; render to a temp string instead by
            # reusing its internal assembly via a throwaway path.
            import tempfile
            with tempfile.TemporaryDirectory() as td:
                return report.build(Path(td) / "riser_report.html")
        except Exception:
            # Any incompatibility -> deterministic fallback renderer.
            pass
    return _render_fallback_html(d, sections, timestamp)


def render_markdown(data: Union[RiserReportData, dict[str, Any]]) -> str:
    """Render the riser report as markdown (deliverable twin)."""
    d = _coerce(data)
    timestamp = datetime.now(timezone.utc).strftime("%Y-%m-%d %H:%M UTC")
    if not d.provenance.generated_utc:
        d.provenance.generated_utc = timestamp

    lines: list[str] = []
    lines.append(f"# {d.riser_name} — Riser Analysis Report")
    lines.append("")
    lines.append(f"*{d.riser_type} · {d.project}*  ")
    lines.append(f"*Generated {timestamp} · {d.provenance.mode_label}*")
    lines.append("")

    lines.append("## Executive Summary")
    lines.append("")
    lines.append(f"- **Project:** {d.project}")
    lines.append(f"- **Riser:** {d.riser_name} ({d.riser_type})")
    lines.append(f"- **Water depth:** {d.water_depth_m:.0f} m")
    lines.append(f"- **Overall status:** {d.overall_status}")
    if d.max_strength_utilisation is not None:
        lines.append(f"- **Max strength utilisation:** {d.max_strength_utilisation:.2f}")
    if d.min_fatigue_life_years is not None:
        lines.append(f"- **Min fatigue life:** {d.min_fatigue_life_years:.1f} yr")
    lines.append(f"- **Classification:** {d.provenance.mode_label}")
    lines.append("")
    if d.executive_summary:
        lines.append(d.executive_summary)
        lines.append("")

    lines.append("## Inputs & Assumptions")
    lines.append("")
    if d.inputs:
        for k, v in d.inputs.items():
            lines.append(f"- **{k}:** {v}")
        lines.append("")
    lines.append("**Assumptions**")
    lines.append("")
    for a in d.assumptions:
        lines.append(f"- {a}")
    lines.append("")

    lines.append("## Method & Standards")
    lines.append("")
    if d.method_summary:
        lines.append(d.method_summary)
        lines.append("")
    lines.append("**Applicable standards**")
    lines.append("")
    for s in d.standards:
        lines.append(f"- {s}")
    lines.append("")

    lines.append("## Strength / Utilisation Summary")
    lines.append("")
    if d.strength_results:
        lines.append("| Location | Arc (m) | Eff. tension (kN) | Bending (kN·m) | "
                     "von Mises (MPa) | Utilisation | Status |")
        lines.append("|---|---|---|---|---|---|---|")
        for r in d.strength_results:
            lines.append(
                f"| {r.location} | {r.arc_length_m:.1f} | {r.effective_tension_kN:.1f} "
                f"| {r.bending_moment_kNm:.1f} | {r.von_mises_stress_MPa:.1f} "
                f"| {r.utilisation:.2f} | {r.status} |"
            )
    else:
        lines.append("_No strength results provided._")
    lines.append("")

    lines.append("## Fatigue Summary")
    lines.append("")
    if d.fatigue_results:
        lines.append("| Location | Arc (m) | S-N curve | SCF | Damage/yr | "
                     "Fatigue life (yr) | Design life (yr) | Status |")
        lines.append("|---|---|---|---|---|---|---|---|")
        for r in d.fatigue_results:
            life = r.fatigue_life_years
            life_s = "inf" if life is None else f"{life:.1f}"
            lines.append(
                f"| {r.location} | {r.arc_length_m:.1f} | {r.sn_curve} | {r.scf:.2f} "
                f"| {r.damage_per_year:.4f} | {life_s} | {r.design_life_years:.0f} "
                f"| {r.status} |"
            )
    else:
        lines.append("_No fatigue results provided._")
    lines.append("")

    lines.append("## Code Checks")
    lines.append("")
    if d.code_checks:
        lines.append("| Check | Standard | Method | Location | Utilisation | Status |")
        lines.append("|---|---|---|---|---|---|")
        for c in d.code_checks:
            lines.append(
                f"| {c.name} | {c.standard} | {c.method} | {c.location} "
                f"| {c.utilisation:.2f} | {c.status} |"
            )
    else:
        lines.append("_No code checks provided._")
    lines.append("")

    lines.append("## Critical-Location Findings")
    lines.append("")
    for f_ in (d.critical_findings or ["_None recorded._"]):
        lines.append(f"- {f_}")
    lines.append("")

    lines.append("## Conclusions")
    lines.append("")
    for c in (d.conclusions or ["_None recorded._"]):
        lines.append(f"- {c}")
    lines.append("")

    lines.append("## Provenance")
    lines.append("")
    p = d.provenance
    lines.append(f"- **Workflow:** {p.workflow} v{p.workflow_version}")
    lines.append(f"- **Standard revision:** {p.standard_revision or '—'}")
    lines.append(f"- **Mode:** {p.mode_label}")
    lines.append(f"- **Run ID:** {p.run_id or '—'}")
    lines.append(f"- **Generated (UTC):** {p.generated_utc or '—'}")
    lines.append("")

    return "\n".join(lines)


def generate_riser_report(
    data: Union[RiserReportData, dict[str, Any]],
    output_path: Union[str, Path],
    *,
    markdown: bool = True,
) -> dict[str, Path]:
    """Render the riser report and write it to disk.

    Parameters
    ----------
    data:
        A :class:`RiserReportData` (or dict of the same shape) -- the
        structured output of a riser workflow. No live analysis is run.
    output_path:
        Destination for the HTML report. Parent dirs are created.
    markdown:
        If True (default), also write a ``.md`` twin next to the HTML.

    Returns
    -------
    dict mapping ``"html"`` (and optionally ``"markdown"``) to written paths.
    """
    output_path = Path(output_path)
    output_path.parent.mkdir(parents=True, exist_ok=True)

    html_str = render_html(data)
    output_path.write_text(html_str, encoding="utf-8")
    written = {"html": output_path}

    if markdown:
        md_path = output_path.with_suffix(".md")
        md_path.write_text(render_markdown(data), encoding="utf-8")
        written["markdown"] = md_path

    return written


__all__ = [
    "CodeCheck",
    "StrengthResult",
    "FatigueResult",
    "Provenance",
    "RiserReportData",
    "PROVENANCE_LABELS",
    "SECTION_ORDER",
    "sample_riser_report_data",
    "render_html",
    "render_markdown",
    "generate_riser_report",
]


if __name__ == "__main__":  # pragma: no cover
    out = generate_riser_report(
        sample_riser_report_data(),
        "output/riser_report.html",
    )
    for kind, path in out.items():
        print(f"[riser-report] wrote {kind}: {path}")
