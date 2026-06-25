#!/usr/bin/env python3
"""
Diffraction Report Generator — Shim + Orchestration

ABOUTME: Backward-compatible shim that re-exports from focused sub-modules
and keeps the top-level orchestration functions.

Sub-modules (WRK-591 split):
  - report_data_models: Pydantic models and constants
  - report_computations: Derived-data computation functions
  - report_extractors: OrcFxAPI data extraction
  - report_builders: HTML section builders

Version: 2.0.0 (post-split)
"""

from __future__ import annotations

import html
from pathlib import Path
from typing import TYPE_CHECKING, Any, Dict, List, Optional

from digitalmodel.reporting import (
    ReportBackbone,
    ReportRenderer,
    ReportSection,
    SectionMode,
)

# --- Re-exports from report_data_models ---
from digitalmodel.hydrodynamics.diffraction.report_data_models import (
    DOF_NAMES,
    DOF_UNITS,
    LOAD_RAO_UNITS,
    DiffractionReportData,
    HydrostaticData,
    LoadRAOData,
    MeshQualityData,
    RollDampingData,
)

# --- Re-exports from report_computations ---
from digitalmodel.hydrodynamics.diffraction.report_computations import (
    _find_closest_idx,
    compute_coupling_significance,
    compute_natural_periods,
    compute_peak_responses,
    compute_radii_of_gyration,
    compute_stability,
    generate_executive_warnings,
)

# --- Re-exports from report_extractors (solver/ subpackage) ---
# These functions live in solver/ because extract_report_data_from_owr
# requires OrcFxAPI. Wrapped in try/except for license-free environments.
try:
    from digitalmodel.hydrodynamics.diffraction.solver.report_extractors import (
        _round_2d,
        build_report_data_from_solver_results,
        extract_report_data_from_owr,
    )
except ImportError:
    _round_2d = None  # type: ignore[assignment]
    build_report_data_from_solver_results = None  # type: ignore[assignment]
    extract_report_data_from_owr = None  # type: ignore[assignment]

# --- Re-exports from report_builders ---
from digitalmodel.hydrodynamics.diffraction.report_builders import (
    HULL_TYPE_NOTES,
    _build_added_mass_diagonal_html,
    _build_appendices_html,
    _build_coupling_assessment_html,
    _build_damping_diagonal_html,
    _build_executive_summary_html,
    _build_header_html,
    _build_hull_description_html,
    _build_infinite_added_mass_html,
    _build_load_raos_html,
    _build_natural_periods_html,
    _build_phase_guide_html,
    _build_roll_damping_html,
    _build_stability_html,
    _build_toc_html,
    _get_hull_type_note,
)

if TYPE_CHECKING:
    from digitalmodel.hydrodynamics.diffraction.assumption_ledger import (
        AssumptionLedger,
    )


# ---------------------------------------------------------------------------
# Orchestration: generate_diffraction_report
# ---------------------------------------------------------------------------


def _build_validation_sanity_html(validation_report: dict[str, Any]) -> str:
    """Render a sanity-check section from a pre-computed validation report.

    Consumes an existing validation report dict (as produced by
    ``OutputValidator`` / ``validation_runner``). This function NEVER re-runs
    validation — it only renders what it is given, so report generation stays
    side-effect free (#625).

    The expected shape is the ``OutputValidator`` report: a flat
    ``overall_status`` string plus category dicts whose values are
    ``list[str]`` issues (or nested category -> ``list[str]``).
    """
    verdict = str(validation_report.get("overall_status", "UNKNOWN"))
    badge_colors = {
        "PASS": "#27ae60",
        "WARNING": "#f39c12",
        "FAIL": "#c0392b",
        "ERROR": "#c0392b",
        "SKIPPED": "#7f8c8d",
    }
    badge_color = badge_colors.get(verdict, "#7f8c8d")

    # Flatten issues by category for a compact summary table.
    rows: list[str] = []
    total = 0
    for category, value in validation_report.items():
        if category in ("overall_status", "vessel_name", "analysis_tool",
                        "validation_date"):
            continue
        flat: list[str] = []
        if isinstance(value, list):
            flat = [str(v) for v in value]
        elif isinstance(value, dict):
            for sub, sub_issues in value.items():
                if isinstance(sub_issues, list):
                    flat.extend(f"{sub}: {v}" for v in sub_issues)
        if not flat:
            continue
        total += len(flat)
        sample = "<br>".join(flat[:3])
        if len(flat) > 3:
            sample += f"<br><em>... and {len(flat) - 3} more</em>"
        rows.append(
            f"<tr><td>{category}</td><td>{len(flat)}</td><td>{sample}</td></tr>"
        )

    if rows:
        issue_table = (
            "<table><thead><tr><th>Category</th><th>Count</th>"
            "<th>Representative issues</th></tr></thead><tbody>"
            + "".join(rows)
            + "</tbody></table>"
        )
    else:
        issue_table = "<p>No validation issues recorded.</p>"

    return (
        '<div class="section" id="validation-sanity">'
        "<h2>Validation Sanity Check</h2>"
        '<p>Overall verdict: '
        f'<span style="display:inline-block;padding:3px 12px;border-radius:4px;'
        f'color:#fff;font-weight:700;background:{badge_color}">{verdict}</span> '
        f"&nbsp;({total} issue(s))</p>"
        f"{issue_table}"
        "</div>"
    )


def _build_assumptions_html(ledger: "AssumptionLedger") -> str:
    """Render the non-user-supplied assumption ledger."""
    if not ledger:
        body = "<p>No assumed values — all inputs were user-supplied.</p>"
    else:
        rows = []
        for record in ledger.rows():
            value = html.escape(str(record.value))
            reference = html.escape(str(record.reference or ""))
            rows.append(
                "<tr>"
                f"<td>{html.escape(record.field)}</td>"
                f"<td>{value}</td>"
                f"<td>{html.escape(record.source.value)}</td>"
                f"<td>{html.escape(record.basis)}</td>"
                f"<td>{reference}</td>"
                f"<td>{html.escape(record.confidence.value)}</td>"
                "</tr>"
            )
        body = (
            "<table><thead><tr><th>Field</th><th>Value</th>"
            "<th>Source</th><th>Basis</th><th>Reference</th>"
            "<th>Confidence</th></tr></thead><tbody>"
            + "".join(rows)
            + "</tbody></table>"
        )
    return (
        '<div class="section" id="assumptions">'
        "<h2>Assumptions</h2>"
        f"{body}"
        "</div>"
    )


# ---------------------------------------------------------------------------
# Report block library adoption (#1018)
#
# The section ordering / conditional rules below are expressed declaratively
# via a module-level ``ReportBackbone``; the document wrapper is assembled via
# ``ReportRenderer``. Builder bodies are NOT modified — each section adapter is
# a thin wrapper that swallows backbone kwargs and forwards to the existing
# ``_build_*_html`` builder. Output is byte-identical to the pre-library code
# (proven by tests/.../test_report_generator_golden.py).
# ---------------------------------------------------------------------------

# Exact CSS from the original wrapper (base, always present).
_BASE_CSS = """\
  * { box-sizing: border-box; }
  body {
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI',
                 Roboto, Arial, sans-serif;
    margin: 0; padding: 0; color: #333; background: #f8f9fa;
    font-size: 14px; line-height: 1.5;
  }
  .container { max-width: 1400px; margin: 0 auto; padding: 1.5em 2em; }
  .report-header {
    background: #2c3e50; color: #fff; padding: 1.2em 2em;
    margin-bottom: 1.5em; border-radius: 6px;
  }
  .report-header h1 { margin: 0 0 0.3em; font-size: 1.6em; }
  .report-header .subtitle { font-size: 1.1em; opacity: 0.9; margin-bottom: 0.5em; font-weight: 300; }
  .report-header .meta { font-size: 0.9em; opacity: 0.85; }
  .nav-bar {
    margin-top: 1.2em; padding-top: 1em; border-top: 1px solid rgba(255,255,255,0.15);
    display: flex; gap: 1.5em; font-size: 0.9em;
  }
  .nav-bar a { color: #fff; text-decoration: none; opacity: 0.8; }
  .nav-bar a:hover { opacity: 1; text-decoration: underline; }
  .nav-bar .active { opacity: 1; font-weight: 600; border-bottom: 2px solid #3498db; }
  .section {
    background: #fff; border-radius: 6px;
    box-shadow: 0 1px 3px rgba(0,0,0,0.08);
    margin-bottom: 1.5em; padding: 1.2em 1.5em;
  }
  .section h2 {
    margin: 0 0 0.8em; font-size: 1.2em; color: #2c3e50;
    border-bottom: 2px solid #3498db; padding-bottom: 0.3em;
  }
  table {
    border-collapse: collapse; margin: 0.5em 0; font-size: 0.85em;
    width: 100%;
  }
  th, td {
    border: 1px solid #ddd; padding: 0.45em 0.7em; text-align: left;
  }
  th {
    background: #34495e; color: #fff; font-weight: 600;
    font-size: 0.85em; text-transform: uppercase; letter-spacing: 0.3px;
  }
  tbody tr:nth-child(even) { background: #f8f9fa; }
  tbody tr:hover { background: #ebf5fb; }
  td { vertical-align: top; font-family: 'Cascadia Code', 'Fira Code', monospace; }
  .matrix-table td { text-align: right; padding: 0.3em 0.5em; font-size: 0.8em; }
  .matrix-table th { text-align: center; padding: 0.3em 0.5em; font-size: 0.8em; }
  .highlight { background: #ffeaa7 !important; font-weight: 600; }
  .plot-container { margin: 1em 0; }
"""

# Exact benchmark-only CSS (appended only when benchmark sections present).
_BENCHMARK_CSS = """
  /* Benchmark-specific styles */
  .report-header .consensus-overall {
    display: inline-block; padding: 4px 12px; border-radius: 4px;
    font-weight: 700; margin-left: 1em; font-size: 0.85em;
  }
  .input-table .param-label { font-weight: 600; }
  .section-row td {
    background: #2c3e50 !important; color: #fff;
    font-weight: 700; font-size: 0.8em; text-transform: uppercase;
    letter-spacing: 0.5px; padding: 0.5em 0.7em;
  }
  .file-viewer { margin-bottom: 1.5em; }
  .file-viewer-header {
    display: flex; justify-content: space-between; align-items: center;
    background: #34495e; color: #fff; padding: 0.5em 1em;
    border-radius: 4px 4px 0 0; font-size: 0.85em;
  }
  .file-viewer-header .file-path {
    font-family: 'SF Mono', 'Cascadia Code', 'Consolas', monospace;
    font-size: 0.9em; word-break: break-all;
  }
  .file-viewer-header .solver-label {
    font-weight: 700; margin-right: 0.8em; white-space: nowrap;
  }
  .file-viewer-header button {
    background: #3498db; color: #fff; border: none; padding: 4px 12px;
    border-radius: 3px; cursor: pointer; font-size: 0.85em; white-space: nowrap;
  }
  .file-viewer-header button:hover { background: #2980b9; }
  .file-content {
    max-height: 400px; overflow-y: auto; overflow-x: auto;
    border: 1px solid #ddd; border-top: none;
    border-radius: 0 0 4px 4px; background: #fafafa; margin: 0;
  }
  .file-content pre {
    margin: 0; padding: 0.8em 1em; font-size: 12px; line-height: 1.5;
    font-family: 'SF Mono', 'Cascadia Code', 'Consolas', monospace;
    counter-reset: line;
  }
  .file-content pre .line { display: block; }
  .file-content pre .line::before {
    counter-increment: line; content: counter(line);
    display: inline-block; width: 3.5em; text-align: right;
    margin-right: 1em; color: #999; font-size: 0.85em;
    border-right: 1px solid #ddd; padding-right: 0.5em;
    -webkit-user-select: none; user-select: none;
  }
  .dof-section { border-top: 2px solid #ecf0f1; padding-top: 1em; margin-top: 1em; }
  .dof-section:first-child { border-top: none; margin-top: 0; }
  .dof-title { font-size: 1.1em; color: #2c3e50; margin: 0 0 0.6em; }
  .dof-grid { display: grid; grid-template-columns: 45% 55%; gap: 1em; align-items: start; }
  .dof-text { font-size: 0.85em; }
  .dof-plot { min-height: 340px; }
  .consensus-badge {
    display: inline-block; padding: 3px 10px; border-radius: 3px;
    color: #fff; font-size: 0.8em; font-weight: 700; margin-bottom: 0.5em;
  }
  .mono { font-family: 'SF Mono', 'Cascadia Code', 'Consolas', 'Fira Code', monospace; }
  .stats-table { width: 100%; margin-bottom: 0.6em; }
  .stats-table td:last-child {
    text-align: right; font-family: 'SF Mono', 'Cascadia Code', 'Consolas', monospace;
  }
  .solver-table { width: 100%; margin-bottom: 0.5em; }
  .solver-table th { font-size: 0.75em; text-align: center; padding: 0.3em 0.4em; }
  .solver-table td {
    text-align: right; font-family: 'SF Mono', 'Cascadia Code', 'Consolas', monospace;
    font-size: 0.8em; padding: 0.25em 0.4em;
  }
  .solver-table td:first-child { text-align: left; font-family: inherit; font-weight: 600; }
  .dof-text h4 {
    margin: 0.7em 0 0.3em; font-size: 0.9em; color: #2c3e50;
    border-bottom: 1px solid #ddd; padding-bottom: 0.15em;
  }
  .observations p { margin: 0.3em 0; line-height: 1.4; }
  .skipped-note {
    font-size: 0.8em; color: #888; font-style: italic; margin-top: 0.5em;
    padding: 0.3em 0.5em; background: #fef9e7;
    border-left: 3px solid #f0c674; border-radius: 2px;
  }
  .plot-links { column-count: 2; font-size: 0.85em; }
  .plot-links li { margin-bottom: 0.3em; }
  @media (max-width: 900px) { .dof-grid { grid-template-columns: 1fr; } }
"""


# --- Section adapters: uniform (data, **kwargs) -> str over the builders. ---
# Each adapter mirrors the conditional rule the original orchestrator applied
# to its builder. Returning "" omits the section (joined output is identical).


def _sec_toc(data, **_):
    return _build_toc_html(data)


def _sec_header(data, **_):
    return _build_header_html(data)


def _sec_executive_summary(data, **_):
    return _build_executive_summary_html(data)


def _sec_validation(data, *, validation_report=None, **_):
    if validation_report is not None:
        return _build_validation_sanity_html(validation_report)
    return ""


def _sec_assumptions(data, *, assumption_ledger=None, **_):
    if assumption_ledger is not None:
        return _build_assumptions_html(assumption_ledger)
    return ""


def _sec_hull_description(data, **_):
    return _build_hull_description_html(data)


def _sec_stability(data, **_):
    if data.hydrostatics:
        return _build_stability_html(data)
    return ""


def _sec_natural_periods(data, **_):
    return _build_natural_periods_html(data)


def _sec_added_mass(data, **_):
    return _build_added_mass_diagonal_html(data)


def _sec_damping(data, **_):
    return _build_damping_diagonal_html(data)


def _sec_coupling(data, **_):
    if data.coupling_significance:
        return _build_coupling_assessment_html(data)
    return ""


def _sec_infinite_added_mass(data, **_):
    if data.infinite_freq_added_mass:
        return _build_infinite_added_mass_html(data.infinite_freq_added_mass)
    return ""


def _sec_load_raos(data, *, include_plotlyjs="cdn", **_):
    if data.load_raos:
        return _build_load_raos_html(data.load_raos, include_plotlyjs)
    return ""


def _sec_roll_damping(data, *, include_plotlyjs="cdn", **_):
    if data.roll_damping:
        return _build_roll_damping_html(data.roll_damping, include_plotlyjs)
    return ""


def _sec_notes(data, **_):
    if not data.notes:
        return ""
    notes_items = "\n".join(f"<li>{n}</li>" for n in data.notes)
    return f'<div class="section"><h2>Notes</h2><ul>{notes_items}</ul></div>'


def _sec_phase_guide(data, **_):
    return _build_phase_guide_html()


def _sec_appendices(data, **_):
    return _build_appendices_html()


def _benchmark_block(key: str):
    """Builder for a BENCHMARK_ONLY section: wrap the fragment for ``key``."""

    def _builder(data, *, benchmark_sections=None, **_):
        frag = (benchmark_sections or {}).get(key)
        return f'<div class="section">{frag}</div>' if frag else ""

    return _builder


# Module-level backbone: the diffraction physics causal chain.
# Geometry -> Hydrostatics -> Stability -> Natural Periods ->
# Coefficients -> Excitation -> Response -> Damping -> Summary.
_DIFFRACTION_BACKBONE = ReportBackbone(
    title="Diffraction Analysis Report",
    sections=[
        ReportSection("toc", "Table of Contents", SectionMode.ALWAYS, _sec_toc),
        ReportSection("header", "Header & Identification", SectionMode.ALWAYS, _sec_header),
        ReportSection("executive_summary", "Executive Summary", SectionMode.ALWAYS, _sec_executive_summary),
        ReportSection("validation_sanity", "Validation Sanity Check", SectionMode.ALWAYS, _sec_validation),
        ReportSection("assumptions", "Assumptions", SectionMode.ALWAYS, _sec_assumptions),
        ReportSection("hull_description", "Hull Description & Mesh Quality", SectionMode.COMPACT_SKIP, _sec_hull_description),
        ReportSection("mesh_schematic", "Mesh Schematic", SectionMode.BENCHMARK_ONLY, _benchmark_block("mesh_schematic")),
        ReportSection("input_comparison", "Input Comparison", SectionMode.BENCHMARK_ONLY, _benchmark_block("input_comparison")),
        ReportSection("input_files", "Input Files", SectionMode.BENCHMARK_ONLY, _benchmark_block("input_files")),
        ReportSection("stability", "Hydrostatic Properties & Stability", SectionMode.ALWAYS, _sec_stability),
        ReportSection("natural_periods", "Natural Period Estimates", SectionMode.COMPACT_SKIP, _sec_natural_periods),
        ReportSection("added_mass_diagonal", "Added Mass Diagonal", SectionMode.COMPACT_SKIP, _sec_added_mass),
        ReportSection("damping_diagonal", "Radiation Damping Diagonal", SectionMode.COMPACT_SKIP, _sec_damping),
        ReportSection("coupling_assessment", "Coupling Assessment", SectionMode.COMPACT_SKIP, _sec_coupling),
        ReportSection("infinite_added_mass", "Infinite Frequency Added Mass", SectionMode.COMPACT_SKIP, _sec_infinite_added_mass),
        ReportSection("hydro_coefficients", "Hydrodynamic Coefficients", SectionMode.BENCHMARK_ONLY, _benchmark_block("hydro_coefficients")),
        ReportSection("load_raos", "Wave Excitation Forces (Load RAOs)", SectionMode.COMPACT_SKIP, _sec_load_raos),
        ReportSection("consensus_summary", "Displacement RAOs — Consensus", SectionMode.BENCHMARK_ONLY, _benchmark_block("consensus_summary")),
        ReportSection("dof_sections", "Displacement RAOs — Per-DOF", SectionMode.BENCHMARK_ONLY, _benchmark_block("dof_sections")),
        ReportSection("roll_damping", "Roll Critical Damping Analysis", SectionMode.ALWAYS, _sec_roll_damping),
        ReportSection("raw_rao_data", "Raw Data", SectionMode.BENCHMARK_ONLY, _benchmark_block("raw_rao_data")),
        ReportSection("overlay_plots", "Overlay Plots", SectionMode.BENCHMARK_ONLY, _benchmark_block("overlay_plots")),
        ReportSection("notes", "Notes", SectionMode.ALWAYS, _sec_notes),
        ReportSection("phase_guide", "Phase Interpretation Guide", SectionMode.COMPACT_SKIP, _sec_phase_guide),
        ReportSection("appendices", "Appendices", SectionMode.COMPACT_SKIP, _sec_appendices),
    ],
)


def generate_diffraction_report(
    report_data: DiffractionReportData,
    output_path: Path,
    include_plotlyjs: str = "cdn",
    mode: str = "full",
    validation_report: Optional[dict[str, Any]] = None,
    assumption_ledger: Optional["AssumptionLedger"] = None,
) -> Path:
    """Generate a self-contained HTML diffraction report.

    Sections follow the physics causal chain:
    Geometry -> Hydrostatics -> Stability -> Natural Periods ->
    Coefficients -> Excitation -> Response -> Damping -> Summary

    Args:
        report_data: Populated DiffractionReportData.
        output_path: Path for the output HTML file.
        include_plotlyjs: 'cdn' for CDN link, True for inline JS.
        mode: 'full' for all sections, 'compact' for executive summary only.
        validation_report: Optional pre-computed validation report dict. When
            provided, a sanity-check section is rendered near the executive
            summary. This NEVER re-runs validation (no ``OutputValidator`` is
            instantiated here).
        assumption_ledger: Optional assumption provenance ledger. When
            provided, an Assumptions section is rendered.

    Returns:
        Path to the generated HTML file.
    """
    output_path = Path(output_path)
    output_path.parent.mkdir(parents=True, exist_ok=True)

    # Override mode from report_data if set.
    effective_mode = mode if mode != "full" else report_data.mode

    bm = report_data.benchmark_html_sections or {}

    # Build the ordered section list declaratively via the shared backbone.
    sections = _DIFFRACTION_BACKBONE.render(
        report_data,
        mode=effective_mode,
        benchmark_sections=bm,
        include_plotlyjs=include_plotlyjs,
        validation_report=validation_report,
        assumption_ledger=assumption_ledger,
    )

    plotly_src = (
        '<script src="https://cdn.plot.ly/plotly-latest.min.js"></script>'
        if include_plotlyjs == "cdn"
        else ""
    )

    # Benchmark CSS is appended only when benchmark sections are present.
    css = _BASE_CSS + (_BENCHMARK_CSS if bm else "")

    renderer = ReportRenderer()
    html = renderer.render_html(
        sections,
        title=f"Diffraction Report - {report_data.vessel_name}",
        css=css,
        plotlyjs_src=plotly_src,
        container_class="container",
    )
    return renderer.write_html(html, output_path)


# ---------------------------------------------------------------------------
# Convenience: generate report from .owr file
# ---------------------------------------------------------------------------


def generate_report_from_owr(
    owr_path: Path,
    output_path: Optional[Path] = None,
    include_plotlyjs: str = "cdn",
    validation_report: Optional[dict[str, Any]] = None,
) -> Path:
    """One-shot: extract data from .owr and generate HTML report.

    Args:
        owr_path: Path to OrcaWave results file.
        output_path: Path for HTML output. Defaults to same dir as owr.
        include_plotlyjs: 'cdn' or True for inline.
        validation_report: Optional pre-computed validation report dict, passed
            through to ``generate_diffraction_report`` for the sanity-check
            section. Validation is NOT re-run here.

    Returns:
        Path to generated HTML report.
    """
    owr_path = Path(owr_path)
    if output_path is None:
        output_path = owr_path.parent / f"{owr_path.stem}_diffraction_report.html"

    data = extract_report_data_from_owr(owr_path)
    return generate_diffraction_report(
        data,
        output_path,
        include_plotlyjs,
        validation_report=validation_report,
    )


__all__ = [
    # Data models
    "HydrostaticData",
    "RollDampingData",
    "LoadRAOData",
    "MeshQualityData",
    "DiffractionReportData",
    # Constants
    "DOF_NAMES",
    "DOF_UNITS",
    "LOAD_RAO_UNITS",
    # Extractors
    "extract_report_data_from_owr",
    "build_report_data_from_solver_results",
    # Computations
    "compute_stability",
    "compute_radii_of_gyration",
    "compute_natural_periods",
    "compute_peak_responses",
    "compute_coupling_significance",
    "generate_executive_warnings",
    # Orchestration
    "generate_diffraction_report",
    "generate_report_from_owr",
    # Builders (re-exported for backward compat)
    "HULL_TYPE_NOTES",
    "_get_hull_type_note",
]
