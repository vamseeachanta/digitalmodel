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

from pathlib import Path
from typing import Any, Dict, List, Optional

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

# --- Re-exports from report_extractors ---
from digitalmodel.hydrodynamics.diffraction.report_extractors import (
    _round_2d,
    build_report_data_from_solver_results,
    extract_report_data_from_owr,
)

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


# ---------------------------------------------------------------------------
# Orchestration: generate_diffraction_report
# ---------------------------------------------------------------------------


def generate_diffraction_report(
    report_data: DiffractionReportData,
    output_path: Path,
    include_plotlyjs: str = "cdn",
    mode: str = "full",
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

    Returns:
        Path to the generated HTML file.
    """
    output_path = Path(output_path)
    output_path.parent.mkdir(parents=True, exist_ok=True)

    # Override mode from report_data if set
    effective_mode = mode if mode != "full" else report_data.mode
    compact = effective_mode == "compact"

    bm = report_data.benchmark_html_sections or {}
    sections = []

    # S0: Table of Contents
    sections.append(_build_toc_html(report_data))

    # S1: Header
    sections.append(_build_header_html(report_data))

    # S2: Executive Summary (always shown)
    sections.append(_build_executive_summary_html(report_data))

    if not compact:
        # S3: Hull Description & Mesh Quality
        sections.append(_build_hull_description_html(report_data))

    # S3b: Mesh schematic (BENCHMARK ONLY)
    if bm.get("mesh_schematic"):
        sections.append(f'<div class="section">{bm["mesh_schematic"]}</div>')

    # S4: Input Configuration (BENCHMARK ONLY)
    if bm.get("input_comparison"):
        sections.append(f'<div class="section">{bm["input_comparison"]}</div>')
    if bm.get("input_files"):
        sections.append(f'<div class="section">{bm["input_files"]}</div>')

    # S5: Hydrostatic Properties & Stability
    if report_data.hydrostatics:
        sections.append(_build_stability_html(report_data))

    if not compact:
        # S6: Natural Period Estimates
        sections.append(_build_natural_periods_html(report_data))

        # S7: Hydrodynamic Coefficients
        sections.append(_build_added_mass_diagonal_html(report_data))
        sections.append(_build_damping_diagonal_html(report_data))

        if report_data.coupling_significance:
            sections.append(_build_coupling_assessment_html(report_data))

        if report_data.infinite_freq_added_mass:
            sections.append(
                _build_infinite_added_mass_html(report_data.infinite_freq_added_mass)
            )

    # S7e: Solver correlation heatmaps (BENCHMARK ONLY)
    if bm.get("hydro_coefficients"):
        sections.append(f'<div class="section">{bm["hydro_coefficients"]}</div>')

    if not compact:
        # S8: Wave Excitation Forces (Load RAOs)
        if report_data.load_raos:
            sections.append(
                _build_load_raos_html(report_data.load_raos, include_plotlyjs)
            )

    # S9: Displacement RAOs — consensus + per-DOF (BENCHMARK ONLY)
    if bm.get("consensus_summary"):
        sections.append(f'<div class="section">{bm["consensus_summary"]}</div>')
    if bm.get("dof_sections"):
        sections.append(f'<div class="section">{bm["dof_sections"]}</div>')

    # S10: Roll Damping
    if report_data.roll_damping:
        sections.append(
            _build_roll_damping_html(report_data.roll_damping, include_plotlyjs)
        )

    # S11: Raw Data & Overlay Plots (BENCHMARK ONLY)
    if bm.get("raw_rao_data"):
        sections.append(f'<div class="section">{bm["raw_rao_data"]}</div>')
    if bm.get("overlay_plots"):
        sections.append(f'<div class="section">{bm["overlay_plots"]}</div>')

    # S11b: Notes (benchmark or standalone)
    if report_data.notes:
        notes_items = "\n".join(f"<li>{n}</li>" for n in report_data.notes)
        sections.append(
            f'<div class="section"><h2>Notes</h2><ul>{notes_items}</ul></div>'
        )

    if not compact:
        # S12: Phase interpretation guide
        sections.append(_build_phase_guide_html())

        # S13: Appendices
        sections.append(_build_appendices_html())

    plotly_src = (
        '<script src="https://cdn.plot.ly/plotly-latest.min.js"></script>'
        if include_plotlyjs == "cdn"
        else ""
    )

    benchmark_css = ""
    if bm:
        benchmark_css = """
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

    html = f"""\
<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<title>Diffraction Report - {report_data.vessel_name}</title>
{plotly_src}
<style>
  * {{ box-sizing: border-box; }}
  body {{
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI',
                 Roboto, Arial, sans-serif;
    margin: 0; padding: 0; color: #333; background: #f8f9fa;
    font-size: 14px; line-height: 1.5;
  }}
  .container {{ max-width: 1400px; margin: 0 auto; padding: 1.5em 2em; }}
  .report-header {{
    background: #2c3e50; color: #fff; padding: 1.2em 2em;
    margin-bottom: 1.5em; border-radius: 6px;
  }}
  .report-header h1 {{ margin: 0 0 0.3em; font-size: 1.6em; }}
  .report-header .subtitle {{ font-size: 1.1em; opacity: 0.9; margin-bottom: 0.5em; font-weight: 300; }}
  .report-header .meta {{ font-size: 0.9em; opacity: 0.85; }}
  .nav-bar {{
    margin-top: 1.2em; padding-top: 1em; border-top: 1px solid rgba(255,255,255,0.15);
    display: flex; gap: 1.5em; font-size: 0.9em;
  }}
  .nav-bar a {{ color: #fff; text-decoration: none; opacity: 0.8; }}
  .nav-bar a:hover {{ opacity: 1; text-decoration: underline; }}
  .nav-bar .active {{ opacity: 1; font-weight: 600; border-bottom: 2px solid #3498db; }}
  .section {{
    background: #fff; border-radius: 6px;
    box-shadow: 0 1px 3px rgba(0,0,0,0.08);
    margin-bottom: 1.5em; padding: 1.2em 1.5em;
  }}
  .section h2 {{
    margin: 0 0 0.8em; font-size: 1.2em; color: #2c3e50;
    border-bottom: 2px solid #3498db; padding-bottom: 0.3em;
  }}
  table {{
    border-collapse: collapse; margin: 0.5em 0; font-size: 0.85em;
    width: 100%;
  }}
  th, td {{
    border: 1px solid #ddd; padding: 0.45em 0.7em; text-align: left;
  }}
  th {{
    background: #34495e; color: #fff; font-weight: 600;
    font-size: 0.85em; text-transform: uppercase; letter-spacing: 0.3px;
  }}
  tbody tr:nth-child(even) {{ background: #f8f9fa; }}
  tbody tr:hover {{ background: #ebf5fb; }}
  td {{ vertical-align: top; font-family: 'Cascadia Code', 'Fira Code', monospace; }}
  .matrix-table td {{ text-align: right; padding: 0.3em 0.5em; font-size: 0.8em; }}
  .matrix-table th {{ text-align: center; padding: 0.3em 0.5em; font-size: 0.8em; }}
  .highlight {{ background: #ffeaa7 !important; font-weight: 600; }}
  .plot-container {{ margin: 1em 0; }}
{benchmark_css}</style>
</head>
<body>
<div class="container">
{''.join(sections)}
</div>
</body>
</html>"""

    output_path.write_text(html, encoding="utf-8")
    return output_path


# ---------------------------------------------------------------------------
# Convenience: generate report from .owr file
# ---------------------------------------------------------------------------


def generate_report_from_owr(
    owr_path: Path,
    output_path: Optional[Path] = None,
    include_plotlyjs: str = "cdn",
) -> Path:
    """One-shot: extract data from .owr and generate HTML report.

    Args:
        owr_path: Path to OrcaWave results file.
        output_path: Path for HTML output. Defaults to same dir as owr.
        include_plotlyjs: 'cdn' or True for inline.

    Returns:
        Path to generated HTML report.
    """
    owr_path = Path(owr_path)
    if output_path is None:
        output_path = owr_path.parent / f"{owr_path.stem}_diffraction_report.html"

    data = extract_report_data_from_owr(owr_path)
    return generate_diffraction_report(data, output_path, include_plotlyjs)


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
