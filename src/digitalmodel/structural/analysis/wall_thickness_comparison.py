# ABOUTME: Three-way design code comparison for pipeline/riser wall thickness (WRK-159)
# ABOUTME: Runs same pipe through multiple codes, generates side-by-side HTML report

"""
Wall Thickness — Design Code Comparison Report

Runs the same pipe specification through multiple registered design codes
and produces a side-by-side comparison showing:
- Utilisation per check (burst, collapse, propagation, combined)
- Governing check and maximum utilisation per code
- Summary table with pass/fail verdicts
- Bar chart comparing max utilisation across codes

Supports any codes in CODE_REGISTRY (API RP 1111, API RP 2RD, API STD 2RD,
DNV-ST-F101, etc.).
"""

import logging
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, List, Optional

from digitalmodel.structural.analysis.wall_thickness import (
    DesignCode,
    DesignFactors,
    DesignLoads,
    PipeGeometry,
    PipeMaterial,
    WallThicknessAnalyzer,
)
from digitalmodel.structural.analysis.wall_thickness_codes import CODE_REGISTRY

logger = logging.getLogger(__name__)


@dataclass
class CodeComparisonResult:
    """Result of running a single design code on a pipe configuration."""

    code: DesignCode
    code_label: str
    checks: Dict[str, float] = field(default_factory=dict)
    details: Dict[str, Dict[str, float]] = field(default_factory=dict)
    governing_check: Optional[str] = None
    max_utilisation: float = 0.0
    is_safe: bool = True


def compare_codes(
    geometry: PipeGeometry,
    material: PipeMaterial,
    loads: DesignLoads,
    codes: List[DesignCode],
    factors: Optional[DesignFactors] = None,
) -> List[CodeComparisonResult]:
    """Run the same pipe through multiple design codes.

    Args:
        geometry: Pipe geometric properties.
        material: Pipe material properties.
        loads: Applied design loads.
        codes: List of DesignCode enums to compare.
        factors: Optional design factors (defaults to medium safety class).

    Returns:
        List of CodeComparisonResult, one per code.
    """
    if factors is None:
        factors = DesignFactors()

    results = []
    for code in codes:
        analyzer = WallThicknessAnalyzer(geometry, material, loads, factors, code)
        analysis = analyzer.perform_analysis()

        result = CodeComparisonResult(
            code=code,
            code_label=code.value,
            checks=analysis.checks,
            details=analysis.details,
            governing_check=analysis.governing_check,
            max_utilisation=analysis.max_utilisation,
            is_safe=analysis.is_safe,
        )
        results.append(result)
        logger.info(
            "Code %s: max_util=%.3f, governing=%s, safe=%s",
            code.value, result.max_utilisation, result.governing_check, result.is_safe,
        )

    return results


def generate_comparison_report(
    geometry: PipeGeometry,
    material: PipeMaterial,
    loads: DesignLoads,
    codes: List[DesignCode],
    title: str = "Design Code Comparison",
    factors: Optional[DesignFactors] = None,
    output_path: Optional[str] = None,
) -> str:
    """Generate an HTML comparison report for multiple design codes.

    Args:
        geometry: Pipe geometric properties.
        material: Pipe material properties.
        loads: Applied design loads.
        codes: List of DesignCode enums to compare.
        title: Report title.
        factors: Optional design factors.
        output_path: Optional file path to write the HTML report.

    Returns:
        HTML string.
    """
    results = compare_codes(geometry, material, loads, codes, factors)
    html = _build_comparison_html(geometry, material, loads, results, title)

    if output_path:
        Path(output_path).write_text(html, encoding="utf-8")
        logger.info("Comparison report written to %s", output_path)

    return html


def _build_comparison_html(
    geometry: PipeGeometry,
    material: PipeMaterial,
    loads: DesignLoads,
    results: List[CodeComparisonResult],
    title: str,
) -> str:
    """Build the HTML report string."""

    # Collect all unique check names across codes
    all_checks = []
    seen = set()
    for r in results:
        for check in r.checks:
            if check not in seen:
                all_checks.append(check)
                seen.add(check)

    # Summary table rows
    summary_rows = ""
    for r in results:
        verdict = "PASS" if r.is_safe else "FAIL"
        verdict_class = "pass" if r.is_safe else "fail"
        summary_rows += f"""
        <tr>
            <td><strong>{r.code_label}</strong></td>
            <td>{r.max_utilisation:.3f}</td>
            <td>{r.governing_check or 'N/A'}</td>
            <td class="{verdict_class}">{verdict}</td>
        </tr>"""

    # Detailed check comparison table
    detail_header = "<th>Check</th>"
    for r in results:
        detail_header += f"<th>{r.code_label}</th>"

    detail_rows = ""
    for check in all_checks:
        detail_rows += f"<tr><td><strong>{check}</strong></td>"
        for r in results:
            util = r.checks.get(check)
            if util is not None:
                cell_class = "pass" if util <= 1.0 else "fail"
                detail_rows += f'<td class="{cell_class}">{util:.4f}</td>'
            else:
                detail_rows += '<td class="na">N/A</td>'
        detail_rows += "</tr>"

    # Bar chart data (inline SVG for zero-dependency rendering)
    bar_chart = _build_bar_chart_svg(results)

    # Input data section
    pi_mpa = loads.internal_pressure / 1e6
    pe_mpa = loads.external_pressure / 1e6
    od_mm = geometry.outer_diameter * 1000
    wt_mm = geometry.wall_thickness * 1000
    ca_mm = geometry.corrosion_allowance * 1000
    smys_mpa = material.smys / 1e6
    smts_mpa = material.smts / 1e6

    html = f"""<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>{title}</title>
    <style>
        body {{ font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
               max-width: 1100px; margin: 0 auto; padding: 20px; color: #333; }}
        h1 {{ color: #1a1a2e; border-bottom: 3px solid #16213e; padding-bottom: 10px; }}
        h2 {{ color: #16213e; margin-top: 30px; }}
        table {{ border-collapse: collapse; width: 100%; margin: 15px 0; }}
        th {{ background: #16213e; color: white; padding: 10px 14px; text-align: left; }}
        td {{ padding: 8px 14px; border-bottom: 1px solid #ddd; }}
        tr:hover {{ background: #f5f5f5; }}
        .pass {{ color: #2e7d32; font-weight: bold; }}
        .fail {{ color: #c62828; font-weight: bold; }}
        .na {{ color: #999; font-style: italic; }}
        .summary-box {{ background: #f8f9fa; border: 1px solid #dee2e6; border-radius: 8px;
                        padding: 20px; margin: 20px 0; }}
        .input-grid {{ display: grid; grid-template-columns: 1fr 1fr; gap: 20px; }}
        .input-card {{ background: #fff; border: 1px solid #e0e0e0; border-radius: 6px; padding: 15px; }}
        .input-card h3 {{ margin-top: 0; color: #444; font-size: 14px; text-transform: uppercase;
                          letter-spacing: 1px; }}
        .chart-container {{ text-align: center; margin: 20px 0; }}
        .footer {{ margin-top: 40px; padding-top: 15px; border-top: 1px solid #ddd;
                   font-size: 12px; color: #888; }}
    </style>
</head>
<body>

<h1>{title}</h1>

<div class="summary-box">
    <h2>Executive Summary</h2>
    <table>
        <thead>
            <tr>
                <th>Design Code</th>
                <th>Max Utilisation</th>
                <th>Governing Check</th>
                <th>Verdict</th>
            </tr>
        </thead>
        <tbody>
            {summary_rows}
        </tbody>
    </table>
</div>

<h2>Utilisation Comparison</h2>
<div class="chart-container">
    {bar_chart}
</div>

<h2>Detailed Check-by-Check Comparison</h2>
<table>
    <thead>
        <tr>{detail_header}</tr>
    </thead>
    <tbody>
        {detail_rows}
    </tbody>
</table>

<h2>Input Data</h2>
<div class="input-grid">
    <div class="input-card">
        <h3>Geometry</h3>
        <table>
            <tr><td>Outer Diameter</td><td>{od_mm:.1f} mm ({geometry.outer_diameter:.4f} m)</td></tr>
            <tr><td>Wall Thickness</td><td>{wt_mm:.1f} mm ({geometry.wall_thickness:.4f} m)</td></tr>
            <tr><td>D/t Ratio</td><td>{geometry.d_over_t:.1f}</td></tr>
            <tr><td>Corrosion Allowance</td><td>{ca_mm:.1f} mm</td></tr>
        </table>
    </div>
    <div class="input-card">
        <h3>Material — {material.grade}</h3>
        <table>
            <tr><td>SMYS</td><td>{smys_mpa:.0f} MPa</td></tr>
            <tr><td>SMTS</td><td>{smts_mpa:.0f} MPa</td></tr>
            <tr><td>Young's Modulus</td><td>{material.youngs_modulus / 1e9:.0f} GPa</td></tr>
        </table>
    </div>
    <div class="input-card">
        <h3>Design Loads</h3>
        <table>
            <tr><td>Internal Pressure</td><td>{pi_mpa:.1f} MPa</td></tr>
            <tr><td>External Pressure</td><td>{pe_mpa:.1f} MPa</td></tr>
            <tr><td>Net Pressure</td><td>{pi_mpa - pe_mpa:.1f} MPa</td></tr>
        </table>
    </div>
</div>

<div class="footer">
    Generated by digitalmodel structural analysis — Design Code Comparison (WRK-159)
</div>

</body>
</html>"""

    return html


def _build_bar_chart_svg(results: List[CodeComparisonResult]) -> str:
    """Build an inline SVG bar chart comparing max utilisation across codes."""
    if not results:
        return ""

    n = len(results)
    bar_width = 80
    bar_gap = 30
    chart_width = n * (bar_width + bar_gap) + 60
    chart_height = 280
    plot_height = 200
    margin_left = 50
    margin_top = 20

    max_util = max(r.max_utilisation for r in results)
    y_max = max(max_util * 1.2, 1.1)  # at least show unity line

    colors = ["#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2"]

    bars = ""
    labels = ""
    values = ""
    for i, r in enumerate(results):
        x = margin_left + i * (bar_width + bar_gap)
        bar_h = (r.max_utilisation / y_max) * plot_height
        y = margin_top + plot_height - bar_h
        color = colors[i % len(colors)]

        bars += f'<rect x="{x}" y="{y}" width="{bar_width}" height="{bar_h}" fill="{color}" rx="3"/>\n'
        labels += f'<text x="{x + bar_width / 2}" y="{margin_top + plot_height + 18}" text-anchor="middle" font-size="11" fill="#333">{r.code_label}</text>\n'
        values += f'<text x="{x + bar_width / 2}" y="{y - 5}" text-anchor="middle" font-size="12" font-weight="bold" fill="{color}">{r.max_utilisation:.3f}</text>\n'

    # Unity line
    unity_y = margin_top + plot_height - (1.0 / y_max) * plot_height
    unity_line = f'<line x1="{margin_left - 10}" y1="{unity_y}" x2="{chart_width - 10}" y2="{unity_y}" stroke="#c62828" stroke-width="1.5" stroke-dasharray="6,3"/>\n'
    unity_label = f'<text x="{margin_left - 15}" y="{unity_y + 4}" text-anchor="end" font-size="10" fill="#c62828">1.0</text>\n'

    # Y-axis label
    y_axis = f'<text x="12" y="{margin_top + plot_height / 2}" text-anchor="middle" font-size="11" fill="#666" transform="rotate(-90, 12, {margin_top + plot_height / 2})">Max Utilisation</text>\n'

    return f"""<svg xmlns="http://www.w3.org/2000/svg" width="{chart_width}" height="{chart_height}" viewBox="0 0 {chart_width} {chart_height}">
    <rect width="100%" height="100%" fill="#fafafa" rx="6"/>
    {y_axis}
    {bars}
    {unity_line}
    {unity_label}
    {labels}
    {values}
</svg>"""
