"""FFS HTML report generator.

Produces a self-contained single-file HTML report suitable for printing to PDF.
The report includes:

  1. Executive Summary — component ID, verdict (colour-coded), key metrics.
  2. Component Data Sheet — geometry and design parameters.
  3. Level 1 Assessment — t_mm vs t_min table.
  4. Level 2 Assessment — RSF, Folias factor, area-averaged thickness.
  5. Remaining Life — simple linear projection.
  6. Methodology & References — API 579 clause references.
  7. Appendix — raw grid data table (first 20 rows × 10 cols).

Plotly CDN is referenced for interactive charts (if Plotly heatmap data is
supplied via the optional ``plotly_heatmap_json`` parameter).  The report
remains fully self-contained (with static tables) when Plotly is not available.

No external file writes are performed — the caller receives an HTML string
and may write it wherever appropriate.
"""

from __future__ import annotations

import html
import math
from datetime import datetime
from typing import Optional

import pandas as pd

_VERDICT_COLORS = {
    "ACCEPT": "#2e7d32",   # green
    "MONITOR": "#f57c00",  # amber
    "RE_RATE": "#e65100",  # deep orange
    "REPAIR": "#c62828",   # red
    "REPLACE": "#4a148c",  # purple
    "FAIL_LEVEL_1": "#c62828",
    "FAIL_LEVEL_2": "#c62828",
}
_DEFAULT_COLOR = "#757575"

_CSS = (
    "<style>"
    "body{font-family:Arial,sans-serif;font-size:10pt;margin:20px;color:#212121}"
    "h1{font-size:16pt;border-bottom:2px solid #1565c0;padding-bottom:4px}"
    "h2{font-size:13pt;border-bottom:1px solid #90caf9;padding-bottom:2px;margin-top:18px}"
    "h3{font-size:11pt;margin-bottom:4px}"
    "table{border-collapse:collapse;width:100%;margin-bottom:12px}"
    "th{background-color:#1565c0;color:white;padding:5px 8px;text-align:left;font-size:9pt}"
    "td{padding:4px 8px;border:1px solid #bdbdbd;font-size:9pt;font-family:monospace}"
    "tr:nth-child(even){background-color:#f5f5f5}"
    ".verdict-badge{display:inline-block;padding:6px 16px;border-radius:4px;"
    "color:white;font-weight:bold;font-size:14pt;letter-spacing:1px}"
    ".section{page-break-inside:avoid}"
    ".disclaimer{color:#616161;font-size:8pt;font-style:italic;margin-top:12px}"
    "@media print{.no-print{display:none}}"
    "</style>"
)


class FFSReport:
    """Generate an HTML FFS assessment report."""

    @staticmethod
    def generate_html(
        grid_df: pd.DataFrame,
        decision: dict,
        component_id: str,
        nominal_od_in: float,
        nominal_wt_in: float,
        t_min_in: float,
        design_code: str,
        design_pressure_psi: float,
        *,
        assessment_date: Optional[str] = None,
        nominal_id_in: Optional[float] = None,
        smys_psi: Optional[float] = None,
        corrosion_rate_in_per_yr: Optional[float] = None,
        plotly_heatmap_json: Optional[str] = None,
    ) -> str:
        """Render the full FFS assessment as a self-contained HTML string.

        Args:
            grid_df: Parsed wall-thickness grid (inches).
            decision: Result dict from :meth:`FFSDecision.decide`.
            component_id: Asset / tag identifier.
            nominal_od_in: Nominal outside diameter (inches).
            nominal_wt_in: Nominal wall thickness (inches).
            t_min_in: Code-required minimum wall thickness (inches).
            design_code: Design code used for t_min (e.g. 'B31.8').
            design_pressure_psi: Internal design pressure (psi).
            assessment_date: ISO date string; defaults to today.
            nominal_id_in: Nominal inside diameter (inches); derived if None.
            smys_psi: SMYS for component material (psi); optional metadata.
            corrosion_rate_in_per_yr: Future corrosion rate; optional metadata.
            plotly_heatmap_json: Pre-serialised Plotly figure JSON for
                interactive heatmap embedding; omitted when None.

        Returns:
            Complete HTML document as a string.
        """
        today = assessment_date or datetime.utcnow().strftime("%Y-%m-%d")
        nominal_id = nominal_id_in if nominal_id_in else nominal_od_in - 2.0 * nominal_wt_in

        verdict = decision.get("verdict", "UNKNOWN")
        rsf = decision.get("rsf", float("nan"))
        rsf_a = decision.get("rsf_a", 0.9)
        remaining_life = decision.get("remaining_life_yr", float("nan"))
        criterion = decision.get("governing_criterion", "")

        # Derive t_mm from grid
        t_mm = float(grid_df.min(skipna=True).min())
        t_am = float(grid_df.mean(skipna=True).mean())

        verdict_color = _VERDICT_COLORS.get(verdict, _DEFAULT_COLOR)

        sections = [
            FFSReport._html_head(component_id, today),
            FFSReport._section_executive_summary(
                component_id, today, verdict, verdict_color, criterion,
                remaining_life, design_code, rsf, rsf_a
            ),
            FFSReport._section_component_data(
                component_id, today, nominal_od_in, nominal_id, nominal_wt_in,
                design_pressure_psi, design_code, smys_psi, corrosion_rate_in_per_yr
            ),
            FFSReport._section_level1(t_mm, t_min_in, design_code),
            FFSReport._section_level2(rsf, rsf_a, t_am, t_mm, decision),
            FFSReport._section_remaining_life(
                t_mm, t_min_in, remaining_life, corrosion_rate_in_per_yr
            ),
            FFSReport._section_methodology(design_code),
            FFSReport._section_appendix(grid_df),
            FFSReport._html_foot(),
        ]

        if plotly_heatmap_json:
            # Insert after executive summary
            heatmap_section = FFSReport._section_heatmap(plotly_heatmap_json)
            sections.insert(2, heatmap_section)

        return "\n".join(sections)

    # ------------------------------------------------------------------
    # Section builders
    # ------------------------------------------------------------------

    @staticmethod
    def _html_head(component_id: str, date: str) -> str:
        title = html.escape(f"FFS Assessment Report — {component_id} — {date}")
        return (
            f"<!DOCTYPE html>\n<html lang='en'>\n<head>\n"
            f"<meta charset='UTF-8'>\n"
            f"<meta name='viewport' content='width=device-width, initial-scale=1.0'>\n"
            f"<title>{title}</title>\n"
            f"{_CSS}\n"
            f"</head>\n<body>\n"
            f"<h1>Fitness-for-Service Assessment Report</h1>\n"
            f"<p><strong>Component:</strong> {html.escape(component_id)} &nbsp;|&nbsp; "
            f"<strong>Date:</strong> {html.escape(date)}</p>\n"
        )

    @staticmethod
    def _html_foot() -> str:
        return (
            "<div class='disclaimer'>"
            "This report is generated by the digitalmodel FFS assessment module. "
            "All results are based on the data and assumptions provided. "
            "Engineering judgement is required to interpret results near acceptance limits. "
            "This report does not constitute a certified engineering assessment. "
            "API 579-1/ASME FFS-1 2021 Edition criteria are applied."
            "</div>\n</body>\n</html>"
        )

    @staticmethod
    def _section_executive_summary(
        component_id: str,
        date: str,
        verdict: str,
        verdict_color: str,
        criterion: str,
        remaining_life: float,
        design_code: str,
        rsf: float,
        rsf_a: float,
    ) -> str:
        rl_str = (
            f"{remaining_life:.1f} yr"
            if math.isfinite(remaining_life)
            else "&#x221e; (no corrosion)"
        )
        rsf_str = f"{rsf:.4f}" if math.isfinite(rsf) else "N/A"
        return (
            "<div class='section'>\n"
            "<h2>1. Executive Summary</h2>\n"
            f"<p><span class='verdict-badge' style='background-color:{verdict_color}'>"
            f"{html.escape(verdict)}</span></p>\n"
            "<table>\n"
            "<tr><th>Parameter</th><th>Value</th></tr>\n"
            f"<tr><td>Component ID</td><td>{html.escape(component_id)}</td></tr>\n"
            f"<tr><td>Assessment Date</td><td>{html.escape(date)}</td></tr>\n"
            f"<tr><td>Design Code</td><td>{html.escape(design_code)}</td></tr>\n"
            f"<tr><td>Governing Standard</td><td>API 579-1/ASME FFS-1 2021 Edition</td></tr>\n"
            f"<tr><td>Assessment Verdict</td><td><strong>{html.escape(verdict)}</strong></td></tr>\n"
            f"<tr><td>Remaining Strength Factor (RSF)</td><td>{rsf_str}</td></tr>\n"
            f"<tr><td>Allowable RSF (RSFa)</td><td>{rsf_a:.2f}</td></tr>\n"
            f"<tr><td>Estimated Remaining Life</td><td>{rl_str}</td></tr>\n"
            f"<tr><td>Governing Criterion</td><td>{html.escape(criterion)}</td></tr>\n"
            "</table>\n"
            "</div>\n"
        )

    @staticmethod
    def _section_component_data(
        component_id: str,
        date: str,
        od: float,
        id_: float,
        wt: float,
        pressure: float,
        code: str,
        smys: Optional[float],
        corr_rate: Optional[float],
    ) -> str:
        smys_str = f"{smys:,} psi" if smys else "Not specified"
        rate_str = (
            f"{corr_rate:.4f} in/yr" if corr_rate is not None else "Not specified"
        )
        return (
            "<div class='section'>\n"
            "<h2>2. Component Data Sheet</h2>\n"
            "<table>\n"
            "<tr><th>Property</th><th>Value</th><th>Unit</th></tr>\n"
            f"<tr><td>Component ID</td><td>{html.escape(component_id)}</td><td>—</td></tr>\n"
            f"<tr><td>Nominal Outside Diameter</td><td>{od:.4f}</td><td>inch</td></tr>\n"
            f"<tr><td>Nominal Inside Diameter</td><td>{id_:.4f}</td><td>inch</td></tr>\n"
            f"<tr><td>Nominal Wall Thickness</td><td>{wt:.4f}</td><td>inch</td></tr>\n"
            f"<tr><td>Design Pressure</td><td>{pressure:,.1f}</td><td>psi</td></tr>\n"
            f"<tr><td>Design Code</td><td>{html.escape(code)}</td><td>—</td></tr>\n"
            f"<tr><td>SMYS</td><td>{smys_str}</td><td>—</td></tr>\n"
            f"<tr><td>Corrosion Rate</td><td>{rate_str}</td><td>—</td></tr>\n"
            f"<tr><td>Assessment Date</td><td>{html.escape(date)}</td><td>—</td></tr>\n"
            "</table>\n"
            "</div>\n"
        )

    @staticmethod
    def _section_level1(t_mm: float, t_min: float, code: str) -> str:
        margin = t_mm - t_min
        verdict = "ACCEPT" if t_mm >= t_min else "FAIL"
        color = _VERDICT_COLORS.get("ACCEPT" if t_mm >= t_min else "REPAIR", _DEFAULT_COLOR)
        return (
            "<div class='section'>\n"
            "<h2>3. Level 1 Assessment (API 579 Part 4/5 §4.3 / §5.3)</h2>\n"
            "<table>\n"
            "<tr><th>Parameter</th><th>Value (inch)</th><th>Status</th></tr>\n"
            f"<tr><td>Minimum Measured Thickness (t_mm)</td><td>{t_mm:.4f}</td><td>—</td></tr>\n"
            f"<tr><td>Required Minimum Thickness (t_min) [{html.escape(code)}]</td>"
            f"<td>{t_min:.4f}</td><td>—</td></tr>\n"
            f"<tr><td>Margin (t_mm &#8722; t_min)</td><td>{margin:.4f}</td>"
            f"<td style='color:{color}'><strong>{html.escape(verdict)}</strong></td></tr>\n"
            "</table>\n"
            "</div>\n"
        )

    @staticmethod
    def _section_level2(
        rsf: float,
        rsf_a: float,
        t_am: float,
        t_mm: float,
        decision: dict,
    ) -> str:
        rsf_str = f"{rsf:.4f}" if math.isfinite(rsf) else "N/A"
        verdict = "ACCEPT" if rsf >= rsf_a else "FAIL"
        color = _VERDICT_COLORS.get("ACCEPT" if rsf >= rsf_a else "REPAIR", _DEFAULT_COLOR)
        folias = decision.get("folias_factor", 1.0)
        l2_verdict = decision.get("verdict", "")
        return (
            "<div class='section'>\n"
            "<h2>4. Level 2 Assessment (API 579 Part 4/5 §4.4 / §5.4)</h2>\n"
            "<table>\n"
            "<tr><th>Parameter</th><th>Value</th><th>Status</th></tr>\n"
            f"<tr><td>Area-Averaged Thickness (t_am)</td><td>{t_am:.4f} inch</td><td>—</td></tr>\n"
            f"<tr><td>Minimum Measured Thickness (t_mm)</td><td>{t_mm:.4f} inch</td><td>—</td></tr>\n"
            f"<tr><td>Folias Bulging Factor (M_t)</td><td>{folias:.4f}</td><td>—</td></tr>\n"
            f"<tr><td>Remaining Strength Factor (RSF)</td><td>{rsf_str}</td>"
            f"<td style='color:{color}'><strong>{html.escape(verdict)}</strong></td></tr>\n"
            f"<tr><td>Allowable RSF (RSFa)</td><td>{rsf_a:.2f}</td><td>—</td></tr>\n"
            f"<tr><td>Level 2 Verdict</td>"
            f"<td colspan='2'><strong>{html.escape(l2_verdict)}</strong></td></tr>\n"
            "</table>\n"
            "<p class='disclaimer'>RSF calculated per API 579-1/ASME FFS-1 2021 Part 5 §5.4.2.2 "
            "(LML) or Part 4 §4.4.2 (GML).  RSFa = 0.9 per Annex 2B.</p>\n"
            "</div>\n"
        )

    @staticmethod
    def _section_remaining_life(
        t_mm: float,
        t_min: float,
        remaining_life: float,
        corr_rate: Optional[float],
    ) -> str:
        rl_str = (
            f"{remaining_life:.2f} years"
            if math.isfinite(remaining_life)
            else "Unlimited (no further corrosion assumed)"
        )
        rate_str = f"{corr_rate:.4f} in/yr" if corr_rate is not None else "Not provided"
        return (
            "<div class='section'>\n"
            "<h2>5. Remaining Life Projection</h2>\n"
            "<table>\n"
            "<tr><th>Parameter</th><th>Value</th></tr>\n"
            f"<tr><td>Current Minimum Thickness (t_mm)</td><td>{t_mm:.4f} inch</td></tr>\n"
            f"<tr><td>Acceptance Threshold (t_min)</td><td>{t_min:.4f} inch</td></tr>\n"
            f"<tr><td>Corrosion Rate</td><td>{rate_str}</td></tr>\n"
            f"<tr><td>Estimated Remaining Life</td><td><strong>{rl_str}</strong></td></tr>\n"
            "</table>\n"
            "<p class='disclaimer'>Remaining life = (t_mm &#8722; t_min) / corrosion_rate.  "
            "Based on linear extrapolation only.  Accelerating corrosion or "
            "pitting may reduce actual service life.  Consult API 570/510 for "
            "inspection interval planning.</p>\n"
            "</div>\n"
        )

    @staticmethod
    def _section_methodology(design_code: str) -> str:
        rows = [
            ("API 579-1/ASME FFS-1", "2021 3rd Ed.", "Part 4 GML; Part 5 LML; Fig 2.1"),
            (html.escape(design_code), "Current", "t_min calculation"),
        ]
        eq_rows = [
            ("t_min B31.8", "t=P&#183;D/(2&#183;SMYS&#183;F&#183;E&#183;T)", "B31.8 §841.1.1"),
            ("t_min B31.4", "t=P&#183;D/(2&#183;SMYS&#183;F&#183;E)", "B31.4 §403.2.1"),
            ("t_min ASME VIII", "t=P&#183;R/(S&#183;E&#8722;0.6P)", "BPVC VIII UG-27(c)(1)"),
            ("Folias M_t", "&#8730;(1+0.48&#955;&#178;), &#955;=1.285L_a/&#8730;(Dt_c)", "API 579 Tbl 4.4"),
            ("RSF LML", "R_t/(1&#8722;(1&#8722;R_t)/M_t)", "API 579 §5.4.2.2"),
        ]
        std_table = ("<table><tr><th>Standard</th><th>Edition</th><th>Application</th></tr>"
                     + "".join(f"<tr><td>{a}</td><td>{b}</td><td>{c}</td></tr>" for a, b, c in rows)
                     + "</table>")
        eq_table = ("<table><tr><th>Parameter</th><th>Equation</th><th>Reference</th></tr>"
                    + "".join(f"<tr><td>{a}</td><td>{b}</td><td>{c}</td></tr>" for a, b, c in eq_rows)
                    + "</table>")
        limits = ("<ul><li>Calculations in US Customary units (inches, psi).</li>"
                  "<li>Remaining life: linear extrapolation only.</li>"
                  "<li>Part 6 pitting and Part 7 HIC out of scope.</li>"
                  "<li>Results near limits require certified engineering review.</li></ul>")
        return (
            "<div class='section'>\n<h2>6. Methodology &amp; References</h2>\n"
            f"<h3>Standards</h3>{std_table}"
            f"<h3>Key Equations</h3>{eq_table}"
            f"<h3>Assumptions &amp; Limitations</h3>{limits}\n</div>\n"
        )

    @staticmethod
    def _section_heatmap(plotly_json: str) -> str:
        return (
            "<div class='section'>\n"
            "<h2>Inspection Data — Wall Thickness Heatmap</h2>\n"
            "<div id='ffs-heatmap'></div>\n"
            "<script src='https://cdn.plot.ly/plotly-2.27.0.min.js'></script>\n"
            f"<script>Plotly.newPlot('ffs-heatmap', {plotly_json});</script>\n"
            "</div>\n"
        )

    @staticmethod
    def _section_appendix(grid_df: pd.DataFrame) -> str:
        # Show up to 20 rows x 10 cols to keep the report manageable
        display_df = grid_df.iloc[:20, :10].round(4)
        nr, nc = display_df.shape
        tr, tc = grid_df.shape
        hdr = "<tr><th>Row\\Col</th>" + "".join(f"<th>{c}</th>" for c in display_df.columns) + "</tr>"
        rows = "".join(
            f"<tr><td>{idx}</td>" + "".join(f"<td>{v:.4f}</td>" for v in row.values) + "</tr>"
            for idx, row in display_df.iterrows()
        )
        note = (f"<p class='disclaimer'>Showing {nr} of {tr} rows, {nc} of {tc} cols.</p>"
                if tr > nr or tc > nc else "")
        return (
            "<div class='section'>\n<h2>Appendix — Grid Data (inches)</h2>\n"
            f"<table>{hdr}{rows}</table>{note}\n</div>\n"
        )
