#!/usr/bin/env python3
# ABOUTME: Shared HTML report template for all GTM demo reports.
# ABOUTME: digitalmodel branded, Plotly charts, PDF-friendly CSS, LIVE MODE teaser.
"""
GTM Demo Report Template
=========================

Shared report builder used by all 5 GTM demos. Produces self-contained HTML
files with embedded Plotly charts, digitalmodel branding, and print-friendly
CSS for PDF export.

Usage::

    from report_template import GTMReportBuilder

    report = GTMReportBuilder(
        title="Wall Thickness Multi-Code Comparison",
        subtitle="108 parametric cases across 6 pipe sizes and 3 design codes",
        demo_id="demo_02",
        case_count=108,
    )
    report.add_section("Methodology", methodology_html)
    report.add_chart("lifecycle_util", fig)  # Plotly figure
    report.add_table("results_summary", df)  # pandas DataFrame
    report.add_live_mode_teaser()
    report.build("output/demo_02_wall_thickness_report.html")
"""

from __future__ import annotations

import json
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

import pandas as pd

try:
    import plotly.graph_objects as go
    import plotly.io as pio
except ImportError:
    go = None  # type: ignore[assignment]
    pio = None  # type: ignore[assignment]


# ── Brand colours ────────────────────────────────────────────────────────────

COLORS = {
    "primary": "#1a365d",       # deep navy
    "secondary": "#2c5282",     # medium blue
    "accent": "#ed8936",        # warm orange (CTA / highlights)
    "success": "#38a169",       # green (PASS)
    "warning": "#d69e2e",       # amber (MARGINAL)
    "danger": "#e53e3e",        # red (FAIL)
    "bg_light": "#f7fafc",      # light grey background
    "bg_dark": "#1a202c",       # dark header
    "text": "#2d3748",          # body text
    "text_muted": "#718096",    # secondary text
    "border": "#e2e8f0",        # light border
    "chart_bg": "#ffffff",      # chart background
}

# Plotly trace colours for multi-series charts
CHART_PALETTE = [
    "#2c5282",  # blue (DNV)
    "#ed8936",  # orange (API)
    "#38a169",  # green (PD8010)
    "#e53e3e",  # red
    "#805ad5",  # purple
    "#d69e2e",  # amber
    "#3182ce",  # lighter blue
    "#dd6b20",  # darker orange
]


# ── CSS ──────────────────────────────────────────────────────────────────────

_CSS = """
:root {
    --clr-primary: %(primary)s;
    --clr-secondary: %(secondary)s;
    --clr-accent: %(accent)s;
    --clr-success: %(success)s;
    --clr-warning: %(warning)s;
    --clr-danger: %(danger)s;
    --clr-bg: %(bg_light)s;
    --clr-text: %(text)s;
    --clr-muted: %(text_muted)s;
    --clr-border: %(border)s;
}

* { margin: 0; padding: 0; box-sizing: border-box; }

body {
    font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
    color: var(--clr-text);
    background: var(--clr-bg);
    line-height: 1.6;
    font-size: 15px;
}

/* ── Header ─────────────────────────────────────────────────── */
.report-header {
    background: linear-gradient(135deg, var(--clr-primary) 0%%, var(--clr-secondary) 100%%);
    color: white;
    padding: 2.5rem 3rem;
    position: relative;
    overflow: hidden;
}
.report-header::after {
    content: '';
    position: absolute;
    top: -50%%; right: -10%%;
    width: 400px; height: 400px;
    background: rgba(255,255,255,0.03);
    border-radius: 50%%;
}
.report-header .brand {
    font-size: 0.85rem;
    letter-spacing: 2px;
    text-transform: uppercase;
    opacity: 0.8;
    margin-bottom: 0.5rem;
}
.report-header h1 {
    font-size: 1.8rem;
    font-weight: 700;
    margin-bottom: 0.3rem;
}
.report-header .subtitle {
    font-size: 1.05rem;
    opacity: 0.85;
    font-weight: 400;
}
.report-header .case-badge {
    display: inline-block;
    background: var(--clr-accent);
    color: white;
    padding: 0.3rem 1rem;
    border-radius: 20px;
    font-weight: 700;
    font-size: 0.95rem;
    margin-top: 1rem;
}
.report-header .meta {
    margin-top: 1rem;
    font-size: 0.8rem;
    opacity: 0.6;
}

/* ── Content ────────────────────────────────────────────────── */
.report-body {
    max-width: 1200px;
    margin: 0 auto;
    padding: 2rem 3rem;
}

.section {
    margin-bottom: 2.5rem;
}
.section h2 {
    font-size: 1.3rem;
    color: var(--clr-primary);
    border-bottom: 2px solid var(--clr-accent);
    padding-bottom: 0.4rem;
    margin-bottom: 1rem;
}
.section h3 {
    font-size: 1.05rem;
    color: var(--clr-secondary);
    margin: 1rem 0 0.5rem;
}
.section p, .section li {
    color: var(--clr-text);
    margin-bottom: 0.5rem;
}
.section ul { padding-left: 1.5rem; }

/* ── Charts ─────────────────────────────────────────────────── */
.chart-container {
    background: white;
    border: 1px solid var(--clr-border);
    border-radius: 8px;
    padding: 1.5rem;
    margin-bottom: 2rem;
    box-shadow: 0 1px 3px rgba(0,0,0,0.06);
}
.chart-title {
    font-size: 1rem;
    font-weight: 600;
    color: var(--clr-primary);
    margin-bottom: 0.3rem;
}
.chart-subtitle {
    font-size: 0.85rem;
    color: var(--clr-muted);
    margin-bottom: 1rem;
}

/* ── Tables ─────────────────────────────────────────────────── */
.data-table {
    width: 100%%;
    border-collapse: collapse;
    font-size: 0.85rem;
    margin-bottom: 1.5rem;
}
.data-table th {
    background: var(--clr-primary);
    color: white;
    padding: 0.6rem 0.8rem;
    text-align: left;
    font-weight: 600;
    font-size: 0.8rem;
    text-transform: uppercase;
    letter-spacing: 0.5px;
}
.data-table td {
    padding: 0.5rem 0.8rem;
    border-bottom: 1px solid var(--clr-border);
}
.data-table tr:nth-child(even) { background: rgba(247,250,252,0.5); }
.data-table tr:hover { background: rgba(44,82,130,0.04); }

.status-pass { color: var(--clr-success); font-weight: 700; }
.status-fail { color: var(--clr-danger); font-weight: 700; }
.status-marginal { color: var(--clr-warning); font-weight: 700; }

/* ── Live Mode Teaser ───────────────────────────────────────── */
.live-mode-teaser {
    background: linear-gradient(135deg, #1a202c 0%%, #2d3748 100%%);
    color: white;
    padding: 2rem 2.5rem;
    border-radius: 8px;
    margin: 2rem 0;
    position: relative;
    overflow: hidden;
}
.live-mode-teaser::before {
    content: 'LIVE';
    position: absolute;
    top: 1rem; right: 1.5rem;
    background: var(--clr-danger);
    color: white;
    padding: 0.2rem 0.8rem;
    border-radius: 12px;
    font-size: 0.75rem;
    font-weight: 700;
    letter-spacing: 1px;
    animation: pulse-live 2s infinite;
}
@keyframes pulse-live {
    0%%, 100%% { opacity: 1; }
    50%% { opacity: 0.5; }
}
.live-mode-teaser h3 {
    color: var(--clr-accent);
    font-size: 1.15rem;
    margin-bottom: 0.8rem;
}
.live-mode-teaser p {
    color: rgba(255,255,255,0.85);
    font-size: 0.95rem;
    line-height: 1.7;
}
.live-mode-teaser .cta {
    display: inline-block;
    margin-top: 1rem;
    background: var(--clr-accent);
    color: white;
    padding: 0.6rem 1.5rem;
    border-radius: 6px;
    text-decoration: none;
    font-weight: 600;
    font-size: 0.9rem;
}

/* ── Funding Badge ──────────────────────────────────────────── */
.funding-badge {
    background: rgba(237,137,54,0.1);
    border: 1px solid var(--clr-accent);
    border-radius: 6px;
    padding: 0.8rem 1.2rem;
    margin: 1rem 0;
    font-size: 0.85rem;
    color: var(--clr-text);
}
.funding-badge strong { color: var(--clr-accent); }

/* ── Footer ─────────────────────────────────────────────────── */
.report-footer {
    border-top: 2px solid var(--clr-border);
    padding: 1.5rem 3rem;
    max-width: 1200px;
    margin: 0 auto;
    font-size: 0.8rem;
    color: var(--clr-muted);
}
.report-footer .disclaimer {
    font-style: italic;
    margin-bottom: 0.5rem;
}

/* ── Print / PDF ────────────────────────────────────────────── */
@media print {
    body { background: white; font-size: 12px; }
    .report-header { padding: 1.5rem 2rem; }
    .report-body { padding: 1rem 2rem; }
    .chart-container { break-inside: avoid; page-break-inside: avoid; }
    .live-mode-teaser { break-inside: avoid; }
    .no-print { display: none !important; }
    @page { margin: 1.5cm; }
}
""" % COLORS


# ── Builder ──────────────────────────────────────────────────────────────────

class GTMReportBuilder:
    """Build a branded GTM demo HTML report."""

    def __init__(
        self,
        title: str,
        subtitle: str,
        demo_id: str,
        case_count: int,
        code_refs: list[str] | None = None,
    ):
        self.title = title
        self.subtitle = subtitle
        self.demo_id = demo_id
        self.case_count = case_count
        self.code_refs = code_refs or []
        self._sections: list[str] = []
        self._charts: list[tuple[str, str, str, Any]] = []  # (id, title, subtitle, fig_html)
        self._timestamp = datetime.now(timezone.utc).strftime("%Y-%m-%d %H:%M UTC")

    # ── Add content ──────────────────────────────────────────

    def add_section(self, title: str, html_content: str) -> None:
        """Add a free-form HTML section."""
        self._sections.append(
            f'<div class="section"><h2>{title}</h2>{html_content}</div>'
        )

    def add_chart(
        self,
        chart_id: str,
        fig: Any,
        title: str = "",
        subtitle: str = "",
    ) -> None:
        """Add a Plotly figure as an embedded chart.

        Parameters
        ----------
        chart_id : str
            Unique identifier for the chart div.
        fig : plotly.graph_objects.Figure
            Plotly figure object.
        title : str
            Chart heading.
        subtitle : str
            Chart description / code reference.
        """
        if pio is None:
            raise ImportError("plotly is required for chart embedding")

        # Apply brand styling to figure
        fig.update_layout(
            template="plotly_white",
            font=dict(family="Inter, sans-serif", size=13, color=COLORS["text"]),
            paper_bgcolor=COLORS["chart_bg"],
            plot_bgcolor=COLORS["chart_bg"],
            margin=dict(l=60, r=30, t=40, b=60),
            hoverlabel=dict(
                bgcolor="white",
                font_size=12,
                font_family="Inter, sans-serif",
            ),
        )

        fig_html = pio.to_html(
            fig,
            full_html=False,
            include_plotlyjs=False,
            div_id=f"chart-{chart_id}",
            config={
                "displayModeBar": True,
                "modeBarButtonsToRemove": ["lasso2d", "select2d"],
                "displaylogo": False,
                "toImageButtonOptions": {
                    "format": "png",
                    "filename": f"{self.demo_id}_{chart_id}",
                    "height": 600,
                    "width": 1000,
                    "scale": 2,
                },
            },
        )
        self._charts.append((chart_id, title, subtitle, fig_html))

    def add_table(
        self,
        title: str,
        df: pd.DataFrame,
        subtitle: str = "",
        status_col: str | None = None,
    ) -> None:
        """Add a pandas DataFrame as a styled HTML table.

        Parameters
        ----------
        title : str
            Table heading.
        df : pd.DataFrame
            Data to render.
        subtitle : str
            Table description.
        status_col : str, optional
            Column name containing PASS/FAIL/GO/NO_GO values for colour coding.
        """
        # Convert to HTML
        table_html = df.to_html(
            classes="data-table",
            index=False,
            border=0,
            na_rep="—",
            escape=False,
        )

        # Colour-code status cells
        if status_col and status_col in df.columns:
            table_html = table_html.replace(
                ">PASS<", ' class="status-pass">PASS<'
            ).replace(
                ">FAIL<", ' class="status-fail">FAIL<'
            ).replace(
                ">GO<", ' class="status-pass">GO<'
            ).replace(
                ">NO_GO<", ' class="status-fail">NO_GO<'
            ).replace(
                ">MARGINAL<", ' class="status-marginal">MARGINAL<'
            )

        section_html = f"""
        <div class="section">
            <h2>{title}</h2>
            {"<p>" + subtitle + "</p>" if subtitle else ""}
            {table_html}
        </div>
        """
        self._sections.append(section_html)

    def add_live_mode_teaser(self, analysis_type: str = "this analysis") -> None:
        """Add the LIVE MODE operations teaser section."""
        teaser = f"""
        <div class="live-mode-teaser">
            <h3>Take This Analysis Live During Operations</h3>
            <p>
                This report used <strong>design sea states</strong> to screen
                parametric cases overnight. During the actual operation,
                <strong>digitalmodel</strong> can feed your vessel's measured
                motion data — VMMS, IMMS, MRU — directly into the same
                engineering models for <strong>real-time go/no-go decisions</strong>.
            </p>
            <p>
                Instead of relying on forecasted Hs limits, {analysis_type}
                updates continuously with actual crane tip motions, hook loads,
                and dynamic amplification factors measured on your vessel.
            </p>
            <div class="funding-badge">
                <strong>$500K VC-funded</strong> — we are building overnight
                engineering and real-time operations support exclusively for
                marine installation contractors. Early adopters get priority
                onboarding and custom model calibration for their fleet.
            </div>
            <a class="cta" href="mailto:info@aceengineer.com?subject=digitalmodel%20Demo%20Inquiry">
                Schedule a Technical Demo →
            </a>
        </div>
        """
        self._sections.append(teaser)

    def add_methodology(self, html_content: str, code_refs: list[str] | None = None) -> None:
        """Add a methodology section with code references."""
        refs = code_refs or self.code_refs
        refs_html = ""
        if refs:
            refs_html = "<h3>Applicable Codes & Standards</h3><ul>"
            for ref in refs:
                refs_html += f"<li>{ref}</li>"
            refs_html += "</ul>"

        self._sections.insert(0, f"""
        <div class="section">
            <h2>Methodology</h2>
            {html_content}
            {refs_html}
        </div>
        """)

    def add_assumptions(self, assumptions: list[str]) -> None:
        """Add an assumptions & limitations section."""
        items = "".join(f"<li>{a}</li>" for a in assumptions)
        self._sections.append(f"""
        <div class="section">
            <h2>Assumptions & Limitations</h2>
            <ul>{items}</ul>
        </div>
        """)

    # ── Build ────────────────────────────────────────────────

    def build(self, output_path: str | Path) -> str:
        """Render the complete HTML report and write to file.

        Parameters
        ----------
        output_path : str or Path
            Where to write the HTML file.

        Returns
        -------
        str
            The complete HTML string.
        """
        output_path = Path(output_path)
        output_path.parent.mkdir(parents=True, exist_ok=True)

        # Build charts HTML
        charts_html = ""
        for chart_id, title, subtitle, fig_html in self._charts:
            charts_html += f"""
            <div class="chart-container">
                {"<div class='chart-title'>" + title + "</div>" if title else ""}
                {"<div class='chart-subtitle'>" + subtitle + "</div>" if subtitle else ""}
                {fig_html}
            </div>
            """

        # Build sections HTML
        sections_html = "\n".join(self._sections)

        # Code references for footer
        codes_footer = ""
        if self.code_refs:
            codes_footer = "<strong>References:</strong> " + " · ".join(self.code_refs)

        html = f"""<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{self.title} — digitalmodel</title>
    <link rel="preconnect" href="https://fonts.googleapis.com">
    <link href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap" rel="stylesheet">
    <script src="https://cdn.plot.ly/plotly-2.32.0.min.js"></script>
    <style>{_CSS}</style>
</head>
<body>

<div class="report-header">
    <div class="brand">digitalmodel — Engineering Intelligence</div>
    <h1>{self.title}</h1>
    <div class="subtitle">{self.subtitle}</div>
    <div class="case-badge">{self.case_count:,} parametric cases analysed overnight</div>
    <div class="meta">Generated {self._timestamp} · {self.demo_id}</div>
</div>

<div class="report-body">
    {sections_html}
    {charts_html}
</div>

<div class="report-footer">
    <p class="disclaimer">
        These results are preliminary engineering estimates generated by automated
        parametric analysis. All outputs require review by a qualified engineer
        before use in design or operational decisions.
    </p>
    <p>{codes_footer}</p>
    <p>© {datetime.now().year} digitalmodel · aceengineer.com</p>
</div>

</body>
</html>"""

        output_path.write_text(html, encoding="utf-8")
        return html

    # ── Utilities ────────────────────────────────────────────

    @staticmethod
    def status_class(status: str) -> str:
        """Return CSS class for a status string."""
        s = status.upper().replace(" ", "_")
        if s in ("PASS", "GO"):
            return "status-pass"
        elif s in ("FAIL", "NO_GO"):
            return "status-fail"
        elif s in ("MARGINAL",):
            return "status-marginal"
        return ""

    @staticmethod
    def format_status(status: str) -> str:
        """Wrap a status string in a coloured span."""
        cls = GTMReportBuilder.status_class(status)
        return f'<span class="{cls}">{status}</span>'

    @staticmethod
    def utilisation_color(util: float) -> str:
        """Return hex colour for a utilisation ratio."""
        if util <= 0.7:
            return COLORS["success"]
        elif util <= 0.9:
            return COLORS["warning"]
        else:
            return COLORS["danger"]
