# ABOUTME: HTML report builders for the unified parametric study coordinator (WRK-256)
# ABOUTME: Generates self-contained HTML pages for wall-thickness, fatigue, and combined studies

"""
Parametric Study HTML Report Builders
======================================

Private module — imported by :mod:`digitalmodel.structural.parametric_coordinator`.
Provides self-contained HTML generation for wall-thickness, fatigue, OrcaFlex
campaign, and combined study results.
"""

from __future__ import annotations

from typing import TYPE_CHECKING

import pandas as pd

if TYPE_CHECKING:
    from digitalmodel.structural.parametric_coordinator import StudyResult

# ---------------------------------------------------------------------------
# Shared constants
# ---------------------------------------------------------------------------

_SHARED_CSS = (
    "body{font-family:Arial,sans-serif;margin:20px}"
    "h1{color:#2c3e50}"
    "h2{color:#34495e;margin-top:30px}"
    ".result-table{border-collapse:collapse;width:100%}"
    ".result-table th,.result-table td{border:1px solid #ddd;padding:6px;text-align:right}"
    ".result-table th{background:#4472C4;color:#fff}"
)

_WT_DISPLAY_COLUMNS = [
    "wall_thickness_mm", "outer_diameter_mm", "grade",
    "internal_pressure_MPa", "external_pressure_MPa",
    "max_utilisation", "governing_check", "is_safe",
]


# ---------------------------------------------------------------------------
# Public builders
# ---------------------------------------------------------------------------

def build_wt_html(df: pd.DataFrame, title: str) -> str:
    """Return a self-contained HTML page for wall-thickness sweep results."""
    table_html = ""
    if not df.empty:
        cols = [c for c in _WT_DISPLAY_COLUMNS if c in df.columns]
        table_html = df[cols].to_html(
            index=False, float_format="{:.4f}".format, classes="result-table"
        )

    return (
        f"<!DOCTYPE html><html><head><title>{title}</title>"
        f"<style>{_SHARED_CSS}</style></head>"
        f"<body><h1>{title}</h1>"
        f"<p>Combinations: {len(df)}</p>"
        f"{table_html}</body></html>"
    )


def build_combined_html(
    wt_result: "StudyResult",
    fat_result: "StudyResult",
) -> str:
    """Merge wall-thickness and fatigue HTML into one self-contained document.

    CSS is embedded in the combined document's own ``<head>`` so that
    styling is not lost when sub-report body content is extracted.
    """
    title = "Combined Wall Thickness + Fatigue Parametric Study"

    wt_section = (
        "<h2>Wall Thickness Analysis</h2>"
        + _body_content(wt_result.html_report)
    )
    fat_section = (
        "<h2>Fatigue Analysis</h2>"
        + _body_content(fat_result.html_report)
    )

    return (
        f"<!DOCTYPE html><html><head><title>{title}</title>"
        f"<style>{_SHARED_CSS}</style></head>"
        f"<body><h1>{title}</h1>"
        f"{wt_section}"
        f"{fat_section}"
        f"</body></html>"
    )


def build_simple_html(df: pd.DataFrame, title: str) -> str:
    """Return a minimal self-contained HTML page for an arbitrary DataFrame."""
    table_html = df.to_html(index=False, classes="result-table") if not df.empty else ""
    return (
        f"<!DOCTYPE html><html><head><title>{title}</title>"
        f"<style>{_SHARED_CSS}</style></head>"
        f"<body><h1>{title}</h1><p>Combinations: {len(df)}</p>"
        f"{table_html}</body></html>"
    )


# ---------------------------------------------------------------------------
# Private helpers
# ---------------------------------------------------------------------------

def _body_content(html: str) -> str:
    """Extract text between ``<body>`` and ``</body>``; return full html on miss."""
    if "<body>" in html:
        return html.split("<body>", 1)[1].rsplit("</body>", 1)[0]
    return html
