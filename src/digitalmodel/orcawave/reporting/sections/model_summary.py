"""Section 1: Model summary — geometry, mesh stats, periods, headings."""
from __future__ import annotations

import math

from ..config import ModelSummaryConfig


def build_model_summary(diff, config: ModelSummaryConfig) -> str:
    """Build HTML table summarising the OrcaWave model properties.

    Args:
        diff: OrcFxAPI.Diffraction result object (already loaded).
        config: ModelSummaryConfig for this section.

    Returns:
        HTML string for the model summary table.
    """
    rows: list[tuple[str, str]] = []

    # --- Body count (inferred from addedMass shape) ---
    n_bodies = _infer_body_count(diff)
    rows.append(("Number of bodies", str(n_bodies)))

    # --- Frequencies / periods ---
    freqs_hz = list(diff.frequencies)  # Hz, descending
    n_freqs = len(freqs_hz)
    if n_freqs > 0:
        freqs_sorted = sorted(freqs_hz)
        period_min = 1.0 / freqs_sorted[-1] if freqs_sorted[-1] > 0 else float("inf")
        period_max = 1.0 / freqs_sorted[0] if freqs_sorted[0] > 0 else float("inf")
        rows.append(("Number of periods / frequencies", str(n_freqs)))
        rows.append(("Period range (s)", f"{period_min:.2f} – {period_max:.2f}"))
    else:
        rows.append(("Number of periods / frequencies", "0"))

    # --- Headings ---
    headings = list(diff.headings)
    n_headings = len(headings)
    if n_headings > 0:
        hdg_min = min(headings)
        hdg_max = max(headings)
        rows.append(("Number of headings", str(n_headings)))
        rows.append(("Heading range (deg)", f"{hdg_min:.1f} – {hdg_max:.1f}"))
    else:
        rows.append(("Number of headings", "0"))

    # --- Water depth ---
    depth = _safe_get(diff, "WaterDepth", None)
    if depth is not None:
        depth_str = f"{depth:.1f} m" if not math.isinf(depth) else "Infinite"
        rows.append(("Water depth", depth_str))

    # --- Water density ---
    density = _safe_get(diff, "WaterDensity", None)
    if density is not None:
        rows.append(("Water density (kg/m³)", f"{density:.1f}"))

    return _render_table(rows)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _infer_body_count(diff) -> int:
    """Infer body count from addedMass shape (6N × 6N per frequency)."""
    try:
        am = diff.addedMass
        return am.shape[1] // 6
    except Exception:
        return 1


def _safe_get(obj, attr: str, default):
    """Return attribute value or default if unavailable."""
    try:
        return getattr(obj, attr)
    except Exception:
        return default


def _render_table(rows: list[tuple[str, str]]) -> str:
    """Render a two-column Bootstrap table from (label, value) pairs."""
    tr_rows = "\n".join(
        f"    <tr><th scope='row'>{label}</th><td>{value}</td></tr>"
        for label, value in rows
    )
    return (
        "<div class='table-responsive' style='max-width:600px'>\n"
        "  <table class='table table-sm table-bordered'>\n"
        "    <thead class='table-light'>"
        "<tr><th>Property</th><th>Value</th></tr></thead>\n"
        f"    <tbody>\n{tr_rows}\n    </tbody>\n"
        "  </table>\n"
        "</div>"
    )
