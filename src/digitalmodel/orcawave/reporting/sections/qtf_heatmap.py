"""Section 7: QTF heatmap (stub)."""
from __future__ import annotations

from ..config import QTFHeatmapConfig


def build_qtf_heatmap(diff, config: QTFHeatmapConfig) -> str:
    """Build HTML for QTF heatmap section.

    Gracefully skips if QTF results are unavailable on the diff object.

    Args:
        diff: OrcFxAPI.Diffraction result object.
        config: QTFHeatmapConfig for this section.

    Returns:
        HTML string (stub placeholder or unavailable notice).
    """
    qtf_available = _qtf_results_available(diff)
    if not qtf_available:
        return (
            "<div class='section-placeholder border rounded p-3 text-warning'>"
            "QTF results not available in this .owr file — section skipped."
            "</div>"
        )

    return (
        f"<div class='section-placeholder border rounded p-3'>"
        f"QTF heatmap (max &Delta;&omega; = {config.max_delta_omegas}) "
        f"— not yet implemented."
        f"</div>"
    )


def _qtf_results_available(diff) -> bool:
    """Return True if QTF results are present on the Diffraction object."""
    try:
        qtf = diff.QTFResults
        return qtf is not None
    except Exception:
        return False
