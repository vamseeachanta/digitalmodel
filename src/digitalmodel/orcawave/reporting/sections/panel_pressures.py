"""Section 5: Panel pressure output information (stub)."""
from __future__ import annotations

from ..config import PanelPressuresConfig


def build_panel_pressures(diff, config: PanelPressuresConfig) -> str:
    """Build HTML for panel pressures section.

    Args:
        diff: OrcFxAPI.Diffraction result object.
        config: PanelPressuresConfig for this section.

    Returns:
        HTML string (stub placeholder).
    """
    return (
        "<div class='section-placeholder border rounded p-3'>"
        "Panel pressure contour output — not yet implemented."
        "</div>"
    )
