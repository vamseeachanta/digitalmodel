"""Section 4: Mean drift table and polar plot (stub)."""
from __future__ import annotations

from ..config import MeanDriftConfig


def build_mean_drift(diff, config: MeanDriftConfig) -> str:
    """Build HTML for mean drift table and optional polar plot section.

    Args:
        diff: OrcFxAPI.Diffraction result object.
        config: MeanDriftConfig for this section.

    Returns:
        HTML string (stub placeholder).
    """
    polar_note = " (polar plot enabled)" if config.include_polar else ""
    return (
        f"<div class='section-placeholder border rounded p-3'>"
        f"Mean drift table{polar_note} — not yet implemented."
        f"</div>"
    )
