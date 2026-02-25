"""Section 6: Multi-body coupling matrix (stub)."""
from __future__ import annotations

from ..config import MultiBodyConfig


def build_multi_body(diff, config: MultiBodyConfig) -> str:
    """Build HTML for multi-body coupling matrix section.

    Args:
        diff: OrcFxAPI.Diffraction result object.
        config: MultiBodyConfig for this section.

    Returns:
        HTML string (stub placeholder).
    """
    return (
        "<div class='section-placeholder border rounded p-3'>"
        "Multi-body coupling matrix — not yet implemented."
        "</div>"
    )
