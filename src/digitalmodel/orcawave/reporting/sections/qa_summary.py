"""Section 8: QA pass/fail summary (stub)."""
from __future__ import annotations

from ..config import QASummaryConfig


def build_qa_summary(diff, config: QASummaryConfig) -> str:
    """Build HTML for QA pass/fail summary section.

    Args:
        diff: OrcFxAPI.Diffraction result object.
        config: QASummaryConfig for this section.

    Returns:
        HTML string (stub placeholder).
    """
    return (
        "<div class='section-placeholder border rounded p-3'>"
        "QA pass/fail summary — not yet implemented."
        "</div>"
    )
