"""Section 3: Added mass and damping matrices (stub)."""
from __future__ import annotations

from ..config import HydroMatricesConfig


def build_hydro_matrices(diff, config: HydroMatricesConfig) -> str:
    """Build HTML for added mass / damping matrices section.

    Args:
        diff: OrcFxAPI.Diffraction result object.
        config: HydroMatricesConfig for this section.

    Returns:
        HTML string (stub placeholder).
    """
    return (
        "<div class='section-placeholder border rounded p-3'>"
        "Hydrodynamic matrices (added mass &amp; damping) — not yet implemented."
        "</div>"
    )
