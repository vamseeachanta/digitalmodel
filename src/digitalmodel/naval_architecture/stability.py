# ABOUTME: Ship stability calculations — GZ curves, free surface effect
# ABOUTME: Validated against USNA EN400 Chapter 4 worked examples
"""
Ship stability calculations.

Covers righting arm (GZ) from cross curves, TCG cosine correction,
and free surface effect from USNA EN400 Chapter 4.
"""

import math
import warnings
from pathlib import Path
from typing import Optional, Tuple

from digitalmodel.citations.registry import get_en400_reference
from digitalmodel.citations.schema import CitationResolutionError

# One-shot guard so standalone mode warns once, not per call (mirrors the
# DNV-OS-E301 mooring pilot's degradation behavior).
_EN400_STANDALONE_WARNED = False

G = 32.17  # ft/s²
LT_TO_LB = 2240.0


def gz_from_cross_curves(
    heel_deg: float, kn_ft: float, kg_ft: float
) -> float:
    """Corrected righting arm from cross curves of stability.

    GZ = KN - KG * sin(heel)

    Args:
        heel_deg: heel angle in degrees
        kn_ft: KN value from cross curves at this displacement/heel
        kg_ft: height of center of gravity above keel
    """
    return kn_ft - kg_ft * math.sin(math.radians(heel_deg))


def gz_from_cross_curves_cited(
    heel_deg: float,
    kn_ft: float,
    kg_ft: float,
    *,
    repo_root: Optional[Path] = None,
) -> dict:
    """`gz_from_cross_curves` with an EN400 provenance sidecar.

    Mirrors the DNV-OS-E301 mooring pilot's opt-in-by-name design: the legacy
    `gz_from_cross_curves` stays unchanged; callers opt into citation emission
    by name. Returns ``{"value", "units", "citations"}``.

    Fail-closed: a configured-but-missing/stale wiki page raises
    CitationResolutionError. Standalone mode (resolver unconfigured) degrades
    gracefully with a one-shot RuntimeWarning and an empty ``citations`` list.
    """
    global _EN400_STANDALONE_WARNED
    gz = gz_from_cross_curves(heel_deg, kn_ft, kg_ft)
    citations: list = []
    try:
        cited = get_en400_reference(
            "Chapter 4 — Stability (GZ = KN − KG·sin φ)",
            note="righting arm from cross curves",
            repo_root=repo_root,
        )
        citations = [cited.citation]
    except CitationResolutionError as exc:
        if exc.reason.startswith("resolver_unconfigured"):
            if not _EN400_STANDALONE_WARNED:
                warnings.warn(
                    "digitalmodel standalone mode: EN400 citation unavailable; "
                    "proceeding without a provenance sidecar.",
                    RuntimeWarning,
                    stacklevel=2,
                )
                _EN400_STANDALONE_WARNED = True
            citations = []
        else:
            raise
    return {"value": gz, "units": "ft", "citations": citations}


def gz_with_tcg_correction(
    gz_ft: float, tcg_ft: float, heel_deg: float
) -> float:
    """Correct GZ for transverse CG offset (cosine correction).

    GZ_corrected = GZ - TCG * cos(heel)
    """
    return gz_ft - tcg_ft * math.cos(math.radians(heel_deg))


def free_surface_correction(
    displacement_lt: float,
    kg_ft: float,
    km_ft: float,
    tank_length_ft: float,
    tank_breadth_ft: float,
    rho_fluid: float,
) -> Tuple[float, float]:
    """Calculate free surface correction and effective GM.

    FSC = rho * g * i / delta(lb)
    where i = L * B³ / 12 for rectangular tank

    Args:
        displacement_lt: ship displacement in long tons
        kg_ft: KG in feet
        km_ft: KM (metacentric height above keel) in feet
        tank_length_ft: tank length in feet
        tank_breadth_ft: tank breadth in feet
        rho_fluid: fluid density in slug/ft³

    Returns:
        (free_surface_correction_ft, effective_gm_ft)
    """
    gm_solid = km_ft - kg_ft
    i_tank = tank_length_ft * tank_breadth_ft**3 / 12.0
    fsc = rho_fluid * G * i_tank / (displacement_lt * LT_TO_LB)
    gm_effective = gm_solid - fsc
    return fsc, gm_effective
