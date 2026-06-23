# ABOUTME: Hydrostatics calculations — buoyancy, CG shifts, inclining experiment
# ABOUTME: Validated against USNA EN400 Chapters 2-3 worked examples
"""
Hydrostatics calculations for naval architecture.

Covers buoyant force, submerged volume, center of gravity shifts,
and inclining experiment analysis from USNA EN400 Chapters 2-3.
"""
from __future__ import annotations

import warnings
from pathlib import Path
from typing import Optional

from digitalmodel.citations.registry import get_en400_reference
from digitalmodel.citations.schema import CitationResolutionError

# One-shot guard so standalone mode warns once, not per call (mirrors the
# DNV-OS-E301 mooring pilot's degradation behavior).
_EN400_STANDALONE_WARNED = False

GAMMA_SW = 64.0  # lb/ft³
GAMMA_FW = 62.4  # lb/ft³
LT_TO_LB = 2240.0


def buoyant_force(volume_ft3: float, water: str = "saltwater") -> float:
    """Calculate buoyant force in pounds.

    Fb = gamma * V
    """
    gamma = GAMMA_SW if water == "saltwater" else GAMMA_FW
    return gamma * volume_ft3


def buoyant_force_cited(
    volume_ft3: float, water: str = "saltwater", *, repo_root: Optional[Path] = None
) -> dict:
    """`buoyant_force` with an EN400 provenance sidecar.

    Mirrors the DNV-OS-E301 mooring pilot's opt-in-by-name design: the legacy
    `buoyant_force` stays unchanged; callers opt into citation emission by name.
    Returns ``{"value", "units", "citations"}``.

    Fail-closed: a configured-but-missing/stale wiki page raises
    CitationResolutionError. Standalone mode (resolver unconfigured) degrades
    gracefully with a one-shot RuntimeWarning and an empty ``citations`` list.
    """
    global _EN400_STANDALONE_WARNED
    force = buoyant_force(volume_ft3, water)
    citations: list = []
    try:
        cited = get_en400_reference(
            "Chapter 2 — Hull Form & Buoyancy (F_b = γ·V, Archimedes)",
            note="buoyant force from displaced volume",
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
    return {"value": force, "units": "lb", "citations": citations}


def submerged_volume(
    displacement_lt: float, water: str = "saltwater"
) -> float:
    """Calculate submerged volume from displacement.

    V = W(lb) / gamma
    """
    gamma = GAMMA_SW if water == "saltwater" else GAMMA_FW
    return displacement_lt * LT_TO_LB / gamma


def vertical_cg_after_weight_change(
    displacement_lt: float,
    initial_kg_ft: float,
    weight_lt: float,
    weight_kg_ft: float,
) -> float:
    """New KG after adding/removing weight.

    KG_new = (W1*KG1 + w*kg) / (W1 + w)
    Use negative weight_lt for removal.
    """
    new_disp = displacement_lt + weight_lt
    return (
        displacement_lt * initial_kg_ft + weight_lt * weight_kg_ft
    ) / new_disp


def transverse_cg_after_weight_shift(
    displacement_lt: float,
    initial_tcg_ft: float,
    weight_moved_lt: float,
    distance_ft: float,
    direction: str = "port",
) -> float:
    """New TCG after moving a weight transversely.

    TCG_new = (W*TCG ∓ w*d) / W
    Port shift subtracts, starboard adds.
    """
    sign = -1 if direction == "port" else 1
    return (
        displacement_lt * initial_tcg_ft + sign * weight_moved_lt * distance_ft
    ) / displacement_lt


def gm_from_inclining_experiment(
    weight_moved_lt: float,
    distance_ft: float,
    displacement_lt: float,
    tan_phi: float,
) -> float:
    """Calculate GM from inclining experiment data.

    GM = (w * d) / (W * tan(phi))
    """
    return (weight_moved_lt * distance_ft) / (displacement_lt * tan_phi)
