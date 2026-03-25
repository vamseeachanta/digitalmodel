"""
Lifecycle Planning Reference — decommissioning-by-design flags and
tieback accommodation scoring for offshore marginal fields.

Connects to:
- WRK-220 (decommissioning analytics)
- WRK-157 (fatigue budget)

References
----------
BSEE Notice-to-Lessees NTL-2016-N07 — Decommissioning Requirements.
API RP 2A-WSD — Appendix A (Grouted pile connections).
OSPAR Decision 98/3 — Disposal of Disused Offshore Installations.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum
from typing import Dict, List, Optional


# ---------------------------------------------------------------------------
# Enumerations
# ---------------------------------------------------------------------------

class DecomFlagSeverity(str, Enum):
    """Decommissioning flag severity level."""
    INFO = "info"
    MODERATE = "moderate"
    HIGH = "high"


class PipelineLayout(str, Enum):
    """Subsea pipeline routing topology."""
    SINGLE_TRUNK = "single_trunk"
    DAISY_CHAIN = "daisy_chain"
    LOOP = "loop"


# ---------------------------------------------------------------------------
# Decommissioning flag
# ---------------------------------------------------------------------------

@dataclass
class DecomFlag:
    """
    A design-choice flag that complicates future decommissioning.

    Attributes
    ----------
    category : str
        Thematic category (e.g. "grouted_connections", "pile_removal").
    description : str
        Human-readable explanation.
    severity : DecomFlagSeverity
        Indicative complexity level.
    mitigation : str
        Recommended design change to reduce decom complexity.
    """

    category: str
    description: str
    severity: DecomFlagSeverity
    mitigation: str


# ---------------------------------------------------------------------------
# Decommissioning-by-design flag evaluation
# ---------------------------------------------------------------------------

def decom_flags_for_design_choices(
    has_grouted_connections: bool,
    pile_removal_strategy: str,
    jacket_pieces: int,
    has_mechanical_connectors: bool,
) -> List[DecomFlag]:
    """
    Evaluate design choices and return decommissioning complexity flags.

    Parameters
    ----------
    has_grouted_connections : bool
        True if pile-to-sleeve or conductor grouted connections are used.
    pile_removal_strategy : str
        One of "cut", "pull", "leave_in_situ".
    jacket_pieces : int
        Number of jacket sections (1 = single-lift; >1 = sectioned).
    has_mechanical_connectors : bool
        True if mechanical (non-grouted) connectors are used for primary
        connections, enabling non-destructive removal.

    Returns
    -------
    List[DecomFlag]
        Flags raised, in descending severity order.
    """
    flags: List[DecomFlag] = []

    if has_grouted_connections and not has_mechanical_connectors:
        flags.append(DecomFlag(
            category="grouted_connections",
            description=(
                "Grouted pile/conductor connections require abrasive cutting "
                "or hydraulic jacking during decommissioning, increasing cost "
                "and offshore duration."
            ),
            severity=DecomFlagSeverity.HIGH,
            mitigation=(
                "Specify mechanical connectors (e.g. Voith, Titan) at critical "
                "interfaces to enable non-destructive separation."
            ),
        ))

    if pile_removal_strategy == "leave_in_situ":
        flags.append(DecomFlag(
            category="pile_removal",
            description=(
                "Piles planned for leave-in-situ disposal may face regulatory "
                "challenge under BSEE NTL-2016-N07 and OSPAR Decision 98/3."
            ),
            severity=DecomFlagSeverity.HIGH,
            mitigation=(
                "Specify cut-at-mudline as default strategy; obtain country-specific "
                "regulatory pre-approval if leave-in-situ is technically justified."
            ),
        ))
    elif pile_removal_strategy == "pull":
        flags.append(DecomFlag(
            category="pile_removal",
            description=(
                "Pull-out pile removal requires hydraulic jack or vibratory hammer "
                "spread; feasibility depends on soil type and pile fatigue history."
            ),
            severity=DecomFlagSeverity.MODERATE,
            mitigation=(
                "Confirm pull-out feasibility in detailed decom planning; "
                "verify no fatigue cracks at pile head connection."
            ),
        ))

    if jacket_pieces == 1:
        flags.append(DecomFlag(
            category="jacket_lift",
            description=(
                "Single-piece jacket requires heavy-lift vessel at decommissioning, "
                "significantly increasing marine spread cost."
            ),
            severity=DecomFlagSeverity.MODERATE,
            mitigation=(
                "Specify pre-installed cut-and-lift connectors at planned "
                "section interfaces to enable multi-piece removal with lighter spread."
            ),
        ))

    return flags


# ---------------------------------------------------------------------------
# Tieback accommodation scoring
# ---------------------------------------------------------------------------

def score_tieback_accommodation(
    manifold_future_slots: int,
    pipeline_layout: PipelineLayout,
    mudmat_expansion_margin_pct: float,
) -> float:
    """
    Score a subsea layout on its ability to accommodate future tiebacks (0–1).

    Parameters
    ----------
    manifold_future_slots : int
        Number of unoccupied connection slots on the manifold reserved
        for future wells or satellite tiebacks (0 = none).
    pipeline_layout : PipelineLayout
        Routing topology; LOOP is most flexible for future laterals.
    mudmat_expansion_margin_pct : float
        Additional mudmat footprint reserved for future template/receiver
        additions (percentage of current mudmat area; >= 0).

    Returns
    -------
    float
        Tieback accommodation score in [0, 1].
    """
    if mudmat_expansion_margin_pct < 0.0:
        raise ValueError(
            "mudmat_expansion_margin_pct must be >= 0.0; "
            f"got {mudmat_expansion_margin_pct}"
        )

    # --- Manifold slot score (0 slots → 0, 4+ slots → 1) ---
    slot_score = min(manifold_future_slots / 4.0, 1.0)

    # --- Pipeline layout score ---
    layout_scores: Dict[PipelineLayout, float] = {
        PipelineLayout.SINGLE_TRUNK: 0.20,
        PipelineLayout.DAISY_CHAIN: 0.55,
        PipelineLayout.LOOP: 1.00,
    }
    layout_score = layout_scores[pipeline_layout]

    # --- Mudmat margin score (0% → 0, 30%+ → 1) ---
    mudmat_score = min(mudmat_expansion_margin_pct / 30.0, 1.0)

    # Equal-weight composite
    return (slot_score + layout_score + mudmat_score) / 3.0


# ---------------------------------------------------------------------------
# Marginal field lifecycle score (composite)
# ---------------------------------------------------------------------------

def marginal_field_lifecycle_score(
    installation_difficulty: int,
    operational_flexibility: int,
    decommissioning_complexity: int,
    expansion_headroom: int,
    tieback_score: float,
) -> Dict[str, object]:
    """
    Produce a composite lifecycle score and design recommendation.

    Combines the lifecycle_value_index (from minimum_facility module) with
    the tieback accommodation score.

    Parameters
    ----------
    installation_difficulty : int
        1 (easy) – 5 (very difficult).
    operational_flexibility : int
        1 (rigid) – 5 (flexible).
    decommissioning_complexity : int
        1 (simple) – 5 (very complex).
    expansion_headroom : int
        1 (none) – 5 (ample).
    tieback_score : float
        Tieback accommodation score in [0, 1].

    Returns
    -------
    dict with keys:
        "lifecycle_value_index" : float — pure lifecycle score [0,1]
        "tieback_combined"      : float — composite score [0,1]
        "recommendation"        : str
    """
    from digitalmodel.structural.offshore_resilience.minimum_facility import (
        lifecycle_value_index,
    )

    if not (0.0 <= tieback_score <= 1.0):
        raise ValueError(
            f"tieback_score must be in [0, 1]; got {tieback_score}"
        )

    lvi = lifecycle_value_index(
        installation_difficulty=installation_difficulty,
        operational_flexibility=operational_flexibility,
        decommissioning_complexity=decommissioning_complexity,
        expansion_headroom=expansion_headroom,
    )

    # Weighted composite: 70 % lifecycle index, 30 % tieback accommodation
    combined = 0.70 * lvi + 0.30 * tieback_score

    recommendation: str
    if combined >= 0.70:
        recommendation = "strongly_recommended"
    elif combined >= 0.50:
        recommendation = "proceed"
    elif combined >= 0.30:
        recommendation = "review"
    else:
        recommendation = "caution"

    return {
        "lifecycle_value_index": lvi,
        "tieback_combined": combined,
        "recommendation": recommendation,
    }
