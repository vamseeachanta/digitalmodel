"""
Installation-Centric Engineering Checklist.

Formalizes installation constraints as first-class design inputs for both
fixed/floating offshore structures and offshore wind foundations.

Usage
-----
    from digitalmodel.structural.offshore_resilience.installation_checklist import (
        build_structure_checklist,
        build_wind_foundation_checklist,
        FoundationType,
    )
    cl = build_structure_checklist("fixed")
    for item in cl.items:
        item.set_value(actual_value)
    print(cl.summary())
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum
from typing import Dict, List, Optional


# ---------------------------------------------------------------------------
# Enumerations
# ---------------------------------------------------------------------------

class ChecklistStatus(str, Enum):
    """Status of a single checklist parameter."""
    PENDING = "pending"
    PASS = "pass"
    FAIL = "fail"


class HammerType(str, Enum):
    """Offshore pile driving hammer type."""
    HYDRAULIC_IMPACT = "hydraulic_impact"
    VIBRATORY = "vibratory"
    DRILLED_GROUTED = "drilled_grouted"


class FoundationType(str, Enum):
    """Offshore wind foundation type."""
    MONOPILE = "monopile"
    JACKET = "jacket"


_VALID_STRUCTURE_TYPES = frozenset({"fixed", "floating"})


# ---------------------------------------------------------------------------
# Checklist item
# ---------------------------------------------------------------------------

@dataclass
class ChecklistItem:
    """
    A single installation engineering parameter.

    Parameters
    ----------
    category : str
        Grouping label (e.g. "vessel_spread", "weather_window").
    parameter : str
        Machine-readable parameter key.
    description : str
        Human-readable description.
    min_value : float
        Minimum acceptable value (inclusive).
    max_value : float
        Maximum acceptable value (inclusive).
    unit : str
        Physical unit string (e.g. "t", "m", "kJ").
    """

    category: str
    parameter: str
    description: str
    min_value: float
    max_value: float
    unit: str
    status: ChecklistStatus = field(default=ChecklistStatus.PENDING, init=False)
    actual_value: Optional[float] = field(default=None, init=False)

    def set_value(self, value: float) -> None:
        """Evaluate the parameter and set pass/fail status."""
        self.actual_value = value
        if self.min_value <= value <= self.max_value:
            self.status = ChecklistStatus.PASS
        else:
            self.status = ChecklistStatus.FAIL


# ---------------------------------------------------------------------------
# Checklist container
# ---------------------------------------------------------------------------

@dataclass
class InstallationChecklist:
    """Container for a set of checklist items with summary helpers."""

    structure_description: str
    items: List[ChecklistItem] = field(default_factory=list)

    @property
    def pass_count(self) -> int:
        return sum(1 for i in self.items if i.status == ChecklistStatus.PASS)

    @property
    def fail_count(self) -> int:
        return sum(1 for i in self.items if i.status == ChecklistStatus.FAIL)

    @property
    def pending_count(self) -> int:
        return sum(1 for i in self.items if i.status == ChecklistStatus.PENDING)

    @property
    def completion_ratio(self) -> float:
        """Fraction of items that have passed (0.0 – 1.0)."""
        if not self.items:
            return 0.0
        return self.pass_count / len(self.items)

    @property
    def is_complete(self) -> bool:
        """True when all items have passed."""
        return self.pending_count == 0 and self.fail_count == 0

    def summary(self) -> Dict[str, int]:
        return {
            "total": len(self.items),
            "pass": self.pass_count,
            "fail": self.fail_count,
            "pending": self.pending_count,
        }


# ---------------------------------------------------------------------------
# Factory — fixed / floating structures
# ---------------------------------------------------------------------------

def build_structure_checklist(structure_type: str) -> InstallationChecklist:
    """
    Build an installation checklist for a fixed or floating offshore structure.

    Parameters
    ----------
    structure_type : str
        "fixed" or "floating".

    Returns
    -------
    InstallationChecklist
    """
    if structure_type not in _VALID_STRUCTURE_TYPES:
        raise ValueError(
            f"structure_type must be one of {sorted(_VALID_STRUCTURE_TYPES)}; "
            f"got '{structure_type}'"
        )

    items: List[ChecklistItem] = [
        # ---- Vessel spread ----
        ChecklistItem(
            category="vessel_spread",
            parameter="crane_capacity_tonnes",
            description="Maximum crane hook load (t); must exceed structure weight",
            min_value=200.0,
            max_value=10_000.0,
            unit="t",
        ),
        ChecklistItem(
            category="vessel_spread",
            parameter="vessel_deck_area_m2",
            description="Available lay-down deck area for jacket/topside pre-assembly",
            min_value=500.0,
            max_value=50_000.0,
            unit="m2",
        ),
        # ---- Weather window ----
        ChecklistItem(
            category="weather_window",
            parameter="max_hs_lift_m",
            description="Maximum Hs permitted during crane lift operations",
            min_value=0.0,
            max_value=2.5,
            unit="m",
        ),
        ChecklistItem(
            category="weather_window",
            parameter="required_window_days",
            description="Consecutive low-sea-state days required for installation",
            min_value=1.0,
            max_value=30.0,
            unit="days",
        ),
        # ---- Pile hammer ----
        ChecklistItem(
            category="pile_hammer",
            parameter="max_driving_energy_kj",
            description="Maximum available hammer driving energy (kJ)",
            min_value=100.0,
            max_value=3_000.0,
            unit="kJ",
        ),
        ChecklistItem(
            category="pile_hammer",
            parameter="soil_refusal_blowcount_per_m",
            description="Soil refusal blow count threshold (blows/m)",
            min_value=50.0,
            max_value=800.0,
            unit="blows/m",
        ),
    ]

    if structure_type == "floating":
        items.extend([
            ChecklistItem(
                category="towout_stability",
                parameter="metacentric_height_m",
                description="GM (metacentric height) during tow-out (m)",
                min_value=0.1,
                max_value=5.0,
                unit="m",
            ),
            ChecklistItem(
                category="towout_stability",
                parameter="max_hs_towout_m",
                description="Maximum Hs permitted during tow-out",
                min_value=0.0,
                max_value=3.5,
                unit="m",
            ),
        ])

    return InstallationChecklist(
        structure_description=f"{structure_type} offshore structure",
        items=items,
    )


# ---------------------------------------------------------------------------
# Factory — offshore wind foundations
# ---------------------------------------------------------------------------

def build_wind_foundation_checklist(
    foundation_type: FoundationType,
) -> InstallationChecklist:
    """
    Build an installation checklist for an offshore wind foundation.

    Parameters
    ----------
    foundation_type : FoundationType
        MONOPILE or JACKET.

    Returns
    -------
    InstallationChecklist
    """
    items: List[ChecklistItem] = [
        # ---- Vessel spread (common) ----
        ChecklistItem(
            category="vessel_spread",
            parameter="crane_capacity_tonnes",
            description="Crane capacity for transition piece / jacket lift",
            min_value=500.0,
            max_value=5_000.0,
            unit="t",
        ),
        # ---- Weather window (common) ----
        ChecklistItem(
            category="weather_window",
            parameter="max_hs_piling_m",
            description="Max Hs permitted during pile installation",
            min_value=0.0,
            max_value=2.0,
            unit="m",
        ),
    ]

    if foundation_type == FoundationType.MONOPILE:
        items.extend([
            # ---- Pile run analysis ----
            ChecklistItem(
                category="pile_run",
                parameter="pile_run_penetration_m",
                description=(
                    "Predicted free-running pile penetration before"
                    " hammer engagement (m)"
                ),
                min_value=0.0,
                max_value=10.0,
                unit="m",
            ),
            ChecklistItem(
                category="pile_run",
                parameter="soil_plug_factor",
                description="Internal soil plug plugging ratio (0=fully coring, 1=fully plugged)",
                min_value=0.0,
                max_value=1.0,
                unit="-",
            ),
            # ---- Hammer selection ----
            ChecklistItem(
                category="hammer_selection",
                parameter="hammer_rated_energy_kj",
                description="IHC/Menck hydraulic impact hammer rated energy (kJ)",
                min_value=500.0,
                max_value=4_000.0,
                unit="kJ",
            ),
            ChecklistItem(
                category="hammer_selection",
                parameter="blow_count_at_target_depth",
                description="Predicted blow count at final pile toe depth (blows/0.25m)",
                min_value=1.0,
                max_value=250.0,
                unit="blows/0.25m",
            ),
        ])

    elif foundation_type == FoundationType.JACKET:
        items.extend([
            ChecklistItem(
                category="pile_hammer",
                parameter="max_driving_energy_kj",
                description="Hydraulic hammer max driving energy for jacket piles",
                min_value=200.0,
                max_value=2_500.0,
                unit="kJ",
            ),
            # ---- In-place fatigue during construction ----
            ChecklistItem(
                category="construction_fatigue",
                parameter="fatigue_damage_during_installation",
                description=(
                    "Accumulated fatigue damage fraction during installation"
                    " sequence (must remain < design budget fraction)"
                ),
                min_value=0.0,
                max_value=0.05,
                unit="-",
            ),
        ])

    return InstallationChecklist(
        structure_description=f"offshore wind {foundation_type.value} foundation",
        items=items,
    )
