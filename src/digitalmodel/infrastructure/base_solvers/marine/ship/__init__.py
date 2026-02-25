"""
ABOUTME: Ship structural design and fatigue analysis solvers.
ABOUTME: Migrated from infrastructure/common/ in Phase 2D (WRK-415).
"""

from .ship_design import ShipDesign
from .ship_fatigue_analysis import ShipFatigueAnalysis

__all__ = ["ShipDesign", "ShipFatigueAnalysis"]
