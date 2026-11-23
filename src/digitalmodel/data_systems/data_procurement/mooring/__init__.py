# ABOUTME: Mooring systems data procurement module for marine engineering
# ABOUTME: Component databases (chains, ropes, anchors) with OrcaFlex export

"""
Mooring Systems Data Procurement Module
========================================

Repository-based component databases for mooring system design.

Key Components:
- Chain database (R3-R6 grades, 50-200mm diameters)
- Wire rope database (IWRC, FC constructions)
- Synthetic rope database (polyester, nylon, HMPE)
- Anchor database (drag embedment, suction pile, driven)
- Connector database (shackles, links, swivels)

Architecture:
- Repository-based (local JSON databases, not web APIs)
- Component selection by design criteria
- Standards validation (API RP 2SK, DNV-OS-E301)
- Direct OrcaFlex line type export (in-memory)
"""

from .client import MooringClient

__version__ = "3.0.0"  # Phase 3: Mooring Systems
__all__ = [
    "MooringClient",
]
