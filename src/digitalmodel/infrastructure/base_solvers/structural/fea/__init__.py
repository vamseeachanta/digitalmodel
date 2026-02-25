"""
ABOUTME: FEA (Finite Element Analysis) component helpers.
ABOUTME: Migrated from infrastructure/common/ in Phase 2D (WRK-415).
"""

try:
    from .FEAComponents import FEAComponents
    __all__ = ["FEAComponents"]
except ImportError:
    __all__ = []
