"""
ABOUTME: Well trajectory and path geometry solvers.
ABOUTME: Migrated from infrastructure/common/ in Phase 2D (WRK-415).
ABOUTME: Note: wellpath3D requires tkinter (GUI) which may not be available on all systems.
"""

try:
    from .wellpath3D import newwell, well, wellmap
    __all__ = ["newwell", "well", "wellmap"]
except ImportError:
    __all__ = []
