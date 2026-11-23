"""Design Tools package."""

from digitalmodel.visualization.design_tools.design_table import DesignTable
from digitalmodel.visualization.design_tools.hull_hydrostatics import (
    HullHydrostatics,
)
from digitalmodel.visualization.design_tools.manifold_check import (
    ManifoldChecker,
)

try:
    from digitalmodel.visualization.design_tools.freecad_hull import (
        FREECAD_AVAILABLE,
        FreeCADHullGenerator,
    )
except ImportError:
    FREECAD_AVAILABLE = False

__all__ = [
    "DesignTable",
    "HullHydrostatics",
    "ManifoldChecker",
    "FreeCADHullGenerator",
    "FREECAD_AVAILABLE",
]
