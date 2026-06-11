"""
FreeCAD Agent Package
"""

from .core.agent import FreeCADAgent
from .api.wrapper import FreeCADAPIWrapper
from .api.geometry import GeometryHandler
from .core.capabilities import CapabilityRegistry

__version__ = "1.0.0"
__all__ = [
    "FreeCADAgent",
    "FreeCADAPIWrapper", 
    "GeometryHandler",
    "CapabilityRegistry"
]