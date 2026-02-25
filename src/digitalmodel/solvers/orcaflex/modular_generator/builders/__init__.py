"""Builder modules for OrcaFlex modular model generation."""

from .base import BaseBuilder
from .context import BuilderContext
from .registry import BuilderRegistry

# Import all builders to trigger @BuilderRegistry.register decorators
# Pipeline builders
from .general_builder import GeneralBuilder
from .environment_builder import EnvironmentBuilder
from .vardata_builder import VarDataBuilder
from .linetype_builder import LineTypeBuilder
from .supports_builder import SupportsBuilder
from .morison_builder import MorisonBuilder
from .shapes_builder import ShapesBuilder
from .buoys_builder import BuoysBuilder
from .lines_builder import LinesBuilder
from .groups_builder import GroupsBuilder
from .vessel_type_builder import VesselTypeBuilder
from .vessel_builder import VesselBuilder
from .winch_builder import WinchBuilder

# Riser builders
from .riser_clumptype_builder import RiserClumpTypeBuilder
from .riser_linetype_builder import RiserLineTypeBuilder
from .riser_vessel_builder import RiserVesselBuilder
from .riser_lines_builder import RiserLinesBuilder
from .riser_links_builder import RiserLinksBuilder

# Generic builders
from .generic_builder import GenericModelBuilder

__all__ = [
    'BaseBuilder',
    'BuilderContext',
    'BuilderRegistry',
    # Pipeline builders
    'GeneralBuilder', 'EnvironmentBuilder', 'VarDataBuilder',
    'LineTypeBuilder', 'SupportsBuilder', 'MorisonBuilder',
    'ShapesBuilder', 'BuoysBuilder', 'LinesBuilder', 'GroupsBuilder',
    'VesselTypeBuilder', 'VesselBuilder', 'WinchBuilder',
    # Riser builders
    'RiserClumpTypeBuilder', 'RiserLineTypeBuilder',
    'RiserVesselBuilder', 'RiserLinesBuilder', 'RiserLinksBuilder',
    # Generic builders
    'GenericModelBuilder',
]
