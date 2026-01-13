from typing import List, Optional
from pydantic import BaseModel, Field
import numpy as np

class CardData(BaseModel):
    """Raw position and load data for a dynamometer card."""
    position: List[float]
    load: List[float]
    timestamp: Optional[str] = None

class RodSection(BaseModel):
    """Properties for a single section of the sucker rod string."""
    diameter: float
    length: float
    modulus_of_elasticity: float = 30000000.0
    density: float = 490.0  # lbs/ft3
    damping_factor: float = 0.05

class PumpProperties(BaseModel):
    """Downhole pump specifications."""
    diameter: float
    depth: float
    efficiency: float = 1.0

class SurfaceUnit(BaseModel):
    """Pumping unit surface equipment specifications."""
    manufacturer: str
    unit_type: str
    stroke_length: float
    gear_box_rating: float
    structural_imbalance: float = 0.0

class DynacardAnalysisContext(BaseModel):
    """Complete context for a single well analysis."""
    api14: str
    surface_card: CardData
    rod_string: List[RodSection]
    pump: PumpProperties
    surface_unit: SurfaceUnit
    spm: float
    fluid_density: float = 62.4
    
    class Config:
        arbitrary_types_allowed = True

class AnalysisResults(BaseModel):
    """Output results from the dynacard analysis."""
    ctx: Optional[DynacardAnalysisContext] = None
    downhole_card: Optional[CardData] = None
    peak_polished_rod_load: float = 0.0
    minimum_polished_rod_load: float = 0.0
    pump_fillage: float = 0.0
    inferred_production: float = 0.0
    buckling_detected: bool = False
    diagnostic_message: str = "Analysis not yet performed"
