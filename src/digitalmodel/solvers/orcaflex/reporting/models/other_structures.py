from pydantic import BaseModel, Field
from typing import List, Optional


class AttachedStructureData(BaseModel):
    """Additional objects attached to the primary structure."""
    name: str
    type: str  # e.g., "Buoyancy Module", "Clamp", "Vessel"
    mass_kg: float
    arc_length_m: Optional[float] = None
    x: Optional[float] = None
    y: Optional[float] = None
    z: Optional[float] = None
    connected_to: Optional[str] = None


class OtherStructuresData(BaseModel):
    """Inventory of all non-primary structures in the model."""
    attached_structures: List[AttachedStructureData] = Field(default_factory=list)
    vessels: List[dict] = Field(default_factory=list)
    minimum_clearance_m: Optional[float] = None
