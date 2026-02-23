from pydantic import BaseModel, Field
from typing import List, Optional


class LineProfileData(BaseModel):
    """3D coordinates along the line arc length."""
    arc_length: List[float] = Field(..., description="Arc length along the line [m]")
    x: List[float] = Field(..., description="X coordinate [m]")
    y: List[float] = Field(..., description="Y coordinate [m]")
    z: List[float] = Field(..., description="Z coordinate [m]")


class KeyPointData(BaseModel):
    """Specific location of interest on the structure."""
    label: str = Field(..., description="Name of the key point")
    arc_length_m: float = Field(..., description="Arc length position [m]")
    x: Optional[float] = None
    y: Optional[float] = None
    z: Optional[float] = None


class GeometryData(BaseModel):
    """Structural geometry information."""
    coordinate_system: Optional[str] = "MSL, z-positive-up"
    water_depth_m: Optional[float] = None
    line_profile: Optional[LineProfileData] = None
    key_points: List[KeyPointData] = Field(default_factory=list)
    
    # Structure-specific geometry fields
    hang_off_angle_deg: Optional[float] = None
    tdp_excursion_near_m: Optional[float] = None
    tdp_excursion_far_m: Optional[float] = None
    span_length_m: Optional[float] = None
    arch_height_m: Optional[float] = None
    offset_angle_deg: Optional[float] = None
    kp_chainage_table: List[dict] = Field(default_factory=list, description="List of dictionaries with KP, E, N, WD")
    inline_fittings: List[dict] = Field(default_factory=list, description="List of dictionaries with label, KP")
    seabed_profile: Optional[dict] = None  # Could be another LineProfileData or similar
