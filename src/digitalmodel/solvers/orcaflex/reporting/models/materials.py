from pydantic import BaseModel, Field
from typing import List, Optional


class LineTypeData(BaseModel):
    """Properties for a single OrcaFlex line type."""
    name: str
    od: float = Field(..., description="Outer Diameter [m]")
    id: float = Field(..., description="Inner Diameter [m]")
    wt: float = Field(..., description="Wall Thickness [m]")
    grade: Optional[str] = None
    smys_mpa: Optional[float] = None
    youngs_modulus_mpa: Optional[float] = None
    density_kg_m3: float = Field(..., description="Material density")
    content_density_kg_m3: Optional[float] = None
    ea_kn: Optional[float] = Field(None, description="Axial stiffness")
    ei_knm2: Optional[float] = Field(None, description="Bending stiffness")
    gj_knm2: Optional[float] = Field(None, description="Torsional stiffness")
    mass_per_m_kg_m: Optional[float] = None


class CoatingData(BaseModel):
    """Coating or insulation layer properties."""
    name: str
    thickness_mm: float
    density_kg_m3: float
    type: Optional[str] = None


class BuoyancyModuleData(BaseModel):
    """Properties of buoyancy modules attached to the line."""
    name: str
    spacing_m: float
    net_buoyancy_kn: float
    arc_length_start_m: float
    arc_length_end_m: float


class MaterialData(BaseModel):
    """Material and line type summary."""
    line_types: List[LineTypeData] = Field(default_factory=list)
    coatings: List[CoatingData] = Field(default_factory=list)
    buoyancy_modules: List[BuoyancyModuleData] = Field(default_factory=list)
    submerged_weight_profile: Optional[dict] = Field(None, description="arc_length and w_s [kN/m] arrays")
