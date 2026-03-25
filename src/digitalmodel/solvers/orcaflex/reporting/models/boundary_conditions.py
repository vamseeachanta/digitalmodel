from pydantic import BaseModel, Field
from typing import List, Optional, Union


class BCEndData(BaseModel):
    """Boundary condition at a line end."""
    name: str  # e.g., "End A", "Hang-off"
    type: str  # e.g., "Fixed", "Pinned", "Vessel"
    x: float
    y: float
    z: float
    dof_fixity: Optional[str] = None
    connected_to: Optional[str] = None
    flex_joint_stiffness_knm_per_deg: Optional[float] = None


class SeabedModelData(BaseModel):
    """Seabed interaction parameters."""
    type: str = Field(..., description="Linear or Non-linear")
    stiffness_kn_m2: Optional[float] = None
    friction_axial: Optional[float] = None
    friction_lateral: Optional[float] = None
    slope_deg: Optional[float] = None


class ConstraintData(BaseModel):
    """Intermediate constraint or connection."""
    name: str
    arc_length_m: float
    type: str
    connected_to: Optional[str] = None


class BCData(BaseModel):
    """Boundary conditions summary."""
    end_a: Optional[BCEndData] = None
    end_b: Optional[BCEndData] = None
    seabed: Optional[SeabedModelData] = None
    constraints: List[ConstraintData] = Field(default_factory=list)
    relative_displacement_m: Optional[float] = None
