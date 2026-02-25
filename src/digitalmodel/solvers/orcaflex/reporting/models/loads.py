from pydantic import BaseModel, Field
from typing import List, Optional


class LoadCaseData(BaseModel):
    """Parameters for a specific design load case."""
    case_id: str
    hs_m: Optional[float] = None
    tp_s: Optional[float] = None
    gamma: Optional[float] = None
    current_velocity_m_s: Optional[float] = None
    current_direction_deg: Optional[float] = None
    internal_pressure_mpa: Optional[float] = None
    content_density_kg_m3: Optional[float] = None
    thermal_delta_t_c: Optional[float] = None
    description: Optional[str] = None


class HydroCoeffData(BaseModel):
    """Hydrodynamic coefficients used in the analysis."""
    line_type_name: str
    cd_normal: float
    cd_axial: Optional[float] = None
    ca_normal: float
    ca_axial: Optional[float] = None
    cm_normal: Optional[float] = None


class EnvironmentData(BaseModel):
    """Environmental loading conditions summary."""
    load_cases: List[LoadCaseData] = Field(default_factory=list)
    hydrodynamic_coefficients: List[HydroCoeffData] = Field(default_factory=list)
    current_profile: Optional[dict] = Field(None, description="depth and velocity arrays")
    wave_scatter_diagram: Optional[dict] = None
    governing_case_strength: Optional[str] = None
    governing_case_fatigue: Optional[str] = None
