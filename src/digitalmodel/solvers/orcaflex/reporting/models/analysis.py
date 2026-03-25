from pydantic import BaseModel, Field
from typing import Optional, Dict, Any


class SolverSettingsData(BaseModel):
    """Detailed OrcaFlex solver configuration."""
    static_convergence_criterion: float
    static_max_iterations: int
    dynamic_time_step_s: float
    ramp_duration_s: float
    simulation_duration_s: float
    wave_theory: str
    damping_model: Optional[str] = None
    orcaflex_version: str
    python_api_version: Optional[str] = None


class AnalysisSetupData(BaseModel):
    """General analysis configuration metadata."""
    analysis_types: str = Field(..., description="static, time-domain dynamic, etc.")
    design_life_yrs: Optional[float] = None
    safety_class: Optional[str] = None
    location_class: Optional[str] = None
    solver_settings: Optional[SolverSettingsData] = None
    input_files: Optional[str] = None
