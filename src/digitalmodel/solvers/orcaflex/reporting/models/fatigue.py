from pydantic import BaseModel, Field
from typing import List, Optional


class FatigueResultsData(BaseModel):
    """Fatigue analysis summary."""
    method: str  # e.g., "Time-domain rainflow"
    sn_curve: str
    scf: float = 1.0
    design_life_yrs: float
    damage_per_node: Optional[dict] = Field(None, description="arc_length and damage_yr arrays")
    life_per_node_yrs: Optional[dict] = Field(None, description="arc_length and life_yrs arrays")
    rainflow_matrix: Optional[dict] = None  # stress_range, mean_stress, counts
    max_damage: Optional[float] = None
    max_damage_location_arc_m: Optional[float] = None
