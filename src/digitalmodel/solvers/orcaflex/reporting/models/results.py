from pydantic import BaseModel, Field
from typing import List, Optional, Dict


class TimeSeriesData(BaseModel):
    """Time-history of a result variable at a specific location."""
    id: str
    label: str
    t: List[float] = Field(..., description="Time [s]")
    values: List[float] = Field(..., description="Variable values")
    units: str
    arc_length_m: Optional[float] = None


class EnvelopeData(BaseModel):
    """Maximum and minimum envelopes along the structure length."""
    id: str
    label: str
    arc_length: List[float]
    max_values: List[float]
    min_values: List[float]
    units: str


class StaticResultsData(BaseModel):
    """Summary of the static equilibrium state."""
    end_tensions_kn: Dict[str, float] = Field(default_factory=dict)
    tdp_position_m: Optional[float] = None
    minimum_clearance_m: Optional[float] = None
    tension_profile: Optional[dict] = None  # arc_length and tension arrays
    bm_profile: Optional[dict] = None  # arc_length and BM arrays
    per_line_tensions: Optional[dict] = None  # For mooring


class DynamicResultsData(BaseModel):
    """Summary of time-domain dynamic analysis results."""
    ramp_end_time_s: float
    time_series: List[TimeSeriesData] = Field(default_factory=list)
    envelopes: List[EnvelopeData] = Field(default_factory=list)
    statistical_summary: List[dict] = Field(default_factory=list)
    tdp_excursion_history: Optional[TimeSeriesData] = None


class ExtremeResultsData(BaseModel):
    """Most Probable Maximum (MPM) and extreme value summary."""
    mpm_values: List[dict] = Field(default_factory=list)
    governing_load_case: Optional[str] = None
    governing_location_arc_m: Optional[float] = None
