"""
Results models for OrcaFlex simulation data.
"""

from datetime import datetime
from typing import Any, Dict, List, Optional
from uuid import UUID

from pydantic import BaseModel, Field


class TimeSeriesPoint(BaseModel):
    """Single time series data point."""
    
    time: float = Field(..., description="Time value")
    value: float = Field(..., description="Data value")
    
    class Config:
        schema_extra = {
            "example": {
                "time": 10.5,
                "value": 245.7
            }
        }


class TimeSeriesData(BaseModel):
    """Time series data container."""
    
    variable_name: str = Field(..., description="Variable name")
    unit: str = Field(..., description="Data unit")
    object_name: Optional[str] = Field(None, description="OrcaFlex object name")
    data_type: str = Field(..., description="Type of data (e.g., 'tension', 'position')")
    
    # Data points
    data: List[TimeSeriesPoint] = Field(..., description="Time series data points")
    
    # Metadata
    sample_rate: Optional[float] = Field(None, description="Sample rate in Hz")
    duration: Optional[float] = Field(None, description="Total duration")
    
    class Config:
        schema_extra = {
            "example": {
                "variable_name": "Top Tension",
                "unit": "kN",
                "object_name": "Mooring Line 1",
                "data_type": "tension",
                "data": [
                    {"time": 0.0, "value": 1000.0},
                    {"time": 0.1, "value": 1005.2}
                ],
                "sample_rate": 10.0,
                "duration": 100.0
            }
        }


class StatisticalSummary(BaseModel):
    """Statistical summary of time series data."""
    
    variable_name: str
    count: int
    mean: float
    std: float
    min: float
    max: float
    percentile_25: float
    percentile_50: float  # median
    percentile_75: float
    percentile_95: float
    percentile_99: float
    
    class Config:
        schema_extra = {
            "example": {
                "variable_name": "Top Tension",
                "count": 1000,
                "mean": 1250.5,
                "std": 45.2,
                "min": 1180.0,
                "max": 1420.0,
                "percentile_25": 1210.0,
                "percentile_50": 1250.0,
                "percentile_75": 1290.0,
                "percentile_95": 1350.0,
                "percentile_99": 1380.0
            }
        }


class FrequencyDomainData(BaseModel):
    """Frequency domain analysis results."""
    
    variable_name: str
    frequencies: List[float] = Field(..., description="Frequency values in Hz")
    magnitudes: List[float] = Field(..., description="Magnitude values")
    phases: Optional[List[float]] = Field(None, description="Phase values in radians")
    psd: Optional[List[float]] = Field(None, description="Power spectral density")
    
    class Config:
        schema_extra = {
            "example": {
                "variable_name": "Top Tension",
                "frequencies": [0.1, 0.2, 0.3],
                "magnitudes": [10.5, 8.2, 5.1],
                "phases": [0.0, 1.57, 3.14],
                "psd": [110.25, 67.24, 26.01]
            }
        }


class SimulationResult(BaseModel):
    """Complete simulation result container."""
    
    analysis_id: UUID
    result_id: UUID
    name: str = Field(..., description="Result name")
    description: Optional[str] = None
    
    # Result data
    time_series: List[TimeSeriesData] = Field(default_factory=list)
    statistics: List[StatisticalSummary] = Field(default_factory=list)
    frequency_domain: List[FrequencyDomainData] = Field(default_factory=list)
    
    # Metadata
    simulation_time: Optional[float] = Field(None, description="Total simulation time")
    time_step: Optional[float] = Field(None, description="Time step size")
    solver_info: Dict[str, Any] = Field(default_factory=dict)
    
    # File references
    source_files: List[str] = Field(default_factory=list)
    output_files: List[str] = Field(default_factory=list)
    
    # Timestamps
    created_at: datetime = Field(default_factory=datetime.utcnow)
    
    class Config:
        json_encoders = {
            datetime: lambda v: v.isoformat(),
            UUID: lambda v: str(v)
        }
        schema_extra = {
            "example": {
                "analysis_id": "123e4567-e89b-12d3-a456-426614174000",
                "result_id": "987fcdeb-51a2-43d1-b234-567890123456",
                "name": "Dynamic Analysis Results",
                "description": "Results from dynamic analysis of mooring system",
                "simulation_time": 1800.0,
                "time_step": 0.1,
                "solver_info": {
                    "solver": "OrcaFlex Dynamic",
                    "version": "11.3a",
                    "convergence_tolerance": 1e-6
                }
            }
        }


class ResultsQuery(BaseModel):
    """Query parameters for filtering results."""
    
    analysis_id: Optional[UUID] = None
    variable_names: Optional[List[str]] = None
    object_names: Optional[List[str]] = None
    data_types: Optional[List[str]] = None
    time_range: Optional[tuple[float, float]] = None
    include_statistics: bool = True
    include_frequency_domain: bool = False
    
    class Config:
        json_encoders = {UUID: lambda v: str(v)}


class ResultsMetadata(BaseModel):
    """Metadata about available results."""
    
    analysis_id: UUID
    total_variables: int
    available_variables: List[str]
    available_objects: List[str]
    data_types: List[str]
    time_range: tuple[float, float]
    sample_rate: float
    file_size: int
    
    class Config:
        json_encoders = {UUID: lambda v: str(v)}