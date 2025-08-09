"""
Analysis models for OrcaFlex simulation processing.
"""

from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional
from uuid import UUID, uuid4

from pydantic import BaseModel, Field


class AnalysisStatus(str, Enum):
    """Analysis status enumeration."""
    PENDING = "pending"
    RUNNING = "running"
    COMPLETED = "completed"
    FAILED = "failed"
    CANCELLED = "cancelled"


class AnalysisType(str, Enum):
    """Analysis type enumeration."""
    STATIC = "static"
    DYNAMIC = "dynamic"
    FATIGUE = "fatigue"
    FREQUENCY_DOMAIN = "frequency_domain"
    MODAL = "modal"


class Analysis(BaseModel):
    """Analysis configuration and metadata."""
    
    id: UUID = Field(default_factory=uuid4)
    name: str = Field(..., description="Analysis name")
    description: Optional[str] = Field(None, description="Analysis description")
    analysis_type: AnalysisType = Field(..., description="Type of analysis")
    
    # File references
    orcaflex_file: str = Field(..., description="Path to OrcaFlex simulation file")
    configuration: Dict[str, Any] = Field(default_factory=dict, description="Analysis configuration")
    
    # Status and timing
    status: AnalysisStatus = Field(default=AnalysisStatus.PENDING)
    created_at: datetime = Field(default_factory=datetime.utcnow)
    started_at: Optional[datetime] = None
    completed_at: Optional[datetime] = None
    
    # Metadata
    user_id: Optional[str] = None
    tags: List[str] = Field(default_factory=list)
    
    class Config:
        use_enum_values = True
        json_encoders = {
            datetime: lambda v: v.isoformat(),
            UUID: lambda v: str(v)
        }


class AnalysisRequest(BaseModel):
    """Request model for creating new analysis."""
    
    name: str = Field(..., min_length=1, max_length=200)
    description: Optional[str] = Field(None, max_length=1000)
    analysis_type: AnalysisType
    orcaflex_file: str = Field(..., description="Path to uploaded OrcaFlex file")
    configuration: Dict[str, Any] = Field(default_factory=dict)
    tags: List[str] = Field(default_factory=list)


class AnalysisResult(BaseModel):
    """Analysis result data."""
    
    analysis_id: UUID
    status: AnalysisStatus
    
    # Result data
    results: Dict[str, Any] = Field(default_factory=dict)
    metrics: Dict[str, float] = Field(default_factory=dict)
    
    # Output files
    output_files: List[str] = Field(default_factory=list)
    visualization_data: Optional[Dict[str, Any]] = None
    
    # Error information
    error_message: Optional[str] = None
    error_details: Optional[Dict[str, Any]] = None
    
    # Performance metrics
    execution_time: Optional[float] = None
    memory_usage: Optional[float] = None
    
    class Config:
        use_enum_values = True
        json_encoders = {
            UUID: lambda v: str(v)
        }


class AnalysisProgress(BaseModel):
    """Analysis progress information."""
    
    analysis_id: UUID
    progress: float = Field(..., ge=0, le=100)
    current_step: str
    total_steps: int
    completed_steps: int
    estimated_completion: Optional[datetime] = None
    
    class Config:
        json_encoders = {
            datetime: lambda v: v.isoformat(),
            UUID: lambda v: str(v)
        }


class AnalysisSummary(BaseModel):
    """Summary of analysis for listing views."""
    
    id: UUID
    name: str
    analysis_type: AnalysisType
    status: AnalysisStatus
    created_at: datetime
    completed_at: Optional[datetime]
    execution_time: Optional[float]
    has_results: bool = False
    
    class Config:
        use_enum_values = True
        json_encoders = {
            datetime: lambda v: v.isoformat(),
            UUID: lambda v: str(v)
        }