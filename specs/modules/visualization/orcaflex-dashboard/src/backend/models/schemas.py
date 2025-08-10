"""
Pydantic schemas for API request/response validation
"""

from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional

from pydantic import BaseModel, Field


class ComponentType(str, Enum):
    """Component type enumeration"""
    FST1 = "fst1"
    FST2 = "fst2"
    STRUT = "strut"
    JACKET = "jacket"
    LNGC = "lngc"


class LoadingCondition(str, Enum):
    """Loading condition enumeration"""
    HWL_125KM3_PB = "hwl_125km3_pb"
    HWL_125KM3_SB = "hwl_125km3_sb"
    HWL_180KM3_PB = "hwl_180km3_pb"
    HWL_180KM3_SB = "hwl_180km3_sb"
    LWL_125KM3_PB = "lwl_125km3_pb"
    LWL_125KM3_SB = "lwl_125km3_sb"
    LWL_180KM3_PB = "lwl_180km3_pb"
    LWL_180KM3_SB = "lwl_180km3_sb"


class DataPoint(BaseModel):
    """Single data point"""
    timestamp: Optional[datetime] = None
    heading: Optional[float] = Field(None, ge=0, le=360)
    value: float
    unit: str = ""
    metadata: Dict[str, Any] = Field(default_factory=dict)


class PolarData(BaseModel):
    """Polar plot data structure"""
    case: str
    component: str
    loading_condition: str
    headings: List[float] = Field(..., min_items=24, max_items=24)
    values: List[float] = Field(..., min_items=24, max_items=24)
    unit: str
    max_value: float
    min_value: float
    mean_value: float
    std_dev: float


class TimeTraceData(BaseModel):
    """Time trace data structure"""
    case: str
    component: str
    heading: float
    timestamps: List[float]
    values: List[float]
    unit: str
    statistics: Dict[str, float] = Field(default_factory=dict)


class DataQuery(BaseModel):
    """Data query parameters"""
    case: str
    component: Optional[str] = None
    loading_condition: Optional[str] = None
    heading: Optional[float] = Field(None, ge=0, le=360)
    start_time: Optional[datetime] = None
    end_time: Optional[datetime] = None
    include_statistics: bool = True


class DataResponse(BaseModel):
    """Generic data response"""
    success: bool
    data: Any
    message: str = ""
    errors: List[str] = Field(default_factory=list)


class AnalysisRequest(BaseModel):
    """Analysis request parameters"""
    cases: List[str]
    components: List[str]
    analysis_type: str
    parameters: Dict[str, Any] = Field(default_factory=dict)


class AnalysisResult(BaseModel):
    """Analysis result"""
    analysis_type: str
    timestamp: datetime = Field(default_factory=datetime.now)
    results: Dict[str, Any]
    statistics: Dict[str, float]
    plots: List[Dict[str, Any]] = Field(default_factory=list)


class ComparisonRequest(BaseModel):
    """Case comparison request"""
    baseline_case: str
    comparison_cases: List[str]
    components: List[str]
    metrics: List[str] = Field(default=["max", "mean", "std"])


class ComparisonResult(BaseModel):
    """Case comparison result"""
    baseline: str
    comparisons: Dict[str, Dict[str, Any]]
    differences: Dict[str, Dict[str, float]]
    summary: str


class ExportRequest(BaseModel):
    """Export request parameters"""
    data_type: str
    query: DataQuery
    include_metadata: bool = True
    include_statistics: bool = True


class ExportResponse(BaseModel):
    """Export response"""
    success: bool
    file_path: str
    format: str
    size_bytes: Optional[int] = None
    message: str = ""