from pydantic import BaseModel, Field
from typing import List, Optional


class SegmentData(BaseModel):
    """Information about a single mesh segment."""
    arc_length_m: float
    length_m: float


class MeshQualityData(BaseModel):
    """Quality metrics for the finite element mesh."""
    max_adjacent_ratio: float
    worst_ratio_arc_length_m: float
    verdict: str = Field(..., description="PASS, WARNING, or FAIL")
    adjacent_ratios: List[float] = Field(default_factory=list)


class MeshData(BaseModel):
    """Mesh discretization summary."""
    total_segment_count: int
    segments: List[SegmentData] = Field(default_factory=list)
    quality: Optional[MeshQualityData] = None
