from pydantic import BaseModel, Field
from typing import List, Optional
from datetime import datetime

from .geometry import GeometryData
from .materials import MaterialData
from .boundary_conditions import BCData
from .mesh import MeshData
from .other_structures import OtherStructuresData
from .loads import EnvironmentData
from .analysis import AnalysisSetupData
from .results import StaticResultsData, DynamicResultsData, ExtremeResultsData
from .design_checks import DesignCheckData
from .fatigue import FatigueResultsData


class OrcaFlexAnalysisReport(BaseModel):
    """Canonical data model for an OrcaFlex analysis report."""
    
    # Header metadata
    project_name: str
    structure_id: str
    structure_type: str = Field(..., description="pipeline, riser, jumper, mooring, or installation")
    analysis_ref: Optional[str] = None
    analyst: Optional[str] = None
    date: datetime = Field(default_factory=datetime.now)
    orcaflex_version: Optional[str] = None
    design_codes: List[str] = Field(default_factory=list)
    
    # Report Sections (FEA causal chain)
    geometry: Optional[GeometryData] = None
    materials: Optional[MaterialData] = None
    boundary_conditions: Optional[BCData] = None
    mesh: Optional[MeshData] = None
    other_structures: Optional[OtherStructuresData] = None
    loads: Optional[EnvironmentData] = None
    analysis_setup: Optional[AnalysisSetupData] = None
    
    # Results
    static_results: Optional[StaticResultsData] = None
    dynamic_results: Optional[DynamicResultsData] = None
    extreme_results: Optional[ExtremeResultsData] = None
    design_checks: Optional[DesignCheckData] = None
    fatigue: Optional[FatigueResultsData] = None
    
    # Summary
    recommendations: List[str] = Field(default_factory=list)
    summary_notes: Optional[str] = None

    @property
    def overall_pass(self) -> Optional[bool]:
        """Determines if the overall analysis passes based on design checks."""
        if self.design_checks:
            return self.design_checks.overall_pass
        return None
