"""OrcaWave HTML report configuration schema."""
from __future__ import annotations

from typing import Optional

from pydantic import BaseModel, Field


class SectionConfig(BaseModel):
    enabled: bool = True


class ModelSummaryConfig(SectionConfig):
    pass


class RAOPlotsConfig(SectionConfig):
    headings: Optional[list[float]] = None  # None = all headings
    dofs: list[str] = Field(
        default_factory=lambda: ["surge", "sway", "heave", "roll", "pitch", "yaw"]
    )
    include_phase: bool = False


class HydroMatricesConfig(SectionConfig):
    frequencies_to_show: Optional[list[float]] = None  # None = all


class MeanDriftConfig(SectionConfig):
    include_polar: bool = True


class PanelPressuresConfig(SectionConfig):
    pass


class MultiBodyConfig(SectionConfig):
    pass


class QTFHeatmapConfig(SectionConfig):
    max_delta_omegas: int = 3


class QASummaryConfig(SectionConfig):
    pass


class ReportConfig(BaseModel):
    title: str = "OrcaWave Analysis Report"
    include_plotlyjs: str = "cdn"
    model_summary: ModelSummaryConfig = Field(default_factory=ModelSummaryConfig)
    rao_plots: RAOPlotsConfig = Field(default_factory=RAOPlotsConfig)
    hydro_matrices: HydroMatricesConfig = Field(default_factory=HydroMatricesConfig)
    mean_drift: MeanDriftConfig = Field(default_factory=MeanDriftConfig)
    panel_pressures: PanelPressuresConfig = Field(default_factory=PanelPressuresConfig)
    multi_body: MultiBodyConfig = Field(default_factory=MultiBodyConfig)
    qtf_heatmap: QTFHeatmapConfig = Field(default_factory=QTFHeatmapConfig)
    qa_summary: QASummaryConfig = Field(default_factory=QASummaryConfig)

    @classmethod
    def from_yaml(cls, path: str) -> "ReportConfig":
        import yaml

        with open(path) as f:
            data = yaml.safe_load(f)
        return cls(**(data or {}))
