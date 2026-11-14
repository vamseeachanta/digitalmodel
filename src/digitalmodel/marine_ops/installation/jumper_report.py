"""Adapt Python installation calculations to the existing reporting dispatch."""
from dataclasses import asdict, is_dataclass
from datetime import datetime
from enum import Enum
from pathlib import Path
from typing import TYPE_CHECKING

from digitalmodel.solvers.orcaflex.reporting import OrcaFlexAnalysisReport, generate_orcaflex_report

if TYPE_CHECKING:
    from .jumper_installation import PipelineOutput


def _normalize(value):
    if is_dataclass(value):
        value = asdict(value)
    if isinstance(value, Enum):
        return value.value
    if isinstance(value, dict):
        return {key: _normalize(item) for key, item in value.items()}
    if isinstance(value, (list, tuple)):
        return [_normalize(item) for item in value]
    return value


def build_installation_report(output: "PipelineOutput") -> OrcaFlexAnalysisReport:
    """Copy only the calculation contract; source narratives are not metadata."""
    results = output.jumper_analysis
    payload = {key: _normalize(results[key]) for key in (
        "config", "pipe_properties", "buoyancy_module", "strake_module", "pipe_geometry",
        "orcaflex_sections", "weight_check", "weight_uninsulated", "cog_uninsulated",
        "cog_insulated", "cranes",
    )}
    weight = payload["weight_check"]
    payload["weight_items"] = {
        **weight["kit_weights_kg"], "Buoyancy modules": weight["total_buoy_kg"],
        "Strakes": weight["total_strake_kg"], "Clamps": weight["total_clamp_kg"],
    }
    payload["criteria"] = _normalize(output.go_no_go.criteria) if output.go_no_go else []
    payload["decision"] = output.go_no_go.overall_state.value if output.go_no_go else "not evaluated"
    return OrcaFlexAnalysisReport(
        project_name="Jumper installation calculations", structure_id=output.config_name,
        structure_type="jumper", date=datetime.fromisoformat(output.timestamp),
        jumper_installation=payload,
    )


def write_installation_report(output: "PipelineOutput", output_dir: str) -> Path:
    """Write one offline HTML file; PDF is a separately scoped deliverable."""
    return generate_orcaflex_report(
        build_installation_report(output), Path(output_dir) / "jumper_installation.html",
        include_plotlyjs="inline",
    )
