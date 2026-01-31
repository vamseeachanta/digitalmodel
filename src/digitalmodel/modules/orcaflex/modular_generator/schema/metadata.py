"""Metadata models for OrcaFlex project identification."""

from __future__ import annotations

from pydantic import BaseModel, Field


class Metadata(BaseModel):
    """
    Model metadata for identification and categorization.

    Attributes:
        name: Unique model identifier (e.g., '30in_pipeline_installation').
        description: Human-readable description of the model.
        structure: Type of offshore structure being modeled.
        operation: Operation type (e.g., 'installation/floating', 'in-place').
        project: Project code or name for traceability.
        version: Optional schema version for future compatibility.
        author: Optional author name or team.
    """

    name: str = Field(..., min_length=1, description="Unique model identifier")
    description: str = Field(..., description="Human-readable model description")
    structure: str = Field(..., description="Structure type (pipeline, riser, mooring, etc.)")
    operation: str = Field(..., description="Operation type (installation/floating, in-place, etc.)")
    project: str = Field(..., description="Project code for traceability")
    version: str = Field(default="1.0", description="Schema version")
    author: str | None = Field(default=None, description="Author name or team")
