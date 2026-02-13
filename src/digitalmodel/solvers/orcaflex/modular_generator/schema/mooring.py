"""Mooring system models for catenary and taut mooring analysis."""

from __future__ import annotations

from enum import Enum
from typing import Literal

from pydantic import BaseModel, Field, field_validator, model_validator


class MooringSegmentType(str, Enum):
    """Mooring line segment material types."""

    CHAIN = "chain"
    WIRE_ROPE = "wire_rope"
    POLYESTER = "polyester"


class ChainGrade(str, Enum):
    """Chain grades per DNV-OS-E302 / API 2SK."""

    R3 = "R3"
    R3S = "R3S"
    R4 = "R4"
    R4S = "R4S"
    R5 = "R5"


class MooringSegment(BaseModel):
    """Mooring line segment definition.

    Attributes:
        type: Segment material type (chain, wire_rope, polyester).
        diameter: Nominal diameter (m).
        length: Segment length (m).
        grade: Chain grade (for chain segments only).
        breaking_load: Minimum breaking load (kN). For wire/polyester.
        axial_stiffness: Override axial stiffness EA (kN).
        segment_length: FE mesh target segment length (m). Default 5.0.
    """

    type: MooringSegmentType = Field(..., description="Segment material type")
    diameter: float = Field(..., gt=0, description="Nominal diameter (m)")
    length: float = Field(..., gt=0, description="Segment length (m)")
    grade: ChainGrade | None = Field(default=None, description="Chain grade (chain only)")
    breaking_load: float | None = Field(default=None, gt=0, description="MBL (kN)")
    axial_stiffness: float | None = Field(default=None, gt=0, description="EA override (kN)")
    segment_length: float = Field(default=5.0, gt=0, le=50, description="FE segment length (m)")

    @model_validator(mode="after")
    def validate_chain_grade(self) -> "MooringSegment":
        """Chain segments should have a grade."""
        if self.type == MooringSegmentType.CHAIN and self.grade is None:
            raise ValueError("Chain segments require 'grade' field")
        return self


class MooringEndpoint(BaseModel):
    """Mooring line endpoint (anchor or fairlead).

    Attributes:
        type: Endpoint type (anchor, fairlead, fixed).
        position: Position [x, y, z] (m). For anchors/fixed endpoints this is
            the global position. For fairlead endpoints connected to a vessel,
            this is the local offset relative to the vessel origin.
        vessel: Vessel name for fairlead connections.
    """

    type: Literal["anchor", "fairlead", "fixed"] = Field(..., description="Endpoint type")
    position: list[float] = Field(
        ..., min_length=3, max_length=3, description="Position [x, y, z] (m)"
    )
    vessel: str | None = Field(default=None, description="Vessel name (fairlead only)")

    @field_validator("position")
    @classmethod
    def validate_position(cls, v: list[float]) -> list[float]:
        if len(v) != 3:
            raise ValueError(f"Position must have 3 components, got {len(v)}")
        return v

    @model_validator(mode="after")
    def validate_fairlead_vessel(self) -> "MooringEndpoint":
        """Fairlead endpoints require a vessel name."""
        if self.type == "fairlead" and not self.vessel:
            raise ValueError("Fairlead endpoint requires 'vessel' field")
        return self


class MooringLine(BaseModel):
    """Complete mooring line definition.

    Attributes:
        name: Unique line name.
        segments: Ordered list of segments (anchor to fairlead).
        anchor: Anchor endpoint.
        fairlead: Fairlead endpoint.
        pretension: Target pretension (kN). Creates a winch if set.
        lay_azimuth: Line lay azimuth (deg).
    """

    name: str = Field(..., min_length=1, description="Line name")
    segments: list[MooringSegment] = Field(..., min_length=1, description="Segments (anchor to fairlead)")
    anchor: MooringEndpoint = Field(..., description="Anchor endpoint")
    fairlead: MooringEndpoint = Field(..., description="Fairlead endpoint")
    pretension: float | None = Field(default=None, gt=0, description="Target pretension (kN)")
    lay_azimuth: float | None = Field(default=None, ge=0, lt=360, description="Lay azimuth (deg)")

    @model_validator(mode="after")
    def validate_endpoints(self) -> "MooringLine":
        """Validate anchor is anchor type and fairlead is fairlead type."""
        if self.anchor.type != "anchor" and self.anchor.type != "fixed":
            raise ValueError(f"Anchor endpoint must be 'anchor' or 'fixed', got '{self.anchor.type}'")
        if self.fairlead.type != "fairlead":
            raise ValueError(f"Fairlead endpoint must be 'fairlead', got '{self.fairlead.type}'")
        return self

    def get_total_length(self) -> float:
        """Calculate total line length from segments."""
        return sum(s.length for s in self.segments)


class MooringSystem(BaseModel):
    """Complete mooring system specification.

    Attributes:
        lines: List of mooring lines.
    """

    lines: list[MooringLine] = Field(..., min_length=1, description="Mooring lines")

    @model_validator(mode="after")
    def validate_unique_names(self) -> "MooringSystem":
        """Validate all line names are unique."""
        names = [line.name for line in self.lines]
        if len(names) != len(set(names)):
            dupes = [n for n in names if names.count(n) > 1]
            raise ValueError(f"Duplicate line names: {set(dupes)}")
        return self
