"""Riser models for production and export risers."""

from __future__ import annotations

from enum import Enum
from typing import Literal

from pydantic import BaseModel, Field, field_validator, model_validator


class RiserConfiguration(str, Enum):
    """Riser configuration types."""

    CATENARY = "catenary"
    LAZY_WAVE = "lazy_wave"
    STEEP_WAVE = "steep_wave"
    PLIANT_WAVE = "pliant_wave"
    FREE_HANGING = "free_hanging"


class ConnectionType(str, Enum):
    """End connection types for risers."""

    VESSEL = "vessel"
    ANCHOR = "anchor"
    FIXED = "fixed"
    FREE = "free"
    BUOY = "buoy"


class RiserLineType(BaseModel):
    """
    Line type definition for riser segments.

    Attributes:
        name: Unique identifier for this line type.
        description: Human-readable description.
        outer_diameter: Outer diameter (m).
        inner_diameter: Inner diameter (m).
        mass_per_length: Mass per unit length (te/m).
        bending_stiffness: Bending stiffness EI (kN.m2).
        axial_stiffness: Axial stiffness EA (kN).
        torsional_stiffness: Torsional stiffness GJ (kN.m2). Default 10.
        poisson_ratio: Poisson's ratio. Default 0.5.
        allowable_tension: Maximum allowable tension (kN). Optional.
        min_bend_radius: Minimum bend radius (m). Optional.
        drag_coefficient: Drag coefficient or table name. Default 1.2.
        added_mass_coefficient: Added mass coefficient Ca. Default 1.0.
        contact_diameter: Contact diameter for clash detection (m). Optional.
    """

    name: str = Field(..., min_length=1, description="Line type identifier")
    description: str | None = Field(default=None, description="Description")

    # Geometry
    outer_diameter: float = Field(..., gt=0, description="OD (m)")
    inner_diameter: float = Field(..., ge=0, description="ID (m)")
    contact_diameter: float | None = Field(default=None, gt=0, description="Contact OD (m)")

    # Mass
    mass_per_length: float = Field(..., ge=0, description="Mass per length (te/m)")

    # Structural
    bending_stiffness: float = Field(..., ge=0, description="EI (kN.m2)")
    axial_stiffness: float = Field(..., gt=0, description="EA (kN)")
    torsional_stiffness: float = Field(default=10, ge=0, description="GJ (kN.m2)")
    poisson_ratio: float = Field(default=0.5, ge=0, le=0.5, description="Poisson's ratio")

    # Limits
    allowable_tension: float | None = Field(default=None, gt=0, description="Max tension (kN)")
    min_bend_radius: float | None = Field(default=None, gt=0, description="Min bend radius (m)")

    # Hydrodynamic
    drag_coefficient: float | str = Field(default=1.2, description="Cd or table name")
    added_mass_coefficient: float = Field(default=1.0, ge=0, description="Ca")
    axial_drag_coefficient: float = Field(default=0.008, ge=0, description="Axial Cd")

    # Stress calculation diameters (if different from geometry)
    stress_od: float | None = Field(default=None, gt=0, description="Stress calc OD (m)")
    stress_id: float | None = Field(default=None, ge=0, description="Stress calc ID (m)")

    @model_validator(mode="after")
    def validate_diameters(self) -> "RiserLineType":
        """Validate ID < OD."""
        if self.inner_diameter >= self.outer_diameter:
            raise ValueError(
                f"inner_diameter ({self.inner_diameter}) must be less than "
                f"outer_diameter ({self.outer_diameter})"
            )
        return self


class RiserSection(BaseModel):
    """
    Section definition for riser discretization.

    Attributes:
        line_type: Name of the line type for this section.
        length: Section length (m).
        segment_length: Target FE segment length (m).
    """

    line_type: str = Field(..., description="Line type name reference")
    length: float = Field(..., gt=0, description="Section length (m)")
    segment_length: float = Field(..., gt=0, le=50, description="Target segment length (m)")


class EndConnection(BaseModel):
    """
    End connection specification for a riser.

    Attributes:
        type: Connection type (vessel, anchor, fixed, buoy).
        name: Connected object name (vessel/buoy name). Required for vessel/buoy.
        position: Connection position [x, y, z] (m).
        azimuth: Azimuth angle (deg). Default 0.
        declination: Declination angle (deg). Default 90 (vertical).
        gamma: Gamma rotation angle (deg). Default 0.
        bending_stiffness: End bending stiffness (kN.m/rad). Default 0 (pinned).
    """

    type: ConnectionType = Field(..., description="Connection type")
    name: str | None = Field(default=None, description="Connected object name")
    position: list[float] = Field(
        ..., min_length=3, max_length=3, description="Position [x, y, z] (m)"
    )
    azimuth: float = Field(default=0, ge=0, lt=360, description="Azimuth (deg)")
    declination: float = Field(default=90, ge=0, le=180, description="Declination (deg)")
    gamma: float = Field(default=0, description="Gamma rotation (deg)")
    bending_stiffness: float = Field(default=0, ge=0, description="End stiffness (kN.m/rad)")

    @field_validator("position")
    @classmethod
    def validate_position(cls, v: list[float]) -> list[float]:
        if len(v) != 3:
            raise ValueError(f"Position must have 3 components, got {len(v)}")
        return v

    @model_validator(mode="after")
    def validate_name_for_type(self) -> "EndConnection":
        """Validate name is provided for vessel/buoy connections."""
        if self.type in (ConnectionType.VESSEL, ConnectionType.BUOY) and not self.name:
            raise ValueError(f"Connection type '{self.type.value}' requires 'name' field")
        return self


class RiserContents(BaseModel):
    """
    Riser contents specification.

    Attributes:
        density: Contents density (te/m3). Default 1.0 (seawater).
        pressure: Internal pressure (kPa). Default 0.
        flow_rate: Contents flow rate (m/s). Default 0.
    """

    density: float = Field(default=1.0, ge=0, description="Contents density (te/m3)")
    pressure: float = Field(default=0, ge=0, description="Internal pressure (kPa)")
    flow_rate: float = Field(default=0, ge=0, description="Flow rate (m/s)")


class BuoyancyZone(BaseModel):
    """
    Distributed buoyancy zone definition.

    Attributes:
        start_arc_length: Start position from End A (m).
        end_arc_length: End position from End A (m).
        line_type: Line type with buoyancy properties.
    """

    start_arc_length: float = Field(..., ge=0, description="Zone start (m)")
    end_arc_length: float = Field(..., ge=0, description="Zone end (m)")
    line_type: str = Field(..., description="Buoyant line type name")

    @model_validator(mode="after")
    def validate_range(self) -> "BuoyancyZone":
        if self.start_arc_length >= self.end_arc_length:
            raise ValueError(
                f"start_arc_length ({self.start_arc_length}) must be < "
                f"end_arc_length ({self.end_arc_length})"
            )
        return self


class ClumpType(BaseModel):
    """
    Discrete clump/buoyancy module type definition.

    Attributes:
        name: Clump type identifier.
        mass: Clump mass (te).
        volume: Displaced volume (m3).
        height: Clump height (m). Default 1.0.
        drag_area: Drag area [normal, axial] (m2). Optional.
        drag_coefficient: Drag coefficients [Cd_normal, Cd_axial]. Default [0.6, 1.0].
        added_mass_coefficient: Added mass [Ca_normal, Ca_axial]. Default [1.0, 0.5].
    """

    name: str = Field(..., min_length=1, description="Clump type name")
    mass: float = Field(..., ge=0, description="Mass (te)")
    volume: float = Field(..., gt=0, description="Displaced volume (m3)")
    height: float = Field(default=1.0, gt=0, description="Height (m)")
    drag_area: list[float] | None = Field(
        default=None, min_length=2, max_length=2, description="Drag area [normal, axial] (m2)"
    )
    drag_coefficient: list[float] = Field(
        default=[0.6, 1.0], min_length=2, max_length=2, description="Cd [normal, axial]"
    )
    added_mass_coefficient: list[float] = Field(
        default=[1.0, 0.5], min_length=2, max_length=2, description="Ca [normal, axial]"
    )


class ClumpAttachments(BaseModel):
    """
    Discrete clump attachments along the riser.

    Attributes:
        clump_type: Name of ClumpType to attach.
        start_arc_length: First attachment position (m from End A).
        end_arc_length: Last attachment position (m from End A).
        spacing: Spacing between attachments (m).
    """

    clump_type: str = Field(..., description="ClumpType name reference")
    start_arc_length: float = Field(..., ge=0, description="First clump position (m)")
    end_arc_length: float = Field(..., ge=0, description="Last clump position (m)")
    spacing: float = Field(..., gt=0, description="Clump spacing (m)")

    @model_validator(mode="after")
    def validate_range(self) -> "ClumpAttachments":
        if self.start_arc_length > self.end_arc_length:
            raise ValueError(
                f"start_arc_length ({self.start_arc_length}) must be <= "
                f"end_arc_length ({self.end_arc_length})"
            )
        return self


class LinkType(str, Enum):
    """OrcaFlex Link types."""

    TETHER = "tether"
    SPRING_DAMPER = "spring_damper"


class LinkConnection(BaseModel):
    """Connection point for an OrcaFlex Link.

    Each Link has exactly 2 connections. A connection references either
    an OrcaFlex object (vessel, line, 6D buoy) or "Anchored" for a
    fixed seabed point.

    Attributes:
        object_name: Name of connected object, or "Anchored" for seabed.
        x: X coordinate at connection (m).
        y: Y coordinate at connection (m).
        z: Z coordinate at connection (m or arc length).
        z_relative_to: Reference for z (e.g. "End A"). Required for lines.
    """

    object_name: str = Field(..., min_length=1, description="Connected object or 'Anchored'")
    x: float = Field(..., description="X position (m)")
    y: float = Field(..., description="Y position (m)")
    z: float = Field(..., description="Z position (m or arc length)")
    z_relative_to: str | None = Field(default=None, description="Z reference (e.g. 'End A')")


class RiserLink(BaseModel):
    """OrcaFlex Link definition (tether or spring/damper).

    Links connect two points in the model with a specified stiffness
    and unstretched length. Tethers resist tension only; spring/dampers
    can resist both tension and compression.

    Attributes:
        name: Unique link name for OrcaFlex.
        link_type: Type of link (tether or spring_damper).
        connections: Exactly 2 connection points.
        unstretched_length: Unstretched length (m).
        stiffness: Axial stiffness (kN).
        release_stage: Optional release stage number.
    """

    name: str = Field(..., min_length=1, description="Link name")
    link_type: LinkType = Field(default=LinkType.TETHER, description="Link type")
    connections: list[LinkConnection] = Field(
        ..., min_length=2, max_length=2, description="Exactly 2 connection points"
    )
    unstretched_length: float = Field(..., gt=0, description="Unstretched length (m)")
    stiffness: float = Field(..., gt=0, description="Axial stiffness (kN)")
    release_stage: int | None = Field(default=None, ge=1, description="Release stage")


class RiserVessel(BaseModel):
    """
    Vessel definition for riser top connection.

    Attributes:
        name: Vessel name for OrcaFlex.
        type_name: Vessel type reference.
        position: Initial position [x, y, z] (m).
        orientation: Initial orientation [heading, pitch, roll] (deg).
        length: Optional vessel length (m).
        primary_motion: Primary motion mode. Default 'none'.
        superimposed_motion: Superimposed motion. Default 'raos_harmonics'.
    """

    name: str = Field(..., min_length=1, description="Vessel name")
    type_name: str = Field(default="Vessel Type1", description="Vessel type reference")
    position: list[float] = Field(
        default=[0, 0, 0], min_length=3, max_length=3, description="Position [x, y, z] (m)"
    )
    orientation: list[float] = Field(
        default=[0, 0, 0], min_length=3, max_length=3, description="Orientation [rx, ry, rz] (deg)"
    )
    length: float | None = Field(default=None, gt=0, description="Vessel length (m)")
    primary_motion: str = Field(default="None", description="Primary motion mode")
    superimposed_motion: str = Field(
        default="RAOs + harmonics", description="Superimposed motion mode"
    )
    included_in_statics: str = Field(default="None", description="Statics inclusion")

    @field_validator("position", "orientation")
    @classmethod
    def validate_vector3(cls, v: list[float]) -> list[float]:
        if len(v) != 3:
            raise ValueError(f"Must have 3 components, got {len(v)}")
        return v


class RiserLine(BaseModel):
    """
    Complete riser line definition.

    Attributes:
        name: Riser line name for OrcaFlex.
        configuration: Riser configuration type.
        end_a: Top end connection (typically vessel).
        end_b: Bottom end connection (typically anchor/seabed).
        sections: Ordered list of line sections.
        contents: Optional contents specification.
        buoyancy_zone: Optional distributed buoyancy zone.
        clump_attachments: Optional discrete clump attachments.
        lay_azimuth: Lay azimuth for statics (deg). Default 180.
        statics_method: Statics initialization method. Default 'Catenary'.
    """

    name: str = Field(..., min_length=1, description="Line name")
    configuration: RiserConfiguration = Field(
        default=RiserConfiguration.CATENARY, description="Riser configuration"
    )
    end_a: EndConnection = Field(..., description="Top end connection")
    end_b: EndConnection = Field(..., description="Bottom end connection")
    sections: list[RiserSection] = Field(..., min_length=1, description="Line sections")
    contents: RiserContents = Field(default_factory=RiserContents, description="Contents")
    buoyancy_zone: BuoyancyZone | None = Field(default=None, description="Buoyancy zone")
    clump_attachments: ClumpAttachments | None = Field(default=None, description="Clump attachments")
    lay_azimuth: float = Field(default=180, ge=0, lt=360, description="Lay azimuth (deg)")
    statics_method: str = Field(default="Catenary", description="Statics step 1 method")
    statics_step2: str | None = Field(default=None, description="Statics step 2 method (e.g. 'Full statics')")
    include_torsion: bool = Field(default=False, description="Include torsion in analysis")

    def get_total_length(self) -> float:
        """Calculate total line length from sections."""
        return sum(s.length for s in self.sections)


class Riser(BaseModel):
    """
    Complete riser system specification.

    Attributes:
        vessel: Vessel definition for top connection.
        line_types: Line type definitions for riser segments.
        clump_types: Optional clump type definitions.
        lines: Riser line definitions.
    """

    vessel: RiserVessel = Field(..., description="Vessel for top connection")
    line_types: list[RiserLineType] = Field(..., min_length=1, description="Line types")
    clump_types: list[ClumpType] = Field(default_factory=list, description="Clump types")
    links: list[RiserLink] = Field(default_factory=list, description="Link/tether definitions")
    lines: list[RiserLine] = Field(..., min_length=1, description="Riser lines")

    @model_validator(mode="after")
    def validate_line_type_references(self) -> "Riser":
        """Validate all line type references exist."""
        type_names = {lt.name for lt in self.line_types}
        for line in self.lines:
            for section in line.sections:
                if section.line_type not in type_names:
                    raise ValueError(
                        f"Line '{line.name}' references unknown line_type "
                        f"'{section.line_type}'. Available: {type_names}"
                    )
        return self

    @model_validator(mode="after")
    def validate_clump_type_references(self) -> "Riser":
        """Validate all clump type references exist."""
        clump_names = {ct.name for ct in self.clump_types}
        for line in self.lines:
            if line.clump_attachments:
                if line.clump_attachments.clump_type not in clump_names:
                    raise ValueError(
                        f"Line '{line.name}' references unknown clump_type "
                        f"'{line.clump_attachments.clump_type}'. Available: {clump_names}"
                    )
        return self

    @model_validator(mode="after")
    def validate_vessel_references(self) -> "Riser":
        """Validate vessel connections reference the defined vessel."""
        vessel_name = self.vessel.name
        for line in self.lines:
            if line.end_a.type == ConnectionType.VESSEL:
                if line.end_a.name != vessel_name:
                    raise ValueError(
                        f"Line '{line.name}' end_a references vessel '{line.end_a.name}' "
                        f"but defined vessel is '{vessel_name}'"
                    )
            if line.end_b.type == ConnectionType.VESSEL:
                if line.end_b.name != vessel_name:
                    raise ValueError(
                        f"Line '{line.name}' end_b references vessel '{line.end_b.name}' "
                        f"but defined vessel is '{vessel_name}'"
                    )
        return self

    @model_validator(mode="after")
    def validate_link_references(self) -> "Riser":
        """Validate link connection object names reference existing entities."""
        valid_names = {"Anchored", self.vessel.name}
        valid_names.update(line.name for line in self.lines)
        for link in self.links:
            for conn in link.connections:
                if conn.object_name not in valid_names:
                    raise ValueError(
                        f"Link '{link.name}' references unknown object "
                        f"'{conn.object_name}'. Available: {valid_names}"
                    )
        return self
