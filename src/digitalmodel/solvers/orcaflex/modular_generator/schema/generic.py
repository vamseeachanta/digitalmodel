"""Generic OrcaFlex model schema with typed core fields + properties pass-through.

Covers all OrcaFlex object types (28+ found across 62 example models).
Each object has typed core fields for key properties and a ``properties``
bag that passes through all remaining OrcaFlex properties verbatim.
This guarantees round-trip fidelity between monolithic and modular models.
"""

from __future__ import annotations

from typing import Any

from pydantic import BaseModel, Field


class GenericObject(BaseModel):
    """Base for all generic OrcaFlex objects.

    Attributes:
        name: Object name (unique within its type).
        properties: Pass-through dict of all OrcaFlex properties not
            captured by typed fields. Builder merges these verbatim
            into the output YAML.
    """

    name: str = Field(..., min_length=1, description="Object name")
    properties: dict[str, Any] = Field(
        default_factory=dict,
        description="Pass-through OrcaFlex properties",
    )


class GenericLineType(GenericObject):
    """OrcaFlex LineType definition.

    Attributes:
        category: LineType category (General, Homogeneous Pipe, etc.).
        outer_diameter: Outer diameter (m).
        inner_diameter: Inner diameter (m).
        mass_per_length: Mass per unit length (te/m).
        bending_stiffness: Bending stiffness EI (kN.m2).
        axial_stiffness: Axial stiffness EA (kN).
    """

    category: str | None = Field(default=None, description="LineType category")
    outer_diameter: float | str | None = Field(default=None, description="OD (m) or data name ref")
    inner_diameter: float | str | None = Field(default=None, description="ID (m) or data name ref")
    mass_per_length: float | str | None = Field(
        default=None, description="Mass per length (te/m) or data name ref"
    )
    bending_stiffness: float | str | list | None = Field(
        default=None, description="EI (kN.m2) or data name ref"
    )
    axial_stiffness: float | str | None = Field(default=None, description="EA (kN) or data name ref")


class GenericVesselType(GenericObject):
    """OrcaFlex VesselType definition.

    Attributes:
        length: Vessel length (m).
    """

    length: float | None = Field(default=None, description="Vessel length (m)")


class GenericVessel(GenericObject):
    """OrcaFlex Vessel instance.

    Attributes:
        vessel_type: Reference to VesselType name.
        connection: Connection string.
        initial_position: Initial position [x, y, z] (m).
    """

    vessel_type: str | None = Field(default=None, description="VesselType reference")
    connection: str | None = Field(default=None, description="Connection")
    initial_position: list[float] | None = Field(
        default=None, description="Initial position [x, y, z] (m)"
    )


class GenericLine(GenericObject):
    """OrcaFlex Line instance.

    Lines are complex objects with sections, connections, attachments.
    Most properties pass through via the ``properties`` bag.

    Attributes:
        line_type_refs: Informational list of line type names used
            (extracted from sections). Not used by builder directly.
    """

    line_type_refs: list[str] = Field(
        default_factory=list,
        description="Line type names used (informational)",
    )


class GenericBuoy6D(GenericObject):
    """OrcaFlex 6D Buoy instance.

    Attributes:
        buoy_type: Buoy type reference.
        connection: Connection string.
        initial_position: Initial position [x, y, z] (m).
    """

    buoy_type: str | None = Field(default=None, description="Buoy type reference")
    connection: str | None = Field(default=None, description="Connection")
    initial_position: list[float] | None = Field(
        default=None, description="Initial position [x, y, z] (m)"
    )


class GenericBuoy3D(GenericObject):
    """OrcaFlex 3D Buoy instance.

    Attributes:
        connection: Connection string.
        initial_position: Initial position [x, y, z] (m).
    """

    connection: str | None = Field(default=None, description="Connection")
    initial_position: list[float] | None = Field(
        default=None, description="Initial position [x, y, z] (m)"
    )


class GenericShape(GenericObject):
    """OrcaFlex Shape instance.

    Attributes:
        shape_type: Shape type (Drawing, Elastic solid, etc.).
        shape: Shape geometry reference.
        connection: Connection string.
        origin: Shape origin [x, y, z] (m).
    """

    shape_type: str | None = Field(default=None, description="Shape type")
    shape: str | None = Field(default=None, description="Shape geometry")
    connection: str | None = Field(default=None, description="Connection")
    origin: list[float] | None = Field(
        default=None, description="Shape origin [x, y, z] (m)"
    )


class GenericConstraint(GenericObject):
    """OrcaFlex Constraint instance.

    Attributes:
        in_frame_connection: In-frame connection string.
        constraint_type: Constraint type (Calculated, Fixed, etc.).
    """

    in_frame_connection: str | None = Field(
        default=None, description="In-frame connection"
    )
    constraint_type: str | None = Field(
        default=None, description="Constraint type"
    )


class GenericLink(GenericObject):
    """OrcaFlex Link instance.

    Attributes:
        link_type: Link type (Tether, Spring/damper, etc.).
    """

    link_type: str | None = Field(default=None, description="Link type")


class GenericWinch(GenericObject):
    """OrcaFlex Winch instance."""


class GenericClumpType(GenericObject):
    """OrcaFlex ClumpType definition.

    Attributes:
        mass: Clump mass (te).
        volume: Displaced volume (m3).
    """

    mass: float | None = Field(default=None, description="Mass (te)")
    volume: float | None = Field(default=None, description="Displaced volume (m3)")


class GenericWingType(GenericObject):
    """OrcaFlex WingType definition."""


class GenericFlexJointType(GenericObject):
    """OrcaFlex FlexJointType definition."""


class GenericFlexJoint(GenericObject):
    """OrcaFlex FlexJoint instance."""


class GenericDragChainType(GenericObject):
    """OrcaFlex DragChainType definition."""


class GenericDragChain(GenericObject):
    """OrcaFlex DragChain instance."""


class GenericStiffenerType(GenericObject):
    """OrcaFlex StiffenerType definition."""


class GenericSupportType(GenericObject):
    """OrcaFlex SupportType definition."""


class GenericMorisonElementType(GenericObject):
    """OrcaFlex MorisonElementType definition."""


class GenericTurbine(GenericObject):
    """OrcaFlex Turbine instance.

    Attributes:
        connection: Connection string.
    """

    connection: str | None = Field(default=None, description="Connection")


class GenericAttachedBuoy(GenericObject):
    """OrcaFlex AttachedBuoy instance."""


class GenericVariableData(GenericObject):
    """OrcaFlex VariableDataSource.

    Attributes:
        data_type: Variable data type name.
        entries: Data entries (format depends on data type).
    """

    data_type: str | None = Field(default=None, description="Variable data type")
    entries: list[Any] | None = Field(default=None, description="Data entries")


class GenericFrictionCoefficients(BaseModel):
    """OrcaFlex SolidFrictionCoefficients section.

    This is a single-instance object, not a list. All properties
    pass through the ``data`` dict.

    Attributes:
        data: Pass-through dict of all friction coefficient properties.
    """

    data: dict[str, Any] = Field(
        default_factory=dict,
        description="Pass-through friction properties",
    )


class GenericSingletonSection(BaseModel):
    """Pass-through for singleton OrcaFlex sections.

    Used for: LineContactData, CodeChecks, Shear7Data, VIVAData.

    Attributes:
        data: Pass-through dict of all section properties.
    """

    data: dict[str, Any] = Field(
        default_factory=dict,
        description="Pass-through section properties",
    )


# ---------------------------------------------------------------------------
# Section name -> (model class, is_list) mapping for builder/extractor
# ---------------------------------------------------------------------------

SECTION_REGISTRY: dict[str, tuple[type, bool]] = {
    "LineTypes": (GenericLineType, True),
    "VesselTypes": (GenericVesselType, True),
    "Vessels": (GenericVessel, True),
    "Lines": (GenericLine, True),
    "6DBuoys": (GenericBuoy6D, True),
    "3DBuoys": (GenericBuoy3D, True),
    "Shapes": (GenericShape, True),
    "Constraints": (GenericConstraint, True),
    "Links": (GenericLink, True),
    "Winches": (GenericWinch, True),
    "ClumpTypes": (GenericClumpType, True),
    "WingTypes": (GenericWingType, True),
    "FlexJointTypes": (GenericFlexJointType, True),
    "FlexJoints": (GenericFlexJoint, True),
    "DragChainTypes": (GenericDragChainType, True),
    "DragChains": (GenericDragChain, True),
    "StiffenerTypes": (GenericStiffenerType, True),
    "SupportTypes": (GenericSupportType, True),
    "MorisonElementTypes": (GenericMorisonElementType, True),
    "Turbines": (GenericTurbine, True),
    "AttachedBuoys": (GenericAttachedBuoy, True),
    "VariableDataSources": (GenericVariableData, True),
    "ExpansionTables": (GenericObject, True),
    "PyModels": (GenericObject, True),
    "WakeModels": (GenericObject, True),
    "MultibodyGroups": (GenericObject, True),
    "BrowserGroups": (GenericObject, True),
}

SINGLETON_SECTIONS: dict[str, str] = {
    "SolidFrictionCoefficients": "friction_coefficients",
    "LineContactData": "line_contact_data",
    "CodeChecks": "code_checks",
    "Shear7Data": "shear7_data",
    "VIVAData": "viva_data",
    "RayleighDampingCoefficients": "rayleigh_damping",
}

# Map spec field name -> OrcaFlex YAML section key
FIELD_TO_SECTION: dict[str, str] = {
    "line_types": "LineTypes",
    "vessel_types": "VesselTypes",
    "vessels": "Vessels",
    "lines": "Lines",
    "buoys_6d": "6DBuoys",
    "buoys_3d": "3DBuoys",
    "shapes": "Shapes",
    "constraints": "Constraints",
    "links": "Links",
    "winches": "Winches",
    "clump_types": "ClumpTypes",
    "wing_types": "WingTypes",
    "flex_joint_types": "FlexJointTypes",
    "flex_joints": "FlexJoints",
    "drag_chain_types": "DragChainTypes",
    "drag_chains": "DragChains",
    "stiffener_types": "StiffenerTypes",
    "support_types": "SupportTypes",
    "morison_element_types": "MorisonElementTypes",
    "turbines": "Turbines",
    "attached_buoys": "AttachedBuoys",
    "variable_data_sources": "VariableDataSources",
    "expansion_tables": "ExpansionTables",
    "py_models": "PyModels",
    "wake_models": "WakeModels",
    "multibody_groups": "MultibodyGroups",
    "browser_groups": "BrowserGroups",
}

# Map typed field name -> OrcaFlex YAML key for _merge_object()
TYPED_FIELD_MAP: dict[str, str] = {
    "name": "Name",
    "category": "Category",
    "outer_diameter": "OD",
    "inner_diameter": "ID",
    "mass_per_length": "MassPerUnitLength",
    "bending_stiffness": "EI",
    "axial_stiffness": "EA",
    "vessel_type": "VesselType",
    "connection": "Connection",
    "initial_position": "InitialPosition",
    "buoy_type": "BuoyType",
    "shape_type": "ShapeType",
    "shape": "Shape",
    "origin": "Origin",
    "in_frame_connection": "InFrameConnection",
    "constraint_type": "ConstraintType",
    "link_type": "LinkType",
    "mass": "Mass",
    "volume": "Volume",
    "length": "Length",
    "data_type": "DataType",
}


class GenericModel(BaseModel):
    """Complete generic OrcaFlex model specification.

    Composes all object types as optional lists plus pass-through dicts
    for singleton sections. Any field that is ``None`` or empty will be
    skipped by the builder.

    Attributes:
        general_properties: Pass-through for General section overrides.
        line_types: LineType definitions.
        vessel_types: VesselType definitions.
        vessels: Vessel instances.
        lines: Line instances.
        buoys_6d: 6D Buoy instances.
        buoys_3d: 3D Buoy instances.
        shapes: Shape instances.
        constraints: Constraint instances.
        links: Link instances.
        winches: Winch instances.
        clump_types: ClumpType definitions.
        wing_types: WingType definitions.
        flex_joint_types: FlexJointType definitions.
        flex_joints: FlexJoint instances.
        drag_chain_types: DragChainType definitions.
        drag_chains: DragChain instances.
        stiffener_types: StiffenerType definitions.
        support_types: SupportType definitions.
        morison_element_types: MorisonElementType definitions.
        turbines: Turbine instances.
        attached_buoys: AttachedBuoy instances.
        variable_data_sources: VariableDataSource definitions.
        expansion_tables: ExpansionTable definitions.
        py_models: PyModel definitions.
        wake_models: WakeModel definitions.
        multibody_groups: MultibodyGroup definitions.
        browser_groups: BrowserGroup definitions.
        friction_coefficients: SolidFrictionCoefficients section.
        line_contact_data: LineContactData section.
        code_checks: CodeChecks section.
        shear7_data: Shear7Data section.
        viva_data: VIVAData section.
        rayleigh_damping: RayleighDampingCoefficients section.
    """

    # General overrides (merged with builder-generated General section)
    general_properties: dict[str, Any] = Field(
        default_factory=dict,
        description="Pass-through General section overrides",
    )

    # List-based object sections
    line_types: list[GenericLineType] = Field(default_factory=list)
    vessel_types: list[GenericVesselType] = Field(default_factory=list)
    vessels: list[GenericVessel] = Field(default_factory=list)
    lines: list[GenericLine] = Field(default_factory=list)
    buoys_6d: list[GenericBuoy6D] = Field(default_factory=list)
    buoys_3d: list[GenericBuoy3D] = Field(default_factory=list)
    shapes: list[GenericShape] = Field(default_factory=list)
    constraints: list[GenericConstraint] = Field(default_factory=list)
    links: list[GenericLink] = Field(default_factory=list)
    winches: list[GenericWinch] = Field(default_factory=list)
    clump_types: list[GenericClumpType] = Field(default_factory=list)
    wing_types: list[GenericWingType] = Field(default_factory=list)
    flex_joint_types: list[GenericFlexJointType] = Field(default_factory=list)
    flex_joints: list[GenericFlexJoint] = Field(default_factory=list)
    drag_chain_types: list[GenericDragChainType] = Field(default_factory=list)
    drag_chains: list[GenericDragChain] = Field(default_factory=list)
    stiffener_types: list[GenericStiffenerType] = Field(default_factory=list)
    support_types: list[GenericSupportType] = Field(default_factory=list)
    morison_element_types: list[GenericMorisonElementType] = Field(
        default_factory=list
    )
    turbines: list[GenericTurbine] = Field(default_factory=list)
    attached_buoys: list[GenericAttachedBuoy] = Field(default_factory=list)
    variable_data_sources: list[GenericVariableData] = Field(default_factory=list)
    expansion_tables: list[GenericObject] = Field(default_factory=list)
    py_models: list[GenericObject] = Field(default_factory=list)
    wake_models: list[GenericObject] = Field(default_factory=list)
    multibody_groups: list[GenericObject] = Field(default_factory=list)
    browser_groups: list[GenericObject] = Field(default_factory=list)

    # Singleton sections (pass-through dicts)
    friction_coefficients: GenericFrictionCoefficients | None = Field(default=None)
    line_contact_data: GenericSingletonSection | None = Field(default=None)
    code_checks: GenericSingletonSection | None = Field(default=None)
    shear7_data: GenericSingletonSection | None = Field(default=None)
    viva_data: GenericSingletonSection | None = Field(default=None)
    rayleigh_damping: GenericSingletonSection | None = Field(default=None)
