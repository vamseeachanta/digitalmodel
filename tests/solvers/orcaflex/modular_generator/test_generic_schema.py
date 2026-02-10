"""Tests for generic OrcaFlex model Pydantic schema.

Covers GenericObject, all typed object subclasses, GenericModel composition,
mapping constants (FIELD_TO_SECTION, TYPED_FIELD_MAP, SECTION_REGISTRY,
SINGLETON_SECTIONS), and ProjectInputSpec integration with the generic field.
"""

import pytest

from digitalmodel.solvers.orcaflex.modular_generator.schema import (
    FIELD_TO_SECTION,
    SECTION_REGISTRY,
    SINGLETON_SECTIONS,
    TYPED_FIELD_MAP,
    GenericAttachedBuoy,
    GenericBuoy3D,
    GenericBuoy6D,
    GenericClumpType,
    GenericConstraint,
    GenericDragChain,
    GenericDragChainType,
    GenericFlexJoint,
    GenericFlexJointType,
    GenericFrictionCoefficients,
    GenericLine,
    GenericLineType,
    GenericLink,
    GenericModel,
    GenericMorisonElementType,
    GenericObject,
    GenericShape,
    GenericSingletonSection,
    GenericStiffenerType,
    GenericSupportType,
    GenericTurbine,
    GenericVariableData,
    GenericVessel,
    GenericVesselType,
    GenericWinch,
    GenericWingType,
    ProjectInputSpec,
)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _minimal_env():
    return {
        "water": {"depth": 100, "density": 1.025},
        "seabed": {"stiffness": {"normal": 100, "shear": 100}},
    }


def _minimal_metadata():
    return {
        "name": "test",
        "description": "test",
        "structure": "generic",
        "operation": "generic",
        "project": "test",
    }


# ---------------------------------------------------------------------------
# GenericObject
# ---------------------------------------------------------------------------


class TestGenericObject:
    """Tests for the GenericObject base model."""

    def test_create_with_name_and_empty_properties(self):
        obj = GenericObject(name="myobj")
        assert obj.name == "myobj"
        assert obj.properties == {}

    def test_create_with_name_and_properties(self):
        obj = GenericObject(name="myobj", properties={"Drag": 1.2, "Lift": 0.5})
        assert obj.name == "myobj"
        assert obj.properties["Drag"] == 1.2
        assert obj.properties["Lift"] == 0.5

    def test_name_cannot_be_empty(self):
        with pytest.raises(Exception):
            GenericObject(name="")

    def test_name_minimum_length_one(self):
        obj = GenericObject(name="x")
        assert obj.name == "x"


# ---------------------------------------------------------------------------
# GenericLineType
# ---------------------------------------------------------------------------


class TestGenericLineType:
    """Tests for GenericLineType typed fields."""

    def test_create_with_all_typed_fields(self):
        lt = GenericLineType(
            name="pipe1",
            category="General",
            outer_diameter=0.3,
            inner_diameter=0.2,
            mass_per_length=0.1,
            bending_stiffness=100.0,
            axial_stiffness=500000.0,
        )
        assert lt.name == "pipe1"
        assert lt.category == "General"
        assert lt.outer_diameter == 0.3
        assert lt.inner_diameter == 0.2
        assert lt.mass_per_length == 0.1
        assert lt.bending_stiffness == 100.0
        assert lt.axial_stiffness == 500000.0

    def test_typed_fields_are_optional(self):
        lt = GenericLineType(name="bare")
        assert lt.category is None
        assert lt.outer_diameter is None
        assert lt.inner_diameter is None
        assert lt.mass_per_length is None
        assert lt.bending_stiffness is None
        assert lt.axial_stiffness is None

    def test_properties_bag_alongside_typed_fields(self):
        lt = GenericLineType(
            name="pipe2",
            category="General",
            properties={"Drag": 1.0, "Added Mass": 0.5},
        )
        assert lt.category == "General"
        assert lt.properties["Drag"] == 1.0
        assert lt.properties["Added Mass"] == 0.5

    def test_bending_stiffness_accepts_list(self):
        lt = GenericLineType(name="variable_ei", bending_stiffness=[100, 200])
        assert lt.bending_stiffness == [100, 200]


# ---------------------------------------------------------------------------
# Typed object subclasses
# ---------------------------------------------------------------------------


class TestGenericVesselType:
    def test_create_with_length(self):
        vt = GenericVesselType(name="barge", length=100.0)
        assert vt.name == "barge"
        assert vt.length == 100.0

    def test_length_optional(self):
        vt = GenericVesselType(name="barge")
        assert vt.length is None


class TestGenericVessel:
    def test_create_with_typed_fields(self):
        v = GenericVessel(
            name="vessel1",
            vessel_type="barge",
            connection="Fixed",
            initial_position=[0, 0, 0],
        )
        assert v.vessel_type == "barge"
        assert v.connection == "Fixed"
        assert v.initial_position == [0, 0, 0]


class TestGenericBuoy6D:
    def test_create_with_typed_fields(self):
        b = GenericBuoy6D(
            name="buoy1",
            buoy_type="Spar",
            connection="Free",
            initial_position=[10, 0, -5],
        )
        assert b.buoy_type == "Spar"
        assert b.initial_position == [10, 0, -5]


class TestGenericBuoy3D:
    def test_create_with_typed_fields(self):
        b = GenericBuoy3D(
            name="buoy3d",
            connection="Fixed",
            initial_position=[0, 0, 0],
        )
        assert b.connection == "Fixed"


class TestGenericLine:
    def test_create_basic(self):
        line = GenericLine(name="riser1")
        assert line.name == "riser1"
        assert line.line_type_refs == []

    def test_create_with_line_type_refs(self):
        line = GenericLine(name="riser1", line_type_refs=["pipe1", "flex1"])
        assert line.line_type_refs == ["pipe1", "flex1"]


class TestGenericShape:
    def test_create_with_typed_fields(self):
        s = GenericShape(
            name="shape1",
            shape_type="Drawing",
            shape="cylinder",
            connection="Fixed",
            origin=[0, 0, 0],
        )
        assert s.shape_type == "Drawing"
        assert s.shape == "cylinder"


class TestGenericConstraint:
    def test_create_basic(self):
        c = GenericConstraint(
            name="con1",
            in_frame_connection="vessel1",
            constraint_type="Calculated",
        )
        assert c.in_frame_connection == "vessel1"
        assert c.constraint_type == "Calculated"


class TestGenericLink:
    def test_create_basic(self):
        link = GenericLink(name="link1", link_type="Tether")
        assert link.link_type == "Tether"


# ---------------------------------------------------------------------------
# Variable data and singleton sections
# ---------------------------------------------------------------------------


class TestGenericVariableData:
    def test_create_with_data_type_and_entries(self):
        vd = GenericVariableData(
            name="GenericDrag",
            data_type="Dragcoefficient",
            entries=[{"x": 0, "y": 1.0}],
        )
        assert vd.data_type == "Dragcoefficient"
        assert len(vd.entries) == 1

    def test_entries_optional(self):
        vd = GenericVariableData(name="empty")
        assert vd.entries is None


class TestGenericFrictionCoefficients:
    def test_create_with_data(self):
        fc = GenericFrictionCoefficients(
            data={"StaticFriction": 0.3, "DynamicFriction": 0.2}
        )
        assert fc.data["StaticFriction"] == 0.3

    def test_empty_data_default(self):
        fc = GenericFrictionCoefficients()
        assert fc.data == {}


class TestGenericSingletonSection:
    def test_create_with_data(self):
        s = GenericSingletonSection(data={"ContactStiffness": 100})
        assert s.data["ContactStiffness"] == 100

    def test_empty_data_default(self):
        s = GenericSingletonSection()
        assert s.data == {}


# ---------------------------------------------------------------------------
# GenericModel composition
# ---------------------------------------------------------------------------


class TestGenericModel:
    def test_create_empty(self):
        model = GenericModel()
        assert model.line_types == []
        assert model.vessels == []
        assert model.lines == []
        assert model.general_properties == {}
        assert model.friction_coefficients is None

    def test_create_with_line_types(self):
        lt = GenericLineType(name="pipe1", category="General")
        model = GenericModel(line_types=[lt])
        assert len(model.line_types) == 1
        assert model.line_types[0].name == "pipe1"

    def test_create_with_multiple_object_lists(self):
        lt = GenericLineType(name="pipe1")
        v = GenericVessel(name="vessel1")
        line = GenericLine(name="riser1")
        model = GenericModel(line_types=[lt], vessels=[v], lines=[line])
        assert len(model.line_types) == 1
        assert len(model.vessels) == 1
        assert len(model.lines) == 1

    def test_general_properties(self):
        model = GenericModel(
            general_properties={"StaticsMethod": "Full statics"}
        )
        assert model.general_properties["StaticsMethod"] == "Full statics"

    def test_singleton_sections(self):
        fc = GenericFrictionCoefficients(data={"StaticFriction": 0.3})
        lc = GenericSingletonSection(data={"ContactStiffness": 50})
        model = GenericModel(friction_coefficients=fc, line_contact_data=lc)
        assert model.friction_coefficients.data["StaticFriction"] == 0.3
        assert model.line_contact_data.data["ContactStiffness"] == 50

    def test_all_list_fields_default_empty(self):
        model = GenericModel()
        list_fields = [
            "line_types", "vessel_types", "vessels", "lines",
            "buoys_6d", "buoys_3d", "shapes", "constraints",
            "links", "winches", "clump_types", "wing_types",
            "flex_joint_types", "flex_joints", "drag_chain_types",
            "drag_chains", "stiffener_types", "support_types",
            "morison_element_types", "turbines", "attached_buoys",
            "variable_data_sources", "expansion_tables", "py_models",
            "wake_models", "multibody_groups", "browser_groups",
        ]
        for field_name in list_fields:
            assert getattr(model, field_name) == [], f"{field_name} should be empty list"


# ---------------------------------------------------------------------------
# Mapping constants
# ---------------------------------------------------------------------------


class TestFieldToSection:
    def test_contains_line_types(self):
        assert FIELD_TO_SECTION["line_types"] == "LineTypes"

    def test_contains_vessels(self):
        assert FIELD_TO_SECTION["vessels"] == "Vessels"

    def test_contains_lines(self):
        assert FIELD_TO_SECTION["lines"] == "Lines"

    def test_contains_buoys_6d(self):
        assert FIELD_TO_SECTION["buoys_6d"] == "6DBuoys"

    def test_contains_variable_data_sources(self):
        assert FIELD_TO_SECTION["variable_data_sources"] == "VariableDataSources"

    def test_contains_links(self):
        assert FIELD_TO_SECTION["links"] == "Links"


class TestTypedFieldMap:
    def test_name_maps_to_name(self):
        assert TYPED_FIELD_MAP["name"] == "Name"

    def test_category_maps_to_category(self):
        assert TYPED_FIELD_MAP["category"] == "Category"

    def test_outer_diameter_maps_to_od(self):
        assert TYPED_FIELD_MAP["outer_diameter"] == "OD"

    def test_inner_diameter_maps_to_id(self):
        assert TYPED_FIELD_MAP["inner_diameter"] == "ID"

    def test_mass_per_length_maps_to_mass_per_unit_length(self):
        assert TYPED_FIELD_MAP["mass_per_length"] == "MassPerUnitLength"

    def test_bending_stiffness_maps_to_ei(self):
        assert TYPED_FIELD_MAP["bending_stiffness"] == "EI"

    def test_axial_stiffness_maps_to_ea(self):
        assert TYPED_FIELD_MAP["axial_stiffness"] == "EA"

    def test_vessel_type_maps(self):
        assert TYPED_FIELD_MAP["vessel_type"] == "VesselType"

    def test_connection_maps(self):
        assert TYPED_FIELD_MAP["connection"] == "Connection"

    def test_link_type_maps(self):
        assert TYPED_FIELD_MAP["link_type"] == "LinkType"


class TestSectionRegistry:
    def test_line_types_entry(self):
        cls, is_list = SECTION_REGISTRY["LineTypes"]
        assert cls is GenericLineType
        assert is_list is True

    def test_vessels_entry(self):
        cls, is_list = SECTION_REGISTRY["Vessels"]
        assert cls is GenericVessel
        assert is_list is True

    def test_contains_all_field_to_section_values(self):
        for section_key in FIELD_TO_SECTION.values():
            assert section_key in SECTION_REGISTRY, (
                f"FIELD_TO_SECTION value '{section_key}' missing from SECTION_REGISTRY"
            )


class TestSingletonSections:
    def test_solid_friction_coefficients(self):
        assert SINGLETON_SECTIONS["SolidFrictionCoefficients"] == "friction_coefficients"

    def test_line_contact_data(self):
        assert SINGLETON_SECTIONS["LineContactData"] == "line_contact_data"

    def test_code_checks(self):
        assert SINGLETON_SECTIONS["CodeChecks"] == "code_checks"


# ---------------------------------------------------------------------------
# ProjectInputSpec with generic
# ---------------------------------------------------------------------------


class TestProjectInputSpecGeneric:
    """Tests for ProjectInputSpec integration with GenericModel."""

    def test_create_spec_with_generic(self):
        spec = ProjectInputSpec(
            metadata=_minimal_metadata(),
            environment=_minimal_env(),
            generic=GenericModel(
                line_types=[GenericLineType(name="pipe1")]
            ),
        )
        assert spec.generic is not None
        assert len(spec.generic.line_types) == 1

    def test_is_generic_returns_true(self):
        spec = ProjectInputSpec(
            metadata=_minimal_metadata(),
            environment=_minimal_env(),
            generic=GenericModel(),
        )
        assert spec.is_generic() is True

    def test_is_pipeline_returns_false_for_generic(self):
        spec = ProjectInputSpec(
            metadata=_minimal_metadata(),
            environment=_minimal_env(),
            generic=GenericModel(),
        )
        assert spec.is_pipeline() is False

    def test_is_riser_returns_false_for_generic(self):
        spec = ProjectInputSpec(
            metadata=_minimal_metadata(),
            environment=_minimal_env(),
            generic=GenericModel(),
        )
        assert spec.is_riser() is False

    def test_cannot_have_generic_and_pipeline(self):
        """Defining both generic and pipeline must raise a validation error.

        Pydantic may reject the pipeline data at field-level first (missing
        required fields) or the model_validator catches the mutual exclusion.
        Either way the spec cannot be constructed.
        """
        with pytest.raises(Exception):
            ProjectInputSpec(
                metadata=_minimal_metadata(),
                environment=_minimal_env(),
                generic=GenericModel(),
                pipeline={
                    "name": "pipe",
                    "material": "steel",
                    "dimensions": {"outer_diameter": 0.6096, "wall_thickness": 0.02},
                    "coatings": {"corrosion": {"thickness": 0.003, "density": 1.3}},
                    "segments": [{"type": "seabed", "segment_length": 1000, "length": 1000}],
                },
            )

    def test_cannot_have_generic_and_riser(self):
        """Defining both generic and riser must raise a validation error.

        Pydantic may reject the riser data at field-level first or the
        model_validator catches the mutual exclusion. Either way the spec
        cannot be constructed.
        """
        with pytest.raises(Exception):
            ProjectInputSpec(
                metadata=_minimal_metadata(),
                environment=_minimal_env(),
                generic=GenericModel(),
                riser={
                    "vessel": {"name": "fpso", "position": [0, 0, 0]},
                    "line_types": [{
                        "name": "pipe",
                        "outer_diameter": 0.3,
                        "inner_diameter": 0.2,
                        "mass_per_length": 0.1,
                        "bending_stiffness": 100,
                        "axial_stiffness": 500000,
                    }],
                    "lines": [{
                        "name": "riser1",
                        "sections": [{"line_type": "pipe", "segment_length": 100}],
                        "end_a": {"type": "vessel", "name": "fpso", "position": [0, 0, 0]},
                        "end_b": {"type": "anchor", "position": [0, 0, -100]},
                    }],
                },
            )

    def test_none_of_three_raises_error(self):
        with pytest.raises(Exception, match="must be defined"):
            ProjectInputSpec(
                metadata=_minimal_metadata(),
                environment=_minimal_env(),
            )
