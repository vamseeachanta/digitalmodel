"""Tests for GenericModelBuilder.

Covers should_generate gating, build output for list sections, singleton
sections, general properties, _merge_object typed field/properties merging,
and context registration of entity names.
"""

import pytest

from digitalmodel.solvers.orcaflex.modular_generator.builders.generic_builder import (
    GenericModelBuilder,
)
from digitalmodel.solvers.orcaflex.modular_generator.builders.context import (
    BuilderContext,
)
from digitalmodel.solvers.orcaflex.modular_generator.schema import (
    GenericConstraint,
    GenericFrictionCoefficients,
    GenericLine,
    GenericLineType,
    GenericLink,
    GenericModel,
    GenericObject,
    GenericSingletonSection,
    GenericVariableData,
    GenericVessel,
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


def _make_builder(generic_model: GenericModel) -> tuple[GenericModelBuilder, BuilderContext]:
    """Create a GenericModelBuilder from a GenericModel with minimal spec."""
    spec = ProjectInputSpec(
        metadata=_minimal_metadata(),
        environment=_minimal_env(),
        generic=generic_model,
    )
    ctx = BuilderContext()
    builder = GenericModelBuilder(spec, ctx)
    return builder, ctx


# ---------------------------------------------------------------------------
# should_generate
# ---------------------------------------------------------------------------


class TestShouldGenerate:
    """Tests for GenericModelBuilder.should_generate gating logic."""

    def test_returns_true_for_generic_spec(self):
        builder, _ = _make_builder(GenericModel())
        assert builder.should_generate() is True

    def test_returns_false_for_pipeline_spec(self):
        spec = ProjectInputSpec(
            metadata={
                "name": "test",
                "description": "test",
                "structure": "pipeline",
                "operation": "installation",
                "project": "test",
            },
            environment=_minimal_env(),
            pipeline={
                "name": "30in Line",
                "material": "X65",
                "dimensions": {"outer_diameter": 0.6096, "wall_thickness": 0.02},
                "coatings": {"corrosion": {"thickness": 0.003, "density": 1.3}},
                "segments": [{"type": "seabed", "length": 1000, "segment_length": 10}],
            },
        )
        ctx = BuilderContext()
        builder = GenericModelBuilder(spec, ctx)
        assert builder.should_generate() is False

    def test_returns_false_for_riser_spec(self):
        spec = ProjectInputSpec(
            metadata={
                "name": "test",
                "description": "test",
                "structure": "riser",
                "operation": "production",
                "project": "test",
            },
            environment=_minimal_env(),
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
                    "sections": [{"line_type": "pipe", "length": 100, "segment_length": 5}],
                    "end_a": {"type": "vessel", "name": "fpso", "position": [0, 0, 0]},
                    "end_b": {"type": "anchor", "position": [0, 0, -100]},
                }],
            },
        )
        ctx = BuilderContext()
        builder = GenericModelBuilder(spec, ctx)
        assert builder.should_generate() is False


# ---------------------------------------------------------------------------
# build: list sections
# ---------------------------------------------------------------------------


class TestBuildListSections:
    """Tests for building list-based OrcaFlex sections."""

    def test_build_line_types_creates_section(self):
        model = GenericModel(
            line_types=[
                GenericLineType(
                    name="pipe1",
                    category="General",
                    outer_diameter=0.3,
                    properties={"Drag": 1.0},
                )
            ]
        )
        builder, _ = _make_builder(model)
        result = builder.build()

        assert "LineTypes" in result
        assert len(result["LineTypes"]) == 1
        lt = result["LineTypes"][0]
        assert lt["Name"] == "pipe1"
        assert lt["Category"] == "General"
        assert lt["OD"] == 0.3
        assert lt["Drag"] == 1.0

    def test_typed_fields_override_properties_on_conflict(self):
        model = GenericModel(
            line_types=[
                GenericLineType(
                    name="pipe1",
                    category="General",
                    properties={"Category": "Homogeneous Pipe"},
                )
            ]
        )
        builder, _ = _make_builder(model)
        result = builder.build()

        lt = result["LineTypes"][0]
        # Typed field "category" -> "Category" should override properties["Category"]
        assert lt["Category"] == "General"

    def test_build_multiple_sections(self):
        model = GenericModel(
            line_types=[GenericLineType(name="pipe1")],
            vessels=[GenericVessel(name="vessel1", vessel_type="barge")],
            lines=[GenericLine(name="riser1")],
        )
        builder, _ = _make_builder(model)
        result = builder.build()

        assert "LineTypes" in result
        assert "Vessels" in result
        assert "Lines" in result
        assert result["Vessels"][0]["Name"] == "vessel1"
        assert result["Vessels"][0]["VesselType"] == "barge"

    def test_empty_lists_are_skipped(self):
        model = GenericModel(line_types=[], vessels=[])
        builder, _ = _make_builder(model)
        result = builder.build()

        assert "LineTypes" not in result
        assert "Vessels" not in result


# ---------------------------------------------------------------------------
# build: variable data sources
# ---------------------------------------------------------------------------


class TestBuildVariableDataSources:
    """Tests for building VariableDataSources section."""

    def test_build_variable_data_sources(self):
        model = GenericModel(
            variable_data_sources=[
                GenericVariableData(
                    name="GenericDrag",
                    data_type="Dragcoefficient",
                    entries=[{"x": 0, "y": 1.0}],
                    properties={"Interpolation": "Linear"},
                )
            ]
        )
        builder, _ = _make_builder(model)
        result = builder.build()

        assert "VariableDataSources" in result
        vds = result["VariableDataSources"][0]
        assert vds["Name"] == "GenericDrag"
        assert vds["DataType"] == "Dragcoefficient"
        assert vds["Interpolation"] == "Linear"


# ---------------------------------------------------------------------------
# build: singleton sections
# ---------------------------------------------------------------------------


class TestBuildSingletonSections:
    """Tests for building singleton OrcaFlex sections."""

    def test_build_friction_coefficients(self):
        model = GenericModel(
            friction_coefficients=GenericFrictionCoefficients(
                data={"StaticFriction": 0.3, "DynamicFriction": 0.2}
            )
        )
        builder, _ = _make_builder(model)
        result = builder.build()

        assert "SolidFrictionCoefficients" in result
        assert result["SolidFrictionCoefficients"]["StaticFriction"] == 0.3

    def test_build_line_contact_data(self):
        model = GenericModel(
            line_contact_data=GenericSingletonSection(
                data={"ContactStiffness": 100}
            )
        )
        builder, _ = _make_builder(model)
        result = builder.build()

        assert "LineContactData" in result
        assert result["LineContactData"]["ContactStiffness"] == 100

    def test_none_singleton_is_skipped(self):
        model = GenericModel()
        builder, _ = _make_builder(model)
        result = builder.build()

        assert "SolidFrictionCoefficients" not in result
        assert "LineContactData" not in result

    def test_empty_singleton_data_is_skipped(self):
        model = GenericModel(
            friction_coefficients=GenericFrictionCoefficients(data={})
        )
        builder, _ = _make_builder(model)
        result = builder.build()

        assert "SolidFrictionCoefficients" not in result


# ---------------------------------------------------------------------------
# build: general properties
# ---------------------------------------------------------------------------


class TestBuildGeneralProperties:
    """Tests for building the General section."""

    def test_build_general_properties(self):
        model = GenericModel(
            general_properties={"StaticsMethod": "Full statics", "DynamicsSolutionMethod": "Implicit"}
        )
        builder, _ = _make_builder(model)
        result = builder.build()

        assert "General" in result
        assert result["General"]["StaticsMethod"] == "Full statics"
        assert result["General"]["DynamicsSolutionMethod"] == "Implicit"

    def test_empty_general_properties_skipped(self):
        model = GenericModel(general_properties={})
        builder, _ = _make_builder(model)
        result = builder.build()

        assert "General" not in result


# ---------------------------------------------------------------------------
# _merge_object
# ---------------------------------------------------------------------------


class TestMergeObject:
    """Tests for GenericModelBuilder._merge_object static method."""

    def test_merges_typed_fields_via_map(self):
        obj = GenericLineType(
            name="pipe1",
            category="General",
            outer_diameter=0.3,
        )
        merged = GenericModelBuilder._merge_object(obj)

        assert merged["Name"] == "pipe1"
        assert merged["Category"] == "General"
        assert merged["OD"] == 0.3

    def test_properties_pass_through(self):
        obj = GenericLineType(
            name="pipe1",
            properties={"Drag": 1.2, "AddedMass": 0.5},
        )
        merged = GenericModelBuilder._merge_object(obj)

        assert merged["Drag"] == 1.2
        assert merged["AddedMass"] == 0.5
        assert merged["Name"] == "pipe1"

    def test_typed_fields_take_priority(self):
        obj = GenericLineType(
            name="pipe1",
            category="General",
            properties={"Category": "Homogeneous Pipe", "Extra": 42},
        )
        merged = GenericModelBuilder._merge_object(obj)

        assert merged["Category"] == "General"
        assert merged["Extra"] == 42

    def test_none_typed_fields_excluded(self):
        obj = GenericLineType(name="pipe1")
        merged = GenericModelBuilder._merge_object(obj)

        assert "OD" not in merged
        assert "ID" not in merged
        assert "Category" not in merged
        assert merged["Name"] == "pipe1"

    def test_merge_with_generic_object_base(self):
        obj = GenericObject(name="obj1", properties={"Foo": "bar"})
        merged = GenericModelBuilder._merge_object(obj)

        assert merged["Name"] == "obj1"
        assert merged["Foo"] == "bar"


# ---------------------------------------------------------------------------
# Context registration
# ---------------------------------------------------------------------------


class TestContextRegistration:
    """Tests for entity name registration into BuilderContext."""

    def test_line_type_names_registered(self):
        model = GenericModel(
            line_types=[
                GenericLineType(name="pipe1"),
                GenericLineType(name="pipe2"),
            ]
        )
        builder, ctx = _make_builder(model)
        builder.build()

        assert ctx.line_type_names == ["pipe1", "pipe2"]

    def test_vessel_names_registered(self):
        model = GenericModel(
            vessels=[GenericVessel(name="vessel1")]
        )
        builder, ctx = _make_builder(model)
        builder.build()

        assert ctx.vessel_names == ["vessel1"]

    def test_line_names_registered(self):
        model = GenericModel(
            lines=[GenericLine(name="line1"), GenericLine(name="line2")]
        )
        builder, ctx = _make_builder(model)
        builder.build()

        assert ctx.line_names == ["line1", "line2"]

    def test_shape_names_registered(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema import GenericShape

        model = GenericModel(
            shapes=[GenericShape(name="seabed_shape")]
        )
        builder, ctx = _make_builder(model)
        builder.build()

        assert ctx.shape_names == ["seabed_shape"]

    def test_sections_without_context_key_do_not_register(self):
        """Constraints have no context key mapping, so build succeeds without
        registering names into the context."""
        model = GenericModel(
            constraints=[GenericConstraint(name="con1")]
        )
        builder, ctx = _make_builder(model)
        result = builder.build()

        assert "Constraints" in result

    def test_empty_context_when_no_objects(self):
        model = GenericModel()
        builder, ctx = _make_builder(model)
        builder.build()

        assert ctx.line_type_names == []
        assert ctx.vessel_names == []
        assert ctx.line_names == []
