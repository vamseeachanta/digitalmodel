"""Tests for LineTypeBuilder property mapping."""
from __future__ import annotations

import pytest

from digitalmodel.solvers.orcaflex.modular_generator.builders.context import (
    BuilderContext,
)
from digitalmodel.solvers.orcaflex.modular_generator.builders.linetype_builder import (
    DEFAULT_HYDRO_COEFFS,
    MATERIAL_PROPERTIES,
    LineTypeBuilder,
)
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _make_pipeline_spec(
    od=0.762,
    wt=0.0254,
    material="X65",
    segments=None,
    coatings=None,
    tugs=None,
    tensioner=None,
    vessel=None,
    youngs_modulus=None,
    poissons_ratio=None,
):
    """Create a pipeline spec for LineTypeBuilder testing."""
    pipeline = {
        "name": "test_pipe",
        "material": material,
        "dimensions": {"outer_diameter": od, "wall_thickness": wt},
        "coatings": coatings or {"corrosion": {"thickness": 0.003, "density": 1.1}},
        "segments": segments or [{"type": "X65+CWC80", "length": 1000, "segment_length": 5}],
    }
    if youngs_modulus is not None:
        pipeline["youngs_modulus"] = youngs_modulus
    if poissons_ratio is not None:
        pipeline["poissons_ratio"] = poissons_ratio

    data = {
        "metadata": {
            "name": "test",
            "description": "test",
            "structure": "pipeline",
            "operation": "installation",
            "project": "TEST",
        },
        "environment": {
            "water": {"depth": 100, "density": 1.025},
            "seabed": {"stiffness": {"normal": 100, "shear": 50}},
        },
        "pipeline": pipeline,
    }
    equipment = {}
    if tugs:
        equipment["tugs"] = tugs
    if tensioner:
        equipment["tensioner"] = tensioner
    if vessel:
        equipment["vessel"] = vessel
    if equipment:
        data["equipment"] = equipment
    return ProjectInputSpec(**data)


def _build(spec, coating_names=None):
    """Run builder and return the LineTypes list."""
    ctx = BuilderContext()
    if coating_names:
        ctx.coating_names = coating_names
    builder = LineTypeBuilder(spec, ctx)
    result = builder.build()
    return result["LineTypes"]


# ---------------------------------------------------------------------------
# Pipe dimensions
# ---------------------------------------------------------------------------

class TestPipeDimensions:
    def test_od_mapping(self):
        lt_list = _build(_make_pipeline_spec(od=0.762, wt=0.0254))
        lt = lt_list[0]
        assert lt["OD"] == 0.762

    def test_id_calculated(self):
        """ID = OD - 2*wall_thickness."""
        lt_list = _build(_make_pipeline_spec(od=0.762, wt=0.0254))
        lt = lt_list[0]
        assert lt["ID"] == pytest.approx(0.762 - 2 * 0.0254)

    def test_small_pipe(self):
        lt_list = _build(_make_pipeline_spec(od=0.3048, wt=0.0127))
        lt = lt_list[0]
        assert lt["OD"] == 0.3048
        assert lt["ID"] == pytest.approx(0.3048 - 2 * 0.0127)


# ---------------------------------------------------------------------------
# Material properties
# ---------------------------------------------------------------------------

class TestMaterialProperties:
    def test_x65_defaults(self):
        lt_list = _build(_make_pipeline_spec(material="X65"))
        lt = lt_list[0]
        assert lt["MaterialDensity"] == MATERIAL_PROPERTIES["X65"]["MaterialDensity"]
        assert lt["E"] == MATERIAL_PROPERTIES["X65"]["E"]
        assert lt["PoissonRatio"] == MATERIAL_PROPERTIES["X65"]["PoissonRatio"]

    def test_x70_grade(self):
        lt_list = _build(_make_pipeline_spec(material="X70"))
        lt = lt_list[0]
        assert lt["DNVSTF101Fy"] == MATERIAL_PROPERTIES["X70"]["DNVSTF101Fy"]

    def test_unknown_grade_falls_back_to_x65(self):
        lt_list = _build(_make_pipeline_spec(material="X999"))
        lt = lt_list[0]
        assert lt["MaterialDensity"] == MATERIAL_PROPERTIES["X65"]["MaterialDensity"]

    def test_custom_youngs_modulus(self):
        """Spec-level youngs_modulus overrides grade default."""
        lt_list = _build(_make_pipeline_spec(youngs_modulus=200e6))
        lt = lt_list[0]
        assert lt["E"] == 200e6

    def test_custom_poissons_ratio(self):
        lt_list = _build(_make_pipeline_spec(poissons_ratio=0.3))
        lt = lt_list[0]
        assert lt["PoissonRatio"] == 0.3


# ---------------------------------------------------------------------------
# Drag coefficients
# ---------------------------------------------------------------------------

class TestDragCoefficients:
    def test_normal_drag(self):
        lt_list = _build(_make_pipeline_spec())
        lt = lt_list[0]
        assert lt["Cdn"] == DEFAULT_HYDRO_COEFFS["Cdn"]
        assert lt["Cdn"] == 1.18

    def test_axial_drag(self):
        lt_list = _build(_make_pipeline_spec())
        lt = lt_list[0]
        assert lt["Cdz"] == DEFAULT_HYDRO_COEFFS["Cdz"]

    def test_added_mass(self):
        lt_list = _build(_make_pipeline_spec())
        lt = lt_list[0]
        assert lt["Can"] == DEFAULT_HYDRO_COEFFS["Can"]
        assert lt["Caz"] == DEFAULT_HYDRO_COEFFS["Caz"]

    def test_lift_coefficient(self):
        lt_list = _build(_make_pipeline_spec())
        lt = lt_list[0]
        assert lt["Cl"] == DEFAULT_HYDRO_COEFFS["Cl"]


# ---------------------------------------------------------------------------
# Category
# ---------------------------------------------------------------------------

class TestCategory:
    def test_homogeneous_pipe_category(self):
        lt_list = _build(_make_pipeline_spec())
        lt = lt_list[0]
        assert lt["Category"] == "Homogeneous pipe"


# ---------------------------------------------------------------------------
# Multiple segment types
# ---------------------------------------------------------------------------

class TestMultipleSegmentTypes:
    def test_unique_types(self):
        """Each unique segment type generates one LineType entry."""
        lt_list = _build(_make_pipeline_spec(segments=[
            {"type": "X65+CWC80", "length": 500, "segment_length": 5},
            {"type": "X65+CWC120", "length": 300, "segment_length": 5},
            {"type": "X65+CWC80", "length": 200, "segment_length": 5},
        ]))
        names = [lt["Name"] for lt in lt_list]
        assert names == ["X65+CWC80", "X65+CWC120"]

    def test_single_segment_type(self):
        lt_list = _build(_make_pipeline_spec(segments=[
            {"type": "X65+CWC80", "length": 1000, "segment_length": 5},
        ]))
        assert len(lt_list) == 1
        assert lt_list[0]["Name"] == "X65+CWC80"


# ---------------------------------------------------------------------------
# Coating reference
# ---------------------------------------------------------------------------

class TestCoatingReference:
    def test_coating_name_from_context(self):
        """Coating name resolved from VarDataBuilder's registered names."""
        lt_list = _build(
            _make_pipeline_spec(),
            coating_names=["coating+CWC80"],
        )
        assert lt_list[0]["CoatingThickness"] == "coating+CWC80"

    def test_coating_name_fallback(self):
        """Without context coating names, falls back to segment type suffix."""
        lt_list = _build(
            _make_pipeline_spec(segments=[{"type": "X65+3LPP+CWC80", "length": 1000, "segment_length": 5}]),
            coating_names=[],
        )
        assert lt_list[0]["CoatingThickness"] == "3LPP+CWC80"


# ---------------------------------------------------------------------------
# Winch wire line type
# ---------------------------------------------------------------------------

class TestWinchWireLineType:
    def test_winch_wire_added_with_tugs(self):
        """Winch wire LineType is added when tugs are present."""
        lt_list = _build(_make_pipeline_spec(tugs={
            "count": 1,
            "spacing": 100,
            "first_position": [500, -20, 0],
            "properties": {"mass": 30, "volume": 100},
        }))
        names = [lt["Name"] for lt in lt_list]
        assert "Winch wire_LT" in names

    def test_winch_wire_category_general(self):
        lt_list = _build(_make_pipeline_spec(tugs={
            "count": 1,
            "spacing": 100,
            "first_position": [500, -20, 0],
            "properties": {"mass": 30, "volume": 100},
        }))
        winch = [lt for lt in lt_list if lt["Name"] == "Winch wire_LT"][0]
        assert winch["Category"] == "General"

    def test_no_winch_wire_without_tugs(self):
        lt_list = _build(_make_pipeline_spec())
        names = [lt["Name"] for lt in lt_list]
        assert "Winch wire_LT" not in names


# ---------------------------------------------------------------------------
# should_generate
# ---------------------------------------------------------------------------

class TestShouldGenerate:
    def test_pipeline_model_generates(self):
        spec = _make_pipeline_spec()
        builder = LineTypeBuilder(spec, BuilderContext())
        assert builder.should_generate() is True

    def test_generic_model_skips(self):
        spec = ProjectInputSpec(**{
            "metadata": {"name": "t", "description": "t", "structure": "generic", "operation": "generic", "project": "t"},
            "environment": {"water": {"depth": 100}, "seabed": {"stiffness": {"normal": 100, "shear": 50}}},
            "generic": {"line_types": [], "vessels": []},
        })
        builder = LineTypeBuilder(spec, BuilderContext())
        assert builder.should_generate() is False
