"""Tests for GeneralBuilder property mapping."""
from __future__ import annotations

import pytest

from digitalmodel.solvers.orcaflex.modular_generator.builders.context import (
    BuilderContext,
)
from digitalmodel.solvers.orcaflex.modular_generator.builders.general_builder import (
    GeneralBuilder,
)
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _make_spec(simulation=None, generic=None):
    """Create a minimal pipeline spec with configurable simulation."""
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
        "pipeline": {
            "name": "test_pipe",
            "material": "X65",
            "dimensions": {"outer_diameter": 0.3, "wall_thickness": 0.015},
            "coatings": {"corrosion": {"thickness": 0.003, "density": 1.1}},
            "segments": [{"type": "test", "length": 1000, "segment_length": 0.5}],
        },
    }
    if simulation is not None:
        data["simulation"] = simulation
    if generic is not None:
        # Generic and pipeline are mutually exclusive; swap to generic model
        del data["pipeline"]
        data["metadata"]["structure"] = "generic"
        data["generic"] = generic
    return ProjectInputSpec(**data)


def _build(spec):
    """Run the builder and return the General dict."""
    builder = GeneralBuilder(spec, BuilderContext())
    result = builder.build()
    return result["General"]


# ---------------------------------------------------------------------------
# Stage durations
# ---------------------------------------------------------------------------

class TestStageDurations:
    def test_default_stages(self):
        gen = _build(_make_spec())
        assert gen["StageDuration"] == [8, 16]

    def test_custom_stages(self):
        gen = _build(_make_spec(simulation={"time_step": 0.1, "stages": [4, 8, 20]}))
        assert gen["StageDuration"] == [4, 8, 20]

    def test_single_stage(self):
        gen = _build(_make_spec(simulation={"time_step": 0.1, "stages": [10]}))
        assert gen["StageDuration"] == [10]


# ---------------------------------------------------------------------------
# Time step
# ---------------------------------------------------------------------------

class TestTimeStep:
    def test_default_time_step(self):
        gen = _build(_make_spec())
        assert gen["ImplicitConstantTimeStep"] == 0.1
        assert gen["TargetLogSampleInterval"] == 0.1

    def test_custom_time_step(self):
        gen = _build(_make_spec(simulation={"time_step": 0.05, "stages": [8, 16]}))
        assert gen["ImplicitConstantTimeStep"] == 0.05
        assert gen["TargetLogSampleInterval"] == 0.05


# ---------------------------------------------------------------------------
# Dynamics solution method
# ---------------------------------------------------------------------------

class TestDynamicsSolution:
    def test_implicit_time_domain(self):
        gen = _build(_make_spec())
        assert gen["DynamicsSolutionMethod"] == "Implicit time domain"

    def test_variable_time_step_disabled(self):
        gen = _build(_make_spec())
        assert gen["ImplicitUseVariableTimeStep"] is False


# ---------------------------------------------------------------------------
# Units system
# ---------------------------------------------------------------------------

class TestUnitsSystem:
    def test_default_si(self):
        gen = _build(_make_spec())
        assert gen["UnitsSystem"] == "SI"

    def test_non_si_units_from_generic(self):
        """Non-SI unit system emits unit sub-properties."""
        gen = _build(_make_spec(generic={
            "general_properties": {
                "UnitsSystem": "User",
                "LengthUnits": "ft",
                "MassUnits": "lbm",
                "ForceUnits": "lbf",
                "g": 32.174,
            },
            "line_types": [],
            "vessels": [],
        }))
        assert gen["UnitsSystem"] == "User"
        assert gen["LengthUnits"] == "ft"
        assert gen["MassUnits"] == "lbm"
        assert gen["ForceUnits"] == "lbf"
        assert gen["g"] == 32.174

    def test_si_units_no_sub_properties(self):
        """SI unit system should not emit LengthUnits/MassUnits/etc."""
        gen = _build(_make_spec())
        assert "LengthUnits" not in gen
        assert "MassUnits" not in gen
        assert "ForceUnits" not in gen


# ---------------------------------------------------------------------------
# North direction
# ---------------------------------------------------------------------------

class TestNorthDirection:
    def test_default_no_north_direction(self):
        """Default north_direction=0 should not be emitted."""
        gen = _build(_make_spec())
        assert "NorthDirection" not in gen

    def test_nonzero_north_direction(self):
        gen = _build(_make_spec(simulation={"time_step": 0.1, "stages": [8, 16], "north_direction": 45}))
        assert gen["NorthDirection"] == 45


# ---------------------------------------------------------------------------
# Other general properties
# ---------------------------------------------------------------------------

class TestOtherProperties:
    def test_statics_policies(self):
        gen = _build(_make_spec())
        assert gen["BuoysIncludedInStatics"] == "Individually specified"
        assert gen["LineStaticsStep1Policy"] == "All lines included"
        assert gen["LineStaticsStep2Policy"] == "Solve coupled systems"
        assert gen["WholeSystemStaticsEnabled"] is True

    def test_log_precision(self):
        gen = _build(_make_spec())
        assert gen["LogPrecision"] == "Single"

    def test_jacobian_settings(self):
        gen = _build(_make_spec())
        assert gen["JacobianBufferingPolicy"] == 1
        assert gen["JacobianPerturbationFactor"] == 0
