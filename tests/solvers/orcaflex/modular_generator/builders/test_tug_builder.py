"""Tests for TugBuilder SRP sub-builder."""
from __future__ import annotations

import pytest


def _make_spec(tug_count=2, spacing=500, first_position=None, mass=30, volume=100, height=None):
    from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec

    props: dict = {"mass": mass, "volume": volume}
    if height is not None:
        props["height"] = height

    data = {
        "metadata": {
            "name": "test",
            "description": "test",
            "structure": "pipeline",
            "operation": "installation/floating",
            "project": "TEST",
        },
        "environment": {
            "water": {"depth": 10, "density": 1.025},
            "seabed": {"slope": 0, "stiffness": {"normal": 1000, "shear": 100}},
        },
        "pipeline": {
            "name": "Test Line",
            "material": "X65",
            "dimensions": {"outer_diameter": 0.3, "wall_thickness": 0.01},
            "coatings": {"corrosion": {"thickness": 0.004, "density": 0.95}},
            "segments": [{"type": "X65", "length": 1000, "segment_length": 5}],
        },
        "equipment": {
            "tugs": {
                "count": tug_count,
                "spacing": spacing,
                "first_position": first_position or [500, -20, 0],
                "properties": props,
            }
        },
        "simulation": {"time_step": 0.1, "stages": [8, 16]},
    }
    return ProjectInputSpec(**data)


def _make_no_tug_spec():
    from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec

    data = {
        "metadata": {
            "name": "test",
            "description": "test",
            "structure": "pipeline",
            "operation": "installation/floating",
            "project": "TEST",
        },
        "environment": {
            "water": {"depth": 10, "density": 1.025},
            "seabed": {"slope": 0, "stiffness": {"normal": 100, "shear": 10}},
        },
        "pipeline": {
            "name": "L1",
            "material": "X65",
            "dimensions": {"outer_diameter": 0.3, "wall_thickness": 0.01},
            "coatings": {"corrosion": {"thickness": 0.003, "density": 1.0}},
            "segments": [{"type": "X65", "length": 500, "segment_length": 5}],
        },
        "equipment": {},
        "simulation": {"time_step": 0.1, "stages": [8]},
    }
    return ProjectInputSpec(**data)


class TestTugBuilderBuild:
    def test_empty_when_no_tugs(self):
        from digitalmodel.solvers.orcaflex.modular_generator.builders.tug_builder import TugBuilder
        from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext

        result = TugBuilder(_make_no_tug_spec(), BuilderContext()).build()
        assert result["buoys"] == []
        assert result["names"] == []

    def test_tug_count_generates_n_buoys(self):
        from digitalmodel.solvers.orcaflex.modular_generator.builders.tug_builder import TugBuilder
        from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext

        result = TugBuilder(_make_spec(tug_count=3), BuilderContext()).build()
        assert len(result["buoys"]) == 3
        assert result["names"] == ["Tug1", "Tug2", "Tug3"]

    def test_tug_positions_spaced(self):
        from digitalmodel.solvers.orcaflex.modular_generator.builders.tug_builder import TugBuilder
        from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext

        spec = _make_spec(tug_count=3, spacing=500, first_position=[100, -20, 0])
        result = TugBuilder(spec, BuilderContext()).build()
        buoys = result["buoys"]
        assert buoys[0]["InitialPosition"] == [100, -20, 0]
        assert buoys[1]["InitialPosition"] == [600, -20, 0]
        assert buoys[2]["InitialPosition"] == [1100, -20, 0]

    def test_tug_has_bulk_modulus_infinity(self):
        from digitalmodel.solvers.orcaflex.modular_generator.builders.tug_builder import TugBuilder
        from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext

        result = TugBuilder(_make_spec(tug_count=1), BuilderContext()).build()
        assert result["buoys"][0]["BulkModulus"] == "Infinity"

    def test_tug_uses_default_moi_when_not_specified(self):
        from digitalmodel.solvers.orcaflex.modular_generator.builders.tug_builder import TugBuilder
        from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext

        result = TugBuilder(_make_spec(tug_count=1), BuilderContext()).build()
        assert result["buoys"][0]["MomentsOfInertia"] == [100, 100, 100]

    def test_tug_sequential_names(self):
        from digitalmodel.solvers.orcaflex.modular_generator.builders.tug_builder import TugBuilder
        from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext

        result = TugBuilder(_make_spec(tug_count=4), BuilderContext()).build()
        assert result["names"] == ["Tug1", "Tug2", "Tug3", "Tug4"]
