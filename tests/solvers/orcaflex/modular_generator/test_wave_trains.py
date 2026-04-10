"""Tests for multi-wave-train support in schema and builder."""

from __future__ import annotations

import logging

import pytest

from digitalmodel.solvers.orcaflex.modular_generator.schema.environment import (
    Waves,
    WaveTrain,
)


# ------------------------------------------------------------------ #
#  Schema: WaveTrain model
# ------------------------------------------------------------------ #

class TestWaveTrain:
    """WaveTrain model tests."""

    def test_defaults(self):
        wt = WaveTrain()
        assert wt.name == "Wave 1"
        assert wt.type == "airy"
        assert wt.height == 0
        assert wt.period == 8
        assert wt.direction == 0
        assert wt.phase == 0
        assert wt.gamma == pytest.approx(3.3)

    def test_custom_values(self):
        wt = WaveTrain(name="Swell", type="jonswap", height=5.0, period=12.0,
                       direction=180, phase=45, gamma=2.5)
        assert wt.name == "Swell"
        assert wt.type == "jonswap"
        assert wt.height == 5.0


# ------------------------------------------------------------------ #
#  Schema: Waves backward compatibility
# ------------------------------------------------------------------ #

class TestWavesBackwardCompat:
    """Flat-field specs must still work after adding trains support."""

    def test_flat_fields_wrapped_into_trains(self):
        """Flat type/height/period create a single-element trains list."""
        w = Waves(type="jonswap", height=3.5, period=10, direction=270)
        assert len(w.trains) == 1
        assert w.trains[0].type == "jonswap"
        assert w.trains[0].height == 3.5
        assert w.trains[0].period == 10
        assert w.trains[0].direction == 270

    def test_property_delegation(self):
        """Properties type/height/period/direction/phase/gamma delegate to trains[0]."""
        w = Waves(type="airy", height=2.0, period=6, direction=90, phase=30, gamma=2.0)
        assert w.type == "airy"
        assert w.height == 2.0
        assert w.period == 6
        assert w.direction == 90
        assert w.phase == 30
        assert w.gamma == pytest.approx(2.0)

    def test_default_waves(self):
        """Default Waves() creates one default train."""
        w = Waves()
        assert len(w.trains) == 1
        assert w.type == "airy"
        assert w.height == 0

    def test_existing_test_compat(self):
        """Mimic existing test: waves.height == 0 for calm sea."""
        w = Waves(height=0)
        assert w.height == pytest.approx(0)


# ------------------------------------------------------------------ #
#  Schema: Multi-wave-train
# ------------------------------------------------------------------ #

class TestWavesMultiTrain:
    """Multi-wave-train specs using the trains list."""

    def test_two_trains(self):
        w = Waves(trains=[
            WaveTrain(name="Wind sea", type="jonswap", height=3.0, period=8, direction=0),
            WaveTrain(name="Swell", type="airy", height=1.5, period=14, direction=180),
        ])
        assert len(w.trains) == 2
        assert w.trains[0].name == "Wind sea"
        assert w.trains[1].name == "Swell"
        # Properties delegate to first train
        assert w.type == "jonswap"
        assert w.height == 3.0

    def test_trains_from_dict(self):
        """Trains list can be provided as raw dicts."""
        w = Waves(**{
            "trains": [
                {"name": "A", "type": "airy", "height": 1.0, "period": 6, "direction": 0},
                {"name": "B", "type": "jonswap", "height": 4.0, "period": 10, "direction": 90},
            ]
        })
        assert len(w.trains) == 2
        assert w.trains[1].type == "jonswap"

    def test_flat_fields_ignored_when_trains_present(self):
        """When 'trains' key is present, flat fields should not be used."""
        w = Waves(**{
            "trains": [{"type": "jonswap", "height": 5.0, "period": 12, "direction": 0}],
        })
        assert w.type == "jonswap"
        assert w.height == 5.0


# ------------------------------------------------------------------ #
#  Builder: multi-train emission
# ------------------------------------------------------------------ #

class TestEnvironmentBuilderMultiTrain:
    """EnvironmentBuilder emits multiple WaveTrains entries."""

    @pytest.fixture
    def _make_spec(self):
        """Helper to build a minimal ProjectInputSpec with wave trains."""
        from digitalmodel.solvers.orcaflex.modular_generator.schema import (
            ProjectInputSpec,
        )

        def _build(wave_data: dict):
            return ProjectInputSpec(**{
                "metadata": {
                    "name": "test",
                    "description": "test",
                    "structure": "pipeline",
                    "operation": "installation",
                    "project": "TEST",
                },
                "environment": {
                    "water": {"depth": 100},
                    "seabed": {"stiffness": {"normal": 100, "shear": 50}},
                    "waves": wave_data,
                },
                "pipeline": {
                    "name": "test_pipe",
                    "material": "X65",
                    "dimensions": {"outer_diameter": 0.3, "wall_thickness": 0.015},
                    "coatings": {
                        "corrosion": {"thickness": 0.003, "density": 1.1},
                    },
                    "segments": [{"type": "test", "length": 1000, "segment_length": 0.5}],
                },
            })

        return _build

    def test_single_train_builder(self, _make_spec):
        """Single train produces one WaveTrains entry."""
        from digitalmodel.solvers.orcaflex.modular_generator.builders.environment_builder import (
            EnvironmentBuilder,
        )

        spec = _make_spec({"type": "airy", "height": 2.0, "period": 8, "direction": 0})
        from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext
        builder = EnvironmentBuilder(spec, BuilderContext())
        result = builder.build()
        trains = result["Environment"]["WaveTrains"]
        assert len(trains) == 1
        assert trains[0]["WaveType"] == "Airy"
        assert trains[0]["WaveHeight"] == 2.0

    def test_multi_train_builder(self, _make_spec):
        """Multiple trains produce multiple WaveTrains entries."""
        from digitalmodel.solvers.orcaflex.modular_generator.builders.environment_builder import (
            EnvironmentBuilder,
        )

        spec = _make_spec({
            "trains": [
                {"name": "Wind sea", "type": "jonswap", "height": 3.0, "period": 8, "direction": 0},
                {"name": "Swell", "type": "airy", "height": 1.5, "period": 14, "direction": 180},
            ]
        })
        from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext
        builder = EnvironmentBuilder(spec, BuilderContext())
        result = builder.build()
        trains = result["Environment"]["WaveTrains"]
        assert len(trains) == 2
        assert trains[0]["WaveType"] == "JONSWAP"
        assert trains[0]["WaveHs"] == 3.0
        assert trains[0]["Name"] == "Wind sea"
        assert trains[1]["WaveType"] == "Airy"
        assert trains[1]["WaveHeight"] == 1.5
        assert trains[1]["WaveDirection"] == 180

    def test_dean_stream_multi_train(self, _make_spec):
        """Dean stream train gets stream function order."""
        from digitalmodel.solvers.orcaflex.modular_generator.builders.environment_builder import (
            EnvironmentBuilder,
        )

        spec = _make_spec({
            "trains": [
                {"name": "Main", "type": "dean_stream", "height": 5.0, "period": 10, "direction": 0},
                {"name": "Secondary", "type": "airy", "height": 1.0, "period": 6, "direction": 90},
            ]
        })
        from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext
        builder = EnvironmentBuilder(spec, BuilderContext())
        result = builder.build()
        trains = result["Environment"]["WaveTrains"]
        assert trains[0]["WaveStreamFunctionOrder"] == 5
        assert "WaveStreamFunctionOrder" not in trains[1]


# ------------------------------------------------------------------ #
#  Cross-validation: current profile depth vs water depth
# ------------------------------------------------------------------ #

class TestCurrentProfileDepthValidation:
    """Current profile depth must not exceed water depth."""

    @staticmethod
    def _make_spec(water_depth: float, profile: list[list[float]]):
        from digitalmodel.solvers.orcaflex.modular_generator.schema import (
            ProjectInputSpec,
        )

        return ProjectInputSpec(**{
            "metadata": {
                "name": "test",
                "description": "test",
                "structure": "pipeline",
                "operation": "installation",
                "project": "TEST",
            },
            "environment": {
                "water": {"depth": water_depth},
                "seabed": {"stiffness": {"normal": 100, "shear": 50}},
                "current": {
                    "speed": 1.0,
                    "direction": 0,
                    "profile": profile,
                },
            },
            "pipeline": {
                "name": "test_pipe",
                "material": "X65",
                "dimensions": {"outer_diameter": 0.3, "wall_thickness": 0.015},
                "coatings": {
                    "corrosion": {"thickness": 0.003, "density": 1.1},
                },
                "segments": [{"type": "test", "length": 1000, "segment_length": 0.5}],
            },
        })

    def test_valid_profile(self):
        # Profile depth == water depth: should pass
        spec = self._make_spec(100, [[0, 1.0], [100, 0.5]])
        assert spec.environment.current.profile[-1][0] == 100

    def test_profile_exceeds_depth_warns(self, caplog):
        """Profile deeper than water depth emits a warning (not an error)."""
        with caplog.at_level(logging.WARNING):
            spec = self._make_spec(50, [[0, 1.0], [80, 0.5]])
        assert spec is not None
        assert "Current profile extends to" in caplog.text

    def test_profile_within_tolerance(self):
        """Profile depth within 1% tolerance of water depth should pass."""
        # 100.5 is within 1% of 100 (tolerance is 101)
        spec = self._make_spec(100, [[0, 1.0], [100.5, 0.5]])
        assert spec is not None
