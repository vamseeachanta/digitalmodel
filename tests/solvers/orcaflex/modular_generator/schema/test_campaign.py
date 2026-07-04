"""Tests for campaign models."""
from __future__ import annotations

import pytest
from pydantic import ValidationError


def _make_base_spec_data(s_lay: bool = False) -> dict:
    """Create minimal base spec data for testing."""
    data = {
        "metadata": {
            "name": "test_campaign",
            "description": "Test campaign for unit tests",
            "structure": "pipeline",
            "operation": "installation/floating",
            "project": "TEST-001",
        },
        "environment": {
            "water": {"depth": 10, "density": 1.025},
            "seabed": {"slope": 0, "stiffness": {"normal": 1000, "shear": 100}},
            "waves": {"type": "airy", "height": 1.0, "period": 6, "direction": 180},
            "current": {"speed": 0.5, "direction": 270, "profile": [[0, 1.0], [10, 0.5]]},
            "wind": {"speed": 5, "direction": 270},
        },
        "pipeline": {
            "name": "Test Line",
            "material": "X65",
            "dimensions": {"outer_diameter": 0.762, "wall_thickness": 0.0254},
            "coatings": {
                "corrosion": {"thickness": 0.004, "density": 0.95},
                "weight": [{"name": "CWC", "thickness": 0.1, "density": 3.0}],
            },
            "segments": [
                {"type": "X65+CWC", "length": 2000, "segment_length": 5},
                {"type": "X65+CWC", "length": 3000, "segment_length": 5},
            ],
        },
        "equipment": {
            "tugs": {
                "count": 3,
                "spacing": 500,
                "first_position": [500, -20, 0],
                "properties": {"mass": 30, "volume": 100},
            },
            "rollers": {"position": [5, 0, -2], "supports": 4},
            "buoyancy_modules": {
                "spacing": 4,
                "properties": {"volume": 5.0},
            },
        },
        "simulation": {"time_step": 0.1, "stages": [8, 16]},
    }
    if s_lay:
        data["metadata"]["operation"] = "installation/s-lay"
        data["equipment"] = {
            "vessel": {
                "name": "Eclipse",
                "properties": {
                    "loa": 130, "beam": 28, "draft": 4.0, "gmt": 2.5,
                    "cog": [0, 0, 3], "gyration_radii": [10, 30, 30],
                },
            },
            "stinger": {"radius": 150},
            "tensioner": {"capacity_kn": 500, "tension_value": 400},
        }
    return data


def _make_base_spec(s_lay: bool = False):
    from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec
    return ProjectInputSpec(**_make_base_spec_data(s_lay))


class TestEnvironmentVariation:
    def test_valid(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import EnvironmentVariation
        ev = EnvironmentVariation(
            name="storm",
            waves={"type": "jonswap", "height": 3.0, "period": 9, "direction": 180},
            current={"speed": 1.0, "direction": 270},
            wind={"speed": 12, "direction": 270},
        )
        assert ev.name == "storm"

    def test_empty_name_raises(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import EnvironmentVariation
        with pytest.raises(ValidationError):
            EnvironmentVariation(
                name="",
                waves={"type": "airy", "height": 1, "period": 6, "direction": 0},
                current={"speed": 0.5, "direction": 0},
                wind={"speed": 5, "direction": 0},
            )


class TestSoilVariation:
    def test_valid_with_slope(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import SoilVariation
        sv = SoilVariation(
            name="clay",
            stiffness={"normal": 500, "shear": 50},
            friction_coefficient=0.3,
            slope=5.0,
        )
        assert sv.slope == 5.0

    def test_slope_none_by_default(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import SoilVariation
        sv = SoilVariation(
            name="sand",
            stiffness={"normal": 2000, "shear": 200},
            friction_coefficient=0.5,
        )
        assert sv.slope is None


class TestParameterSweep:
    def test_parameter_sweep_valid(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import ParameterSweep
        ps = ParameterSweep(
            parameter="environment.waves.height",
            values=[1.0, 2.0, 3.0],
        )
        assert ps.parameter == "environment.waves.height"
        assert ps.values == [1.0, 2.0, 3.0]

    def test_parameter_sweep_empty_values_rejected(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import ParameterSweep
        with pytest.raises(ValidationError):
            ParameterSweep(parameter="environment.waves.height", values=[])

    def test_parameter_sweep_empty_parameter_rejected(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import ParameterSweep
        with pytest.raises(ValidationError):
            ParameterSweep(parameter="", values=[1.0, 2.0])

    def test_parameter_sweep_dot_terminal_rejected(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import ParameterSweep
        with pytest.raises(ValidationError):
            ParameterSweep(parameter="environment.waves.", values=[1.0, 2.0])


class TestApplyDottedOverride:
    def test_apply_dotted_override_leaf_value_set(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema._overrides import apply_dotted_override
        spec = _make_base_spec()
        assert spec.environment.waves.height == 1.0
        result = apply_dotted_override(spec, "environment.waves.trains.0.height", 5.0)
        assert result.environment.waves.height == 5.0
        assert spec.environment.waves.height == 1.0

    def test_apply_dotted_override_pydantic_validates_type_error(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema._overrides import apply_dotted_override
        spec = _make_base_spec()
        # #535: invalid sweep value is re-raised as a ValueError whose message
        # names the dotted path + offending value, with the original
        # ValidationError preserved on __cause__.
        with pytest.raises(ValueError) as excinfo:
            apply_dotted_override(spec, "environment.waves.trains.0.height", "not-a-float")
        assert "environment.waves.trains.0.height" in str(excinfo.value)
        assert "not-a-float" in str(excinfo.value)
        assert isinstance(excinfo.value.__cause__, ValidationError)

    def test_apply_dotted_override_unresolvable_path_raises(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema._overrides import apply_dotted_override
        spec = _make_base_spec()
        with pytest.raises(KeyError):
            apply_dotted_override(spec, "environment.nonexistent.foo", 1.0)

    def test_apply_dotted_override_list_index_sets_value(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema._overrides import apply_dotted_override
        spec = _make_base_spec()
        assert spec.pipeline.segments[0].length == 2000
        result = apply_dotted_override(spec, "pipeline.segments.0.length", 999.0)
        assert result.pipeline.segments[0].length == 999
        assert spec.pipeline.segments[0].length == 2000

    def test_apply_dotted_override_list_index_out_of_bounds_raises(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema._overrides import apply_dotted_override
        spec = _make_base_spec()
        with pytest.raises(IndexError):
            apply_dotted_override(spec, "pipeline.segments.99.length", 1.0)

    def test_apply_dotted_override_terminal_typo_raises(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema._overrides import apply_dotted_override
        spec = _make_base_spec()
        with pytest.raises(KeyError, match="heigh"):
            apply_dotted_override(spec, "environment.waves.trains.0.heigh", 5.0)


class TestCampaignMatrixCombinations:
    def test_depths_only(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import CampaignMatrix
        m = CampaignMatrix(water_depths=[8, 15, 30])
        combos = m.combinations()
        assert len(combos) == 3
        assert all("water_depth" in c for c in combos)
        assert combos[0]["water_depth"] == 8

    def test_depths_x_environments(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            CampaignMatrix, EnvironmentVariation,
        )
        m = CampaignMatrix(
            water_depths=[8, 15, 30],
            environments=[
                EnvironmentVariation(
                    name="calm",
                    waves={"type": "airy", "height": 0.5, "period": 5, "direction": 180},
                    current={"speed": 0.3, "direction": 270},
                    wind={"speed": 3, "direction": 270},
                ),
                EnvironmentVariation(
                    name="storm",
                    waves={"type": "jonswap", "height": 3.0, "period": 9, "direction": 180},
                    current={"speed": 1.0, "direction": 270},
                    wind={"speed": 12, "direction": 270},
                ),
            ],
        )
        combos = m.combinations()
        assert len(combos) == 6  # 3 x 2
        assert combos[0] == {"water_depth": 8, "environment": "calm"}

    def test_full_cartesian(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            CampaignMatrix, EnvironmentVariation, SoilVariation,
        )
        m = CampaignMatrix(
            water_depths=[10, 20],
            route_lengths=[3000, 5000],
            environments=[
                EnvironmentVariation(
                    name="calm",
                    waves={"type": "airy", "height": 0.5, "period": 5, "direction": 180},
                    current={"speed": 0.3, "direction": 270},
                    wind={"speed": 3, "direction": 270},
                ),
            ],
            soils=[
                SoilVariation(name="clay", stiffness={"normal": 500, "shear": 50}, friction_coefficient=0.3),
            ],
        )
        combos = m.combinations()
        assert len(combos) == 4  # 2 x 2 x 1 x 1

    def test_negative_depth_raises(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import CampaignMatrix
        with pytest.raises(ValidationError):
            CampaignMatrix(water_depths=[-5, 10])

    def test_campaign_matrix_single_sweep_only(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            CampaignMatrix, ParameterSweep,
        )
        m = CampaignMatrix(
            sweeps=[ParameterSweep(parameter="environment.waves.trains.0.height", values=[1.0, 2.0, 3.0])]
        )
        combos = m.combinations()
        assert len(combos) == 3
        assert [c["environment.waves.trains.0.height"] for c in combos] == [1.0, 2.0, 3.0]

    def test_campaign_matrix_sweeps_crossed_with_typed_axis(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            CampaignMatrix, ParameterSweep,
        )
        m = CampaignMatrix(
            water_depths=[10, 20],
            sweeps=[ParameterSweep(parameter="environment.waves.trains.0.height", values=[1.0, 2.0])],
        )
        combos = m.combinations()
        assert len(combos) == 4  # 2 x 2

    def test_campaign_matrix_two_sweeps_crossed(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            CampaignMatrix, ParameterSweep,
        )
        m = CampaignMatrix(
            sweeps=[
                ParameterSweep(parameter="environment.waves.trains.0.height", values=[1.0, 2.0]),
                ParameterSweep(parameter="environment.waves.trains.0.period", values=[6, 8, 10]),
            ]
        )
        combos = m.combinations()
        assert len(combos) == 6  # 2 x 3

    def test_campaign_matrix_no_axes_and_no_sweeps_rejected(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import CampaignMatrix
        with pytest.raises(ValidationError):
            CampaignMatrix()

    def test_sweep_parameter_shadowing_typed_axis_rejected(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            CampaignMatrix, ParameterSweep,
        )
        with pytest.raises(ValidationError):
            CampaignMatrix(
                water_depths=[10, 20],
                sweeps=[ParameterSweep(parameter="water_depth", values=[30, 40])],
            )

    def test_sweep_alias_shadowing_typed_axis_rejected(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            CampaignMatrix, ParameterSweep,
        )
        with pytest.raises(ValidationError):
            CampaignMatrix(
                water_depths=[10, 20],
                sweeps=[ParameterSweep(
                    parameter="environment.waves.trains.0.height",
                    values=[1.0, 2.0],
                    alias="water_depth",
                )],
            )

    def test_backward_compat_no_sweeps_field(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import CampaignMatrix
        m = CampaignMatrix(water_depths=[10, 20])
        combos = m.combinations()
        assert len(combos) == 2
        assert all(set(c.keys()) == {"water_depth"} for c in combos)


class TestApplyOverridesWithSweeps:
    def test_apply_overrides_with_sweep_modifies_spec(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            _apply_overrides, CampaignMatrix, ParameterSweep,
        )
        spec = _make_base_spec()
        matrix = CampaignMatrix(
            sweeps=[ParameterSweep(parameter="environment.waves.trains.0.height", values=[5.0])]
        )
        combo = {"environment.waves.trains.0.height": 5.0}
        result = _apply_overrides(spec, combo, matrix)
        assert result.environment.waves.height == 5.0

    def test_apply_overrides_sweep_applies_after_typed_axis(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            _apply_overrides, CampaignMatrix, ParameterSweep,
        )
        spec = _make_base_spec()
        matrix = CampaignMatrix(
            water_depths=[50],
            sweeps=[ParameterSweep(parameter="environment.water.depth", values=[200])],
        )
        combo = {"water_depth": 50, "environment.water.depth": 200}
        result = _apply_overrides(spec, combo, matrix)
        assert result.environment.water.depth == 200

    def test_apply_overrides_unknown_environment_raises_value_error(self):
        # #534: a hand-built combo naming an environment absent from
        # matrix.environments must raise ValueError, not StopIteration.
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            _apply_overrides, CampaignMatrix, EnvironmentVariation,
        )
        spec = _make_base_spec()
        matrix = CampaignMatrix(
            environments=[
                EnvironmentVariation(
                    name="calm",
                    waves={"type": "airy", "height": 0.5, "period": 5, "direction": 180},
                    current={"speed": 0.3, "direction": 270},
                    wind={"speed": 3, "direction": 270},
                ),
            ],
        )
        with pytest.raises(ValueError, match="environment 'storm' not in matrix.environments"):
            _apply_overrides(spec, {"environment": "storm"}, matrix)

    def test_apply_overrides_unknown_soil_raises_value_error(self):
        # #534: same guard for the soil branch.
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            _apply_overrides, CampaignMatrix, SoilVariation,
        )
        spec = _make_base_spec()
        matrix = CampaignMatrix(
            soils=[
                SoilVariation(name="clay", stiffness={"normal": 500, "shear": 50}, friction_coefficient=0.3),
            ],
        )
        with pytest.raises(ValueError, match="soil 'sand' not in matrix.soils"):
            _apply_overrides(spec, {"soil": "sand"}, matrix)

    def test_validate_output_naming_warns_on_sweep_axis_without_alias(self, caplog):
        import logging
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            CampaignSpec, CampaignMatrix, ParameterSweep,
        )
        base = _make_base_spec()
        caplog.set_level(logging.WARNING)
        CampaignSpec(
            base=base,
            campaign=CampaignMatrix(
                water_depths=[10],
                sweeps=[ParameterSweep(parameter="environment.waves.trains.0.height", values=[1.0, 2.0])]
            ),
            output_naming="{base_name}_wd{water_depth}m",
        )
        assert any("environment.waves.trains.0.height" in r.message for r in caplog.records)


class TestSweepNaming:
    def test_parameter_sweep_alias_accepted(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import ParameterSweep
        ps = ParameterSweep(
            parameter="environment.waves.trains.0.height",
            values=[1.0, 2.0],
            alias="wave_h",
        )
        assert ps.alias == "wave_h"

    def test_sweep_naming_with_alias(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            CampaignSpec, CampaignMatrix, ParameterSweep,
        )
        base = _make_base_spec()
        spec = CampaignSpec(
            base=base,
            campaign=CampaignMatrix(
                water_depths=[10],
                sweeps=[ParameterSweep(
                    parameter="environment.waves.trains.0.height",
                    values=[1.5, 2.5],
                    alias="wave_h",
                )]
            ),
            output_naming="{base_name}_wd{water_depth}m_h{wave_h}",
        )
        names = [name for name, _ in spec.generate_run_specs()]
        assert len(names) == 2
        assert "h1.5" in names[0] and "h2.5" in names[1]

    def test_sweep_naming_slug_fallback(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            CampaignSpec, CampaignMatrix, ParameterSweep,
        )
        base = _make_base_spec()
        spec = CampaignSpec(
            base=base,
            campaign=CampaignMatrix(
                water_depths=[10],
                sweeps=[ParameterSweep(
                    parameter="environment.waves.trains.0.height",
                    values=[1.5],
                )]
            ),
            output_naming="{base_name}_wd{water_depth}m_h{environment-waves-trains-0-height}",
        )
        names = [name for name, _ in spec.generate_run_specs()]
        assert "h1.5" in names[0]

    def test_validate_output_naming_raises_on_aliased_sweep_missing_from_template(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            CampaignSpec, CampaignMatrix, ParameterSweep,
        )
        base = _make_base_spec()
        with pytest.raises(ValidationError, match="alias"):
            CampaignSpec(
                base=base,
                campaign=CampaignMatrix(
                    water_depths=[10],
                    sweeps=[ParameterSweep(
                        parameter="environment.waves.trains.0.height",
                        values=[1.0],
                        alias="wave_h",
                    )]
                ),
                output_naming="{base_name}_wd{water_depth}m",
            )


class TestCampaignSpecValidators:
    def test_tensions_without_slay_raises(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            CampaignSpec, CampaignMatrix,
        )
        base = _make_base_spec(s_lay=False)
        with pytest.raises(ValidationError, match="tensions can only be specified"):
            CampaignSpec(
                base=base,
                campaign=CampaignMatrix(water_depths=[10], tensions=[400, 500]),
                output_naming="{base_name}_wd{water_depth}m_t{tension}kN",
            )

    def test_tensions_with_slay_ok(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            CampaignSpec, CampaignMatrix,
        )
        base = _make_base_spec(s_lay=True)
        spec = CampaignSpec(
            base=base,
            campaign=CampaignMatrix(water_depths=[10], tensions=[400, 500]),
            output_naming="{base_name}_wd{water_depth}m_t{tension}kN",
        )
        assert spec.campaign.tensions == [400, 500]

    def test_output_naming_missing_param_raises(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            CampaignSpec, CampaignMatrix, EnvironmentVariation,
        )
        base = _make_base_spec()
        with pytest.raises(ValidationError, match="environment.*missing from output_naming"):
            CampaignSpec(
                base=base,
                campaign=CampaignMatrix(
                    water_depths=[10],
                    environments=[
                        EnvironmentVariation(
                            name="calm",
                            waves={"type": "airy", "height": 0.5, "period": 5, "direction": 180},
                            current={"speed": 0.3, "direction": 270},
                            wind={"speed": 3, "direction": 270},
                        ),
                    ],
                ),
                output_naming="{base_name}_wd{water_depth}m",  # Missing {environment}
            )

    def test_max_runs_limits_generation(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            CampaignSpec, CampaignMatrix,
        )
        base = _make_base_spec()
        spec = CampaignSpec(
            base=base,
            campaign=CampaignMatrix(water_depths=[8, 15, 30]),
            output_naming="{base_name}_wd{water_depth}m",
            max_runs=2,
        )
        runs = list(spec.generate_run_specs())
        assert len(runs) == 2


class TestGenerateRunSpecs:
    def test_returns_iterator(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            CampaignSpec, CampaignMatrix,
        )
        import types
        base = _make_base_spec()
        spec = CampaignSpec(
            base=base,
            campaign=CampaignMatrix(water_depths=[10, 20]),
            output_naming="{base_name}_wd{water_depth}m",
        )
        result = spec.generate_run_specs()
        assert isinstance(result, types.GeneratorType)

    def test_deep_copy_independence(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            CampaignSpec, CampaignMatrix,
        )
        base = _make_base_spec()
        spec = CampaignSpec(
            base=base,
            campaign=CampaignMatrix(water_depths=[10, 20]),
            output_naming="{base_name}_wd{water_depth}m",
        )
        runs = list(spec.generate_run_specs())
        # Modifying one run should not affect another
        runs[0][1].environment.water.depth = 999
        assert runs[1][1].environment.water.depth == 20
        assert base.environment.water.depth == 10  # Original unchanged

    def test_water_depth_override(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            CampaignSpec, CampaignMatrix,
        )
        base = _make_base_spec()
        spec = CampaignSpec(
            base=base,
            campaign=CampaignMatrix(water_depths=[25]),
            output_naming="{base_name}_wd{water_depth}m",
        )
        name, run_spec = next(spec.generate_run_specs())
        assert run_spec.environment.water.depth == 25

    def test_current_profile_depth_scaling(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            CampaignSpec, CampaignMatrix,
        )
        base = _make_base_spec()
        # Base has depth=10, profile=[[0, 1.0], [10, 0.5]]
        spec = CampaignSpec(
            base=base,
            campaign=CampaignMatrix(water_depths=[20]),
            output_naming="{base_name}_wd{water_depth}m",
        )
        _, run_spec = next(spec.generate_run_specs())
        # Scale factor = 20/10 = 2
        assert run_spec.environment.current.profile[0][0] == 0  # 0 * 2 = 0
        assert run_spec.environment.current.profile[1][0] == 20  # 10 * 2 = 20
        assert run_spec.environment.current.profile[1][1] == 0.5  # factors unchanged

    def test_route_length_adjusts_last_segment(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            CampaignSpec, CampaignMatrix,
        )
        base = _make_base_spec()
        # Base: segments [2000, 3000] = 5000 total
        spec = CampaignSpec(
            base=base,
            campaign=CampaignMatrix(water_depths=[10], route_lengths=[4000]),
            output_naming="{base_name}_wd{water_depth}m_rl{route_length}m",
        )
        _, run_spec = next(spec.generate_run_specs())
        # Delta = 4000 - 5000 = -1000, applied to last: 3000 - 1000 = 2000
        assert run_spec.pipeline.segments[0].length == 2000  # unchanged
        assert run_spec.pipeline.segments[-1].length == 2000

    def test_route_length_negative_last_segment_raises(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            CampaignSpec, CampaignMatrix,
        )
        base = _make_base_spec()
        # Base total = 5000; setting to 1500 makes last = 3000 - 3500 = -500
        spec = CampaignSpec(
            base=base,
            campaign=CampaignMatrix(water_depths=[10], route_lengths=[1500]),
            output_naming="{base_name}_wd{water_depth}m_rl{route_length}m",
        )
        with pytest.raises(ValueError, match="<=0"):
            list(spec.generate_run_specs())

    def test_naming_template(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            CampaignSpec, CampaignMatrix,
        )
        base = _make_base_spec()
        spec = CampaignSpec(
            base=base,
            campaign=CampaignMatrix(water_depths=[15]),
            output_naming="{base_name}_wd{water_depth}m",
        )
        name, _ = next(spec.generate_run_specs())
        assert name == "test_campaign_wd15.0m"
