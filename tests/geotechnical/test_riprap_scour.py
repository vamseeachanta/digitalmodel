# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Synthetic checks for the Yang et al. (2026) steady-current equations."""

import math

import pytest

from digitalmodel.geotechnical.scour import (
    approach_bed_shear_stress,
    critical_shear_stress,
    dimensionless_grain_size,
    grain_chezy_coefficient,
    riprap_residual_scour_depth,
    riprap_scour_from_dimensionless,
)


def inputs():
    return dict(
        pile_diameter_m=0.1,
        water_depth_m=0.3,
        current_velocity_ms=0.4,
        sediment_d50_m=0.0003,
        sediment_d90_m=0.0006,
        rock_d50_m=0.005,
        sediment_density_kg_m3=2650.0,
        rock_density_kg_m3=2250.0,
        water_density_kg_m3=1000.0,
        kinematic_viscosity_m2_s=1.01e-6,
    )


def test_shear_and_grain_units():
    assert grain_chezy_coefficient(0.3, 0.0006) == pytest.approx(59.44999454)
    assert approach_bed_shear_stress(
        0.4, 0.3, 0.0003, 1000, sediment_d90_m=0.0006
    ) == pytest.approx(
        0.4441046822
    )
    assert critical_shear_stress(0.005, 2250, 1000) == pytest.approx(2.7590625)
    assert dimensionless_grain_size(0.0003, 2650, 1000, 1.01e-6) == pytest.approx(
        7.538610807
    )
    # Larger supplied roughness increases approach shear.
    assert (
        approach_bed_shear_stress(0.4, 0.3, 0.0003, 1000, sediment_d90_m=0.0012)
        > 0.4441046822
    )


def test_stage_two_synthetic_value():
    result = riprap_scour_from_dimensionless(0.5, 7.5, 100, 0.4)
    assert result.scour_to_diameter_ratio == pytest.approx(0.2768747773)
    assert result.in_calibration_range
    assert result.out_of_range_inputs == ()


def test_physical_chain_and_density_inputs():
    result = riprap_residual_scour_depth(**inputs())
    assert result.shear_amplification == pytest.approx(9.674585854)
    assert result.approach_shear_pa == pytest.approx(0.4441046822)
    assert result.peak_shear_pa == pytest.approx(4.2965288757)
    assert result.critical_shear_pa == pytest.approx(2.7590625)
    assert result.excess_shear == pytest.approx(0.5572423153)
    assert result.scour_depth_m == pytest.approx(result.scour_to_diameter_ratio * 0.1)
    assert result.in_calibration_range
    changed = riprap_residual_scour_depth(**(inputs() | {"rock_density_kg_m3": 2300}))
    assert changed.excess_shear < result.excess_shear
    assert changed.rock_d_star > result.rock_d_star
    assert changed.scour_depth_m < result.scour_depth_m


@pytest.mark.parametrize("velocity", [0, 0.01])
def test_below_motion_is_unavailable_not_zero(velocity):
    result = riprap_residual_scour_depth(
        **(inputs() | {"current_velocity_ms": velocity})
    )
    assert result.excess_shear <= 0
    assert not result.in_calibration_range
    assert result.scour_depth_m is None
    assert result.scour_to_diameter_ratio is None
    assert "excess_shear" in result.out_of_range_inputs
    assert "incipient motion" in result.limitation


def test_prototype_rock_is_not_extrapolated():
    result = riprap_residual_scour_depth(
        **(
            inputs()
            | dict(
                pile_diameter_m=8,
                water_depth_m=20,
                current_velocity_ms=2,
                rock_d50_m=0.3,
                rock_density_kg_m3=2650,
                water_density_kg_m3=1025,
                kinematic_viscosity_m2_s=1.3e-6,
            )
        )
    )
    assert result.rock_d_star > 6000
    assert "rock_d_star" in result.out_of_range_inputs
    assert not result.in_calibration_range
    assert result.scour_depth_m is None


@pytest.mark.parametrize(
    "index,low,high,name",
    [
        (0, 0.04, 3.60, "excess_shear"),
        (1, 3.77, 23.87, "sediment_d_star"),
        (2, 35.87, 1339.64, "rock_d_star"),
        (3, 0.15, 0.80, "pile_depth_ratio"),
    ],
)
def test_each_calibration_boundary(index, low, high, name):
    for boundary in (low, high):
        args = [0.5, 7.5, 100, 0.4]
        args[index] = boundary
        assert riprap_scour_from_dimensionless(*args).in_calibration_range
    for outside in (low * (1 - 1e-6), high * (1 + 1e-6)):
        args[index] = outside
        result = riprap_scour_from_dimensionless(*args)
        assert result.out_of_range_inputs == (name,)
        assert result.scour_to_diameter_ratio is None


@pytest.mark.parametrize("name", list(inputs()))
@pytest.mark.parametrize("bad", [math.nan, math.inf, -math.inf, True, "1"])
def test_reject_invalid_physical_inputs(name, bad):
    with pytest.raises(ValueError, match=name):
        riprap_residual_scour_depth(**(inputs() | {name: bad}))


@pytest.mark.parametrize("name", [n for n in inputs() if n != "current_velocity_ms"])
@pytest.mark.parametrize("bad", [0, -1])
def test_positive_inputs(name, bad):
    with pytest.raises(ValueError):
        riprap_residual_scour_depth(**(inputs() | {name: bad}))


def test_invalid_physics_and_roughness():
    for change in (
        {"current_velocity_ms": -1},
        {"rock_density_kg_m3": 999},
        {"sediment_density_kg_m3": 1000},
        {"sediment_d90_m": 0},
    ):
        with pytest.raises(ValueError):
            riprap_residual_scour_depth(**(inputs() | change))
    with pytest.raises(ValueError):
        grain_chezy_coefficient(0.001, 0.01)


@pytest.mark.parametrize("index", range(4))
@pytest.mark.parametrize("bad", [math.nan, math.inf, True, "1"])
def test_dimensionless_input_validation(index, bad):
    args = [0.5, 7.5, 100, 0.4]
    args[index] = bad
    with pytest.raises(ValueError):
        riprap_scour_from_dimensionless(*args)


def test_workflow_optional_riprap_block_and_json_null(tmp_path):
    import json
    from digitalmodel.geotechnical.cfg_workflows import ScourWorkflow

    cfg = {"scour": {"riprap": inputs(), "outputs": {"directory": str(tmp_path)}}}
    output = ScourWorkflow().router(cfg)["scour"]
    assert output["results"]["riprap"]["scour_depth_m"] > 0
    written = json.loads((tmp_path / "scour_summary.json").read_text())
    assert written["results"]["riprap"]["source"].endswith("105302")
    assert written["results"]["riprap"]["sediment_d90_m"] == 0.0006
    assert written["results"]["riprap"]["d90_source"] == "supplied"
    assert written["results"]["riprap"]["critical_shields"] == 0.045
    cfg["scour"]["riprap"]["current_velocity_ms"] = 0
    output = ScourWorkflow().router(cfg)["scour"]
    assert output["results"]["riprap"]["scour_depth_m"] is None
    written = json.loads((tmp_path / "scour_summary.json").read_text())
    assert written["results"]["riprap"]["scour_depth_m"] is None
    assert not written["results"]["riprap"]["in_calibration_range"]


def test_workflow_without_riprap_retains_existing_result(tmp_path):
    from digitalmodel.geotechnical.cfg_workflows import ScourWorkflow

    cfg = {
        "scour": {
            "monopile": {
                "pile_diameter_m": 6,
                "current_velocity_ms": 1,
                "water_depth_m": 30,
            },
            "outputs": {"directory": str(tmp_path)},
        }
    }
    result = ScourWorkflow().router(cfg)["scour"]["results"]
    assert set(result) == {"monopile"}
    assert result["monopile"]["scour_depth_m"] == pytest.approx(7.8)


def test_checked_in_example(tmp_path):
    from pathlib import Path

    import yaml

    from digitalmodel.geotechnical.cfg_workflows import ScourWorkflow

    example = (
        Path(__file__).resolve().parents[2]
        / "examples/workflows/scour-assessment/input.yml"
    )
    cfg = yaml.safe_load(example.read_text())
    cfg["scour"]["outputs"]["directory"] = str(tmp_path)
    results = ScourWorkflow().router(cfg)["scour"]["results"]
    assert results["pipeline"]["scour_depth_m"] == pytest.approx(0.225)
    assert results["monopile"]["scour_depth_m"] == pytest.approx(7.8)
    assert results["riprap"]["in_calibration_range"]
    assert results["riprap"]["scour_depth_m"] == pytest.approx(0.0289772316)


def test_explicit_roughness_is_required():
    physical = inputs()
    physical.pop("sediment_d90_m")
    with pytest.raises(TypeError, match="sediment_d90_m"):
        riprap_residual_scour_depth(**physical)
    with pytest.raises(TypeError, match="sediment_d90_m"):
        approach_bed_shear_stress(0.4, 0.3, 0.0003, 1000)


def test_explicit_none_roughness_is_rejected():
    with pytest.raises(ValueError, match="sediment_d90_m must be supplied"):
        riprap_residual_scour_depth(**(inputs() | {"sediment_d90_m": None}))
    with pytest.raises(ValueError, match="sediment_d90_m must be supplied"):
        approach_bed_shear_stress(0.4, 0.3, 0.0003, 1000, sediment_d90_m=None)


def test_critical_shear_uses_explicit_shields_and_gravity():
    # 0.05 * (2250 - 1000) * 10 * 0.005 = 3.125 Pa.
    assert critical_shear_stress(
        0.005, 2250, 1000, critical_shields=0.05, gravity_ms2=10.0
    ) == pytest.approx(3.125)


@pytest.mark.parametrize("particle_density", [900.0, 1000.0])
def test_critical_shear_density_error_names_public_parameters(particle_density):
    with pytest.raises(
        ValueError,
        match=r"^particle_density_kg_m3 must exceed water_density_kg_m3$",
    ):
        critical_shear_stress(0.005, particle_density, 1000.0)


@pytest.mark.parametrize("gravity", [0, -1, math.nan, math.inf, True, "10"])
def test_critical_shear_rejects_invalid_gravity(gravity):
    with pytest.raises(ValueError, match="gravity_ms2"):
        critical_shear_stress(0.005, 2250, 1000, gravity_ms2=gravity)


def test_critical_shear_rejects_unrepresentable_threshold():
    with pytest.raises(ValueError, match="representable"):
        critical_shear_stress(1e200, 1e200, 1000)


@pytest.mark.parametrize("d90", [0.0006, 0.0012])
def test_roughness_and_shields_are_recorded(d90):
    result = riprap_residual_scour_depth(**(inputs() | {"sediment_d90_m": d90}))
    assert result.sediment_d90_m == d90
    assert result.d90_source == "supplied"
    assert result.critical_shields == 0.045
    assert result.approach_shear_pa == pytest.approx(
        approach_bed_shear_stress(0.4, 0.3, 0.0003, 1000, sediment_d90_m=d90)
    )
