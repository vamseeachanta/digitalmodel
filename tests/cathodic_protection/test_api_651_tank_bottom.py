"""Hand calculations for a screening model, not an API 651 qualification."""

import math
from pathlib import Path
import runpy

import pytest
from pydantic import ValidationError

from digitalmodel.cathodic_protection.api_651_tank_bottom import (
    TankBottomInput,
    assess_tank_bottom,
    check_center_edge_polarization,
    conductor_voltage_drop,
    ribbon_grid,
)


def inputs(**changes):
    values = dict(
        diameter_m=40, current_density_mA_m2=25, ribbon_output_A_m=0.047,
        ribbon_spacing_m=1.8, max_spacing_m=2.0,
        ribbon_resistance_ohm_m=0.1, ribbon_feed_span_m=8,
        busbar_resistance_ohm_m=0.01, busbar_feed_length_m=20,
        busbar_current_A=10, foundation_resistivity_ohm_m=100,
        max_foundation_resistivity_ohm_m=200,
    )
    return TankBottomInput(**(values | changes))


def test_worked_example():
    result = assess_tank_bottom(inputs())
    # pi * 20^2 = 1256.637061 m2; *25/1000 = 31.41592654 A.
    # 31.41592654 / 0.047 = 668.4239688 m (assumed rating, not API).
    assert result.area_m2 == pytest.approx(1256.6370614)
    assert result.current_demand_A == pytest.approx(31.41592654)
    assert result.minimum_ribbon_length_m == pytest.approx(668.4239688)
    assert result.installed_ribbon_length_m > result.minimum_ribbon_length_m
    assert result.ribbon_capacity_meets_demand
    assert result.spacing_within_input_limit
    assert result.busbar_voltage_drop_V == pytest.approx(2.0)
    assert any("polarization" in warning for warning in result.warnings)
    assert result.method_source.section == "section 5.11, printed pp128-131"


def test_executable_example_readback():
    example = Path(__file__).resolve().parents[2] / "examples/cathodic_protection/api_651_tank_bottom.py"
    result = runpy.run_path(str(example))["worked_example"]()
    assert result["data_status"].startswith("synthetic")
    assert round(result["sizing"]["minimum_ribbon_length_m"], 1) == 668.4
    assert result["synthetic_potential_check"]["both_locations_meet_criterion"]


def test_grid_chords_and_units():
    # D=4 m, s=2 m -> chord rows at y=+-1 m, each 2*sqrt(3) m.
    grid = ribbon_grid(4, 2)
    assert grid.offsets_m == pytest.approx((-1, 1))
    assert grid.chord_lengths_m == pytest.approx((2 * math.sqrt(3),) * 2)
    assert grid.total_length_m == pytest.approx(4 * math.sqrt(3))
    assert grid.nominal_spacing_m == 2
    assert ribbon_grid(4, 2, directions=2).total_length_m == pytest.approx(8 * math.sqrt(3))
    assert ribbon_grid(4, 10).chord_lengths_m == (4,)


def test_feed_topology():
    # 2 A, 10 m, 0.1 ohm/m: lumped end load I*r*L=2 V.
    # Uniform one-end load integrates a triangle: 1 V. Both ends: 0.25 V.
    assert conductor_voltage_drop(2, 10, 0.1, loading="end_load") == 2
    assert conductor_voltage_drop(2, 10, 0.1, loading="uniform_one_end") == 1
    assert conductor_voltage_drop(2, 10, 0.1, loading="uniform_both_ends") == 0.25
    assert conductor_voltage_drop(0, 10, 0) == 0
    with pytest.raises(ValueError):
        conductor_voltage_drop(2, 10, 0.1, loading="unknown")


@pytest.mark.parametrize("args", [(-1, 10, 1), (1, 0, 1), (1, 10, -1), (1, 10, float("nan"))])
def test_invalid_conductor_inputs(args):
    with pytest.raises(ValueError):
        conductor_voltage_drop(*args)


def test_grid_resource_guard_and_input_units():
    with pytest.raises(ValueError):
        ribbon_grid(40, 1e-300)
    with pytest.raises(ValidationError):
        inputs(diameter_mm=40_000)


def test_grid_rejects_unrepresentable_geometry():
    for diameter, spacing in ((1e308, 1e-308), (1e-308, 1e308)):
        with pytest.raises(ValueError):
            ribbon_grid(diameter, spacing)


def test_adverse_foundation_and_capacity():
    result = assess_tank_bottom(inputs(
        ribbon_output_A_m=0.001, ribbon_spacing_m=4,
        foundation_resistivity_ohm_m=300, dry_sand=True,
        oil_contaminated_sand=True, voids_present=True, anodes_above_liner=False,
    ))
    assert not result.ribbon_capacity_meets_demand
    assert not result.spacing_within_input_limit
    for word in ("resistivity", "dry", "oil", "void", "liner"):
        assert any(word in warning.lower() for warning in result.warnings)


@pytest.mark.parametrize("field", [
    "diameter_m", "current_density_mA_m2", "ribbon_output_A_m",
    "ribbon_spacing_m", "max_spacing_m", "ribbon_feed_span_m",
    "busbar_feed_length_m", "foundation_resistivity_ohm_m",
])
@pytest.mark.parametrize("value", [0, -1, float("nan"), float("inf")])
def test_invalid_si_inputs(field, value):
    with pytest.raises(ValidationError):
        inputs(**{field: value})


@pytest.mark.parametrize("args", [(0, 1), (1, 0), (float("nan"), 1)])
def test_bad_grid(args):
    with pytest.raises(ValueError):
        ribbon_grid(*args)


def test_measured_center_edge_check_and_missing_evidence():
    result = check_center_edge_polarization(-0.90, -0.95, criterion_V=-0.85)
    assert result.center_meets_criterion and result.edge_meets_criterion
    assert result.center_minus_edge_V == pytest.approx(0.05)
    assert not check_center_edge_polarization(-0.80, -0.95, criterion_V=-0.85).center_meets_criterion
    result = check_center_edge_polarization(None, -0.95, criterion_V=-0.85)
    assert result.center_meets_criterion is None
    assert not result.both_locations_meet_criterion
    with pytest.raises(ValueError, match="IR-corrected"):
        check_center_edge_polarization(-1.2, -1.2, criterion_V=-0.85, ir_corrected=False)


def test_center_edge_in_survey_electrode():
    result = check_center_edge_polarization(
        -0.834, -0.884, criterion_V=-0.85, reference_electrode="Ag/AgCl/seawater",
    )
    assert result.criterion_V == pytest.approx(-0.784)
    assert result.center_meets_criterion and result.edge_meets_criterion

