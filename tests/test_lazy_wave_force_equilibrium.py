"""First-principles hang-off force checks for issue #2044."""

import math
from pathlib import Path

import pytest
import yaml

from digitalmodel.marine_ops.marine_analysis.catenary.lazy_wave import (
    LazyWaveConfiguration,
    LazyWaveSolver,
)

from digitalmodel.subsea.catenary_riser.legacy.catenaryMethods import (
    catenaryEquation,
    lazyWaveCatenaryEquation,
)


def test_legacy_vertical_force_excess_is_horizontal_force_pin():
    """Regression pin: legacy Fv exceeds w*S by Fh under issue #2044.

    Vertical equilibrium from the vertex gives Fv=w*S. Both legacy parity
    tests use this 15 degree, 350 m hang-off span and 1000 N/m submerged weight.
    """
    data = {
        "HangOff": {"q": 15.0, "d": 350.0, "F": None, "X": None, "w": 1000.0},
        "VerticalDistance": 500.0,
        "HangoffBelowMeanSeaLevel": 50.0,
        "HogBendAboveSeabed": 300.0,
        "SagBendElevationAboveSeabed": 150.0,
        "WeightPerUnitLengthWithOutBuoyancy": 1000.0,
        "WeightPerUnitLengthWithBuoyancy": -500.0,
    }
    q = math.radians(data["HangOff"]["q"])
    d, w = data["HangOff"]["d"], data["HangOff"]["w"]
    suspended_length = d * math.cos(q) / (1.0 - math.sin(q))
    horizontal_force = w * d * math.sin(q) / (1.0 - math.sin(q))
    vertical_equilibrium = w * suspended_length
    legacy_hangoff = catenaryEquation(data["HangOff"])
    data["HangOff"].update(legacy_hangoff)
    legacy_result = lazyWaveCatenaryEquation(data)
    legacy_vertical_force = legacy_result["Summary"]["Fv"]
    excess = legacy_vertical_force - vertical_equilibrium
    print(f"w={w}, S={suspended_length}, w*S={vertical_equilibrium}, "
          f"Fh={horizontal_force}, legacy_Fv={legacy_vertical_force}, excess={excess}")
    assert math.isclose(legacy_hangoff["S"], suspended_length, rel_tol=1e-9)
    assert math.isclose(legacy_result["Summary"]["Fh"], horizontal_force, rel_tol=1e-9)
    assert math.isclose(excess, horizontal_force, rel_tol=1e-9)


REFERENCE_CASES = yaml.safe_load(
    (Path(__file__).parent / "fixtures/test_vectors/mooring_risers/lazy_wave_reference_cases.yaml")
    .read_text()
)["cases"]


@pytest.mark.parametrize("case", REFERENCE_CASES, ids=lambda case: case["id"])
def test_hangoff_force_triangle_recovers_declination(case):
    """The hang-off force triangle reproduces the input angle from vertical."""
    result = LazyWaveSolver().solve(LazyWaveConfiguration(**case["inputs"]))
    angle = math.degrees(math.atan(result.horizontal_force / result.vertical_force))
    assert math.isclose(angle, case["inputs"]["hangoff_angle"], rel_tol=1e-9)


@pytest.mark.parametrize("case", REFERENCE_CASES, ids=lambda case: case["id"])
def test_hangoff_vertical_force_balances_suspended_weight(case):
    """Vertical equilibrium uses the fixture's independently retained length."""
    result = LazyWaveSolver().solve(LazyWaveConfiguration(**case["inputs"]))
    expected = case["inputs"]["weight_without_buoyancy"] * case["expected"]["hangoff_S"]
    assert math.isclose(result.vertical_force, expected, rel_tol=1e-12)
    assert math.isclose(result.summary["Fv"], expected, rel_tol=1e-12)


@pytest.mark.parametrize("case", REFERENCE_CASES, ids=lambda case: case["id"])
def test_hangoff_total_tension_matches_catenary_and_force_triangle(case):
    """Total tension satisfies both the catenary identity and force magnitude."""
    result = LazyWaveSolver().solve(LazyWaveConfiguration(**case["inputs"]))
    expected = case["inputs"]["weight_without_buoyancy"] * (
        case["expected"]["hangoff_bend_radius"] + case["expected"]["hangoff_d"]
    )
    assert math.isclose(result.total_tension, expected, rel_tol=1e-9)
    assert math.isclose(
        result.total_tension, math.hypot(result.horizontal_force, result.vertical_force),
        rel_tol=1e-9,
    )
    assert math.isclose(result.summary["T"], expected, rel_tol=1e-9)


@pytest.mark.parametrize("case", REFERENCE_CASES, ids=lambda case: case["id"])
def test_reference_vertical_force_is_recomputed_from_weight_and_length(case):
    """The corrected reference force is the suspended weight under issue #2044."""
    expected = case["inputs"]["weight_without_buoyancy"] * case["expected"]["hangoff_S"]
    assert math.isclose(case["expected"]["Fv"], expected, rel_tol=1e-12)
