"""Issue #3843 transmission-line examples; inputs are illustrative, not criteria."""

import json
import math
from dataclasses import asdict

import pytest
from scipy.integrate import quad

from digitalmodel.cathodic_protection import pipeline_attenuation as pa
from digitalmodel.cathodic_protection import stray_current as sc
from digitalmodel.cathodic_protection._experimental import ExperimentalModelError

INPUTS = dict(pipeline_od_m=0.6096, wall_thickness_m=0.0127,
              steel_resistivity_ohm_m=1.7e-7, coating_resistance_ohm_m2=10000.0,
              coating_resistance_basis="Illustrative area-specific resistance")
EA, EMIN = 0.65, 0.30
R = 1.7e-7 / (math.pi * 0.0127 * (0.6096 - 0.0127))
G = math.pi * 0.6096 / 10000.0
ALPHA, Z0 = math.sqrt(R * G), math.sqrt(R / G)


@pytest.fixture
def line():
    return pa.line_parameters(**INPUTS, experimental=True)


def test_parameters_reuse_unrounded_public_kernel(line, monkeypatch):
    assert line.resistance_ohm_per_m == pytest.approx(R, rel=1e-9)
    assert line.conductance_S_per_m == pytest.approx(G, rel=1e-9)
    assert line.alpha_per_m == pytest.approx(ALPHA, rel=1e-9)
    assert line.characteristic_resistance_ohm == pytest.approx(Z0, rel=1e-9)
    assert line.characteristic_length_m == pytest.approx(1 / ALPHA, rel=1e-9)
    original, calls = sc.pipe_attenuation_constant, []

    def spy(*args, **kwargs):
        calls.append((args, kwargs))
        return original(*args, **kwargs)

    monkeypatch.setattr(sc, "pipe_attenuation_constant", spy)
    assert pa.line_parameters(**INPUTS, experimental=True) == line
    assert calls == [((0.6096, 0.0127, 10000.0, 1.7e-7), {"experimental": True})]


@pytest.mark.parametrize("opt_in", [{}, {"experimental": False}])
def test_experimental_opt_in_is_preserved(opt_in):
    with pytest.raises(ExperimentalModelError) as error:
        pa.line_parameters(**INPUTS, **opt_in)
    assert error.value.model == "stray_current.pipe_attenuation_constant"


def test_infinite_profile_decay(line):
    for x in (0.0, 1 / ALPHA, 5 / ALPHA):
        result = pa.infinite_profile(line, EA, x)
        expected = EA * math.exp(-ALPHA * x)
        assert result.shift_V == pytest.approx(expected, rel=1e-9)
        assert result.one_sided_current_A == pytest.approx(expected / Z0, rel=1e-9)


def test_finite_profile_and_independent_leakage_balance(line):
    length = 12345.0  # Independently chosen, not derived from spacing.
    for x in (0.0, length / 3, length):
        result = pa.insulated_end_profile(line, EA, x, length)
        expected = EA * math.cosh(ALPHA * (length - x)) / math.cosh(ALPHA * length)
        current = EA / Z0 * math.sinh(ALPHA * (length - x)) / math.cosh(ALPHA * length)
        assert result.shift_V == pytest.approx(expected, rel=1e-9)
        assert result.one_sided_current_A == pytest.approx(current, rel=1e-9)
    assert pa.insulated_end_profile(line, EA, 0, length).shift_V == EA
    assert pa.insulated_end_profile(line, EA, length, length).one_sided_current_A == 0
    leakage, _ = quad(lambda x: G * EA * math.cosh(ALPHA * (length - x))
                     / math.cosh(ALPHA * length), 0, length, epsabs=1e-12)
    assert pa.insulated_end_profile(line, EA, 0, length).one_sided_current_A == pytest.approx(
        leakage, rel=1e-9)


def test_spacing_example_midpoint_and_half_span_leakage(line):
    result = pa.drain_point_spacing(line, EA, EMIN)
    assert line.alpha_per_m == pytest.approx(3.697e-5, rel=0.005)
    assert line.characteristic_resistance_ohm == pytest.approx(0.1931, rel=0.005)
    assert result.spacing_m == pytest.approx(76200.0, rel=0.005)
    assert result.interior_drain_current_A == pytest.approx(5.97, rel=0.005)
    assert result.no_overlap_span_m == pytest.approx(41800.0, rel=0.005)
    assert result.spacing_m == pytest.approx(2 * math.acosh(EA / EMIN) / ALPHA, rel=1e-9)
    assert result.no_overlap_span_m == pytest.approx(2 * math.log(EA / EMIN) / ALPHA, rel=1e-9)
    length = result.spacing_m / 2
    assert pa.insulated_end_profile(line, EA, length, length).shift_V == pytest.approx(EMIN, rel=1e-9)
    leakage, _ = quad(lambda x: G * pa.insulated_end_profile(line, EA, x, length).shift_V,
                     0, length, epsabs=1e-12)
    side = pa.insulated_end_profile(line, EA, 0, length).one_sided_current_A
    assert side == pytest.approx(leakage, rel=1e-9)
    assert result.interior_drain_current_A == pytest.approx(2 * leakage, rel=1e-9)


def test_equal_shifts_are_degenerate_limit(line):
    result = pa.drain_point_spacing(line, EA, EA)
    assert (result.spacing_m, result.interior_drain_current_A, result.no_overlap_span_m) == (0, 0, 0)
    result = pa.insulated_end_profile(line, EA, 0, 0)
    assert (result.shift_V, result.one_sided_current_A) == (EA, 0)


def test_smallest_shift_preserves_drain_boundary(line):
    result = pa.insulated_end_profile(line, 5e-324, 0, 0)
    assert result.shift_V == 5e-324
    assert result.one_sided_current_A == 0


@pytest.mark.parametrize("key", ["pipeline_od_m", "wall_thickness_m",
                                  "steel_resistivity_ohm_m", "coating_resistance_ohm_m2"])
@pytest.mark.parametrize("value", [0.0, -1.0, math.nan, math.inf, -math.inf])
def test_invalid_properties(key, value):
    with pytest.raises(pa.AttenuationValidationError):
        pa.line_parameters(**(INPUTS | {key: value}), experimental=True)


@pytest.mark.parametrize("update", [dict(wall_thickness_m=0.6096 / 2),
                                    dict(wall_thickness_m=0.6096),
                                    dict(coating_resistance_basis="  "),
                                    dict(coating_resistance_basis=None)])
def test_geometry_and_basis_required(update):
    with pytest.raises(pa.AttenuationValidationError):
        pa.line_parameters(**(INPUTS | update), experimental=True)


@pytest.mark.parametrize("call", [
    lambda p: pa.infinite_profile(p, EA, -1),
    lambda p: pa.infinite_profile(p, 0, 0),
    lambda p: pa.infinite_profile(p, math.inf, 0),
    lambda p: pa.infinite_profile(p, EA, math.nan),
    lambda p: pa.insulated_end_profile(p, EA, 2, 1),
    lambda p: pa.insulated_end_profile(p, EA, -1, 1),
    lambda p: pa.insulated_end_profile(p, EA, 0, -1),
    lambda p: pa.insulated_end_profile(p, EA, 0, math.inf),
    lambda p: pa.drain_point_spacing(p, EA, EA + 0.1),
    lambda p: pa.drain_point_spacing(p, EA, 0),
    lambda p: pa.drain_point_spacing(p, -EA, EMIN),
    lambda p: pa.drain_point_spacing(p, math.nan, EMIN),
])
def test_profile_and_spacing_validation(line, call):
    with pytest.raises(pa.AttenuationValidationError):
        call(line)


def test_extreme_lengths_and_nonfinite_outputs(line):
    length = 1e308  # alpha*L is finite; hyperbolic evaluation would overflow.
    result = pa.insulated_end_profile(line, EA, 0, length)
    assert result.shift_V == EA
    assert result.one_sided_current_A == pytest.approx(EA / Z0, rel=1e-9)
    assert pa.insulated_end_profile(line, EA, length, length).one_sided_current_A == 0
    assert pa.infinite_profile(line, EA, length).shift_V == 0
    tiny = pa.insulated_end_profile(line, EA, 0, 1e-8)
    assert tiny.one_sided_current_A == pytest.approx(G * EA * 1e-8, rel=1e-9, abs=0)
    with pytest.raises(pa.AttenuationValidationError):
        pa.infinite_profile(line, 1e308, 0)
    with pytest.raises(pa.AttenuationValidationError):
        pa.line_parameters(**(INPUTS | {"coating_resistance_ohm_m2": 5e-324}), experimental=True)


def test_serialized_screening_context(line):
    results = [line, pa.infinite_profile(line, EA, 1),
               pa.insulated_end_profile(line, EA, 0, 1), pa.drain_point_spacing(line, EA, EMIN)]
    for result in results:
        data = json.loads(json.dumps(asdict(result), allow_nan=False))
        assert data["status"] == "screening_only"
        assert data["assumptions"] == ["uniform coating", "constant linear leakage", "remote earth"]
        assert "#3843" in data["equation_basis"]
        assert "passes" not in data
        properties = data if result is line else data["line"]
        assert all(properties[key] == value for key, value in INPUTS.items())
