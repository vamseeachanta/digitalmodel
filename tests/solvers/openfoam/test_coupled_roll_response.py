# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Hand impedance checks for a synthetic linear roll oscillator.

All numeric inputs here are chosen synthetic examples, not calibrated data.
The independent reference is substitution into
I*theta_ddot + B*theta_dot + C*theta = Re(F*exp(i*omega*t)):
Z = C - I*omega**2 + i*omega*B and Theta = F/Z.
The default example has I=2, C=10, B=2, omega=2 and F=1, hence
Z0=2+4i and Theta0=(2-4i)/20. Tank K and B add to C and B.
Support limits and reference amplitude are synthetic declarations only.
"""

from dataclasses import replace
import math

import pytest

from digitalmodel.solvers.openfoam.sloshing_coupling_analysis import (
    coupled_roll_response,
)
from digitalmodel.solvers.openfoam.sloshing_coupling_models import (
    MomentCoefficients,
    NormalizedRollCoefficients,
)


@pytest.fixture
def coefficients():
    return NormalizedRollCoefficients(
        stiffness=0.0,
        damping=0.0,
        omega_rad_s=2.0,
        reference_amplitude_rad=0.2,
        frequency_support_rad_s=(1.0, 3.0),
        amplitude_support_rad=(0.0, 1.0),
        condition="synthetic tank at fixed fill",
        normalization="per_roll_angle_and_rate",
        stiffness_unit="N.m/rad",
        damping_unit="N.m/(rad/s)",
    )


def solve(coefficients, **overrides):
    inputs = dict(
        inertia_kg_m2=2.0,
        restoring_nm_per_rad=10.0,
        damping_nm_s_per_rad=2.0,
        omega_rad_s=2.0,
        excitation_moment_nm=1.0,
        coefficients=coefficients,
    )
    inputs.update(overrides)
    return coupled_roll_response(**inputs)


def test_zero_tank_effect(coefficients):
    result = solve(coefficients)
    expected = complex(2.0, -4.0) / 20.0
    assert result.baseline_response_rad == pytest.approx(expected)
    assert result.coupled_response_rad == pytest.approx(expected)
    assert result.baseline_amplitude_rad == pytest.approx(1.0 / math.sqrt(20.0))
    assert result.coupled_amplitude_rad == result.baseline_amplitude_rad
    assert result.reduction_percent == pytest.approx(0.0)


def test_positive_damping_reduction(coefficients):
    # Tank B=2: Zc=2+8i, Theta_c=(2-8i)/68, |Theta_c|=1/sqrt(68).
    result = solve(replace(coefficients, damping=2.0))
    assert result.coupled_response_rad == pytest.approx(complex(2.0, -8.0) / 68.0)
    assert result.coupled_amplitude_rad == pytest.approx(1.0 / math.sqrt(68.0))
    expected_reduction = 100.0 * (1.0 - math.sqrt(20.0 / 68.0))
    assert result.reduction_percent == pytest.approx(expected_reduction)
    assert result.reduction_percent > 0.0


def test_stiffness_shift_amplification(coefficients):
    # C=4: Z0=-4+4i. Tank K=4 brings Zc to 4i, closer to resonance.
    result = solve(replace(coefficients, stiffness=4.0), restoring_nm_per_rad=4.0)
    assert result.baseline_response_rad == pytest.approx(complex(-4.0, -4.0) / 32.0)
    assert result.coupled_response_rad == pytest.approx(-0.25j)
    assert result.reduction_percent == pytest.approx(100.0 * (1.0 - math.sqrt(2.0)))
    assert result.reduction_percent < 0.0


def test_resonance(coefficients):
    # C=I*omega^2=8, B=2: Z0=4i. Tank B=2 gives Zc=8i.
    result = solve(replace(coefficients, damping=2.0), restoring_nm_per_rad=8.0)
    assert result.baseline_response_rad == pytest.approx(-0.25j)
    assert result.coupled_response_rad == pytest.approx(-0.125j)
    assert result.reduction_percent == pytest.approx(50.0)


@pytest.mark.parametrize("target", ["baseline", "coupled"])
def test_undamped_resonance_refused(coefficients, target):
    # Baseline resonance C=8; coupled resonance C=10, K=-2, B=0.
    tank = replace(coefficients, stiffness=0.0 if target == "baseline" else -2.0)
    with pytest.raises(ValueError, match="singular.*resonance"):
        solve(tank, restoring_nm_per_rad=8.0 if target == "baseline" else 10.0,
              damping_nm_s_per_rad=0.0)


@pytest.mark.parametrize("omega", [0.5, 3.5])
def test_outside_support_refused(coefficients, omega):
    with pytest.raises(ValueError, match="frequency.*support"):
        solve(replace(coefficients, omega_rad_s=omega), omega_rad_s=omega)


def test_frequency_sample_mismatch_refused(coefficients):
    with pytest.raises(ValueError, match="evaluated frequency"):
        solve(coefficients, omega_rad_s=2.5)


def test_outside_amplitude_support_refused(coefficients):
    # |Theta_c|=1/sqrt(20) exceeds the declared upper bound 0.2 rad.
    tank = replace(coefficients, amplitude_support_rad=(0.0, 0.2))
    with pytest.raises(ValueError, match="coupled amplitude.*support"):
        solve(tank)


def test_below_amplitude_support_refused(coefficients):
    # |Theta_c|=1/sqrt(20) is below the declared lower bound 0.3 rad.
    tank = replace(coefficients, reference_amplitude_rad=0.4,
                   amplitude_support_rad=(0.3, 1.0))
    with pytest.raises(ValueError, match="coupled amplitude.*support"):
        solve(tank)


@pytest.mark.parametrize("target", ["tank", "baseline"])
def test_negative_damping_refused(coefficients, target):
    with pytest.raises(ValueError, match="damping.*nonnegative"):
        solve(replace(coefficients, damping=-1.0) if target == "tank" else coefficients,
              damping_nm_s_per_rad=-1.0 if target == "baseline" else 2.0)


@pytest.mark.parametrize("raw", [
    {"in_phase_coeff": 1.0, "quad_coeff": 2.0},
    MomentCoefficients(omega=2.0, fill_level=0.5, in_phase_coeff=1.0, quad_coeff=2.0),
])
def test_raw_unnormalized_records_refused(raw):
    with pytest.raises(TypeError, match="NormalizedRollCoefficients"):
        solve(raw)


@pytest.mark.parametrize("updates", [
    {"normalization": "raw_first_harmonic"},
    {"stiffness_unit": "N.m"},
    {"damping_unit": "N.m"},
    {"condition": " "},
    {"frequency_support_rad_s": (3.0, 1.0)},
    {"frequency_support_rad_s": (0.0, 3.0)},
    {"amplitude_support_rad": (-1.0, 1.0)},
    {"amplitude_support_rad": (1.0, 0.0)},
    {"reference_amplitude_rad": 0.0},
    {"reference_amplitude_rad": 2.0},
])
def test_invalid_coefficient_declarations_refused(coefficients, updates):
    with pytest.raises(ValueError):
        solve(replace(coefficients, **updates))


@pytest.mark.parametrize("field", [
    "inertia_kg_m2", "restoring_nm_per_rad", "damping_nm_s_per_rad",
    "omega_rad_s", "excitation_moment_nm",
])
@pytest.mark.parametrize("value", [float("nan"), float("inf"), -float("inf")])
def test_nonfinite_inputs_refused(coefficients, field, value):
    with pytest.raises(ValueError, match="finite"):
        solve(coefficients, **{field: value})


@pytest.mark.parametrize("field", [
    "stiffness", "damping", "omega_rad_s", "reference_amplitude_rad",
    "frequency_support_rad_s", "amplitude_support_rad",
])
@pytest.mark.parametrize("value", [float("nan"), float("inf"), -float("inf")])
def test_nonfinite_coefficients_refused(coefficients, field, value):
    update = (1.0, value) if "support" in field else value
    with pytest.raises(ValueError, match="finite"):
        solve(replace(coefficients, **{field: update}))


@pytest.mark.parametrize("field", ["inertia_kg_m2", "restoring_nm_per_rad", "omega_rad_s"])
@pytest.mark.parametrize("value", [0.0, -1.0])
def test_nonpositive_physical_inputs_refused(coefficients, field, value):
    with pytest.raises(ValueError, match="positive"):
        solve(coefficients, **{field: value})


def test_zero_excitation_refused(coefficients):
    with pytest.raises(ValueError, match="nonzero"):
        solve(coefficients, excitation_moment_nm=0.0)


@pytest.mark.parametrize("stiffness", [-10.0, -11.0])
def test_nonpositive_total_restoring_refused(coefficients, stiffness):
    with pytest.raises(ValueError, match="total restoring.*positive"):
        solve(replace(coefficients, stiffness=stiffness))


def test_complex_excitation_and_provenance(coefficients):
    # F=i rotates (2-4i)/20 into (4+2i)/20 without changing amplitude.
    result = solve(coefficients, excitation_moment_nm=1j)
    assert result.baseline_response_rad == pytest.approx(complex(4.0, 2.0) / 20.0)
    assert result.coupled_response_rad == result.baseline_response_rad
    assert result.coefficients == coefficients
    assert result.omega_rad_s == 2.0
    assert result.excitation_moment_nm == 1j
    assert result.inertia_kg_m2 == 2.0
    assert result.restoring_nm_per_rad == 10.0
    assert result.damping_nm_s_per_rad == 2.0
    assert result.support_status == "within_declared_support"
    assert result.amplitude_basis == "computed_with_fixed_coefficients"


def test_nonfinite_complex_excitation_refused(coefficients):
    with pytest.raises(ValueError, match="finite"):
        solve(coefficients, excitation_moment_nm=complex(1.0, float("inf")))


def test_support_boundary_inclusive(coefficients):
    # |Theta_c|=1/sqrt(20) is admissible at an exact support boundary.
    amplitude = abs(complex(2.0, -4.0) / 20.0)
    tank = replace(coefficients, reference_amplitude_rad=amplitude,
                   amplitude_support_rad=(amplitude, amplitude),
                   frequency_support_rad_s=(2.0, 2.0))
    assert solve(tank).support_status == "within_declared_support"


def test_negative_tank_stiffness_supported(coefficients):
    # K=-2 gives Zc=4i while total restoring remains positive at C+K=8.
    result = solve(replace(coefficients, stiffness=-2.0))
    assert result.coupled_response_rad == pytest.approx(-0.25j)
    assert result.reduction_percent < 0.0


def test_small_positive_damping_at_resonance_supported(coefficients):
    # C=8 gives zero real impedance; B=1e-12 and F=1e-12 yield Theta=-0.5i.
    result = solve(coefficients, restoring_nm_per_rad=8.0,
                   damping_nm_s_per_rad=1e-12, excitation_moment_nm=1e-12)
    assert result.coupled_response_rad == pytest.approx(-0.5j)


@pytest.mark.parametrize("field", ["frequency_support_rad_s", "amplitude_support_rad"])
@pytest.mark.parametrize("bounds", [(), (1.0,), (1.0, 2.0, 3.0), [1.0, 3.0], None])
def test_malformed_support_refused(coefficients, field, bounds):
    with pytest.raises(ValueError, match="pair of finite bounds"):
        solve(replace(coefficients, **{field: bounds}))


@pytest.mark.parametrize("value", [True, "2.0", 1j, None])
def test_nonreal_domain_refused(coefficients, value):
    with pytest.raises(ValueError, match="finite real scalar"):
        solve(coefficients, inertia_kg_m2=value)


@pytest.mark.parametrize("value", [True, "1.0", None])
def test_nonnumeric_excitation_refused(coefficients, value):
    with pytest.raises(ValueError, match="finite complex scalar"):
        solve(coefficients, excitation_moment_nm=value)


def test_impedance_overflow_refused(coefficients):
    # Finite I=1e308 overflows I*omega^2 for the default omega=2.
    with pytest.raises(ValueError, match="impedance.*finite"):
        solve(coefficients, inertia_kg_m2=1e308)


def test_total_damping_overflow_refused(coefficients):
    # The sum of two finite damping contributions 1e308 is unrepresentable.
    with pytest.raises(ValueError, match="total damping.*finite"):
        solve(replace(coefficients, damping=1e308), damping_nm_s_per_rad=1e308)


def test_response_underflow_refused(coefficients):
    # The smallest positive subnormal force divided by |Z|>1 rounds to zero.
    with pytest.raises(ValueError, match="representable and nonzero"):
        solve(coefficients, excitation_moment_nm=math.ulp(0.0))
