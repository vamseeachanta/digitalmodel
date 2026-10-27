"""Independent analytic fixtures for the three blade-passage harmonics.

Basis: cos(x + phi) = cos(phi) cos(x) - sin(phi) sin(x), so the
amplitude is hypot(cosine coefficient, sine coefficient) and phi is
atan2(-sine coefficient, cosine coefficient). All pressures, phases and
sampling grids below are prescribed synthetic choices, not measured data.
The tolerances allow floating-point least-squares roundoff only.
"""

import numpy as np
import pytest

from digitalmodel.solvers.openfoam.pressure_harmonics import fit_blade_harmonics


def test_exact_pressure_harmonics():
    """Hand calculation: coefficient pairs (3,4), (-5,12), (8,-15)."""
    # At 120 RPM and five blades: (120 / 60) * 5 = 10 Hz.
    # The three Pythagorean pairs give amplitudes 5, 13 and 17 Pa.
    times = np.linspace(0.013, 0.387, 301)  # Nonintegral number of cycles.
    frequencies = np.array([10.0, 20.0, 30.0])
    cosine = np.array([3.0, -5.0, 8.0])
    sine = np.array([4.0, 12.0, -15.0])
    phases = np.arctan2(-sine, cosine)
    amplitudes = np.sqrt(cosine**2 + sine**2)
    pressure = 101.0 + np.sum(
        amplitudes * np.cos(2 * np.pi * times[:, None] * frequencies + phases),
        axis=1,
    )

    result = fit_blade_harmonics(times, pressure, rpm=120.0, blade_count=5)

    assert result.mean_pa == pytest.approx(101.0, abs=1e-10)
    assert result.blade_passage_hz == pytest.approx(10.0)
    np.testing.assert_allclose(result.frequencies_hz, frequencies)
    np.testing.assert_allclose(result.amplitudes_pa, [5.0, 13.0, 17.0], atol=1e-10)
    np.testing.assert_allclose(result.phases_rad, phases, atol=1e-10)


def test_rotational_units_and_phase_translation():
    """Changing t to u=t-delta adds omega*delta to cosine phase."""
    # 90 RPM = 1.5 revolutions/s; four blades give 6, 12, 18 Hz.
    times = np.linspace(0.0, 0.73, 211)
    frequencies = np.array([6.0, 12.0, 18.0])
    amplitudes = np.array([2.0, 3.0, 4.0])
    phases = np.array([0.2, -0.4, 0.7])
    delta = 0.17  # Chosen to exercise phase wrapping across pi.
    angles = 2 * np.pi * times[:, None] * frequencies
    # Independent expansion of cos(omega*t + phi).
    pressure = 9.0 + np.sum(
        amplitudes * (np.cos(phases) * np.cos(angles)
                      - np.sin(phases) * np.sin(angles)),
        axis=1,
    )
    original = fit_blade_harmonics(times, pressure, rpm=90.0, blade_count=4)
    translated = fit_blade_harmonics(
        times - delta, pressure, rpm=90.0, blade_count=4
    )
    expected = phases + 2 * np.pi * frequencies * delta
    expected = np.arctan2(np.sin(expected), np.cos(expected))

    assert original.blade_passage_hz == pytest.approx(6.0)
    np.testing.assert_allclose(original.frequencies_hz, frequencies)
    np.testing.assert_allclose(original.phases_rad, phases, atol=1e-10)
    np.testing.assert_allclose(translated.phases_rad, expected, atol=1e-10)
    np.testing.assert_allclose(translated.amplitudes_pa, amplitudes, atol=1e-10)
    assert translated.mean_pa == pytest.approx(9.0, abs=1e-10)


def test_nonuniform_samples_and_array_like_inputs():
    """A nonuniform grid still fits a known-frequency linear model."""
    times = np.linspace(0.0, 1.0, 101) ** 2
    pressure = 7.0 + 3.0 * np.sin(2 * np.pi * 4.0 * times)
    result = fit_blade_harmonics(
        times.tolist(), pressure.tolist(), rpm=60.0, blade_count=np.int64(4)
    )
    assert result.mean_pa == pytest.approx(7.0, abs=1e-10)
    np.testing.assert_allclose(result.amplitudes_pa, [3.0, 0.0, 0.0], atol=1e-10)
    assert result.phases_rad[0] == pytest.approx(-np.pi / 2, abs=1e-10)


@pytest.mark.parametrize("blade_count", [0, -1, 2.5, 4.0, True, "4", None])
def test_invalid_blade_count(blade_count):
    """A blade count is a positive integer, excluding booleans."""
    times = np.linspace(0.0, 1.0, 101)
    with pytest.raises(ValueError, match="blade_count"):
        fit_blade_harmonics(times, times, rpm=60.0, blade_count=blade_count)


@pytest.mark.parametrize("rpm", [0.0, -1.0, np.nan, np.inf, -np.inf, True, "60"])
def test_invalid_rpm(rpm):
    """Rotational speed must be numeric, finite and positive."""
    times = np.linspace(0.0, 1.0, 101)
    with pytest.raises(ValueError, match="rpm"):
        fit_blade_harmonics(times, times, rpm=rpm, blade_count=4)


@pytest.mark.parametrize(
    "times",
    [np.arange(10)[::-1], np.zeros(10), [0, 1, 2, 3, 3, 5, 6, 7]],
)
def test_nonmonotonic_time(times):
    """Reversed and duplicate time samples are explicitly rejected."""
    with pytest.raises(ValueError, match="strictly increasing"):
        fit_blade_harmonics(times, np.ones(len(times)), rpm=60.0, blade_count=4)


@pytest.mark.parametrize("times", [np.arange(6), np.arange(10) / 4])
def test_rank_deficient_samples(times):
    """Seven coefficients need seven independent columns, not just samples.

    The second grid samples at one blade-passage period: all cosine columns
    coincide with the mean and all sine columns vanish (4 Hz at 60 RPM).
    """
    with pytest.raises(ValueError, match="rank|seven"):
        fit_blade_harmonics(times, np.ones(len(times)), rpm=60.0, blade_count=4)


@pytest.mark.parametrize(
    "times, pressure",
    [
        ([], []),
        (np.ones((8, 1)), np.ones(8)),
        (np.arange(8), np.ones((8, 1))),
        (np.arange(8), np.ones(9)),
        ([0, 1, 2, 3, 4, 5, 6, np.nan], np.ones(8)),
        ([0, 1, 2, 3, 4, 5, 6, np.inf], np.ones(8)),
        (np.arange(8), [0, 1, 2, 3, 4, 5, 6, np.nan]),
        (np.arange(8), [0, 1, 2, 3, 4, 5, 6, np.inf]),
    ],
)
def test_invalid_trace(times, pressure):
    """Only matching finite one-dimensional time and pressure vectors fit."""
    with pytest.raises(ValueError, match="samples|one-dimensional|length|finite"):
        fit_blade_harmonics(times, pressure, rpm=60.0, blade_count=4)
