"""Validation tests for the regular-wave numerical wave tank (#1170).

ALWAYS (no OpenFOAM): the wave-theory helpers return known values and the
case builder produces a structurally valid interFoam wave case.

The historical fast NWT configuration is outside the sourced absorption
range and is now rejected before a solve. A replacement solver benchmark
requires separate qualification; the theory reference remains available.
"""

from __future__ import annotations

import json
import math

import pytest

from digitalmodel.solvers.openfoam.validation import (
    WAVE_TANK_CASE,
    WaveTankConfig,
    build_wave_tank_case,
    celerity,
    dispersion_wavenumber,
    reflection_coefficient,
    wavelength,
)
from digitalmodel.solvers.openfoam.validation.wave_tank import (
    GRAVITY,
)

from .conftest import assert_valid_case_dir


# --------------------------------------------------------------------------- #
#  Wave-theory helpers — always run                                           #
# --------------------------------------------------------------------------- #


def test_dispersion_satisfies_relation() -> None:
    period, depth = 3.0, 0.4
    k = dispersion_wavenumber(period, depth)
    om = 2 * math.pi / period
    # omega^2 = g k tanh(k d) holds at the returned k.
    assert om**2 == pytest.approx(GRAVITY * k * math.tanh(k * depth), rel=1e-10)
    # Known value for the verified case: k ~= 1.0899 rad/m (lambda ~= 5.77 m).
    assert k == pytest.approx(1.0899, rel=1e-3)
    assert wavelength(period, depth) == pytest.approx(2 * math.pi / k)
    assert celerity(period, depth) == pytest.approx(om / k)


def test_dispersion_deep_water_limit() -> None:
    # kd >> 1: omega^2 -> g k, i.e. k -> omega^2/g.
    period = 2.0
    om = 2 * math.pi / period
    assert dispersion_wavenumber(period, 500.0) == pytest.approx(
        om * om / GRAVITY, rel=1e-6
    )


def test_dispersion_shallow_water_limit() -> None:
    # kd << 1: c -> sqrt(g d).
    depth = 0.05
    c = celerity(60.0, depth)
    assert c == pytest.approx(math.sqrt(GRAVITY * depth), rel=0.01)


@pytest.mark.parametrize("bad", [0.0, -1.0])
def test_dispersion_rejects_nonpositive(bad: float) -> None:
    with pytest.raises(ValueError):
        dispersion_wavenumber(bad, 0.4)
    with pytest.raises(ValueError):
        dispersion_wavenumber(3.0, bad)


def test_reflection_coefficient_recovers_synthetic_split() -> None:
    import numpy as np

    k = 1.09
    xs = [8.0, 8.8, 9.6, 10.4, 11.2, 12.0, 12.8]
    a_i, a_r = 0.025, 0.005  # Kr = 0.2
    amps = [a_i * np.exp(-1j * k * x) + a_r * np.exp(1j * k * x) for x in xs]
    kr, ai, ar = reflection_coefficient(xs, amps, k)
    assert kr == pytest.approx(0.2, rel=1e-6)
    assert ai == pytest.approx(a_i, rel=1e-6)
    assert ar == pytest.approx(a_r, rel=1e-6)


def test_reflection_coefficient_requires_three_gauges() -> None:
    with pytest.raises(ValueError):
        reflection_coefficient([1.0, 2.0], [1 + 0j, 1 + 0j], 1.0)


def test_validation_case_registered() -> None:
    assert WAVE_TANK_CASE.metadata["issue"] == "#1170"
    assert WAVE_TANK_CASE.within_tolerance(
        "wavenumber", WAVE_TANK_CASE.reference["wavenumber"]
    )


# --------------------------------------------------------------------------- #
#  Config + builder — always run                                              #
# --------------------------------------------------------------------------- #


def test_config_wave_properties_consistent() -> None:
    cfg = WaveTankConfig()
    assert cfg.wavelength == pytest.approx(2 * math.pi / cfg.wavenumber)
    # tank spans ~3.5 wavelengths
    assert 3.0 <= cfg.tank_length / cfg.wavelength <= 4.0
    # the 7-gauge reflection array spans ~0.8 wavelengths
    arr = [x for x in cfg.gauges_x if 7.9 <= x <= 12.9]
    assert len(arr) == 7
    assert (arr[-1] - arr[0]) == pytest.approx(4.8)


@pytest.fixture
def shallow_config():
    """Hand calculation: h=0.4 m, q=kh=pi/20 gives L=2*pi*h/q=16 m.

    Higuera (2018), section 3, eq. (2): T=2*pi*sqrt(h/(g*q*tanh(q))).
    Table 1: q is half the shallow-water limit. These synthetic inputs
    test file generation only, without claiming solver performance.
    """
    depth, q = 0.4, math.pi / 20
    period = 2 * math.pi * math.sqrt(depth / (GRAVITY * q * math.tanh(q)))
    return WaveTankConfig(wave_period=period, depth=depth)


def test_build_produces_valid_wave_case(tmp_path, shallow_config) -> None:
    case_dir = build_wave_tank_case(shallow_config, parent_dir=tmp_path)
    assert_valid_case_dir(case_dir)
    control = (case_dir / "system" / "controlDict").read_text()
    assert "interFoam" in control
    assert "interfaceHeight" in control  # wave gauges
    waveprops = (case_dir / "constant" / "waveProperties").read_text()
    assert "StokesII" in waveprops
    assert "shallowWaterAbsorption" in waveprops
    assert "activeAbsorption yes" in waveprops
    fields = (case_dir / "0" / "U").read_text()
    assert "waveVelocity" in fields
    assert (case_dir / "0" / "alpha.water").is_file()


def test_build_stamps_provenance_with_citation(tmp_path, shallow_config) -> None:
    case_dir = build_wave_tank_case(shallow_config, parent_dir=tmp_path)
    prov = json.loads((case_dir / "provenance.json").read_text())
    assert prov["issue"] == "#1170"
    assert any("stokesII" in c for c in prov["citations"])
    assert any("1804.01158" in c for c in prov["citations"])
    # L=16 m from the independent fixture calculation; default roundoff tolerance.
    assert prov["wave"]["wavelength_m"] == pytest.approx(16)


def test_historical_solver_configuration_requires_new_absorber(tmp_path) -> None:
    """The old solver benchmark cannot run with its out-of-range absorber.

    Higuera (2018), Table 1: kh < pi/10. For the historical configuration,
    at q=pi/10, g*q*tanh(q)/h < omega squared. Monotonic q*tanh(q)
    therefore puts the actual solution above that limit. Preserve the
    reference inputs and reject, rather than assert solver quality for
    an unqualified replacement configuration.
    """
    cfg = WaveTankConfig()
    q = math.pi / 10
    assert GRAVITY * q * math.tanh(q) / cfg.depth < (2 * math.pi / cfg.wave_period)**2
    with pytest.raises(ValueError, match="shallowWaterAbsorption"):
        build_wave_tank_case(cfg, parent_dir=tmp_path)
