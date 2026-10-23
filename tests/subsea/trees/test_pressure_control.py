# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Hand-checkable liquid Cv calculations; inputs are arithmetic fixtures."""

import importlib
import math

import pytest


def core():
    return importlib.import_module("digitalmodel.subsea.trees.pressure_control")


def test_linear_and_equal_percentage_trim_and_closed_seat():
    assert core().trim_cv(100, 0.5, "linear") == 50
    assert core().trim_cv(100, 0.5, "equal-percentage", 100) == pytest.approx(10)
    assert core().trim_cv(100, 0, "equal-percentage", 100) == 0
    assert core().trim_cv(100, 1, "equal-percentage", 100) == 100


def test_liquid_valve_losses_add_in_series():
    # SG*(Q/Cv)^2: 1*(20/10)^2 = 4 psi per valve.
    assert core().valve_pressure_drop(20, 10, 1) == 4
    assert core().series_pressure_drop(20, (10, 20), 1) == 5
    assert core().valve_pressure_drop(0, 0, 1) == 0
    with pytest.raises(ValueError, match="closed"):
        core().valve_pressure_drop(1, 0, 1)


def test_liquid_choke_distinguishes_subcritical_and_choked_flow():
    result = core().liquid_choke_flow(100, 96, 10, 1, 25)
    assert result.flow_gpm == 20
    assert result.regime == "subcritical"
    result = core().liquid_choke_flow(100, 50, 10, 1, 25)
    assert result.flow_gpm == 50
    assert result.regime == "critical"
    assert result.effective_drop_psi == 25
    assert core().liquid_choke_flow(100, 75, 10, 1, 25).regime == "critical"


@pytest.mark.parametrize("args", [
    (-1, 0, 1, 1, 1), (1, 2, 1, 1, 1), (1, 0, 1, 0, 1),
    (1, 0, 1, 1, 0), (1, 0, math.nan, 1, 1),
])
def test_invalid_choke_inputs(args):
    with pytest.raises(ValueError):
        core().liquid_choke_flow(*args)


@pytest.mark.parametrize("args", [
    (10, -0.1, "linear"), (10, 1.1, "linear"), (10, 0.5, "unknown"),
    (10, 0.5, "equal-percentage"), (10, 0.5, "equal-percentage", 1),
])
def test_invalid_trim_inputs(args):
    with pytest.raises(ValueError):
        core().trim_cv(*args)


# New slice-1 fixtures below are synthetic hand calculations, not equipment data.
# Source: Fisher, Control Valve Sourcebook — Oil & Gas (2013), ch. 3,
# p. 3-5 (critical differential), p. 3-3 and Table 3-2 (US gpm/psi Cv).


def test_critical_drop_from_supplied_fluid_and_recovery_factors():
    """Hand calculation: FL²(P1-FF*Pv)=0.8²*(100-0.9*10)=58.24 psi.

    All four inputs are chosen arithmetic fixtures, not standard coefficients.
    """
    assert core().liquid_critical_drop(100, 10, 0.8, 0.9) == pytest.approx(58.24)


@pytest.mark.parametrize("downstream_psi", [20, 30])
def test_derived_critical_drop_caps_flow_at_both_outlet_pressures(downstream_psi):
    """Hand calculation: dpcrit=58.24; drops 80/70 exceed it.

    Fisher (2013), ch. 3 pp. 3-3, 3-5: with supplied Cv=10 and SG=0.91,
    Q=10*sqrt(58.24/0.91)=10*sqrt(64)=80 US gpm at either outlet.
    """
    critical = core().liquid_critical_drop(100, 10, 0.8, 0.9)
    result = core().liquid_choke_flow(100, downstream_psi, 10, 0.91, critical)
    assert result.flow_gpm == pytest.approx(80)
    assert result.effective_drop_psi == pytest.approx(58.24)
    assert result.regime == "critical"


def test_derived_limit_preserves_subcritical_flow():
    """Hand calculation: 100-85.44=14.56 < 58.24 psi.

    Fisher (2013), ch. 3 pp. 3-3, 3-5: Q=10*sqrt(14.56/0.91)=40 US gpm.
    Inputs are synthetic; no fluid-property or valve data are inferred.
    """
    critical = core().liquid_critical_drop(100, 10, 0.8, 0.9)
    result = core().liquid_choke_flow(100, 85.44, 10, 0.91, critical)
    assert result.flow_gpm == pytest.approx(40)
    assert result.effective_drop_psi == pytest.approx(14.56)
    assert result.regime == "subcritical"


def test_zero_vapor_pressure_and_critical_onset():
    """Hand calculation: 0.5²*(100-0.9*0)=25 psi; 100-75=25 psi.

    Fisher (2013), ch. 3 pp. 3-3, 3-5: Q=10*sqrt(25/1)=50 US gpm.
    Equality is critical onset under the existing regime convention.
    """
    critical = core().liquid_critical_drop(100, 0, 0.5, 0.9)
    result = core().liquid_choke_flow(100, 75, 10, 1, critical)
    assert critical == 25
    assert result.flow_gpm == 50
    assert result.regime == "critical"


def test_unity_factor_boundaries_and_public_export():
    """Hand calculation: 1²*(100-1*10)=90 psi; unity is a ratio boundary.

    Equation: Fisher (2013), ch. 3 p. 3-5; inputs are synthetic.
    """
    trees = importlib.import_module("digitalmodel.subsea.trees")
    assert trees.liquid_critical_drop(100, 10, 1, 1) == 90


@pytest.mark.parametrize("index", range(4))  # Four required physical inputs.
@pytest.mark.parametrize("value", [math.nan, math.inf, -math.inf])
def test_critical_drop_rejects_nonfinite_inputs(index, value):
    """The numerical domain excludes nonfinite values for every input."""
    args = [100, 10, 0.8, 0.9]  # Synthetic case worked above.
    args[index] = value
    with pytest.raises(ValueError, match="finite"):
        core().liquid_critical_drop(*args)


@pytest.mark.parametrize("args", [
    (0, 0, 0.8, 0.9), (-100, 10, 0.8, 0.9), (100, -10, 0.8, 0.9),
    (100, 100, 0.8, 0.9), (100, 110, 0.8, 0.9),
    (100, 10, 0, 0.9), (100, 10, -0.8, 0.9), (100, 10, 1.1, 0.9),
    (100, 10, 0.8, 0), (100, 10, 0.8, -0.9), (100, 10, 0.8, 1.1),
    (100, 10, 1e-300, 0.9),
])
def test_critical_drop_rejects_invalid_physical_or_underflow_inputs(args):
    """Fisher (2013), ch. 3 pp. 3-2, 3-5: absolute liquid-inlet domain.

    Synthetic probes cross zero/unity ratio boundaries or Pv=P1. The tiny
    recovery factor probes floating-point underflow, not a design threshold.
    """
    with pytest.raises(ValueError):
        core().liquid_critical_drop(*args)


def test_critical_drop_has_no_default_coefficients():
    """Missing fluid/recovery evidence is an input error, never a guessed value."""
    with pytest.raises(TypeError):
        core().liquid_critical_drop(100, 10)


@pytest.mark.parametrize("upstream, recovery, expected", [
    (1e300, 1e-200, 1e-100), (1e300, 1e-300, 1e-300),
])
def test_critical_drop_preserves_representable_extreme_products(
    upstream, recovery, expected,
):
    """Independent exponent arithmetic at Pv=0, FF=1 (synthetic stress cases).

    Fisher (2013), ch. 3 p. 3-5: (10^-200)^2*10^300=10^-100;
    (10^-300)^2*10^300=10^-300. Both final values are representable even
    though squaring FL first underflows. These are numerical probes only.
    """
    actual = core().liquid_critical_drop(upstream, 0, recovery, 1)
    # Binary64 arithmetic tolerance, not an engineering acceptance limit;
    # zero absolute tolerance prevents a zero result passing a tiny-value test.
    assert actual == pytest.approx(expected, rel=1e-12, abs=0)
