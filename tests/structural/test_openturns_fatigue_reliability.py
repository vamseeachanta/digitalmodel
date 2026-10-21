# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Independent constant-amplitude reliability checks.

Synthetic inputs below are mathematical examples, not material properties.
NIST/SEMATECH e-Handbook (online edition accessed 2026), sections 1.3.6.6.9
and 1.3.6.6.1, supplies the lognormal moments and normal CDF:
https://www.itl.nist.gov/div898/handbook/eda/section3/eda3669.htm
https://www.itl.nist.gov/div898/handbook/eda/section3/eda3661.htm

Numerical comparisons allow 1e-12 relative/absolute roundoff; this is a test
precision choice, not an engineering acceptance limit. Invalid-input examples
exercise the mathematical domains in those same handbook sections.
"""

import builtins
import math
import subprocess
import sys

import pytest

from digitalmodel.structural.fatigue_reliability import FatigueReliability


def model(**overrides):
    """Synthetic baseline: A=e^2, m=2, E[S]=1, Var[ln A]=1."""
    inputs = dict(
        A=math.exp(2),
        m=2,
        stress_range=1,
        cov_stress=0,
        cov_sn=math.sqrt(math.expm1(1)),
    )
    inputs.update(overrides)
    return FatigueReliability(**inputs)


@pytest.mark.parametrize("beta", [2, 0, -2])
def test_hand_calculation_intercept_uncertainty(beta):
    """ln A ~ N(1.5,1); ln S=0; choose ln Nd=1.5-beta.

    Thus mu_g=beta, sigma_g=1, and Pf=erfc(beta/sqrt(2))/2.
    The erfc identity is the integral of the NIST normal PDF (section 1.3.6.6.1),
    evaluated independently by the standard library rather than OpenTURNS.
    """
    result = model()
    life = math.exp(1.5 - beta)
    assert result.reliability_index(life) == pytest.approx(beta, abs=1e-12)


@pytest.mark.parametrize("beta", [2, 0, -2])
def test_intercept_uncertainty_probability(beta):
    pytest.importorskip("openturns")
    result = model()
    life = math.exp(1.5 - beta)
    expected = math.erfc(beta / math.sqrt(2)) / 2
    assert result.failure_probability(life) == pytest.approx(
        expected, rel=1e-12, abs=1e-12
    )


def test_hand_calculation_both_uncertainties():
    """A=e^2, E[S]=e, both log variances=1, m=2, Nd=1.

    mu_lnA=1.5; mu_lnS=0.5; mu_g=0.5; var_g=1+4=5.
    Independence gives beta=0.5/sqrt(5), Pf=erfc(0.5/sqrt(10))/2.
    """
    result = model(stress_range=math.e, cov_stress=math.sqrt(math.expm1(1)))
    assert result.reliability_index(1) == pytest.approx(0.5 / math.sqrt(5))


def test_both_uncertainties_probability():
    pytest.importorskip("openturns")
    result = model(stress_range=math.e, cov_stress=math.sqrt(math.expm1(1)))
    assert result.failure_probability(1) == pytest.approx(
        math.erfc(0.5 / math.sqrt(10)) / 2
    )


def test_hand_calculation_stress_uncertainty_only():
    """A=1, E[S]=1, var_lnS=1, m=2, Nd=1: g ~ N(1,4)."""
    result = model(A=1, cov_sn=0, cov_stress=math.sqrt(math.expm1(1)))
    assert result.reliability_index(1) == pytest.approx(0.5)


def test_stress_uncertainty_probability():
    pytest.importorskip("openturns")
    result = model(A=1, cov_sn=0, cov_stress=math.sqrt(math.expm1(1)))
    assert result.failure_probability(1) == pytest.approx(
        math.erfc(0.5 / math.sqrt(2)) / 2
    )


@pytest.mark.parametrize("life, pf", [(0.5, 0), (1, 1), (2, 1)])
def test_deterministic_failure_includes_boundary(life, pf):
    """A=S=1, m=2 => N_allowable=1; failure is N_allowable <= Nd.

    The point mass therefore fails at equality, unlike a continuous margin.
    Beta is +infinity for Pf=0, -infinity for Pf=1 by the normal quantile limit.
    """
    result = model(A=1, cov_sn=0)
    assert result.failure_probability(life) == pf
    assert result.reliability_index(life) == (math.inf if pf == 0 else -math.inf)


def test_beta_preserved_when_probability_underflows():
    """ln Nd=1.5-40 => beta=40; double-precision Pf rounds to zero.

    The chosen 40 is a synthetic numerical stress case, not a design target.
    """
    result = model()
    life = math.exp(1.5 - 40)
    assert result.reliability_index(life) == pytest.approx(40)


def test_probability_underflow():
    pytest.importorskip("openturns")
    assert model().failure_probability(math.exp(1.5 - 40)) == 0


def test_nonunit_deterministic_boundary():
    """Synthetic A=10^8, S=10, m=2 gives Nd=10^6 exactly.

    Independent division gives 10^8 / 10^2 = 10^6. Subtracting rounded
    logarithms can leave a positive residual and incorrectly report safety.
    """
    result = model(A=1e8, stress_range=10, cov_sn=0)
    assert result.failure_probability(1e6) == 1
    assert result.reliability_index(1e6) == -math.inf


@pytest.mark.parametrize("stress, expected", [(10, 1), (0.1, 0)])
def test_deterministic_power_outside_float_range(stress, expected):
    """Synthetic m=400, A=Nd=1: S=10 fails; S=0.1 survives.

    Direct S**m overflows/underflows; the log margins have opposite signs.
    """
    result = model(A=1, m=400, stress_range=stress, cov_sn=0)
    assert result.failure_probability(1) == expected


def test_unrepresentable_random_margin_is_rejected():
    """Synthetic m=10^308 and CoV=e^400 give sigma_g > float capacity."""
    result = model(m=1e308, cov_stress=math.exp(400))
    with pytest.raises(ValueError, match="floating-point range"):
        result.failure_probability(1)


def test_small_uncertainty_is_not_discarded():
    """CoV=1e-10 is a synthetic roundoff regression: var_lnA ~ 1e-20.

    For A=Nd=S=1, mu_g=-sigma^2/2, hence beta=-sigma/2.
    """
    cov = 1e-10
    result = model(A=1, cov_sn=cov)
    assert result.reliability_index(1) == pytest.approx(
        -math.sqrt(math.log1p(cov * cov)) / 2, rel=1e-12, abs=0
    )


def test_large_finite_uncertainty_does_not_overflow():
    """CoV=e^400 is synthetic: ln(1+CoV^2) rounds to 800.

    A=Nd=S=1 => beta=-sqrt(800)/2. Squaring CoV directly would overflow.
    """
    result = model(A=1, cov_sn=math.exp(400))
    assert result.reliability_index(1) == pytest.approx(-math.sqrt(800) / 2)


@pytest.mark.parametrize("name", ["A", "m", "stress_range"])
@pytest.mark.parametrize("value", [0, -1, math.inf, math.nan])
def test_invalid_positive_inputs(name, value):
    with pytest.raises(ValueError, match=name):
        model(**{name: value})


@pytest.mark.parametrize("name", ["cov_stress", "cov_sn"])
@pytest.mark.parametrize("value", [-1, math.inf, math.nan])
def test_invalid_covariation(name, value):
    with pytest.raises(ValueError, match=name):
        model(**{name: value})


@pytest.mark.parametrize("life", [0, -1, math.inf, math.nan])
@pytest.mark.parametrize("method", ["failure_probability", "reliability_index"])
def test_invalid_life(life, method):
    with pytest.raises(ValueError, match="life"):
        getattr(model(), method)(life)


def test_missing_dependency_has_install_guidance(monkeypatch):
    original = builtins.__import__

    def without_openturns(name, *args, **kwargs):
        if name == "openturns":
            raise ModuleNotFoundError("openturns", name="openturns")
        return original(name, *args, **kwargs)

    monkeypatch.setattr(builtins, "__import__", without_openturns)
    with pytest.raises(ImportError, match=r"digitalmodel\[reliability\]"):
        model().failure_probability(1)


def test_module_import_does_not_load_optional_dependency():
    """A fresh interpreter must not eagerly import the probability backend."""
    subprocess.run(
        [
            sys.executable,
            "-c",
            "import sys; import digitalmodel.structural.fatigue_reliability; "
            "assert 'openturns' not in sys.modules",
        ],
        check=True,
    )


@pytest.mark.parametrize("cov", [1e-160, 1e-200, 1e-300])
@pytest.mark.parametrize("uncertainty", ["intercept", "stress", "both"])
def test_tiny_positive_uncertainty_index(cov, uncertainty):
    """For tiny c, sigma_lnX=c to float precision; normalize before squaring.

    At A=S=Nd=1 and m=2, beta is -c/2, c/2, or c/(2*sqrt(5))
    for intercept-only, stress-only, or equal independent uncertainty.
    """
    result = model(
        A=1,
        cov_sn=cov if uncertainty != "stress" else 0,
        cov_stress=cov if uncertainty != "intercept" else 0,
    )
    expected = {
        "intercept": -cov / 2,
        "stress": cov / 2,
        "both": cov / (2 * math.sqrt(5)),
    }[uncertainty]
    assert result.reliability_index(1) == pytest.approx(expected, rel=1e-12, abs=0)


@pytest.mark.parametrize("uncertainty", ["intercept", "stress", "both"])
def test_tiny_positive_uncertainty_probability(uncertainty):
    """The normal integral at these near-zero indices rounds to one half."""
    pytest.importorskip("openturns")
    result = model(
        A=1,
        cov_sn=1e-200 if uncertainty != "stress" else 0,
        cov_stress=1e-200 if uncertainty != "intercept" else 0,
    )
    assert result.failure_probability(1) == 0.5
