"""Property-based tests for the sacrificial-anode CP kernel (#2213).

Hypothesis explores the input space of :mod:`digitalmodel.cathodic_protection._kernels`
(and the DNV-RP-B401 / DNV-RP-F103 wrappers that call it) for the algebraic
identities the formulas must satisfy, and the negative / zero inputs every
function must reject with ``ValueError``. Run with ``--hypothesis-seed=0``
for a reproducible example set; the settings below disable the deadline and
the example database so the suite is deterministic and leaves no files.
"""

from __future__ import annotations

import math

import pytest
from hypothesis import assume, given, settings
from hypothesis import strategies as st

from digitalmodel.cathodic_protection import _kernels as kernel
from digitalmodel.cathodic_protection.dnv_rp_b401 import (
    anode_mass_requirement,
    anode_resistance_slender_standoff,
    current_demand,
    number_of_anodes,
    protected_length,
)
from digitalmodel.cathodic_protection.dnv_rp_f103 import (
    protected_length as f103_protected_length,
)

EDITION = "2021"
F103_EDITION = "2010"

PROPERTY = settings(deadline=None, max_examples=100, database=None)


def _finite(lo: float, hi: float) -> st.SearchStrategy[float]:
    return st.floats(min_value=lo, max_value=hi, allow_nan=False, allow_infinity=False)


positive = _finite(1e-3, 1e4)
area = _finite(0.0, 1e6)
density = _finite(0.0, 1.0)
fraction = _finite(0.0, 1.0)
utilisation = _finite(0.05, 1.0)
scale = _finite(1.0, 50.0)


# ---------------------------------------------------------------------------
# Algebraic identities
# ---------------------------------------------------------------------------


class TestMass:
    @PROPERTY
    @given(I_mean=_finite(0.0, 1e3), T=positive, eps=_finite(100.0, 3000.0), u=utilisation)
    def test_mass_doubles_with_current(self, I_mean, T, eps, u):
        """M = I t 8760 / (u eps): doubling I doubles M."""
        m1 = kernel.anode_mass(I_mean, T, eps, u)
        m2 = kernel.anode_mass(2.0 * I_mean, T, eps, u)
        assert m2 == pytest.approx(2.0 * m1, rel=1e-12)

    @PROPERTY
    @given(I_mean=_finite(0.0, 1e3), T=positive, eps=_finite(100.0, 3000.0), u=utilisation)
    def test_mass_doubles_with_life(self, I_mean, T, eps, u):
        """Doubling the design life doubles M."""
        m1 = kernel.anode_mass(I_mean, T, eps, u)
        m2 = kernel.anode_mass(I_mean, 2.0 * T, eps, u)
        assert m2 == pytest.approx(2.0 * m1, rel=1e-12)

    @PROPERTY
    @given(I_mean=_finite(0.0, 1e3), T=positive)
    def test_b401_wrapper_matches_kernel(self, I_mean, T):
        assert anode_mass_requirement(I_mean, T, edition=EDITION) == pytest.approx(
            kernel.anode_mass(I_mean, T, 2000.0, 0.90), rel=1e-12
        )


class TestSlenderResistance:
    @PROPERTY
    @given(rho=_finite(0.05, 5.0), L=positive, ratio=_finite(4.0, 1e3), k=scale)
    def test_long_slender_scales_with_resistivity(self, rho, L, ratio, k):
        """R_long = rho / (2 pi L) (ln(4L/r) - 1) is homogeneous of degree 1 in rho."""
        r = L / ratio  # guarantees L >= 4 r
        R1 = kernel.long_slender_standoff(rho, L, r)
        Rk = kernel.long_slender_standoff(k * rho, L, r)
        assert Rk == pytest.approx(k * R1, rel=1e-12)
        assert R1 > 0.0

    @PROPERTY
    @given(rho=_finite(0.05, 5.0), L=positive, ratio=_finite(4.0, 1e3), k=_finite(1.0, 1.3))
    def test_b401_wrapper_applies_proximity_factor(self, rho, L, ratio, k):
        r = L / ratio
        R = anode_resistance_slender_standoff(rho, L, r, proximity_factor=k, edition=EDITION)
        assert R == pytest.approx(k * kernel.long_slender_standoff(rho, L, r), rel=1e-12)


class TestCurrentDemand:
    @PROPERTY
    @given(A=area, i=density, f=fraction, k=scale)
    def test_linear_in_area(self, A, i, f, k):
        """I_c = A i f: scaling the area scales the demand."""
        assert kernel.current_demand(k * A, i, f) == pytest.approx(
            k * kernel.current_demand(A, i, f), rel=1e-12
        )

    @PROPERTY
    @given(A=area, i=density, f1=fraction, f2=fraction)
    def test_additive_in_breakdown(self, A, i, f1, f2):
        """I_c(f1 + f2) = I_c(f1) + I_c(f2) while f1 + f2 <= 1."""
        assume(f1 + f2 <= 1.0)
        assert kernel.current_demand(A, i, f1 + f2) == pytest.approx(
            kernel.current_demand(A, i, f1) + kernel.current_demand(A, i, f2), rel=1e-9, abs=1e-12
        )

    @PROPERTY
    @given(A=area, i=density, f=fraction)
    def test_b401_wrapper_matches_kernel(self, A, i, f):
        assert current_demand(A, i, f, edition=EDITION) == kernel.current_demand(A, i, f)


class TestCoatingBreakdown:
    @PROPERTY
    @given(a=fraction, b=_finite(0.0, 0.5), t1=_finite(0.0, 100.0), t2=_finite(0.0, 100.0))
    def test_monotone_non_decreasing_and_clamped(self, a, b, t1, t2):
        """t1 <= t2 implies f(t1) <= f(t2), and every value is in [a, 1]."""
        lo, hi = sorted((t1, t2))
        f_lo = kernel.coating_breakdown_linear(a, b, lo)
        f_hi = kernel.coating_breakdown_linear(a, b, hi)
        assert f_lo <= f_hi <= 1.0
        assert f_lo >= min(a, 1.0)

    @PROPERTY
    @given(a=fraction, b=_finite(1e-3, 0.5), extra=_finite(0.0, 1e3))
    def test_clamped_at_one_beyond_full_breakdown(self, a, b, extra):
        """Once a + b t >= 1 the factor stays exactly 1.0."""
        t = (1.0 - a) / b + extra
        assume(a + b * t >= 1.0)  # (1 - a) / b can round a hair below the boundary
        assert kernel.coating_breakdown_linear(a, b, t) == 1.0

    @PROPERTY
    @given(a=fraction, b=_finite(0.0, 0.5), T=positive)
    def test_mean_never_exceeds_final(self, a, b, T):
        assert kernel.coating_breakdown_mean(a, b, T) <= kernel.coating_breakdown_final(a, b, T)


class TestAnodeCount:
    @PROPERTY
    @given(M=_finite(0.0, 1e6), m_a=positive)
    def test_count_covers_mass(self, M, m_a):
        """n = ceil(M / m_a): n m_a >= M and n is the smallest such integer."""
        n = kernel.anode_count(M, m_a)
        assert n >= M / m_a
        assert n == math.ceil(M / m_a)
        assert n == number_of_anodes(M, m_a, edition=EDITION)

    @PROPERTY
    @given(M=_finite(0.0, 1e6), m_a=positive)
    def test_even_rounding_is_at_least_the_ceiling(self, M, m_a):
        n = kernel.anode_count(M, m_a)
        n_even = kernel.anode_count(M, m_a, round_to_even=True)
        assert n_even >= n >= M / m_a
        assert n_even % 2 == 0
        assert n_even - n <= 1


class TestProtectedLength:
    @PROPERTY
    @given(
        dE=_finite(0.01, 1.0), D=_finite(0.05, 2.0), wt_ratio=_finite(0.01, 0.5),
        f=_finite(1e-4, 1.0), i=_finite(1e-3, 1.0), k=scale,
    )
    def test_decreasing_in_current_density(self, dE, D, wt_ratio, f, i, k):
        """PL ~ 1 / sqrt(i_cm): raising i_cm by k divides PL by sqrt(k)."""
        WT = wt_ratio * D
        PL1 = f103_protected_length(dE, WT, D, 2.0e-7, f, i, edition=F103_EDITION)
        PLk = f103_protected_length(dE, WT, D, 2.0e-7, f, k * i, edition=F103_EDITION)
        assert PLk <= PL1
        assert PLk == pytest.approx(PL1 / math.sqrt(k), rel=1e-12)

    @PROPERTY
    @given(
        dE=_finite(0.01, 1.0), D=_finite(0.05, 2.0), wt_ratio=_finite(0.01, 0.5),
        f=_finite(1e-4, 1.0), i=_finite(1e-3, 1.0), k=scale,
    )
    def test_decreasing_in_final_breakdown(self, dE, D, wt_ratio, f, i, k):
        """PL ~ 1 / sqrt(f_cf) likewise (f_cf clamped to the physical range)."""
        assume(k * f <= 1.0)
        WT = wt_ratio * D
        PL1 = protected_length(dE, WT, D, 2.0e-7, f, i, edition=EDITION)
        PLk = protected_length(dE, WT, D, 2.0e-7, k * f, i, edition=EDITION)
        assert PLk <= PL1
        assert PLk == pytest.approx(PL1 / math.sqrt(k), rel=1e-12)


# ---------------------------------------------------------------------------
# Negative / zero inputs raise ValueError
# ---------------------------------------------------------------------------


@pytest.mark.parametrize(
    "args",
    [(-1.0, 0.1, 0.5), (10.0, -0.1, 0.5), (10.0, 0.1, -0.5), (10.0, 0.1, 1.5)],
    ids=["negative_area", "negative_density", "negative_breakdown", "breakdown_above_one"],
)
def test_current_demand_rejects(args):
    with pytest.raises(ValueError):
        kernel.current_demand(*args)
    with pytest.raises(ValueError):
        current_demand(*args, edition=EDITION)


@pytest.mark.parametrize(
    "args",
    [
        (-1.0, 25.0, 2000.0, 0.9),
        (1.0, 0.0, 2000.0, 0.9),
        (1.0, -25.0, 2000.0, 0.9),
        (1.0, 25.0, 0.0, 0.9),
        (1.0, 25.0, 2000.0, 0.0),
        (1.0, 25.0, 2000.0, -0.9),
        (1.0, 25.0, 2000.0, 1.5),
    ],
    ids=["negative_current", "zero_life", "negative_life", "zero_capacity",
         "zero_utilisation", "negative_utilisation", "utilisation_above_one"],
)
def test_anode_mass_rejects(args):
    with pytest.raises(ValueError):
        kernel.anode_mass(*args)
    with pytest.raises(ValueError):
        anode_mass_requirement(*args, edition=EDITION)


@pytest.mark.parametrize(
    "args",
    [
        (0.0, 0.022, 0.329, 2.0e-7, 0.0042, 0.25),
        (0.15, 0.0, 0.329, 2.0e-7, 0.0042, 0.25),
        (0.15, 0.022, 0.0, 2.0e-7, 0.0042, 0.25),
        (0.15, 0.022, 0.329, 0.0, 0.0042, 0.25),
        (0.15, 0.022, 0.329, 2.0e-7, 0.0, 0.25),
        (0.15, 0.022, 0.329, 2.0e-7, 0.0042, 0.0),
        (0.15, 0.022, 0.329, 2.0e-7, -0.0042, 0.25),
        (0.15, 0.022, 0.329, 2.0e-7, 0.0042, -0.25),
        (0.15, 0.329, 0.329, 2.0e-7, 0.0042, 0.25),
    ],
    ids=["zero_dE", "zero_WT", "zero_D", "zero_rho_me", "zero_f_cf", "zero_i_cm",
         "negative_f_cf", "negative_i_cm", "WT_not_below_D"],
)
def test_protected_length_rejects(args):
    with pytest.raises(ValueError):
        f103_protected_length(*args, edition=F103_EDITION)
    with pytest.raises(ValueError):
        protected_length(*args, edition=EDITION)


@pytest.mark.parametrize(
    "args", [(100.0, 0.0), (100.0, -5.0), (-100.0, 5.0)],
    ids=["zero_unit_mass", "negative_unit_mass", "negative_total"],
)
def test_number_of_anodes_rejects(args):
    with pytest.raises(ValueError):
        kernel.anode_count(*args)
    with pytest.raises(ValueError):
        number_of_anodes(*args, edition=EDITION)


@pytest.mark.parametrize("bad", [0.0, -0.3], ids=["zero", "negative"])
@pytest.mark.parametrize("position", [0, 1, 2], ids=["rho", "L", "r"])
def test_long_slender_standoff_rejects(bad, position):
    args = [0.30, 1.5, 0.05]
    args[position] = bad
    with pytest.raises(ValueError):
        kernel.long_slender_standoff(*args)
    with pytest.raises(ValueError):
        anode_resistance_slender_standoff(*args, edition=EDITION)


@pytest.mark.parametrize("bad", [0.0, -0.3], ids=["zero", "negative"])
@pytest.mark.parametrize("position", [0, 1, 2], ids=["rho", "L", "r"])
def test_short_slender_standoff_rejects(bad, position):
    args = [0.30, 0.30, 0.10]  # L < 4 r
    args[position] = bad
    with pytest.raises(ValueError):
        kernel.short_slender_standoff(*args)


@pytest.mark.parametrize("bad", [0.0, -0.3], ids=["zero", "negative"])
@pytest.mark.parametrize("position", [0, 1, 2, 3], ids=["rho", "length", "width", "thickness"])
def test_long_flush_rejects(bad, position):
    args = [0.30, 1.0, 0.2, 0.05]  # length >= 4 width and >= 4 thickness
    args[position] = bad
    with pytest.raises(ValueError):
        kernel.long_flush(*args)


@pytest.mark.parametrize("bad", [0.0, -0.3], ids=["zero", "negative"])
@pytest.mark.parametrize("position", [0, 1], ids=["rho", "area"])
def test_short_flush_or_bracelet_rejects(bad, position):
    args = [0.30, 0.25]
    args[position] = bad
    with pytest.raises(ValueError):
        kernel.short_flush_or_bracelet(*args)


def test_slender_forms_reject_wrong_regime():
    """The long form refuses L < 4r and the short form refuses L >= 4r."""
    with pytest.raises(ValueError, match="L >= 4 r"):
        kernel.long_slender_standoff(0.30, 0.30, 0.10)
    with pytest.raises(ValueError, match="L < 4 r"):
        kernel.short_slender_standoff(0.30, 1.5, 0.05)
    with pytest.raises(ValueError, match="length >= 4 width"):
        kernel.long_flush(0.30, 0.5, 0.2, 0.05)
