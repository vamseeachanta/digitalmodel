"""Finite-input regressions for the local component contract.

Source: issue 2749, item 2 (positive ratings and nonnegative transfer times).
Python 3.11 Library Reference, math.isfinite:
https://docs.python.org/3.11/library/math.html#math.isfinite
These are input-domain checks, not equipment performance benchmarks.
"""

import math

import pytest

from digitalmodel.power.datacenter.models import STSModule, UPSModule


@pytest.mark.parametrize("module", [UPSModule, STSModule])
# Issue 2749 item 2: zero is the excluded rating boundary; -1 is below it.
@pytest.mark.parametrize("rating", [math.nan, math.inf, -math.inf, 0.0, -0.0, -1.0])
def test_reject_invalid_rating(module, rating):
    """NaN and infinities are nonfinite; signed zeros and -1 are not positive.

    Each case therefore lies outside the specified finite, positive domain,
    independently of any capacity calculation or constructor implementation.
    """
    with pytest.raises(ValueError, match="rated_kva"):
        module(rated_kva=rating)


# Issue 2749 item 2: negative time is excluded; 1 kVA is a positive test input.
@pytest.mark.parametrize("time_ms", [math.nan, math.inf, -math.inf, -1.0])
def test_reject_invalid_transfer_time(time_ms):
    """NaN and infinities are nonfinite; -1 < 0, so all cases are rejected."""
    with pytest.raises(ValueError, match="transfer_time_ms"):
        STSModule(rated_kva=1.0, transfer_time_ms=time_ms)


@pytest.mark.parametrize("module", [UPSModule, STSModule])
# Arbitrary exact binary fractions, not equipment limits: 0 < 0.5 < 1.
@pytest.mark.parametrize("rating", [0.5, 1.0])
def test_preserve_finite_positive_rating(module, rating):
    """Both 1/2 and 1 are finite and > 0, so the supplied rating is retained."""
    assert module(rated_kva=rating).rated_kva == rating


# Issue 2749 item 2 includes zero; 0.5 is an arbitrary positive test input.
@pytest.mark.parametrize("time_ms", [0.0, -0.0, 0.5])
def test_preserve_finite_nonnegative_transfer_time(time_ms):
    """Signed zeros equal 0; 1/2 > 0, so each supplied time is retained."""
    sts = STSModule(rated_kva=1.0, transfer_time_ms=time_ms)
    assert sts.transfer_time_ms == time_ms
