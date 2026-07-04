"""Tests for ROV LARS sea-state operability (API 17H / IMCA R 004)."""

import pytest

from digitalmodel.subsea.rov.seastate_limits import (
    scatter_operability,
    seastate_operability,
)


# --------------------------------------------------------------------------- #
# Worked example (hand-verified)
#
#   Hs = 2.0 m, Hs_limit = 3.0 m, alpha = 1.0
#   Hs_oplim    = 1.0 * 3.0 = 3.0 m
#   utilisation = 2.0 / 3.0  = 0.6667
#   margin      = 3.0 - 2.0  = 1.0 m
#   2.0 <= 3.0  -> operable
# --------------------------------------------------------------------------- #
def test_within_limit_is_operable():
    res = seastate_operability(hs=2.0, hs_limit=3.0)
    assert res.hs_oplim == pytest.approx(3.0)
    assert res.utilisation == pytest.approx(2.0 / 3.0)
    assert res.margin == pytest.approx(1.0)
    assert res.operable is True


def test_over_limit_is_not_operable():
    # Hs = 3.5 m above the 3.0 m limit -> abort.
    res = seastate_operability(hs=3.5, hs_limit=3.0)
    assert res.utilisation == pytest.approx(3.5 / 3.0)
    assert res.margin == pytest.approx(-0.5)
    assert res.operable is False


def test_boundary_exactly_at_limit_is_operable():
    # Hs == Hs_oplim is acceptable (<=).
    res = seastate_operability(hs=3.0, hs_limit=3.0)
    assert res.utilisation == pytest.approx(1.0)
    assert res.margin == pytest.approx(0.0, abs=1e-12)
    assert res.operable is True


def test_alpha_factor_derates_limit():
    # alpha = 0.8 derates a 3.0 m limit to 2.4 m; Hs = 2.5 m is then NOT operable.
    res = seastate_operability(hs=2.5, hs_limit=3.0, alpha=0.8)
    assert res.hs_oplim == pytest.approx(2.4)
    assert res.operable is False
    # Same Hs would be operable without the allowance.
    assert seastate_operability(hs=2.5, hs_limit=3.0).operable is True


@pytest.mark.parametrize(
    "kwargs",
    [
        {"hs": -0.1, "hs_limit": 3.0},     # negative Hs
        {"hs": 2.0, "hs_limit": 0.0},      # non-positive limit
        {"hs": 2.0, "hs_limit": 3.0, "alpha": 0.0},   # alpha out of (0,1]
        {"hs": 2.0, "hs_limit": 3.0, "alpha": 1.5},   # alpha out of (0,1]
    ],
)
def test_invalid_inputs_raise(kwargs):
    with pytest.raises(ValueError):
        seastate_operability(**kwargs)


# --------------------------------------------------------------------------- #
# Scatter-diagram operability (hand-verified)
#
#   Hs_limit = 3.0 m, alpha = 1.0 -> Hs_oplim = 3.0 m
#   cells: (1.0, 0.5), (2.5, 0.3), (4.0, 0.2)
#   operable cells (Hs <= 3.0): 1.0 and 2.5  -> weight 0.5 + 0.3 = 0.8
#   total weight = 1.0 ; pct = 100 * 0.8 / 1.0 = 80.0 %
# --------------------------------------------------------------------------- #
def test_scatter_operability_percentage():
    scatter = [
        {"hs": 1.0, "probability": 0.5},
        {"hs": 2.5, "probability": 0.3},
        {"hs": 4.0, "probability": 0.2},
    ]
    res = scatter_operability(scatter, hs_limit=3.0)
    assert res.operability_pct == pytest.approx(80.0)
    assert res.hs_oplim == pytest.approx(3.0)
    assert res.n_operable == 2
    assert res.n_total == 3


def test_scatter_operability_unnormalised_weights():
    # Weights need not sum to 1: 5 + 3 operable out of 5 + 3 + 2 = 10 -> 80 %.
    scatter = [
        {"hs": 1.0, "probability": 5.0},
        {"hs": 2.5, "probability": 3.0},
        {"hs": 4.0, "probability": 2.0},
    ]
    res = scatter_operability(scatter, hs_limit=3.0)
    assert res.operability_pct == pytest.approx(80.0)


def test_scatter_operability_alpha_excludes_more_cells():
    # alpha = 0.8 -> Hs_oplim = 2.4 m; now only the 1.0 m cell is operable -> 50 %.
    scatter = [
        {"hs": 1.0, "probability": 0.5},
        {"hs": 2.5, "probability": 0.3},
        {"hs": 4.0, "probability": 0.2},
    ]
    res = scatter_operability(scatter, hs_limit=3.0, alpha=0.8)
    assert res.hs_oplim == pytest.approx(2.4)
    assert res.n_operable == 1
    assert res.operability_pct == pytest.approx(50.0)


@pytest.mark.parametrize(
    "scatter,kwargs",
    [
        ([], {"hs_limit": 3.0}),                                      # empty
        ([{"hs": 1.0, "probability": -0.1}], {"hs_limit": 3.0}),      # neg prob
        ([{"hs": -1.0, "probability": 0.5}], {"hs_limit": 3.0}),      # neg hs
        ([{"hs": 1.0, "probability": 0.0}], {"hs_limit": 3.0}),       # zero total
        ([{"hs": 1.0, "probability": 0.5}], {"hs_limit": 0.0}),       # bad limit
    ],
)
def test_scatter_invalid_inputs_raise(scatter, kwargs):
    with pytest.raises(ValueError):
        scatter_operability(scatter, **kwargs)
