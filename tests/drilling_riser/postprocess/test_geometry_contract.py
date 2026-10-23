"""Geometry contract tests use synthetic inputs, not engineering acceptance limits.

Arithmetic source: OpenStax, College Algebra 2e (2021), section 9.2,
https://openstax.org/books/college-algebra-2e/pages/9-2-arithmetic-sequences.
Every finite fixture below is a chosen input; expected positions/counts are
independent applications of the arithmetic sequence written in each docstring.
"""

import pytest

from digitalmodel.drilling_riser.postprocess.evaluate import evaluate_case
from digitalmodel.drilling_riser.postprocess.station_geometry import joint_counts
from digitalmodel.drilling_riser.postprocess.stress_range import (
    classify_stations,
    coupling_positions,
)

from .test_w510_significant_range import ROW, _doc


@pytest.mark.parametrize("joint", [0, -1, float("nan"), float("inf"), None, True, "2"])
def test_invalid_joint_length_is_refused(joint):
    """A joint is a positive finite length; no arithmetic sequence exists otherwise."""
    with pytest.raises(ValueError, match="joint_length_m"):
        coupling_positions([2.0], joint_length_m=joint)


@pytest.mark.parametrize("tol", [-1, float("nan"), float("inf"), None, True, "0"])
def test_invalid_joint_tolerance_is_refused(tol):
    """A rounding tolerance is a finite nonnegative input, never a NaN bypass."""
    with pytest.raises(ValueError, match="tol_m"):
        coupling_positions([2.0], joint_length_m=2.0, tol_m=tol)


def test_joint_counts_and_budget_include_short_sections_and_end_b():
    """Lengths [1, 6, 4], J=2: [1], [3,5,7], [9,11] -> counts [1,3,2]."""
    assert joint_counts([1.0, 6.0, 4.0], 2.0, 0.0) == [1, 3, 2]
    assert coupling_positions([1.0, 6.0, 4.0], 2.0, tol_m=0.0, max_couplings=6) == [
        1.0, 3.0, 5.0, 7.0, 9.0, 11.0,
    ]
    with pytest.raises(ValueError, match="max_couplings"):
        coupling_positions([1.0, 6.0, 4.0], 2.0, tol_m=0.0, max_couplings=5)


@pytest.mark.parametrize("budget", [0, -1, True, 2.0, "2", float("inf")])
def test_budget_requires_a_positive_integer(budget):
    """The caller supplies a count, not a length or rounded floating estimate."""
    with pytest.raises(ValueError, match="max_couplings"):
        coupling_positions([2.0], 2.0, max_couplings=budget)


def test_budget_is_checked_before_any_joint_iteration(monkeypatch):
    """L=2**100, J=1 -> 2**100 couplings; caller budget 4 refuses expansion."""
    from digitalmodel.drilling_riser.postprocess import stress_range

    def no_expansion(*args):
        pytest.fail("joint enumeration started before checking the budget")

    monkeypatch.setattr(stress_range, "range", no_expansion, raising=False)
    with pytest.raises(ValueError, match="max_couplings"):
        coupling_positions([float(2**100)], 1.0, tol_m=0.0, max_couplings=4)


def test_nonrepresentable_joint_ratio_is_refused():
    """Finite L=1e300 divided by J=1e-300 overflows binary float, not geometry."""
    with pytest.raises(ValueError, match="joint count"):
        coupling_positions([1e300], 1e-300, tol_m=0.0, max_couplings=4)


def test_count_preflight_checks_later_sections_before_expansion(monkeypatch):
    """[6,3], J=2: first gives 3 joints, second gives 1.5 and is invalid."""
    from digitalmodel.drilling_riser.postprocess import stress_range

    def no_expansion(*args):
        pytest.fail("joint enumeration started before validating all sections")

    monkeypatch.setattr(stress_range, "range", no_expansion, raising=False)
    with pytest.raises(ValueError, match="whole number"):
        coupling_positions([6.0, 3.0], 2.0, tol_m=0.0)


@pytest.mark.parametrize("length, tol, expected", [(6.125, 0.125, [2.0, 4.0, 6.125]),
                                                 (6.125, 0.0625, None)])
def test_joint_multiple_tolerance_is_a_length_residual(length, tol, expected):
    """L=6.125, J=2 -> 3 joints plus 0.125 residual; supplied tol decides."""
    if expected is None:
        with pytest.raises(ValueError, match="whole number"):
            coupling_positions([length], 2.0, tol_m=tol, max_couplings=3)
    else:
        assert coupling_positions([length], 2.0, tol_m=tol, max_couplings=3) == expected


@pytest.mark.parametrize("wave", ["regular", "irregular"])
def test_cr10_respects_expansion_budget(wave):
    """[2,6], J=2 -> 1+3=4 couplings, exceeding the supplied budget 3."""
    stations = {"sections_m": [2.0, 6.0], "joint_length_m": 2.0,
                "exclude_below_m": 2.0, "max_couplings": 3}
    result = evaluate_case({None: _doc([2.0, 4.0, 6.0, 8.0], sig=[1.0]*4)}, ROW,
                           {"wave_kind": wave, "cr10_stations": stations})
    assert result.status == "NOT_EVALUATED"
    assert "max_couplings" in result.reason


@pytest.mark.parametrize("name", ["exclude_below_m", "exclude_above_m", "hot_spot_m",
                                  "hot_spot_tol_m", "pup_side_tol_m", "coupling_tol_m"])
@pytest.mark.parametrize("value", [float("nan"), float("inf"), float("-inf"), True, "1"])
def test_invalid_station_parameters_are_refused(name, value):
    """Finite station coordinates and finite nonnegative distances are required."""
    stations = {"sections_m": [2.0, 2.0], "joint_length_m": 2.0, "exclude_below_m": 2.0,
                name: value}
    with pytest.raises(ValueError, match=name):
        classify_stations([2.0, 4.0], **stations)


@pytest.mark.parametrize("name", ["hot_spot_tol_m", "pup_side_tol_m", "coupling_tol_m"])
def test_negative_sampling_tolerances_are_refused(name):
    """A negative distance cannot define a sampling neighbourhood."""
    with pytest.raises(ValueError, match=name):
        classify_stations([2.0, 4.0], sections_m=[2.0, 2.0], exclude_below_m=2.0, **{name: -1.0})


@pytest.mark.parametrize("wave", ["regular", "irregular"])
@pytest.mark.parametrize("name", ["exclude_below_m", "exclude_above_m", "coupling_tol_m"])
def test_nonfinite_station_map_is_not_evaluated(wave, name):
    """An undefined coordinate/tolerance must not produce a stress verdict."""
    stations = {"sections_m": [2.0, 2.0], "exclude_below_m": 2.0, name: float("nan")}
    result = evaluate_case({None: _doc([2.0, 4.0], sig=[1.0, 1.0])}, ROW,
                           {"wave_kind": wave, "cr10_stations": stations})
    assert result.status == "NOT_EVALUATED"
    assert name in result.reason


def test_zero_sampling_tolerances_accept_exact_coordinates():
    """[2,2], J=2 -> couplings 2,4; exact samples need zero distance."""
    assert classify_stations([2.0, 4.0], sections_m=[2.0, 2.0], joint_length_m=2.0,
                             exclude_below_m=2.0, coupling_tol_m=0.0,
                             hot_spot_tol_m=0.0, pup_side_tol_m=0.0) == ["coupling", "coupling"]


@pytest.mark.parametrize("value", [float("nan"), float("inf"), True, "2", 10**400])
def test_pup_coordinates_are_validated_before_matching(value):
    """An undefined pup coordinate cannot identify a section boundary."""
    with pytest.raises(ValueError, match="pup_side_couplings_m"):
        classify_stations([2.0, 4.0], sections_m=[2.0, 2.0], exclude_below_m=2.0,
                          pup_side_couplings_m=[value])


def test_accepted_budget_preserves_station_kinds():
    """[2,4], J=2 -> [2,4,6]; budget 3 permits all exact coupling samples."""
    assert classify_stations([2.0, 4.0, 6.0], sections_m=[2.0, 4.0], joint_length_m=2.0,
                             exclude_below_m=2.0, coupling_tol_m=0.0,
                             max_couplings=3) == ["coupling"] * 3
