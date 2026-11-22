"""Finite-input degeneracy at the correlation and report dictionary boundaries."""

import json

import numpy as np
import pandas as pd
import pytest

from digitalmodel.solvers.orcaflex.results_analysis import statistical_analysis
from digitalmodel.solvers.orcaflex.results_analysis.statistical_analysis import (
    StatisticalAnalysisService,
)


@pytest.fixture(params=["pearson", "spearman"])
def method(request):
    """Exercise both supported correlation methods with warnings enabled."""
    return request.param


@pytest.fixture
def service():
    return StatisticalAnalysisService()


def assert_json_report(service, result, expected_max):
    """Check strict JSON and native scalars before a report reaches consumers."""
    assert json.loads(json.dumps(result, allow_nan=False)) == result
    for key in ("correlation_matrix", "p_value_matrix"):
        assert all(value is None or type(value) is float
                   for row in result[key] for value in row)
    assert all(type(value) is bool
               for row in result["significance_matrix"] for value in row)
    report = service.generate_statistical_report(result)
    assert report["detailed_results"] == result
    assert report["summary"]["max_correlation"] == expected_max
    if expected_max is None:
        assert report["summary"]["max_correlation_reason"] == "no_defined_correlations"
    else:
        assert "max_correlation_reason" not in report["summary"]
        assert type(report["summary"]["max_correlation"]) is float
    assert json.loads(json.dumps(report, allow_nan=False)) == report


def forbid_scipy(monkeypatch):
    """Undefined pairs must be identified before either SciPy call."""
    def fail(*args, **kwargs):
        pytest.fail("A degenerate pair reached SciPy")

    monkeypatch.setattr(statistical_analysis, "pearsonr", fail)
    monkeypatch.setattr(statistical_analysis, "spearmanr", fail)


@pytest.mark.parametrize("constant_side", ["left", "right", "both"])
def test_constant_pairs(service, method, constant_side):
    varying = [1.0, 2.0, 3.0]
    # Exact equality must detect constants even when std has round-off residue.
    left = [0.1] * 3 if constant_side in ("left", "both") else varying
    right = [7.0] * 3 if constant_side in ("right", "both") else varying
    result = service.calculate_correlation_matrix(
        pd.DataFrame({"x": left, "y": right}), method=method
    )
    reason = {"left": "constant_left", "right": "constant_right",
              "both": "both_constant"}[constant_side]
    reverse = {"left": "constant_right", "right": "constant_left",
               "both": "both_constant"}[constant_side]
    assert result["reason_matrix"][0][1] == reason
    assert result["reason_matrix"][1][0] == reverse
    for i, constant in enumerate((constant_side != "right", constant_side != "left")):
        assert result["reason_matrix"][i][i] == ("both_constant" if constant else None)
        for key in ("correlation_matrix", "p_value_matrix"):
            assert result[key][0][1] is None
            assert result[key][1][0] is None
            expected = None if constant else (1.0 if key == "correlation_matrix" else 0.0)
            assert result[key][i][i] == expected
        assert result["significance_matrix"][i][i] is (not constant)
    assert result["significance_matrix"][0][1] is False
    assert result["significance_matrix"][1][0] is False
    assert_json_report(service, result, None if constant_side == "both" else 1.0)


@pytest.mark.parametrize("constant", [True, False])
def test_single_column(service, method, constant, monkeypatch):
    forbid_scipy(monkeypatch)
    values = [0.1] * 5 if constant else [1.0, 2.0, 3.0, 4.0, 5.0]
    result = service.calculate_correlation_matrix(pd.DataFrame({"x": values}), method)
    assert result["correlation_matrix"] == [[None if constant else 1.0]]
    assert result["p_value_matrix"] == [[None if constant else 0.0]]
    assert result["significance_matrix"] == [[not constant]]
    assert result["reason_matrix"] == [["both_constant" if constant else None]]
    assert_json_report(service, result, None if constant else 1.0)


@pytest.mark.parametrize("n_samples", [1, 2])
@pytest.mark.parametrize("n_columns", [1, 2])
def test_insufficient_observations(service, method, n_samples, n_columns, monkeypatch):
    forbid_scipy(monkeypatch)
    data = pd.DataFrame({str(i): np.arange(n_samples, dtype=float)
                         for i in range(n_columns)})
    result = service.calculate_correlation_matrix(data, method)
    assert result["n_samples"] == n_samples
    for key in ("correlation_matrix", "p_value_matrix"):
        assert result[key] == [[None] * n_columns for _ in range(n_columns)]
    assert result["significance_matrix"] == [[False] * n_columns for _ in range(n_columns)]
    assert result["reason_matrix"] == [
        ["insufficient_observations"] * n_columns for _ in range(n_columns)
    ]
    assert_json_report(service, result, None)


@pytest.mark.parametrize("direction", [1.0, -1.0])
def test_varying_controls(service, method, direction):
    data = pd.DataFrame({"x": [1.0, 2.0, 3.0],
                         "y": [direction * x for x in (2.0, 4.0, 6.0)]})
    result = service.calculate_correlation_matrix(data, method)
    assert result["correlation_matrix"][0][1] == pytest.approx(direction)
    assert result["p_value_matrix"][0][1] == pytest.approx(0.0)
    assert result["significance_matrix"] == [[True, True], [True, True]]
    assert result["reason_matrix"] == [[None, None], [None, None]]
    assert_json_report(service, result, 1.0)


def test_mixed_matrix_preserves_metadata_and_defined_pairs(service, method, monkeypatch):
    original = getattr(statistical_analysis, method + "r")
    calls = []

    def checked_correlation(x, y):
        assert len(x) >= 3
        assert not np.all(x == x.iloc[0])
        assert not np.all(y == y.iloc[0])
        calls.append((x.name, y.name))
        return original(x, y)

    monkeypatch.setattr(statistical_analysis, method + "r", checked_correlation)
    data = pd.DataFrame({"fixed": [0.1] * 5, "up": [1.0, 2, 3, 4, 5],
                         "down": [5.0, 4, 3, 2, 1], "label": ["a"] * 5})
    result = service.calculate_correlation_matrix(data, method, confidence_level=0.99)
    assert set(calls) == {("up", "down"), ("down", "up")}
    assert result["column_names"] == ["fixed", "up", "down"]
    assert result["method"] == method
    assert result["n_samples"] == 5
    assert result["confidence_level"] == 0.99
    assert result["reason_matrix"] == [
        ["both_constant", "constant_left", "constant_left"],
        ["constant_right", None, None], ["constant_right", None, None],
    ]
    for key in ("correlation_matrix", "p_value_matrix", "significance_matrix"):
        assert result[key] == [list(row) for row in zip(*result[key])]
    assert result["correlation_matrix"][1][2] == pytest.approx(-1.0)
    assert result["p_value_matrix"][1][2] == pytest.approx(0.0)
    assert result["significance_matrix"][1][2] is True
    assert result["correlation_matrix"][0] == [None, None, None]
    assert result["p_value_matrix"][0] == [None, None, None]
    assert result["significance_matrix"][0] == [False, False, False]
    assert_json_report(service, result, 1.0)


@pytest.mark.parametrize("matrix, expected", [
    ([[None, None], [None, None]], None),
    ([[None, -0.8], [-0.8, None]], 0.8),
    ([[1.0, -0.8], [-0.8, 1.0]], 1.0),
])
def test_report_filters_undefined_cells(service, matrix, expected):
    result = service.generate_statistical_report({"correlation_matrix": matrix})
    assert result["summary"]["max_correlation"] == expected
    if expected is None:
        assert result["summary"]["max_correlation_reason"] == "no_defined_correlations"
    json.dumps(result, allow_nan=False)


def test_nonfinite_policy_is_outside_finite_input_contract(service, method):
    data = pd.DataFrame({"x": [1.0, np.nan, 3.0], "y": [2.0, 4.0, 6.0]})
    result = service.calculate_correlation_matrix(data, method)
    assert np.isnan(result["correlation_matrix"][0][1])
    assert np.isnan(result["p_value_matrix"][0][1])
    assert result["significance_matrix"][0][1] is False
    with pytest.raises(ValueError, match="Out of range float"):
        json.dumps(result, allow_nan=False)
