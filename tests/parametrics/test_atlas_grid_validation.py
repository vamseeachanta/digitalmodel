"""Finite unique Cartesian membership, per atlas-spec.md Decision 2 and issue 2118."""

import pytest

from digitalmodel.parametrics.atlas_bridge import atlas_from_summary


# Independent Cartesian example: x,y in {1,2,3}; response=x+y.
# There are 3*3=9 distinct nodes. At (1.5,2.5), bilinear weights are
# 1/4 on (1,2),(1,3),(2,2),(2,3), giving (3+4+4+5)/4=4.
def _cartesian_summary():
    from itertools import product

    from digitalmodel.parametrics import (
        CaseResult, ParameterSweep, ParametricResultsSummary, ParametricStudy,
    )

    study = ParametricStudy(parameters=[
        ParameterSweep(name=name, values=[1.0, 2.0, 3.0]) for name in ("x", "y")
    ])
    summary = ParametricResultsSummary(results=[
        CaseResult(case_id=str(i), parameters={"x": x, "y": y},
                   max_utilisation=x + y, status="completed")
        for i, (x, y) in enumerate(product([1.0, 2.0, 3.0], repeat=2))
    ])
    return study, summary


def _build(study, summary):
    return atlas_from_summary(study, summary, "max_utilisation",
                              basename="synthetic", atlas_id="sum-example")


def test_complete_cartesian_grid_is_not_holdout_evidence():
    study, summary = _cartesian_summary()
    atlas = _build(study, summary)
    assert atlas.predict({"x": 1.5, "y": 2.5}).value == 4.0
    assert not atlas.validation_available
    assert atlas.validation["reason"] == "no holdout evaluation"


@pytest.mark.parametrize("bad", ["duplicate", "foreign", "axis_nan", "axis_inf",
                                  "response_nan", "response_inf", "response_neg_inf"])
def test_rejects_invalid_cartesian_membership_or_nonfinite_values(bad):
    import math

    study, summary = _cartesian_summary()
    row = summary.results[-1]
    if bad == "duplicate":
        row.parameters = dict(summary.results[0].parameters)
    elif bad == "foreign":
        row.parameters["x"] = 4.0  # outside declared {1,2,3}
    elif bad.startswith("axis_"):
        row.parameters["x"] = math.nan if bad == "axis_nan" else math.inf
    else:
        row.max_utilisation = {
            "response_nan": math.nan, "response_inf": math.inf,
            "response_neg_inf": -math.inf,
        }[bad]
    with pytest.raises(ValueError, match="finite|full-factorial|duplicate"):
        _build(study, summary)


@pytest.mark.parametrize("values", [[1.0, 1.0, 2.0], [], [1.0, float("inf")]])
def test_rejects_invalid_declared_axis(values):
    study, summary = _cartesian_summary()
    study.parameters[0].values = values
    with pytest.raises(ValueError, match="finite|unique|empty|full-factorial"):
        _build(study, summary)
