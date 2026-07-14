from __future__ import annotations

import pandas as pd
import pytest

from digitalmodel.uncertainty import run_matrix


class ObjectEvaluator:
    def run(self, sample: dict[str, float]) -> dict[str, float]:
        return {"y": sample["x"] * 2.0}


def test_run_matrix_accepts_object_evaluator_and_preserves_inputs():
    samples = pd.DataFrame({"x": [1.0, 2.0, 3.0]})

    result = run_matrix(ObjectEvaluator(), samples)

    assert list(result.columns) == ["x", "y"]
    assert result["y"].tolist() == [2.0, 4.0, 6.0]


def test_run_matrix_accepts_plain_callable_and_prefixes_output_collisions():
    samples = pd.DataFrame({"x": [1.0, 2.0]})

    result = run_matrix(lambda sample: {"x": sample["x"] + 10.0}, samples)

    assert list(result.columns) == ["x", "output_x"]
    assert result["output_x"].tolist() == [11.0, 12.0]


def test_run_matrix_fails_closed_with_row_index_when_evaluator_raises():
    samples = pd.DataFrame({"x": [1.0, 2.0, 3.0]})

    def evaluator(sample: dict[str, float]) -> dict[str, float]:
        if sample["x"] == 2.0:
            raise RuntimeError("boom")
        return {"y": sample["x"]}

    with pytest.raises(RuntimeError, match="row 1"):
        run_matrix(evaluator, samples)


@pytest.mark.parametrize(
    "bad_output",
    [
        {"y": [1.0, 2.0]},
        {"y": float("nan")},
        {"y": float("inf")},
    ],
)
def test_run_matrix_rejects_non_scalar_or_non_finite_outputs(bad_output):
    samples = pd.DataFrame({"x": [1.0]})

    with pytest.raises((TypeError, ValueError), match="row 0"):
        run_matrix(lambda sample: bad_output, samples)
