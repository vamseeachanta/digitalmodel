from __future__ import annotations

import math
from collections.abc import Callable, Mapping
from numbers import Real
from typing import Protocol

import pandas as pd


class Evaluator(Protocol):
    def run(self, sample: dict[str, float]) -> dict[str, float]:
        """Evaluate one uncertainty sample and return scalar outputs."""


def _call_evaluator(
    evaluator: Evaluator | Callable[[dict[str, float]], Mapping[str, float]],
    sample: dict[str, float],
) -> Mapping[str, float]:
    if hasattr(evaluator, "run"):
        return evaluator.run(sample)
    return evaluator(sample)


def run_matrix(
    evaluator: Evaluator | Callable[[dict[str, float]], Mapping[str, float]],
    samples: pd.DataFrame,
) -> pd.DataFrame:
    """Run an evaluator over a sample matrix and return inputs plus outputs.

    Output columns that collide with input names are prefixed with ``output_``.
    Evaluator exceptions abort the batch and name the row index that failed.
    """
    rows: list[dict[str, float]] = []
    input_columns = set(samples.columns)
    for row_index, row in samples.iterrows():
        sample = row.to_dict()
        try:
            outputs = _call_evaluator(evaluator, sample)
        except Exception as exc:
            raise RuntimeError(f"Evaluator failed at row {row_index}: {exc}") from exc
        if not isinstance(outputs, Mapping):
            raise TypeError(f"Evaluator row {row_index} did not return a mapping")
        combined = dict(sample)
        for key, value in outputs.items():
            if not isinstance(value, Real) or not math.isfinite(float(value)):
                raise ValueError(
                    f"Evaluator output {key!r} at row {row_index} must be a finite scalar"
                )
            output_key = f"output_{key}" if key in input_columns else key
            combined[output_key] = value
        rows.append(combined)
    return pd.DataFrame(rows)
