from __future__ import annotations

import numpy as np
import pandas as pd
from scipy.stats import spearmanr


def _zscore(values: pd.Series) -> np.ndarray:
    array = values.to_numpy(dtype=float)
    scale = array.std(ddof=0)
    if scale == 0.0:
        raise ValueError(f"{values.name} has zero variance")
    return (array - array.mean()) / scale


def tornado(run_df: pd.DataFrame, inputs: list[str], output: str) -> pd.DataFrame:
    """Rank inputs by standardized regression coefficient magnitude."""
    x = np.column_stack([_zscore(run_df[name]) for name in inputs])
    y = _zscore(run_df[output])
    coefficients, *_ = np.linalg.lstsq(x, y, rcond=None)
    result = pd.DataFrame(
        {
            "input": inputs,
            "coefficient": coefficients,
            "abs_coefficient": np.abs(coefficients),
        }
    ).sort_values("abs_coefficient", ascending=False, ignore_index=True)
    result["abs_rank"] = np.arange(1, len(result) + 1)
    return result[["input", "coefficient", "abs_rank"]]


def spearman_ranks(
    run_df: pd.DataFrame, inputs: list[str], output: str
) -> pd.DataFrame:
    """Compute Spearman rank correlation from each input to one output."""
    rows = []
    for name in inputs:
        coefficient, pvalue = spearmanr(run_df[name], run_df[output])
        rows.append({"input": name, "coefficient": coefficient, "pvalue": pvalue})
    return pd.DataFrame(rows)


def percentile_summary(
    run_df: pd.DataFrame,
    output: str,
    percentiles: tuple[int, ...] = (10, 50, 90),
) -> dict[str, float]:
    """Return percentile summary for a scalar output column."""
    values = np.percentile(run_df[output].to_numpy(dtype=float), percentiles)
    return {
        f"p{percentile}": float(value) for percentile, value in zip(percentiles, values)
    }


def fan(
    run_df: pd.DataFrame,
    step_columns: list[str],
    percentiles: tuple[int, int, int] = (10, 50, 90),
) -> pd.DataFrame:
    """Return P10/P50/P90 rows for output columns that encode a step axis."""
    rows = []
    for column in step_columns:
        summary = percentile_summary(run_df, column, percentiles=percentiles)
        rows.append({"step": column, **summary})
    return pd.DataFrame(rows)
