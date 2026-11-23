from __future__ import annotations

import numpy as np
import pandas as pd

from digitalmodel.uncertainty import (
    fan,
    percentile_summary,
    run_matrix,
    spearman_ranks,
    tornado,
)


def _run_df() -> pd.DataFrame:
    x = np.repeat(np.linspace(0.0, 1.0, 8), 5)
    y = np.tile(np.linspace(0.0, 1.0, 5), 8)
    weak = np.sin(np.linspace(0.0, np.pi, 40))
    output = 10.0 * x - 1.0 * y + 0.1 * weak
    return pd.DataFrame(
        {
            "x": x,
            "y": y,
            "weak": weak,
            "output": output,
            "step_1": output,
            "step_2": output + 5.0,
        }
    )


def test_tornado_ranks_dominant_input_first():
    result = tornado(_run_df(), inputs=["x", "y", "weak"], output="output")

    assert result.iloc[0]["input"] == "x"
    assert result.iloc[0]["abs_rank"] == 1


def test_spearman_signs_and_percentiles_from_one_run_matrix():
    run_df = _run_df()

    ranks = spearman_ranks(run_df, inputs=["x", "y"], output="output")
    summary = percentile_summary(run_df, output="output")

    assert ranks.set_index("input").loc["x", "coefficient"] > 0
    assert ranks.set_index("input").loc["y", "coefficient"] < 0
    assert summary["p10"] < summary["p50"] < summary["p90"]


def test_fan_returns_p10_p50_p90_per_step():
    result = fan(_run_df(), step_columns=["step_1", "step_2"])

    assert list(result.columns) == ["step", "p10", "p50", "p90"]
    assert result["step"].tolist() == ["step_1", "step_2"]
    assert (result["p10"] < result["p50"]).all()
    assert (result["p50"] < result["p90"]).all()


def test_analytics_share_one_run_matrix_result():
    samples = pd.DataFrame(
        {
            "x": np.repeat(np.linspace(0.0, 1.0, 8), 5),
            "y": np.tile(np.linspace(0.0, 1.0, 5), 8),
        }
    )

    run_df = run_matrix(
        lambda sample: {
            "output": 8.0 * sample["x"] - sample["y"],
            "step_1": sample["x"] + sample["y"],
            "step_2": 2.0 * sample["x"] + sample["y"],
        },
        samples,
    )

    tornado_result = tornado(run_df, inputs=["x", "y"], output="output")
    spearman_result = spearman_ranks(run_df, inputs=["x", "y"], output="output")
    summary = percentile_summary(run_df, output="output")
    fan_result = fan(run_df, step_columns=["step_1", "step_2"])

    assert tornado_result.iloc[0]["input"] == "x"
    assert spearman_result.set_index("input").loc["x", "coefficient"] > 0
    assert summary["p10"] < summary["p50"] < summary["p90"]
    assert fan_result["step"].tolist() == ["step_1", "step_2"]
