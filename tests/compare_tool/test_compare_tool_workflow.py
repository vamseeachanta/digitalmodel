from pathlib import Path

import pandas as pd
import pytest
import yaml

from digitalmodel.engine import engine


def test_compare_tool_workflow_writes_outer_join_and_metrics(tmp_path):
    data_dir = tmp_path / "data"
    data_dir.mkdir()
    (data_dir / "run_a.csv").write_text("x,y\n0,10\n1,20\n2,30\n")
    (data_dir / "run_b.csv").write_text("x,y\n1,19\n2,33\n3,42\n")
    cfg = {
        "basename": "compare_tool",
        "compare_tool": {
            "key": "x",
            "value": "y",
            "sources": [
                {"label": "run_a", "file": "data/run_a.csv"},
                {"label": "run_b", "file": "data/run_b.csv"},
            ],
            "output_dir": "results",
        },
        "default": {"log_level": "INFO", "config": {"overwrite": {"output": True}}},
    }
    input_path = tmp_path / "case.yml"
    input_path.write_text(yaml.safe_dump(cfg, sort_keys=False))

    result = engine(inputfile=str(input_path))

    summary = result["compare_tool"]
    comparison_csv = Path(summary["comparison_csv"])
    table = pd.read_csv(comparison_csv)
    row_x2 = table.loc[table["x"] == 2].iloc[0]

    assert comparison_csv.resolve() == (
        tmp_path / "results" / "case_comparison.csv"
    ).resolve()
    assert summary["n_sources"] == 2
    assert summary["n_rows"] == 4
    assert summary["columns"] == [
        "x",
        "run_a_y",
        "run_b_y",
        "run_b_minus_run_a",
        "run_b_ratio",
    ]
    assert row_x2["run_b_minus_run_a"] == pytest.approx(3.0)
    assert row_x2["run_b_ratio"] == pytest.approx(1.1)
    assert summary["max_abs_delta"]["run_b"] == pytest.approx(3.0)
