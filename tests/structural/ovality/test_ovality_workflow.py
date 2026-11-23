from pathlib import Path

import pandas as pd
import pytest
import yaml

from digitalmodel.engine import engine


def test_pipe_ovality_workflow_writes_geometry_check_next_to_input(tmp_path):
    cfg = {
        "basename": "pipe_ovality",
        "pipe_ovality": {
            "nominal_OD": 323.85,
            "max_OD": 325.0,
            "min_OD": 322.7,
            "allowable": 0.03,
            "output_dir": "results",
        },
        "default": {"log_level": "INFO", "config": {"overwrite": {"output": True}}},
    }
    input_path = tmp_path / "case.yml"
    input_path.write_text(yaml.safe_dump(cfg, sort_keys=False))

    result = engine(inputfile=str(input_path))

    summary = result["pipe_ovality"]
    results_csv = Path(summary["results_csv"])
    table = pd.read_csv(results_csv)

    expected_out_of_roundness = (325.0 - 322.7) / 323.85
    assert (
        results_csv.resolve() == (tmp_path / "results" / "case_ovality.csv").resolve()
    )
    assert summary["out_of_roundness"] == pytest.approx(expected_out_of_roundness)
    assert summary["ovality_percent"] == pytest.approx(
        expected_out_of_roundness * 100.0
    )
    assert summary["passes"] is True
    assert len(table) == 1
    assert table.loc[0, "out_of_roundness"] == pytest.approx(expected_out_of_roundness)
