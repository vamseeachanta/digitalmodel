import math
from pathlib import Path

import pandas as pd
import pytest
import yaml

from digitalmodel.engine import engine


def test_api_rp_2rd_code_check_workflow_writes_results_and_summary(tmp_path):
    cfg = {
        "basename": "code_check",
        "code_check": {
            "code": "api-rp-2rd",
            "inputs": {
                "outer_diameter": 0.2731,
                "wall_thickness": 0.0254,
                "smys": 448e6,
                "smts": 531e6,
                "design_factor": 0.67,
                "temperature_derating": 1.0,
                "arc_lengths": [0.0, 50.0, 100.0],
                "effective_tension_kN": [900.0, 1200.0, 1500.0],
                "bending_moment_kNm": [120.0, 180.0, 220.0],
                "pressure_diff_MPa": [0.0, 1.0, 2.0],
            },
            "output_dir": "results",
        },
        "default": {"log_level": "INFO", "config": {"overwrite": {"output": True}}},
    }
    input_path = tmp_path / "case.yml"
    input_path.write_text(yaml.safe_dump(cfg, sort_keys=False))

    result = engine(inputfile=str(input_path))

    summary = result["code_check"]
    results_csv = Path(summary["results_csv"])
    table = pd.read_csv(results_csv)

    assert results_csv.resolve() == (
        tmp_path / "results" / "case_code_check.csv"
    ).resolve()
    assert summary["code"] == "API_RP_2RD"
    assert summary["pass"] is True
    assert summary["governing_utilisation"] > 0.0
    assert math.isfinite(summary["governing_utilisation"])
    assert summary["governing_result"]["arc_length"] == pytest.approx(100.0)
    assert len(table) == 3
    assert table["utilisation"].max() == pytest.approx(
        summary["governing_utilisation"]
    )


def test_code_check_workflow_unknown_code_fails_closed(tmp_path):
    cfg = {
        "basename": "code_check",
        "code_check": {
            "code": "unknown-code",
            "inputs": {},
            "output_dir": "results",
        },
        "default": {"log_level": "INFO", "config": {"overwrite": {"output": True}}},
    }
    input_path = tmp_path / "case.yml"
    input_path.write_text(yaml.safe_dump(cfg, sort_keys=False))

    with pytest.raises(ValueError, match="Unsupported code_check code"):
        engine(inputfile=str(input_path))
