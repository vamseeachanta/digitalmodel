import math
from pathlib import Path

import pandas as pd
import pytest
import yaml

from digitalmodel.engine import engine


def test_von_mises_workflow_writes_element_results_next_to_input(tmp_path):
    cfg = {
        "basename": "von_mises",
        "von_mises": {
            "nodes": [
                {"id": 0, "x": 0.0, "y": 0.0},
                {"id": 1, "x": 1.0, "y": 0.0},
                {"id": 2, "x": 2.0, "y": 0.0},
            ],
            "elements": [
                {
                    "id": 0,
                    "node1": 0,
                    "node2": 1,
                    "E": 2.07e11,
                    "I": 1.0e-5,
                    "A": 1.0e-2,
                },
                {
                    "id": 1,
                    "node1": 1,
                    "node2": 2,
                    "E": 2.07e11,
                    "I": 1.0e-5,
                    "A": 1.0e-2,
                },
            ],
            "boundary_conditions": {"fixed_nodes": [0]},
            "loads": [{"node": 2, "direction": "x", "magnitude": -1000.0}],
            "material": {
                "E": 2.07e11,
                "nu": 0.3,
                "rho": 7850.0,
                "sigma_y": 4.48e8,
            },
            "output_dir": "results",
        },
        "default": {"log_level": "INFO", "config": {"overwrite": {"output": True}}},
    }
    input_path = tmp_path / "case.yml"
    input_path.write_text(yaml.safe_dump(cfg, sort_keys=False))

    result = engine(inputfile=str(input_path))

    summary = result["von_mises"]
    csv_path = Path(summary["results_csv"])
    if not csv_path.is_absolute():
        csv_path = Path.cwd() / csv_path
    element_results = pd.read_csv(csv_path)

    assert csv_path.resolve() == (
        tmp_path / "results" / "case_von_mises.csv"
    ).resolve()
    assert summary["status"] == "completed"
    assert summary["num_elements"] == 2
    assert summary["max_stress"] > 0.0
    assert math.isfinite(summary["max_stress"])
    assert summary["min_safety_factor"] == pytest.approx(
        cfg["von_mises"]["material"]["sigma_y"] / summary["max_stress"]
    )
    assert summary["n_critical"] == 0
    assert len(element_results) == 2
    assert (element_results["von_mises_stress"] > 0.0).all()
    assert (element_results["safety_factor"] > 0.0).all()
