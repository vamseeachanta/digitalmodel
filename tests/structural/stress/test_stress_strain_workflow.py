from pathlib import Path

import pandas as pd
import pytest
import yaml

from digitalmodel.engine import engine


def test_stress_strain_workflow_writes_curve_next_to_input(tmp_path):
    """The stress_strain workflow runs via the canonical inputfile path and
    writes the curve CSV into a results/ dir beside the input file."""
    cfg = {
        "basename": "stress_strain",
        "stress_strain": {
            "model": "linear_elastic",
            "material": {
                "elastic_modulus": 200000.0,
                "yield_strength": 450.0,
            },
            "strain_grid": [0.0, 0.001, 0.002],
            "output_dir": "results",
        },
        "default": {"log_level": "INFO", "config": {"overwrite": {"output": True}}},
    }
    input_path = tmp_path / "case.yml"
    input_path.write_text(yaml.safe_dump(cfg, sort_keys=False))

    result = engine(inputfile=str(input_path))

    summary = result["stress_strain"]
    curve_csv = Path(summary["curve_csv"])
    if not curve_csv.is_absolute():
        curve_csv = Path.cwd() / curve_csv
    curve = pd.read_csv(curve_csv)

    assert curve_csv.resolve() == (tmp_path / "results" / "case_stress_strain.csv").resolve()
    assert summary["points"] == 3
    assert summary["elastic_modulus"] == pytest.approx(200000.0)
    assert curve["stress"].tolist() == pytest.approx([0.0, 200.0, 400.0])
