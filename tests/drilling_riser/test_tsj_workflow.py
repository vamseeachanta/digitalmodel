import math
from pathlib import Path

import pandas as pd
import pytest
import yaml

from digitalmodel.engine import engine


def test_tsj_sizing_workflow_writes_section_utilisation_next_to_input(tmp_path):
    cfg = {
        "basename": "tsj_sizing",
        "tsj_sizing": {
            "effective_tension_N": 2_500_000.0,
            "internal_pressure_Pa": 12_000_000.0,
            "external_pressure_Pa": 1_000_000.0,
            "SMYS_Pa": 552_000_000.0,
            "design_factor": 0.67,
            "taper": [
                {
                    "position_m": 0.0,
                    "outer_diameter_mm": 508.0,
                    "wall_thickness_mm": 38.0,
                },
                {
                    "position_m": 10.0,
                    "outer_diameter_mm": 457.0,
                    "wall_thickness_mm": 32.0,
                },
                {
                    "position_m": 20.0,
                    "outer_diameter_mm": 406.0,
                    "wall_thickness_mm": 25.0,
                },
                {
                    "position_m": 30.0,
                    "outer_diameter_mm": 356.0,
                    "wall_thickness_mm": 20.0,
                },
            ],
            "output_dir": "results",
        },
        "default": {"log_level": "INFO", "config": {"overwrite": {"output": True}}},
    }
    input_path = tmp_path / "case.yml"
    input_path.write_text(yaml.safe_dump(cfg, sort_keys=False))

    result = engine(inputfile=str(input_path))

    summary = result["tsj_sizing"]
    results_csv = Path(summary["results_csv"])
    table = pd.read_csv(results_csv)

    assert results_csv.resolve() == (tmp_path / "results" / "case_tsj.csv").resolve()
    assert len(table) == 4
    assert summary["passes"] is True
    assert 0.0 < summary["max_utilisation"] < 1.0
    assert math.isfinite(summary["max_utilisation"])
    assert summary["governing_section"]["position_m"] == pytest.approx(30.0)
    assert (table["utilisation"] > 0.0).all()
    assert table["utilisation"].map(math.isfinite).all()
    assert table["utilisation"].iloc[-1] == pytest.approx(summary["max_utilisation"])
    assert table["vm_stress_Pa"].iloc[-1] > table["vm_stress_Pa"].iloc[0]
