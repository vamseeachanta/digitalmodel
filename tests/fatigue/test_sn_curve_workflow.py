from pathlib import Path

import pandas as pd
import pytest
import yaml

from digitalmodel.engine import engine


def test_sn_curve_workflow_writes_endurance_csv_next_to_input(tmp_path):
    """The sn_curve workflow runs through the canonical engine path and
    writes allowable cycles beside the input file."""
    cfg = {
        "basename": "sn_curve",
        "sn_curve": {
            "curve_id": "DNV-RP-C203:D:air",
            "stress_ranges": [50.0, 100.0, 300.0],
            "output_dir": "results",
        },
        "default": {"log_level": "INFO", "config": {"overwrite": {"output": True}}},
    }
    input_path = tmp_path / "case.yml"
    input_path.write_text(yaml.safe_dump(cfg, sort_keys=False))

    result = engine(inputfile=str(input_path))

    summary = result["sn_curve"]
    curve_csv = Path(summary["curve_csv"])
    if not curve_csv.is_absolute():
        curve_csv = Path.cwd() / curve_csv
    curve = pd.read_csv(curve_csv)

    assert curve_csv.resolve() == (tmp_path / "results" / "case_sn_curve.csv").resolve()
    assert summary["points"] == 3
    assert summary["curves"][0]["curve_id"] == "DNV-RP-C203:D:air"
    assert summary["curves"][0]["m1"] == pytest.approx(3.0)
    assert summary["curves"][0]["log_a1"] == pytest.approx(12.164)
    assert curve["stress_range_mpa"].tolist() == pytest.approx([50.0, 100.0, 300.0])
    assert (curve["allowable_cycles_n"] > 0).all()
    assert curve["allowable_cycles_n"].is_monotonic_decreasing
