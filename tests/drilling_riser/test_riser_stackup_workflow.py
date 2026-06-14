from pathlib import Path

import pandas as pd
import pytest
import yaml

from digitalmodel.engine import engine


def test_riser_stackup_workflow_writes_profile_next_to_input(tmp_path):
    """The riser_stackup workflow runs through the canonical engine path and
    writes the effective-tension profile beside the input file."""
    cfg = {
        "basename": "riser_stackup",
        "riser_stackup": {
            "submerged_weight_kn": 1000.0,
            "dynamic_factor": 1.3,
            "od_mm": 533.4,
            "design_pressure_mpa": 20.0,
            "smys_mpa": 552.0,
            "barlow_safety_factor": 1.6,
            "buoyancy_uplift_kn": 250.0,
            "internal_area_m2": 0.16,
            "mud_density_kn_m3": 13.0,
            "mud_column_m": 1500.0,
            "seawater_density_kn_m3": 10.05,
            "seawater_column_m": 1500.0,
            "depth_factors": [0.0, 0.5, 1.0],
            "output_dir": "results",
        },
        "default": {"log_level": "INFO", "config": {"overwrite": {"output": True}}},
    }
    input_path = tmp_path / "case.yml"
    input_path.write_text(yaml.safe_dump(cfg, sort_keys=False))

    result = engine(inputfile=str(input_path))

    summary = result["riser_stackup"]
    profile_csv = Path(summary["profile_csv"])
    if not profile_csv.is_absolute():
        profile_csv = Path.cwd() / profile_csv
    profile = pd.read_csv(profile_csv)

    assert profile_csv.resolve() == (
        tmp_path / "results" / "case_riser_stackup.csv"
    ).resolve()
    assert summary["top_tension_required_kn"] == pytest.approx(1300.0)
    assert summary["wall_thickness_required_mm"] == pytest.approx(
        (20.0 * 533.4 * 1.6) / (2.0 * 552.0)
    )
    assert summary["min_slip_ring_tension_kn"] == pytest.approx(
        1000.0 * 1.05 - 250.0 * 0.96 + 0.16 * (13.0 * 1500.0 - 10.05 * 1500.0)
    )
    assert profile["depth_factor"].tolist() == pytest.approx([0.0, 0.5, 1.0])
    assert profile["effective_tension_kn"].tolist() == pytest.approx(
        [1300.0, 800.0, 300.0]
    )
