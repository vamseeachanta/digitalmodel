import copy
from pathlib import Path

import pandas as pd
import pytest
import yaml

from digitalmodel.engine import engine


EXPECTED_COLUMNS = [
    "period_s",
    "heading_deg",
    "surge",
    "sway",
    "heave",
    "roll",
    "pitch",
    "yaw",
    "surge_phase_deg",
    "sway_phase_deg",
    "heave_phase_deg",
    "roll_phase_deg",
    "pitch_phase_deg",
    "yaw_phase_deg",
]


def _base_cfg():
    return {
        "basename": "rao_tabulation",
        "rao_tabulation": {
            "rao_database": {
                "periods_s": [8.0, 12.0, 16.0],
                "headings_deg": [0.0, 90.0],
                "amplitudes": [
                    [
                        [0.05, 0.02, 0.60, 0.010, 0.020, 0.005],
                        [0.04, 0.03, 0.55, 0.020, 0.015, 0.006],
                    ],
                    [
                        [0.08, 0.03, 0.90, 0.015, 0.025, 0.007],
                        [0.06, 0.04, 0.85, 0.025, 0.020, 0.008],
                    ],
                    [
                        [0.10, 0.04, 1.00, 0.020, 0.030, 0.009],
                        [0.08, 0.05, 0.95, 0.030, 0.025, 0.010],
                    ],
                ],
                "phases_deg": [
                    [[0.0, 5.0, 10.0, 0.0, 2.0, 1.0], [1.0, 6.0, 12.0, 1.0, 3.0, 2.0]],
                    [[2.0, 7.0, 15.0, 2.0, 4.0, 3.0], [3.0, 8.0, 17.0, 3.0, 5.0, 4.0]],
                    [[4.0, 9.0, 20.0, 4.0, 6.0, 5.0], [5.0, 10.0, 22.0, 5.0, 7.0, 6.0]],
                ],
            },
            "query_points": [
                {"period_s": 10.0, "heading_deg": 45.0},
                {"period_s": 16.0, "heading_deg": 0.0},
            ],
            "output_dir": "results",
        },
        "default": {"log_level": "INFO", "config": {"overwrite": {"output": True}}},
    }


def _run_case(tmp_path, cfg):
    input_path = tmp_path / "case.yml"
    input_path.write_text(yaml.safe_dump(cfg, sort_keys=False))
    return engine(inputfile=str(input_path))


def test_rao_tabulation_workflow_writes_interpolated_6dof_table(tmp_path):
    result = _run_case(tmp_path, _base_cfg())

    summary = result["rao_tabulation"]
    results_csv = Path(summary["rao_csv"])
    table = pd.read_csv(results_csv)

    assert results_csv.resolve() == (tmp_path / "results" / "case_rao.csv").resolve()
    assert summary["n_query_points"] == 2
    assert summary["dofs"] == ["surge", "sway", "heave", "roll", "pitch", "yaw"]
    assert summary["peak_heave_amplitude"] == pytest.approx(1.0)
    assert summary["peak_heave_period_s"] == pytest.approx(16.0)
    assert summary["peak_heave_heading_deg"] == pytest.approx(0.0)

    assert list(table.columns) == EXPECTED_COLUMNS
    assert len(table) == 2
    interpolated_heave = table.loc[0, "heave"]
    assert interpolated_heave == pytest.approx(0.755)
    assert 0.55 < interpolated_heave < 0.90
    assert table.loc[1, "heave"] == pytest.approx(1.0)


@pytest.mark.parametrize(
    ("mutator", "message"),
    [
        (
            lambda cfg: cfg["rao_tabulation"]["rao_database"].update(
                {"amplitudes": cfg["rao_tabulation"]["rao_database"]["amplitudes"][:-1]}
            ),
            "shape",
        ),
        (
            lambda cfg: cfg["rao_tabulation"].update({"query_points": []}),
            "query_points",
        ),
        (
            lambda cfg: cfg["rao_tabulation"]["query_points"][0].update(
                {"frequency_rad_s": 1.0}
            ),
            "disagree",
        ),
        (
            lambda cfg: cfg["rao_tabulation"]["query_points"][0].update(
                {"period_s": 4.0}
            ),
            "outside the RAO database range",
        ),
    ],
)
def test_rao_tabulation_workflow_fails_closed_on_malformed_inputs(
    tmp_path,
    mutator,
    message,
):
    cfg = copy.deepcopy(_base_cfg())
    mutator(cfg)

    with pytest.raises(ValueError, match=message):
        _run_case(tmp_path, cfg)
