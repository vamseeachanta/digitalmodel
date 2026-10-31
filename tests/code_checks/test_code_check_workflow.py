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
    input_path.write_text(
        yaml.safe_dump(cfg, sort_keys=False), encoding="utf-8", newline="\n"
    )

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
    assert b"\r" not in results_csv.read_bytes()


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
    input_path.write_text(
        yaml.safe_dump(cfg, sort_keys=False), encoding="utf-8", newline="\n"
    )

    with pytest.raises(ValueError, match="Unsupported code_check code"):
        engine(inputfile=str(input_path))


@pytest.mark.parametrize(
    "target_utilisation, expected_pass",
    [
        pytest.param(1.0 - 0.00002, True, id="below-limit"),
        pytest.param(1.0 + 0.00002, False, id="above-limit"),
    ],
)
def test_workflow_preserves_unrounded_failure(
    tmp_path, target_utilisation, expected_pass
):
    """Synthetic API tension control, derived without production capacities.

    OD=2 m, wall=0.5 m, SMYS=1000 Pa and derating=1 give Ty=3*pi/4 kN.
    Design factor=0.5 and M=0 give u=T/(3*pi/8). SMTS=2000 Pa is explicit.
    The chosen u=1+/-0.00002 values both display as 1.0 to four decimals.
    Verdicts must survive configuration routing, summary formatting and CSV.
    """
    cfg = {
        "basename": "code_check",
        "code_check": {
            "code": "api-rp-2rd",
            "inputs": {
                "outer_diameter": 2.0,
                "wall_thickness": 0.5,
                "smys": 1000.0,
                "smts": 2000.0,
                "temperature_derating": 1.0,
                "design_factor": 0.5,
                "arc_lengths": [0.0],
                "effective_tension_kN": [
                    (3.0 * math.pi / 8.0) * target_utilisation
                ],
                "bending_moment_kNm": [0.0],
                "pressure_diff_MPa": [0.0],
            },
            "output_dir": "results",
        },
        "default": {"log_level": "INFO", "config": {"overwrite": {"output": True}}},
    }
    input_path = tmp_path / "boundary.yml"
    input_path.write_text(
        yaml.safe_dump(cfg, sort_keys=False), encoding="utf-8", newline="\n"
    )

    result = engine(inputfile=str(input_path))
    summary = result["code_check"]
    table = pd.read_csv(summary["results_csv"])

    assert math.isfinite(summary["governing_utilisation"])
    assert summary["governing_utilisation"] == 1.0
    assert summary["pass"] is expected_pass
    assert summary["governing_result"]["pass"] is expected_pass
    assert summary["results"][0]["pass"] is expected_pass
    assert table["utilisation"].tolist() == [1.0]
    assert table["pass"].tolist() == [expected_pass]


def _two_position_inputs(code, samples):
    """Both synthetic stations display unity; only the 50 m station fails.

    The API tension capacity with OD=2, ID=1, SMYS=1000 and factor=0.5
    is 3*pi/8 kN. The existing DNV contract with wall=0.5, fab=0.8 and
    SMYS=1000 gives Mp=3367/3840 kN.m and gamma_SC*gamma_m=57/40.
    Thus T=(3*pi/8)*u for API, or M=Mp*sqrt((40/57)*u) for DNV,
    gives u=0.99998 at 0 m and u=1.00002 at 50 m independently.
    These synthetic controls exercise selection, not standards qualification.
    """
    inputs = {
        "outer_diameter": 2.0,
        "wall_thickness": 0.5,
        "smys": 1000.0,
        "smts": 2000.0,
        "temperature_derating": 1.0,
        "design_factor": 0.5,
        "arc_lengths": [position for position, _ in samples],
        "effective_tension_kN": [
            (3.0 * math.pi / 8.0) * u for _, u in samples
        ],
        "bending_moment_kNm": [0.0, 0.0],
    }
    expected_tension = (3.0 * math.pi / 8.0) * 1.00002
    expected_moment = 0.0
    if code == "dnv-os-f201":
        inputs.update(
            smts=2300.0,
            safety_class="normal",
            fab_factor=0.8,
            material_factor=1.25,
            condition_factor=1.0,
            effective_tension_kN=[0.0, 0.0],
            bending_moment_kNm=[
                (3367.0 / 3840.0) * math.sqrt((40.0 / 57.0) * u)
                for _, u in samples
            ],
        )
        expected_tension = 0.0
        expected_moment = (3367.0 / 3840.0) * math.sqrt((40.0 / 57.0) * 1.00002)
    return inputs, expected_tension, expected_moment


@pytest.mark.parametrize("code", ["api-rp-2rd", "dnv-os-f201"])
@pytest.mark.parametrize("failing_first", [False, True])
def test_workflow_governing_row_prefers_failure_at_rounded_maximum(
    tmp_path, code, failing_first
):
    """The failed station governs in either order when displayed maxima tie."""
    samples = [(0.0, 0.99998), (50.0, 1.00002)]
    if failing_first:
        samples.reverse()
    inputs, expected_tension, expected_moment = _two_position_inputs(code, samples)
    cfg = {
        "basename": "code_check",
        "code_check": {"code": code, "inputs": inputs, "output_dir": "results"},
        "default": {"log_level": "INFO", "config": {"overwrite": {"output": True}}},
    }
    input_path = tmp_path / "two_positions.yml"
    input_path.write_text(
        yaml.safe_dump(cfg, sort_keys=False), encoding="utf-8", newline="\n"
    )

    summary = engine(inputfile=str(input_path))["code_check"]
    governing = summary["governing_result"]
    table = pd.read_csv(summary["results_csv"], encoding="utf-8")

    assert summary["points"] == 2
    assert summary["pass"] is False
    assert summary["governing_utilisation"] == 1.0
    assert governing["pass"] is False
    assert governing["arc_length"] == 50.0
    assert governing["tension_kN"] == pytest.approx(expected_tension)
    assert governing["bending_moment_kNm"] == pytest.approx(expected_moment)
    expected_passes = [not failing_first, failing_first]
    assert [row["pass"] for row in summary["results"]] == expected_passes
    assert table["pass"].tolist() == expected_passes
    assert table["utilisation"].tolist() == [1.0, 1.0]
    assert table["arc_length"].tolist() == inputs["arc_lengths"]
