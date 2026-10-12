"""Engine-level required-input, applicability and envelope screening checks."""

import copy
import hashlib
from pathlib import Path

import pandas as pd
import pytest
import yaml

from digitalmodel.engine import engine

REPO_ROOT = Path(__file__).resolve().parents[2]
EXAMPLE = REPO_ROOT / "examples/workflows/riser-viv-screening/input.yml"


@pytest.fixture
def screening_input():
    return {
        "basename": "viv_screening",
        "viv_screening": {
            "design_life_years": 20.0,
            "dff": 3.0,
            "structure": {
                "length_m": 60.0,
                "outer_diameter_m": 0.2731,
                "inner_diameter_m": 0.2287,
                "youngs_modulus_Pa": 2.07e11,
                "mass_per_length_kg_m": 180.0,
                "effective_tension_N": 0.0,
                "boundary": "pinned_pinned",
            },
            "environment": {
                "kinematic_viscosity_m2_s": 1.19e-6,
                "mass_ratio": 1.8,
                "damping_ratio": 0.01,
                "current": {"speed_m_s": 0.22},
                "exposure_fraction": 0.05,
            },
            "unit_modal_stress_MPa": 45.0,
        },
        "default": {
            "log_level": "INFO",
            "config": {"overwrite": {"output": True}},
        },
    }


def run_input(tmp_path, data):
    path = tmp_path / "input.yml"
    path.write_text(yaml.safe_dump(data), encoding="utf-8")
    return engine(inputfile=str(path))["viv_screening"]


def assert_escalated(result, *triggers):
    assert result["screening_status"] == "escalate"
    assert result["escalation"]["required"] is True
    assert result["screening_damage_rate_per_year"] is None
    assert result["screening_life_years"] is None
    assert result["dff_margin"] is None
    for trigger in triggers:
        assert trigger in result["escalation"]["reason"]
    for path_key in ("results_csv", "summary_csv"):
        table = pd.read_csv(result[path_key])
        damage_columns = [
            key for key in table if "damage" in key or key == "screening_life_years"
        ]
        assert table[damage_columns].isna().all().all()


def test_viv_screening_workflow_escalates_out_of_applicability(
    tmp_path, screening_input
):
    del screening_input["viv_screening"]["unit_modal_stress_MPa"]
    result = run_input(tmp_path, screening_input)
    assert_escalated(result, "T1", "unit_modal_stress_MPa")


def test_multimode_near_peak_escalates(tmp_path, screening_input):
    settings = screening_input["viv_screening"]
    settings["structure"].update(length_m=350.0, effective_tension_N=1.2e6)
    settings["environment"]["current"]["speed_m_s"] = 0.9
    assert_escalated(run_input(tmp_path, screening_input), "T2", "T3")


@pytest.mark.parametrize("speed", [0.001, 50.0])
def test_reynolds_window_escalates(tmp_path, screening_input, speed):
    screening_input["viv_screening"]["environment"]["current"]["speed_m_s"] = speed
    assert_escalated(run_input(tmp_path, screening_input), "T5")


def test_unchecked_modes_escalate(tmp_path, screening_input):
    settings = screening_input["viv_screening"]
    settings["n_modes"] = 1
    settings["environment"]["current"]["speed_m_s"] = 0.25
    assert_escalated(run_input(tmp_path, screening_input), "T4")


@pytest.mark.parametrize("required", ["dff", "design_life_years"])
def test_engine_keeps_verdict_inputs_required(tmp_path, screening_input, required):
    del screening_input["viv_screening"][required]
    with pytest.raises(ValueError, match=f"viv_screening.*{required}"):
        run_input(tmp_path, screening_input)


@pytest.mark.parametrize(
    "group,key",
    [
        ("structure", "length_m"),
        ("structure", "outer_diameter_m"),
        ("structure", "inner_diameter_m"),
        ("structure", "youngs_modulus_Pa"),
        ("structure", "mass_per_length_kg_m"),
        ("structure", "effective_tension_N"),
        ("structure", "boundary"),
        ("environment", "kinematic_viscosity_m2_s"),
        ("environment", "mass_ratio"),
        ("environment", "damping_ratio"),
        ("environment", "exposure_fraction"),
        ("environment", "current"),
    ],
)
def test_engine_keeps_physics_inputs_required(tmp_path, screening_input, group, key):
    del screening_input["viv_screening"][group][key]
    with pytest.raises(ValueError, match=f"viv_screening.*{key}"):
        run_input(tmp_path, screening_input)


@pytest.mark.parametrize(
    "key,value",
    [
        ("dff", float("nan")),
        ("dff", True),
        ("dff", 0),
        ("design_life_years", float("inf")),
        ("n_modes", 1.5),
        ("n_modes", False),
        ("unit_modal_stress_MPa", -1),
        ("lock_in_vr_range", [4, 8]),
    ],
)
def test_invalid_settings_fail_closed(tmp_path, screening_input, key, value):
    screening_input["viv_screening"][key] = value
    with pytest.raises(ValueError, match="viv_screening"):
        run_input(tmp_path, screening_input)


@pytest.mark.parametrize(
    "current",
    [
        {},
        {"speed_m_s": 0},
        {"speed_m_s": -1},
        {"profile": []},
        {"profile": [{"depth_m": 0, "speed_m_s": 0}]},
        {"profile": [{"speed_m_s": 0.22}]},
        {"speed_m_s": 0.22, "profile": [{"depth_m": 0, "speed_m_s": 0.22}]},
    ],
)
def test_invalid_current_fails_closed(tmp_path, screening_input, current):
    screening_input["viv_screening"]["environment"]["current"] = current
    with pytest.raises(ValueError, match="viv_screening"):
        run_input(tmp_path, screening_input)


@pytest.mark.parametrize(
    "group,key,value",
    [
        ("structure", "inner_diameter_m", 0.30),
        ("structure", "effective_tension_N", -1),
        ("structure", "boundary", "unknown"),
        ("environment", "damping_ratio", float("nan")),
        ("environment", "exposure_fraction", 1.1),
        ("environment", "lock_in_vr_range", [4, 8]),
    ],
)
def test_invalid_physics_fails_closed(tmp_path, screening_input, group, key, value):
    screening_input["viv_screening"][group][key] = value
    with pytest.raises(ValueError, match="viv_screening"):
        run_input(tmp_path, screening_input)


def test_damage_rate_is_annual_and_verdict_uses_dff(tmp_path, screening_input):
    first = run_input(tmp_path, screening_input)
    settings = screening_input["viv_screening"]
    settings.update(design_life_years=1.0, dff=1.0)
    second = run_input(tmp_path, screening_input)
    assert first["screening_status"] == "fail"
    assert second["screening_status"] == "pass"
    assert second["screening_damage_rate_per_year"] == pytest.approx(
        first["screening_damage_rate_per_year"]
    )


def test_profile_intersection_and_il_only_requires_stress(tmp_path, screening_input):
    settings = screening_input["viv_screening"]
    settings["environment"]["current"] = {
        "profile": [
            {"depth_m": 0, "speed_m_s": 0.20},
            {"depth_m": 60, "speed_m_s": 0.22},
        ]
    }
    result = run_input(tmp_path, screening_input)
    assert result["lock_in_modes"] == [1, 2]
    assert result["critical_vr"] == pytest.approx(4.621509426631515)
    settings["environment"]["current"] = {"speed_m_s": 0.10}
    del settings["unit_modal_stress_MPa"]
    assert_escalated(run_input(tmp_path, screening_input), "T1")


def test_no_lock_in_needs_no_stress(tmp_path, screening_input):
    settings = screening_input["viv_screening"]
    settings["environment"]["current"] = {"speed_m_s": 0.01}
    del settings["unit_modal_stress_MPa"]
    result = run_input(tmp_path, screening_input)
    assert result["screening_status"] == "pass"
    assert result["lock_in_modes"] == []
    assert result["critical_mode"] is None
    assert result["screening_damage_rate_per_year"] == 0
    assert result["screening_life_years"] == float("inf")


def test_committed_example_runs_through_engine_and_is_stable(tmp_path):
    data = yaml.safe_load(EXAMPLE.read_text(encoding="utf-8"))
    result = run_input(tmp_path, copy.deepcopy(data))
    assert result["screening_status"] == "fail"
    assert result["il_lock_in_unassessed"] is True
    paths = [
        tmp_path / "results/input.yml",
        Path(result["results_csv"]),
        Path(result["summary_csv"]),
    ]
    before = [hashlib.sha256(path.read_bytes()).hexdigest() for path in paths]
    run_input(tmp_path, data)
    assert before == [hashlib.sha256(path.read_bytes()).hexdigest() for path in paths]
    echo = paths[0].read_text(encoding="utf-8")
    assert str(tmp_path) not in echo
    assert "start_time" not in echo


def test_il_only_lock_in_is_unassessed(tmp_path, screening_input):
    screening_input["viv_screening"]["environment"]["current"] = {"speed_m_s": 0.10}
    result = run_input(tmp_path, screening_input)
    assert result["cf_lock_in_count"] == 0
    assert result["il_lock_in_count"] == 1
    assert result["screening_status"] == "pass"
    assert result["screening_damage_rate_per_year"] == 0.0
    assert result["screening_life_years"] == float("inf")
    assert result.get("il_lock_in_unassessed") is True
    summary = pd.read_csv(result["summary_csv"]).iloc[0]
    assert bool(summary["il_lock_in_unassessed"]) is True


def test_no_il_lock_in_flag_is_unset(tmp_path, screening_input):
    settings = screening_input["viv_screening"]
    settings["environment"]["current"] = {"speed_m_s": 0.01}
    del settings["unit_modal_stress_MPa"]
    result = run_input(tmp_path, screening_input)
    assert result["il_lock_in_count"] == 0
    assert result["screening_status"] == "pass"
    assert result["screening_damage_rate_per_year"] == 0.0
    assert result["screening_life_years"] == float("inf")
    assert result.get("il_lock_in_unassessed") is False
    summary = pd.read_csv(result["summary_csv"]).iloc[0]
    assert bool(summary["il_lock_in_unassessed"]) is False


def test_mixed_lock_in_preserves_example_totals(tmp_path):
    data = yaml.safe_load(EXAMPLE.read_text(encoding="utf-8"))
    result = run_input(tmp_path, data)
    assert result["lock_in_modes"] == [1, 2]
    assert result["cf_lock_in_count"] == result["il_lock_in_count"] == 1
    assert result["screening_status"] == "fail"
    assert result["screening_damage_rate_per_year"] == 0.09283365625182306
    assert result["screening_life_years"] == 10.771955348687046
    assert result.get("il_lock_in_unassessed") is True
    summary = pd.read_csv(result["summary_csv"]).iloc[0]
    assert bool(summary["il_lock_in_unassessed"]) is True


@pytest.mark.parametrize("absolute", [False, True], ids=["relative", "absolute"])
def test_custom_output_dir_contains_all_workflow_files(
    tmp_path, screening_input, absolute
):
    directory = tmp_path / "requested_output"
    screening_input["viv_screening"]["output_dir"] = (
        str(directory) if absolute else directory.name
    )
    result = run_input(tmp_path, screening_input)
    names = ["input.yml", "input_viv_screening.csv", "input_viv_screening_summary.csv"]
    assert Path(result["results_csv"]).parent == directory
    assert Path(result["summary_csv"]).parent == directory
    for name in names:
        assert (directory / name).is_file(), name
        assert not (tmp_path / "results" / name).exists(), name


def test_default_output_locations_are_preserved(tmp_path, screening_input):
    result = run_input(tmp_path, screening_input)
    directory = tmp_path / "results"
    assert Path(result["results_csv"]) == directory / "input_viv_screening.csv"
    assert Path(result["summary_csv"]) == directory / "input_viv_screening_summary.csv"
    for name in (
        "input.yml", "input_viv_screening.csv", "input_viv_screening_summary.csv"
    ):
        assert (directory / name).is_file()
