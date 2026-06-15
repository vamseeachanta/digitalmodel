from pathlib import Path
import math

import pandas as pd
import pytest


REPO_ROOT = Path(__file__).resolve().parents[2]


def _repo_path(path: str) -> Path:
    result = Path(path)
    if not result.is_absolute():
        result = REPO_ROOT / result
    return result


def assert_hydrodynamics_workflow(workflow_id: str, cfg: dict) -> None:
    if workflow_id == "hydro-coefficients":
        _assert_hydro_coefficients(cfg)
        return
    if workflow_id == "wave-spectra":
        _assert_wave_spectra(cfg)
        return
    if workflow_id == "passing-ship":
        _assert_passing_ship(cfg)
        return
    raise AssertionError(f"Missing hydrodynamics workflow assertion for {workflow_id}")


def _assert_hydro_coefficients(cfg: dict) -> None:
    summary = cfg["hydro_coefficients"]
    query_csv = _repo_path(summary["query_csv"])
    queries = pd.read_csv(query_csv)

    assert summary["n_frequencies"] == 2
    assert summary["frequency_min_rad_s"] == pytest.approx(0.5)
    assert summary["frequency_max_rad_s"] == pytest.approx(1.5)
    assert summary["causality"]["dof_i"] == "Surge"
    assert summary["causality"]["dof_j"] == "Surge"
    assert summary["causality"]["is_valid"] is True
    assert summary["causality"]["max_error"] == pytest.approx(0.0, abs=1.0e-9)

    assert len(queries) == 2
    heave = queries.loc[queries["dof_i"] == "Heave"].iloc[0]
    assert heave["frequency_rad_s"] == pytest.approx(1.0)
    assert heave["added_mass"] == pytest.approx(345.0)
    assert heave["damping"] == pytest.approx(48.0)


def _assert_wave_spectra(cfg: dict) -> None:
    summary = cfg["wave_spectra"]
    spectrum_csv = _repo_path(summary["spectrum_csv"])
    spectrum = pd.read_csv(spectrum_csv)
    omega_p = 2.0 * math.pi / summary["Tp"]

    assert summary["spectrum"] == "pierson_moskowitz"
    assert summary["Hs_input"] == pytest.approx(2.5)
    assert summary["Hs_check"] == pytest.approx(2.5, rel=0.05)
    assert summary["m0"] == pytest.approx(2.5**2 / 16.0, rel=0.05)
    assert summary["peak_frequency"] == pytest.approx(omega_p, abs=0.02)
    assert summary["points"] == 231
    assert len(spectrum) == 231
    assert (spectrum["S"] >= 0.0).all()


def _assert_passing_ship(cfg: dict) -> None:
    summary = cfg["passing_ship"]
    results_csv = _repo_path(summary["results_csv"])
    results = pd.read_csv(results_csv)
    row = results.iloc[0]

    assert summary["formulation"] == "wang_infinite_depth"
    assert summary["separation_m"] == pytest.approx(100.0)
    assert summary["stagger_m"] == pytest.approx(50.0)
    assert summary["velocity_m_s"] == pytest.approx(4.0)
    assert summary["forces"]["surge_N"] == pytest.approx(106667.508431)
    assert summary["forces"]["sway_N"] == pytest.approx(146542.545189)
    assert summary["forces"]["yaw_Nm"] == pytest.approx(-4165048.806963)

    assert len(results) == 1
    assert row["surge_N"] == pytest.approx(summary["forces"]["surge_N"])
    assert row["sway_N"] == pytest.approx(summary["forces"]["sway_N"])
    assert row["yaw_Nm"] == pytest.approx(summary["forces"]["yaw_Nm"])
