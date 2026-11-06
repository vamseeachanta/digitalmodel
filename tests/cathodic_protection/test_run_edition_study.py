"""Public edition-study example: pinned calculations and command-line checks."""

from __future__ import annotations

import importlib.util
import os
from pathlib import Path
import subprocess
import sys
import tomllib

import pytest

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "examples/cathodic_protection/run_edition_study.py"


@pytest.fixture
def study():
    spec = importlib.util.spec_from_file_location("run_edition_study", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@pytest.mark.parametrize(
    ("edition", "current_a", "mass_kg", "count"),
    [("2010", 1.3279, 218.111, 9), ("2019", 5.1514, 846.119, 34)],
)
def test_study_reproduces_public_pipeline(study, edition, current_a, mass_kg, count):
    results = study.run_study()
    assert set(results) == {"2010", "2019"}
    row = results[edition]
    assert row["mean_current_a"] == pytest.approx(current_a, abs=0.00005)
    assert row["required_mass_kg"] == pytest.approx(mass_kg, abs=0.0005)
    assert row["anode_count"] == count
    # The runner's independent check values also remain pinned to these numbers.
    assert study.EXPECTED_RESULTS[edition] == {
        "mean_current_a": current_a,
        "required_mass_kg": mass_kg,
        "anode_count": count,
    }
    study.check_results(results)


@pytest.mark.parametrize("field", ["mean_current_a", "required_mass_kg", "anode_count"])
def test_check_detects_changed_calculations(study, field):
    results = study.run_study()
    results["2019"][field] += 1
    with pytest.raises(ValueError, match=f"2019.*{field}"):
        study.check_results(results)


def test_main_reports_failed_check(study, monkeypatch, capsys):
    results = study.run_study()
    results["2010"]["anode_count"] = 10
    monkeypatch.setattr(study, "run_study", lambda: results)
    assert study.main() == 1
    assert "2010" in capsys.readouterr().err


def test_cli_runs_outside_checkout_with_optimization(tmp_path):
    env = os.environ.copy()
    env["PYTHONPATH"] = str(ROOT / "src")
    env["PYTHONDONTWRITEBYTECODE"] = "1"
    result = subprocess.run(
        [sys.executable, "-B", "-O", str(SCRIPT)],
        cwd=tmp_path,
        env=env,
        capture_output=True,
        text=True,
        timeout=30,
        creationflags=getattr(subprocess, "CREATE_NO_WINDOW", 0),
    )
    assert result.returncode == 0, result.stderr
    assert "Mean current [A]" in result.stdout
    assert "Required mass [kg]" in result.stdout
    assert "Anodes" in result.stdout
    assert "2010" in result.stdout and "218.111" in result.stdout
    assert "2019" in result.stdout and "846.119" in result.stdout
    assert "Checks passed" in result.stdout


def test_assetutilities_uses_public_dependency_default():
    config = tomllib.loads((ROOT / "pyproject.toml").read_text(encoding="utf-8"))
    assert "assetutilities" not in config.get("tool", {}).get("uv", {}).get(
        "sources", {}
    )
    assert any(
        dep.startswith("assetutilities>=") for dep in config["project"]["dependencies"]
    )
