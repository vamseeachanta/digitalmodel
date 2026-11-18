# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Exercise the standalone synthetic YAML example without writing caller files."""

import importlib.util
import os
from pathlib import Path
import subprocess
import sys

import pytest

ROOT = Path(__file__).resolve().parents[2]
RUNNER = ROOT / "examples/cathodic_protection/run_design.py"
TEMPLATE = RUNNER.with_name("synthetic_design.yml")


def invoke(cwd, *args):
    env = {**os.environ, "PYTHONPATH": str(ROOT / "src")}
    return subprocess.run(
        [sys.executable, str(RUNNER), *map(str, args)],
        cwd=cwd, env=env, capture_output=True, text=True, timeout=30,
    )


def snapshot(folder):
    return {p.name: p.read_bytes() for p in folder.iterdir() if p.is_file()}


def test_default_is_repeatable_from_unrelated_cwd(tmp_path):
    before = TEMPLATE.read_bytes()
    first, second = invoke(tmp_path), invoke(tmp_path)
    assert first.returncode == second.returncode == 0, first.stderr
    assert first.stdout == second.stdout
    assert first.stderr == second.stderr == ""
    assert "Mean current demand: 1.2 A" in first.stdout
    assert "Lifetime charge: 105120.0 Ah" in first.stdout
    assert "Required net anode mass: 87.6 kg" in first.stdout
    from digitalmodel.cathodic_protection.design_spec import design_from_yaml

    assert design_from_yaml(str(TEMPLATE))["limitation"] in first.stdout
    assert list(tmp_path.iterdir()) == []
    assert TEMPLATE.read_bytes() == before


def test_explicit_config_takes_precedence(tmp_path):
    config = tmp_path / "custom.yml"
    config.write_text(TEMPLATE.read_text().replace("area_m2: 120.0", "area_m2: 240.0"))
    before = snapshot(tmp_path)
    result = invoke(tmp_path, "--config", config.name)
    assert result.returncode == 0, result.stderr
    assert "Mean current demand: 2.4 A" in result.stdout
    assert "Lifetime charge: 210240.0 Ah" in result.stdout
    assert "Required net anode mass: 175.2 kg" in result.stdout
    assert snapshot(tmp_path) == before


@pytest.mark.parametrize("kind", ["missing", "malformed", "duplicate", "unsafe", "units", "range"])
def test_invalid_config_fails_without_results(tmp_path, kind):
    config, marker = tmp_path / "invalid.yml", tmp_path / "must-not-exist"
    text = TEMPLATE.read_text()
    documents = {
        "malformed": "basis_description: [",
        "duplicate": text + "\narea_m2: 240.0\n",
        "unsafe": "!!python/object/apply:pathlib.Path.write_text\n"
        + f"- {marker}\n- unexpected\n",
        "units": text.replace("area_m2: m2", "area_m2: unsupported-unit"),
        "range": text.replace("area_m2: 120.0", "area_m2: 1.0e+308")
        .replace("current_density_A_m2: 0.05", "current_density_A_m2: 10.0"),
    }
    if kind != "missing":
        config.write_text(documents[kind], encoding="utf-8")
    before = snapshot(tmp_path)
    result = invoke(tmp_path, "--config", config)
    assert result.returncode == 1
    assert result.stdout == ""
    assert result.stderr.startswith("CP design failed:")
    assert not marker.exists()
    assert snapshot(tmp_path) == before


def test_import_has_no_execution_and_unexpected_errors_propagate(tmp_path, monkeypatch, capsys):
    import digitalmodel.cathodic_protection.design_spec as workflow

    calls = []

    def unexpected(path):
        calls.append(path)
        raise RuntimeError("unexpected calculation error")

    monkeypatch.setattr(workflow, "design_from_yaml", unexpected)
    monkeypatch.chdir(tmp_path)
    spec = importlib.util.spec_from_file_location("design_example", RUNNER)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    assert calls == []
    assert capsys.readouterr() == ("", "")
    with pytest.raises(RuntimeError, match="unexpected calculation error"):
        module.main([])
    assert calls == [str(TEMPLATE)]
    assert capsys.readouterr() == ("", "")
    assert list(tmp_path.iterdir()) == []
