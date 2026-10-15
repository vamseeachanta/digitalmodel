# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Dry-run contract for queue-ready S-lay inputs, without native access."""

import json
import hashlib
from pathlib import Path, PureWindowsPath
import subprocess
import sys

import pytest

ROOT = Path(__file__).resolve().parents[3]
SCRIPT = ROOT / "scripts/orcaflex/prepare_slay_campaign.py"
CAMPAIGN = ROOT / "docs/domains/pipeline-installation/slay-campaign"


def dry_run(campaign, output, *options):
    code = '''
import importlib.abc, runpy, sys
class DenyNative(importlib.abc.MetaPathFinder):
    def find_spec(self, fullname, path=None, target=None):
        if fullname.startswith(("OrcFxAPI", "digitalmodel.solvers.orcaflex.orcaflex_api",
                                "digitalmodel.workflows.orcaflex_run_batch")):
            raise AssertionError("Native access in dry run")
sys.meta_path.insert(0, DenyNative())
sys.argv = sys.argv[1:]
runpy.run_path(sys.argv[0], run_name="__main__")
'''
    return subprocess.run([sys.executable, "-c", code, str(SCRIPT),
        "--campaign", str(campaign), "--output", str(output), "--dry-run",
        "--queue", "--expected-hours", "1", "--max-hours", "2", *options],
        capture_output=True, text=True)


@pytest.fixture
def prepared_queue_inputs(tmp_path, monkeypatch):
    """Exercise queue serialization separately from checkout cleanliness."""
    import importlib.abc
    import importlib.util
    from digitalmodel.solvers.orcaflex.modular_generator.slay_execution import read_contract

    class DenyNative(importlib.abc.MetaPathFinder):
        def find_spec(self, fullname, path=None, target=None):
            if fullname.startswith(("OrcFxAPI", "digitalmodel.solvers.orcaflex.orcaflex_api",
                                    "digitalmodel.workflows.orcaflex_run_batch")):
                raise AssertionError("Native access in dry run")

    monkeypatch.setattr(sys, "meta_path", [DenyNative(), *sys.meta_path])
    module_spec = importlib.util.spec_from_file_location("preparation", SCRIPT)
    driver = importlib.util.module_from_spec(module_spec)
    module_spec.loader.exec_module(driver)
    output = tmp_path / "prepared"
    driver.prepare(CAMPAIGN, output)
    return driver, output, read_contract(CAMPAIGN, 1, 2)


def test_dry_run_writes_pinned_queue_canary_and_keeps_acceptance_open(prepared_queue_inputs):
    driver, output, contract = prepared_queue_inputs
    driver.write_queue(CAMPAIGN, output, ROOT, "a" * 40, contract)
    run = json.loads((output / "run-manifest.json").read_text(encoding="utf-8"))
    queue_path = output / "queue/slay-d0-t0.json"
    queue = json.loads(queue_path.read_text(encoding="utf-8"))
    assert run["launch_enabled"] is False
    assert run["native_attempted"] == run["native_completed"] == run["qualified_results"] == 0
    assert run["approval_required"]["manifest_sha256"] == hashlib.sha256(queue_path.read_bytes()).hexdigest()
    for relative, digest in run["input_byte_sha256"].items():
        assert hashlib.sha256((output / relative).read_bytes()).hexdigest() == digest
    assert run["criteria"]["installation_acceptance"]["status"] == "not established"
    assert run["first_timed_case"]["case_id"] == "d0-t0"
    assert queue["id"] == queue_path.stem
    assert len(queue["ref"]) == 40
    assert queue["driver"][:3] == [sys.executable, "-m", "digitalmodel"]
    assert queue["progress"]["total"] == 1
    assert run["remaining_cases_status"] == "held pending canary readback and criteria"
    batch = json.loads(Path(queue["driver"][3]).read_text(encoding="utf-8"))
    settings = batch["orcaflex_run_batch"]
    assert settings["analysis"]["type"] == "both"
    assert settings["run_batch"]["workers"] == settings["run_batch"]["solver_threads"] == 1
    assert settings["run_batch"]["mock"] is False
    assert settings["models"]["files"] == [str(output / "models/d0-t0/master.yml")]


@pytest.mark.parametrize("expected,maximum", [("nan", "2"), ("2", "1"), ("0", "2")])
def test_invalid_queue_budget_leaves_output_untouched(tmp_path, expected, maximum):
    output = tmp_path / "prepared"
    result = dry_run(CAMPAIGN, output, "--expected-hours", expected, "--max-hours", maximum)
    assert result.returncode != 0
    assert not output.exists()


def test_missing_criteria_source_leaves_output_untouched(tmp_path):
    import shutil
    source = tmp_path / "source"
    source.mkdir()
    for name in ("case-table.yml", "manifest.template.json", "criteria.json", "first-timed-case.json"):
        shutil.copyfile(CAMPAIGN / name, source / name)
    criteria_path = source / "criteria.json"
    criteria = json.loads(criteria_path.read_text(encoding="utf-8"))
    criteria["criteria"]["geometry"]["source"] = {}
    criteria_path.write_text(json.dumps(criteria), encoding="utf-8")
    output = tmp_path / "prepared"
    result = dry_run(source, output)
    assert result.returncode != 0
    assert not output.exists()


@pytest.mark.parametrize("fault", ["model_path", "threads", "missing_criterion", "missing_readback"])
def test_incomplete_or_changed_canary_contract_leaves_output_untouched(tmp_path, fault):
    import shutil
    source = tmp_path / "source"
    source.mkdir()
    for name in ("case-table.yml", "manifest.template.json", "criteria.json", "first-timed-case.json"):
        shutil.copyfile(CAMPAIGN / name, source / name)
    first_path = source / "first-timed-case.json"
    first = json.loads(first_path.read_text(encoding="utf-8"))
    if fault == "model_path":
        first["model_locator"] = "../outside.yml"
    elif fault == "threads":
        first["solver_threads"] = 2
    elif fault == "missing_readback":
        first["readback"] = {}
    else:
        path = source / "criteria.json"
        criteria = json.loads(path.read_text(encoding="utf-8"))
        del criteria["criteria"]["installation_acceptance"]
        path.write_text(json.dumps(criteria), encoding="utf-8")
    first_path.write_text(json.dumps(first), encoding="utf-8")
    output = tmp_path / "prepared"
    result = dry_run(source, output)
    assert result.returncode != 0
    assert not output.exists()


def test_source_drift_prevents_queue_creation(tmp_path):
    import importlib.util
    import shutil
    from digitalmodel.solvers.orcaflex.modular_generator.slay_execution import read_contract, write_queue
    source = tmp_path / "source"
    source.mkdir()
    for name in ("case-table.yml", "manifest.template.json", "criteria.json", "first-timed-case.json"):
        shutil.copyfile(CAMPAIGN / name, source / name)
    contract = read_contract(source, 1, 2)
    module_spec = importlib.util.spec_from_file_location("preparation", SCRIPT)
    driver = importlib.util.module_from_spec(module_spec)
    module_spec.loader.exec_module(driver)
    output = tmp_path / "prepared"
    driver.prepare(source, output)
    path = source / "criteria.json"
    path.write_bytes(path.read_bytes() + b"\n")
    with pytest.raises(ValueError, match="changed during preparation"):
        write_queue(source, output, ROOT, "a" * 40, contract)
    assert not (output / "queue").exists()


def test_queue_input_keys_use_posix_separators(prepared_queue_inputs, monkeypatch):
    """Model input keys match preparation keys on either path platform."""
    driver, output, contract = prepared_queue_inputs
    relative_to = Path.relative_to

    def windows_relative(path, *parents):
        relative = relative_to(path, *parents)
        if parents == (output.resolve(),) and relative.parts[0] == "models":
            return PureWindowsPath(relative.as_posix())
        return relative

    monkeypatch.setattr(Path, "relative_to", windows_relative)
    record = driver.write_queue(CAMPAIGN, output, ROOT, "a" * 40, contract)
    prepared = json.loads((output / "preparation.json").read_text())
    expected = {name for case in prepared["cases"] for name in case["files"]}
    assert set(record["input_byte_sha256"]) == expected | {"native-input.json"}


@pytest.mark.parametrize("status", [" M tracked.py\n", "M  tracked.py\n", "?? added.py\n"])
def test_dirty_checkout_refuses_queue_before_writing(tmp_path, monkeypatch, capsys, status):
    """A commit pin cannot describe staged, unstaged or untracked inputs."""
    import importlib.util
    module_spec = importlib.util.spec_from_file_location("preparation", SCRIPT)
    driver = importlib.util.module_from_spec(module_spec)
    module_spec.loader.exec_module(driver)
    output = tmp_path / "prepared"
    monkeypatch.setattr(sys, "argv", [str(SCRIPT), "--campaign", str(CAMPAIGN),
        "--output", str(output), "--dry-run", "--queue", "--expected-hours", "1", "--max-hours", "2"])
    calls = []

    def git_result(command, **kwargs):
        calls.append(command)
        if command[-2:] == ["status", "--porcelain"]:
            return subprocess.CompletedProcess(command, 0, status, "")
        return subprocess.CompletedProcess(command, 0, "a" * 40 + "\n", "")

    monkeypatch.setattr(driver.subprocess, "run", git_result)
    with pytest.raises(SystemExit, match="2"):
        driver.main()
    assert "requires a clean working tree" in capsys.readouterr().err
    assert calls[0][-2:] == ["status", "--porcelain"]
    assert not output.exists()


def test_clean_checkout_cli_writes_queue(prepared_queue_inputs, monkeypatch):
    """Keep the CLI preparation and queue wiring covered with clean Git responses."""
    driver, prepared, contract = prepared_queue_inputs
    output = prepared.parent / "clean-cli"
    monkeypatch.setattr(sys, "argv", [str(SCRIPT), "--campaign", str(CAMPAIGN),
        "--output", str(output), "--dry-run", "--queue", "--expected-hours", "1", "--max-hours", "2"])
    calls = []

    def git_result(command, **kwargs):
        calls.append(command[-2:])
        assert all(key not in kwargs["env"] for key in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"))
        text = "" if command[-2:] == ["status", "--porcelain"] else "a" * 40 + "\n"
        return subprocess.CompletedProcess(command, 0, text, "")

    monkeypatch.setattr(driver.subprocess, "run", git_result)
    assert driver.main() == 0
    assert calls == [["status", "--porcelain"], ["rev-parse", "HEAD"]]
    queue = json.loads((output / "queue/slay-d0-t0.json").read_text())
    record = json.loads((output / "run-manifest.json").read_text())
    assert queue["ref"] == record["input_revision"] == "a" * 40
    assert queue["driver"][:3] == [sys.executable, "-m", "digitalmodel"]
    assert record["launch_enabled"] is False
