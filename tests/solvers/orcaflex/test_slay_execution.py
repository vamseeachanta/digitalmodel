# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Dry-run contract for queue-ready S-lay inputs, without native access."""

import json
import hashlib
from pathlib import Path
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


def test_dry_run_writes_pinned_queue_canary_and_keeps_acceptance_open(tmp_path):
    result = dry_run(CAMPAIGN, tmp_path / "prepared")
    assert result.returncode == 0, result.stderr
    output = tmp_path / "prepared"
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
    assert queue["driver"][:3] == ["python", "-m", "digitalmodel"]
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
