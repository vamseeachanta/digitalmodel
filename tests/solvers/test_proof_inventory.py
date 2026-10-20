"""Bounded source-only audit probes; production solver modules are never imported."""

import ast
from dataclasses import dataclass
from datetime import timedelta
import hashlib
import json
import os
from pathlib import Path
import signal
import subprocess
from types import SimpleNamespace
from typing import Optional

import pytest

from digitalmodel.infrastructure.solver_proof import capture_inputs

ROOT = Path(__file__).resolve().parents[2]
AUDITED_REVISION = "e92a4e7ad2a7766f7c040ce4795b431d9da608fa"


def _audited_bytes(name):
    """Freeze observations at the audited revision; later repairs may change behavior."""
    env = {key: value for key, value in os.environ.items()
           if key not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"}}
    return subprocess.run(["git", "--no-replace-objects", "show", f"{AUDITED_REVISION}:{name}"],
                          cwd=ROOT, env=env, check=True, capture_output=True).stdout


def _audited_source(name):
    return _audited_bytes(name).decode("utf-8")


def _source_objects(source, names, namespace):
    """Execute only selected definitions against fake operating-system objects."""
    tree = ast.parse(source)
    definitions = [node for node in tree.body
                   if isinstance(node, (ast.FunctionDef, ast.ClassDef)) and node.name in names]
    assert {node.name for node in definitions} == set(names)
    exec(compile(ast.Module(body=definitions, type_ignores=[]), "<audit-double>", "exec"), namespace)
    return namespace


def test_imported_result_helper_is_conditional_in_existing_source_inventory(tmp_path):
    """A missing helper is omitted from the list, despite its unconditional import.

    This proves a declaration gap, not a successful solve with a missing helper.
    The independent mandatory declaration rejects the same missing input.
    """
    harness = _audited_source("docs/plans/evidence/issue-2082-native.ps1")
    worker = harness.split("$worker = @'", 1)[1].split("\n'@", 1)[0]
    objects = _source_objects(worker, {"model_sources"}, {})
    name = "src/digitalmodel/solvers/smoke/model_results.py"
    assert tmp_path / name not in objects["model_sources"](tmp_path)
    probe = ast.parse(_audited_source("src/digitalmodel/solvers/smoke/model_probe.py"))
    assert any(isinstance(node, ast.ImportFrom) and node.module == "model_results"
               for node in probe.body)
    # The pinned NIST abc digest is used solely to demonstrate missing-file refusal.
    digest = "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    with pytest.raises(ValueError, match="input_unreadable"):
        capture_inputs(tmp_path, {name: digest})


def test_existing_detached_poll_cannot_establish_cleanup_when_signalling_is_denied(tmp_path):
    """Synthetic elapsed two hours exceeds a one-hour budget; all processes stay live.

    Permission denial is injected; no process or solver is signalled. The current
    caller records termination although its observer still reports a live root.
    """
    source = _audited_source("src/digitalmodel/solvers/openfoam/runner.py")
    attempts = []

    def denied(group, sig):
        attempts.append(sig)
        raise PermissionError("synthetic signalling refusal")

    namespace = dict(Path=Path, Optional=Optional, dataclass=dataclass, json=json, signal=signal,
                     time=SimpleNamespace(time=lambda: timedelta(hours=2).total_seconds()),
                     os=SimpleNamespace(getpgid=lambda pid: pid, killpg=denied),
                     _process_alive=lambda pid: True, _last_iteration=lambda path: None)
    objects = _source_objects(source, {"PollResult", "poll_detached_run", "_terminate_group"}, namespace)
    record = {"pid": 1, "wallclock_budget_hours": 1, "started_epoch": 0, "log_file": "absent.log"}
    (tmp_path / "detached_run.json").write_text(json.dumps(record), encoding="utf-8")
    result = objects["poll_detached_run"](tmp_path)
    assert attempts == [signal.SIGTERM]
    assert result.terminated is True and result.running is False
    assert objects["_process_alive"](record["pid"]) is True


def test_inventory_binds_every_candidate_to_the_audited_revision():
    """Each recorded digest is recomputed from Git bytes, not the inventory writer."""
    inventory = json.loads((ROOT / "docs/reports/solver-proof-inventory.json").read_text())
    assert inventory["audited_revision"] == AUDITED_REVISION
    records = inventory["files"]
    assert records and len(records) == len({record["path"] for record in records})
    for record in records:
        assert record["sha256"] == hashlib.sha256(_audited_bytes(record["path"])).hexdigest()
        assert record["disposition"] in {"selected_owner", "NOT_EVALUATED"}


@pytest.mark.parametrize("name", ["solver-proof-inventory.json", "solver-proof-completeness.html"])
def test_audit_artifacts_are_eligible_for_tracking(name):
    env = {key: value for key, value in os.environ.items()
           if key not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"}}
    ignored = subprocess.run(["git", "check-ignore", "-q", "--", f"docs/reports/{name}"],
                             cwd=ROOT, env=env, check=False)
    assert ignored.returncode == 1
