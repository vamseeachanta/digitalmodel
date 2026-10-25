# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Evidence retention uses fixture bytes and identity checks, never a solver.

Expected digests are independently SHA-256 hashed from the retained input bytes.
No engineering constants, coefficients or acceptance limits are introduced.
"""

import hashlib
import json
from pathlib import Path
import sys
from types import ModuleType

import pytest

from assetutilities.workflow_api import ResultLocator
from digitalmodel.workflow_api import runner


@pytest.fixture
def execution(tmp_path, monkeypatch):
    """Replace the engine module before import, preventing native imports."""
    root = tmp_path / "evidence"
    root.mkdir()
    cfg = {"basename": "synthetic", "setting": "fixture"}

    def engine(**kwargs):
        assert kwargs["embed"] is True
        assert kwargs["log_to_file"] is False
        assert kwargs["root_folder"] == str(root)
        (root / "partial.txt").write_bytes(b"partial evidence")
        kwargs["cfg"]["setting"] = "engine mutation"
        return {"result": "synthetic"}

    module = ModuleType("digitalmodel.engine")
    module.engine = engine
    monkeypatch.setitem(sys.modules, "digitalmodel.engine", module)
    monkeypatch.setattr(runner.tempfile, "mkdtemp", lambda **kwargs: str(root))
    monkeypatch.setattr(runner, "extract_result", lambda *args: ({"value": "fixture"}, []))
    return root, cfg, module


@pytest.mark.parametrize("phase", ["engine", "extraction", "hashing"])
def test_failure_preserves_original_exception_inputs_and_partial_files(execution, monkeypatch, phase):
    """Each failure preserves exactly the supplied bytes and exception object."""
    root, cfg, module = execution
    original = RuntimeError("synthetic failure")
    if phase == "engine":
        successful_engine = module.engine

        def fail(**kwargs):
            successful_engine(**kwargs)
            raise original

        module.engine = fail
    else:
        def fail(*args):
            raise original

        monkeypatch.setattr(runner, "extract_result" if phase == "extraction" else "result_hash", fail)

    with pytest.raises(RuntimeError) as caught:
        runner._run_once(cfg, ResultLocator())

    assert caught.value is original
    assert (root / "partial.txt").read_bytes() == b"partial evidence"
    metadata = root / ".workflow-evidence"
    input_bytes = (metadata / "input.json").read_bytes()
    assert json.loads(input_bytes) == cfg
    record = json.loads((metadata / "failure.json").read_text())
    assert record == {
        "status": "failed", "phase": phase,
        "input_hash": hashlib.sha256(input_bytes).hexdigest(),
    }
    assert any(str(root) in note for note in original.__notes__)
    assert cfg["setting"] == "fixture"


def test_success_removes_temporary_evidence_root(execution):
    root, cfg, _ = execution
    payload, warnings, digest = runner._run_once(cfg, ResultLocator())
    assert payload == {"value": "fixture"}
    assert warnings == []
    assert digest
    assert not root.exists()


def test_error_envelope_exposes_retention_location_and_input_digest(execution, monkeypatch):
    root, cfg, module = execution
    original = RuntimeError("synthetic failure")

    def fail(**kwargs):
        (root / "partial.txt").write_bytes(b"partial evidence")
        raise original

    module.engine = fail
    monkeypatch.setattr(runner, "lookup_row_for_cfg", lambda cfg: None)
    envelope = runner.run_workflow(cfg=cfg)
    assert envelope.status == "error"
    assert envelope.result == {}
    assert envelope.determinism == {"result_hash": None, "reproducible": None}
    assert envelope.warnings[0] == str(original)
    assert any(str(root) in warning for warning in envelope.warnings)
    assert envelope.provenance["input_hash"] == hashlib.sha256(
        (root / ".workflow-evidence" / "input.json").read_bytes()
    ).hexdigest()


def test_metadata_write_failure_does_not_mask_original_exception(execution, monkeypatch):
    root, cfg, module = execution
    original = RuntimeError("primary failure")

    def fail(**kwargs):
        (root / ".workflow-evidence").write_bytes(b"pre-existing evidence")
        raise original

    module.engine = fail
    with pytest.raises(RuntimeError) as caught:
        runner._run_once(cfg, ResultLocator())
    assert caught.value is original
    assert (root / ".workflow-evidence").read_bytes() == b"pre-existing evidence"
    assert any("metadata" in note for note in original.__notes__)
    assert any(str(root) in note for note in original.__notes__)


def test_interruption_retains_partial_output(execution):
    root, cfg, module = execution
    original = KeyboardInterrupt()
    successful_engine = module.engine

    def fail(**kwargs):
        successful_engine(**kwargs)
        raise original

    module.engine = fail
    with pytest.raises(KeyboardInterrupt) as caught:
        runner._run_once(cfg, ResultLocator())
    assert caught.value is original
    assert (root / "partial.txt").read_bytes() == b"partial evidence"


def test_failed_reproducibility_rerun_retains_only_its_evidence(execution, tmp_path, monkeypatch):
    """The first successful root is removed; the second failed root survives."""
    first, cfg, module = execution
    second = tmp_path / "rerun"
    second.mkdir()
    roots = iter((first, second))
    monkeypatch.setattr(runner.tempfile, "mkdtemp", lambda **kwargs: str(next(roots)))
    monkeypatch.setattr(runner, "lookup_row_for_cfg", lambda cfg: None)
    original = RuntimeError("rerun failed")

    def engine(**kwargs):
        root = Path(kwargs["root_folder"])
        (root / "partial.txt").write_bytes(b"fixture")
        if root == second:
            raise original
        return {}

    module.engine = engine
    envelope = runner.run_workflow(cfg=cfg, verify_reproducible=True)
    assert envelope.status == "error"
    assert envelope.determinism["reproducible"] is None
    assert not first.exists()
    assert (second / "partial.txt").read_bytes() == b"fixture"
    assert any(str(second) in warning for warning in envelope.warnings)


def test_input_snapshot_uses_shared_nonvolatile_contract(execution):
    """Runtime-only Analysis/default/cfg_array keys are excluded by contract."""
    root, cfg, module = execution
    cfg.update(Analysis={"runtime": "fixture"}, default={}, cfg_array=[])

    def fail(**kwargs):
        raise ValueError("synthetic failure")

    module.engine = fail
    with pytest.raises(ValueError):
        runner._run_once(cfg, ResultLocator())
    snapshot = json.loads((root / ".workflow-evidence" / "input.json").read_bytes())
    assert snapshot == {"basename": "synthetic", "setting": "fixture"}


@pytest.mark.parametrize("rerun", [False, True])
def test_failure_digest_binds_saved_bytes_without_reserializing(execution, monkeypatch, rerun):
    """A non-JSON object's changing text must not invalidate the saved digest."""
    root, cfg, module = execution
    serializations = []

    class ChangingText:
        text = "fixture"

        def __str__(self):
            result = self.text
            self.text += " changed"
            serializations.append(result)
            return result

    cfg["setting"] = ChangingText()

    calls = []

    def fail(**kwargs):
        calls.append(kwargs)
        if rerun and len(calls) == 1:
            return {}
        raise RuntimeError("synthetic failure")

    module.engine = fail
    monkeypatch.setattr(runner, "lookup_row_for_cfg", lambda cfg: None)
    if rerun:
        first = root.parent / "successful"
        first.mkdir()
        roots = iter((first, root))
        monkeypatch.setattr(runner.tempfile, "mkdtemp", lambda **kwargs: str(next(roots)))
    envelope = runner.run_workflow(cfg=cfg, verify_reproducible=rerun)
    assert envelope.status == "error"
    assert len(calls) == (2 if rerun else 1)
    if rerun:
        assert not first.exists()
    metadata = root / ".workflow-evidence"
    record = json.loads((metadata / "failure.json").read_bytes())
    saved = (metadata / "input.json").read_bytes()
    assert envelope.provenance["input_hash"] == record["input_hash"] == hashlib.sha256(saved).hexdigest()
    assert json.loads(saved) == {"basename": "synthetic", "setting": "fixture"}
    assert serializations == ["fixture"]
