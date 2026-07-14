"""External routing binds engine-owned identity evidence before mutation."""

from copy import deepcopy
from pathlib import Path

import pytest
import yaml

from digitalmodel.workflows import openfoam_run_batch as ofb

EXAMPLE = (
    Path(__file__).resolve().parents[2]
    / "examples"
    / "workflows"
    / "openfoam-run-batch"
    / "input.yml"
)
IDENTITY = {
    "schema_version": 1,
    "identity_kind": "openfoam-run-v1",
    "identity_sha256": "e" * 64,
}


def _external_cfg(tmp_path: Path) -> tuple[dict, Path]:
    cfg_dir = tmp_path / "input"
    root = tmp_path / "operator"
    cfg_dir.mkdir()
    root.mkdir()
    request = cfg_dir / "request.yml"
    request.write_text(EXAMPLE.read_text())
    cfg = yaml.safe_load(request.read_text())
    cfg["_config_dir_path"] = str(cfg_dir)
    cfg["_config_file_path"] = str(request)
    run = cfg["openfoam_run_batch"]["run_batch"]
    run.update(
        execution_context="trusted-local",
        work_root=str(root),
        work_root_namespace="team/lane",
    )
    return cfg, root


def test_external_route_rejects_missing_request_evidence_before_mutation(tmp_path):
    cfg, root = _external_cfg(tmp_path)
    cfg.pop("_config_file_path")
    with pytest.raises(ValueError, match="config file evidence"):
        ofb.router(cfg)
    assert list(root.iterdir()) == []


def test_external_route_rejects_yaml_identity_blob_before_mutation(tmp_path):
    cfg, root = _external_cfg(tmp_path)
    cfg["openfoam_run_batch"]["run_batch"]["run_identity"] = IDENTITY
    with pytest.raises(ValueError, match="identity"):
        ofb.router(cfg)
    assert list(root.iterdir()) == []


def test_external_output_dir_cannot_escape_input_directory(tmp_path, monkeypatch):
    cfg, root = _external_cfg(tmp_path)
    cfg["openfoam_run_batch"]["run_batch"]["output_dir"] = str(
        tmp_path / "escaped-results"
    )
    monkeypatch.setattr(ofb, "_build_run_identity", lambda **_evidence: IDENTITY)
    with pytest.raises(ValueError, match="output_dir"):
        ofb.router(cfg)
    assert not (tmp_path / "escaped-results").exists()
    assert list(root.iterdir()) == []


def test_external_route_uses_approved_builder_and_owned_layout(
    tmp_path, monkeypatch
):
    cfg, root = _external_cfg(tmp_path)
    calls = []

    def identity_builder(**evidence):
        calls.append(evidence)
        return deepcopy(IDENTITY)

    monkeypatch.setattr(ofb, "_build_run_identity", identity_builder)
    result = ofb.router(cfg)
    run = root / "team" / "lane" / f"openfoam-run-{IDENTITY['identity_sha256']}"
    assert calls and calls[0]["config_path"] == Path(cfg["_config_file_path"])
    assert calls[0]["effective_config"] == cfg["openfoam_run_batch"]
    assert calls[0]["selected_executables"] == {}
    assert calls[0]["work_layout_version"] == "work-layout-v1"
    for case in ("current_simpleFoam", "current_pimpleFoam"):
        assert (run / "batch_runs" / case / "system" / "controlDict").is_file()
        assert not (Path(cfg["_config_dir_path"]) / "batch_runs" / case).exists()
    outputs = result["openfoam_run_batch"]["outputs"]
    assert outputs["manifest"] == str(Path(cfg["_config_dir_path"]) / "results" / "cases.csv")
    assert not list(root.rglob("cases.csv"))
