"""CLI contract tests for the synthetic tank 3D smoke entry point."""

from __future__ import annotations

import importlib.util
import sys
from types import SimpleNamespace
from pathlib import Path

import pytest


SCRIPT = Path(__file__).resolve().parents[2] / "scripts/cfd/run_synthetic_tank_3d_smoke.py"


def load_script():
    spec = importlib.util.spec_from_file_location("synthetic_tank_3d_smoke", SCRIPT)
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_cli_requires_dispatcher_rank_binding(monkeypatch: pytest.MonkeyPatch) -> None:
    script = load_script()
    monkeypatch.delenv("CFD_DISPATCH_RANKS", raising=False)

    assert script.main(["--ranks", "8", "--work-dir", "/tmp/synthetic-smoke"]) == 2


def test_cli_rejects_rank_different_from_dispatcher(monkeypatch: pytest.MonkeyPatch) -> None:
    script = load_script()
    monkeypatch.setenv("CFD_DISPATCH_RANKS", "8")

    assert script.main(["--ranks", "4", "--work-dir", "/tmp/synthetic-smoke"]) == 2


def test_cli_uses_dispatcher_rank_when_explicit_rank_is_omitted(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    script = load_script()
    monkeypatch.setenv("CFD_DISPATCH_RANKS", "8")
    monkeypatch.setenv("CFD_EXECUTION_CLASS", "test")
    monkeypatch.setattr(script, "run_pipeline", lambda **kwargs: kwargs["ranks"])

    assert script.main(["--work-dir", "/tmp/synthetic-smoke"]) == 0


def test_toolchain_uses_installed_gmsh_version(monkeypatch: pytest.MonkeyPatch) -> None:
    script = load_script()
    monkeypatch.setitem(sys.modules, "gmsh", SimpleNamespace(__version__="4.15.1"))

    assert script._toolchain().gmsh_version == "4.15.1"


def test_prepare_case_uses_exact_converted_boundary_contract(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    script = load_script()
    spec = SimpleNamespace(
        tank=SimpleNamespace(length=3.0, breadth=2.0, height=1.0)
    )
    input_yaml = tmp_path / "input.yml"
    input_yaml.write_text("synthetic: true\n", encoding="utf-8")
    calls: dict[str, object] = {}
    monkeypatch.setattr(script, "load_tank_fixture_spec", lambda _path: spec)
    monkeypatch.setattr(
        script,
        "write_sloshing_case",
        lambda *args, **kwargs: calls.update(writer=(args, kwargs)),
    )
    monkeypatch.setattr(
        script,
        "build_tank_fixture",
        lambda _spec, path: calls.update(source=path),
    )

    _, source, case = script._prepare_case(
        tmp_path / "work", input_yaml, 6, 0.30, 0.001, 8
    )

    writer_kwargs = calls["writer"][1]
    assert writer_kwargs["boundary_contract"].patch_names == ("walls", "atmosphere")
    assert calls["writer"][0][1].decompose_ranks == 8
    assert source == case / "source.msh"
    assert (case / "input.yml").read_bytes() == input_yaml.read_bytes()


def test_pipeline_rechecks_snapshot_and_passes_attestation_to_evidence(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    script = load_script()
    case = tmp_path / "case"
    snapshot = tmp_path / "snapshot"
    snapshot.mkdir()
    spec = SimpleNamespace(
        tank=SimpleNamespace(length=3.0, breadth=2.0, height=1.0)
    )
    source = case / "source.msh"
    bridge = SimpleNamespace(
        manifest_path=case / "constant/polyMesh.manifest.json",
        manifest={"schema_version": 1, "status": "completed"},
    )
    state = {"verified": 0, "released": 0}
    execution = SimpleNamespace(
        case_dir=snapshot,
        verify_unchanged=lambda: state.update(verified=state["verified"] + 1),
        release=lambda: state.update(released=state["released"] + 1),
    )
    result = SimpleNamespace(status="completed")
    evidence_calls: list[dict] = []
    monkeypatch.setattr(script, "_prepare_case", lambda *_args: (spec, source, case))
    monkeypatch.setattr(script, "prepare_gmsh_poly_mesh", lambda *_a, **_k: bridge)
    monkeypatch.setattr(script, "prepare_prebuilt_execution", lambda *_a: execution)
    monkeypatch.setattr(script, "_prescribed_yaw_angle", lambda *_a: 0.2)
    monkeypatch.setattr(script, "execute_smoke", lambda *_a, **_k: result)
    monkeypatch.setattr(
        script,
        "write_reduced_evidence",
        lambda *_a, **kwargs: evidence_calls.append(kwargs),
    )
    monkeypatch.setattr(
        script,
        "_evidence_context",
        lambda **_kwargs: {
            "artifacts": {},
            "source_provenance": {"clean": True},
            "execution_class": "shared-fallback",
            "execution_metrics": {"load1": 0.5, "load_per_core": 0.125},
        },
    )

    returned = script.run_pipeline(
        ranks=2,
        selected_ranks=2,
        visible_cpus=4,
        work_dir=tmp_path,
        input_yaml=tmp_path / "input.yml",
        evidence=tmp_path / "evidence.json",
        cpb=6,
        end_time=0.30,
        delta_t=0.001,
        time_precision=8,
        execution_class="shared-fallback",
    )

    assert returned is result
    assert state == {"verified": 1, "released": 1}
    assert evidence_calls[0]["bridge_manifest"] == bridge.manifest
