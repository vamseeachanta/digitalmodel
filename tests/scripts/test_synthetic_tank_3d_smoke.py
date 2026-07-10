"""CLI contract tests for the synthetic tank 3D smoke entry point."""

from __future__ import annotations

import importlib.util
import stat
import subprocess
import sys
from pathlib import Path
from types import SimpleNamespace

import pytest


SCRIPT = Path(__file__).resolve().parents[2] / "scripts/cfd/run_synthetic_tank_3d_smoke.py"
OPENFOAM_VERSION = "2312.260127-2"
OPENMPI_VERSION = "4.1.6-7ubuntu2"
ASSETUTILITIES_COMMIT = "993f1b5ddc90b56ecf531bedb1b84f5efe096700"


def load_script():
    spec = importlib.util.spec_from_file_location("synthetic_tank_3d_smoke", SCRIPT)
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def mock_package_query(
    monkeypatch: pytest.MonkeyPatch, script, versions: dict[str, str]
) -> list[list[str]]:
    calls: list[list[str]] = []

    def run(argv: list[str], **kwargs: object) -> subprocess.CompletedProcess:
        calls.append(argv)
        package = argv[-1]
        return subprocess.CompletedProcess(
            argv, 0, stdout=versions[package], stderr=""
        )

    monkeypatch.setattr(script.subprocess, "run", run)
    return calls


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


def test_cli_requires_projected_load_for_non_test_execution(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    script = load_script()
    monkeypatch.setenv("CFD_DISPATCH_RANKS", "8")
    monkeypatch.setenv("CFD_EXECUTION_CLASS", "dedicated")
    monkeypatch.delenv("CFD_DISPATCH_PROJECTED_LOAD_PER_CORE", raising=False)
    monkeypatch.setattr(
        script, "run_pipeline", lambda **_kwargs: pytest.fail("pipeline must not run")
    )

    assert script.main(["--work-dir", "/tmp/synthetic-smoke"]) == 2


def test_toolchain_attests_live_package_versions(monkeypatch: pytest.MonkeyPatch) -> None:
    script = load_script()
    monkeypatch.setitem(sys.modules, "gmsh", SimpleNamespace(__version__="4.15.1"))
    calls = mock_package_query(
        monkeypatch,
        script,
        {
            "openfoam2312-default": OPENFOAM_VERSION,
            "openmpi-bin": OPENMPI_VERSION,
        },
    )

    toolchain = script._toolchain()

    assert toolchain.gmsh_version == "4.15.1"
    assert toolchain.openfoam_package == OPENFOAM_VERSION
    assert toolchain.openmpi_package == OPENMPI_VERSION
    assert calls == [
        ["dpkg-query", "-W", "-f=${Version}", "openfoam2312-default"],
        ["dpkg-query", "-W", "-f=${Version}", "openmpi-bin"],
    ]


@pytest.mark.parametrize(
    ("package", "actual"),
    [
        ("openfoam2312-default", "2312.260127-1"),
        ("openmpi-bin", "4.1.6-7ubuntu1"),
    ],
)
def test_toolchain_rejects_package_version_drift(
    monkeypatch: pytest.MonkeyPatch, package: str, actual: str
) -> None:
    script = load_script()
    monkeypatch.setitem(sys.modules, "gmsh", SimpleNamespace(__version__="4.15.1"))
    versions = {
        "openfoam2312-default": OPENFOAM_VERSION,
        "openmpi-bin": OPENMPI_VERSION,
    }
    versions[package] = actual
    mock_package_query(monkeypatch, script, versions)

    with pytest.raises(script.SmokeError, match=package):
        script._toolchain()


@pytest.mark.parametrize("failure", ["missing", "nonzero", "timeout"])
def test_package_query_fails_cleanly(
    monkeypatch: pytest.MonkeyPatch, failure: str
) -> None:
    script = load_script()

    def run(argv: list[str], **_kwargs: object) -> subprocess.CompletedProcess:
        if failure == "missing":
            raise FileNotFoundError("dpkg-query")
        if failure == "timeout":
            raise subprocess.TimeoutExpired(argv, 30)
        return subprocess.CompletedProcess(argv, 1, stdout=OPENFOAM_VERSION, stderr="")

    monkeypatch.setattr(script.subprocess, "run", run)
    with pytest.raises(script.SmokeError, match="openfoam2312-default"):
        script._package_version("openfoam2312-default", OPENFOAM_VERSION)


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

    work = tmp_path / "work"
    _, source, case = script._prepare_case(work, input_yaml, 6, 0.30, 0.001, 8)
    _, _, second_case = script._prepare_case(work, input_yaml, 6, 0.30, 0.001, 8)

    writer_kwargs = calls["writer"][1]
    assert writer_kwargs["boundary_contract"].patch_names == ("walls", "atmosphere")
    assert calls["writer"][0][1].decompose_ranks == 8
    assert source == case / "source.msh"
    assert (case / "input.yml").read_bytes() == input_yaml.read_bytes()
    assert case != second_case
    assert case.name == second_case.name == "case"
    assert case.parent.parent == second_case.parent.parent == work
    assert stat.S_IMODE(case.parent.stat().st_mode) == 0o700


def _provenance(commit: str) -> dict[str, object]:
    return {"clean": True, "commit": commit, "tracked_sources_sha256": "b" * 64}


def _patch_pipeline_records(script, monkeypatch, events, evidence_calls) -> None:
    def provenance(kind: str, commit: str) -> dict[str, object]:
        events.append(kind)
        return _provenance(commit)

    monkeypatch.setattr(
        script,
        "_capture_pre_run_artifacts",
        lambda _case: events.append("artifacts") or {},
    )
    monkeypatch.setattr(
        script, "_source_provenance", lambda: provenance("source", "a" * 40)
    )
    monkeypatch.setattr(
        script,
        "_dependency_provenance",
        lambda: provenance("dependency", ASSETUTILITIES_COMMIT),
    )
    monkeypatch.setattr(
        script,
        "write_reduced_evidence",
        lambda *_a, **kwargs: events.append("evidence") or evidence_calls.append(kwargs),
    )


def patch_pipeline_fakes(
    script, tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> SimpleNamespace:
    case = tmp_path / "case"
    snapshot = tmp_path / "snapshot"
    snapshot.mkdir()
    spec = SimpleNamespace(tank=SimpleNamespace(length=3.0, breadth=2.0, height=1.0))
    source = case / "source.msh"
    bridge = SimpleNamespace(
        manifest_path=case / "constant/polyMesh.manifest.json",
        manifest={"schema_version": 1, "status": "completed"},
    )
    events: list[str] = []
    state = {"verified": 0, "released": 0}
    execution = SimpleNamespace(
        case_dir=snapshot,
        verify_unchanged=lambda: state.update(verified=state["verified"] + 1),
        release=lambda: state.update(released=state["released"] + 1),
    )
    result = SimpleNamespace(status="completed")
    evidence_calls: list[dict] = []

    def execute(*_args: object, **_kwargs: object) -> SimpleNamespace:
        events.append("execute")
        return result

    monkeypatch.setattr(script, "_prepare_case", lambda *_args: (spec, source, case))
    monkeypatch.setattr(script, "_toolchain", lambda: SimpleNamespace())
    monkeypatch.setattr(script, "prepare_gmsh_poly_mesh", lambda *_a, **_k: bridge)
    monkeypatch.setattr(script, "prepare_prebuilt_execution", lambda *_a: execution)
    monkeypatch.setattr(script, "_prescribed_yaw_angle", lambda *_a: 0.2)
    monkeypatch.setattr(script, "execute_smoke", execute)
    monkeypatch.setattr(
        script, "_execution_metrics", lambda *_a: {"actual_load1": 0.5, "actual_load_per_core": 0.125}
    )
    _patch_pipeline_records(script, monkeypatch, events, evidence_calls)
    return SimpleNamespace(
        result=result,
        state=state,
        bridge=bridge,
        events=events,
        evidence_calls=evidence_calls,
    )


def test_pipeline_rechecks_snapshot_and_passes_attestation_to_evidence(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    script = load_script()
    fakes = patch_pipeline_fakes(script, tmp_path, monkeypatch)

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
        projected_load_per_core=0.625,
    )

    assert returned is fakes.result
    assert fakes.state == {"verified": 1, "released": 1}
    assert fakes.events == [
        "source",
        "dependency",
        "artifacts",
        "execute",
        "source",
        "dependency",
        "evidence",
    ]
    assert fakes.evidence_calls[0]["bridge_manifest"] == fakes.bridge.manifest
    assert set(fakes.evidence_calls[0]["source_provenance"]) == {
        "pre_execution",
        "post_execution",
    }


@pytest.mark.parametrize(
    ("load1", "projected", "message"),
    [(8.0, 0.5, "actual load/core"), (0.4, 1.6, "projected load/core")],
)
def test_pipeline_rejects_actual_and_projected_overload(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    load1: float,
    projected: float,
    message: str,
) -> None:
    script = load_script()
    monkeypatch.setattr(script.os, "getloadavg", lambda: (load1, 0.0, 0.0))

    with pytest.raises(script.SmokeError, match=message):
        script.run_pipeline(
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
            execution_class="dedicated",
            projected_load_per_core=projected,
        )


def test_pre_run_artifacts_cover_inputs_fields_dictionaries_and_mesh(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    script = load_script()
    repo = tmp_path / "repo"
    case = tmp_path / "case"
    files = {
        repo / "uv.lock": "lock",
        case / "input.yml": "input",
        case / "source.msh": "mesh",
        case / "0/U": "field",
        case / "constant/dynamicMeshDict": "motion",
        case / "system/controlDict": "control",
        case / "constant/polyMesh/points": "points",
    }
    for path, content in files.items():
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content, encoding="utf-8")
    monkeypatch.setattr(script, "REPO_ROOT", repo)

    artifacts = script._capture_pre_run_artifacts(case)

    assert set(artifacts) == {
        "uv_lock",
        "input_yaml",
        "source_msh",
        "initial_fields",
        "case_dictionaries",
        "poly_mesh",
    }
    assert artifacts["case_dictionaries"]["roots"] == ["constant", "system"]
    assert artifacts["initial_fields"]["path"] == "0"
    assert artifacts["poly_mesh"]["path"] == "constant/polyMesh"


def test_dependency_provenance_requires_the_pinned_assetutilities_commit(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    script = load_script()
    monkeypatch.setattr(
        script, "_repo_provenance", lambda _root: _provenance("c" * 40)
    )

    with pytest.raises(script.SmokeError, match="assetutilities commit"):
        script._dependency_provenance()
