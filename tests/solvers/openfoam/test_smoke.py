"""TDD contract tests for the synthetic tank MPI smoke state machine."""

from __future__ import annotations

import math
import json
import subprocess
import sys
from pathlib import Path

import pytest

from digitalmodel.solvers.openfoam.smoke import (
    SmokeError,
    execute_smoke,
    parse_points,
    plan_smoke,
    verify_rigid_rotation,
    write_reduced_evidence,
)


def _points_text(points: list[tuple[float, float, float]]) -> str:
    vectors = "\n".join(f"({x} {y} {z})" for x, y, z in points)
    return f"FoamFile {{ object points; }}\n{len(points)}\n(\n{vectors}\n)\n"


def _case_with_points(root: Path, points: list[tuple[float, float, float]]) -> Path:
    points_path = root / "constant" / "polyMesh" / "points"
    points_path.parent.mkdir(parents=True)
    points_path.write_text(_points_text(points), encoding="ascii")
    return root


def _success_runner(rotated: str, *, solver_output: str = "Time = 0.30\nEnd\n"):
    calls: list[tuple[list[str], dict]] = []

    def run(argv: list[str], **kwargs: object) -> subprocess.CompletedProcess:
        calls.append((list(argv), dict(kwargs)))
        if argv[0] == "reconstructPar":
            points = Path(kwargs["cwd"]) / "0.30" / "polyMesh" / "points"
            points.parent.mkdir(parents=True)
            points.write_text(rotated, encoding="ascii")
        output = solver_output if argv[0] == "mpirun" else "End\n"
        return subprocess.CompletedProcess(argv, 0, stdout=output, stderr="")

    run.calls = calls  # type: ignore[attr-defined]
    return run


def test_points_parser_rejects_declared_count_mismatch(tmp_path: Path) -> None:
    points = tmp_path / "points"
    points.write_text(
        "FoamFile { object points; }\n3\n(\n(0 0 0)\n(1 0 0)\n)\n",
        encoding="ascii",
    )

    with pytest.raises(SmokeError, match="count"):
        parse_points(points)


def test_plan_requires_visible_cpus_and_dispatcher_selected_ranks() -> None:
    with pytest.raises(SmokeError, match="visible CPUs"):
        plan_smoke(ranks=9, visible_cpus=8, selected_ranks=9)
    with pytest.raises(SmokeError, match="selected dispatcher ranks"):
        plan_smoke(ranks=4, visible_cpus=8, selected_ranks=8)


def test_plan_has_fixed_threshold_and_exact_post_bridge_sequence() -> None:
    plan = plan_smoke(ranks=8, visible_cpus=16, selected_ranks=8)

    assert plan.threshold == 1.5
    assert plan.commands == (
        ("setFields",),
        ("decomposePar", "-force"),
        ("mpirun", "-np", "8", "interFoam", "-parallel"),
        ("reconstructParMesh", "-latestTime"),
        ("reconstructPar", "-latestTime", "-no-fields", "-no-lagrangian"),
    )
    assert "--oversubscribe" not in plan.flat_argv


def test_execute_stops_at_first_failed_stage_and_disables_shell(tmp_path: Path) -> None:
    case = _case_with_points(tmp_path / "case", [(0.0, 0.0, 0.0), (1.0, 0.0, 0.0)])
    calls: list[tuple[list[str], dict]] = []

    def run(argv: list[str], **kwargs: object) -> subprocess.CompletedProcess:
        calls.append((list(argv), dict(kwargs)))
        code = 7 if argv[0] == "decomposePar" else 0
        return subprocess.CompletedProcess(argv, code, stdout="", stderr="bad")

    with pytest.raises(SmokeError, match="decomposePar"):
        execute_smoke(
            plan_smoke(2, 4, 2),
            case,
            end_time=0.30,
            time_precision=2,
            length=1.0,
            rotation_radians=0.2,
            runner=run,
        )

    assert [argv for argv, _ in calls] == [["setFields"], ["decomposePar", "-force"]]
    assert all(kwargs["shell"] is False for _, kwargs in calls)


@pytest.mark.parametrize(
    "solver_output",
    ["FOAM FATAL ERROR\n", "DIVERGENCE detected\n", "Time = 0.10\nEnd\n"],
)
def test_execute_rejects_fatal_divergence_or_early_end(
    tmp_path: Path, solver_output: str
) -> None:
    initial = [(0.0, 0.0, 0.0), (1.0, 0.0, 0.0)]
    case = _case_with_points(tmp_path / "case", initial)
    runner = _success_runner(_points_text(initial), solver_output=solver_output)

    with pytest.raises(SmokeError):
        execute_smoke(
            plan_smoke(2, 4, 2),
            case,
            end_time=0.30,
            time_precision=2,
            length=1.0,
            rotation_radians=0.2,
            runner=runner,
        )


def test_execute_verifies_final_time_and_nonzero_z_rotation(tmp_path: Path) -> None:
    initial = [(0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (0.0, 0.0, 1.0)]
    angle = 0.2
    rotated = [
        (0.0, 0.0, 0.0),
        (math.cos(angle), math.sin(angle), 0.0),
        (0.0, 0.0, 1.0),
    ]
    case = _case_with_points(tmp_path / "case", initial)
    runner = _success_runner(_points_text(rotated))

    result = execute_smoke(
        plan_smoke(2, 4, 2),
        case,
        end_time=0.30,
        time_precision=2,
        length=1.0,
        rotation_radians=angle,
        runner=runner,
    )

    assert result.final_time == pytest.approx(0.30)
    assert result.max_displacement > 1e-8
    assert result.max_rotation_error <= 1e-6


def test_execute_prefers_reconstructed_time_points_over_initial_mesh(
    tmp_path: Path,
) -> None:
    initial = [(0.0, 0.0, 0.0), (1.0, 0.0, 0.0)]
    angle = 0.2
    rotated = [(0.0, 0.0, 0.0), (math.cos(angle), math.sin(angle), 0.0)]
    case = _case_with_points(tmp_path / "case", initial)

    def run(argv: list[str], **kwargs: object) -> subprocess.CompletedProcess:
        if argv[0] == "reconstructPar":
            points = Path(kwargs["cwd"]) / "0.30" / "polyMesh" / "points"
            points.parent.mkdir(parents=True)
            points.write_text(_points_text(rotated), encoding="ascii")
        output = "Time = 0.30\nEnd\n" if argv[0] == "mpirun" else "End\n"
        return subprocess.CompletedProcess(argv, 0, stdout=output, stderr="")

    result = execute_smoke(
        plan_smoke(2, 4, 2),
        case,
        end_time=0.30,
        time_precision=2,
        length=1.0,
        rotation_radians=angle,
        runner=run,
    )

    assert result.max_displacement > 1e-8


def test_rotation_checker_rejects_zero_motion_and_wrong_axis() -> None:
    initial = [(0.0, 0.0, 0.0), (1.0, 0.0, 0.0)]
    with pytest.raises(SmokeError, match="nonzero"):
        verify_rigid_rotation(initial, initial, rotation_radians=0.0, length=1.0)
    with pytest.raises(SmokeError, match="rotation error"):
        verify_rigid_rotation(
            initial,
            [(0.0, 0.1, 0.0), (1.0, 0.1, 0.0)],
            rotation_radians=0.2,
            length=1.0,
        )


def _completed_result(tmp_path: Path):
    initial = [(0.0, 0.0, 0.0), (1.0, 0.0, 0.0)]
    angle = 0.2
    rotated = [(0.0, 0.0, 0.0), (math.cos(angle), math.sin(angle), 0.0)]
    case = _case_with_points(tmp_path / "case", initial)
    return execute_smoke(
        plan_smoke(2, 4, 2),
        case,
        end_time=0.30,
        time_precision=2,
        length=1.0,
        rotation_radians=angle,
        runner=_success_runner(_points_text(rotated)),
    )


def _bridge_manifest() -> dict:
    command_hashes = {
        "return_code": 0,
        "stdout_sha256": "c" * 64,
        "stderr_sha256": "d" * 64,
    }
    return {
        "schema_version": 1,
        "status": "completed",
        "created_utc": "2026-07-10T12:00:00Z",
        "source_msh": {
            "path": "source.msh", "sha256": "a" * 64, "size": 3, "format": "2.2",
        },
        "case_inputs": {
            "tree_sha256": "9" * 64, "file_count": 12, "total_bytes": 200,
        },
        "poly_mesh": {
            "path": "constant/polyMesh",
            "tree_sha256": "b" * 64,
            "file_count": 9,
            "total_bytes": 100,
        },
        "contract": {
            "patches": ["walls", "atmosphere"],
            "wall_patches": ["walls"],
            "atmosphere_patch": "atmosphere",
            "fluid_zone": "fluid",
            "cells": 10,
            "faces": 20,
            "internal_faces": 5,
        },
        "commands": [
            {"argv": ["gmshToFoam", "source.msh"], **command_hashes},
            {
                "argv": ["changeDictionary", "-constant", "-subDict", "dictionaryReplacement"],
                **command_hashes,
            },
            {"argv": ["checkMesh", "-allGeometry", "-allTopology"], **command_hashes},
        ],
        "toolchain": {
            "gmsh": "4.15.1",
            "openfoam_package": "2312.260127-2",
            "openmpi_package": "4.1.6-7ubuntu2",
        },
    }


def _artifacts() -> dict:
    return {
        "uv_lock": {"path": "uv.lock", "size": 1, "sha256": "1" * 64},
        "input_yaml": {"path": "input.yml", "size": 2, "sha256": "2" * 64},
        "source_msh": {"path": "source.msh", "size": 3, "sha256": "a" * 64},
        "initial_fields": {
            "path": "0", "file_count": 3, "total_bytes": 4, "tree_sha256": "3" * 64,
        },
        "case_dictionaries": {
            "roots": ["constant", "system"],
            "file_count": 4,
            "total_bytes": 5,
            "tree_sha256": "4" * 64,
        },
        "poly_mesh": {
            "path": "constant/polyMesh",
            "file_count": 9,
            "total_bytes": 100,
            "tree_sha256": "b" * 64,
        },
    }


def _provenance(commit: str, digest: str) -> dict:
    return {"clean": True, "commit": commit, "tracked_sources_sha256": digest}


def _evidence_kwargs() -> dict:
    source = _provenance("e" * 40, "e" * 64)
    dependency = _provenance(
        "993f1b5ddc90b56ecf531bedb1b84f5efe096700", "f" * 64
    )
    return {
        "bridge_manifest": _bridge_manifest(),
        "artifacts": _artifacts(),
        "source_provenance": {
            "pre_execution": source.copy(),
            "post_execution": source.copy(),
        },
        "dependency_provenance": {
            "assetutilities": {
                "pre_execution": dependency.copy(),
                "post_execution": dependency.copy(),
            }
        },
        "execution_class": "test",
        "dispatcher": {
            "ranks": 2,
            "selected_ranks": 2,
            "visible_cpus": 4,
            "load_threshold": 1.5,
            "projected_load_per_core": 0.625,
            "actual_load1": 0.5,
            "actual_load_per_core": 0.125,
        },
    }


def _write_valid_evidence(tmp_path: Path) -> Path:
    output = tmp_path / "evidence.json"
    write_reduced_evidence(output, _completed_result(tmp_path), **_evidence_kwargs())
    return output


def test_execute_records_timing_and_point_digest_evidence(tmp_path: Path) -> None:
    result = _completed_result(tmp_path)

    assert result.expected_end_time == pytest.approx(0.30)
    assert result.time_tolerance == pytest.approx(0.01)
    assert result.motion.axis == (0.0, 0.0, 1.0)
    assert result.motion.angle_radians == pytest.approx(0.2)
    assert result.motion.point_count == 2
    assert len(result.motion.initial_points_sha256) == 64
    assert len(result.motion.reconstructed_points_sha256) == 64


def test_evidence_writer_is_closed_self_validating_and_atomic(tmp_path: Path) -> None:
    from digitalmodel.solvers.openfoam.smoke_evidence import (
        ARTIFACT_KEYS,
        TOP_LEVEL_FIELDS,
        validate_evidence,
    )

    output = _write_valid_evidence(tmp_path)
    payload = json.loads(output.read_text(encoding="utf-8"))

    assert set(payload) == TOP_LEVEL_FIELDS
    assert set(payload["artifacts"]) == ARTIFACT_KEYS
    assert payload["purpose"] == "methodology_bridge_validation_only"
    assert payload["engineering_claim"] == "none"
    assert validate_evidence(payload) is payload
    assert output.read_text(encoding="utf-8").endswith("\n")
    assert not list(tmp_path.glob(".evidence.json.*.tmp"))


def test_evidence_preserves_bridge_case_input_attestation(tmp_path: Path) -> None:
    payload = json.loads(_write_valid_evidence(tmp_path).read_text(encoding="utf-8"))

    assert payload["bridge"]["case_inputs"] == _bridge_manifest()["case_inputs"]


def test_evidence_binds_bridge_source_size_to_artifact(tmp_path: Path) -> None:
    payload = json.loads(_write_valid_evidence(tmp_path).read_text(encoding="utf-8"))

    assert payload["bridge"]["source_msh"]["size"] == payload["artifacts"]["source_msh"]["size"]


def test_evidence_validator_rejects_unknown_and_missing_records(tmp_path: Path) -> None:
    from digitalmodel.solvers.openfoam.smoke_evidence import (
        EvidenceValidationError,
        validate_evidence,
    )

    payload = json.loads(_write_valid_evidence(tmp_path).read_text(encoding="utf-8"))
    payload["unexpected"] = True
    with pytest.raises(EvidenceValidationError, match="top-level"):
        validate_evidence(payload)
    del payload["unexpected"]
    del payload["artifacts"]["case_dictionaries"]
    with pytest.raises(EvidenceValidationError, match="artifact keys"):
        validate_evidence(payload)


@pytest.mark.parametrize(
    "value",
    ["/srv/private/case", "10.20.30.40", "gpu-claw", "user@example.com"],
)
def test_evidence_privacy_validator_rejects_sensitive_strings(value: str) -> None:
    from digitalmodel.solvers.openfoam.smoke_evidence import (
        EvidenceValidationError,
        validate_privacy,
    )

    with pytest.raises(EvidenceValidationError, match="privacy"):
        validate_privacy({"value": value})


def test_evidence_cli_validates_schema_and_privacy(tmp_path: Path) -> None:
    output = _write_valid_evidence(tmp_path)
    process = subprocess.run(
        [sys.executable, "-m", "digitalmodel.solvers.openfoam.smoke_evidence", str(output)],
        check=False,
        text=True,
        capture_output=True,
    )

    assert process.returncode == 0, process.stderr
    assert "valid smoke evidence" in process.stdout
