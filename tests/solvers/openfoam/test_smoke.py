"""TDD contract tests for the synthetic tank MPI smoke state machine."""

from __future__ import annotations

import math
import json
import subprocess
from pathlib import Path

import pytest

from digitalmodel.solvers.openfoam.smoke import (
    SmokeError,
    execute_smoke,
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
        if argv[0] == "reconstructParMesh":
            Path(kwargs["cwd"]) .joinpath("constant/polyMesh/points").write_text(
                rotated, encoding="ascii"
            )
        output = solver_output if argv[0] == "mpirun" else "End\n"
        return subprocess.CompletedProcess(argv, 0, stdout=output, stderr="")

    run.calls = calls  # type: ignore[attr-defined]
    return run


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


def test_evidence_writer_is_versioned_and_atomic(tmp_path: Path) -> None:
    initial = [(0.0, 0.0, 0.0), (1.0, 0.0, 0.0)]
    angle = 0.2
    rotated = [(0.0, 0.0, 0.0), (math.cos(angle), math.sin(angle), 0.0)]
    case = _case_with_points(tmp_path / "case", initial)
    result = execute_smoke(
        plan_smoke(2, 4, 2),
        case,
        end_time=0.30,
        time_precision=2,
        length=1.0,
        rotation_radians=angle,
        runner=_success_runner(_points_text(rotated)),
    )

    output = tmp_path / "evidence.json"
    write_reduced_evidence(output, result)
    assert output.read_text(encoding="utf-8").endswith("\n")
    assert '"schema_version": 1' in output.read_text(encoding="utf-8")
    assert not list(tmp_path.glob(".evidence.json.*.tmp"))


def test_evidence_writer_preserves_bridge_attestation(tmp_path: Path) -> None:
    initial = [(0.0, 0.0, 0.0), (1.0, 0.0, 0.0)]
    angle = 0.2
    rotated = [(0.0, 0.0, 0.0), (math.cos(angle), math.sin(angle), 0.0)]
    result = execute_smoke(
        plan_smoke(2, 4, 2),
        _case_with_points(tmp_path / "case", initial),
        end_time=0.30,
        time_precision=2,
        length=1.0,
        rotation_radians=angle,
        runner=_success_runner(_points_text(rotated)),
    )
    output = tmp_path / "evidence.json"
    write_reduced_evidence(
        output,
        result,
        bridge_manifest={"schema_version": 1, "poly_mesh": {"tree_sha256": "abc"}},
    )

    payload = json.loads(output.read_text(encoding="utf-8"))
    assert payload["bridge"]["poly_mesh"]["tree_sha256"] == "abc"
