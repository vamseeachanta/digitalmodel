"""MPI resume is accepted only against a decomposition that matches it (#1968).

Replaces ``test_mpi_resume_restarts_from_latest_time``, which created exactly
one ``processor0`` and then ran ``workers=4`` -- a 4-rank resume from a 1-rank
decomposition -- and asserted it completed. Its "must not rebuild the case"
guard is carried forward into ``test_resume_accepts_matching_decomposition``
(the accept path, where it belongs) and its complement into
``test_refused_resume_does_not_rebuild_the_case``.

The guard is also retargeted. The original patched ``ofb._build_case``, a name
binding in the ``openfoam_run_batch`` facade, while ``run_case_mpi`` resolves
``build_case`` in ``openfoam_batch_execution``'s own globals -- so the original
AssertionError could never fire. These tests patch the module the call site
actually reads.

Every fixture is pure filesystem under ``tmp_path`` with an injected command
runner: no OpenFOAM binary is required, and none exists on this host.
``decomposeParDict`` is written as literal text rather than through the
production writer, and every expected rank count is a literal in the test body,
so no expectation is read back through the parser under test.
"""

from __future__ import annotations

from hashlib import sha256
from pathlib import Path
from unittest.mock import patch

from digitalmodel.solvers.openfoam import artifact_index
from digitalmodel.workflows import openfoam_batch_decomposition as decomposition
from digitalmodel.workflows import openfoam_batch_execution as execution
from digitalmodel.workflows import openfoam_run_batch as ofb


CONTROL_DICT = "application     interFoam;\nstartFrom       startTime;\n"


def _case(tmp_path: Path) -> tuple[dict, Path]:
    base = {"case_type": "current_loading", "solver": "interFoam"}
    item = ofb._render_cases(base, [{}], {}, tmp_path / "work")[0]
    case_dir = tmp_path / "work" / item["name"]
    (case_dir / "system").mkdir(parents=True)
    (case_dir / "system" / "controlDict").write_text(CONTROL_DICT)
    return item, case_dir


def _write_subdomains(case_dir: Path, subdomains: int) -> None:
    # Literal text on purpose: a fixture must not describe its own input
    # through the production writer the gate is being tested against.
    (case_dir / "system" / "decomposeParDict").write_text(
        "FoamFile { version 2.0; format ascii; class dictionary; "
        "object decomposeParDict; }\n\n"
        f"numberOfSubdomains {subdomains};\nmethod scotch;\n"
    )


def _write_ranks(case_dir: Path, ranks: dict[int, tuple[str, ...]]) -> None:
    for rank, times in ranks.items():
        (case_dir / f"processor{rank}").mkdir(parents=True)
        for name in times:
            (case_dir / f"processor{rank}" / name).mkdir()


def _recorder() -> tuple[list, object]:
    recorded: list = []

    def run(argv, cwd, log, timeout):
        recorded.append(argv)
        return 0

    return recorded, run


def _resume(item: dict, workers: int, runner) -> dict:
    with patch.object(
        execution, "build_case",
        side_effect=AssertionError("resume must not rebuild the case"),
    ):
        return ofb._run_case_mpi(
            item, {"resume": True, "reconstruct": True}, workers=workers,
            mock=False, command_runner=runner,
        )


# --------------------------------------------------------------------------- #
#  accept path (anti-vacuity guard: a gate that refuses everything fails here) #
# --------------------------------------------------------------------------- #


def test_resume_accepts_matching_decomposition(tmp_path):
    item, case_dir = _case(tmp_path)
    _write_subdomains(case_dir, 4)
    _write_ranks(case_dir, {0: ("0.5",), 1: ("0.5",), 2: ("0.5",), 3: ("0.5",)})
    recorded, runner = _recorder()

    row = _resume(item, workers=4, runner=runner)

    assert row["status"] == "completed"
    assert [argv[0] for argv in recorded] == ["mpirun", "reconstructPar"]
    assert "latestTime" in (case_dir / "system" / "controlDict").read_text()
    assert ofb.mpi_command_plan("interFoam", 4, resume=True)[0][0] == "mpirun"


# --------------------------------------------------------------------------- #
#  check 1 -- the processor* set must be exactly processor0..processor{N-1}    #
# --------------------------------------------------------------------------- #


def test_resume_refuses_rank_count_mismatch(tmp_path):
    item, case_dir = _case(tmp_path)
    _write_subdomains(case_dir, 4)
    _write_ranks(case_dir, {0: ("0.5",)})
    recorded, runner = _recorder()

    row = _resume(item, workers=4, runner=runner)

    assert row["error"] == (
        "resume refused: processor directory set does not match workers 4: "
        "observed 1, required 4; missing processor1, processor2, processor3"
    )
    assert recorded == []


def test_resume_refuses_non_contiguous_ranks(tmp_path):
    item, case_dir = _case(tmp_path)
    _write_subdomains(case_dir, 4)
    _write_ranks(case_dir, {0: ("0.5",), 1: ("0.5",), 3: ("0.5",)})
    recorded, runner = _recorder()

    row = _resume(item, workers=4, runner=runner)

    assert row["error"] == (
        "resume refused: processor directory set does not match workers 4: "
        "observed 3, required 4; missing processor2"
    )


# --------------------------------------------------------------------------- #
#  check 2 -- decomposeParDict numberOfSubdomains must equal workers           #
# --------------------------------------------------------------------------- #


def test_resume_refuses_subdomain_dict_mismatch(tmp_path):
    item, case_dir = _case(tmp_path)
    _write_subdomains(case_dir, 8)
    _write_ranks(case_dir, {0: ("0.5",), 1: ("0.5",), 2: ("0.5",), 3: ("0.5",)})
    recorded, runner = _recorder()

    row = _resume(item, workers=4, runner=runner)

    assert row["error"] == (
        "resume refused: system/decomposeParDict declares numberOfSubdomains 8, "
        "required 4"
    )


def test_resume_refuses_missing_decompose_par_dict(tmp_path):
    item, case_dir = _case(tmp_path)
    _write_ranks(case_dir, {0: ("0.5",), 1: ("0.5",), 2: ("0.5",), 3: ("0.5",)})
    recorded, runner = _recorder()

    row = _resume(item, workers=4, runner=runner)

    assert row["error"] == (
        "resume refused: system/decomposeParDict is missing, so the "
        "decomposition cannot be shown to match workers 4"
    )


# --------------------------------------------------------------------------- #
#  check 3 -- every rank has a time directory and all ranks agree on the max   #
# --------------------------------------------------------------------------- #


def test_resume_refuses_divergent_latest_times(tmp_path):
    item, case_dir = _case(tmp_path)
    _write_subdomains(case_dir, 4)
    _write_ranks(case_dir, {0: ("0.4", "0.5"), 1: ("0.4", "0.5"),
                            2: ("0.4", "0.5"), 3: ("0.3", "0.4")})
    recorded, runner = _recorder()

    row = _resume(item, workers=4, runner=runner)

    assert row["error"] == (
        "resume refused: ranks disagree on the latest time: processor0 is at "
        "0.5 but processor3 is at 0.4"
    )


def test_resume_refuses_when_a_rank_has_no_time_directory(tmp_path):
    item, case_dir = _case(tmp_path)
    _write_subdomains(case_dir, 4)
    _write_ranks(case_dir, {0: ("0.5",), 1: ("0.5",), 2: ("0.5",), 3: ()})
    recorded, runner = _recorder()

    row = _resume(item, workers=4, runner=runner)

    assert row["error"] == (
        "resume refused: processor3 holds no numeric time directory to "
        "restart from"
    )


# --------------------------------------------------------------------------- #
#  check 4 -- time names are validated by artifact_index, not a second parser  #
# --------------------------------------------------------------------------- #


def test_resume_time_name_validation_reuses_artifact_index(tmp_path):
    item, case_dir = _case(tmp_path)
    _write_subdomains(case_dir, 4)
    _write_ranks(case_dir, {0: ("0.5.0",), 1: ("0.5.0",),
                            2: ("0.5.0",), 3: ("0.5.0",)})
    recorded, runner = _recorder()

    row = _resume(item, workers=4, runner=runner)

    # "0.5.0" is not a valid OpenFOAM time name, so it is not a restart point.
    assert row["error"] == (
        "resume refused: processor0 holds no numeric time directory to "
        "restart from"
    )
    # And it is rejected by the one parser in the repo, not a second one.
    assert (decomposition.is_numeric_time_name
            is artifact_index.is_numeric_time_name)


# --------------------------------------------------------------------------- #
#  D4 -- refusal happens before any mutation, and never rebuilds the case      #
# --------------------------------------------------------------------------- #


def test_refused_resume_does_not_mutate_control_dict(tmp_path):
    item, case_dir = _case(tmp_path)
    _write_subdomains(case_dir, 4)
    _write_ranks(case_dir, {0: ("0.5",)})
    control = case_dir / "system" / "controlDict"
    before = sha256(control.read_bytes()).hexdigest()
    recorded, runner = _recorder()

    row = _resume(item, workers=4, runner=runner)

    assert row["status"] == "failed"
    assert sha256(control.read_bytes()).hexdigest() == before
    assert recorded == []


def test_refused_resume_does_not_rebuild_the_case(tmp_path):
    item, case_dir = _case(tmp_path)
    _write_subdomains(case_dir, 4)
    _write_ranks(case_dir, {0: ("0.5",)})
    recorded, runner = _recorder()

    # build_case is patched to raise; the refusal must surface instead, which
    # proves the gate declined before the case could be rebuilt.
    row = _resume(item, workers=4, runner=runner)

    assert row["error"] == (
        "resume refused: processor directory set does not match workers 4: "
        "observed 1, required 4; missing processor1, processor2, processor3"
    )


# --------------------------------------------------------------------------- #
#  regression fence -- the gate must not touch the fresh (resume=False) path   #
# --------------------------------------------------------------------------- #


def test_fresh_run_plan_is_unchanged(tmp_path):
    base = {"case_type": "current_loading", "solver": "interFoam"}
    item = ofb._render_cases(base, [{}], {}, tmp_path / "work")[0]
    recorded = []

    def runner(argv, cwd, log, timeout):
        recorded.append(argv)
        if argv[0] == "decomposePar":
            (Path(cwd) / "processor0").mkdir(parents=True, exist_ok=True)
        return 0

    row = ofb._run_case_mpi(
        item, {"resume": False, "reconstruct": True}, workers=8, mock=False,
        command_runner=runner,
    )

    assert row["status"] == "completed"
    names = [argv[0] for argv in recorded]
    assert names.index("decomposePar") < names.index("mpirun")
    assert recorded[names.index("mpirun")][:3] == ["mpirun", "-np", "8"]
    dpd = tmp_path / "work" / item["name"] / "system" / "decomposeParDict"
    assert "numberOfSubdomains 8" in dpd.read_text()
