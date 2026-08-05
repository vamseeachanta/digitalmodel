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

import pytest

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
            # A time directory with no fields in it is not a restart point,
            # so the ordinary fixture writes one.
            (case_dir / f"processor{rank}" / name / "U").write_text("field\n")


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


def test_resume_refuses_a_non_positive_rank_count(tmp_path):
    # workers=0 satisfies checks 1 and 2 vacuously -- an empty required set
    # matches an empty observed set -- so the gate must reject it up front
    # rather than fall through to indexing rank 0 of an empty list.
    # resolve_workers already rejects workers < 1 on the router path; the gate
    # is directly callable and must refuse in its own right rather than raise
    # an IndexError that says nothing about the failed check.
    item, case_dir = _case(tmp_path)
    _write_subdomains(case_dir, 0)

    with pytest.raises(decomposition.DecompositionMismatch) as caught:
        decomposition.verify_resumable_decomposition(case_dir, 0)

    assert str(caught.value) == (
        "resume refused: workers must be >= 1 to reuse a decomposition, got 0"
    )


def test_resume_refuses_a_rank_set_of_the_right_size_but_wrong_names(tmp_path):
    # The COUNT matches (4 directories, workers=4) but the SET does not.
    # Without this, a gate that only counts processor* directories satisfies
    # every other test in this file while letting a 4-rank run resume into
    # ranks {0, 1, 2, 4}.
    item, case_dir = _case(tmp_path)
    _write_subdomains(case_dir, 4)
    _write_ranks(case_dir, {0: ("0.5",), 1: ("0.5",), 2: ("0.5",), 4: ("0.5",)})
    recorded, runner = _recorder()

    row = _resume(item, workers=4, runner=runner)

    assert row["error"] == (
        "resume refused: processor directory set does not match workers 4: "
        "observed 4, required 4; missing processor3; unexpected processor4"
    )
    assert recorded == []


def test_resume_refuses_ranks_beyond_workers(tmp_path):
    # Extras with nothing missing: required is a strict subset of observed.
    item, case_dir = _case(tmp_path)
    _write_subdomains(case_dir, 2)
    _write_ranks(case_dir, {0: ("0.5",), 1: ("0.5",), 2: ("0.5",)})
    recorded, runner = _recorder()

    row = _resume(item, workers=2, runner=runner)

    assert row["error"] == (
        "resume refused: processor directory set does not match workers 2: "
        "observed 3, required 2; unexpected processor2"
    )


def test_resume_refuses_symlinked_rank_directories(tmp_path):
    # Four names, but three are symlinks to the one real rank -- a 1-rank
    # decomposition wearing a 4-rank costume. Path.is_dir() follows symlinks,
    # so a naive check accepts this.
    item, case_dir = _case(tmp_path)
    _write_subdomains(case_dir, 4)
    _write_ranks(case_dir, {0: ("0.5",)})
    for rank in (1, 2, 3):
        (case_dir / f"processor{rank}").symlink_to(case_dir / "processor0")
    recorded, runner = _recorder()

    row = _resume(item, workers=4, runner=runner)

    assert row["error"] == (
        "resume refused: processor1 is a symlink, not a real decomposition "
        "directory"
    )
    assert recorded == []


def test_resume_refuses_a_rank_symlinked_outside_the_case(tmp_path):
    # The worst version: a rank pointing at an unrelated case, so the gate
    # would report a "common latest time" read from outside this case.
    item, case_dir = _case(tmp_path)
    _write_subdomains(case_dir, 2)
    _write_ranks(case_dir, {0: ("0.5",)})
    elsewhere = tmp_path / "some_other_case"
    (elsewhere / "0.9").mkdir(parents=True)
    (case_dir / "processor1").symlink_to(elsewhere)
    recorded, runner = _recorder()

    row = _resume(item, workers=2, runner=runner)

    assert row["error"] == (
        "resume refused: processor1 is a symlink, not a real decomposition "
        "directory"
    )


def test_resume_refuses_a_symlinked_time_directory(tmp_path):
    # A rank whose newest time is a symlink to an older one would report a
    # latest time it does not actually hold.
    item, case_dir = _case(tmp_path)
    _write_subdomains(case_dir, 2)
    _write_ranks(case_dir, {0: ("0.5",), 1: ("0.4",)})
    (case_dir / "processor1" / "0.5").symlink_to(case_dir / "processor1" / "0.4")
    recorded, runner = _recorder()

    row = _resume(item, workers=2, runner=runner)

    assert row["error"] == (
        "resume refused: processor1/0.5 is a symlink, not a real time directory"
    )


def test_resume_refuses_duplicate_time_spellings_within_a_rank(tmp_path):
    # 0.5 and 0.50 are the same instant spelled two ways. Which one OpenFOAM
    # restarts from is not verified here, and max() would break the tie by
    # filesystem iteration order. artifact_index._time_roots treats this exact
    # condition as fatal; this gate agrees rather than guessing.
    item, case_dir = _case(tmp_path)
    _write_subdomains(case_dir, 2)
    _write_ranks(case_dir, {0: ("0.5", "0.50"), 1: ("0.5",)})
    recorded, runner = _recorder()

    row = _resume(item, workers=2, runner=runner)

    assert row["error"] == (
        "resume refused: processor0 holds duplicate numeric time spellings: "
        "0.5, 0.50"
    )


def test_resume_with_no_processor_directories_refuses_instead_of_rebuilding(tmp_path):
    # Previously "resume: true" with no processor* dirs short-circuited to
    # resume=False, so _prepare_mpi_case took the _clean path and rmtree'd the
    # case. An operator asking to resume got a full wipe reported as
    # "completed". The gate must see this input, not be bypassed before it.
    item, case_dir = _case(tmp_path)
    _write_subdomains(case_dir, 4)
    (case_dir / "0.5").mkdir()
    (case_dir / "0.5" / "U").write_text("solved field\n")
    recorded, runner = _recorder()

    row = _resume(item, workers=4, runner=runner)

    assert row["error"] == (
        "resume refused: processor directory set does not match workers 4: "
        "observed 0, required 4; missing processor0, processor1, processor2, "
        "processor3"
    )
    # The solved field the operator wanted to resume from still exists.
    assert (case_dir / "0.5" / "U").read_text() == "solved field\n"


def test_resume_refuses_a_symlinked_control_dict(tmp_path):
    # set_start_from_latest_time writes THROUGH a symlink, so an accepted
    # resume whose system/controlDict points elsewhere rewrites a DIFFERENT
    # case's controlDict and reports completed. Reading a shared dictionary is
    # defensible; mutating a file outside the case under test is not.
    item, case_dir = _case(tmp_path)
    _write_subdomains(case_dir, 4)
    _write_ranks(case_dir, {rank: ("0.5",) for rank in range(4)})
    victim = tmp_path / "neighbour_case" / "system"
    victim.mkdir(parents=True)
    (victim / "controlDict").write_text(CONTROL_DICT)
    before = sha256((victim / "controlDict").read_bytes()).hexdigest()
    (case_dir / "system" / "controlDict").unlink()
    (case_dir / "system" / "controlDict").symlink_to(victim / "controlDict")
    recorded, runner = _recorder()

    row = _resume(item, workers=4, runner=runner)

    assert row["error"] == (
        "resume refused: system/controlDict resolves outside the case, so "
        "restarting would rewrite another case's controlDict"
    )
    assert sha256((victim / "controlDict").read_bytes()).hexdigest() == before
    assert recorded == []


def test_resume_refuses_a_rank_whose_time_directory_is_empty(tmp_path):
    # An interrupted parallel write is exactly how a time directory ends up
    # created but empty, and that is the resume scenario. There is nothing to
    # restart from. Emptiness is decided by artifact_index.snapshot_tree, the
    # same helper _candidate_has_files uses, not a second walker.
    item, case_dir = _case(tmp_path)
    _write_subdomains(case_dir, 4)
    _write_ranks(case_dir, {rank: ("0.5",) for rank in range(4)})
    (case_dir / "processor3" / "0.5" / "U").unlink()
    recorded, runner = _recorder()

    row = _resume(item, workers=4, runner=runner)

    assert row["error"] == (
        "resume refused: processor3/0.5 is empty, so there is nothing to "
        "restart from"
    )


def test_resume_accepts_the_zero_orig_symlink_idiom(tmp_path):
    # processorN/0 -> 0.orig is standard OpenFOAM. It must not refuse when a
    # real later time exists: the symlink is not, and could not be, the
    # maximum.
    item, case_dir = _case(tmp_path)
    _write_subdomains(case_dir, 2)
    for rank in range(2):
        rank_dir = case_dir / f"processor{rank}"
        (rank_dir / "0.orig").mkdir(parents=True)
        (rank_dir / "0.orig" / "U").write_text("initial\n")
        (rank_dir / "0").symlink_to(rank_dir / "0.orig")
        (rank_dir / "1.0").mkdir()
        (rank_dir / "1.0" / "U").write_text("solved\n")
    recorded, runner = _recorder()

    row = _resume(item, workers=2, runner=runner)

    assert row["status"] == "completed"


def test_resume_reports_a_set_mismatch_for_a_stray_symlink(tmp_path):
    # A stray processor_backup symlink beside an otherwise perfect set must
    # not pre-empt the set-mismatch message, which is the more informative
    # refusal.
    item, case_dir = _case(tmp_path)
    _write_subdomains(case_dir, 2)
    _write_ranks(case_dir, {0: ("0.5",), 1: ("0.5",)})
    (case_dir / "processor_backup").symlink_to(tmp_path)
    recorded, runner = _recorder()

    row = _resume(item, workers=2, runner=runner)

    assert row["error"] == (
        "resume refused: processor directory set does not match workers 2: "
        "observed 3, required 2; unexpected processor_backup"
    )


def test_mock_resume_previews_the_plan_without_a_decomposition(tmp_path):
    # A plan preview must not require a real on-disk decomposition, and must
    # not mutate the case. Before the gate, mock + resume on a virgin case
    # failed outright.
    base = {"case_type": "current_loading", "solver": "interFoam"}
    item = ofb._render_cases(base, [{}], {}, tmp_path / "work")[0]

    row = ofb._run_case_mpi(
        item, {"resume": True, "reconstruct": True}, workers=4, mock=True,
        command_runner=None,
    )

    assert row["status"] == "completed"
    assert row["mpi_plan"] == ["mpirun -np 4 interFoam -parallel",
                               "reconstructPar"]


def test_resume_refuses_a_missing_case_directory(tmp_path):
    with pytest.raises(decomposition.DecompositionMismatch) as caught:
        decomposition.verify_resumable_decomposition(tmp_path / "absent", 4)

    assert str(caught.value) == (
        "resume refused: the case directory does not exist, so there is no "
        "decomposition to reuse"
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


def _ranks_and_dict(case_dir: Path, dict_text: str) -> None:
    (case_dir / "system" / "decomposeParDict").write_text(dict_text)
    _write_ranks(case_dir, {rank: ("0.5",) for rank in range(4)})


def test_resume_ignores_a_commented_out_subdomain_count(tmp_path):
    # A stale "// numberOfSubdomains 4;" above the live "numberOfSubdomains 8;"
    # must not satisfy the check. Reading the commented value accepts an
    # 8-rank decomposition for a 4-rank run, which is exactly the hazard the
    # gate exists to prevent.
    item, case_dir = _case(tmp_path)
    _ranks_and_dict(case_dir, "// numberOfSubdomains 4;\n"
                              "numberOfSubdomains 8;\nmethod scotch;\n")
    recorded, runner = _recorder()

    row = _resume(item, workers=4, runner=runner)

    assert row["error"] == (
        "resume refused: system/decomposeParDict declares numberOfSubdomains 8, "
        "required 4"
    )


def test_resume_ignores_a_block_commented_subdomain_count(tmp_path):
    item, case_dir = _case(tmp_path)
    _ranks_and_dict(case_dir, "/* numberOfSubdomains 4; */\n"
                              "numberOfSubdomains 8;\n")
    recorded, runner = _recorder()

    row = _resume(item, workers=4, runner=runner)

    assert row["error"] == (
        "resume refused: system/decomposeParDict declares numberOfSubdomains 8, "
        "required 4"
    )


def test_resume_refuses_a_dictionary_declaring_no_subdomain_count(tmp_path):
    # A decomposeParDict that exists but takes its value from an #include.
    item, case_dir = _case(tmp_path)
    _ranks_and_dict(case_dir, "FoamFile { object decomposeParDict; }\n"
                              "#include \"decomposeParDict.local\"\n")
    recorded, runner = _recorder()

    row = _resume(item, workers=4, runner=runner)

    assert row["error"] == (
        "resume refused: system/decomposeParDict declares no "
        "numberOfSubdomains, so the decomposition cannot be shown to match "
        "workers 4"
    )


def test_resume_refuses_unicode_digits_in_the_subdomain_count(tmp_path):
    # Python's \d matches Unicode decimal digits, so "٤" would parse as 4.
    # OpenFOAM would not read that file, so neither does this gate.
    item, case_dir = _case(tmp_path)
    _ranks_and_dict(case_dir, "numberOfSubdomains ٤;\n")
    recorded, runner = _recorder()

    row = _resume(item, workers=4, runner=runner)

    assert row["error"] == (
        "resume refused: system/decomposeParDict declares no "
        "numberOfSubdomains, so the decomposition cannot be shown to match "
        "workers 4"
    )


def test_resume_reads_a_symlinked_decompose_par_dict(tmp_path):
    # Deliberate asymmetry with the rank-directory rule. Sharing a dictionary
    # between cases by symlink is ordinary OpenFOAM practice and the solver
    # will read the same target, so following it makes the check agree with
    # the run it validates. A symlinked RANK is different: four names pointing
    # at one directory is one rank of data, not four.
    item, case_dir = _case(tmp_path)
    _write_ranks(case_dir, {rank: ("0.5",) for rank in range(4)})
    shared = tmp_path / "shared_decomposeParDict"
    shared.write_text("numberOfSubdomains 4;\nmethod scotch;\n")
    (case_dir / "system" / "decomposeParDict").symlink_to(shared)
    recorded, runner = _recorder()

    row = _resume(item, workers=4, runner=runner)

    assert row["status"] == "completed"


def test_resume_refuses_a_doubly_declared_subdomain_count(tmp_path):
    # Two live, conflicting declarations. Which one OpenFOAM honours is not
    # verified here and is not guessed: an ambiguous dictionary is refused.
    item, case_dir = _case(tmp_path)
    _ranks_and_dict(case_dir, "numberOfSubdomains 4;\nnumberOfSubdomains 8;\n")
    recorded, runner = _recorder()

    row = _resume(item, workers=4, runner=runner)

    assert row["error"] == (
        "resume refused: system/decomposeParDict declares numberOfSubdomains "
        "more than once, with conflicting values 4, 8"
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
