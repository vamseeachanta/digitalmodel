"""MPI stage order, rank ceiling and utility preflight for issue 1576.

The MPI path historically stopped after optional reconstruction: it never
scheduled ``foamToVTK`` however the request was written, it passed an
oversubscription flag, it accepted any rank count, and its readiness probe
required a different utility set than the one it actually bound and launched.
These tests pin each of those as an exact value.
"""

from __future__ import annotations

import pytest

from digitalmodel.workflows import openfoam_run_batch as ofb
from digitalmodel.workflows.openfoam_batch_execution import (
    mpi_command_plan,
    validate_workers,
)


SOLVER = "interFoam"
MESH = "blockMesh"


def _mpi_view(*, run_set_fields: bool = False, to_vtk: bool = False) -> dict:
    return {
        "mesh_utility": MESH,
        "solver": SOLVER,
        "run_set_fields": run_set_fields,
        "to_vtk": to_vtk,
    }


# --------------------------------------------------------------------------- #
#  exact stage order                                                          #
# --------------------------------------------------------------------------- #
def test_fresh_plan_without_vtk_is_the_exact_stage_list() -> None:
    assert mpi_command_plan(SOLVER, 8, MESH, reconstruct=True) == [
        [MESH],
        ["decomposePar", "-force"],
        ["mpirun", "-np", "8", SOLVER, "-parallel"],
        ["reconstructPar"],
    ]


def test_no_stage_carries_an_oversubscription_flag() -> None:
    plan = mpi_command_plan(SOLVER, 8, MESH, reconstruct=True, to_vtk=True)
    tokens = [token for argv in plan for token in argv]

    assert "--oversubscribe" not in tokens


def test_eight_rank_canary_argv_is_exact() -> None:
    plan = mpi_command_plan(SOLVER, 8, MESH, reconstruct=True)
    solver_stage = next(argv for argv in plan if argv[0] == "mpirun")

    assert solver_stage == ["mpirun", "-np", "8", SOLVER, "-parallel"]


def test_to_vtk_appends_foam_to_vtk_after_reconstruction() -> None:
    assert mpi_command_plan(
        SOLVER, 8, MESH, run_set_fields=True, reconstruct=True, to_vtk=True
    ) == [
        [MESH],
        ["setFields"],
        ["decomposePar", "-force"],
        ["mpirun", "-np", "8", SOLVER, "-parallel"],
        ["reconstructPar"],
        ["foamToVTK"],
    ]


def test_reconstruct_mesh_precedes_reconstruct_par() -> None:
    assert mpi_command_plan(
        SOLVER, 8, MESH, reconstruct=True, reconstruct_mesh=True
    ) == [
        [MESH],
        ["decomposePar", "-force"],
        ["mpirun", "-np", "8", SOLVER, "-parallel"],
        ["reconstructParMesh", "-latestTime"],
        ["reconstructPar"],
    ]


def test_vtk_without_reconstruction_rejects_before_mutation() -> None:
    with pytest.raises(ValueError, match="to_vtk"):
        mpi_command_plan(SOLVER, 8, MESH, reconstruct=False, to_vtk=True)


def test_requesting_vtk_changes_the_emitted_stage_set_by_exactly_one_stage() -> None:
    without = [argv[0] for argv in mpi_command_plan(SOLVER, 8, MESH, reconstruct=True)]
    with_vtk = [
        argv[0]
        for argv in mpi_command_plan(SOLVER, 8, MESH, reconstruct=True, to_vtk=True)
    ]

    assert set(with_vtk) - set(without) == {"foamToVTK"}


# --------------------------------------------------------------------------- #
#  rank ceiling                                                               #
# --------------------------------------------------------------------------- #
def test_true_is_not_a_rank_count() -> None:
    with pytest.raises(TypeError):
        validate_workers(True, visible_rank_count=8)


def test_false_is_not_a_rank_count() -> None:
    with pytest.raises(TypeError):
        validate_workers(False, visible_rank_count=8)


def test_zero_ranks_reject() -> None:
    with pytest.raises(ValueError):
        validate_workers(0, visible_rank_count=8)


def test_negative_ranks_reject() -> None:
    with pytest.raises(ValueError):
        validate_workers(-1, visible_rank_count=8)


def test_request_above_visible_ranks_names_request_and_ceiling() -> None:
    with pytest.raises(ValueError) as excinfo:
        validate_workers(9, visible_rank_count=8)
    message = str(excinfo.value)

    assert "9" in message
    assert "8" in message


def test_request_at_the_visible_ceiling_is_returned_unchanged() -> None:
    assert validate_workers(8, visible_rank_count=8) == 8


def test_dispatcher_limit_is_a_ceiling_not_a_second_request() -> None:
    assert validate_workers(8, visible_rank_count=16, dispatcher_rank_limit=8) == 8
    with pytest.raises(ValueError):
        validate_workers(9, visible_rank_count=16, dispatcher_rank_limit=8)


# --------------------------------------------------------------------------- #
#  utility preflight                                                          #
# --------------------------------------------------------------------------- #
def test_mpi_vtk_request_selects_foam_to_vtk() -> None:
    assert ofb.required_utilities(
        "mpi", _mpi_view(to_vtk=True), {"reconstruct": True}
    ) == [MESH, SOLVER, "decomposePar", "mpirun", "reconstructPar", "foamToVTK"]


def test_mpi_set_fields_request_selects_set_fields() -> None:
    assert ofb.required_utilities(
        "mpi", _mpi_view(run_set_fields=True), {"reconstruct": True}
    ) == [MESH, SOLVER, "decomposePar", "mpirun", "setFields", "reconstructPar"]


def test_pool_vtk_request_selects_foam_to_vtk() -> None:
    assert ofb.required_utilities("pool", _mpi_view(to_vtk=True), {}) == [
        MESH,
        SOLVER,
        "foamToVTK",
    ]


def test_readiness_probe_requires_every_selected_utility(monkeypatch) -> None:
    view = _mpi_view(run_set_fields=True, to_vtk=True)
    run_settings = {"reconstruct": True}
    present = set(ofb.required_utilities("mpi", view, run_settings))
    monkeypatch.setattr(
        ofb.shutil, "which",
        lambda exe: "/usr/bin/stub" if exe in present else None,
    )

    assert ofb._solver_ready(
        "mpi", MESH, SOLVER, True, run_set_fields=True, to_vtk=True
    ) is True

    present.discard("foamToVTK")

    assert ofb._solver_ready(
        "mpi", MESH, SOLVER, True, run_set_fields=True, to_vtk=True
    ) is False
