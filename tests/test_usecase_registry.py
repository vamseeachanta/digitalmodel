"""Solver use-case registry integrity (digitalmodel #938 b / #941 / #942)."""

from __future__ import annotations

import subprocess
from pathlib import Path

import pytest

from digitalmodel.usecase_registry import (
    READINESS,
    REPO_ROOT,
    SOLVERS,
    by_solver,
    load_usecases,
    readiness_counts,
    validate_registry,
)


def test_registry_loads_and_is_nonempty():
    cases = load_usecases()
    assert len(cases) > 20
    assert all(c.solver in SOLVERS for c in cases)
    assert all(c.readiness in READINESS for c in cases)


def test_registry_is_internally_valid():
    # The core invariant: ready use-cases have a resolvable template, ids are
    # unique, solvers/readiness are known. Empty issue list == sound registry.
    issues = validate_registry(REPO_ROOT)
    assert issues == [], "registry integrity issues:\n" + "\n".join(issues)


def test_all_solvers_present():
    grouped = by_solver()
    assert set(grouped) == SOLVERS
    # AQWA + ANSYS are now in the registry (deployable programs #939/#940).
    assert grouped["aqwa"]
    assert grouped["ansys"]
    # OpenFOAM CFD route is registered (engine basename openfoam/cfd, #1161).
    assert grouped["openfoam"]


def test_readiness_counts_cover_every_case():
    cases = load_usecases()
    counts = readiness_counts(cases)
    assert sum(counts.values()) == len(cases)
    assert counts["ready"] >= 1


def test_ready_cases_have_existing_templates():
    for case in load_usecases():
        if case.readiness == "ready":
            path = case.template_path()
            assert path is not None and path.exists(), f"{case.id}: {case.template}"


@pytest.mark.parametrize("case_id", ["ansys-padeye", "ansys-pressure-vessel", "ansys-mudmat"])
def test_ansys_examples_await_native_qualification(case_id):
    """Issue 2094: archived regression values do not qualify the current run lane.

    Remove a case from this hold only with reviewed native qualification evidence.
    Template existence and a golden file alone cannot lift the hold.
    """
    case = next(case for case in load_usecases() if case.id == case_id)
    assert case.readiness == "partial", f"{case_id}: native qualification is outstanding"


def test_ready_ansys_cases_have_committed_goldens():
    """Apply the approved golden prerequisite only to the inspected ANSYS lane."""
    tracked = set(subprocess.check_output(
        ["git", "ls-files", "--", "examples/ansys"], cwd=REPO_ROOT, text=True,
    ).splitlines())
    for case in by_solver()["ansys"]:
        if case.readiness != "ready":
            continue
        golden = Path(case.template).parent / "golden"
        provenance = golden / "PROVENANCE.json"
        assert provenance.as_posix() in tracked, f"{case.id}: no committed provenance"
        assert (REPO_ROOT / provenance).is_file(), f"{case.id}: provenance unavailable"
        digests = list((REPO_ROOT / golden).glob("*_result.csv"))
        assert digests, f"{case.id}: no golden result digest"
        assert all(path.relative_to(REPO_ROOT).as_posix() in tracked for path in digests)


def test_ansys_scratch_ignore_preserves_golden_and_source_evidence():
    cases = by_solver()["ansys"]
    scratch = [str(Path(case.template).parent / "results" / "native.out") for case in cases]
    evidence = [str(Path(case.template)) for case in cases]
    evidence += ["examples/ansys/pressure-vessel/golden/PROVENANCE.json",
                 "examples/ansys/pressure-vessel/golden/pv_result.csv",
                 "examples/ansys/pressure-vessel/pv.inp",
                 "examples/ansys/mudmat/golden/PROVENANCE.json",
                 "examples/ansys/mudmat/golden/mudmat_result.csv",
                 "examples/ansys/mudmat/mudmat.inp"]
    for path in scratch:
        assert subprocess.run(
            ["git", "check-ignore", "--no-index", "--quiet", "--", path], cwd=REPO_ROOT,
        ).returncode == 0, path
    for path in evidence:
        assert subprocess.run(
            ["git", "check-ignore", "--no-index", "--quiet", "--", path], cwd=REPO_ROOT,
        ).returncode == 1, path
