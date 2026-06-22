"""Solver use-case registry integrity (digitalmodel #938 b / #941 / #942)."""

from __future__ import annotations

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


def test_all_four_solvers_present():
    grouped = by_solver()
    assert set(grouped) == SOLVERS
    # AQWA + ANSYS are now in the registry (deployable programs #939/#940).
    assert grouped["aqwa"]
    assert grouped["ansys"]


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
