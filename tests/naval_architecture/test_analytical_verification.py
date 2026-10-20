# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Evidence bookkeeping, using synthetic residuals and declared test criteria.

Residual 0 satisfies limit 0; residual 2 exceeds limit 1. These are arithmetic
tests of the gate, not PDE solutions. An independently derived steady uniform
flow has zero derivatives, continuity and acceleration in an unbounded domain;
it supplies only a possible future anchor, never hull boundary conditions.
"""

from dataclasses import replace

import pytest

from digitalmodel.naval_architecture.analytical_verification import (
    REQUIRED_CHECKS,
    Candidate,
    EvidenceCheck,
    assess_candidate,
)


@pytest.fixture
def candidate():
    return Candidate(
        "synthetic-expression",
        "exact_under_assumptions",
        ("synthetic domain",),
        "Independent synthetic derivation v1",
    )


def checks():
    return {
        key: EvidenceCheck(
            0,
            0,
            "dimensionless",
            "Synthetic zero-residual criterion",
            "Independent hand calculation in test docstring",
        )
        for key in REQUIRED_CHECKS
    }


def test_complete_evidence_is_not_proof(candidate):
    result = assess_candidate(candidate, checks(), {"x": (0, 1)}, {"x": 0.5})
    assert result["status"] == "CHECKS_PASSED"
    assert result["formal_proof"] is False
    assert result["method_class"] == "exact_under_assumptions"


def test_missing_checks_not_evaluated(candidate):
    result = assess_candidate(candidate, {}, {"x": (0, 1)}, {"x": 0.5})
    assert result["status"] == "NOT_EVALUATED"
    assert set(result["missing_checks"]) == set(REQUIRED_CHECKS)


def test_falsification_preserves_negative_result(candidate):
    evidence = checks()
    evidence["boundary_conditions"] = replace(
        evidence["boundary_conditions"], magnitude=2, limit=1
    )
    result = assess_candidate(candidate, evidence, {"x": (0, 1)}, {"x": 0.5})
    assert result["status"] == "REJECTED"
    assert result["failed_checks"] == ["boundary_conditions"]
    assert result["checks"]["boundary_conditions"]["magnitude"] == 2


def test_extrapolation_rejected(candidate):
    assert (
        assess_candidate(candidate, checks(), {"x": (0, 1)}, {"x": 2})["status"]
        == "REJECTED"
    )


@pytest.mark.parametrize(
    "bounds,point",
    [
        ({}, {}),
        ({"x": (1, 0)}, {"x": 0}),
        ({"x": (0, 1)}, {}),
        ({"x": (0, 1)}, {"x": float("nan")}),
    ],
)
def test_unknown_domain_refused(candidate, bounds, point):
    with pytest.raises(ValueError):
        assess_candidate(candidate, checks(), bounds, point)


def test_surrogate_label_retained(candidate):
    surrogate = replace(candidate, method_class="learned_surrogate")
    assert (
        assess_candidate(surrogate, checks(), {"x": (0, 1)}, {"x": 0})["method_class"]
        == "learned_surrogate"
    )


@pytest.mark.parametrize("magnitude,limit", [(float("nan"), 0), (0, -1), (-1, 0)])
def test_invalid_residual_refused(magnitude, limit):
    with pytest.raises(ValueError):
        EvidenceCheck(magnitude, limit, "m/s", "synthetic", "independent")


def test_unclassified_method_refused(candidate):
    with pytest.raises(ValueError):
        replace(candidate, method_class="closed_form_arbitrary_hull")
