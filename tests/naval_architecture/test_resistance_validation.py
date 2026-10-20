# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic benchmark gates with explicit, nonphysical acceptance choices.

The comparison example is independent arithmetic: |103-100|=3 N;
atol=1 N plus rtol=.02*|100|=2 N gives a 3 N inclusive limit.
All other numbers are declared synthetic input or rejection cases.
No experimental resistance data or standards-derived limits are used.
"""

from dataclasses import replace

import pytest

from digitalmodel.naval_architecture.mesh_resistance import FlowConditions
from digitalmodel.naval_architecture.resistance_validation import (
    BenchmarkCase,
    compare_cases,
    ensure_independent_split,
)


@pytest.fixture
def case():
    return BenchmarkCase(
        case_id="synthetic",
        family="box",
        geometry_digest="a" * 64,
        source_digest="b" * 64,
        source_revision="analytic-derivation-v1",
        rights_basis="Original synthetic arithmetic",
        redistribution_allowed=True,
        role="test",
        conditions=FlowConditions(
            1, 1000, 1e-5, 10, 1, 0, None, "calm", "smooth", "absent"
        ),
        length_m=10,
        wetted_area_m2=44,
        observable="friction_force",
        normalization="dimensional",
        units="N",
        value=100,
    )


def compare(a, b):
    return compare_cases(
        a,
        b,
        absolute_tolerance=1,
        relative_tolerance=0.02,
        tolerance_source="Synthetic arithmetic in test docstring",
    )


def test_independent_comparison_limit(case):
    result = compare(replace(case, value=103), case)
    assert result["status"] == "PASS"
    assert result["absolute_error"] == 3
    assert result["limit"] == 3
    assert compare(replace(case, value=104), case)["status"] == "FAIL"


@pytest.mark.parametrize(
    "field,value",
    [
        ("observable", "total_force"),
        ("normalization", "rho-U2-S/2"),
        ("units", "kN"),
        ("length_m", 20),
        ("wetted_area_m2", 45),
        ("geometry_digest", "c" * 64),
    ],
)
def test_mismatched_comparisons_refused(case, field, value):
    with pytest.raises(ValueError, match="mismatch"):
        compare(replace(case, **{field: value}), case)


def test_flow_mismatch_refused(case):
    with pytest.raises(ValueError, match="conditions"):
        compare(
            replace(case, conditions=replace(case.conditions, viscosity_m2_s=2e-5)),
            case,
        )


@pytest.mark.parametrize(
    "field,value",
    [
        ("family", "box"),
        ("geometry_digest", "a" * 64),
        ("source_digest", "b" * 64),
    ],
)
def test_family_geometry_or_source_leakage_refused(case, field, value):
    train = replace(
        case,
        case_id="renamed",
        family="other",
        geometry_digest="c" * 64,
        source_digest="d" * 64,
        role="train",
    )
    train = replace(train, **{field: value})
    with pytest.raises(ValueError, match="leakage"):
        ensure_independent_split([case, train])


def test_disjoint_split_and_same_role_repeats(case):
    other = replace(
        case,
        case_id="other",
        family="other",
        geometry_digest="c" * 64,
        source_digest="d" * 64,
        role="train",
    )
    ensure_independent_split([case, other, replace(case, case_id="repeat")])


@pytest.mark.parametrize(
    "field,value",
    [
        ("source_revision", ""),
        ("rights_basis", ""),
        ("geometry_digest", "bad"),
        ("role", "unknown"),
        ("value", float("nan")),
        ("length_m", 0),
    ],
)
def test_incomplete_records_refused(case, field, value):
    with pytest.raises(ValueError):
        replace(case, **{field: value})


def test_rights_block_export_without_blocking_read_only_comparison(case):
    restricted = replace(case, redistribution_allowed=False)
    assert compare(restricted, case)["status"] == "PASS"
    with pytest.raises(ValueError, match="redistribution"):
        restricted.to_public_dict()


def test_missing_tolerance_source_refused(case):
    with pytest.raises(ValueError):
        compare_cases(
            case, case, absolute_tolerance=1, relative_tolerance=0, tolerance_source=""
        )


@pytest.mark.parametrize(
    "atol,rtol", [(-1, 0), (0, -1), (float("nan"), 0), (0, float("inf"))]
)
def test_invalid_tolerances_refused(case, atol, rtol):
    with pytest.raises(ValueError):
        compare_cases(
            case,
            case,
            absolute_tolerance=atol,
            relative_tolerance=rtol,
            tolerance_source="synthetic rejection case",
        )


def test_zero_reference_uses_absolute_limit(case):
    assert compare(replace(case, value=1), replace(case, value=0))["status"] == "PASS"
