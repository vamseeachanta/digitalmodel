"""Tests for the concept-screening orchestrator (issue #1024).

Closed-form / licence-free. Verifies the variant grid, the four checks, the
governing-margin verdict and the tidy row export.
"""

import pytest

from digitalmodel.floating_wind.floaters import IEA_15MW_RNA
from digitalmodel.floating_wind.screening import (
    LoadCase,
    ScreeningCriteria,
    screen_concept,
)
from digitalmodel.orcaflex.batch_parametric import ParameterSweep


@pytest.fixture
def load_cases() -> list[LoadCase]:
    return [
        LoadCase(name="operational", hs_m=2.5, tp_s=8.0, steady_load_kN=2400.0),
        LoadCase(name="extreme", hs_m=10.0, tp_s=14.0, steady_load_kN=3000.0),
    ]


SEMI_BASE = dict(
    n_columns=3,
    column_diameter=12.5,
    column_radius=51.75,
    draft=20.0,
    pontoon_height=7.0,
    pontoon_volume=10000.0,
    steel_mass_t=4000.0,
)


def test_grid_is_full_factorial(load_cases):
    res = screen_concept(
        "semi",
        SEMI_BASE,
        sweeps=[
            ParameterSweep(name="column_diameter", values=[11.0, 12.5, 14.0]),
            ParameterSweep(name="column_radius", values=[45.0, 51.75]),
        ],
        topside=IEA_15MW_RNA,
        load_cases=load_cases,
    )
    assert res.n_variants == 6  # 3 x 2 full factorial
    assert res.archetype.value == "semi"
    assert res.load_cases == ["operational", "extreme"]


def test_a_well_proportioned_semi_passes(load_cases):
    res = screen_concept(
        "semi",
        SEMI_BASE,
        sweeps=[ParameterSweep(name="column_diameter", values=[12.5])],
        topside=IEA_15MW_RNA,
        load_cases=load_cases,
    )
    v = res.variants[0]
    assert v.feasible
    assert v.passed
    assert res.n_passed == 1
    # Every check present and passing; stability + modal are per-variant,
    # motion is per-load-case.
    names = {c.name for c in v.checks}
    assert {"stability", "modal", "motion"} <= names


def test_barge_is_flagged_for_wave_band_resonance(load_cases):
    # A barge's natural periods land inside the energetic wave band.
    res = screen_concept(
        "barge",
        dict(length=60.0, beam=60.0, draft=12.0, steel_mass_t=5000.0),
        sweeps=[ParameterSweep(name="draft", values=[10.0, 12.0])],
        topside=IEA_15MW_RNA,
        load_cases=load_cases,
    )
    assert res.n_passed == 0
    for v in res.variants:
        assert not v.passed
        assert v.governing_check in {"modal", "motion"}


def test_governing_check_has_smallest_margin(load_cases):
    res = screen_concept(
        "semi",
        SEMI_BASE,
        sweeps=[ParameterSweep(name="column_diameter", values=[12.5])],
        topside=IEA_15MW_RNA,
        load_cases=load_cases,
    )
    v = res.variants[0]
    smallest = min(v.checks, key=lambda c: c.margin)
    assert v.governing_check == smallest.name
    assert v.governing_margin == pytest.approx(smallest.margin)


def test_mooring_check_only_when_criteria_given(load_cases):
    without = screen_concept(
        "semi",
        SEMI_BASE,
        sweeps=[ParameterSweep(name="column_diameter", values=[12.5])],
        topside=IEA_15MW_RNA,
        load_cases=load_cases,
    )
    assert "mooring" not in {c.name for c in without.variants[0].checks}

    with_moor = screen_concept(
        "semi",
        SEMI_BASE,
        sweeps=[ParameterSweep(name="column_diameter", values=[12.5])],
        topside=IEA_15MW_RNA,
        load_cases=load_cases,
        criteria=ScreeningCriteria(
            mooring_stiffness_kN_per_m=400.0, max_offset_m=30.0
        ),
    )
    moor = [c for c in with_moor.variants[0].checks if c.name == "mooring"]
    assert len(moor) == 1
    # Offset = governing (largest) steady load / stiffness = 3000 / 400 = 7.5 m.
    assert moor[0].value == pytest.approx(3000.0 / 400.0)


def test_infeasible_variant_does_not_pass(load_cases):
    res = screen_concept(
        "barge",
        dict(length=20.0, beam=20.0, draft=4.0, steel_mass_t=3000.0),
        sweeps=[ParameterSweep(name="draft", values=[4.0])],
        topside=IEA_15MW_RNA,
        load_cases=load_cases,
    )
    v = res.variants[0]
    assert not v.feasible
    assert not v.passed


def test_to_rows_is_tidy_long_form(load_cases):
    res = screen_concept(
        "semi",
        SEMI_BASE,
        sweeps=[ParameterSweep(name="column_diameter", values=[11.0, 12.5])],
        topside=IEA_15MW_RNA,
        load_cases=load_cases,
    )
    rows = res.to_rows()
    # One row per variant x check.
    assert len(rows) == sum(len(v.checks) for v in res.variants)
    row = rows[0]
    assert {"case_id", "archetype", "check", "value", "passed", "margin"} <= set(row)
    assert "param_column_diameter" in row
