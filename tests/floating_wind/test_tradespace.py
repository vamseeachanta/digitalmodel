"""Tests for trade-space analysis (issue #1026): Pareto front + correlation map."""

import math

import pytest

from digitalmodel.floating_wind.floaters import IEA_15MW_RNA
from digitalmodel.floating_wind.screening import LoadCase, screen_concept
from digitalmodel.floating_wind.tradespace import (
    Objective,
    correlation_map,
    metric_records,
    pareto_front,
)
from digitalmodel.orcaflex.batch_parametric import ParameterSweep

SEMI_BASE = dict(
    n_columns=3,
    column_diameter=12.5,
    column_radius=51.75,
    draft=20.0,
    pontoon_height=7.0,
    pontoon_volume=10000.0,
    steel_mass_t=4000.0,
)


@pytest.fixture
def results():
    return screen_concept(
        "semi",
        SEMI_BASE,
        sweeps=[
            ParameterSweep(name="column_diameter", values=[11.0, 12.5, 14.0]),
            ParameterSweep(name="column_radius", values=[45.0, 51.75, 58.0]),
        ],
        topside=IEA_15MW_RNA,
        load_cases=[
            LoadCase(name="operational", hs_m=2.5, tp_s=8.0),
            LoadCase(name="extreme", hs_m=10.0, tp_s=14.0),
        ],
    )


def test_pareto_front_is_nonempty_and_nondominated(results):
    objectives = [
        Objective(key="steel_mass_t", direction="min"),
        Objective(key="governing_margin", direction="max"),
    ]
    front = pareto_front(results, objectives)
    assert front
    # No front member dominates another on both objectives simultaneously.
    for a in front:
        for b in front:
            if a is b:
                continue
            strictly_lighter = a["steel_mass_t"] < b["steel_mass_t"]
            strictly_tougher = a["governing_margin"] > b["governing_margin"]
            at_least_as_light = a["steel_mass_t"] <= b["steel_mass_t"]
            at_least_as_tough = a["governing_margin"] >= b["governing_margin"]
            dominates = (
                at_least_as_light
                and at_least_as_tough
                and (strictly_lighter or strictly_tougher)
            )
            assert not dominates


def test_pareto_accepts_tuple_objectives(results):
    front_tuples = pareto_front(results, [("displacement_t", "min")])
    # A single-objective front is the unique optimum (ties allowed).
    best = min(r["displacement_t"] for r in metric_records(results))
    assert all(r["displacement_t"] == pytest.approx(best) for r in front_tuples)


def test_pareto_passed_only_subsets_the_front(results):
    objs = [Objective(key="steel_mass_t", direction="min")]
    all_front = pareto_front(results, objs)
    passed_front = pareto_front(results, objs, passed_only=True)
    assert len(passed_front) <= len(all_front)
    assert all(r["passed"] for r in passed_front)


def test_correlation_map_recovers_known_relationships(results):
    cmap = correlation_map(results)
    # Self-correlation is 1 (displacement varies with the swept column diameter).
    assert cmap["displacement_t"]["displacement_t"] == pytest.approx(1.0)
    # GM rises with column spread (radius) -- strong positive correlation.
    assert cmap["param_column_radius"]["GM_m"] > 0.5
    # Symmetry.
    assert cmap["GM_m"]["param_column_radius"] == pytest.approx(
        cmap["param_column_radius"]["GM_m"]
    )


def test_correlation_drops_constant_columns(results):
    cmap = correlation_map(results)
    # draft is fixed across the sweep -> zero variance -> dropped.
    assert "param_draft" not in cmap


def test_metric_records_can_include_infeasible():
    res = screen_concept(
        "barge",
        dict(length=20.0, beam=20.0, draft=4.0, steel_mass_t=3000.0),
        sweeps=[ParameterSweep(name="draft", values=[4.0])],
        topside=IEA_15MW_RNA,
        load_cases=[LoadCase(name="op", hs_m=2.0, tp_s=7.0)],
    )
    assert metric_records(res, feasible_only=True) == []
    assert len(metric_records(res, feasible_only=False)) == 1
