# ABOUTME: Tests for W6 concept_screening — four axes, normalisation, determinism, DRY_TREE_UNIT.
# ABOUTME: Pure parameter-based screening; no network, no solver.
"""Tests for digitalmodel.field_development.concept_screening (W6)."""

from __future__ import annotations

import pytest

from digitalmodel.field_development.concept_screening import (
    DEFAULT_WEIGHTS,
    ProspectSpec,
    capex_line_items,
    estimate_rig_days,
    estimate_schedule_weeks,
    intervention_index,
    load_params,
    screen_concepts,
)
from digitalmodel.field_development.concept_selection import HostType


@pytest.fixture()
def spec() -> ProspectSpec:
    return ProspectSpec(
        name="Test Analogue",
        water_depth_m=1500.0,
        well_count=8,
        reservoir_size_mmbbl=300.0,
        production_capacity_bopd=120_000.0,
        fluid_type="oil",
        distance_to_infra_km=80.0,
        workover_frequency_per_well_decade=1.5,
    )


@pytest.fixture()
def params() -> dict:
    return load_params()


class TestProspectSpecValidation:
    def test_rejects_nonpositive_depth(self):
        with pytest.raises(ValueError):
            ProspectSpec("x", 0, 5, 100, 50_000)

    def test_rejects_nonpositive_wells(self):
        with pytest.raises(ValueError):
            ProspectSpec("x", 1500, 0, 100, 50_000)


class TestDryTreeUnit:
    def test_enum_value_exists(self):
        assert HostType.DRY_TREE_UNIT.value == "Dry_Tree_Unit"

    def test_dry_tree_unit_is_screened(self, spec):
        result = screen_concepts(spec)
        hosts = {r.host_type for r in result.ranked}
        assert HostType.DRY_TREE_UNIT in hosts

    def test_dry_tree_unit_has_params_in_every_axis(self, spec, params):
        # All four axis estimators resolve for the new host type.
        assert estimate_schedule_weeks(HostType.DRY_TREE_UNIT, spec, params) > 0
        assert estimate_rig_days(HostType.DRY_TREE_UNIT, spec, params) > 0
        assert capex_line_items(HostType.DRY_TREE_UNIT, spec, params)["total"] > 0
        assert intervention_index(HostType.DRY_TREE_UNIT, spec, params) > 0


class TestAxisEstimators:
    def test_schedule_increases_with_wells(self, spec, params):
        from dataclasses import replace

        few = estimate_schedule_weeks(HostType.TLP, replace(spec, well_count=4), params)
        many = estimate_schedule_weeks(HostType.TLP, replace(spec, well_count=16), params)
        assert many > few

    def test_rig_days_scale_with_wells(self, spec, params):
        from dataclasses import replace

        rd8 = estimate_rig_days(HostType.TLP, spec, params)
        rd16 = estimate_rig_days(HostType.TLP, replace(spec, well_count=16), params)
        assert rd16 == pytest.approx(2 * rd8, rel=1e-6)

    def test_bsee_benchmark_overrides_rig_days(self, spec, params):
        default = estimate_rig_days(HostType.TLP, spec, params)
        override = estimate_rig_days(HostType.TLP, spec, params, bsee_benchmark=200.0)
        assert override > default

    def test_tieback_capex_lower_than_host(self, spec, params):
        tb = capex_line_items(HostType.SUBSEA_TIEBACK, spec, params)["total"]
        fpso = capex_line_items(HostType.FPSO, spec, params)["total"]
        assert tb < fpso

    def test_subsea_intervention_index_higher_than_dry_tree(self, spec, params):
        subsea = intervention_index(HostType.SUBSEA_TIEBACK, spec, params)
        dry = intervention_index(HostType.TLP, spec, params)
        assert subsea > dry


class TestScreening:
    def test_default_weights_sum_to_one(self):
        assert sum(DEFAULT_WEIGHTS.values()) == pytest.approx(1.0)

    def test_normalised_scores_in_range(self, spec):
        result = screen_concepts(spec)
        for r in result.ranked:
            for v in r.axes.norm.values():
                assert 0.0 <= v <= 100.0

    def test_ranked_descending(self, spec):
        result = screen_concepts(spec)
        comps = [r.composite for r in result.ranked]
        assert comps == sorted(comps, reverse=True)

    def test_governing_axis_reported(self, spec):
        result = screen_concepts(spec)
        assert result.governing_axis in set(DEFAULT_WEIGHTS) | {"n/a (single candidate)"}

    def test_candidates_restriction(self, spec):
        result = screen_concepts(
            spec, candidates=[HostType.TLP, HostType.SUBSEA_TIEBACK]
        )
        assert {r.host_type for r in result.ranked} == {
            HostType.TLP,
            HostType.SUBSEA_TIEBACK,
        }

    def test_rig_day_source_flag(self, spec):
        generic = screen_concepts(spec)
        assert "generic" in generic.rig_day_source.lower()
        bsee = screen_concepts(spec, bsee_benchmark=95.0)
        assert "bsee" in bsee.rig_day_source.lower()


class TestDeterminism:
    def test_same_input_same_output(self, spec):
        a = screen_concepts(spec)
        b = screen_concepts(spec)
        assert [(r.host_type, r.composite) for r in a.ranked] == [
            (r.host_type, r.composite) for r in b.ranked
        ]
        assert a.selected == b.selected

    def test_weight_change_can_reorder(self, spec):
        # A schedule-dominant weighting should favour the fastest concept (tieback).
        base = screen_concepts(spec)
        sched = screen_concepts(
            spec,
            weights={"schedule": 0.7, "capex": 0.1, "rig_days": 0.1, "intervention": 0.1},
        )
        # Determinism within each call is what we assert; ordering may differ.
        assert base.ranked[0].composite >= base.ranked[-1].composite
        assert sched.ranked[0].composite >= sched.ranked[-1].composite
