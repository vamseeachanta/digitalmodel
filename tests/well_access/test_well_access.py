# ABOUTME: Tests for W7 well_access — NHPP demand/realized/deferred, queue, uptime, determinism.
# ABOUTME: Asserts circularity guard (demand access-neutral) + FrPS-assumption flag.
"""Tests for digitalmodel.well_access (W7)."""

from __future__ import annotations

import pytest

from digitalmodel.well_access import (
    AccessClass,
    AgeBandRate,
    InterventionType,
    ResourcePool,
    WellProgram,
    compute_resource_demand,
    expected_demand_per_well,
    expected_events,
    resolve_access_multiplier,
    rollup_uptime,
    run_lifecycle,
    compare_architectures,
)


@pytest.fixture()
def calibration() -> dict:
    return {
        "base_rates": {
            "light_well_intervention": {
                "duration_days": 8.0,
                "resource_class": "lwi_vessel",
                "age_curve": [
                    {"age_min_yr": 0, "age_max_yr": 10, "rate_per_well_year": 0.15},
                    {"age_min_yr": 10, "age_max_yr": 30, "rate_per_well_year": 0.25},
                ],
            },
            "workover": {
                "duration_days": 45.0,
                "resource_class": "modu",
                "age_curve": [
                    {"age_min_yr": 0, "age_max_yr": 30, "rate_per_well_year": 0.03},
                ],
            },
            "recompletion": {
                "duration_days": 60.0,
                "resource_class": "modu",
                "low_confidence": True,
                "age_curve": [
                    {"age_min_yr": 0, "age_max_yr": 30, "rate_per_well_year": 0.01},
                ],
            },
        }
    }


@pytest.fixture()
def program() -> WellProgram:
    return WellProgram(
        n_wells=8,
        field_life_years=25,
        access_class=AccessClass.DRY_TREE_TLP,
        facility_availability=0.95,
    )


@pytest.fixture()
def access_table() -> dict:
    return {
        "dry_tree_tlp": {
            "light_well_intervention": 1.0,
            "workover": 1.0,
            "recompletion": 1.0,
        },
        "subsea_hub_spoke": {
            "light_well_intervention": 0.8,
            "workover": 0.6,
            "recompletion": 0.45,
        },
        "dry_tree_frps": {
            "light_well_intervention": 1.0,
            "workover": 0.98,
            "recompletion": 0.95,
        },
    }


@pytest.fixture()
def resources() -> list[dict]:
    return [
        {"resource_class": "modu", "available_days_per_year": 120.0,
         "mobilisation_lead_days": 45.0, "batch_campaign": True},
        {"resource_class": "lwi_vessel", "available_days_per_year": 200.0,
         "mobilisation_lead_days": 10.0, "batch_campaign": False},
    ]


class TestProgramValidation:
    def test_rejects_bad_availability(self):
        with pytest.raises(ValueError):
            WellProgram(4, 20, AccessClass.DRY_TREE_TLP, facility_availability=1.5)

    def test_rejects_zero_wells(self):
        with pytest.raises(ValueError):
            WellProgram(0, 20, AccessClass.DRY_TREE_TLP)


class TestNHPP:
    def test_demand_integrates_over_bands(self):
        itype = InterventionType(
            "x",
            [AgeBandRate(0, 10, 0.1), AgeBandRate(10, 30, 0.2)],
            duration_days=10.0,
            resource_class="modu",
        )
        # life 25 yr -> 10*0.1 + 15*0.2 = 1.0 + 3.0 = 4.0 per well
        assert expected_demand_per_well(itype, 25) == pytest.approx(4.0)

    def test_life_caps_band_integration(self):
        itype = InterventionType(
            "x", [AgeBandRate(0, 100, 0.1)], duration_days=1.0, resource_class="modu"
        )
        assert expected_demand_per_well(itype, 5) == pytest.approx(0.5)


class TestCircularityGuard:
    def test_dry_tree_demand_equals_realized(self, program, calibration, access_table):
        from digitalmodel.well_access.intervention_rates import build_intervention_types

        itypes = build_intervention_types(calibration)
        mult, _ = resolve_access_multiplier(AccessClass.DRY_TREE_TLP, access_table)
        events = expected_events(program, itypes, mult)
        for e in events:
            # dry-tree m_access == 1 -> demand == realized, zero deferred
            assert e.realized == pytest.approx(e.demand)
            assert e.deferred == pytest.approx(0.0)

    def test_subsea_defers_demand(self, program, calibration, access_table):
        from dataclasses import replace
        from digitalmodel.well_access.intervention_rates import build_intervention_types

        itypes = build_intervention_types(calibration)
        sub = replace(program, access_class=AccessClass.SUBSEA_HUB_SPOKE)
        mult, _ = resolve_access_multiplier(AccessClass.SUBSEA_HUB_SPOKE, access_table)
        events = expected_events(sub, itypes, mult)
        total_deferred = sum(e.deferred for e in events)
        assert total_deferred > 0  # access suppression creates backlog


class TestFrPSAssumptionFlag:
    def test_frps_multiplier_flagged_as_assumption(self, access_table):
        _, basis = resolve_access_multiplier(AccessClass.DRY_TREE_FRPS, access_table)
        assert "ASSUMPTION" in basis
        assert "no operating analog" in basis


class TestResourceQueue:
    def test_rho_and_wait(self, program, calibration, access_table, resources):
        from digitalmodel.well_access.intervention_rates import build_intervention_types
        from digitalmodel.well_access.lifecycle_summary import _build_pools

        itypes = build_intervention_types(calibration)
        mult, _ = resolve_access_multiplier(AccessClass.DRY_TREE_TLP, access_table)
        events = expected_events(program, itypes, mult)
        demand = compute_resource_demand(events, itypes, _build_pools(resources), 25)
        by_class = {d.resource_class: d for d in demand}
        assert by_class["modu"].rho > 0
        # if rho <= 1, no wait
        for d in demand:
            if d.rho <= 1.0:
                assert d.wait_days == 0.0


class TestUptime:
    def test_uptime_bracket_bounded(self, program, calibration, access_table):
        from digitalmodel.well_access.intervention_rates import build_intervention_types

        itypes = build_intervention_types(calibration)
        mult, _ = resolve_access_multiplier(AccessClass.DRY_TREE_TLP, access_table)
        events = expected_events(program, itypes, mult)
        u = rollup_uptime(events, itypes, program, deferred_loss_per_event_days=90.0)
        assert 0.0 <= u.field_uptime <= program.facility_availability


class TestComparisonAndDeterminism:
    def test_dry_tree_uptime_geq_subsea(
        self, program, calibration, access_table, resources
    ):
        cmp = compare_architectures(
            program,
            AccessClass.DRY_TREE_TLP,
            AccessClass.SUBSEA_HUB_SPOKE,
            calibration,
            access_table,
            resources,
            deferred_loss_per_event_days=90.0,
        )
        # dry-tree should have >= uptime and subsea should defer more
        assert cmp["deltas"]["uptime_delta"] >= 0
        assert cmp["deltas"]["deferred_backlog_delta_events"] >= 0

    def test_run_lifecycle_deterministic(
        self, program, calibration, access_table, resources
    ):
        a = run_lifecycle(program, calibration, access_table, resources, 90.0)
        b = run_lifecycle(program, calibration, access_table, resources, 90.0)
        assert a.total_demand_events == b.total_demand_events
        assert a.total_realized_events == b.total_realized_events
        assert a.uptime.field_uptime == b.uptime.field_uptime
