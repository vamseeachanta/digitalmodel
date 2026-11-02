"""Permanent-barrier screening against explicit, synthetic design inputs."""

from dataclasses import replace

import pytest

from digitalmodel.well.barriers import (
    BarrierCriteria, FlowZone, PermanentBarrier, screen_permanent_barriers,
    public_norsok_2013_criteria,
)


def criteria(**changes):
    # Public D-010 rev.4 seminar, slides 8, 9 and 12. This example selects
    # two separate 100 m plugs, each at least 50 m above a mobile gas zone.
    return replace(BarrierCriteria(2, 100, 50, "Public D-010 rev.4 seminar"), **changes)


def barrier(name="primary", **changes):
    return replace(PermanentBarrier(
        name, "gas", 1800, 1900, 35, 30, True, True, name,
        "Synthetic placement, cross-section and formation records",
    ), **changes)


def screen(plugs=None, **changes):
    return screen_permanent_barriers(
        (FlowZone("gas", 2000, criteria()),),
        plugs if plugs is not None else (
            barrier(), barrier("secondary", top_md_m=1600, base_md_m=1700),
        ), gas_migration=False, sustained_casing_pressure=False, **changes,
    )


def test_two_verified_envelopes_hand_example():
    result = screen()
    # Lengths: 1900-1800=100 m and 1700-1600=100 m.
    # Above-zone coverage: 2000-1800=200 m and 2000-1600=400 m >= 50 m.
    # Base-pressure margins: 35-30=5 MPa, strictly positive.
    assert result.meets_screen
    assert result.zones[0].qualified_barriers == ("primary", "secondary")
    assert result.zones[0].pressure_margins_mpa == (5, 5)
    assert result.zones[0].criteria.min_plug_length_md_m == 100
    assert result.zones[0].criteria.min_above_zone_md_m == 50
    assert result.citations


@pytest.mark.parametrize("change, reason", [
    ({"base_md_m": 1899}, "plug_length"),
    ({"top_md_m": 1960, "base_md_m": 2060}, "above_zone"),
    ({"formation_integrity_at_base_mpa": 30}, "formation_strength"),
    ({"formation_integrity_at_base_mpa": 29}, "formation_strength"),
    ({"rock_to_rock_verified": False}, "rock_to_rock"),
    ({"placement_verified": False}, "placement"),
])
def test_defective_barrier_is_not_counted(change, reason):
    result = screen((barrier(**change),))
    assert not result.meets_screen
    assert reason in result.zones[0].rejections[0][1]
    assert "barrier_count" in result.zones[0].flags


def test_exact_length_and_position_boundaries():
    assert screen((barrier(top_md_m=1950, base_md_m=2050),),
                  ).zones[0].qualified_barriers == ("primary",)


def test_duplicate_independence_group_and_overlapping_plugs_do_not_count_twice():
    a = barrier()
    for b in (barrier("b", top_md_m=1600, base_md_m=1700,
                      independence_group="primary"),
              barrier("b", top_md_m=1850, base_md_m=1950)):
        assert not screen((a, b)).meets_screen
        assert "independence_not_established" in screen((a, b)).zones[0].flags


@pytest.mark.parametrize("gas, scp, flag", [
    (True, False, "gas_migration"), (False, True, "sustained_casing_pressure"),
    (None, False, "gas_migration_unknown"), (False, None, "casing_pressure_unknown"),
])
def test_anomaly_or_missing_observation_holds_screen(gas, scp, flag):
    result = screen_permanent_barriers(
        (FlowZone("gas", 2000, criteria()),),
        (barrier(), barrier("b", top_md_m=1600, base_md_m=1700)),
        gas_migration=gas, sustained_casing_pressure=scp,
    )
    assert not result.meets_screen
    assert flag in result.flags


def test_count_is_per_zone_and_input_criteria_are_respected():
    zones = (FlowZone("gas", 2000, criteria()),
             FlowZone("water", 1500, criteria(required_barriers=1)))
    result = screen_permanent_barriers(zones, (barrier(),),
                                      gas_migration=False, sustained_casing_pressure=False)
    assert [len(z.qualified_barriers) for z in result.zones] == [1, 0]
    assert not result.meets_screen


@pytest.mark.parametrize("change", [
    {"base_md_m": 1800}, {"top_md_m": -1},
    {"formation_integrity_at_base_mpa": float("nan")},
    {"maximum_pressure_below_at_base_mpa": float("inf")},
    {"placement_verified": "yes"}, {"evidence": ""}, {"zone_id": "missing"},
])
def test_invalid_inputs_fail_closed(change):
    with pytest.raises(ValueError):
        screen((barrier(**change),))


@pytest.mark.parametrize("count", [0, -1, 1.5, True])
def test_count_must_be_positive_integer(count):
    with pytest.raises(ValueError):
        screen_permanent_barriers((FlowZone("gas", 2000, criteria(required_barriers=count)),),
                                  (), gas_migration=False, sustained_casing_pressure=False)


def test_empty_or_duplicate_zone_and_barrier_ids_rejected():
    with pytest.raises(ValueError):
        screen_permanent_barriers((), (), gas_migration=False, sustained_casing_pressure=False)
    with pytest.raises(ValueError):
        screen((barrier(), barrier()))


@pytest.mark.parametrize("inflow, count", [
    ("mobile_hydrocarbon", 2), ("overpressured_flow_to_surface", 2),
    ("normal_water_no_surface_flow", 1), ("crossflow_only", 1),
])
@pytest.mark.parametrize("setting, length", [
    ("open_hole", 100), ("cased_without_foundation", 100),
    ("cased_on_verified_foundation", 50),
])
def test_public_historical_criteria_select_count_and_length(inflow, count, setting, length):
    c = public_norsok_2013_criteria(inflow, setting, min_above_zone_md_m=50)
    assert c.required_barriers == count
    assert c.min_plug_length_md_m == length
    assert "2013" in c.basis


def test_unsupported_inflow_or_setting_and_open_hole_coverage_rejected():
    for inflow, setting, coverage in (("unknown", "open_hole", 50),
                                      ("mobile_hydrocarbon", "shoe_transition", 50),
                                      ("mobile_hydrocarbon", "open_hole", 49)):
        with pytest.raises(ValueError):
            public_norsok_2013_criteria(inflow, setting, min_above_zone_md_m=coverage)
