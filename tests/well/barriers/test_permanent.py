"""Permanent-barrier screening against explicit, synthetic design inputs."""

from dataclasses import replace

import pytest

from digitalmodel.well.barriers import (
    BarrierCriteria, FlowZone, PermanentBarrier, screen_permanent_barriers,
)


def criteria(**changes):
    return replace(BarrierCriteria(2, 120, 80, "Synthetic screening inputs"), **changes)


def barrier(name="primary", **changes):
    return replace(PermanentBarrier(
        name, "gas", 1800, 1920, 35, 30, True, True, name,
        "Synthetic placement, cross-section and formation records",
    ), **changes)


def screen(plugs=None, **changes):
    return screen_permanent_barriers(
        (FlowZone("gas", 2000, criteria()),),
        plugs if plugs is not None else (
            barrier(), barrier("secondary", top_md_m=1600, base_md_m=1720),
        ), gas_migration=False, sustained_casing_pressure=False, **changes,
    )


def test_two_verified_envelopes_hand_example():
    result = screen()
    # Lengths: 1920-1800=120 m and 1720-1600=120 m.
    # Cemented length above inflow: 120 m in each plug, exceeding 80 m.
    # Base-pressure margins: 35-30=5 MPa, strictly positive.
    assert result.meets_screen
    assert result.zones[0].qualified_barriers == ("primary", "secondary")
    assert result.zones[0].pressure_margins_mpa == (5, 5)
    assert result.zones[0].criteria.min_plug_length_md_m == 120
    assert result.zones[0].criteria.min_above_zone_md_m == 80
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
    assert screen((barrier(top_md_m=1920, base_md_m=2040),),
                  ).zones[0].qualified_barriers == ("primary",)


def test_duplicate_independence_group_and_overlapping_plugs_do_not_count_twice():
    a = barrier()
    for b in (barrier("b", top_md_m=1600, base_md_m=1720,
                      independence_group="primary"),
              barrier("b", top_md_m=1850, base_md_m=1970)):
        assert not screen((a, b)).meets_screen
        assert "independence_not_established" in screen((a, b)).zones[0].flags


@pytest.mark.parametrize("gas, scp, flag", [
    (True, False, "gas_migration"), (False, True, "sustained_casing_pressure"),
    (None, False, "gas_migration_unknown"), (False, None, "casing_pressure_unknown"),
])
def test_anomaly_or_missing_observation_holds_screen(gas, scp, flag):
    result = screen_permanent_barriers(
        (FlowZone("gas", 2000, criteria()),),
        (barrier(), barrier("b", top_md_m=1600, base_md_m=1720)),
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


def test_criteria_have_no_builtin_standard_selector():
    from digitalmodel.well import barriers
    from digitalmodel.well.barriers import permanent

    assert not hasattr(barriers, "public_norsok_2013_criteria")
    assert not hasattr(permanent, "public_norsok_2013_criteria")


@pytest.mark.parametrize("top, base, expected", [
    (1800, 1860, False),  # The uncemented gap contributes no coverage.
    (1800, 1890, True),   # Exact cemented-length boundary wholly above inflow.
    (1910, 2030, True),   # Exact boundary for a plug straddling inflow.
    (1911, 2031, False),
    (2010, 2130, False),  # No cement above inflow.
])
def test_above_zone_counts_only_cemented_length(top, base, expected):
    result = screen_permanent_barriers(
        (FlowZone("gas", 2000, criteria(required_barriers=1,
            min_plug_length_md_m=60, min_above_zone_md_m=90)),),
        (barrier(top_md_m=top, base_md_m=base),),
        gas_migration=False, sustained_casing_pressure=False,
    )
    assert result.meets_screen is expected
    if not expected:
        assert "above_zone" in result.zones[0].rejections[0][1]
