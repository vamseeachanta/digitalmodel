"""Illustrative example inputs; expectations use independent hand formulas."""

from dataclasses import FrozenInstanceError
from decimal import Decimal
import math

import numpy as np
import pytest

from digitalmodel.naval_architecture.resistance_trim_comparison import (
    RelativeChangeInput,
    ResistanceComponents,
    TrimCondition,
    component_split_consistent,
    effective_power_kw,
    relative_change,
    tabulate_trim,
)
from digitalmodel.naval_architecture.trim_hydrostatics import end_drafts


def example_condition(trim_m=-0.4, speed_m_s=8.0):
    """Illustrative example: forces are kN, trim is forward minus aft draft."""
    return TrimCondition(trim_m, speed_m_s, ResistanceComponents(420, 260, 160))


def example_reference():
    """Illustrative example with a separately specified reference split."""
    return TrimCondition(0.2, 8.0, ResistanceComponents(525, 325, 200))


@pytest.mark.parametrize("unit", ["kN", "kW", "N", "W", "Pa", "m/s", "custom-unit"])
@pytest.mark.parametrize("value,reference", [(420, 525), (525, 420), (0, 525)])
def test_relative_change_hand_formula(unit, value, reference):
    expected = float((Decimal(value) - Decimal(reference)) / Decimal(reference))
    actual = relative_change(RelativeChangeInput(value, reference, unit))
    assert actual == pytest.approx(expected)
    assert actual * 100 == pytest.approx(expected * 100)


def test_relative_change_zero_reference():
    with pytest.raises(ValueError, match="nonzero"):
        relative_change(RelativeChangeInput(420, 0, "kN"))


@pytest.mark.parametrize("unit", ["", None, 1, True, []])
def test_relative_change_rejects_empty_or_nonstring_unit(unit):
    with pytest.raises(ValueError, match="unit"):
        RelativeChangeInput(420, 525, unit)


def test_component_split_uses_absolute_tolerance_only():
    assert component_split_consistent(ResistanceComponents(420, 260, 160),
                                      absolute_tolerance_kn=0)
    rounded = ResistanceComponents(420.125, 260, 160)
    assert component_split_consistent(rounded, absolute_tolerance_kn=0.125)
    assert not component_split_consistent(rounded, absolute_tolerance_kn=0.124)
    assert not component_split_consistent(
        ResistanceComponents(420_000_001, 260_000_000, 160_000_000),
        absolute_tolerance_kn=0,
    )


@pytest.mark.parametrize("force,speed", [(420, 8), (0, 8), (420, 0)])
def test_power_unit_identity(force, speed):
    # kN * m/s = kW; the Decimal product is independent of the public helper.
    expected = Decimal(force) * Decimal(speed)
    assert effective_power_kw(force, speed) == pytest.approx(float(expected))


def test_table_negative_trim_and_selected_reference():
    conditions = [example_condition(), example_reference()]
    table = tabulate_trim(iter(conditions), reference_index=1,
                          absolute_tolerance_kn=0)
    assert isinstance(table, tuple)
    assert [r.condition for r in table] == conditions
    row, reference = table
    assert row.condition.trim_m == -0.4
    for name, value, baseline in [("total", 420, 525), ("viscous", 260, 325),
                                  ("pressure", 160, 200)]:
        assert getattr(row, name + "_delta_kn") == value - baseline
        assert getattr(row, name + "_change_fraction") == pytest.approx(
            float((Decimal(value) - Decimal(baseline)) / Decimal(baseline))
        )
        assert getattr(reference, name + "_delta_kn") == 0
        assert getattr(reference, name + "_change_fraction") == 0
    assert row.effective_power_kw == 420 * 8
    assert row.power_delta_kw == (420 - 525) * 8
    assert row.power_change_fraction == pytest.approx((420 - 525) / 525)
    assert reference.power_delta_kw == 0
    assert reference.power_change_fraction == 0


def test_by_stern_trim_matches_hydrostatics_end_drafts():
    draft_fwd, draft_aft = end_drafts(draft_m=5.0, trim_m=-0.4)
    assert draft_aft > draft_fwd
    condition = example_condition(trim_m=draft_fwd - draft_aft)
    (row,) = tabulate_trim([condition], absolute_tolerance_kn=0)
    assert row.condition.trim_m == pytest.approx(-0.4)
    assert end_drafts(5.0, row.condition.trim_m) == pytest.approx(
        (draft_fwd, draft_aft)
    )


@pytest.mark.parametrize("scalar", [np.int64, np.float32, np.float64])
def test_numpy_real_scalars_in_comparison(scalar):
    condition = TrimCondition(
        scalar(-1), scalar(8),
        ResistanceComponents(scalar(420), scalar(260), scalar(160)),
    )
    table = tabulate_trim([example_reference(), condition],
                          absolute_tolerance_kn=scalar(0))
    assert table[1].condition.trim_m == -1
    assert table[1].effective_power_kw == pytest.approx(3360)
    assert table[1].total_delta_kn == pytest.approx(-105)
    assert table[1].total_change_fraction == pytest.approx(-0.2)
    assert relative_change(RelativeChangeInput(scalar(420), scalar(525), "kN")) == (
        pytest.approx(-0.2)
    )


@pytest.mark.parametrize("component", [np.int64(2**62), np.float32(2e38)])
def test_numpy_component_sum_uses_finite_float_arithmetic(component):
    total = 2 * float(component)
    forces = ResistanceComponents(total, component, component)
    assert component_split_consistent(forces, absolute_tolerance_kn=0)


def test_decreasing_unsigned_forces_have_negative_deltas():
    reference = TrimCondition(
        0.2, 8, ResistanceComponents(*map(np.uint16, (525, 325, 200))),
    )
    condition = TrimCondition(
        -0.4, 8, ResistanceComponents(*map(np.uint16, (420, 260, 160))),
    )
    _, row = tabulate_trim([reference, condition], absolute_tolerance_kn=0)
    assert (row.total_delta_kn, row.viscous_delta_kn, row.pressure_delta_kn) == (
        -105, -65, -40,
    )
    assert row.total_change_fraction == pytest.approx(-0.2)
    assert row.effective_power_kw == 3360
    assert row.power_delta_kw == -840


def test_float32_total_does_not_erase_split_residual():
    forces = ResistanceComponents(np.float32(420), 260, 160.000001)
    assert not component_split_consistent(forces, absolute_tolerance_kn=0)
    condition = TrimCondition(-0.4, 8, forces)
    with pytest.raises(ValueError, match="split"):
        tabulate_trim([condition], absolute_tolerance_kn=0)


def test_single_row_is_its_own_reference():
    condition = example_condition()
    (row,) = tabulate_trim([condition], absolute_tolerance_kn=0)
    assert row.condition == condition
    assert row.total_delta_kn == row.viscous_delta_kn == row.pressure_delta_kn == 0
    assert row.total_change_fraction == row.viscous_change_fraction == 0
    assert row.pressure_change_fraction == row.power_change_fraction == 0
    assert row.power_delta_kw == 0
    assert row.effective_power_kw == 420 * 8


def test_table_can_represent_zero_nonreference_resistance():
    zero = TrimCondition(-0.8, 8, ResistanceComponents(0, 0, 0))
    table = tabulate_trim([example_reference(), zero], absolute_tolerance_kn=0)
    assert table[1].effective_power_kw == 0
    assert table[1].total_delta_kn == -525
    assert table[1].total_change_fraction == -1
    assert table[1].viscous_change_fraction == -1
    assert table[1].pressure_change_fraction == -1
    assert table[1].power_change_fraction == -1


def test_rounded_split_requires_explicit_caller_tolerance():
    rounded = TrimCondition(-0.4, 8, ResistanceComponents(420.125, 260, 160))
    (row,) = tabulate_trim([rounded], absolute_tolerance_kn=0.125)
    assert row.total_delta_kn == 0
    with pytest.raises(ValueError, match="split"):
        tabulate_trim([rounded], absolute_tolerance_kn=0.124)


def test_common_scaling_preserves_fraction_and_scales_power():
    original = example_condition()
    scaled = TrimCondition(original.trim_m, original.speed_m_s,
                           ResistanceComponents(840, 520, 320))
    table = tabulate_trim([original, scaled], absolute_tolerance_kn=0)
    assert table[1].effective_power_kw == 2 * table[0].effective_power_kw
    assert table[1].effective_power_kw == 6720  # 840 kN * 8 m/s = 6720 kW.
    assert table[1].total_change_fraction == 1
    assert relative_change(RelativeChangeInput(840, 1050, "kN")) == pytest.approx(
        float((Decimal(420) - Decimal(525)) / Decimal(525))
    )


def test_signed_scalar_changes_retain_reference_sign():
    assert relative_change(RelativeChangeInput(-420, -525, "kW")) == pytest.approx(
        float((Decimal(-420) - Decimal(-525)) / Decimal(-525))
    )


@pytest.mark.parametrize("index", [-1, 1, True, 0.5])
def test_invalid_reference_index(index):
    with pytest.raises(ValueError, match="reference_index"):
        tabulate_trim([example_condition()], reference_index=index,
                      absolute_tolerance_kn=0)


def test_table_rejects_empty_inconsistent_and_different_speed():
    bad_split = TrimCondition(0.2, 8, ResistanceComponents(425, 260, 160))
    for rows in [[], [bad_split], [example_condition(), example_condition(0.2, 9)]]:
        with pytest.raises(ValueError):
            tabulate_trim(rows, absolute_tolerance_kn=0)


@pytest.mark.parametrize("reference_speed", [8.0, np.float32(8)])
def test_table_rejects_nearly_equal_speeds(reference_speed):
    conditions = [example_condition(speed_m_s=reference_speed),
                  example_condition(speed_m_s=8.000000001)]
    with pytest.raises(ValueError, match="same speed_m_s"):
        tabulate_trim(conditions, absolute_tolerance_kn=0)


@pytest.mark.parametrize("forces,speed", [((0, 0, 0), 8), ((420, 420, 0), 8),
                                         ((420, 0, 420), 8), ((420, 260, 160), 0)])
def test_table_rejects_zero_reference_quantities(forces, speed):
    condition = TrimCondition(-0.4, speed, ResistanceComponents(*forces))
    with pytest.raises(ValueError, match="nonzero"):
        tabulate_trim([condition], absolute_tolerance_kn=0)


@pytest.mark.parametrize("bad", [None, True, "8", math.nan, math.inf, -math.inf])
def test_finite_numeric_boundaries(bad):
    for make in [lambda: RelativeChangeInput(bad, 525, "kN"),
                 lambda: RelativeChangeInput(420, bad, "kW"),
                 lambda: ResistanceComponents(420, bad, 160),
                 lambda: ResistanceComponents(bad, 260, 160),
                 lambda: ResistanceComponents(420, 260, bad),
                 lambda: example_condition(bad),
                 lambda: example_condition(speed_m_s=bad),
                 lambda: effective_power_kw(bad, 8),
                 lambda: effective_power_kw(420, bad),
                 lambda: component_split_consistent(ResistanceComponents(420, 260, 160),
                                                    absolute_tolerance_kn=bad)]:
        with pytest.raises(ValueError):
            make()


def test_negative_magnitudes_and_tolerance_rejected():
    for make in [lambda: ResistanceComponents(-1, 0, 0),
                 lambda: ResistanceComponents(0, -1, 0),
                 lambda: ResistanceComponents(0, 0, -1),
                 lambda: example_condition(speed_m_s=-1),
                 lambda: effective_power_kw(-1, 8),
                 lambda: effective_power_kw(420, -1),
                 lambda: component_split_consistent(ResistanceComponents(420, 260, 160),
                                                    absolute_tolerance_kn=-1)]:
        with pytest.raises(ValueError):
            make()


def test_nonfinite_results_rejected():
    with pytest.raises(ValueError):
        relative_change(RelativeChangeInput(1e308, 1e-308, "kN"))
    with pytest.raises(ValueError):
        effective_power_kw(1e308, 8)
    with pytest.raises(ValueError):
        component_split_consistent(ResistanceComponents(1e308, 1e308, 1e308),
                                   absolute_tolerance_kn=0)


def test_all_records_are_frozen():
    condition = example_condition()
    (row,) = tabulate_trim([condition], absolute_tolerance_kn=0)
    for record, field in [(condition, "trim_m"), (condition.resistance, "total_kn"),
                          (RelativeChangeInput(420, 525, "kN"), "value"),
                          (row, "total_delta_kn")]:
        with pytest.raises(FrozenInstanceError):
            setattr(record, field, 0)
