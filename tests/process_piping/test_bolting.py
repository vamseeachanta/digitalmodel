"""Independent bolt-load and assembly checks; input values are illustrative."""

import math
import pytest

from digitalmodel.process_piping.bolting import (
    BoltUp,
    check_bolt_up,
    tightening_passes,
    joint_leak_checks,
)


def joint(**changes):
    # NPS6 Class300: twelve 3/4-in bolts (B16.5; API International chart).
    # Illustrative 3/4-10 UNC tensile area .3345in² = 215.80602mm².
    values = dict(
        facing="rf_spiral_wound",
        bolt_count=12,
        bolt_area_m2=0.00021580602,
        bolt_diameter_m=0.01905,
        target_bolt_stress_pa=300e6,
        maximum_bolt_stress_pa=450e6,
        minimum_bolt_stress_pa=100e6,
        nut_factor=0.18,
        gasket_contact_area_m2=0.008,
        gasket_seating_min_pa=80e6,
        gasket_operating_min_pa=30e6,
        gasket_max_pa=200e6,
        retained_load_fraction=0.8,
        flange_load_limit_n=1.5e6,
        pressure_pa=4e6,
        gasket_inside_diameter_m=0.17,
        effective_gasket_width_m=0.006,
        gasket_reaction_diameter_m=0.19,
        gasket_m=3,
        gasket_y_pa=69e6,
    )
    return BoltUp(**(values | changes))


def test_class300_illustrative_loads():
    # Twelve 215.80602 mm² tensile areas at 300 MPa: F=776901.672 N.
    # Per bolt 64741.806 N; T=.18*64741.806*.01905=221.999652774 N m.
    # Contact stress=776901.672/.008=97.112709 MPa.
    result = check_bolt_up(joint())
    assert result.assembly_load_n == pytest.approx(776901.672)
    assert result.torque_per_bolt_nm == pytest.approx(221.999652774)
    assert result.assembly_gasket_stress_pa == pytest.approx(97.112709e6)
    assert result.seating_design_load_n == pytest.approx(math.pi * 0.006 * 0.19 * 69e6)
    assert result.operating_design_load_n == pytest.approx(
        math.pi * 0.19**2 / 4 * 4e6 + 2 * math.pi * 0.006 * 0.19 * 3 * 4e6
    )
    assert all(result.checks.values())


def test_rtj_and_failures():
    assert all(check_bolt_up(joint(facing="rtj")).checks.values())
    assert not check_bolt_up(joint(target_bolt_stress_pa=500e6)).checks["bolt_maximum"]
    assert not check_bolt_up(joint(flange_load_limit_n=0.7e6)).checks["flange_load"]
    assert not check_bolt_up(joint(gasket_max_pa=90e6)).checks["gasket_maximum"]
    assert not check_bolt_up(joint(retained_load_fraction=0.1)).checks[
        "operating_gasket"
    ]
    assert not check_bolt_up(joint(gasket_y_pa=400e6)).checks["appendix2_seating"]


@pytest.mark.parametrize(
    "changes",
    [
        dict(facing="rf"),
        dict(bolt_count=3.5),
        dict(bolt_area_m2=0),
        dict(nut_factor=math.nan),
        dict(retained_load_fraction=0),
        dict(retained_load_fraction=1.1),
        dict(gasket_m=-1),
        dict(gasket_seating_min_pa=210e6),
        dict(bolt_count=True),
    ],
)
def test_invalid(changes):
    with pytest.raises(ValueError):
        check_bolt_up(joint(**changes))


def test_cross_passes():
    passes = tightening_passes(8)
    assert [p.target_fraction for p in passes] == [0, 0.3, 0.6, 1, 1]
    assert passes[1].bolt_order == (1, 5, 3, 7, 2, 6, 4, 8)
    assert passes[-1].bolt_order == tuple(range(1, 9))
    assert passes[-1].repeat_until_stable
    assert len(joint_leak_checks()) >= 5
    assert tightening_passes(12)[1].bolt_order == (
        1,
        7,
        4,
        10,
        2,
        8,
        5,
        11,
        3,
        9,
        6,
        12,
    )
    for count in (4, 8, 12, 16, 32):
        assert sorted(tightening_passes(count)[1].bolt_order) == list(
            range(1, count + 1)
        )
    with pytest.raises(ValueError):
        tightening_passes(6)
