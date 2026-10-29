# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent arithmetic for D8/D9 in the mining-lift screening basis.

All inputs are manufactured verification quantities, not design data.
"""

from math import pi

import pytest

from digitalmodel.subsea.mining_riser import (
    BundleSection,
    Tube,
    axial_modes,
    free_hanging_screen,
    string_weight_inventory,
    wave_period_screen,
)


def bundle(**overrides):
    """D1: OD=2, wall=.5 gives wall area=3*pi/4 and bore area=pi/4."""
    values = dict(
        length_m=10.0,
        tubes=(Tube(2.0, 0.5, 8.0, 800.0, 2.0, 0.0, True),),
        external_density_kg_m3=1.0,
        additional_mass_kg_m=pi,
        additional_displaced_area_m2=pi,
        axial_added_mass_kg_m=100.0,
    )
    return BundleSection(**(values | overrides))


def test_weight_components_and_top_tension_hand_balance():
    # D8: dry=(8*3*pi/4+pi)*10*10=700*pi N;
    # contents=2*pi/4*10*10=50*pi; buoyancy=1*(pi+pi)*10*10=200*pi.
    # Inertial added mass is excluded; contents remain despite zero coupling.
    sections = [bundle()]
    result = string_weight_inventory(sections, gravity_m_s2=10.0)
    assert result.dry_weight_n == pytest.approx(700 * pi)
    assert result.contents_weight_n == pytest.approx(50 * pi)
    assert result.filled_weight_n == pytest.approx(750 * pi)
    assert result.buoyancy_n == pytest.approx(200 * pi)
    assert result.submerged_weight_n == pytest.approx(550 * pi)
    static = free_hanging_screen(sections, gravity_m_s2=10.0, lower_end_force_n=20.0)
    assert static.effective_tension_n[0] == pytest.approx(550 * pi + 20)


def test_inventory_includes_non_load_carrying_auxiliary_tube():
    # Auxiliary OD=1, wall=.25: wall=3*pi/16, bore=pi/16, displacement=pi/4.
    # With the main tube and attachments: dry=(6*pi+1.5*pi+pi)*100=850*pi;
    # contents=(.5*pi+.125*pi)*100=62.5*pi; buoyancy=(pi+.25*pi+pi)*100=225*pi.
    main = bundle().tubes[0]
    auxiliary = Tube(
        outer_diameter_m=1.0,
        wall_thickness_m=0.25,
        material_density_kg_m3=8.0,
        youngs_modulus_pa=800.0,
        contents_density_kg_m3=2.0,
        contents_inertia_fraction=0.0,
        carries_axial_load=False,
    )
    section = bundle(tubes=(main, auxiliary))
    result = string_weight_inventory([section], gravity_m_s2=10.0)
    assert result.dry_weight_n == pytest.approx(850 * pi)
    assert result.contents_weight_n == pytest.approx(62.5 * pi)
    assert result.filled_weight_n == pytest.approx(912.5 * pi)
    assert result.buoyancy_n == pytest.approx(225 * pi)
    assert result.submerged_weight_n == pytest.approx(687.5 * pi)
    assert main.mass_kg_m == pytest.approx(6.5 * pi)
    assert auxiliary.mass_kg_m == pytest.approx(1.625 * pi)
    assert result.filled_weight_n == pytest.approx(
        (main.mass_kg_m + auxiliary.mass_kg_m + section.additional_mass_kg_m)
        * section.length_m
        * 10.0
    )
    assert section.axial_stiffness_n == pytest.approx(600 * pi)
    static = free_hanging_screen([section], gravity_m_s2=10.0, lower_end_force_n=20.0)
    assert static.effective_tension_n[0] == pytest.approx(687.5 * pi + 20)


def test_inventory_retains_negative_weight_and_multiple_sections():
    # First section net=550*pi; second: 750*pi-1000*pi=-250*pi.
    result = string_weight_inventory(
        [bundle(), bundle(external_density_kg_m3=5.0)], gravity_m_s2=10.0
    )
    assert result.submerged_weight_n == pytest.approx(300 * pi)
    buoyant = string_weight_inventory(
        [bundle(external_density_kg_m3=5.0)], gravity_m_s2=10.0
    )
    assert buoyant.submerged_weight_n == pytest.approx(-250 * pi)


@pytest.mark.parametrize("gravity", [0, -10, True, "10", float("nan"), float("inf")])
def test_invalid_inventory_gravity(gravity):
    with pytest.raises(ValueError):
        string_weight_inventory([bundle()], gravity_m_s2=gravity)


@pytest.mark.parametrize("sections", [None, [], [object()]])
def test_invalid_inventory_sections(sections):
    with pytest.raises(ValueError):
        string_weight_inventory(sections, gravity_m_s2=10.0)


def test_inventory_overflow_rejected():
    with pytest.raises(ValueError, match="finite"):
        string_weight_inventory([bundle(length_m=1e308)], gravity_m_s2=10.0)


@pytest.mark.parametrize(
    "bounds,within",
    [
        ((4.0, 6.0), True),
        ((5.0, 6.0), True),
        ((4.0, 5.0), True),
        ((5.0, 5.0), True),
        ((1.0, 4.0), False),
        ((6.0, 8.0), False),
    ],
)
def test_wave_period_hand_reciprocal_and_inclusive_bounds(bounds, within):
    # R5/D9: T=1/f=1/0.2=5 s; Hz must not be compared directly with seconds.
    result = wave_period_screen(
        first_axial_frequency_hz=0.2,
        wave_period_min_s=bounds[0],
        wave_period_max_s=bounds[1],
    )
    assert result.first_axial_period_s == 5.0
    assert result.within_wave_period_range is within


@pytest.mark.parametrize(
    "field",
    [
        "first_axial_frequency_hz",
        "wave_period_min_s",
        "wave_period_max_s",
    ],
)
@pytest.mark.parametrize("invalid", [0, -1, True, "1", float("nan"), float("inf")])
def test_wave_period_invalid_inputs(field, invalid):
    values = dict(
        first_axial_frequency_hz=0.2, wave_period_min_s=4.0, wave_period_max_s=6.0
    )
    values[field] = invalid
    with pytest.raises(ValueError):
        wave_period_screen(**values)


def test_wave_period_reversed_bounds_and_overflow():
    with pytest.raises(ValueError):
        wave_period_screen(
            first_axial_frequency_hz=0.2, wave_period_min_s=6.0, wave_period_max_s=4.0
        )
    with pytest.raises(ValueError, match="finite"):
        wave_period_screen(
            first_axial_frequency_hz=5e-324,
            wave_period_min_s=4.0,
            wave_period_max_s=6.0,
        )


def test_wave_screen_consumes_first_mode_with_lumped_bottom_mass():
    modes = axial_modes(
        [bundle()],
        gravity_m_s2=10.0,
        lower_end_force_n=20.0,
        lower_end_mass_kg=100.0,
        elements_per_section=16,
        mode_count=1,
    )
    result = wave_period_screen(
        first_axial_frequency_hz=modes.frequencies_hz[0],
        wave_period_min_s=1.0,
        wave_period_max_s=100.0,
    )
    assert result.first_axial_period_s == pytest.approx(modes.periods_s[0])
    assert result.within_wave_period_range
