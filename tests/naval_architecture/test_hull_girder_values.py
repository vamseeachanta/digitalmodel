# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Hand examples for lumped section geometry and explicitly supplied capacities.

Source S20: *Principles of Ship Performance*, Summer 2020 edition, section 1.7.7,
Appendix C (rectangle inertia), sections 6.2.3 (sigma=M y/I), 5.3 (elasticity).
Source E: ``HullGirderElement.stress`` documented load-shortening model; this
is a software-model check, not qualification against a classification rule.
Every material parameter, load, safety factor and capacity is a synthetic input.
Unopened classification-rule factors, plate buckling coefficients and wave-rule
loads remain NOT_EVALUATED. The tests do not reproduce licensed rule values.
Relative tolerance 1e-10 is for floating-point arithmetic, except for the
explicitly bounded midpoint section discretisation.
"""

import pytest

from digitalmodel.naval_architecture import hull_girder_strength as strength
from digitalmodel.naval_architecture.hull_girder_smith import (
    HullGirderElement,
    hard_corner_element,
    plate_element,
    section_centroid_m,
    section_modulus_m3,
)
from digitalmodel.structural.structural_analysis.models import (
    MaterialProperties,
    PlateGeometry,
)


def test_plate_element_area_and_elastic_stress_values():
    """S20 sec.5.3: sigma=E epsilon; E=200000 MPa, epsilon=+/-0.0001
    => sigma=+/-20 MPa. Plate area=1000*10 mm2=0.01 m2, z=3 m.

    All material properties are stipulated, not a named steel grade. Compression
    is probed in the elastic region only; the delegated buckling capacity is
    NOT_EVALUATED without its rule source. The factory must preserve input E.
    """
    material = MaterialProperties(300.0, 450.0, 200000.0, 0.25, 7800.0)
    element = plate_element(PlateGeometry(2000.0, 1000.0, 10.0), material, 3.0)
    assert element.area_m2 == pytest.approx(0.01, rel=1e-10)
    assert element.z_m == pytest.approx(3.0, rel=1e-10)
    assert element.youngs_modulus_mpa == pytest.approx(200000.0, rel=1e-10)
    assert element.stress(0.0001) == pytest.approx(20.0, rel=1e-10)
    assert element.stress(-0.0001) == pytest.approx(-20.0, rel=1e-10)


@pytest.mark.parametrize(
    "strain, expected",
    [(0.0005, 100.0), (-0.0005, -100.0), (0.003, 300.0), (-0.003, -300.0)],
)
def test_hard_corner_element_stress_values(strain, expected):
    """Source E: non-buckling element is elastic-perfectly-plastic.

    Stipulate E=200000 MPa and fy=300 MPa: strain magnitude .0005
    gives 100 MPa, magnitude .003 gives 600 MPa elastically but caps at 300 MPa.
    """
    element = hard_corner_element(0.02, 4.0, 300.0, youngs_modulus_mpa=200000.0)
    assert element.area_m2 == pytest.approx(0.02, rel=1e-10)
    assert element.z_m == pytest.approx(4.0, rel=1e-10)
    assert not element.buckles
    assert element.compressive_capacity_mpa == pytest.approx(300.0, rel=1e-10)
    assert element.stress(strain) == pytest.approx(expected, rel=1e-10)


def test_plate_model_post_buckling_hand_point():
    """Source E: sigma=-cap*(r+(1-r)*eps_u/abs(eps)).

    Stipulate cap=150 MPa, E=200000 MPa, fy=300 MPa, residual r=.25.
    eps_u=150/200000=.00075; at eps=-.0015, eps_u/abs(eps)=.5;
    sigma=-150*(.25+.75*.5)=-93.75 MPa. This curve is the stated
    empirical model; no independent physical post-buckling validation is implied.
    """
    element = HullGirderElement(
        0.01,
        3.0,
        300.0,
        150.0,
        youngs_modulus_mpa=200000.0,
        post_buckling_residual=0.25,
    )
    assert element.buckles
    assert element.compressive_capacity_mpa == pytest.approx(150.0, rel=1e-10)
    assert element.stress(-0.0015) == pytest.approx(-93.75, rel=1e-10)


def test_section_modulus_rectangular_box_value():
    """S20 Appendix C and sec.6.2.3: I=b h^3/12 minus the inner rectangle.

    Outer b=4,h=6 m; thickness=.2 m gives inner b=3.6,h=5.6 m.
    I=(4*216-3.6*175.616)/12=19.3152 m4; c=3 m; Z=6.4384 m3.
    Each of the four walls is split into 1000 equal midpoint strips.
    The source API neglects strip own-inertia: missing I<0.000006 m4;
    c is short by .0001 m. The relative Z error is bounded by .00004
    (0.004%), including both discretisation effects, not material uncertainty.
    """
    elements = []
    strips = 1000  # synthetic mesh, error bound derived above
    for i in range(strips):
        bottom_z = (i + 0.5) * 0.2 / strips
        side_z = 0.2 + (i + 0.5) * 5.6 / strips
        for area, z in (
            (0.8 / strips, bottom_z),
            (0.8 / strips, 6.0 - bottom_z),
            (1.12 / strips, side_z),
            (1.12 / strips, side_z),
        ):
            elements.append(
                hard_corner_element(area, z, 300.0, youngs_modulus_mpa=200000.0)
            )
    assert section_centroid_m(elements) == pytest.approx(3.0, rel=1e-10)
    assert section_modulus_m3(elements) == pytest.approx(6.4384, rel=4e-5)


@pytest.mark.parametrize(
    "wave_hog, wave_sag, expected_hog, expected_sag",
    [
        (40.0, -60.0, 50.0, -80.0),
        (100.0, -80.0, 110.0, -100.0),
    ],
)
def test_wave_bending_holder_signed_combination_values(
    wave_hog, wave_sag, expected_hog, expected_sag
):
    """Signed superposition (S20 sec.6.2): supplied still-water moments +10/-20
    plus supplied wave +40/-60 => +50/-80; +100/-80 => +110/-100 kN.m.

    The holder has no utilisation or capacity. Its wave coefficient is an
    arbitrary supplied placeholder; rule-generated wave loads are NOT_EVALUATED.
    """
    wave = strength.WaveBendingMoment(wave_hog, wave_sag, wave_coefficient=1.0)
    result = strength.combined_bending_moment(10.0, -20.0, wave)
    assert (result.hogging_kn_m, result.sagging_kn_m) == pytest.approx(
        (expected_hog, expected_sag), rel=1e-10
    )


@pytest.mark.parametrize(
    "positive, negative, expected", [(40.0, -60.0, 100.0), (100.0, -80.0, 180.0)]
)
def test_wave_shear_holder_signed_range_values(positive, negative, expected):
    """Supplied shear extremes +40/-60 span 100 kN; +100/-80 span 180 kN.

    WaveShearForce is a data holder with no utilisation or pass/fail property.
    This checks preservation of signed inputs only, not the wave-rule formula.
    Distribution factors and wave coefficient are stipulated placeholders.
    """
    result = strength.WaveShearForce(
        positive, negative, f1=1.0, f2=1.0, wave_coefficient=1.0
    )
    assert isinstance(result, strength.WaveShearForce)
    assert result.positive_kn == pytest.approx(positive, rel=1e-10)
    assert result.negative_kn == pytest.approx(negative, rel=1e-10)
    assert result.positive_kn - result.negative_kn == pytest.approx(expected, rel=1e-10)


@pytest.mark.parametrize(
    "moment, stress, ratio, passes",
    [
        (50000.0, 50.0, 0.5, True),
        (-100000.0, 100.0, 1.0, True),
        (101000.0, 101.0, 1.01, False),
    ],
)
def test_section_modulus_check_supplied_capacity_values(
    monkeypatch, moment, stress, ratio, passes
):
    """S20 sec.6.2.3: sigma=|M|/Z, converting kN.m/m3 to MPa by /1000.

    Z=1 m3: 50000,100000,101000 kN.m give 50,100,101 MPa.
    Inject a synthetic permissible stress of 100 MPa (not a rule value):
    demand/capacity=.5,1,1.01; equality passes, an excess fails.
    Classification-rule material factors remain NOT_EVALUATED.
    """

    def supplied_capacity(yield_mpa):
        assert yield_mpa == 300.0  # arbitrary input forwarded to the capacity provider
        return 100.0

    monkeypatch.setattr(strength, "permissible_stress", supplied_capacity)
    result = strength.section_modulus_check(moment, 1.0, yield_mpa=300.0)
    assert isinstance(result, strength.SectionModulusCheck)
    assert result.stress_mpa == pytest.approx(stress, rel=1e-10)
    assert result.permissible_mpa == pytest.approx(100.0, rel=1e-10)
    assert result.utilization == pytest.approx(ratio, rel=1e-10)
    assert result.passes is passes


@pytest.mark.parametrize(
    "still, wave, ratio, passes",
    [(20.0, 30.0, 0.5, True), (-40.0, -60.0, 1.0, True), (40.0, 61.0, 1.01, False)],
)
def test_ultimate_strength_check_supplied_capacity_values(still, wave, ratio, passes):
    """Source: ultimate_strength_check public contract, explicit factors only.

    Stipulate all factors=1 and capacity=100 kN.m: magnitudes 20+30=50,
    40+60=100,40+61=101 give utilisation .5,1,1.01; equality passes.
    These are synthetic capacity checks, not evaluation of default rule factors.
    """
    result = strength.ultimate_strength_check(
        still, wave, 100.0, gamma_s=1.0, gamma_w=1.0, gamma_r=1.0
    )
    assert isinstance(result, strength.UltimateStrengthCheck)
    assert result.design_moment_kn_m == pytest.approx(
        {0.5: 50.0, 1.0: 100.0, 1.01: 101.0}[ratio], rel=1e-10
    )
    assert result.factored_capacity_kn_m == pytest.approx(100.0, rel=1e-10)
    assert result.utilization == pytest.approx(ratio, rel=1e-10)
    assert result.passes is passes


def test_ultimate_strength_check_explicit_nonunit_factors_value():
    """Source: public check contract, using synthetic factors, not rule defaults.

    Still-water 20 and wave 35 kN.m, factors 1.5 and 2 give 30+70=100 kN.m.
    Capacity 250 kN.m and resistance factor 1.25 give 200 kN.m available;
    utilisation=100/200=.5. Each input is stipulated for this arithmetic check.
    """
    result = strength.ultimate_strength_check(
        20.0, 35.0, 250.0, gamma_s=1.5, gamma_w=2.0, gamma_r=1.25
    )
    assert result.design_moment_kn_m == pytest.approx(100.0, rel=1e-10)
    assert result.ultimate_capacity_kn_m == pytest.approx(250.0, rel=1e-10)
    assert result.factored_capacity_kn_m == pytest.approx(200.0, rel=1e-10)
    assert result.utilization == pytest.approx(0.5, rel=1e-10)
    assert result.passes
