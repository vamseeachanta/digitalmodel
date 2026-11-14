"""F2/F3 goldens from the F1 validation record and Decimal re-evaluation; #1148.

The approved fallback retains the fixed-end axial base equations and leaves
the unverified OPB brace equation unchanged. These are factor-wiring tests,
not validation of a complete general-fixity or OPB brace model.
"""

import math

import pytest

from digitalmodel.fatigue import scf_library as scf


def test_f2_formula_and_independent_decimal_golden():
    # Independent 50-digit Decimal evaluation: 0.8649930157714557586676...
    expected = 1 - (1.43 * 0.5 - 0.97 * 0.5**2 - 0.03) * 12**0.04 * math.exp(
        -0.71 * 12**-1.38 * 5**2.5
    )
    actual = scf.short_chord_factor_f2(0.5, 12, 5)
    assert actual == pytest.approx(expected, rel=1e-12)
    assert actual == pytest.approx(0.8649930157714558, rel=1e-12)
    assert actual == pytest.approx(0.865, abs=0.0005)


def test_f3_formula_and_independent_decimal_golden():
    # Independent 50-digit Decimal evaluation: 0.9111116947671575262172...
    expected = 1 - 0.55 * 0.5**1.8 * 12**0.16 * math.exp(
        -0.49 * 12**-0.89 * 5**1.8
    )
    actual = scf.short_chord_factor_f3(0.5, 12, 5)
    assert actual == pytest.approx(expected, rel=1e-12)
    assert actual == pytest.approx(0.9111116947671575, rel=1e-12)
    assert actual == pytest.approx(0.911, abs=0.0005)


@pytest.mark.parametrize("factor_name", ["short_chord_factor_f2", "short_chord_factor_f3"])
@pytest.mark.parametrize("alpha", [12.0, 20.0])
def test_long_chord_factors_are_exactly_one(factor_name, alpha):
    assert getattr(scf, factor_name)(0.5, 12, alpha) == 1.0


@pytest.mark.parametrize("factor_name", ["short_chord_factor_f2", "short_chord_factor_f3"])
def test_short_chord_boundary_still_uses_formula(factor_name):
    assert getattr(scf, factor_name)(0.5, 12, 11.999) < 1.0


@pytest.mark.parametrize("factor_name", [
    "short_chord_factor_f1", "short_chord_factor_f2", "short_chord_factor_f3",
])
def test_factors_are_public_exports(factor_name):
    import digitalmodel.fatigue as fatigue

    assert getattr(fatigue, factor_name) is getattr(scf, factor_name)
    assert factor_name in fatigue.__all__


@pytest.mark.parametrize("dimensions, expected", [
    ((1000, 50, 500, 25, 90), (4.178, 4.062)),
    ((1000, 50, 500, 25, 45), (2.400, 2.413)),
    ((1000, 50, 300, 15, 90), (2.221, 3.334)),
    ((1000, 50, 800, 40, 90), (5.370, 3.839)),
    ((1200, 50, 600, 25, 90), (4.755, 4.423)),
])
def test_fixed_axial_default_preserves_five_f1_goldens(dimensions, expected):
    D, T, d, t, theta = dimensions
    geom = scf.TubularJointGeometry(D=D, T=T, d=d, t=t, theta=theta)
    default = scf.efthymiou_ty_axial(geom)
    fixed = scf.efthymiou_ty_axial(geom, chord_ends="fixed")
    assert default == fixed
    assert (fixed.scf_chord, fixed.scf_brace) == expected
    assert fixed.governing == max(expected)
    assert "fixed chord ends" in fixed.method


def test_pinned_axial_applies_f2_to_both_saddles():
    geom = scf.TubularJointGeometry(D=1200, T=50, d=600, t=25, theta=90)
    result = scf.efthymiou_ty_axial(geom, chord_ends="pinned")
    # Decimal-derived saddle values: 5.36925547839777 / 4.99530514105770.
    assert result.scf_chord == 5.369
    assert result.scf_brace == 4.995
    assert result.governing == 5.369
    assert "pinned chord ends" in result.method
    assert "F2" in result.method
    assert "approximation" in result.method


@pytest.mark.parametrize("length", [7200, 12000])
def test_pinned_long_chord_recovers_fixed_base_values(length):
    geom = scf.TubularJointGeometry(D=1200, T=50, d=600, t=25, L=length)
    fixed = scf.efthymiou_ty_axial(geom)
    pinned = scf.efthymiou_ty_axial(geom, chord_ends="pinned")
    assert pinned.scf_chord == fixed.scf_chord
    assert pinned.scf_brace == fixed.scf_brace


def test_pinned_crowns_retain_fixed_end_base_forms():
    geom = scf.TubularJointGeometry(D=1000, T=50, d=500, t=25, theta=30)
    result = scf.efthymiou_ty_axial(geom, chord_ends="pinned")
    chord_crown = 10**0.2 * 0.5 * (2.65 + 5 * (0.5 - 0.65)**2)
    chord_crown += 0.5 * 0.5 * (0.25 * 5 - 3) * math.sin(math.radians(30))
    brace_crown = 3 + 10**1.2 * (0.12 * math.exp(-2) + 0.011 * 0.25 - 0.045)
    brace_crown += 0.5 * 0.5 * (0.1 * 5 - 1.2)
    assert result.scf_chord == round(chord_crown, 3)
    assert result.scf_brace == round(brace_crown, 3)


def test_oblique_pinned_result_discloses_missing_upper_bound():
    geom = scf.TubularJointGeometry(D=1000, T=50, d=500, t=25, theta=45)
    result = scf.efthymiou_ty_axial(geom, chord_ends="pinned")
    # Table 2 records this fallback value, not a verified general-fixity SCF.
    assert result.scf_brace == 2.530
    assert "upper bound not established" in result.method
    assert "upper bound not established" not in scf.efthymiou_ty_axial(geom).method


@pytest.mark.parametrize("chord_ends", ["free", "FIXED", "", None])
def test_axial_rejects_unknown_fixity(chord_ends):
    geom = scf.TubularJointGeometry(D=1200, T=50, d=600, t=25)
    with pytest.raises(ValueError, match="chord_ends"):
        scf.efthymiou_ty_axial(geom, chord_ends=chord_ends)


@pytest.mark.parametrize("length, expected", [(0, 4.288), (7200, 4.706), (12000, 4.706)])
def test_opb_chord_golden_and_long_chord_recovery(length, expected):
    geom = scf.TubularJointGeometry(D=1200, T=50, d=600, t=25, L=length)
    # Eqn 10: 12*0.5*0.5*(1.7-1.05*0.5**3) = 4.70625.
    # At alpha=5: 4.70625*F3 = 4.28791941349793510776 (Decimal).
    result = scf.efthymiou_ty_opb(geom)
    assert result.scf_chord == expected
    assert "Table B-1" in result.method


def test_opb_oblique_angle_is_corrected_once():
    geom = scf.TubularJointGeometry(D=1200, T=50, d=600, t=25, theta=45)
    expected = 4.287919413497935 * math.sin(math.radians(45))**1.6
    assert scf.efthymiou_ty_opb(geom).scf_chord == round(expected, 3)


def test_ipb_is_independent_of_chord_length():
    short = scf.TubularJointGeometry(D=1200, T=50, d=600, t=25, L=2500)
    long = short.model_copy(update={"L": 7200})
    assert scf.efthymiou_ty_ipb(short) == scf.efthymiou_ty_ipb(long)
