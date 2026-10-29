# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic arithmetic checks; sources are identified in each formula case.

These checks establish equation transcription and screening behaviour, rather
than validation against measured squat. All dimensions and allowances are
synthetic inputs, including the supplied gravitational acceleration.
"""

from dataclasses import fields, replace
import math

import pytest

from digitalmodel.hydrodynamics.parametric_hull_analysis.squat_screening import (
    ChannelKind,
    EvaluationStatus,
    Formula,
    SquatInputs,
    SuppliedCorrections,
    UKCBudget,
    barrass_1981,
    compare_formulas,
    density_draught_correction,
    eryuzlu_1994,
    evaluate,
    huuska_guliev,
    maximum_speed,
    romisch_1989,
    ukc_table,
)


def hull(**changes):
    """A synthetic full-form hull; every model field is explicitly supplied."""
    values = dict(
        length_bp_m=240.0,
        beam_m=30.0,
        draught_m=10.0,
        block_coefficient=0.8,
        water_depth_m=14.4,
        channel_width_m=300.0,
        speed_m_s=4.0,
        gravity_m_s2=9.81,
        channel_kind=ChannelKind.OPEN,
    )
    return SquatInputs(**(values | changes))


def budget(**changes):
    values = dict(
        wave_allowance_m=0.4,
        tide_m=0.0,
        density_correction_m=0.0,
        heel_allowance_m=0.1,
        minimum_ukc_m=1.0,
    )
    return UKCBudget(**(values | changes))


def test_all_geometry_and_budget_inputs_required():
    from dataclasses import MISSING

    for model in (SquatInputs, UKCBudget):
        assert all(f.default is MISSING for f in fields(model))
        assert all(f.default_factory is MISSING for f in fields(model))


@pytest.mark.parametrize(
    "field",
    [
        "length_bp_m",
        "beam_m",
        "draught_m",
        "water_depth_m",
        "channel_width_m",
        "gravity_m_s2",
    ],
)
@pytest.mark.parametrize("value", [0, -1, math.nan, math.inf, -math.inf, True])
def test_invalid_dimensions(field, value):
    with pytest.raises(ValueError, match=field):
        hull(**{field: value})


@pytest.mark.parametrize(
    "field,value",
    [
        ("block_coefficient", 0),
        ("block_coefficient", 1.01),
        ("block_coefficient", math.nan),
        ("speed_m_s", -1),
        ("speed_m_s", math.inf),
        ("speed_m_s", math.nan),
        ("beam_m", 240),
        ("channel_width_m", 30),
        ("channel_kind", "unspecified"),
    ],
)
def test_invalid_hull_combinations(field, value):
    with pytest.raises(ValueError):
        hull(**{field: value})


def test_blockage_definitions_are_distinct():
    """Proxy=0.8*30*10/(300*14.4)=1/18; area ratio=5/72.

    Area ratio: Brodogradnja 68(2), 2017, equation (2). The proxy is a
    declared input convention, not that paper's midship-area definition.
    """
    item = hull()
    assert item.blockage_proxy == pytest.approx(1 / 18)
    assert item.area_blockage == pytest.approx(5 / 72)
    assert item.blockage_regime == "open"
    assert hull(channel_width_m=100).blockage_regime == "restricted"
    assert item.depth_froude == pytest.approx(4 / math.sqrt(9.81 * 14.4))


@pytest.mark.parametrize("method", list(Formula))
@pytest.mark.parametrize("depth", [10, 9])
def test_grounded_hulls_not_evaluated(method, depth):
    result = evaluate(hull(water_depth_m=depth), method)
    assert result.status is EvaluationStatus.NOT_EVALUATED
    assert result.maximum_m is None
    assert "draught" in result.reason


@pytest.mark.parametrize("method", list(Formula))
@pytest.mark.parametrize("multiplier", [1.0, 1.1])
def test_critical_depth_froude_rejected(method, multiplier):
    result = evaluate(hull(speed_m_s=multiplier * math.sqrt(9.81 * 14.4)), method)
    assert result.status is EvaluationStatus.NOT_EVALUATED
    assert "Fnh" in result.reason


@pytest.mark.parametrize("method", list(Formula))
def test_zero_and_speed_monotonicity(method):
    results = [evaluate(hull(speed_m_s=v), method) for v in (0, 2, 4, 6)]
    assert all(r.status is EvaluationStatus.EVALUATED for r in results)
    assert results[0].maximum_m == 0
    assert all(a.maximum_m < b.maximum_m for a, b in zip(results, results[1:]))
    assert all(r.source and r.applicability for r in results)


def test_barrass_independent_arithmetic():
    """Brodogradnja 68(2), 2017, (4)-(5): choose S=1/9.

    W=187.5, h=14.4, B*T=300 => S=1/9, S2=1/8.
    At Vk=8 kn, S2**(2/3)=1/4; squat=0.8*8**2.08/120.
    Expected value uses this reduced expression, not the implementation.
    """
    result = barrass_1981(
        hull(
            channel_width_m=187.5,
            channel_kind=ChannelKind.CANAL,
            speed_m_s=8 * 1852 / 3600,
        )
    )
    assert result.maximum_m == pytest.approx(0.8 * 8**2.08 / 120)
    assert result.bow_m == pytest.approx(0.8 * 8**2.08 / 120)
    assert result.stern_m is None and result.trim_change_m is None
    assert result.governing_end == "bow prediction only"


def test_romisch_bow_stern_and_trim_hand_calculation():
    """PIANC On Course 142 (2011), A4 (7)-(10).

    Supplied critical speed=8 m/s, V=4 => r=0.5, Cv=0.125.
    h/T=1.44 => K=0.155*1.2=0.186. L=240, B=30, Cb=0.8
    => Cf_bow=(240/240)**2=1. Bow=stern=0.125*0.186*10=0.2325.
    These are synthetic supplied critical speeds, not measured corrections.
    """
    settings = SuppliedCorrections(8, None, "Synthetic arithmetic input")
    result = romisch_1989(hull(), settings)
    assert result.bow_m == pytest.approx(0.2325)
    assert result.stern_m == pytest.approx(0.2325)
    assert result.governing_end == "both"
    assert result.trim_change_m == pytest.approx(0)
    full = romisch_1989(hull(length_bp_m=200), settings)
    fine = romisch_1989(hull(block_coefficient=0.6), settings)
    assert full.governing_end == "bow"
    assert fine.governing_end == "stern"
    assert full.bow_m == pytest.approx(0.3348)  # 0.2325*(240/200)**2
    assert fine.bow_m == pytest.approx(0.13078125)  # 0.2325*(180/240)**2
    assert full.trim_change_m > 0 and fine.trim_change_m < 0


def test_romisch_open_critical_speed():
    """PIANC 142 (2011), A4 (11): Vcr=sqrt(gh)*.58*(hL/BT)**.125.

    Independent dimensionless calculation: hL/BT=11.52 and
    r=4/sqrt(141.264)/(.58*11.52**.125); stern=1.86*8*r²*((r-.5)^4+.0625).
    """
    r = 4 / math.sqrt(141.264) / (0.58 * 11.52**0.125)
    assert romisch_1989(hull()).stern_m == pytest.approx(
        1.86 * 8 * r**2 * ((r - 0.5) ** 4 + 0.0625)
    )


def test_huuska_icorels_hand_calculation():
    """PIANC 142 (2011), A3 (6), Ks=1 in open water.

    Set Fnh=0.6: Fnh²/sqrt(1-Fnh²)=.36/.8=.45.
    2.4*Cb*B*T/L=2.4; bow squat=2.4*.45=1.08 m.
    """
    result = huuska_guliev(hull(speed_m_s=0.6 * math.sqrt(9.81 * 14.4)))
    assert result.bow_m == pytest.approx(1.08)
    assert result.stern_m is None
    assert result.maximum_m == pytest.approx(1.08)


def test_eryuzlu_hand_calculation_and_width_correction():
    """PIANC 142 (2011), A2 (5); Brodogradnja 2017, (8) for Kb.

    Choose V/sqrt(gT)=0.5, h/T=1.44. Reduced expression is
    .298*10*.5**2.289*1.44**(-.972) m. At W/B=4, Kb=3.1/2=1.55.
    The 2017 (7) misprints the exponent; the PIANC image gives 2.289.
    """
    item = hull(speed_m_s=0.5 * math.sqrt(98.1))
    expected = 2.98 * 0.5**2.289 * 1.44 ** (-0.972)
    assert eryuzlu_1994(item).bow_m == pytest.approx(expected)
    restricted = replace(item, channel_width_m=120, channel_kind=ChannelKind.DREDGED)
    assert eryuzlu_1994(restricted).bow_m == pytest.approx(expected * 1.55)
    assert (
        eryuzlu_1994(replace(restricted, channel_kind=ChannelKind.CANAL)).maximum_m
        is None
    )


@pytest.mark.parametrize(
    "method,depth",
    [
        (Formula.BARRASS_1981, 10.9),
        (Formula.BARRASS_1981, 15.1),
        (Formula.ROMISCH_1989, 11.8),
        (Formula.ROMISCH_1989, 22.6),
        (Formula.HUUSKA_GULIEV, 10.9),
        (Formula.HUUSKA_GULIEV, 20.1),
        (Formula.ERYUZLU_1994, 10.9),
        (Formula.ERYUZLU_1994, 25.1),
    ],
)
def test_published_depth_envelopes(method, depth):
    """Brodogradnja 68(2), 2017, Table 1: no extrapolation."""
    result = evaluate(hull(water_depth_m=depth), method)
    assert result.status is EvaluationStatus.NOT_EVALUATED
    assert "h/T" in result.reason


@pytest.mark.parametrize(
    "method,cb",
    [
        (Formula.BARRASS_1981, 0.49),
        (Formula.BARRASS_1981, 0.91),
        (Formula.ERYUZLU_1994, 0.79),
    ],
)
def test_published_block_coefficients(method, cb):
    assert evaluate(hull(block_coefficient=cb), method).maximum_m is None


def test_missing_restricted_corrections_fail_closed():
    item = hull(channel_width_m=100, channel_kind=ChannelKind.DREDGED)
    assert romisch_1989(item).status is EvaluationStatus.NOT_EVALUATED
    assert "critical speed" in romisch_1989(item).reason
    assert huuska_guliev(item).status is EvaluationStatus.NOT_EVALUATED
    assert "width factor" in huuska_guliev(item).reason
    settings = SuppliedCorrections(8, 1.2, "Synthetic supplied corrections")
    assert romisch_1989(item, settings).maximum_m == pytest.approx(0.2325)
    # 2.4*16/141.264/sqrt(1-16/141.264), multiplied by supplied Ks=1.2.
    assert huuska_guliev(item, settings).maximum_m == pytest.approx(
        2.88 * (16 / 141.264) / math.sqrt(1 - 16 / 141.264)
    )
    assert romisch_1989(replace(item, speed_m_s=8), settings).maximum_m is None


@pytest.mark.parametrize(
    "critical,factor,source",
    [
        (0, None, "x"),
        (-1, None, "x"),
        (math.inf, None, "x"),
        (None, 0, "x"),
        (None, math.nan, "x"),
        (8, 1, ""),
        (8, 1, "  "),
        (8, 1, None),
    ],
)
def test_invalid_supplied_corrections(critical, factor, source):
    with pytest.raises(ValueError):
        SuppliedCorrections(critical, factor, source)


def test_open_geometry_cannot_hide_confinement():
    result = romisch_1989(hull(channel_width_m=60))
    assert result.status is EvaluationStatus.NOT_EVALUATED
    assert "open" in result.reason


@pytest.mark.parametrize("rho,expected", [(1025, 0), (1000, 0.3), (1012.5, 0.15)])
def test_density_interpolation(rho, expected):
    """Load Line Regulations, section 8(1): .3*(1025-rho)/25.

    Synthetic certified fresh-water allowance=.3 m, salt reference=1025 kg/m³.
    """
    assert density_draught_correction(rho, 0.3) == pytest.approx(expected)


@pytest.mark.parametrize(
    "rho,fwa",
    [(999, 0.3), (1026, 0.3), (math.nan, 0.3), (1025, -0.1), (1025, math.inf)],
)
def test_density_outside_interpolation_range(rho, fwa):
    with pytest.raises(ValueError):
        density_draught_correction(rho, fwa)


@pytest.mark.parametrize(
    "field", ["wave_allowance_m", "heel_allowance_m", "minimum_ukc_m"]
)
@pytest.mark.parametrize("value", [-0.1, math.nan, math.inf])
def test_invalid_budget_allowances(field, value):
    with pytest.raises(ValueError):
        budget(**{field: value})


def test_ukc_budget_hand_arithmetic():
    """Brodogradnja 2017, (1): depth+tide-draught-corrections-squat.

    h=14.4+.5=14.9; adjusted T=10+.2=10.2. Fnh=.6 => ratio=.45.
    HG=2.4*.8*30*10.2/240*.45=1.1016.
    Net=14.9-10.2-1.1016-.4-.1=3.0984; margin above 3 m=.0984.
    Tide and density must affect the squat geometry, not just the subtraction.
    """
    rows = ukc_table(
        hull(),
        [0.6 * math.sqrt(9.81 * 14.9)],
        Formula.HUUSKA_GULIEV,
        budget(tide_m=0.5, density_correction_m=0.2, minimum_ukc_m=3),
    )
    row = rows[0]
    assert row.squat.maximum_m == pytest.approx(1.1016)
    assert row.net_ukc_m == pytest.approx(3.0984)
    assert row.margin_m == pytest.approx(0.0984)
    assert row.meets_criterion is True
    assert row.adjusted_draught_m == pytest.approx(10.2)
    assert row.tide_m == 0.5 and row.wave_allowance_m == 0.4
    assert row.density_correction_m == 0.2 and row.heel_allowance_m == 0.1


def test_negative_ukc_and_unknown_rows_are_preserved():
    row = ukc_table(hull(), [4], Formula.HUUSKA_GULIEV, budget(wave_allowance_m=10))[0]
    assert row.net_ukc_m < 0 and row.meets_criterion is False
    unknown = ukc_table(
        hull(), [math.sqrt(9.81 * 14.4)], Formula.HUUSKA_GULIEV, budget()
    )[0]
    assert unknown.net_ukc_m is None
    assert unknown.meets_criterion is None
    assert unknown.squat.status is EvaluationStatus.NOT_EVALUATED


def test_zero_tide_effective_grounding():
    row = ukc_table(hull(), [0], Formula.HUUSKA_GULIEV, budget(tide_m=-5))[0]
    assert row.squat.status is EvaluationStatus.NOT_EVALUATED
    assert row.meets_criterion is None


def test_speed_limit_independent_root_and_monotonic_criterion():
    """HG: choose desired squat=1.08 at Fnh=.6, hence criterion=2.82.

    Still water UKC=14.4-10-.4-.1=3.9. 3.9-1.08=2.82.
    The root is therefore .6*sqrt(141.264), without solving using the code.
    """
    result = maximum_speed(
        hull(), Formula.HUUSKA_GULIEV, budget(minimum_ukc_m=2.82), 8, 1e-7
    )
    expected = 0.6 * math.sqrt(141.264)
    assert result.status is EvaluationStatus.EVALUATED
    assert 0 <= expected - result.speed_m_s <= 1e-7
    assert result.limited_by == "criterion"
    stricter = maximum_speed(
        hull(), Formula.HUUSKA_GULIEV, budget(minimum_ukc_m=3.2), 8, 1e-7
    )
    assert stricter.speed_m_s < result.speed_m_s
    assert ukc_table(
        hull(), [result.speed_m_s], Formula.HUUSKA_GULIEV, budget(minimum_ukc_m=2.82)
    )[0].meets_criterion


def test_speed_cap_and_no_feasible_speed():
    capped = maximum_speed(hull(), Formula.HUUSKA_GULIEV, budget(), 2, 1e-6)
    assert capped.speed_m_s == 2 and capped.limited_by == "supplied cap"
    failed = maximum_speed(
        hull(), Formula.HUUSKA_GULIEV, budget(minimum_ukc_m=5), 8, 1e-6
    )
    assert failed.speed_m_s is None and failed.limited_by == "no feasible speed"
    invalid = maximum_speed(hull(), Formula.HUUSKA_GULIEV, budget(), 20, 1e-6)
    assert invalid.status is EvaluationStatus.NOT_EVALUATED
    assert invalid.speed_m_s is None


@pytest.mark.parametrize(
    "cap,tolerance", [(-1, 0.1), (1, 0), (1, math.nan), (math.inf, 0.1)]
)
def test_invalid_speed_search(cap, tolerance):
    with pytest.raises(ValueError):
        maximum_speed(hull(), Formula.HUUSKA_GULIEV, budget(), cap, tolerance)


def test_cross_check_spread_and_missing_method():
    """Spread is the range of the independent formula results, not an error bar."""
    table = compare_formulas(hull())
    assert len(table.results) == 4
    values = [r.maximum_m for r in table.results]
    assert table.spread_m == pytest.approx(max(values) - min(values))
    assert table.all_evaluated
    bow_only = next(r for r in table.results if r.method is Formula.BARRASS_1981)
    assert bow_only.bow_m == bow_only.maximum_m
    assert bow_only.stern_m is None and bow_only.trim_change_m is None
    assert bow_only.governing_end == "bow prediction only"
    restricted = compare_formulas(
        hull(channel_kind=ChannelKind.DREDGED, channel_width_m=100)
    )
    assert not restricted.all_evaluated
    assert restricted.spread_m is None  # Do not conceal missing methods.
    assert len(restricted.results) == 4


def test_unknown_method_rejected():
    with pytest.raises(ValueError):
        evaluate(hull(), "invented")


@pytest.mark.parametrize(
    "method,ratio",
    [
        (Formula.BARRASS_1981, 1.1),
        (Formula.BARRASS_1981, 1.5),
        (Formula.ROMISCH_1989, 1.19),
        (Formula.ROMISCH_1989, 2.25),
        (Formula.HUUSKA_GULIEV, 1.1),
        (Formula.HUUSKA_GULIEV, 2.0),
        (Formula.ERYUZLU_1994, 1.1),
        (Formula.ERYUZLU_1994, 2.5),
    ],
)
def test_inclusive_source_depth_boundaries(method, ratio):
    """Table 1 bounds in Brodogradnja 2017, included by explicit convention."""
    result = evaluate(
        hull(
            draught_m=1,
            water_depth_m=ratio,
            beam_m=3,
            length_bp_m=24,
            channel_width_m=30,
            speed_m_s=0.1,
        ),
        method,
    )
    assert result.status is EvaluationStatus.EVALUATED


@pytest.mark.parametrize("field", ["tide_m", "density_correction_m"])
@pytest.mark.parametrize("value", [math.nan, math.inf, -math.inf])
def test_nonfinite_signed_budget_inputs(field, value):
    with pytest.raises(ValueError):
        budget(**{field: value})


def test_wave_and_heel_allowances_reduce_screened_speed():
    """Geometric accounting: any added downward allowance consumes margin."""
    reference = maximum_speed(
        hull(), Formula.HUUSKA_GULIEV, budget(minimum_ukc_m=2.82), 8, 1e-7
    )
    for extra in (dict(wave_allowance_m=0.8), dict(heel_allowance_m=0.5)):
        limited = maximum_speed(
            hull(), Formula.HUUSKA_GULIEV, budget(minimum_ukc_m=2.82, **extra), 8, 1e-7
        )
        assert limited.speed_m_s < reference.speed_m_s


def test_inapplicable_effective_geometry_never_passes():
    """A favourable tide can leave the empirical depth range: do not extrapolate."""
    row = ukc_table(hull(), [0], Formula.BARRASS_1981, budget(tide_m=1))[0]
    assert row.squat.status is EvaluationStatus.NOT_EVALUATED
    assert row.meets_criterion is None
    with pytest.raises(ValueError):
        ukc_table(hull(), [0], Formula.BARRASS_1981, budget(tide_m=-15))


def test_unresolvable_numerical_tolerance_is_not_a_limit():
    result = maximum_speed(
        hull(), Formula.HUUSKA_GULIEV, budget(minimum_ukc_m=2.82), 8, 1e-30
    )
    assert result.status is EvaluationStatus.NOT_EVALUATED
    assert result.speed_m_s is None and result.limited_by == "precision"


def test_no_corrections_are_fabricated_or_lost():
    item = hull(channel_kind=ChannelKind.DREDGED, channel_width_m=100)
    supplied = SuppliedCorrections(8, 1.2, "Synthetic required inputs")
    result = huuska_guliev(item, supplied)
    assert result.inputs == item
    assert result.supplied_corrections == supplied
    assert romisch_1989(item).supplied_corrections is None
    assert result.stern_m is None and result.trim_change_m is None


def test_dredged_channel_worked_example():
    """Independent arithmetic for the documented synthetic dredged channel.

    h=14.4+.6=15, T=10+.2=10.2, S=306/1800=.17,
    S2=17/83, Cb=.8, Vk=3600*V/1852; Barrass=.8*(17/83)**(2/3)*Vk**2.08/30.
    Romisch r=V/8, Cf=1, stern=.155*sqrt(153)*8*r²*((r-.5)^4+.0625).
    HG prefactor=2.4*.8*30*10.2/240*1.2=2.9376, Fnh²=V²/147.15.
    Eryuzlu=3.0396*(V/sqrt(100.062))**2.289*(25/17)**(-.972)*1.55.
    UKC=4.3-squat. Values below were independently evaluated from these
    reduced expressions, rounded to six decimals for the test only.
    Sources: [P11] A2-A4; [B17] (4)-(5),(8), as identified in the module.
    """
    from pathlib import Path
    import runpy

    example_path = (
        Path(__file__).resolve().parents[3]
        / "examples"
        / "hydrodynamics"
        / "squat_screening.py"
    )
    namespace = runpy.run_path(str(example_path))
    data = namespace["worked_example"]()
    expected = {
        Formula.BARRASS_1981: (0.156114, 0.660060, 1.534098),
        Formula.ROMISCH_1989: (0.063658, 0.239656, 0.572926),
        Formula.HUUSKA_GULIEV: (0.080961, 0.338336, 0.826914),
        Formula.ERYUZLU_1994: (0.081301, 0.397332, 1.005141),
    }
    for method, values in expected.items():
        for row, value in zip(data["rows"][method], values, strict=True):
            assert row.squat.maximum_m == pytest.approx(value, abs=5.1e-7)
            assert row.net_ukc_m == pytest.approx(4.3 - value, abs=5.1e-7)
            if method is Formula.BARRASS_1981:
                assert row.squat.bow_m == pytest.approx(value, abs=5.1e-7)
                assert row.squat.stern_m is None and row.squat.trim_change_m is None
                assert row.squat.governing_end == "bow prediction only"
    assert not data["rows"][Formula.BARRASS_1981][-1].meets_criterion
    assert all(
        data["rows"][method][-1].meets_criterion
        for method in (
            Formula.ROMISCH_1989,
            Formula.HUUSKA_GULIEV,
            Formula.ERYUZLU_1994,
        )
    )
    assert data["limits"][Formula.BARRASS_1981].speed_m_s < 6
    assert "NOT_EVALUATED" in namespace["render_example"](data)
