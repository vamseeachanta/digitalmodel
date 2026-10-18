"""Independent numerical and invalid-input regressions for the VIV audit."""
from dataclasses import asdict
import math

import pytest

from digitalmodel.citations import Citation, CitationResolutionError
from digitalmodel.subsea.viv_analysis import (
    BoundaryCondition, FrequencyCalculator, STEEL_CARBON, TubularMember,
    VIVFatigueCalculator, VIVScreening, VortexSheddingAnalyzer,
)


@pytest.fixture
def member():
    return TubularMember('uniform-tube', 50.0, 0.5, 0.025, STEEL_CARBON)


def test_d_air_endurance_and_damage():
    # DNV-RP-C203 October 2011 Table 2-1 / Eq. (2.4.1):
    # log10 N = 12.164 - 3*2 = 6.164; N = 1,458,814.2603 cycles.
    calc = VIVFatigueCalculator()
    assert calc.calculate_allowable_cycles(100.0) == pytest.approx(1.46e6, rel=0.01)
    assert calc.calculate_damage(100.0, 0.5, 86400) == pytest.approx(
        0.029613091382755, rel=1e-9,
    )
    assert calc.calculate_fatigue_life(100.0, 0.5) == pytest.approx(
        0.09245406876793, rel=1e-9,
    )


def test_d_air_knee_and_second_slope():
    # Table 2-1: knee = 10^7 cycles, S_k = 10^((12.164-7)/3)
    # = 52.64211545408 MPa. Rounded intercepts permit a 0.2% knee mismatch.
    calc = VIVFatigueCalculator()
    assert calc.SN_CURVES['DNV-D'].knee == 1e7
    for stress in (52.64211545408 - 1e-5, 52.64211545408 + 1e-5):
        assert calc.calculate_allowable_cycles(stress) == pytest.approx(1e7, rel=0.002)
    # log10 N(30) = 15.606 - 5 log10(30) = 8.2203873028
    assert calc.calculate_allowable_cycles(30.0) == pytest.approx(166109215.213006, rel=1e-9)
    assert calc.calculate_fatigue_life(30.0, 0.3) == pytest.approx(17.54561132796, rel=1e-9)


@pytest.mark.parametrize('curve,expected_k', [('DNV-B1', 0), ('DNV-C', .15),
                                             ('DNV-D', .20), ('DNV-F', .25)])
def test_thickness_ratio(curve, expected_k):
    # C203 (2011) Eq. (2.4.3), Table 2-1: S_eff=S*(t/25)^k.
    calc = VIVFatigueCalculator()
    base = calc.calculate_damage(150., .5, 3600, curve, True, 25.)
    thick = calc.calculate_damage(150., .5, 3600, curve, True, 50.)
    slope = 4 if curve == 'DNV-B1' else 3
    assert thick / base == pytest.approx(2 ** (slope * expected_k), abs=1e-9)
    assert thick >= base


def test_thick_member_life_matches_damage(member):
    member.wall_thickness = .05
    result = VIVFatigueCalculator().analyze_viv_fatigue(member, .3, .5, 24)
    assert result.fatigue_damage * result.fatigue_life_years == pytest.approx(
        24 / (365.25 * 24), rel=1e-12,
    )


def test_thickness_requested_without_value_fails():
    with pytest.raises(ValueError, match='thickness'):
        VIVFatigueCalculator().calculate_damage(100., .5, 3600., thickness_correction=True)


def test_curve_citation_sidecar(member):
    calc = VIVFatigueCalculator()
    result = calc.analyze_viv_fatigue(member, .3, .5, 24)
    assert isinstance(calc.citations[0], Citation)
    assert result.citations == [asdict(calc.citations[0])]
    assert result.citations[0]['revision'] == '2011-10'
    assert 'Table 2-1' in result.citations[0]['section']
    assert result.citations[0]['source_sibling'] == 'generic'


def test_missing_citation_fails_closed(tmp_path, monkeypatch):
    empty_wiki = tmp_path / 'empty'
    (empty_wiki / 'wikis').mkdir(parents=True)
    monkeypatch.setenv('LLM_WIKI_PATH', str(empty_wiki))
    with pytest.raises(CitationResolutionError, match='DNV-RP-C203'):
        VIVFatigueCalculator().calculate_damage(100., .5, 3600.)


def test_independent_pinned_frequency(member):
    # Euler-Bernoulli pinned beam; A=.03730641276 m², I=.001055092005 m⁴,
    # m=292.8553402 kg/m, added mass=201.2582794 kg/m, E=207 GPa.
    result = FrequencyCalculator().calculate_natural_frequency(member)
    assert result.frequency == pytest.approx(.41773, abs=1e-4)


@pytest.mark.parametrize('field', ['diameter', 'velocity', 'strouhal_number'])
@pytest.mark.parametrize('bad', [float('nan'), float('inf'), -float('inf'), -1.])
def test_shedding_rejects_invalid_inputs(field, bad):
    args = dict(diameter=.5, velocity=1., strouhal_number=.2)
    args[field] = bad
    with pytest.raises(ValueError):
        VortexSheddingAnalyzer().shedding_frequency(**args)


@pytest.mark.parametrize('field', ['diameter', 'strouhal_number'])
def test_shedding_rejects_zero_positive_quantities(field):
    args = dict(diameter=.5, velocity=1., strouhal_number=.2)
    args[field] = 0.
    with pytest.raises(ValueError):
        VortexSheddingAnalyzer().shedding_frequency(**args)


@pytest.mark.parametrize('field', ['current_velocity', 'natural_frequency', 'diameter'])
@pytest.mark.parametrize('bad', [float('nan'), float('inf'), -1.])
def test_reduced_velocity_rejects_invalid_inputs(field, bad):
    args = dict(current_velocity=1., natural_frequency=.4, diameter=.5)
    args[field] = bad
    with pytest.raises(ValueError):
        VIVScreening().reduced_velocity(**args)


@pytest.mark.parametrize('method', ['check_lock_in', 'calculate_safety_factor'])
@pytest.mark.parametrize('bad', [float('nan'), float('inf'), -1.])
def test_screening_helpers_reject_invalid_reduced_velocity(method, bad):
    with pytest.raises(ValueError):
        getattr(VIVScreening(), method)(bad)


@pytest.mark.parametrize('tension', [-1., 0., float('nan'), float('inf')])
def test_tension_screening_fails_without_status(member, tension):
    member.boundary_condition = BoundaryCondition.TENSION_CONTROLLED
    member.top_tension = tension
    with pytest.raises(ValueError, match='tension'):
        VIVScreening().screen_member(member, 1.5)


def test_nan_velocity_fails_without_status(member):
    with pytest.raises(ValueError):
        VIVScreening().screen_member(member, math.nan)


def test_zero_flow_remains_valid(member):
    result = VIVScreening().screen_member(member, 0.)
    assert result.lock_in_status == 'safe'
    assert result.shedding_frequency == 0.
