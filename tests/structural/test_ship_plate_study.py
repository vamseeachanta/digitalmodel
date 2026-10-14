"""Independent mechanics and eligibility checks for the bounded study."""
import importlib.util
import math
from pathlib import Path

import pytest

PATH = Path(__file__).parents[2] / 'scripts/studies/ship_plate_buckling.py'
spec = importlib.util.spec_from_file_location('ship_study', PATH)
study = importlib.util.module_from_spec(spec)
spec.loader.exec_module(study)


def case(**updates):
    c = dict(structure='plate', profile='none', length_mm=600., breadth_mm=600.,
             initial_thickness_mm=12., sigma_x_MPa=50., sigma_y_MPa=0., tau_MPa=0.,
             support='simply_supported', loss_model='whole_field_uniform',
             gamma_m=1.15, web_loss_mm=0.)
    c.update(updates)
    return c


def test_elastic_square_plate_and_scaling():
    # Hand formula: k*pi^2*E/[12*(1-nu^2)]*(t/b)^2.
    expected = 4*math.pi**2*206000/(12*(1-.3**2))*(6/600)**2
    assert study.evaluate(case(), 6)['elastic_MPa'] == pytest.approx(expected)
    assert study.evaluate(case(), 3)['elastic_MPa'] == pytest.approx(expected/4)


def test_threshold_independent_elastic_solution():
    # At 50 MPa*1.15 < fy/2 the critical point lies on the elastic branch.
    expected = 600*math.sqrt(57.5*12*(1-.3**2)/(4*math.pi**2*206000))
    r = study.compute(case())
    assert r['minimum_remaining_thickness_mm'] == pytest.approx(expected, abs=1e-5)
    assert r['threshold_utilization'] == pytest.approx(1, abs=1e-5)


@pytest.mark.parametrize('updates', [dict(sigma_y_MPa=1), dict(tau_MPa=1),
    dict(support='clamped'), dict(loss_model='local_patch'), dict(profile='bulb'),
    dict(length_mm=float('nan')), dict(sigma_x_MPa=0), dict(web_loss_mm=-1)])
def test_unsupported_inputs_are_not_computed(updates):
    assert study.compute(case(**updates))['status'] == 'inapplicable'


def test_failed_nominal_has_no_threshold():
    r = study.compute(case(sigma_x_MPa=1000))
    assert r['status'] == 'no_passing_thickness'
    assert r['minimum_remaining_thickness_mm'] is None


def test_jo_transition_is_continuous():
    a = study.analyzer()
    assert a.johnson_ostenfeld(355/2) == pytest.approx(355/2)
    assert a.johnson_ostenfeld(355/2+1e-6) == pytest.approx(355/2, abs=2e-6)


def test_lookup_never_interpolates_or_computes():
    row = study.compute(case())
    assert study.lookup([row], case()) == row
    assert study.lookup([row], case(length_mm=601))['status'] == 'uncomputed'


def test_invalid_panel_web_loss_rejected():
    r = study.compute(case(structure='panel', profile='flatbar-200x10', web_loss_mm=10))
    assert r['status'] == 'inapplicable'


@pytest.mark.parametrize('updates', [dict(material='Grade A'), dict(fca_mm=2),
    dict(torsional_restraint_spacing_mm=100), dict(initial_thickness_mm=.1)])
def test_fixed_assumptions_cannot_be_overridden_silently(updates):
    assert study.compute(case(**updates))['status'] == 'inapplicable'


def test_booleans_are_not_physical_numeric_parameters():
    assert study.compute(case(gamma_m=True))['status'] == 'inapplicable'
