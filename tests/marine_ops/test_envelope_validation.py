"""Audit regressions; analytical values are derived in envelope-basis.md."""
from dataclasses import replace
import importlib

import numpy as np
import pytest

from digitalmodel.hydrodynamics.diffraction.output_schemas import RAOSet
from digitalmodel.marine_ops.operation_envelope import (
    MotionLimit, OPERATIONS, operation_envelope,
)
from tests.marine_ops.test_operation_envelope import _rao_set


def full_rao():
    rao = _rao_set()
    for name in ('surge', 'sway', 'heave', 'roll', 'pitch', 'yaw'):
        component = getattr(rao, name)
        component.frequencies.values = np.array([0.001, 0.1, 0.5, 1., 5., 15.])
    return rao


@pytest.mark.parametrize('dof,unit', [('Heave', 'm'), ('heave', 'cm'), ('heave', 'rad'), ('roll', 'm')])
def test_invalid_criterion(dof, unit):
    with pytest.raises(ValueError):
        op = replace(OPERATIONS['transit'], limits=(MotionLimit(dof, 3., unit),))
        operation_envelope(full_rao(), op, tp_range_s=[12.])


@pytest.mark.parametrize('kwargs', [
    {'headings': []}, {'headings': [np.nan]}, {'gamma': 0.}, {'gamma': .9},
    {'gamma': np.nan}, {'hs_ref': 0.}, {'hs_ref': -1.}, {'hs_ref': np.inf},
    {'tp_range_s': []}, {'tp_range_s': [0.]}, {'tp_range_s': [-4.]},
    {'tp_range_s': [np.nan]}, {'hs_cap_m': 0.}, {'hs_cap_m': np.inf},
])
def test_invalid_calculation_inputs(kwargs):
    with pytest.raises(ValueError):
        operation_envelope(full_rao(), 'transit', **kwargs)


@pytest.mark.parametrize('alpha', [0., -.9, 1.1, np.nan, np.inf])
def test_invalid_alpha(alpha):
    with pytest.raises(ValueError):
        operation_envelope(full_rao(), replace(OPERATIONS['transit'], alpha=alpha))


@pytest.mark.parametrize('value', [0., -1., np.nan, np.inf])
def test_invalid_limit(value):
    with pytest.raises(ValueError):
        op = replace(OPERATIONS['transit'], limits=(MotionLimit('heave', value, 'm'),))
        operation_envelope(full_rao(), op)


def test_empty_criteria_and_missing_required_component():
    with pytest.raises(ValueError):
        operation_envelope(full_rao(), replace(OPERATIONS['transit'], limits=()))
    rao = full_rao()
    rao.heave = None
    with pytest.raises(ValueError):
        operation_envelope(rao, 'transit')


@pytest.mark.parametrize('field', ['magnitude', 'frequency', 'heading'])
def test_nonfinite_rao_is_not_silently_dropped(field):
    rao = full_rao()
    rao.heave.magnitude[:, 2] = 2.
    if field == 'magnitude':
        rao.heave.magnitude[0, 2] = np.nan
    elif field == 'frequency':
        rao.heave.frequencies.values[0] = np.nan
    else:
        rao.heave.headings.values = rao.heave.headings.values.copy()
        rao.heave.headings.values[2] = np.nan
    with pytest.raises(ValueError):
        operation_envelope(rao, 'transit', tp_range_s=[12.])


@pytest.mark.parametrize('shape', [(5, 6), (6,), (5, 5)])
def test_bad_or_ambiguous_shape(shape):
    rao = full_rao()
    rao.heave.magnitude = np.ones(shape)
    if shape == (5, 5):
        rao.heave.frequencies.values = rao.heave.frequencies.values[:5]
    with pytest.raises(ValueError):
        operation_envelope(rao, 'transit', tp_range_s=[12.])


def test_declared_units_survive_roundtrip_and_are_rejected():
    rao = full_rao()
    data = rao.to_dict()
    data['raos']['roll']['unit'] = 'rad/m'
    data['raos']['roll']['magnitude'] = (np.ones((6, 5)) * np.pi / 18).tolist()
    restored = RAOSet.from_dict(data)
    assert restored.roll.unit == 'rad/m'
    with pytest.raises(ValueError, match='unit'):
        operation_envelope(restored, 'transit', tp_range_s=[12.])
    rao.heave.frequencies.unit = 'Hz'
    with pytest.raises(ValueError, match='unit'):
        operation_envelope(rao, 'transit', tp_range_s=[12.])


def test_inadequate_band_is_rejected():
    rao = full_rao()
    rao.heave.frequencies.values = np.linspace(.2, 1.2, 6)
    result = operation_envelope(rao, 'transit', tp_range_s=[4.])
    point = result.points[0]
    assert point.status == 'insufficient_coverage'
    assert not point.evaluated and point.hs_limit_m is None
    assert point.extrapolated_energy_fraction > .01
    # Strict consumers still reject inadequate coverage; the grid itself survives.
    with pytest.raises(ValueError, match='coverage'):
        result.require_evaluated()


def test_full_band_retains_hand_value_and_records_energy():
    point = operation_envelope(full_rao(), 'transit', tp_range_s=[12.]).points[0]
    assert point.hs_limit_m == pytest.approx(5.4, rel=1e-3)
    assert point.evaluated
    assert point.status == 'evaluated'
    assert point.extrapolated_energy_fraction == 0.
    assert .99 <= point.captured_variance_fraction <= 1.


def test_circular_heading_and_incomplete_heading_domain():
    rao = full_rao()
    for name in ('surge', 'sway', 'heave', 'roll', 'pitch', 'yaw'):
        component = getattr(rao, name)
        component.headings.values = np.arange(0., 360., 45.)
        component.magnitude = np.full((6, 8), 1. if name == 'heave' else 0.)
        component.phase = np.zeros((6, 8))
    point = operation_envelope(rao, 'transit', headings=[350.], tp_range_s=[12.]).points[0]
    assert point.governing_heading_deg == 0.
    with pytest.raises(ValueError, match='heading'):
        operation_envelope(full_rao(), 'transit', headings=[270.], tp_range_s=[12.])


def test_zero_or_nonfinite_spectrum_is_rejected(monkeypatch):
    module = importlib.import_module('digitalmodel.marine_ops.operation_envelope')
    for bad in (0., np.nan):
        monkeypatch.setattr(module.WaveSpectra, 'jonswap',
                            lambda *a, **k: (np.linspace(.01, 15., 4097), np.full(4097, bad)))
        with pytest.raises(ValueError, match='spectrum'):
            operation_envelope(full_rao(), 'transit', tp_range_s=[12.])


def test_nonfinite_response_moment_rejected():
    module = importlib.import_module('digitalmodel.marine_ops.operation_envelope')
    with pytest.raises(ValueError, match='moment'):
        module._significant_response(np.array([np.nan, 1.]), np.array([1., 2.]), np.ones(2))


def test_short_period_low_pass_hand_integral():
    # Eq. B3: F(wp)=exp(-1.25). Eq. B2: H=5.4/sqrt(F(wp)/F(8wp)).
    rao = full_rao()
    wp = np.pi / 2
    frequencies = np.array([.001, .1, wp, wp + 1e-6, 5., 15.])
    for name in ('surge', 'sway', 'heave', 'roll', 'pitch', 'yaw'):
        component = getattr(rao, name)
        component.frequencies.values = frequencies.copy()
        component.magnitude[:] = 0.
    rao.heave.magnitude[:3, :] = 1.
    point = operation_envelope(rao, 'transit', gamma=1., tp_range_s=[4.]).points[0]
    assert point.hs_limit_m == pytest.approx(10.0873, rel=1e-2)
    assert point.captured_variance_fraction == pytest.approx(.99969487, abs=1e-5)


def test_forecast_sidecar_and_catalogue_units():
    limits = [limit for operation in OPERATIONS.values() for limit in operation.limits]
    assert len(limits) == 12
    assert all(limit.unit == ('m' if limit.dof in ('surge', 'sway', 'heave') else 'deg')
               for limit in limits)
    result = operation_envelope(full_rao(), 'transit', tp_range_s=[12.])
    data = result.to_dict()
    assert data['criterion_type'] == 'forecast_limit'
    assert data['alpha_source']['status'] == 'unverified_screening'
    assert data['alpha_source']['table_reference'] is None
    assert data['citation_sidecar']['limits_status'] == 'unverified_screening'
    assert data['citation_sidecar']['equations'] == ['B1', 'B2', 'B3', 'B4', 'B5']
    assert data['points'][0]['hs_forecast_limit_m'] == pytest.approx(5.4)


def test_consumers_reject_unevaluated_point(monkeypatch, tmp_path):
    module = importlib.import_module('digitalmodel.marine_ops.operation_envelope')
    from digitalmodel.installation.installation_pamphlet import calculations
    result = module.EnvelopeResult('synthetic', 'transit', .9,
                                  [module.EnvelopePoint(12., 20., 'none', float('nan'))])
    with pytest.raises(ValueError, match='evaluated'):
        result.to_dict()
    with pytest.raises(ValueError, match='evaluated'):
        result.hs_limit_at(12.)
    with pytest.raises(ValueError, match='evaluated'):
        module.plot_envelopes({'transit': result}, tmp_path / 'invalid.png')
    monkeypatch.setattr(calculations, '_build_rao_set', lambda *a: (full_rao(), False))
    monkeypatch.setattr(module, 'operation_envelope', lambda *a, **k: result)
    with pytest.raises(ValueError, match='evaluated'):
        calculations.compute_envelopes('synthetic', ['transit'], [12.])
