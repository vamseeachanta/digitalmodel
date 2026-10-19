"""Mutation checks for independent numerical and unit gates."""
from dataclasses import replace
import json

import pytest

from digitalmodel.marine_ops.operation_envelope import OPERATIONS, MotionLimit, operation_envelope
from digitalmodel.marine_ops.envelope_numerics import heading_column
from tests.marine_ops import test_operation_envelope as examples
from tests.marine_ops.test_envelope_validation import (
    full_rao, test_forecast_sidecar_and_catalogue_units as check_catalogue,
)


def test_plot_oracle_detects_lost_heave(monkeypatch, tmp_path):
    original = examples._rao_set
    def without_heave(**kwargs):
        kwargs['heave'] = 0.
        return original(**kwargs)
    monkeypatch.setattr(examples, '_rao_set', without_heave)
    with pytest.raises(AssertionError):
        examples.test_envelope_for_all_operations_and_plot(tmp_path)


@pytest.mark.parametrize('change', ['alpha', 'limit'])
def test_serialization_oracle_detects_changed_criterion(monkeypatch, change):
    operation = OPERATIONS['dp_station_keeping']
    altered = (replace(operation, alpha=.8) if change == 'alpha' else
               replace(operation, limits=(MotionLimit('heave', 2.2, 'm'), *operation.limits[1:])))
    monkeypatch.setitem(OPERATIONS, operation.key, altered)
    with pytest.raises(AssertionError):
        examples.test_envelope_to_dict_roundtrips_points()


def test_catalogue_unit_gate_detects_mutation(monkeypatch):
    operation = OPERATIONS['transit']
    altered = replace(operation, limits=(MotionLimit('heave', 3., 'cm'), *operation.limits[1:]))
    monkeypatch.setitem(OPERATIONS, operation.key, altered)
    with pytest.raises(AssertionError):
        check_catalogue()


def test_heading_tolerance_includes_wrap_step():
    with pytest.raises(ValueError, match='heading'):
        heading_column([0., 100., 200., 350.], 8.)


def test_valid_zero_response_is_explicitly_capped_and_json_finite():
    rao = full_rao()
    rao.heave.magnitude[:] = 0.
    result = operation_envelope(rao, 'transit', tp_range_s=[12.])
    point = result.points[0]
    assert point.evaluated
    assert point.status == 'capped'
    assert point.hs_forecast_limit_m == 20.
    assert point.governing_dof == 'cap'
    assert point.governing_heading_deg is None
    json.dumps(result.to_dict(), allow_nan=False)


@pytest.mark.parametrize('gamma', [1., 3.3, 7.])
def test_short_period_captured_variance(gamma):
    point = operation_envelope(full_rao(), 'transit', tp_range_s=[4.], gamma=gamma).points[0]
    assert .99 <= point.captured_variance_fraction <= 1.
    assert point.hs_limit_m == pytest.approx(5.4, rel=1e-3)
