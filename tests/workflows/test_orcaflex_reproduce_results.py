"""Fail-closed channel extraction tests; no solver or licence required."""
from types import SimpleNamespace as NS
import csv

import numpy as np
import pytest

from digitalmodel.workflows import orcaflex_reproduce_results as results


@pytest.fixture
def case(monkeypatch):
    state = NS(times=[0., 1., 2.], values=[3., 4., 5.], units='kN',
               graph=NS(X=[0., 0.5], Min=[1., 2.], Max=[3., 4.], Mean=[2., 3.]))
    obj = NS(varDetails=lambda *a, **k: [NS(VarName='Effective tension', VarUnits=state.units)])
    class Model(dict):
        general = NS(UnitsSystem='SI')
    model = Model(Wire=obj)
    deps = NS(api=NS(ResultType=NS(TimeHistory=0, RangeGraph=1), oeEndA='A', oeEndB='B'),
              ts=NS(get_time_series_from_orcaflex_run=lambda *a: (state.values, state.times)),
              rg=NS(get_RangeGraph=lambda *a: state.graph),
              objects=NS(get_TimePeriodObject=lambda p: p))
    monkeypatch.setattr(results, '_extraction_dependencies', lambda: deps)
    config = {'period': [0., 2.], 'title': 'Diagnostic',
              'time_histories': [{'object': 'Wire', 'variable': 'Effective tension', 'position': 'End A', 'units': 'kN'}],
              'range_graphs': [{'object': 'Wire', 'variable': 'Effective tension', 'units': 'kN'}]}
    metadata = {'run_id': 'test', 'model_sha256': 'a'*64, 'solver_version': 'mock',
                'simulation_start': 0., 'simulation_stop': 2., 'limitations': ['Diagnostic only']}
    return model, config, metadata, state


def test_success_preserves_channels_and_offline_report(case, tmp_path):
    model, cfg, meta, _ = case
    receipt = results.extract_results(model, cfg, tmp_path, meta)
    assert receipt['status'] == 'complete'
    assert receipt['time_history_count'] == receipt['range_graph_count'] == 1
    with open(tmp_path / 'range_000.csv', newline='') as stream:
        rows = list(csv.DictReader(stream))
    assert rows[1]['arc_length_m'] == '0.5'
    assert rows[1]['Mean [kN]'] == '3.0'
    html = (tmp_path / 'report.html').read_text(encoding='utf-8')
    assert 'Diagnostic only' in html and 'Not evaluated' in html
    assert '<script src="https://cdn.plot.ly/' not in html


@pytest.mark.parametrize('fault', ['empty', 'nan', 'length', 'time', 'units', 'missing', 'range', 'bounds'])
def test_bad_channel_fails_without_success_report(case, tmp_path, fault):
    model, cfg, meta, state = case
    if fault == 'empty': state.values = []
    if fault == 'nan': state.values[1] = np.nan
    if fault == 'length': state.values.pop()
    if fault == 'time': state.times = [0., 0., 2.]
    if fault == 'units': state.units = 'lbf'
    if fault == 'missing': model.clear()
    if fault == 'range': state.graph.Mean = [1.]
    if fault == 'bounds': state.times = [0., 1., 1.9]
    with pytest.raises((ValueError, KeyError)):
        results.extract_results(model, cfg, tmp_path, meta)
    assert not (tmp_path / 'report.html').exists()


@pytest.mark.parametrize('key', ['time_histories', 'range_graphs'])
def test_empty_contract_rejected(case, tmp_path, key):
    model, cfg, meta, _ = case
    cfg[key] = []
    with pytest.raises(ValueError):
        results.extract_results(model, cfg, tmp_path, meta)


@pytest.mark.parametrize('fault', ['non_si', 'unknown_si', 'mean', 'negative_arc', 'duplicate'])
def test_basis_and_envelope_contract(case, tmp_path, fault):
    model, cfg, meta, state = case
    if fault == 'non_si': model.general.UnitsSystem = 'US'
    if fault == 'unknown_si': model.general = None
    if fault == 'mean': state.graph.Mean = [99., 99.]
    if fault == 'negative_arc': state.graph.X = [-1., 1.]
    if fault == 'duplicate': cfg['time_histories'] *= 2
    with pytest.raises(ValueError):
        results.extract_results(model, cfg, tmp_path, meta)


def test_constant_history_retains_true_zero(case, tmp_path):
    model, cfg, meta, state = case
    state.values = [0., 0., 0.]
    receipt = results.extract_results(model, cfg, tmp_path, meta)
    assert receipt['time_histories'][0]['statistics']['max_val'] == 0.
    assert receipt['time_histories'][0]['statistics']['skewness'] is None


def test_report_exposes_every_solver_warning(case, tmp_path):
    model, cfg, meta, _ = case
    meta['warnings'] = ['Euler limit exceeded: Sling#5', 'Torsion: JumperLine', 'Lay azimuth: Connector2']
    results.extract_results(model, cfg, tmp_path, meta)
    html = (tmp_path / 'report.html').read_text(encoding='utf-8')
    assert 'Solver warnings' in html
    for warning in meta['warnings']:
        assert warning in html


def test_renderer_error_cannot_be_reported_as_success(case, tmp_path, monkeypatch):
    from digitalmodel.solvers.orcaflex import reporting
    def broken_report(report, path, **kwargs):
        path.write_text('Demand-only diagnostic. Error rendering section', encoding='utf-8')
        return path
    monkeypatch.setattr(reporting, 'generate_orcaflex_report', broken_report)
    model, cfg, meta, _ = case
    with pytest.raises(ValueError, match='Report'):
        results.extract_results(model, cfg, tmp_path, meta)
