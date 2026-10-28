# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline holding example checked against independent arithmetic and histories."""

import json
import runpy
import socket
from pathlib import Path

import pytest


EXAMPLE = (Path(__file__).resolve().parents[2] /
           'examples/domains/geotechnical/jackup_preload_holding.py')


def test_three_rates_capacity_order_and_existing_sequence_screen():
    result = runpy.run_path(str(EXAMPLE))['run_example']()
    rapid, medium, slow = result['cases']
    # Synthetic assumptions: su0 = .25*80=20 kPa; circular reference
    # Q0 = 6.34*20*160=20288 kN. Applied load is 18000 kN per leg.
    # delta sigma=.25*18000/160=28.125 kPa; s_inf=.001*4*28.125=.1125 m.
    # Sources: FHWA (2006) 5.5.6.1; MIT (2004) V-2 1.1-1.2;
    # circular factor: research equation 5 cited in geotechnical.spudcan.
    first = rapid['responses'][0]
    assert first['capacity_kn'] == pytest.approx(20288)
    assert first['margin_ratio'] == pytest.approx(20288 / 18000 - 1)
    assert first['settlement_m'] == 0
    assert rapid['load_rate_kn_h'] == 180000
    assert medium['load_rate_kn_h'] == 750
    assert slow['load_rate_kn_h'] == 75
    capacities = [case['responses'][0]['capacity_kn'] for case in result['cases']]
    assert capacities == sorted(capacities)
    assert len(set(capacities)) == 3
    for case in result['cases']:
        rows = case['responses']
        assert all(0 <= row['settlement_m'] <= 0.1125 for row in rows)
        assert all(a['settlement_m'] <= b['settlement_m'] for a, b in zip(rows, rows[1:]))
        assert all(a['capacity_kn'] <= b['capacity_kn'] for a, b in zip(rows, rows[1:]))
        screen = case['sequence_screen']
        assert screen['stage_order'] == ('preparation', 'preload', 'holding', 'discharge')
        assert screen['hold_met']
        assert screen['settlement_rate_m_h'] < 0.001
        assert screen['hold_duration_h'] == pytest.approx(case['scheduled_hold_h'])
        assert case['scheduled_hold_h'] >= case['recommendation']['holding_time_h']
        assert case['ramp_capacity_supported']
    assert rapid['recommendation']['holding_time_h'] > slow['recommendation']['holding_time_h']


def test_outputs_are_local_plotly_specs_next_to_results(tmp_path, monkeypatch):
    def no_network(*args, **kwargs):
        pytest.fail('offline example attempted network access')

    monkeypatch.setattr(socket, 'create_connection', no_network)
    monkeypatch.setattr(socket.socket, 'connect', no_network)
    namespace = runpy.run_path(str(EXAMPLE))
    namespace['write_example'](tmp_path)
    result = json.loads((tmp_path / 'results.json').read_text())
    assert len(result['cases']) == 3
    assert result['standards']['sname_holding_clause'] == 'clause: TO CONFIRM'
    assert result['assumption_basis'] == 'assumed, based on previous experience'
    for name in ('settlement', 'capacity'):
        spec = json.loads((tmp_path / f'{name}.figure.json').read_text())
        assert len(spec['data']) == 3
        assert spec['layout']['xaxis']['title']['text'] == 'Holding time (h)'
        assert spec['layout']['xaxis']['type'] == 'linear'
        assert spec['layout']['meta']['standard_compliance'] == 'NOT_ESTABLISHED'
        for trace, case in zip(spec['data'], result['cases']):
            assert trace['type'] == 'scatter'
            assert trace['x'][0] == 0
            assert len(trace['x']) == len(trace['y']) == len(case['responses'])
            assert all(a < b for a, b in zip(trace['x'], trace['x'][1:]))
        # Independent plotted-unit conversion, without a production plotting helper.
        if name == 'settlement':
            assert spec['data'][0]['y'][-1] == pytest.approx(
                result['cases'][0]['responses'][-1]['settlement_m'] * 1000)
        else:
            assert spec['data'][0]['y'][0] == pytest.approx(20.288)
