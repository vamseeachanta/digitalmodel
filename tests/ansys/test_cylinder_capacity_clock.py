"""Synthetic clock tests plus retained read-only observation reproduction."""
from decimal import localcontext
from types import SimpleNamespace

import pytest

from digitalmodel.ansys import cylinder_diagnostic_preflight as proposal
from digitalmodel.ansys.cylinder_diagnostic_resources import validate_capacity


# Retained capacity-only observation; original private receipt SHA256
# 0482dbaec08aa8e90862e79ea6c7663a289428fb0f1ab108cbabfd5dac846d7d
CAPACITY_CAPTURE = {'samples': [{'observed_at': '1789424566.7185044', 'interval_seconds': '1.0', 'logical_processors': 64, 'cpu_percent': '93.7', 'available_memory_bytes': 217434517504}, {'observed_at': '1789424567.7321463', 'interval_seconds': '1.0160000000614673', 'logical_processors': 64, 'cpu_percent': '72.4', 'available_memory_bytes': 217838555136}, {'observed_at': '1789424568.7492228', 'interval_seconds': '1.0150000001303852', 'logical_processors': 64, 'cpu_percent': '90.7', 'available_memory_bytes': 219426021376}, {'observed_at': '1789424569.7671833', 'interval_seconds': '1.0159999998286366', 'logical_processors': 64, 'cpu_percent': '92.3', 'available_memory_bytes': 218520793088}, {'observed_at': '1789424570.7943552', 'interval_seconds': '1.0309999999590218', 'logical_processors': 64, 'cpu_percent': '92.4', 'available_memory_bytes': 218467901440}]}


def retained_capacity():
    return CAPACITY_CAPTURE


def install_clocks(monkeypatch, target, pairs):
    ticks = iter(number for pair in pairs for number in pair)
    monkeypatch.setattr(target.time, 'time_ns', lambda: next(ticks))
    monkeypatch.setattr(target.psutil, 'cpu_percent', lambda interval: 90.0)
    monkeypatch.setattr(target.psutil, 'cpu_count', lambda: 64)
    monkeypatch.setattr(target.psutil, 'virtual_memory',
                        lambda: SimpleNamespace(available=16*1024**3))


def pairs():
    start = 1789424565000000000
    return [(start+i*1015000000, start+i*1015000000+1013641900) for i in range(5)]


def test_real_retained_observation_refuses_only_incoherent_clock_window():
    capacity = retained_capacity()
    assert all(int(r['available_memory_bytes']) >= 8*1024**3 for r in capacity['samples'])
    with localcontext() as context:
        context.prec = 80
        from decimal import Decimal
        assert all(Decimal(r['logical_processors'])*(1-Decimal(r['cpu_percent'])/100) >= 2
                   for r in capacity['samples'])
        second = capacity['samples'][1]
        elapsed = Decimal(second['observed_at']) - Decimal(capacity['samples'][0]['observed_at'])
        assert elapsed == Decimal('1.0136419')
        assert elapsed < Decimal(second['interval_seconds'])
    with pytest.raises(ValueError, match='consecutive window'):
        validate_capacity(capacity, now=capacity['samples'][-1]['observed_at'])


def test_same_clock_collector_passes_unchanged_window_contract(monkeypatch):
    install_clocks(monkeypatch, proposal, pairs())
    captured = proposal._capacity()
    assert captured['samples'][0]['interval_seconds'] == '1.0136419'
    assert captured['samples'][0]['observed_at'] == '1789424566.0136419'
    assert validate_capacity(captured, now=captured['samples'][-1]['observed_at'])['status'] == 'PASS'


@pytest.mark.parametrize('between', [False, True])
def test_backward_clock_refuses(monkeypatch, between):
    sequence = pairs()
    if between:
        sequence[1] = (sequence[0][1]-1, sequence[0][1]+1013641899)
    else:
        sequence[0] = (sequence[0][0], sequence[0][0]-1)
    install_clocks(monkeypatch, proposal, sequence)
    with pytest.raises(ValueError, match='backward'):
        proposal._capacity()


def test_nanosecond_serialization_is_exact_under_low_decimal_precision(monkeypatch):
    install_clocks(monkeypatch, proposal, pairs())
    with localcontext() as context:
        context.prec = 2
        context.Emax = 2
        result = proposal._capacity()
    assert result['samples'][0]['observed_at'] == '1789424566.0136419'
    assert result['samples'][0]['interval_seconds'] == '1.0136419'


@pytest.mark.parametrize('fault', ['short', 'long', 'gap', 'idle', 'memory', 'stale'])
def test_existing_thresholds_still_refuse(monkeypatch, fault):
    install_clocks(monkeypatch, proposal, pairs())
    result = proposal._capacity()
    now = result['samples'][-1]['observed_at']
    if fault in ('short', 'long'):
        result['samples'][0]['interval_seconds'] = '0.999999999' if fault == 'short' else '2.000000001'
    elif fault == 'gap':
        result['samples'][-1]['observed_at'] = '1789424575'
        now = '1789424575'
    elif fault == 'idle':
        result['samples'][0]['cpu_percent'] = '99'
    elif fault == 'memory':
        result['samples'][0]['available_memory_bytes'] = 8*1024**3-1
    else:
        now = '1789424605'
    with pytest.raises(ValueError):
        validate_capacity(result, now=now)
