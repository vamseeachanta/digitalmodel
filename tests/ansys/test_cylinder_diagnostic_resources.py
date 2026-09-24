"""Synthetic operational admission observations; no licence checkout or native run."""
import copy
from pathlib import Path
from decimal import localcontext, DefaultContext, ROUND_UP

import pytest

from digitalmodel.ansys.cylinder_diagnostic_resources import (
    parse_compatible_license, validate_capacity, validate_environment,
)


ENVIRONMENT = {'ANSYS261_PRODUCT': 'ansys', 'ANS_CONSEC': 'YES'}


@pytest.fixture
def capacity():
    return {'samples': [dict(observed_at=str(1001 + index), interval_seconds='1',
                            logical_processors=64, cpu_percent='90',
                            available_memory_bytes=8 * 1024 ** 3)
                        for index in range(5)]}


def test_capacity_meets_declared_operational_thresholds(capacity):
    before = copy.deepcopy(capacity)
    result = validate_capacity(capacity, now='1006', requested_cores=1)
    assert result['status'] == 'PASS'
    assert result['minimum_idle_core_equivalents'] == '6.4'
    assert result['minimum_available_memory_bytes'] == 8 * 1024 ** 3
    assert result['required_idle_core_equivalents'] == '2'
    assert capacity == before


@pytest.mark.parametrize('field,value', [
    ('cpu_percent', '97'), ('cpu_percent', 'NaN'), ('cpu_percent', '-1'),
    ('cpu_percent', '101'), ('logical_processors', True),
    ('available_memory_bytes', 8 * 1024 ** 3 - 1),
    ('available_memory_bytes', True), ('interval_seconds', '0.9'),
    ('interval_seconds', '3'), ('observed_at', '999'),
])
def test_any_sample_failure_refuses_before_consumption(capacity, field, value):
    capacity['samples'][2][field] = value
    with pytest.raises(ValueError):
        validate_capacity(capacity, now='1006', requested_cores=1)


@pytest.mark.parametrize('now', ['1036', '1004', 'NaN', 'Infinity'])
def test_capacity_requires_fresh_finite_nonfuture_window(capacity, now):
    with pytest.raises(ValueError):
        validate_capacity(capacity, now=now, requested_cores=1)


@pytest.mark.parametrize('damage', ['missing', 'extra', 'core_bool', 'two_cores'])
def test_fixed_sample_count_and_single_core_profile(capacity, damage):
    cores = 1
    if damage == 'missing':
        capacity['samples'].pop()
    elif damage == 'extra':
        capacity['samples'].append(copy.deepcopy(capacity['samples'][-1]))
    else:
        cores = True if damage == 'core_bool' else 2
    with pytest.raises(ValueError):
        validate_capacity(capacity, now='1006', requested_cores=cores)


def test_environment_checks_actual_inherited_values_without_mutation():
    inherited = {**ENVIRONMENT, 'UNRELATED': 'preserved'}
    before = copy.deepcopy(inherited)
    assert validate_environment({'launch_environment': ENVIRONMENT}, inherited) == ENVIRONMENT
    assert inherited == before


@pytest.mark.parametrize('key', list(ENVIRONMENT))
@pytest.mark.parametrize('damage', ['missing', 'changed', 'unbound'])
def test_environment_drift_or_config_substitution_refuses(key, damage):
    expected, inherited = copy.deepcopy(ENVIRONMENT), copy.deepcopy(ENVIRONMENT)
    if damage == 'missing':
        del inherited[key]
    elif damage == 'changed':
        inherited[key] = 'different'
    else:
        expected[key] = inherited[key] = 'different'
    with pytest.raises(ValueError):
        validate_environment({'launch_environment': expected}, inherited)


def increment(version, issued=1, used=0, queued=0, reserved=0):
    return (f'Feature "ansys" v{version}, vendor: ansyslmd, expiry: permanent(no expiration date) '
            f'(Total of {issued} license issued;  Total of {used} floating non-reserved licenses in use)\n'
            f'    (Total of {queued} users queued;  Total of {reserved} licenses reserved)\n')


def test_old_increment_excluded_without_claiming_vendor_checkout():
    raw = (increment('2017.1031') + increment('2027.0114')).encode()
    result = parse_compatible_license(raw, feature='ansys', minimum_version='2026.0202')
    assert result['compatible_available'] == 1
    assert result['candidate_versions'] == ['2027.0114']
    assert result['checkout_performed'] is False


def test_retained_observed_vendor_increment_layout():
    path = Path(__file__).parent / 'fixtures/lmstat_ansys_increment_layout.txt'
    result = parse_compatible_license(path.read_bytes(), feature='ansys',
                                      minimum_version='2026.0202')
    assert result['compatible_available'] == 1
    assert result['candidate_versions'] == ['2027.0114']
    assert result['checkout_performed'] is False


def test_capacity_boundary_is_independent_of_ambient_precision(capacity):
    for row in capacity['samples']:
        row.update(logical_processors=4, cpu_percent='50.0001')
    with localcontext() as context:
        context.prec = 2
        with pytest.raises(ValueError):
            validate_capacity(capacity, now='1006', requested_cores=1)


def test_capacity_is_independent_of_mutable_default_context(capacity):
    saved = DefaultContext.copy()
    try:
        DefaultContext.prec, DefaultContext.Emax = 2, 2
        DefaultContext.rounding = ROUND_UP
        result = validate_capacity(capacity, now='1006', requested_cores=1)
        assert result['minimum_idle_core_equivalents'] == '6.4'
    finally:
        for field in ('prec', 'Emin', 'Emax', 'rounding', 'capitals', 'clamp'):
            setattr(DefaultContext, field, getattr(saved, field))
        DefaultContext.flags, DefaultContext.traps = saved.flags, saved.traps


def test_incomplete_feature_cannot_borrow_other_feature_totals():
    raw = (increment('2027.0114').splitlines()[0] + '\n'
           + 'Feature "unrelated" v2027.0114\n'
           + increment('2027.0114').splitlines()[1]).encode()
    with pytest.raises(ValueError):
        parse_compatible_license(raw, feature='ansys', minimum_version='2026.0202')


@pytest.mark.parametrize('raw', [b'', b'license server unavailable',
    increment('2017.1031').encode(), increment('2027.0114', used=1).encode(),
    increment('2027.0114', queued=1).encode(), increment('2027.0114', reserved=1).encode(),
    increment('2027.0114').split('\n')[0].encode(),
    increment('2027.0114').replace('permanent(no expiration date)', 'unknown').encode(),
])
def test_missing_busy_queued_or_unparsed_licence_evidence_refuses(raw):
    with pytest.raises(ValueError):
        parse_compatible_license(raw, feature='ansys', minimum_version='2026.0202')
