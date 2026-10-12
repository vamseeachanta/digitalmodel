"""Branch-bound supplemental consoles; synthetic process evidence only."""
from copy import deepcopy

import pytest

from digitalmodel.ansys.analysis_records import canonical_bytes
from digitalmodel.ansys.cylinder_cfd_owner_evidence import resolve_owner_evidence
from digitalmodel.ansys.cylinder_process_inventory import classify_process_inventory
from digitalmodel.ansys.cylinder_wrapper_consoles import console_pids
from tests.ansys.test_cylinder_cfd_owner_evidence import (
    facts as _facts_fixture,
    process,
    sha,
)
from tests.ansys.test_cylinder_wrapper_consoles import (
    supplemented as _supplemented_fixture,
)

facts = _facts_fixture
supplemented = _supplemented_fixture


def _classify(snapshot, binding):
    return classify_process_inventory(
        snapshot, expected_host='synthetic', cfd_binding=binding,
        now='1001', maximum_age_seconds='30')


def _attach(bundle, parents):
    operation, binding, snapshot, _, _, files = bundle
    for pid, parent in zip((200, 201), parents):
        next(row for row in binding['processes'] if row['pid'] == pid)['parent_pid'] = parent
        next(row for row in snapshot['rows'] if row['pid'] == pid)['parent_pid'] = parent
    raw = canonical_bytes(snapshot)
    files['C:/supplement.json'] = raw
    digest = sha(raw)
    operation['cfd_wrapper_console_evidence']['sha256'] = digest
    binding['wrapper_console_supplement']['sha256'] = digest


@pytest.mark.parametrize('parents', [(1, 3), (2, 4), (2, 3)])
def test_one_console_at_either_endpoint_of_each_branch_is_accepted(
        supplemented, parents):
    _attach(supplemented, parents)
    operation, binding, snapshot, read, _, _ = supplemented
    owner = resolve_owner_evidence(operation, binding, read)
    result = _classify(snapshot, binding)
    assert owner['wrapper_console_pids'] == [200, 201]
    assert result['wrapper_console_pids'] == [200, 201]
    assert result['status'] == 'CLEAR'
    assert len(result['dispositions']) == 128


@pytest.mark.parametrize('parents', [(1, 2), (99, 3)])
def test_duplicate_branch_or_unpaired_parent_refuses(supplemented, parents):
    _attach(supplemented, parents)
    operation, binding, snapshot, read, _, _ = supplemented
    with pytest.raises(ValueError):
        resolve_owner_evidence(operation, binding, read)
    with pytest.raises(ValueError):
        _classify(snapshot, binding)


def test_overlapping_forwarder_pairs_refuse():
    pins = {
        20: process(20, 2, 'conhost.exe'),
        21: process(21, 3, 'conhost.exe'),
    }
    binding = dict(console_helpers=[20, 21], wrapper_console_supplement=dict(
        id='evidence', sha256='a' * 64, pids=[20, 21]))
    forwarders = [dict(parent_pid=1, child_pid=2),
                  dict(parent_pid=2, child_pid=3)]
    with pytest.raises(ValueError, match='disjoint'):
        console_pids(binding, pins, forwarders)


def test_third_endpoint_console_refuses(supplemented):
    _, binding, snapshot, _, _, _ = supplemented
    extra = process(202, 2, 'conhost.exe')
    binding['processes'].append(deepcopy(extra))
    binding['console_helpers'].append(202)
    snapshot['rows'].append(extra)
    snapshot['coverage'].update(selected_count=129, excluded_count=71)
    with pytest.raises(ValueError):
        _classify(snapshot, binding)


@pytest.mark.parametrize('mutation', ['source', 'coverage'])
def test_changed_console_or_incomplete_coverage_refuses(supplemented, mutation):
    _, binding, snapshot, _, _, _ = supplemented
    if mutation == 'source':
        row = next(row for row in snapshot['rows'] if row['pid'] == 200)
        row['script_sources'] = [dict(
            argv_index=1, path='C:/bad.py', sha256='b' * 64,
            resolution_basis='absolute_argument')]
        row['argv'].append('C:/bad.py')
    else:
        snapshot['coverage']['selected_count'] = 127
    if mutation == 'source':
        result = _classify(snapshot, binding)
        assert result['status'] == 'UNKNOWN'
        assert result['wrapper_console_pids'] == [201]
        assert not any(row['classification'] == 'PRESERVED_CFD'
                       for row in result['dispositions'])
    else:
        with pytest.raises(ValueError):
            _classify(snapshot, binding)
