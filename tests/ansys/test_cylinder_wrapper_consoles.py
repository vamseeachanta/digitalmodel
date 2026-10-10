"""Synthetic exact console supplements; no live process or solver calls."""
import base64
from copy import deepcopy
import pytest

from digitalmodel.ansys.analysis_records import canonical_bytes
from digitalmodel.ansys.cylinder_cfd_owner_evidence import resolve_owner_evidence
from digitalmodel.ansys.cylinder_process_inventory import classify_process_inventory
from tests.ansys.test_cylinder_cfd_owner_evidence import facts, assemble, process, sha


def forwarder(parent, child):
    return dict(parent_pid=parent, parent_creation_time=str(parent),
        child_pid=child, child_creation_time=str(child), launcher_sha256='a'*64,
        resource_kind_hex='02', embedded_target_literal='C:/alias/python.exe',
        target_alias_path='C:/alias/python.exe', resolved_target_path='C:/bin/python.exe',
        resolved_target_sha256='a'*64, alias_identity=dict(junction_path='C:/alias',
            device=1, inode=2, raw_link_target='C:\\bin'))


@pytest.fixture
def supplemented(facts):
    operation, binding, read, calls = assemble(facts)
    binding['forwarders'] = [forwarder(1, 2), forwarder(3, 4)]
    binding['processes'] += [process(200, 1, 'conhost.exe'), process(201, 3, 'conhost.exe')]
    binding['console_helpers'] += [200, 201]
    snapshot = dict(schema='process-snapshot-2', host='synthetic', observed_at='1000',
        enumeration_complete=True, rows=deepcopy(binding['processes']), errors=[],
        forwarders=deepcopy(binding['forwarders']), coverage=dict(
            selector='ansys-mpi-lineage-v1', enumerated_count=200, selected_count=128,
            excluded_count=72, selected_details_complete=True))
    raw = canonical_bytes(snapshot)
    facts[0]['C:/supplement.json'] = raw
    reference = dict(id='existing-discovery.json', sha256=sha(raw))
    operation['cfd_wrapper_console_evidence'] = dict(reference, path='C:/supplement.json')
    binding['wrapper_console_supplement'] = dict(reference, pids=[200, 201])
    return operation, binding, snapshot, read, calls, facts[0]


def classify(snapshot, binding):
    return classify_process_inventory(snapshot, expected_host='synthetic',
        cfd_binding=binding, now='1001', maximum_age_seconds='30')


def test_reported_console_set_is_observed_not_copied_from_declaration(supplemented):
    _, binding, snapshot, _, _, _ = supplemented
    row = next(r for r in snapshot['rows'] if r['pid'] == 200)
    row['parent_pid'] = 99
    result = classify(snapshot, binding)
    assert result['status'] == 'UNKNOWN'
    assert result['wrapper_console_pids'] == [201]


def test_two_consoles_preserve_original_owner_checks_and_full_classification(supplemented):
    operation, binding, snapshot, read, calls, files = supplemented
    before = deepcopy(binding)
    owner = resolve_owner_evidence(operation, binding, read)
    result = classify(snapshot, binding)
    assert owner['wrapper_console_pids'] == result['wrapper_console_pids'] == [200, 201]
    assert owner['matched_joins']['execution_rows'] == 122
    assert base64.b64decode(owner['wrapper_console_evidence']['raw_base64']) == files['C:/supplement.json']
    assert result['status'] == 'CLEAR' and len(result['dispositions']) == 128
    assert binding == before
    assert not any('controller-state' in name for name in calls)


@pytest.mark.parametrize('fault', ['no_operation', 'no_binding', 'reference', 'third', 'duplicate', 'extra_key'])
def test_missing_or_malformed_supplement_refuses(supplemented, fault):
    operation, binding, _, read, _, _ = supplemented
    if fault == 'no_operation': operation.pop('cfd_wrapper_console_evidence')
    elif fault == 'no_binding': binding.pop('wrapper_console_supplement')
    elif fault == 'reference': operation['cfd_wrapper_console_evidence']['id'] = 'other'
    elif fault == 'third': binding['wrapper_console_supplement']['pids'].append(202)
    elif fault == 'duplicate': binding['wrapper_console_supplement']['pids'] = [200, 200]
    else: binding['wrapper_console_supplement']['extra'] = True
    with pytest.raises(ValueError): resolve_owner_evidence(operation, binding, read)


@pytest.mark.parametrize('field,value', [('creation_time', '199'), ('parent_pid', 3),
    ('name', 'other.exe'), ('executable_path', 'C:/other/conhost.exe'),
    ('executable_resolved_path', 'C:/other/conhost.exe'), ('executable_sha256', 'b'*64),
    ('argv', ['other']), ('cwd', 'C:/other'), ('script_sources', [])])
def test_complete_supplement_rows_are_pinned(supplemented, field, value):
    operation, binding, _, read, _, _ = supplemented
    row = next(r for r in binding['processes'] if r['pid'] == 200)
    if field == 'script_sources':
        value = [dict(argv_index=1, path='C:/bad.py', sha256='b'*64,
                      resolution_basis='absolute_argument')]
    row[field] = value
    with pytest.raises(ValueError): resolve_owner_evidence(operation, binding, read)


def test_unlisted_live_child_remains_unknown(supplemented):
    _, binding, snapshot, _, _, _ = supplemented
    snapshot['rows'].append(process(202, 1, 'conhost.exe'))
    snapshot['coverage'].update(selected_count=129, excluded_count=71)
    result = classify(snapshot, binding)
    assert result['status'] == 'UNKNOWN'
    assert all(r['classification'] != 'PRESERVED_CFD' for r in result['dispositions'])


def test_every_owner_call_rechecks_supplement(supplemented):
    operation, binding, _, read, _, files = supplemented
    resolve_owner_evidence(operation, binding, read)
    files['C:/supplement.json'] += b' '
    with pytest.raises(ValueError): resolve_owner_evidence(operation, binding, read)
