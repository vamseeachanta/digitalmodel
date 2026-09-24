"""RED-only contract for a pure process-family classifier; no process is launched.

API: classify_process_inventory(snapshot, *, expected_host, cfd_binding, now,
                               maximum_age_seconds) -> dict.
Times are finite decimal-string Unix seconds; an explicit positive freshness bound
is mandatory. Snapshot keys: schema='process-snapshot-1', host, observed_at,
enumeration_complete, coverage, rows. Complete means the initial host PID/name/path
enumeration succeeded, NOT that every Windows process has readable argv/image hash.
Rows select potential ANSYS/MPI families plus ancestors/descendants; selected rows
remain strict. Coverage declares selector and enumerated/selected/excluded counts.
An empty selection means no relevant processes, not an empty host. No System PID0
or unrelated protected-process command line/hash requirement is imposed.
Row keys are demonstrated in process() below.
CFD binding pins exact observed identities, image/argv/script provenance and roles;
optional wrappers and console_helpers have the same identity/provenance checks.
Every selected family descendant must have an explicit role; no conhost basename
exemption exists. Binding is supplied verified evidence, not authenticating itself.
Output: status CLEAR/CONFLICT/UNKNOWN, snapshot_sha256, raw_inventory,
dispositions, conflicts, unknowns, process_inventory. The legacy process_inventory
is a blocking-conflict projection (conflicts plus unresolved relevant unknowns),
NOT the raw observation; it cannot be interpreted as an empty host.
Direct ANSYS/mapdl images and the actual ansys/bin solver subtree conflict, not
every vendor directory. Unrelated licensingclient utilities are not selector
seeds. If a utility enters a selected solver/MPI family it remains UNKNOWN absent
separately reviewed evidence; no resident-process or basename exemption is added.
Malformed/stale evidence raises ValueError.
CLEAR means no competing process within this declared classifier scope only.
Capacity, cooperative reservation, license observation and engineering acceptance
remain separate gates. Fixtures are synthetic and establish no live permission.
"""
from copy import deepcopy
from decimal import DefaultContext, Overflow, ROUND_DOWN

import pytest

from digitalmodel.ansys.analysis_records import canonical_bytes, digest_bytes
from digitalmodel.ansys.cylinder_process_inventory import classify_process_inventory


def process(pid, parent, name, path, *, argv=None, scripts=None):
    return {'pid': pid, 'parent_pid': parent, 'creation_time': str(100 + pid),
            'name': name, 'executable_path': path, 'executable_sha256': 'a' * 64,
            'argv': argv or [path], 'script_sources': scripts or []}


def update_scope_counts(snapshot):
    """Keep synthetic scope coherent when a test deliberately changes row count."""
    selected = len(snapshot['rows'])
    snapshot['coverage']['selected_count'] = selected
    snapshot['coverage']['excluded_count'] = snapshot['coverage']['enumerated_count'] - selected


@pytest.fixture
def known_cfd():
    controller = 'C:/synthetic/controller.py'
    guard = 'C:/synthetic/guard.py'
    rows = [
        process(10, 0, 'python.exe', 'C:/synthetic/python.exe',
                argv=['C:/synthetic/python.exe', controller],
                scripts=[{'path': controller, 'sha256': 'b' * 64}]),
        process(11, 10, 'python.exe', 'C:/synthetic/python.exe',
                argv=['C:/synthetic/python.exe', guard, 'C:/synthetic/case.json'],
                scripts=[{'path': guard, 'sha256': 'c' * 64}]),
        process(12, 11, 'mpiexec.exe', 'C:/Microsoft MPI/mpiexec.exe',
                argv=['C:/Microsoft MPI/mpiexec.exe', '-n', '2',
                      'C:/OpenFOAM/interFoam.exe', '-case', 'C:/synthetic/case']),
        process(13, 12, 'smpd.exe', 'C:/Microsoft MPI/smpd.exe'),
        process(14, 13, 'interFoam.exe', 'C:/OpenFOAM/interFoam.exe'),
        process(15, 13, 'interFoam.exe', 'C:/OpenFOAM/interFoam.exe'),
    ]
    snapshot = {'schema': 'process-snapshot-1', 'host': 'synthetic-host',
                'observed_at': '1000', 'enumeration_complete': True, 'rows': rows,
                'coverage': {'selector': 'ansys-mpi-lineage-v1', 'enumerated_count': 500,
                             'selected_count': 6, 'excluded_count': 494,
                             'selected_details_complete': True}}
    binding = {'schema': 'cfd-process-binding-1', 'host': 'synthetic-host',
               'controller': 10, 'guard': 11,
               'mpi': 12, 'helper': 13, 'ranks': [14, 15], 'wrappers': [],
               'console_helpers': [], 'processes': deepcopy(rows)}
    return snapshot, binding


def classify(snapshot, binding=None, **overrides):
    options = {'expected_host': 'synthetic-host', 'cfd_binding': binding,
               'now': '1001', 'maximum_age_seconds': '5'}
    return classify_process_inventory(snapshot, **{**options, **overrides})


@pytest.fixture
def wrapped_cfd(known_cfd):
    snapshot, binding = known_cfd
    wrapper = process(16, 10, 'python.exe', 'C:/synthetic/venv/python.exe',
                      argv=['C:/synthetic/venv/python.exe', 'C:/synthetic/guard.py',
                            'C:/synthetic/case.json'],
                      scripts=[{'path': 'C:/synthetic/guard.py', 'sha256': 'c' * 64}])
    wrapper['creation_time'] = '110.5'
    snapshot['rows'][1]['parent_pid'] = 16
    snapshot['rows'] += [wrapper, process(17, 14, 'conhost.exe', 'C:/Windows/System32/conhost.exe'),
                         process(18, 15, 'conhost.exe', 'C:/Windows/System32/conhost.exe')]
    binding.update(wrappers=[16], console_helpers=[17, 18], processes=deepcopy(snapshot['rows']))
    update_scope_counts(snapshot)
    return snapshot, binding


def test_known_bound_cfd_preserves_raw_inventory_without_readiness_claim(known_cfd):
    snapshot, binding = known_cfd
    saved_snapshot, saved_binding = deepcopy(snapshot), deepcopy(binding)
    result = classify(snapshot, binding)
    assert result['status'] == 'CLEAR'
    assert result['raw_inventory'] == snapshot['rows']
    assert result['snapshot_sha256'] == digest_bytes(canonical_bytes(snapshot))
    assert result['conflicts'] == result['unknowns'] == []
    assert result['process_inventory'] == []  # No blockers, not an empty host.
    assert len(result['raw_inventory']) == 6
    assert {r['pid'] for r in result['dispositions']} == {r['pid'] for r in snapshot['rows']}
    assert 'ready_to_launch' not in result and 'capacity_verified' not in result
    assert snapshot == saved_snapshot and binding == saved_binding
    result['raw_inventory'][0]['name'] = 'changed-result-only'
    assert snapshot == saved_snapshot


def test_empty_relevant_selection_does_not_mean_empty_host(known_cfd):
    snapshot, _ = known_cfd
    snapshot['rows'] = []
    update_scope_counts(snapshot)
    result = classify(snapshot)
    assert result['status'] == 'CLEAR' and result['raw_inventory'] == []
    assert snapshot['coverage']['enumerated_count'] == 500


@pytest.mark.parametrize('name,path', [
    ('ANSYS261.exe', 'C:/synthetic/ANSYS261.exe'),
    ('ansys252.EXE', 'C:/synthetic/ansys252.EXE'),
    ('mapdl.exe', 'C:/synthetic/mapdl.exe'),
    ('renamed.exe', 'C:/Program Files/ANSYS Inc/v261/ansys/bin/winx64/renamed.exe'),
])
def test_direct_ansys_name_or_mapdl_path_conflicts(known_cfd, name, path):
    snapshot, binding = known_cfd
    snapshot['rows'].append(process(30, 0, name, path))
    update_scope_counts(snapshot)
    result = classify(snapshot, binding)
    assert result['status'] == 'CONFLICT'
    assert any(row['pid'] == 30 for row in result['conflicts'])
    assert result['raw_inventory'] == snapshot['rows']
    assert result['process_inventory'] == result['conflicts'] + result['unknowns']
    assert {r['pid'] for r in result['dispositions']} == {r['pid'] for r in snapshot['rows']}


def test_mixed_ansys_descendant_is_never_cfd_exempt(known_cfd):
    snapshot, binding = known_cfd
    snapshot['rows'].append(process(30, 13, 'ANSYS261.exe', 'C:/ANSYS/ANSYS261.exe'))
    update_scope_counts(snapshot)
    result = classify(snapshot, binding)
    assert result['status'] == 'CONFLICT'


def test_unknown_mpi_requires_disposition_even_when_known_cfd_exists(known_cfd):
    snapshot, binding = known_cfd
    snapshot['rows'].append(process(30, 0, 'mpiexec.exe', 'C:/other/mpiexec.exe'))
    update_scope_counts(snapshot)
    result = classify(snapshot, binding)
    assert result['status'] == 'UNKNOWN'
    assert any(row['pid'] == 30 for row in result['unknowns'])
    assert result['process_inventory'] == result['conflicts'] + result['unknowns']
    assert result['raw_inventory'] == snapshot['rows']


def test_mpi_basename_alone_cannot_establish_cfd(known_cfd):
    snapshot, _ = known_cfd
    assert classify(snapshot)['status'] == 'UNKNOWN'


@pytest.mark.parametrize('field,value', [
    ('creation_time', '112.0000001'), ('executable_path', 'C:/other/mpiexec.exe'),
    ('executable_sha256', 'd' * 64), ('argv', ['C:/Microsoft MPI/mpiexec.exe', '-n', '3']),
    ('parent_pid', 10),
])
def test_changed_mpi_identity_cannot_reuse_binding(known_cfd, field, value):
    snapshot, binding = known_cfd
    snapshot['rows'][2][field] = value
    assert classify(snapshot, binding)['status'] == 'UNKNOWN'


@pytest.mark.parametrize('missing_pid', [10, 11, 12, 13, 14, 15])
def test_missing_controller_helper_or_rank_prevents_exemption(known_cfd, missing_pid):
    snapshot, binding = known_cfd
    snapshot['rows'] = [r for r in snapshot['rows'] if r['pid'] != missing_pid]
    update_scope_counts(snapshot)
    assert classify(snapshot, binding)['status'] == 'UNKNOWN'


@pytest.mark.parametrize('name', ['interFoam.exe', 'python.exe', 'smpd.exe'])
def test_unaccounted_mpi_descendant_is_unknown(known_cfd, name):
    snapshot, binding = known_cfd
    snapshot['rows'].append(process(30, 13, name, 'C:/synthetic/' + name))
    update_scope_counts(snapshot)
    assert classify(snapshot, binding)['status'] == 'UNKNOWN'


@pytest.mark.parametrize('role_index', [0, 1])
def test_altered_controller_or_guard_script_hash_refuses_exemption(known_cfd, role_index):
    snapshot, binding = known_cfd
    snapshot['rows'][role_index]['script_sources'][0]['sha256'] = 'd' * 64
    assert classify(snapshot, binding)['status'] == 'UNKNOWN'


def test_binding_cannot_omit_controller_source_pins(known_cfd):
    snapshot, binding = known_cfd
    snapshot['rows'][0]['script_sources'] = []
    binding['processes'][0]['script_sources'] = []
    with pytest.raises(ValueError):
        classify(snapshot, binding)


@pytest.mark.parametrize('field,value', [('host', 'other-host'), ('enumeration_complete', False),
                                      ('observed_at', 'NaN'), ('observed_at', 'Infinity'),
                                      ('observed_at', float('nan')), ('observed_at', '900'),
                                      ('observed_at', '1002')])
def test_invalid_host_time_or_incomplete_snapshot_refuses(known_cfd, field, value):
    snapshot, binding = known_cfd
    snapshot[field] = value
    with pytest.raises(ValueError):
        classify(snapshot, binding)


@pytest.mark.parametrize('options', [{'now': 'NaN'}, {'maximum_age_seconds': '0'},
                                    {'maximum_age_seconds': '-1'}, {'maximum_age_seconds': 'Infinity'}])
def test_freshness_comparator_must_be_explicit_and_finite(known_cfd, options):
    snapshot, binding = known_cfd
    with pytest.raises(ValueError):
        classify(snapshot, binding, **options)


@pytest.mark.parametrize('fault', ['duplicate_pid', 'cycle', 'parent_created_after_child',
                                 'boolean_pid', 'nonfinite_creation'])
def test_contradictory_process_identities_refuse(known_cfd, fault):
    snapshot, binding = known_cfd
    if fault == 'duplicate_pid':
        snapshot['rows'].append(deepcopy(snapshot['rows'][2]))
        update_scope_counts(snapshot)
    elif fault == 'cycle':
        snapshot['rows'][0]['parent_pid'] = 15
    elif fault == 'parent_created_after_child':
        snapshot['rows'][1]['creation_time'] = '999'
    elif fault == 'boolean_pid':
        snapshot['rows'][0]['pid'] = True
    else:
        snapshot['rows'][2]['creation_time'] = 'NaN'
    with pytest.raises(ValueError):
        classify(snapshot, binding)


def test_pinned_wrappers_and_console_helpers_are_accounted(wrapped_cfd):
    snapshot, binding = wrapped_cfd
    result = classify(snapshot, binding)
    assert result['status'] == 'CLEAR'
    assert result['raw_inventory'] == snapshot['rows']
    assert {r['pid'] for r in result['dispositions']} == {r['pid'] for r in snapshot['rows']}


@pytest.mark.parametrize('pid,field,value', [
    (16, 'creation_time', '110.6'), (16, 'executable_sha256', 'd' * 64),
    (16, 'argv', ['C:/synthetic/venv/python.exe', 'C:/other.py']),
    (17, 'executable_sha256', 'd' * 64), (17, 'executable_path', 'C:/other/conhost.exe'),
    (17, 'creation_time', '117.1'), (17, 'parent_pid', 15),
])
def test_wrapper_and_console_identity_drift_is_unknown(wrapped_cfd, pid, field, value):
    snapshot, binding = wrapped_cfd
    next(r for r in snapshot['rows'] if r['pid'] == pid)[field] = value
    assert classify(snapshot, binding)['status'] == 'UNKNOWN'


def test_unpinned_conhost_is_not_automatically_exempt(wrapped_cfd):
    snapshot, binding = wrapped_cfd
    snapshot['rows'].append(process(19, 14, 'conhost.exe', 'C:/Windows/System32/conhost.exe'))
    update_scope_counts(snapshot)
    assert classify(snapshot, binding)['status'] == 'UNKNOWN'


@pytest.mark.parametrize('role', ['wrappers', 'console_helpers'])
def test_role_omission_cannot_hide_selected_descendants(wrapped_cfd, role):
    snapshot, binding = wrapped_cfd
    binding[role] = []
    with pytest.raises(ValueError):
        classify(snapshot, binding)


@pytest.mark.parametrize('field,value', [('selector', ''), ('selector', 'unreviewed-filter'),
                                      ('enumerated_count', 5), ('selected_count', 0),
                                      ('excluded_count', 0), ('selected_details_complete', False)])
def test_missing_or_contradictory_scope_coverage_refuses(known_cfd, field, value):
    snapshot, binding = known_cfd
    snapshot['coverage'][field] = value
    with pytest.raises(ValueError):
        classify(snapshot, binding)


@pytest.mark.parametrize('rank_pid', [14, 15])
@pytest.mark.parametrize('field,value', [
    ('pid', 31), ('creation_time', '115.5'),
    ('executable_sha256', 'd' * 64), ('executable_path', 'C:/other/interFoam.exe'),
    ('argv', ['C:/OpenFOAM/interFoam.exe', '-case', 'C:/different-case']),
])
def test_rank_identity_hash_and_argv_drift_is_unknown(known_cfd, rank_pid, field, value):
    snapshot, binding = known_cfd
    next(r for r in snapshot['rows'] if r['pid'] == rank_pid)[field] = value
    result = classify(snapshot, binding)
    assert result['status'] == 'UNKNOWN'
    assert result['unknowns']
    assert result['process_inventory'] == result['conflicts'] + result['unknowns']
    assert result['raw_inventory'] == snapshot['rows']


@pytest.mark.parametrize('host', ['different-host', '', None])
def test_cfd_binding_host_must_match_snapshot(known_cfd, host):
    snapshot, binding = known_cfd
    binding['host'] = host
    with pytest.raises(ValueError):
        classify(snapshot, binding)


@pytest.mark.parametrize('name,path', [
    ('ansysli_server.exe', 'C:/Program Files/ANSYS Inc/Shared Files/Licensing/winx64/ansysli_server.exe'),
    ('lmutil.exe', 'C:/Program Files/ANSYS Inc/v261/licensingclient/winx64/lmutil.exe'),
    ('renamed.exe', 'C:/Program Files/ANSYS Inc/v261/licensingclient/winx64/renamed.exe'),
])
def test_selected_licensing_utility_is_unknown_not_automatic_solver_conflict(known_cfd, name, path):
    snapshot, binding = known_cfd
    # Inclusion as an unexpected relevant-family descendant requires disposition;
    # an unrelated resident utility outside the selector is not added to rows.
    snapshot['rows'].append(process(30, 12, name, path))
    update_scope_counts(snapshot)
    result = classify(snapshot, binding)
    assert result['status'] == 'UNKNOWN'
    assert not any(r['pid'] == 30 for r in result['conflicts'])
    assert any(r['pid'] == 30 for r in result['unknowns'])
    assert result['process_inventory'] == result['conflicts'] + result['unknowns']
    assert result['raw_inventory'] == snapshot['rows']


def test_freshness_boundary_preserves_decimal_precision(known_cfd):
    snapshot, binding = known_cfd
    snapshot['observed_at'] = '1000.00000000000000000000000000001'
    with pytest.raises(ValueError, match='stale'):
        classify(snapshot, binding, now='1005.00000000000000000000000000002')


def test_every_controller_chain_python_script_argument_requires_a_pin(known_cfd):
    snapshot, binding = known_cfd
    snapshot['rows'][1]['argv'].append('C:/synthetic/second_guard.py')
    binding['processes'] = deepcopy(snapshot['rows'])
    with pytest.raises(ValueError, match='script'):
        classify(snapshot, binding)


def test_freshness_is_independent_of_mutated_default_decimal_context(known_cfd):
    snapshot, binding = known_cfd
    saved = DefaultContext.copy()
    try:
        DefaultContext.prec = 2
        DefaultContext.rounding = ROUND_DOWN
        DefaultContext.Emin = -2
        DefaultContext.Emax = 0
        DefaultContext.clamp = 1
        DefaultContext.traps[Overflow] = True
        assert classify(snapshot, binding, now='1011', maximum_age_seconds='20')['status'] == 'CLEAR'
    finally:
        for name in ('prec', 'rounding', 'Emin', 'Emax', 'capitals', 'clamp'):
            setattr(DefaultContext, name, getattr(saved, name))
        DefaultContext.flags = saved.flags.copy()
        DefaultContext.traps = saved.traps.copy()
