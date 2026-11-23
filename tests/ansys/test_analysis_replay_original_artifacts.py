"""Synthetic original-execution binding mutations; no native execution."""
import pytest
from digitalmodel.ansys.analysis_records import canonical_bytes, digest_bytes
from digitalmodel.ansys.analysis_replay import _original
from digitalmodel.ansys.analysis_replay_inputs import CASE_ID

NAMES = ('file.db', 'file.err', 'file.mntr', 'file.rst', 'model.cdb',
    CASE_ID+'.inp', CASE_ID+'.out', 'precision_witness.txt', 'state_values.txt',
    'station_values.txt', 'stderr.bin', 'stdout.bin', 'support_reactions.txt')


def fixture(tmp_path):
    raw = {CASE_ID+'/'+name: b'synthetic '+name.encode() for name in NAMES}
    artifacts = {name: dict(path=name, sha256=digest_bytes(raw[CASE_ID+'/'+name]),
        bytes=len(raw[CASE_ID+'/'+name])) for name in NAMES}
    execution = dict(streams_finalized=True, stream_readback_errors=[])
    for stream in ('stdout', 'stderr'):
        digest = artifacts[stream+'.bin']['sha256']
        execution.update({stream+'_sha256': digest, stream+'_retained_sha256': digest,
            stream+'_available': True, stream+'_readback_matches': True})
    outcome = dict(status='INCOMPLETE', attempted=[CASE_ID], records=[dict(case_id=CASE_ID,
        evidence_errors=['Unsupported CDB command OMEGA'], values={}, artifacts=artifacts)])
    raw['outcome.json'] = canonical_bytes(outcome)
    raw[CASE_ID+'/execution.json'] = canonical_bytes(execution)
    return raw, outcome, execution


def authority(tmp_path, raw):
    sources, resolver = {}, {}
    for role, name in [('outcome', 'outcome.json'), ('execution', CASE_ID+'/execution.json')]:
        path = tmp_path/role
        path.write_bytes(raw[name]); resolver[role] = path
        sources[role] = dict(id=role, sha256=digest_bytes(raw[name]))
    encoded = canonical_bytes(dict(sources=sources))
    path = tmp_path/'observation'; path.write_bytes(encoded); resolver['observation'] = path
    old = dict(observation_reference=dict(id='observation', sha256=digest_bytes(encoded)))
    return old, resolver


@pytest.mark.parametrize('name', NAMES)
def test_each_output_mutation_refuses_against_original_record(tmp_path, name):
    raw, _, _ = fixture(tmp_path)
    old, resolver = authority(tmp_path, raw)
    raw[CASE_ID+'/'+name] += b'changed'
    with pytest.raises(ValueError, match='artifact|stream'):
        _original(raw, old, resolver, {})


@pytest.mark.parametrize('fault', ['missing', 'path', 'hash', 'size', 'size_bool'])
def test_original_artifact_metadata_must_match(tmp_path, fault):
    raw, outcome, _ = fixture(tmp_path)
    item = outcome['records'][0]['artifacts']['model.cdb']
    if fault == 'missing':
        outcome['records'][0]['artifacts'].pop('model.cdb')
    else:
        item[{'hash':'sha256', 'size':'bytes', 'size_bool':'bytes'}.get(fault, fault)] = {
            'path':'other.cdb', 'hash':'a'*64, 'size':1, 'size_bool':True}[fault]
    raw['outcome.json'] = canonical_bytes(outcome)
    old, resolver = authority(tmp_path, raw)
    with pytest.raises(ValueError, match='artifact'):
        _original(raw, old, resolver, {})


@pytest.mark.parametrize('field', ['stdout_sha256', 'stderr_sha256',
    'stdout_retained_sha256', 'stderr_retained_sha256', 'stdout_available',
    'stderr_available', 'stdout_readback_matches', 'stderr_readback_matches',
    'streams_finalized', 'stream_readback_errors'])
def test_independently_bound_execution_stream_state_refuses(tmp_path, field):
    raw, _, execution = fixture(tmp_path)
    execution.pop(field)
    raw[CASE_ID+'/execution.json'] = canonical_bytes(execution)
    old, resolver = authority(tmp_path, raw)
    with pytest.raises(ValueError, match='stream'):
        _original(raw, old, resolver, {})


def test_all_original_artifacts_and_streams_match(tmp_path):
    raw, outcome, _ = fixture(tmp_path)
    old, resolver = authority(tmp_path, raw)
    assert _original(raw, old, resolver, {}) == outcome


def test_bound_set_covers_every_current_validator_required_input():
    from digitalmodel.ansys.cylinder_results_validation import REQUIRED
    from digitalmodel.ansys.analysis_replay_inputs import ORIGINAL_ARTIFACT_NAMES
    aliases = {'native.out': CASE_ID+'.out', 'jobname.err': 'file.err',
               'stdout': 'stdout.bin', 'stderr': 'stderr.bin'}
    assert {aliases.get(name, name) for name in REQUIRED} <= set(ORIGINAL_ARTIFACT_NAMES)
    assert set(ORIGINAL_ARTIFACT_NAMES) == set(NAMES)


@pytest.mark.parametrize('field', ['stdout_sha256', 'stderr_sha256',
    'stdout_retained_sha256', 'stderr_retained_sha256'])
def test_changed_execution_hash_refuses_even_when_outcome_map_matches(tmp_path, field):
    raw, _, execution = fixture(tmp_path)
    execution[field] = 'a'*64
    raw[CASE_ID+'/execution.json'] = canonical_bytes(execution)
    old, resolver = authority(tmp_path, raw)
    with pytest.raises(ValueError, match='stream'):
        _original(raw, old, resolver, {})
