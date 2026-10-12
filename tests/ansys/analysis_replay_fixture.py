"""Self-contained synthetic matrix/review/raw fixture; never native authority."""
from copy import deepcopy
from pathlib import Path

from digitalmodel.ansys import analysis_replay_inputs as inputs
from digitalmodel.ansys.analysis_records import canonical_bytes, digest_bytes
from digitalmodel.ansys.analysis_evidence import build_package
from digitalmodel.ansys.cylinder_criteria import CASE_IDS
from tests.ansys.cylinder_synthetic_protocol import synthetic_protocol


def retain(root, resolver, identity, value, *, binary=False):
    raw = value if binary else canonical_bytes(value)
    path = root/identity
    path.write_bytes(raw)
    resolver[identity] = path
    return dict(id=identity, sha256=digest_bytes(raw), required=True)


def mapping_for(case):
    rows = []
    for station in case['stations']:
        for q in ('sigma_r', 'sigma_theta', 'sigma_z', 'tau_rz', 'sigma_vm', 'u_r', 'u_z'):
            rows.append(dict(key=[station['id'], q], name=station['id'] + '.' + q,
                definition='synthetic ' + q, location='station ' + station['id'],
                unit='mm' if q.startswith('u_') else 'MPa'))
    rows.append(dict(key='RFY', name='support.RFY', definition='synthetic reaction sum',
                     location='bottom support', unit='N'))
    return dict(schema='zero-replay-mapping-1', responses=rows)


def make_baseline(root, resolver, observation, mapping):
    ref = retain(root, resolver, 'synthetic-basis', {'synthetic': True})
    cases = []
    ids = [f'history-{i}' for i in range(8)] + list(CASE_IDS)
    for index, count in enumerate([6, 6, 7, 7, 16, 16, 18, 18, 64, 64, 64, 64]):
        rows = [dict(name='q'+str(i), definition='synthetic', location='fixture', unit='MPa')
                for i in range(count)]
        if index == 8:
            rows = [{k: v for k, v in row.items() if k != 'key'} for row in mapping['responses']]
        for row in rows:
            row.update(value=None, calculation_status='failed' if index == 8 else 'not_evaluated',
                limitations=['diagnostic-only', 'unsupported-cdb-omega'] if index == 8 else ['native-not-attempted'],
                evidence_ids=[ref['id']], inherited_findings=[])
        case = dict(case_id=ids[index], component_id='synthetic', model_revision='synthetic',
            parameters={'index': str(index)}, author='unverified', author_status='unverified',
            source_kind='unverified', execution_status='unknown', retention_rights='approved',
            use_rights='unresolved', generated_at='2026-09-14T00:00:00Z', superseded_by=[],
            capture_role='pending_native' if index >= 9 else 'synthetic',
            input_descriptor=dict(load_basis='synthetic', source_revision='synthetic',
                solver='none', frame='synthetic', dependencies=['synthetic-basis']),
            evidence=[dict(ref, role='input_basis')], responses=rows)
        if index >= 9:
            case.update(attempt_consumed=False, native_attempt_count=0)
        if index == 8:
            observed_fields(case, observation)
        cases.append(case)
    return build_package(dict(analysis_id='synthetic', dataset_id='ansys-retained-evidence',
        revision='synthetic-observed', criteria_revision='synthetic', code_revision='synthetic',
        method_revision='synthetic-observed', expected_cases=ids, intended_uses=['diagnostic'],
        cases=cases, finding_ledger=[], criteria_reference=ref, intake_reference=ref, review_sources=[],
        coverage=dict(total_cases=12, total_responses=350, pending_cases=3, pending_responses=192,
            qualified_responses=0, assessment_incomplete_cases=1, assessment_failed_responses=64,
            observed_transition_native_attempts=1)), resolver)


def observed_fields(case, observation):
    case.update(capture_role='diagnostic_observation', attempt_consumed=True, native_attempt_count=1,
        author='SOLVERS', author_status='recorded', source_kind='native', execution_status='completed',
        assessment_status='incomplete', observation_reference={k: observation[k] for k in ('id', 'sha256')},
        observed_execution=dict(return_code=0, duration_seconds='1'),
        input_descriptor_scope='historical-prelaunch-basis', observed_solver_profile=None,
        observed_solver_profile_status='not-established-from-native-header',
        observed_retention=dict(status='retained-local-source-evidence', private_git_backup='not-established'))
    case['evidence'].append(dict(observation, role='diagnostic_intake'))


def synthetic_code_review(root, resolver):
    inventory = inputs.source_inventory()
    source_root = Path(inputs.__file__).resolve().parents[3]
    code, entries = {}, []
    for index, name in enumerate(inventory):
        raw = (source_root/name).read_bytes()
        code[name] = retain(root, resolver, 'source-'+str(index), raw, binary=True)
        entries.append(dict(path=name, sha256=digest_bytes(raw), content=raw.decode('utf-8')))
    bundle = retain(root, resolver, 'review-bundle', {'files': entries})
    result = dict(bundle_sha256=bundle['sha256'], verdict='MINOR', findings=[])
    transport = retain(root, resolver, 'review-transport', dict(is_error=False,
        session_id='SYNTHETIC-NOT-PROVIDER-AUTHORITY', structured_output=result))
    review = retain(root, resolver, 'review', dict(status='REVIEW_RECEIVED',
        bundle_sha256=bundle['sha256'], files=entries, review=result, stdout_sha256=transport['sha256']))
    return code, dict(review=review, review_transport=transport, review_bundle=bundle)


def original_artifact_records(raw):
    artifacts = {name: dict(path=name, sha256=digest_bytes(raw[inputs.CASE_ID+'/'+name]),
        bytes=len(raw[inputs.CASE_ID+'/'+name])) for name in inputs.ORIGINAL_ARTIFACT_NAMES}
    execution = dict(streams_finalized=True, stream_readback_errors=[])
    for stream in ('stdout', 'stderr'):
        digest = artifacts[stream+'.bin']['sha256']
        execution.update({stream+'_sha256': digest, stream+'_retained_sha256': digest,
            stream+'_available': True, stream+'_readback_matches': True})
    raw[inputs.CASE_ID+'/execution.json'] = canonical_bytes(execution)
    return artifacts


def synthetic_inputs(root, resolver, monkeypatch):
    case, artifacts = synthetic_protocol(inputs.CASE_ID)
    raw = {name: b'{}' for name in inputs.RAW_NAMES}
    aliases = {'native.out': inputs.CASE_ID+'.out', 'jobname.err': 'file.err',
               'stdout': 'stdout.bin', 'stderr': 'stderr.bin'}
    for name, data in artifacts.items():
        raw[inputs.CASE_ID+'/'+aliases.get(name, name)] = data
    raw[inputs.CASE_ID+'/'+inputs.CASE_ID+'.inp'] = case['deck_bytes']
    source_root = Path(inputs.__file__).resolve().parents[3]
    reference = (source_root/'examples/ansys/cylinder-benchmark/reference.json').read_bytes()
    runtime = retain(root, resolver, 'runtime', dict(reference='reference.json',
        artifacts=[dict(path='reference.json', sha256=digest_bytes(reference), bytes=len(reference))],
        runtime_lineage=dict(
        source_revision=inputs.RUNTIME_REVISION, runtime_inventory_sha256=inputs.RUNTIME_INVENTORY)))
    monkeypatch.setattr(inputs, 'RUNTIME_HASH', runtime['sha256'])
    config = retain(root, resolver, 'config', dict(execution_binding=dict(manifest_sha256=runtime['sha256'])))
    approval = retain(root, resolver, 'approval', dict(manifest_sha256=runtime['sha256'], config_sha256=config['sha256']))
    raw['operator-outcome.json'] = canonical_bytes(dict(execution_approval_sha256=approval['sha256']))
    raw['outcome.json'] = canonical_bytes(dict(status='INCOMPLETE', attempted=[inputs.CASE_ID],
        records=[dict(case_id=inputs.CASE_ID, reference_sha256=digest_bytes(reference),
                      evidence_errors=['Unsupported CDB command OMEGA'], values={},
                      artifacts=original_artifact_records(raw))]))
    rawrefs = {name: retain(root, resolver, 'raw-'+str(i), data, binary=True)
               for i, (name, data) in enumerate(raw.items())}
    mapping = mapping_for(case)
    source_root = Path(inputs.__file__).resolve().parents[3]
    reference = (source_root/'examples/ansys/cylinder-benchmark/reference.json').read_bytes()
    docs = dict(reference=retain(root, resolver, 'reference', reference, binary=True),
        mapping=retain(root, resolver, 'mapping', mapping), runtime_manifest=runtime, approval=approval, config=config)
    return rawrefs, docs, mapping


def full_fixture(root, monkeypatch):
    resolver = {}
    raw, docs, mapping = synthetic_inputs(root, resolver, monkeypatch)
    code, review = synthetic_code_review(root, resolver)
    docs.update(review)
    observation = retain(root, resolver, 'observation', dict(sources=dict(
        outcome=raw['outcome.json'], execution=raw[inputs.CASE_ID+'/execution.json'],
        approval=docs['approval'], config=docs['config'], runtime_manifest=docs['runtime_manifest'])))
    baseline = make_baseline(root, resolver, observation, mapping)
    request = dict(schema='zero-numeric-replay-1', case_id=inputs.CASE_ID,
        previous_case_hash=baseline['cases'][8]['row_hash'], raw=raw, code=code, documents=docs)
    replay = retain(root, resolver, 'replay', request)
    return dict(baseline=baseline, resolver=resolver, replay_reference=replay,
                review_sha256=docs['review']['sha256'], root=root, receipt=request)
