"""Synthetic admission-contract tests only; no real authority, provider or solver."""
import copy
import json
from pathlib import Path

import pytest

from digitalmodel.ansys.analysis_records import canonical_bytes, digest_bytes
from digitalmodel.ansys import cylinder_diagnostic_admission as admission
from digitalmodel.ansys.cylinder_canary import PROFILE
from digitalmodel.ansys.cylinder_diagnostic_admission import make_diagnostic_admission


def write_json(path, value):
    path.write_bytes(canonical_bytes(value))
    return digest_bytes(path.read_bytes())


SYNTHETIC_SOURCES = (
    'src/digitalmodel/ansys/cylinder_fixture.py',
    'src/digitalmodel/ansys/runner.py',
    'scripts/ansys/run_zero_control_diagnostic.py',
)
LAUNCH_ENVIRONMENT = {'ANSYS261_PRODUCT': 'ansys', 'ANS_CONSEC': 'YES'}


def synthetic_inventory(source):
    return [dict(path=name, sha256=digest_bytes((source / name).read_bytes()))
            for name in SYNTHETIC_SOURCES]


def refresh_bundle(inputs):
    """Rebuild explicitly synthetic review data using retained helper serialization."""
    inputs['expected_config_sha256'] = write_json(inputs['config_path'], inputs['config'])
    paths = [inputs['config_path'], *[inputs['source_root'] / item['path']
                                    for item in inputs['config']['source_files']]]
    files = [dict(path=str(path.resolve()), sha256=digest_bytes(path.read_bytes()),
                  content=path.read_bytes().decode('utf-8-sig')) for path in paths]
    raw = json.dumps({'context': 'Explicitly synthetic review; no execution authority.',
                      'files': files}).encode()
    inputs['review_bundle_path'].write_bytes(raw)
    bundle_sha = digest_bytes(raw)
    inputs['receipt']['files'] = [{key: item[key] for key in ('path', 'sha256')} for item in files]
    inputs['receipt']['bundle_sha256'] = bundle_sha
    inputs['transport']['structured_output']['bundle_sha256'] = bundle_sha
    inputs['approval']['config_sha256'] = inputs['expected_config_sha256']
    rebind_review(inputs)


@pytest.fixture
def admission_inputs(tmp_path, monkeypatch):
    source = tmp_path / 'source'
    for name in SYNTHETIC_SOURCES:
        path = source / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes(b'# explicitly synthetic source; not production authority\n')
    # Test seam only: production must derive loaded ownership and actual __main__.
    monkeypatch.setattr(admission, 'required_source_inventory', synthetic_inventory)
    ledger = tmp_path / 'ledger'
    ledger.mkdir()
    config = dict(schema='cylinder-zero-diagnostic-admission-1',
                  campaign_id='synthetic-fixed-campaign', operator_id='SOLVERS-synthetic-session',
                  ledger_directory=str(ledger.resolve()),
                  execution_binding=dict(manifest_sha256='b' * 64,
                      executable_sha256='c' * 64, execution_host='synthetic-host',
                      profile=copy.deepcopy(PROFILE), launch_environment=copy.deepcopy(LAUNCH_ENVIRONMENT),
                      runtime_profile=dict(release='synthetic', build='synthetic',
                                           update='00000000', platform='synthetic'),
                      capture_allowance_bytes=1073741824, reserve_bytes=2147483648),
                  scope=dict(case_ids=['ocv-zero-t60-n16'], max_attempts=1,
                             qualification='diagnostic_only'), source_files=synthetic_inventory(source))
    review = dict(bundle_sha256='', verdict='MINOR', findings=[])
    transport = dict(session_id='synthetic-independent-review-session', is_error=False,
                     structured_output=review)
    receipt = dict(status='REVIEW_RECEIVED', exit_code=0, review=review)
    approval = dict(approval_id=config['campaign_id'], operator_id=config['operator_id'],
                    checker_id=transport['session_id'], ledger_directory=str(ledger.resolve()))
    approval.update(copy.deepcopy(config['execution_binding']))
    inputs = dict(config_path=tmp_path / 'config.json', review_receipt_path=tmp_path / 'review.json',
                  review_stdout_path=tmp_path / 'review-stdout.json', source_root=source,
                  review_bundle_path=tmp_path / 'review.bundle.json', approval=approval,
                  config=config, receipt=receipt, transport=transport)
    refresh_bundle(inputs)
    return inputs


def make(inputs):
    return make_diagnostic_admission(**{key: value for key, value in inputs.items()
                                       if key not in {'approval', 'config', 'receipt', 'transport'}})


def claim_path(inputs):
    config = inputs['config']
    return Path(config['ledger_directory']) / (digest_bytes(config['campaign_id'].encode()) + '.json')


def test_unconsumed_scope_uses_actual_retained_review_identity(admission_inputs):
    adapters = make(admission_inputs)
    assert adapters['checker_id'] == admission_inputs['transport']['session_id']
    assert adapters['verify_authority'](admission_inputs['approval']) is True
    # No in-memory/call-count allowance: only durable consumption closes the scope.
    assert adapters['verify_authority'](admission_inputs['approval']) is True
    assert not claim_path(admission_inputs).exists()


@pytest.mark.parametrize('raw', [b'', b'{', b'{}', b'{"approval_id":"synthetic-fixed-campaign"}'])
def test_existing_even_partial_claim_refuses_before_runner(admission_inputs, raw):
    claim_path(admission_inputs).write_bytes(raw)
    with pytest.raises(ValueError):
        make(admission_inputs)
    assert claim_path(admission_inputs).read_bytes() == raw


def test_durable_claim_stops_next_preflight_and_process_restart(admission_inputs):
    adapters = make(admission_inputs)
    assert adapters['verify_authority'](admission_inputs['approval']) is True
    claim_path(admission_inputs).write_bytes(b'{"state":"consumed"}')
    with pytest.raises(ValueError, match='scope.*exhausted|consum'):
        adapters['verify_authority'](admission_inputs['approval'])
    with pytest.raises(ValueError):
        make(admission_inputs)


@pytest.mark.parametrize('field,value', [
    ('approval_id', 'invented-retry-campaign'), ('operator_id', 'unbound-operator'),
    ('checker_id', 'invented-checker'), ('config_sha256', '0' * 64),
    ('review_receipt_sha256', '0' * 64), ('ledger_directory', 'relocated-ledger')])
def test_approval_cannot_relocate_or_reidentify_scope(admission_inputs, field, value):
    adapters = make(admission_inputs)
    approval = copy.deepcopy(admission_inputs['approval'])
    approval[field] = value
    with pytest.raises(ValueError):
        adapters['verify_authority'](approval)


@pytest.mark.parametrize('field,value', [
    ('manifest_sha256', 'd' * 64), ('executable_sha256', 'd' * 64),
    ('execution_host', 'different-host'), ('profile', {'threads': 2, 'mode': 'SMP'}),
    ('runtime_profile', {'release': 'different'}), ('capture_allowance_bytes', 1),
    ('reserve_bytes', 1), ('launch_environment', {'ANS_CONSEC': 'NO'})])
def test_whole_execution_binding_cannot_change(admission_inputs, field, value):
    adapters = make(admission_inputs)
    approval = copy.deepcopy(admission_inputs['approval'])
    approval[field] = value
    with pytest.raises(ValueError):
        adapters['verify_authority'](approval)


@pytest.mark.parametrize('damage', ['extra', 'missing', 'boolean_budget'])
def test_approval_requires_exact_typed_object(admission_inputs, damage):
    adapters = make(admission_inputs)
    approval = copy.deepcopy(admission_inputs['approval'])
    if damage == 'extra': approval['arbitrary_override'] = True
    elif damage == 'missing': del approval['manifest_sha256']
    else: approval['capture_allowance_bytes'] = True
    with pytest.raises(ValueError):
        adapters['verify_authority'](approval)


@pytest.mark.parametrize('which', ['config_path', 'review_receipt_path', 'review_stdout_path', 'review_bundle_path', 'source'])
def test_changed_bound_bytes_refuse_at_factory_and_next_preflight(admission_inputs, which):
    adapters = make(admission_inputs)
    path = (admission_inputs['source_root'] / SYNTHETIC_SOURCES[0]
            if which == 'source' else admission_inputs[which])
    original = path.read_bytes()
    path.write_bytes(original + b'\n')
    with pytest.raises(ValueError):
        adapters['verify_authority'](admission_inputs['approval'])
    with pytest.raises(ValueError):
        make(admission_inputs)


def rebind_review(inputs):
    inputs['receipt']['stdout_sha256'] = write_json(inputs['review_stdout_path'], inputs['transport'])
    inputs['expected_review_sha256'] = write_json(inputs['review_receipt_path'], inputs['receipt'])
    inputs['approval']['review_receipt_sha256'] = inputs['expected_review_sha256']


@pytest.mark.parametrize('damage', ['missing_session', 'blank_session', 'failed_transport',
                                  'major', 'invalid_receipt', 'wrong_bundle', 'missing_config',
                                  'missing_code', 'duplicate_file', 'review_mismatch'])
def test_hashed_but_inadequate_provider_evidence_refuses(admission_inputs, damage):
    data = admission_inputs
    transport, receipt = data['transport'], data['receipt']
    if damage == 'missing_session': del transport['session_id']
    elif damage == 'blank_session': transport['session_id'] = ' '
    elif damage == 'failed_transport': transport['is_error'] = True
    elif damage == 'major': transport['structured_output']['verdict'] = 'MAJOR'
    elif damage == 'invalid_receipt': receipt['status'] = 'INVALID_OUTPUT'
    elif damage == 'wrong_bundle': receipt['bundle_sha256'] = 'b' * 64
    elif damage == 'missing_config': receipt['files'].pop(0)
    elif damage == 'missing_code': receipt['files'].pop()
    elif damage == 'duplicate_file': receipt['files'].append(copy.deepcopy(receipt['files'][0]))
    else: transport['structured_output'] = dict(transport['structured_output'], findings=['different'])
    rebind_review(data)
    with pytest.raises(ValueError):
        make(data)


@pytest.mark.parametrize('damage', ['case', 'attempts', 'attempt_bool', 'qualification', 'relative_ledger'])
def test_even_reviewed_config_cannot_expand_diagnostic_scope(admission_inputs, damage):
    data = admission_inputs
    if damage == 'case': data['config']['scope']['case_ids'].append('ocv-t60-p10-n4')
    elif damage == 'attempts': data['config']['scope']['max_attempts'] = 2
    elif damage == 'attempt_bool': data['config']['scope']['max_attempts'] = True
    elif damage == 'qualification': data['config']['scope']['qualification'] = 'engineering'
    else: data['config']['ledger_directory'] = 'relative'
    refresh_bundle(data)
    with pytest.raises(ValueError):
        make(data)


def test_redirected_ledger_refuses_without_claim(admission_inputs, monkeypatch):
    ledger = Path(admission_inputs['config']['ledger_directory'])
    original = Path.is_junction
    monkeypatch.setattr(Path, 'is_junction', lambda path: path == ledger or original(path))
    with pytest.raises(ValueError):
        make(admission_inputs)
    assert not claim_path(admission_inputs).exists()


def test_adjudication_cannot_fabricate_engineering_proof(admission_inputs):
    adapters = make(admission_inputs)
    with pytest.raises(ValueError):
        adapters['adjudicate']({'status': 'PASS'}, admission_inputs['approval'])


@pytest.mark.parametrize('field,value', [
    ('profile', {'cores': True, 'parallel': 'smp', 'timeout_seconds': 300}),
    ('profile', {'cores': 2, 'parallel': 'smp', 'timeout_seconds': 300}),
    ('profile', {'cores': 1, 'parallel': 'SMP', 'timeout_seconds': 300}),
    ('profile', {'cores': 1, 'parallel': 'smp', 'timeout_seconds': 301}),
    ('launch_environment', {'ANSYS261_PRODUCT': 'ansys'}),
    ('launch_environment', {'ANSYS261_PRODUCT': 'ansys', 'ANS_CONSEC': True}),
    ('launch_environment', {'ANSYS261_PRODUCT': 'other', 'ANS_CONSEC': 'YES'}),
    ('launch_environment', dict(LAUNCH_ENVIRONMENT, EXTRA='unreviewed')),
    ('capture_allowance_bytes', True), ('reserve_bytes', 0)])
def test_reviewed_execution_object_still_requires_fixed_typed_profile(admission_inputs, field, value):
    data = admission_inputs
    data['config']['execution_binding'][field] = value
    data['approval'][field] = value
    refresh_bundle(data)
    with pytest.raises(ValueError):
        make(data)


@pytest.mark.parametrize('missing', SYNTHETIC_SOURCES)
def test_reviewed_narrowed_inventory_cannot_omit_runtime_runner_or_entrypoint(admission_inputs, missing):
    data = admission_inputs
    data['config']['source_files'] = [item for item in data['config']['source_files']
                                      if item['path'] != missing]
    refresh_bundle(data)
    with pytest.raises(ValueError):
        make(data)


@pytest.mark.parametrize('damage', ['context_only', 'content', 'raw_sha', 'path', 'missing_file'])
def test_rehashed_bundle_cannot_substitute_reviewed_file_bindings(admission_inputs, damage):
    data = admission_inputs
    bundle = json.loads(data['review_bundle_path'].read_bytes())
    if damage == 'context_only': bundle['files'] = []
    elif damage == 'content': bundle['files'][-1]['content'] += '# forged content'
    elif damage == 'raw_sha': bundle['files'][-1]['sha256'] = 'f' * 64
    elif damage == 'path': bundle['files'][-1]['path'] = str(data['source_root'] / 'absent.py')
    else: bundle['files'].pop()
    raw = json.dumps(bundle).encode()
    data['review_bundle_path'].write_bytes(raw)
    data['receipt']['bundle_sha256'] = digest_bytes(raw)
    data['transport']['structured_output']['bundle_sha256'] = digest_bytes(raw)
    rebind_review(data)
    with pytest.raises(ValueError):
        make(data)


def test_unchanged_claimed_digest_does_not_hide_bundle_context_substitution(admission_inputs):
    data = admission_inputs
    raw = data['review_bundle_path'].read_bytes().replace(b'Explicitly synthetic', b'Substituted context')
    data['review_bundle_path'].write_bytes(raw)
    with pytest.raises(ValueError):
        make(data)


def test_extra_review_context_file_is_also_bound_to_current_bytes(admission_inputs):
    data = admission_inputs
    extra = data['source_root'] / 'review-context.txt'
    extra.write_bytes(b'Explicitly synthetic context')
    bundle = json.loads(data['review_bundle_path'].read_bytes())
    item = dict(path=str(extra.resolve()), sha256=digest_bytes(extra.read_bytes()),
                content=extra.read_text())
    bundle['files'].append(item)
    raw = json.dumps(bundle).encode()
    data['review_bundle_path'].write_bytes(raw)
    data['receipt']['files'].append({key: item[key] for key in ('path', 'sha256')})
    data['receipt']['bundle_sha256'] = digest_bytes(raw)
    data['transport']['structured_output']['bundle_sha256'] = digest_bytes(raw)
    rebind_review(data)
    adapters = make(data)
    assert adapters['verify_authority'](data['approval']) is True
    extra.write_bytes(b'Changed unlisted runtime context')
    with pytest.raises(ValueError):
        adapters['verify_authority'](data['approval'])


@pytest.mark.parametrize('finding', ['[Scope A][MINOR] Synthetic observation.',
                                    '[MINOR] No engineering qualification is established.'])
def test_real_helper_string_findings_and_transport_telemetry_supported(admission_inputs, finding):
    data = admission_inputs
    data['transport']['structured_output']['findings'] = [finding]
    data['transport']['total_cost_usd'] = 0.012
    # Retained provider transport legitimately contains floats outside reviewed values.
    raw = json.dumps(data['transport']).encode()
    data['review_stdout_path'].write_bytes(raw)
    data['receipt']['stdout_sha256'] = digest_bytes(raw)
    data['expected_review_sha256'] = write_json(data['review_receipt_path'], data['receipt'])
    data['approval']['review_receipt_sha256'] = data['expected_review_sha256']
    assert make(data)['verify_authority'](data['approval']) is True


@pytest.mark.parametrize('finding', ['[MAJOR] Synthetic contradiction.', '', 123])
def test_minor_verdict_cannot_hide_blocking_or_invalid_findings(admission_inputs, finding):
    data = admission_inputs
    data['transport']['structured_output']['findings'] = [finding]
    rebind_review(data)
    with pytest.raises(ValueError):
        make(data)


def test_helper_bom_content_and_raw_hash_are_distinct(admission_inputs):
    data = admission_inputs
    source = data['source_root'] / SYNTHETIC_SOURCES[0]
    source.write_bytes(b'\xef\xbb\xbf# synthetic BOM source\n')
    data['config']['source_files'] = synthetic_inventory(data['source_root'])
    refresh_bundle(data)
    assert make(data)['verify_authority'](data['approval']) is True


def test_top_level_operational_object_is_config_bound_not_approval_field(admission_inputs):
    data = admission_inputs
    data['config']['operational'] = {'synthetic_driver_owned_schema': True}
    refresh_bundle(data)
    assert make(data)['verify_authority'](data['approval']) is True
    with pytest.raises(ValueError):
        make(data)['verify_authority'](dict(data['approval'], operational=data['config']['operational']))


def test_production_inventory_refuses_caller_selected_source_root(tmp_path):
    with pytest.raises(ValueError, match='source root'):
        admission.required_source_inventory(tmp_path)
