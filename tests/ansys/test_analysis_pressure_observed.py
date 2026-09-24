"""Synthetic pressure intake controls; never native or qualification fixtures."""
import copy
import json
from pathlib import Path
import pytest
from digitalmodel.ansys.analysis_records import canonical_bytes, digest_bytes, validate_case
from digitalmodel.ansys.analysis_evidence import build_package, publish_package
from digitalmodel.ansys.analysis_matrix_publish import _relation, publish_matrix
from tests.ansys.test_analysis_observed import fixture_baseline, reference

CID = 'ocv-t60-p10-n4'


def rehash(study, resolver):
    study = copy.deepcopy(study)
    study.pop('package_hash', None)
    for case in study['cases']:
        case.pop('row_hash', None)
    return build_package(study, resolver)


def bind(data, role, value):
    ref = reference(data['tmp'], data['resolver'], 'pressure-'+role, value)
    data['receipt']['sources'][role] = ref
    data['docs'][role] = value
    return ref


def baseline_fixture(tmp_path):
    resolver = {}
    base = fixture_baseline(tmp_path, resolver)
    base['revision'] = 'r4'
    base['cases'][8]['capture_role']='fixture'
    case = base['cases'][9]
    case['model_revision'] = digest_bytes(b'synthetic deck')
    quantities = ('sigma_r', 'sigma_theta', 'sigma_z', 'tau_rz', 'sigma_vm', 'u_r', 'u_z')
    names = [(f'{r}_y{y}.{q}', 'mm' if q.startswith('u_') else 'MPa')
             for y in (60,120,180) for r in ('inner','middle','outer') for q in quantities]
    names.append(('support.RFY', 'N'))
    for row, (name, unit) in zip(case['responses'], names):
        row.update(name=name, unit=unit)
    base['coverage'].update(pending_cases=3, pending_responses=192)
    return rehash(base, resolver), resolver


def raw_fixture(data):
    names = ('file.db','file.rst','file.mntr','file.err','model.cdb',CID+'.inp',CID+'.out',
             'state_values.txt','station_values.txt','support_reactions.txt',
             'precision_witness.txt','stdout.bin','stderr.bin')
    rows = []
    for i, name in enumerate(names):
        raw = b'' if name in ('stdout.bin','stderr.bin','file.err') else b'synthetic'
        if name.endswith('.inp'): raw = b'synthetic deck'
        path = data['tmp']/('raw-'+str(i)); path.write_bytes(raw)
        ref = dict(id='pressure-raw-'+str(i),sha256=digest_bytes(raw))
        data['resolver'][ref['id']] = path; data['receipt']['raw'][name] = ref
        rows.append(dict(path=name,sha256=ref['sha256'],bytes=len(raw),status='retained',role='expected_capture'))
    return rows


def authority_fixture(data):
    parent = bind(data,'parent_claim',{'synthetic_parent':True})
    runtime = bind(data,'runtime_manifest',dict(execution_authorized=False,
        case_order=['ocv-zero-t60-n16',CID,'ocv-t60-p10-n8','ocv-t60-p10-n16'],
        runtime_lineage={'original_manifest_sha256':data['base']['cases'][9]['input_descriptor']['benchmark_manifest_reference']['sha256']},
        artifacts=[dict(path='prepared/'+CID+'.inp',sha256=data['base']['cases'][9]['model_revision'])]))
    config = dict(campaign_id='synthetic-pressure-campaign',operator_id='SOLVERS',source_revision='a'*40,
        scope=dict(case_ids=[CID],ordinal=2,max_attempts=1,capture_only=True,qualification='diagnostic_only'),
        execution_binding={'manifest_sha256':runtime['sha256']},source_files=[dict(path='src/synthetic.py',sha256=digest_bytes(b'synthetic source'))],
        lineage={'parent_claim':dict(path='parent',sha256=parent['sha256'])},operational={'output_directory':'synthetic-run'})
    cfg = bind(data,'config',config)
    bundle = bind(data,'review_bundle',dict(context='synthetic',files=[
        dict(path='src/synthetic.py',sha256=digest_bytes(b'synthetic source'),content='synthetic source'),
        dict(path='config.json',sha256=cfg['sha256'],content=canonical_bytes(config).decode())]))
    decision = dict(bundle_sha256=bundle['sha256'],verdict='MINOR',findings=[])
    transport = bind(data,'review_transport',dict(is_error=False,session_id='synthetic-checker',structured_output=decision))
    review = bind(data,'review',dict(status='REVIEW_RECEIVED',exit_code=0,bundle_sha256=bundle['sha256'],
        stdout_sha256=transport['sha256'],review=decision,files=[{k:r[k] for k in ('path','sha256')} for r in data['docs']['review_bundle']['files']]))
    claim = dict(ordinal=2,case_id=CID,state='attempt_consumed',parent_sha256=parent['sha256'],
        input_sha256=data['base']['cases'][9]['model_revision'],config_sha256=cfg['sha256'],review_receipt_sha256=review['sha256'],output='synthetic-run')
    bind(data,'claim',claim);bind(data,'attempt',claim)
    bind(data,'invocation',dict(ordinal=2,case_id=CID,config_sha256=cfg['sha256'],review_receipt_sha256=review['sha256'],start_utc_unix_ns=1789479503640863800))


@pytest.fixture
def pressure(tmp_path,monkeypatch):
    import digitalmodel.ansys.analysis_pressure_source as source
    monkeypatch.setattr(source,'verify_build_source',lambda revision,files: None)
    base,resolver = baseline_fixture(tmp_path)
    data = dict(base=base,resolver=resolver,tmp=tmp_path,docs={},receipt=dict(schema='pressure-diagnostic-intake-1',
        case_id=CID,case_index=9,previous_case_hash=base['cases'][9]['row_hash'],sources={},raw={},limitations=[
        'Readback equality compares reads of the same retained files and is not independent stream-content corroboration.']))
    rows = raw_fixture(data);authority_fixture(data)
    execution = dict(return_code=0,duration_seconds='18.234000000171363',timed_out=False,
        owned_processes_remaining=0,containment_verified=True,evidence_complete=True,
        settlement_required=False,streams_finalized=True)
    for stream in ('stdout','stderr'):
        for suffix in ('_sha256','_retained_sha256'):execution[stream+suffix]=digest_bytes(b'')
        execution[stream+'_readback_matches']=True
    bind(data,'execution',execution)
    fields=('return_code','timed_out','owned_processes_remaining','containment_verified','evidence_complete','settlement_required','streams_finalized')
    capture=dict(case_id=CID,capture_status='INCOMPLETE',retention_status='COMPLETE',independent_check_status='COMPLETE',
        execution_status='COMPLETE',execution_claim={k:execution[k] for k in fields},inventory=rows,checks=[dict(check=n,status='COMPLETE') for n in ('deck','execution','runtime_profile','case_titles','configuration','diagnostics','model','state','precision_witness')],
        accepted_values={},numerical_assessment='NOT_EVALUATED',reason='PRESSURE_PARSER_NOT_VALIDATED',engineering_qualified=False,
        unverified_evidence=['file.db','file.mntr','file.rst','station_values.txt','support_reactions.txt','pressure_loads_and_support_conditions','stress_and_reaction_recovery'],auxiliary_anomalies=[])
    bind(data,'capture',capture)
    outcome=dict(case_id=CID,native_launch_count=1,launch_adapter_calls=1,consumed_count=2,
        no_owned_processes_established=True,reservation_released=True,accepted_values={},engineering_qualified=False,
        assessment_status='NOT_EVALUATED',assessment_reason='PRESSURE_PARSER_NOT_VALIDATED',campaign_status='INCOMPLETE',terminal_reason='PLANNED_SCOPE_STOP')
    bind(data,'outcome',outcome);bind(data,'terminal',outcome)
    bind(data,'prefix_replay',dict(case_id='ocv-zero-t60-n16',capture_role='diagnostic_replay'))
    bind(data,'preflight',dict(synthetic=True))
    result_review_fixture(data)
    historical_proof_fixture(data,monkeypatch)
    return data


def build(data):
    from digitalmodel.ansys.analysis_pressure_observed import build_pressure_observed_package, intake_code_files
    ref=reference(data['tmp'],data['resolver'],'pressure-intake',data['receipt'])
    return build_pressure_observed_package(data['base'],data['resolver'],observation_reference=ref,
        revision='r5',code_files=intake_code_files(),code_revision=digest_bytes(canonical_bytes(intake_code_files())),source_revision='a'*40)


def test_capture_preserves_other_cases_and_nulls(pressure):
    result=build(pressure); case=result['cases'][9]
    assert case['capture_role']=='diagnostic_capture'
    assert case['native_attempt_count']==1 and case['assessment_status']=='not_evaluated'
    assert all(r['value'] is None and r['calculation_status']=='not_evaluated' for r in case['responses'])
    assert all(canonical_bytes(a)==canonical_bytes(b) for i,(a,b) in enumerate(zip(result['cases'],pressure['base']['cases'])) if i!=9)
    assert result==build(pressure)
    assert result['coverage']['pending_cases']==2
    assert result['coverage']['qualified_responses']==0


@pytest.mark.parametrize('role,key,value', [('execution','return_code',True),('capture','accepted_values',{'x':'0'}),
    ('capture','numerical_assessment','PASS'),('outcome','native_launch_count',True),('claim','input_sha256','f'*64),
    ('claim','config_sha256','f'*64),('execution','stdout_retained_sha256','f'*64),('config','source_revision','wrong')])
def test_contradiction_refused(pressure,role,key,value):
    doc=copy.deepcopy(pressure['docs'][role]);doc[key]=value;bind(pressure,role,doc)
    with pytest.raises(ValueError):build(pressure)


def test_mutated_raw_refused(pressure):
    pressure['resolver'][pressure['receipt']['raw']['model.cdb']['id']].write_bytes(b'changed')
    with pytest.raises(ValueError):build(pressure)


@pytest.mark.parametrize('field,value',[('value','0'),('calculation_status','failed'),('observed_value','0'),('unit','N')])
def test_shared_case_schema_refuses_promotion(pressure,field,value):
    case=build(pressure)['cases'][9];case['responses'][0][field]=value
    with pytest.raises(ValueError):validate_case(case)


def test_pure_append_refused_before_prefix_dispatch(pressure):
    base=pressure['base'];candidate=copy.deepcopy(base);extra=copy.deepcopy(base['cases'][0])
    extra['case_id']='appended';extra['parameters']={'index':'999'}
    candidate['cases'].append(extra);candidate['expected_cases'].append('appended')
    candidate.update(revision='r5',previous_package_hash=base['package_hash'])
    candidate=rehash(candidate,pressure['resolver'])
    with pytest.raises(ValueError):_relation(candidate,base,pressure['resolver'])


def test_publication_with_immutable_historical_source(pressure,tmp_path):
    immutable=reference(tmp_path,pressure['resolver'],'historical-code',{'code':'old revision'})
    pressure['base']['cases'][0]['evidence'].append(dict(immutable,role='source',required=True))
    pressure['base']=rehash(pressure['base'],pressure['resolver'])
    result=build(pressure);base=pressure['base'];owner=tmp_path/'owner'
    publish_package(base,owner);manifest=tmp_path/'manifest.json';manifest.write_bytes(canonical_bytes(base))
    (tmp_path/'working-source.py').write_text('new intake code')
    publish_matrix(result,base,manifest,owner,pressure['resolver'])
    assert json.loads(manifest.read_bytes())==result
    pressure['resolver']['historical-code']=tmp_path/'working-source.py'
    with pytest.raises(ValueError,match='evidence changed'):
        publish_matrix(result,base,manifest,owner,pressure['resolver'])


def result_review_fixture(data):
    rows=[dict(path=k+'.json',sha256=data['receipt']['sources'][k]['sha256'],content=canonical_bytes(data['docs'][k]).decode())
          for k in ('capture','execution','outcome')]
    b=bind(data,'result_review_bundle',dict(context='synthetic result',files=rows))
    decision=dict(bundle_sha256=b['sha256'],verdict='MINOR',findings=[])
    t=bind(data,'result_review_transport',dict(is_error=False,session_id='synthetic-result-checker',structured_output=decision))
    bind(data,'result_review',dict(status='REVIEW_RECEIVED',exit_code=0,bundle_sha256=b['sha256'],stdout_sha256=t['sha256'],
        review=decision,files=[{k:r[k] for k in ('path','sha256')} for r in rows]))


@pytest.mark.parametrize('fault',['missing_check','duplicate_check','result_review_missing','source_content','run_output','inventory_bool'])
def test_full_evidence_contract(pressure,fault):
    if fault=='result_review_missing':pressure['receipt']['sources'].pop('result_review')
    elif fault=='source_content':
        d=copy.deepcopy(pressure['docs']['review_bundle']);d['files'][0]['content']='changed';bind(pressure,'review_bundle',d)
    elif fault=='run_output':
        d=copy.deepcopy(pressure['docs']['claim']);d['output']='other-run';bind(pressure,'claim',d);bind(pressure,'attempt',d)
    else:
        d=copy.deepcopy(pressure['docs']['capture'])
        if fault=='missing_check':d['checks'].pop()
        elif fault=='duplicate_check':d['checks'].append(d['checks'][0])
        else:d['inventory'][0]['bytes']=True
        bind(pressure,'capture',d);result_review_fixture(pressure)
    with pytest.raises(ValueError):build(pressure)


def test_engineering_lookup_refuses_pressure(pressure):
    from digitalmodel.ansys.analysis_lookup import _qualify
    package=build(pressure);case=package['cases'][9]
    with pytest.raises(ValueError,match='cannot qualify'):
        _qualify(package,case,case['responses'][0],'engineering',{},pressure['resolver'])


def test_unchanged_zero_replay_does_not_hijack_pressure(pressure,monkeypatch):
    from tests.ansys.test_analysis_replay import replay_case_fixture
    import digitalmodel.ansys.analysis_replay as replay
    base=pressure['base'];old=base['cases'][8];fake=replay_case_fixture()
    old.update({k:v for k,v in fake.items() if k not in ('responses','evidence')})
    for name in ('replay_reference','check_reference','observation_reference'):
        ref=reference(pressure['tmp'],pressure['resolver'],'historical-'+name,{'synthetic':name})
        old[name]=dict(ref,required=True);old['evidence'].append(dict(ref,required=True,role=name))
    for row,other in zip(old['responses'],fake['responses']):row.update(other)
    pressure['base']=rehash(base,pressure['resolver'])
    monkeypatch.setattr(replay,'validate_replay_transition',lambda *a,**k:pytest.fail('zero updater called'))
    candidate=build(pressure)
    _relation(candidate,pressure['base'],pressure['resolver'])
    assert candidate['cases'][8]==pressure['base']['cases'][8]


@pytest.mark.parametrize('fault',['missing','stale','revision'])
def test_intake_fingerprint_refuses(pressure,fault):
    from digitalmodel.ansys.analysis_pressure_observed import build_pressure_observed_package,intake_code_files
    files=intake_code_files();code_revision=digest_bytes(canonical_bytes(files))
    if fault=='missing':files.pop(next(iter(files)))
    elif fault=='stale':files[next(iter(files))]='f'*64
    else:code_revision='f'*64
    ref=reference(pressure['tmp'],pressure['resolver'],'pressure-intake',pressure['receipt'])
    with pytest.raises(ValueError):
        build_pressure_observed_package(pressure['base'],pressure['resolver'],observation_reference=ref,
            revision='r5',code_files=files,code_revision=code_revision,source_revision='a'*40)


def test_empty_stream_limitation_is_mandatory(pressure):
    pressure['receipt']['limitations']=[]
    with pytest.raises(ValueError):build(pressure)


@pytest.mark.parametrize('raw',[b'{"cost":0.0234,"cost":0.02}',b'{"cost":NaN}',b'{"cost":Infinity}',b'{"cost":-Infinity}'])
def test_provider_metadata_rejects_invalid_json(raw):
    from digitalmodel.ansys.analysis_pressure_inputs import decode_document
    with pytest.raises(ValueError):decode_document(raw)


def test_provider_cost_metadata_preserves_finite_lexeme():
    from digitalmodel.ansys.analysis_pressure_inputs import decode_document
    raw=b'{"total_cost_usd":0.01234567890123456789,"modelUsage":{"synthetic":{"costUSD":1e-7}},"is_error":false}'
    data=decode_document(raw)
    assert data['total_cost_usd']=='0.01234567890123456789'
    assert data['modelUsage']['synthetic']['costUSD']=='1e-7'
    assert data['is_error'] is False


@pytest.mark.parametrize('role,raw',[('execution',b'{"duration_seconds":18.234}'),
    ('capture',b'{"accepted_values":{"stress":0.0}}'),('preflight',b'{"capacity":2.5}')])
def test_engineering_document_floats_refused(role,raw):
    from digitalmodel.ansys.analysis_pressure_inputs import document
    with pytest.raises(ValueError):document(role,raw)


def test_provider_role_keeps_telemetry_lexeme():
    from digitalmodel.ansys.analysis_pressure_inputs import document
    assert document('review_transport',b'{"cost":0.25}')['cost']=='0.25'


def test_source_inventory_includes_changed_intake_modules():
    from digitalmodel.ansys.analysis_pressure_observed import intake_code_files
    inventory=intake_code_files();root=Path(__file__).resolve().parents[2]
    for name in ('analysis_pressure_inputs.py','analysis_pressure_observed.py','analysis_records.py'):
        key='src/digitalmodel/ansys/'+name
        assert inventory[key]==digest_bytes((root/key).read_bytes())


def test_historical_source_narrowing_refuses(pressure):
    from digitalmodel.ansys.analysis_pressure_inputs import validate_execution_sources
    config=copy.deepcopy(pressure['docs']['config']);config['source_files']=config['source_files'][:1]
    with pytest.raises(ValueError):validate_execution_sources(config)


def test_distinct_production_and_result_sessions_required(pressure):
    d=copy.deepcopy(pressure['docs']['result_review_transport'])
    d['session_id']=pressure['docs']['review_transport']['session_id']
    ref=bind(pressure,'result_review_transport',d)
    review=copy.deepcopy(pressure['docs']['result_review']);review['stdout_sha256']=ref['sha256']
    bind(pressure,'result_review',review)
    with pytest.raises(ValueError):build(pressure)


def test_coverage_counts_actual_cases(pressure):
    base=pressure['base'];base['cases'][10]['capture_role']='fixture'
    pressure['base']=rehash(base,pressure['resolver']);result=build(pressure)
    assert result['coverage']['pending_cases']==1
    assert result['coverage']['pending_responses']==64


def test_qualified_baseline_counter_refuses(pressure):
    pressure['base']['coverage']['qualified_responses']=1
    pressure['base']=rehash(pressure['base'],pressure['resolver'])
    with pytest.raises(ValueError):build(pressure)


def historical_proof_fixture(data,monkeypatch):
    """Test-only anchors; production constants bind the independently retained Git proof."""
    import digitalmodel.ansys.analysis_pressure_inputs as inputs
    config=data['docs']['config'];content='synthetic historical source'
    config['source_files']=[dict(path=p,sha256=digest_bytes(content.encode())) for p in sorted(inputs.CORE_SOURCES)]
    cfg=bind(data,'config',config)
    bundle=data['docs']['review_bundle']
    bundle['files']=[dict(path=r['path'],sha256=r['sha256'],content=content) for r in config['source_files']]
    bundle['files'].append(dict(path='config.json',sha256=cfg['sha256'],content=canonical_bytes(config).decode()))
    b=bind(data,'review_bundle',bundle);decision=dict(bundle_sha256=b['sha256'],verdict='MINOR',findings=[])
    t=bind(data,'review_transport',dict(is_error=False,session_id='synthetic-checker',structured_output=decision))
    review=bind(data,'review',dict(status='REVIEW_RECEIVED',exit_code=0,bundle_sha256=b['sha256'],
        stdout_sha256=t['sha256'],review=decision,files=[{k:r[k] for k in ('path','sha256')} for r in bundle['files']]))
    for role in ('claim','attempt','invocation'):
        d=copy.deepcopy(data['docs'][role]);d.update(config_sha256=cfg['sha256'],review_receipt_sha256=review['sha256']);bind(data,role,d)
    rows=[dict(source_path=r['path'],git_revision=config['source_revision'],comparison='MATCH',
        **{k:r['sha256'] for k in ('git_blob_sha256','configuration_sha256','working_sha256','copied_readback_sha256')})
        for r in config['source_files']]
    ref=bind(data,'source_provenance',dict(git_revision=config['source_revision'],configuration_sha256=cfg['sha256'],files=rows))
    monkeypatch.setattr(inputs,'EXECUTION_REVISION',config['source_revision'])
    monkeypatch.setattr(inputs,'EXECUTION_INVENTORY_SHA',digest_bytes(canonical_bytes({r['path']:r['sha256'] for r in config['source_files']})))
    monkeypatch.setattr(inputs,'SOURCE_PROVENANCE_SHA',ref['sha256'])


def test_invocation_timestamp_bound_not_completion(pressure):
    d=copy.deepcopy(pressure['docs']['invocation']);d['start_utc_unix_ns']=1789479503640863800
    bind(pressure,'invocation',d)
    case=build(pressure)['cases'][9]
    assert case['generated_at']=='2026-09-15T13:38:23.640863800Z'
    assert case['generated_at_scope']=='native-invocation-start-not-completion'


@pytest.mark.parametrize('value',[None, True, -1, '1789479503640863800'])
def test_invocation_invalid_timestamp_refused(pressure,value):
    d=copy.deepcopy(pressure['docs']['invocation']);d['start_utc_unix_ns']=value
    bind(pressure,'invocation',d)
    with pytest.raises(ValueError,match='invocation start'):build(pressure)


def test_coverage_separates_historical_counts(pressure):
    pressure['base']['coverage'].update(native_captures=6,observed_transition_native_attempts=1)
    pressure['base']=rehash(pressure['base'],pressure['resolver'])
    c=build(pressure)['coverage']
    assert 'native_captures' not in c and 'observed_transition_native_attempts' not in c
    assert c['historical_baseline_counts']==dict(native_captures=6,observed_transition_native_attempts=1)
    assert c['recorded_native_attempts']==1
    assert c['recorded_native_attempts_scope']=='sum-explicit-case-native_attempt_count; absent-counts-unknown'


def test_stored_fingerprint_survives_unrelated_source_changes(pressure,monkeypatch):
    import digitalmodel.ansys.analysis_pressure_observed as module
    result=build(pressure)
    monkeypatch.setattr(module,'intake_code_files',lambda: {'unrelated':'f'*64})
    module.validate_pressure_observed_transition(result,pressure['base'],pressure['resolver'])


def test_package_timestamp_does_not_predate_intaken_run(pressure):
    package=build(pressure)
    assert package['generated_at']==package['cases'][9]['generated_at']
    assert package['generated_at_scope']=='native-invocation-start-not-completion'


def test_source_inventory_scope_is_selected(pressure):
    assert build(pressure)['code_inventory_scope']=='selected-pressure-intake-record-validation-v1'
