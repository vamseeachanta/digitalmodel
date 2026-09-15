"""One metadata-only pressure transition; historical bytes remain independently pinned."""
from copy import deepcopy
from decimal import Decimal
from pathlib import PureWindowsPath
from digitalmodel.ansys.analysis_records import canonical_bytes,decimal_text,digest_bytes
from digitalmodel.ansys.analysis_evidence import build_package,validate_package
from digitalmodel.ansys.analysis_pressure_inputs import CASE_ID,REASON,UNVERIFIED,load_inputs,require,reference,invocation_start


def intake_code_files():
    from digitalmodel.ansys.analysis_pressure_source import source_inventory
    return source_inventory()


def _fingerprint(package):
    from digitalmodel.ansys.analysis_pressure_source import validate_recorded_source
    validate_recorded_source(package)
    require(package.get('code_inventory_scope')=='selected-pressure-intake-record-validation-v1',
        'selected intake inventory scope differs')



def response_units():
    quantities=('sigma_r','sigma_theta','sigma_z','tau_rz','sigma_vm','u_r','u_z')
    return {**{f'{r}_y{y}.{q}': 'mm' if q.startswith('u_') else 'MPa'
        for y in (60,120,180) for r in ('inner','middle','outer') for q in quantities},'support.RFY':'N'}


def _responses(case):
    rows=case['responses']; units=response_units()
    require(len(rows)==64 and {r['name'] for r in rows}==set(units),'pressure response membership differs')
    for row in rows:
        require(row['unit']==units[row['name']] and row['value'] is None
                and row['calculation_status']=='not_evaluated' and 'observed_value' not in row,
                'pressure response must remain typed null/not_evaluated')


def validate_pressure_observed_case(case):
    require(case['case_id']==CASE_ID and case.get('attempt_consumed') is True
            and type(case.get('native_attempt_count')) is int and case['native_attempt_count']==1,
            'fixed pressure attempt required')
    for key,value in dict(capture_role='diagnostic_capture',source_kind='native',execution_status='completed',
        assessment_status='not_evaluated',author='SOLVERS',author_status='recorded',
        input_descriptor_scope='historical-prelaunch-basis',engineering_qualified=False,
        generated_at_scope='native-invocation-start-not-completion').items():
        require(type(case.get(key)) is type(value) and case.get(key)==value,'pressure metadata differs: '+key)
    ref=case.get('observation_reference');reference(ref)
    require(any(r['id']==ref['id'] and r['sha256']==ref['sha256'] and r['role']=='diagnostic_intake'
                and r['required'] is True for r in case['evidence']),'intake reference missing')
    execution=case.get('observed_execution')
    require(isinstance(execution,dict) and set(execution)=={'return_code','duration_seconds'},'execution shape differs')
    require(type(execution['return_code']) is int and execution['return_code']==0
            and decimal_text(execution['duration_seconds'])==execution['duration_seconds']
            and Decimal(execution['duration_seconds'])>0,'execution duration/status invalid')
    capture=case.get('observed_capture');require(isinstance(capture,dict),'observed capture missing')
    expected=dict(capture_status='INCOMPLETE',retention_status='COMPLETE',independent_check_status='COMPLETE',
        numerical_assessment='NOT_EVALUATED',reason=REASON,accepted_values={},unverified_evidence=UNVERIFIED)
    require(set(capture)==set(expected)|{'campaign_id','run_id'},'observed capture shape differs')
    for k,v in expected.items():require(canonical_bytes(capture[k])==canonical_bytes(v),'observed capture differs')
    require(all(isinstance(capture[k],str) and capture[k] for k in ('campaign_id','run_id')),'capture identifiers missing')
    _responses(case)
    for row in case['responses']:
        require('diagnostic-only' in row['limitations'] and REASON in row['limitations']
                and 'native-not-attempted' not in row['limitations'] and ref['id'] in row['evidence_ids'],
                'pressure limitations or receipt missing')


def _baseline(baseline):
    validate_package(baseline)
    require(baseline['dataset_id']=='ansys-retained-evidence' and len(baseline['cases'])==12
            and sum(len(c['responses']) for c in baseline['cases'])==350,'expected 12-case matrix')
    require(baseline['coverage'].get('qualified_responses')==0
            and not any(c.get('engineering_qualified') is True or c.get('capture_role')=='qualified_native'
                        for c in baseline['cases']),'qualified baseline not in diagnostic scope')
    old=baseline['cases'][9]
    require(old['case_id']==CASE_ID and old['capture_role']=='pending_native'
            and old.get('attempt_consumed') is False and old.get('native_attempt_count')==0,
            'expected pending pressure predecessor')
    _responses(old)
    return old


def _evidence(case,ref,receipt):
    existing={r['id']:r for r in case['evidence']}
    entries=[('diagnostic_intake',ref),*receipt['sources'].items(),
             *[('retained_capture',r) for r in receipt['raw'].values()]]
    for role,item in entries:
        require(item['id'] not in existing,'pressure evidence id collides with historical evidence')
        existing[item['id']]=dict(item,role=role,required=True)
    case['evidence']=list(existing.values())


def derive_pressure_observed_case(baseline,observation_reference,resolver):
    old=_baseline(baseline);receipt,d=load_inputs(old,observation_reference,resolver)
    case=deepcopy(old);case.pop('row_hash',None)
    case.update(generated_at=invocation_start(d['invocation']),
        generated_at_scope='native-invocation-start-not-completion')
    case.update(capture_role='diagnostic_capture',attempt_consumed=True,native_attempt_count=1,
        source_kind='native',execution_status='completed',assessment_status='not_evaluated',
        author='SOLVERS',author_status='recorded',engineering_qualified=False,
        input_descriptor_scope='historical-prelaunch-basis',observation_reference=deepcopy(observation_reference),
        observed_execution={k:d['execution'][k] for k in ('return_code','duration_seconds')})
    case['observed_capture']={k:deepcopy(d['capture'][k]) for k in ('capture_status','retention_status',
        'independent_check_status','numerical_assessment','reason','accepted_values','unverified_evidence')}
    case['observed_capture'].update(campaign_id=d['config']['campaign_id'],
        run_id=PureWindowsPath(d['claim']['output']).name)
    _evidence(case,observation_reference,receipt)
    for row in case['responses']:
        row['limitations']=list(dict.fromkeys([v for v in row['limitations'] if v!='native-not-attempted']+['diagnostic-only',REASON]))
        row['evidence_ids']=list(dict.fromkeys(row['evidence_ids']+[observation_reference['id']]))
    validate_pressure_observed_case(case)
    case['row_hash']=digest_bytes(canonical_bytes(case))
    return case


def _coverage(baseline,cases):
    pending=[c for c in cases if c['capture_role']=='pending_native']
    require(not any(c.get('engineering_qualified') is True for c in cases),'diagnostic cases cannot qualify')
    coverage=deepcopy(baseline['coverage'])
    historical=dict(coverage.get('historical_baseline_counts',{}))
    for key in ('native_captures','observed_transition_native_attempts'):
        if key in coverage:
            require(key not in historical or historical[key]==coverage[key],'historical counter collision')
            historical[key]=coverage.pop(key)
    coverage['historical_baseline_counts']=historical
    native=[c for c in cases if c.get('source_kind')=='native']
    known=[c['native_attempt_count'] for c in native if type(c.get('native_attempt_count')) is int]
    coverage.update(pending_cases=len(pending),
        pending_responses=sum(len(c['responses']) for c in pending),
        diagnostic_capture_cases=sum(c['capture_role']=='diagnostic_capture' for c in cases),
        recorded_native_attempts=sum(known),native_cases_without_attempt_count=len(native)-len(known),
        recorded_native_attempts_scope='sum-explicit-case-native_attempt_count; absent-counts-unknown',
        qualified_responses=0)
    return coverage


def validate_pressure_observed_transition(package,baseline,resolver):
    _baseline(baseline)
    _fingerprint(package)
    changed_fields = {'revision', 'previous_package_hash', 'package_hash', 'cases',
        'coverage', 'generated_at', 'generated_at_scope', 'code_revision',
        'source_revision', 'code_files', 'code_canonicalization',
        'code_inventory_scope', 'method_revision'}
    stable = lambda study: {k: v for k, v in study.items() if k not in changed_fields}
    require(canonical_bytes(stable(package)) == canonical_bytes(stable(baseline)),
            'study metadata differs outside the pressure transition')
    require(package.get('method_revision') == 'pressure-diagnostic-intake-1',
            'pressure method revision differs')
    require(len(package['cases'])==12 and package['expected_cases']==baseline['expected_cases'],'matrix membership differs')
    for i in range(12):
        if i!=9:require(canonical_bytes(package['cases'][i])==canonical_bytes(baseline['cases'][i]),'other historical case changed')
    expected=derive_pressure_observed_case(baseline,package['cases'][9]['observation_reference'],resolver)
    require(canonical_bytes(expected)==canonical_bytes(package['cases'][9]),'pressure derivation differs')
    require(canonical_bytes(package.get('coverage'))==canonical_bytes(_coverage(baseline,package['cases'])),
            'coverage differs')
    for key in ('generated_at','generated_at_scope'):
        require(package.get(key)==expected[key],'pressure package timestamp differs')


def build_pressure_observed_package(baseline,resolver,*,observation_reference,revision,code_revision,source_revision,code_files):
    case=derive_pressure_observed_case(baseline,observation_reference,resolver)
    require(all(isinstance(v,str) and v for v in (revision,code_revision,source_revision))
            and revision!=baseline['revision'],'explicit new revisions required')
    study=deepcopy(baseline);study.pop('package_hash',None)
    study.update(revision=revision,code_revision=code_revision,source_revision=source_revision,
        previous_package_hash=baseline['package_hash'],method_revision='pressure-diagnostic-intake-1',
        code_files=deepcopy(code_files),code_canonicalization='raw-sha256-v1',
        code_inventory_scope='selected-pressure-intake-record-validation-v1',
        generated_at=case['generated_at'],generated_at_scope=case['generated_at_scope'])
    _fingerprint(study)
    from digitalmodel.ansys.analysis_pressure_source import verify_build_source
    verify_build_source(source_revision,code_files)
    study['cases'][9]=case
    study['coverage']=_coverage(baseline,study['cases'])
    for row in study['cases']:row.pop('row_hash',None)
    result=build_package(study,resolver)
    validate_pressure_observed_transition(result,baseline,resolver)
    return result
