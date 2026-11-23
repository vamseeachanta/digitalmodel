"""Synthetic N4 capture contracts; no native execution or reference derivation."""
import importlib
import inspect
from pathlib import Path
import pytest
from digitalmodel.ansys import cylinder_adapter as helpers
from digitalmodel.ansys.cylinder_benchmark import build_case
from tests.ansys.cylinder_synthetic_protocol import cdb, dedicated_exports, native_output

CASE_ID='ocv-t60-p10-n4'
PROFILE=dict(release='2026 R1.01',build='26.1',update='20260202',platform='WINDOWS x64')


def module():
    return importlib.import_module('digitalmodel.ansys.cylinder_pressure_capture')


def fixture(tmp_path):
    case=build_case(CASE_ID)
    numerical={n['node_id']:{q:'0' for q in ('sigma_r','sigma_theta','sigma_z','tau_rz','u_r','u_z')}
               for n in case['nodes']}
    artifacts=dedicated_exports(case,numerical)
    header=b'RELEASE= 2026 R1.01 BUILD= 26.1 UP20260202 VERSION=WINDOWS x64\n'
    title=('Open cylinder verification '+case['case_token']+'\n').encode()
    artifacts.update({'model.cdb':cdb(case),CASE_ID+'.out':header+title+native_output(case,numerical),
        CASE_ID+'.inp':case['deck_bytes'],'file.err':b'','stdout.bin':b'','stderr.bin':b'',
        'file.rst':b'synthetic binary','file.db':b'synthetic binary','file.mntr':b'synthetic monitor'})
    for name,raw in artifacts.items():(tmp_path/name).write_bytes(raw)
    execution=dict(return_code=0,timed_out=False,owned_processes_remaining=0,
        containment_verified=True,evidence_complete=True,settlement_required=False,
        streams_finalized=True,stdout=b'',stderr=b'')
    return case,execution


def test_capture_never_accepts_pressure_numerics(tmp_path,monkeypatch):
    case,execution=fixture(tmp_path)
    import digitalmodel.ansys.cylinder_results_validation as numerical
    def prohibited(*a,**k):raise AssertionError('Numerical parser called')
    monkeypatch.setattr(numerical,'validate_native_evidence',prohibited)
    monkeypatch.setattr(helpers,'extract_record',prohibited)
    result=module().capture_pressure(case,tmp_path,execution,runtime_profile=PROFILE)
    assert result['retention_status']=='COMPLETE'
    assert result['independent_check_status']=='COMPLETE'
    assert result['capture_status']=='INCOMPLETE'
    assert result['accepted_values']=={}
    assert result['numerical_assessment']=='NOT_EVALUATED'
    assert result['reason']=='PRESSURE_PARSER_NOT_VALIDATED'
    assert result['engineering_qualified'] is False
    assert len(result['inventory'])==13


@pytest.mark.parametrize('fault,status',[('missing','missing'),('empty','empty'),
    ('oserror','read_error'),('valueerror','invalid')])
def test_partial_inventory_continues_after_failure(tmp_path,monkeypatch,fault,status):
    case,execution=fixture(tmp_path);path=tmp_path/'model.cdb'
    if fault=='missing':path.unlink()
    elif fault=='empty':path.write_bytes(b'')
    else:
        original=helpers._read_owned
        def failed(root,name,**kwargs):
            if name=='model.cdb':raise OSError('synthetic') if fault=='oserror' else ValueError('synthetic')
            return original(root,name,**kwargs)
        monkeypatch.setattr(helpers,'_read_owned',failed)
    (tmp_path/'extra.log').write_bytes(b'auxiliary')
    result=module().capture_pressure(case,tmp_path,execution,runtime_profile=PROFILE)
    rows={r['path']:r for r in result['inventory']}
    assert result['capture_status']=='INCOMPLETE'
    assert rows['model.cdb']['status']==status
    assert rows['extra.log']['status']=='retained'
    assert rows['extra.log']['role']=='auxiliary'
    assert rows['support_reactions.txt']['status']=='retained'
    assert result['accepted_values']=={}


def test_missing_stream_does_not_call_execution_helper(tmp_path,monkeypatch):
    case,execution=fixture(tmp_path);(tmp_path/'stdout.bin').unlink()
    def prohibited(*a,**k):raise AssertionError('Missing stream indexed')
    monkeypatch.setattr(helpers,'_execution_bytes',prohibited)
    result=module().capture_pressure(case,tmp_path,execution,runtime_profile=PROFILE)
    assert result['capture_status']=='INCOMPLETE'
    assert any(r['check']=='execution' and r['status']=='INCOMPLETE' for r in result['checks'])


@pytest.mark.parametrize('fault',['profile','title','config','warning','state','witness','deck','stream'])
def test_independent_checks_refuse(tmp_path,fault):
    case,execution=fixture(tmp_path);out=tmp_path/(CASE_ID+'.out')
    if fault=='profile':out.write_bytes(out.read_bytes().replace(b'26.1',b'26.2'))
    elif fault=='title':out.write_bytes(out.read_bytes().replace(case['case_token'].encode(),b'OTHER'))
    elif fault=='config':out.write_bytes(out.read_bytes().replace(b'NMERR=200',b'UNKNOWN STATUS'))
    elif fault=='warning':out.write_bytes(out.read_bytes()+b'*** WARNING *** CP=1 TIME=00:00:01\n')
    elif fault=='stream':(tmp_path/'stderr.bin').write_bytes(b'nonempty')
    else:(tmp_path/{'state':'state_values.txt','witness':'precision_witness.txt','deck':CASE_ID+'.inp'}[fault]).write_bytes(b'corrupt')
    result=module().capture_pressure(case,tmp_path,execution,runtime_profile=PROFILE)
    assert result['capture_status']=='INCOMPLETE'
    assert result['independent_check_status']=='INCOMPLETE'
    assert result['accepted_values']=={}


def test_unknown_pressure_rows_do_not_gate_configuration(tmp_path):
    case,execution=fixture(tmp_path);out=tmp_path/(CASE_ID+'.out');raw=out.read_bytes()
    start=raw.index(b'ELEMENT FACE KVAL P1 P2');end=raw.index(b'***** ROUTINE COMPLETED',start)
    out.write_bytes(raw[:start]+b'SYNTHETIC UNKNOWN PRESSURE TABLE\n'+raw[end:])
    result=module().capture_pressure(case,tmp_path,execution,runtime_profile=PROFILE)
    assert result['retention_status']=='COMPLETE'
    assert result['independent_check_status']=='COMPLETE'
    assert result['capture_status']=='INCOMPLETE'
    assert result['numerical_assessment']=='NOT_EVALUATED'


def test_reused_private_helper_contract(tmp_path):
    assert list(inspect.signature(helpers._read_owned).parameters)==['root','name','retain','nonempty']
    assert list(inspect.signature(helpers._execution_bytes).parameters)==['execution','artifacts']
    root=tmp_path.resolve();(root/'x').write_bytes(b'synthetic')
    raw,metadata=helpers._read_owned(root,'x')
    assert raw==b'synthetic' and metadata['bytes']==9
    with pytest.raises(ValueError):helpers._read_owned(root,'absent')
    (root/'empty').write_bytes(b'')
    with pytest.raises(ValueError):helpers._read_owned(root,'empty',nonempty=True)


def test_redirected_artifact_is_distinct_and_preserved(tmp_path):
    case,execution=fixture(tmp_path);path=tmp_path/'model.cdb';path.unlink()
    outside=tmp_path.parent/(tmp_path.name+'-outside');outside.write_bytes(b'outside')
    try:path.symlink_to(outside)
    except OSError:pytest.skip('Symlink capability unavailable')
    result=module().capture_pressure(case,tmp_path,execution,runtime_profile=PROFILE)
    assert next(r for r in result['inventory'] if r['path']=='model.cdb')['status']=='redirected'
    assert path.is_symlink() and outside.read_bytes()==b'outside'


@pytest.mark.parametrize('field,value',[('return_code',1),('timed_out',True),
    ('owned_processes_remaining',1),('containment_verified',False),
    ('evidence_complete',False),('streams_finalized',False),('settlement_required',True)])
def test_execution_contract_failure_remains_separate(tmp_path,field,value):
    case,execution=fixture(tmp_path);execution[field]=value
    result=module().capture_pressure(case,tmp_path,execution,runtime_profile=PROFILE)
    assert result['execution_status']=='INCOMPLETE'
    assert result['capture_status']=='INCOMPLETE'
    assert result['execution_claim'][field]==value
    assert result['execution_claim']['return_code']==execution['return_code']
    assert result['accepted_values']=={}


def test_capture_reads_preserve_every_fixture_byte(tmp_path):
    import hashlib
    case,execution=fixture(tmp_path)
    before={p.name:hashlib.sha256(p.read_bytes()).hexdigest() for p in tmp_path.iterdir()}
    result=module().capture_pressure(case,tmp_path,execution,runtime_profile=PROFILE)
    assert before=={p.name:hashlib.sha256(p.read_bytes()).hexdigest() for p in tmp_path.iterdir()}
    assert {row['path']:row['sha256'] for row in result['inventory']}==before


@pytest.mark.parametrize('other',['ocv-zero-t60-n16','ocv-t60-p10-n8','ocv-t60-p10-n16'])
def test_capture_scope_refuses_other_cases(tmp_path,other):
    with pytest.raises(ValueError):module().capture_pressure({'case_id':other},tmp_path,{},runtime_profile=PROFILE)


@pytest.mark.parametrize('name',['station_values.txt','support_reactions.txt'])
def test_retained_unvalidated_exports_are_explicit_not_accepted(tmp_path,name):
    case,execution=fixture(tmp_path);(tmp_path/name).write_bytes(b'corrupt')
    result=module().capture_pressure(case,tmp_path,execution,runtime_profile=PROFILE)
    assert result['retention_status']=='COMPLETE'
    assert result['capture_status']=='INCOMPLETE'
    assert name in result['unverified_evidence']
    assert 'pressure_loads_and_support_conditions' in result['unverified_evidence']
    assert result['accepted_values']=={}


def test_dotdot_directory_is_canonical_before_helper(tmp_path):
    case,execution=fixture(tmp_path);(tmp_path/'child').mkdir()
    (tmp_path/'child').rmdir()
    # Existing intermediate directory is required for Windows path traversal.
    other=tmp_path.parent/(tmp_path.name+'-sibling');other.mkdir()
    alias=other/'..'/tmp_path.name
    result=module().capture_pressure(case,alias,execution,runtime_profile=PROFILE)
    assert result['retention_status']=='COMPLETE'


@pytest.mark.parametrize('fault',['missing','permission'])
def test_root_failures_have_value_error_contract(tmp_path,monkeypatch,fault):
    capture=module()
    if fault=='missing':directory=tmp_path/'absent'
    else:
        directory=tmp_path
        def denied(path):raise PermissionError('synthetic denied root')
        monkeypatch.setattr(capture,'_redirected',denied)
    with pytest.raises(ValueError):capture.inventory_capture(directory)


def test_binary_aux_and_unverified_exports_are_not_retained_in_memory(tmp_path,monkeypatch):
    fixture(tmp_path);(tmp_path/'extra.log').write_bytes(b'large synthetic auxiliary')
    original=helpers._read_owned;calls={}
    def observed(root,name,**kwargs):
        calls[name]=kwargs.get('retain',True)
        return original(root,name,**kwargs)
    monkeypatch.setattr(helpers,'_read_owned',observed)
    rows,artifacts=module().inventory_capture(tmp_path)
    for name in ('file.rst','file.db','file.mntr','extra.log','station_values.txt','support_reactions.txt'):
        assert calls[name] is False
        assert name not in artifacts
        assert next(r for r in rows if r['path']==name)['status']=='retained'


def test_empty_binary_is_not_masked_by_hash_only_mode(tmp_path):
    case,execution=fixture(tmp_path);(tmp_path/'file.rst').write_bytes(b'')
    result=module().capture_pressure(case,tmp_path,execution,runtime_profile=PROFILE)
    assert result['retention_status']=='INCOMPLETE'
    assert next(r for r in result['inventory'] if r['path']=='file.rst')['status']=='empty'


def test_every_unparsed_expected_file_is_disclosed(tmp_path):
    case,execution=fixture(tmp_path);capture=module()
    result=capture.capture_pressure(case,tmp_path,execution,runtime_profile=PROFILE)
    assert set(result['unverified_evidence']) == set(capture.EXPECTED)-capture.PARSED_FILES | {
        'pressure_loads_and_support_conditions','stress_and_reaction_recovery'}
    assert 'deliberate scope exclusion' in result['scope']


@pytest.mark.parametrize('extra',['scratch','with space.log'])
def test_auxiliary_anomalies_do_not_change_expected_retention(tmp_path,extra):
    case,execution=fixture(tmp_path)
    if extra=='scratch':(tmp_path/extra).mkdir()
    else:(tmp_path/extra).write_bytes(b'synthetic')
    result=module().capture_pressure(case,tmp_path,execution,runtime_profile=PROFILE)
    assert result['retention_status']=='COMPLETE'
    assert [row['path'] for row in result['auxiliary_anomalies']]==[extra]


@pytest.mark.parametrize('execution',[None,[],1])
def test_execution_requires_dict(tmp_path,execution):
    case,_=fixture(tmp_path)
    with pytest.raises(ValueError):module().capture_pressure(case,tmp_path,execution,runtime_profile=PROFILE)


def test_execution_claim_keeps_all_gates_without_streams(tmp_path):
    case,execution=fixture(tmp_path);(tmp_path/'stdout.bin').unlink()
    result=module().capture_pressure(case,tmp_path,execution,runtime_profile=PROFILE)
    fields={'return_code','timed_out','owned_processes_remaining','containment_verified',
            'evidence_complete','settlement_required','streams_finalized'}
    assert result['execution_claim']=={key:execution[key] for key in fields}
    assert result['execution_status']=='INCOMPLETE'
    assert 'execution_observation' not in result


def test_hardlinked_expected_file_has_distinct_status(tmp_path):
    import os
    case,execution=fixture(tmp_path);path=tmp_path/'file.rst'
    outside=tmp_path.parent/(tmp_path.name+'-hardlink');outside.write_bytes(path.read_bytes())
    path.unlink();os.link(outside,path)
    result=module().capture_pressure(case,tmp_path,execution,runtime_profile=PROFILE)
    row=next(row for row in result['inventory'] if row['path']=='file.rst')
    assert row['status']=='hardlinked' and row['link_count']>=2
    assert result['retention_status']=='INCOMPLETE' and path.exists()


@pytest.mark.parametrize('error',[TypeError,KeyError])
def test_dependency_errors_remain_explicit_nonacceptance(tmp_path,monkeypatch,error):
    case,execution=fixture(tmp_path);capture=module()
    def broken(*args,**kwargs):raise error('synthetic dependency defect')
    monkeypatch.setattr(capture,'classify_diagnostics',broken)
    result=capture.capture_pressure(case,tmp_path,execution,runtime_profile=PROFILE)
    check=next(row for row in result['checks'] if row['check']=='diagnostics')
    assert check['status']=='INCOMPLETE' and check['error_type']==error.__name__
    assert check['error_class']=='dependency_or_contract_error'
    assert result['independent_check_status']=='INCOMPLETE' and result['accepted_values']=={}
