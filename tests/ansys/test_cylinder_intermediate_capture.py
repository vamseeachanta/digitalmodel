"""Synthetic N8 capture controls; never native or qualified engineering evidence."""
import hashlib
import pytest
from digitalmodel.ansys import cylinder_pressure_capture as capture
from digitalmodel.ansys.cylinder_benchmark import build_case
from tests.ansys.cylinder_synthetic_protocol import cdb,dedicated_exports,native_output
from tests.ansys.test_cylinder_pressure_capture import PROFILE

N8='ocv-t60-p10-n8'


@pytest.fixture
def intermediate(tmp_path):
    case=build_case(N8)
    values={n['node_id']:{q:'0' for q in ('sigma_r','sigma_theta','sigma_z','tau_rz','u_r','u_z')}
            for n in case['nodes']}
    files=dedicated_exports(case,values)
    files.update({'model.cdb':cdb(case),N8+'.inp':case['deck_bytes'],
        N8+'.out':b'RELEASE= 2026 R1.01 BUILD= 26.1 UP20260202 VERSION=WINDOWS x64\n'+
            ('Open cylinder verification '+case['case_token']+'\n').encode()+native_output(case,values),
        'file.err':b'','stdout.bin':b'','stderr.bin':b'',
        'file.rst':b'synthetic binary','file.db':b'synthetic binary','file.mntr':b'synthetic monitor'})
    for name,raw in files.items():(tmp_path/name).write_bytes(raw)
    execution=dict(return_code=0,timed_out=False,owned_processes_remaining=0,
        containment_verified=True,evidence_complete=True,settlement_required=False,
        streams_finalized=True,stdout=b'',stderr=b'')
    return case,tmp_path,execution


def run(data,**kwargs):
    case,path,execution=data
    return capture.capture_pressure(case,path,execution,runtime_profile=PROFILE,capture_case_id=N8,**kwargs)


def test_explicit_intermediate_capture_never_adopts_values(intermediate,monkeypatch):
    import digitalmodel.ansys.cylinder_results_validation as numerical
    def prohibited(*args,**kwargs):raise AssertionError('numerical validation called')
    monkeypatch.setattr(numerical,'validate_native_evidence',prohibited)
    monkeypatch.setattr(capture.helpers,'extract_record',prohibited)
    result=run(intermediate)
    assert result['case_id']==N8
    assert result['retention_status']==result['independent_check_status']=='COMPLETE'
    assert result['capture_status']=='INCOMPLETE'
    assert result['numerical_assessment']=='NOT_EVALUATED'
    assert result['reason']=='PRESSURE_NUMERICAL_ASSESSMENT_NOT_EVALUATED'
    assert result['engineering_qualified'] is False and result['accepted_values']=={}
    expected={N8+'.inp',N8+'.out'}
    assert expected<={r['path'] for r in result['inventory'] if r['role']=='expected_capture'}
    assert not any('n4' in r['path'] for r in result['inventory'])


def test_legacy_default_still_refuses_intermediate(intermediate):
    case,path,execution=intermediate
    with pytest.raises(ValueError):capture.capture_pressure(case,path,execution,runtime_profile=PROFILE)


@pytest.mark.parametrize('identity',['ocv-t60-p10-n16','ocv-zero-t60-n16','../ocv-t60-p10-n8','OCV-T60-P10-N8',None,8])
def test_only_exact_pressure_scope_admitted(intermediate,identity):
    case,path,execution=intermediate
    with pytest.raises(ValueError):
        capture.capture_pressure(case,path,execution,runtime_profile=PROFILE,capture_case_id=identity)


def test_selected_case_and_descriptor_must_agree(intermediate):
    case,path,execution=intermediate
    with pytest.raises(ValueError):
        capture.capture_pressure(case,path,execution,runtime_profile=PROFILE,capture_case_id=capture.CASE_ID)


@pytest.mark.parametrize('fault',['deck','title','model','state','witness','stream'])
def test_intermediate_identity_evidence_refuses_mismatch(intermediate,fault):
    case,path,execution=intermediate
    if fault=='title':
        p=path/(N8+'.out');p.write_bytes(p.read_bytes().replace(b'P10N8',b'P10N4'))
    elif fault=='deck':(path/(N8+'.inp')).write_bytes(build_case(capture.CASE_ID)['deck_bytes'])
    elif fault=='model':(path/'model.cdb').write_bytes(cdb(build_case(capture.CASE_ID)))
    else:(path/{'state':'state_values.txt','witness':'precision_witness.txt','stream':'stderr.bin'}[fault]).write_bytes(b'corrupt')
    result=run(intermediate)
    assert result['independent_check_status']=='INCOMPLETE'
    assert result['accepted_values']=={} and result['engineering_qualified'] is False


def test_n8_inventory_retains_unparsed_values_without_adoption(intermediate):
    _,path,_=intermediate
    (path/'station_values.txt').write_bytes(b'not parsed')
    result=run(intermediate)
    assert result['retention_status']=='COMPLETE'
    assert 'station_values.txt' in result['unverified_evidence']
    assert result['accepted_values']=={}


def test_n8_missing_native_output_cannot_use_coarse_filename(intermediate):
    _,path,_=intermediate
    (path/(N8+'.out')).rename(path/(capture.CASE_ID+'.out'))
    result=run(intermediate)
    assert result['retention_status']==result['independent_check_status']=='INCOMPLETE'
    rows={r['path']:r for r in result['inventory']}
    assert rows[N8+'.out']['status']=='missing'
    assert rows[capture.CASE_ID+'.out']['role']=='auxiliary'


def test_n8_capture_does_not_modify_files(intermediate):
    _,path,_=intermediate
    before={p.name:hashlib.sha256(p.read_bytes()).hexdigest() for p in path.iterdir()}
    run(intermediate)
    assert before=={p.name:hashlib.sha256(p.read_bytes()).hexdigest() for p in path.iterdir()}


@pytest.mark.parametrize('name',['../ocv-t60-p10-n8.inp','ocv-t60-p10-n16.inp','ocv-zero-t60-n16.inp'])
def test_inventory_rejects_nonfixed_deck_names(tmp_path,name):
    with pytest.raises(ValueError):capture.inventory_capture(tmp_path,name)


@pytest.mark.parametrize('fault',['redirected','empty','missing'])
def test_n8_required_file_anomalies_stay_incomplete(intermediate,monkeypatch,fault):
    _,path,_=intermediate
    if fault=='redirected':
        original=capture._redirected
        monkeypatch.setattr(capture,'_redirected',lambda p:p==path/'file.db' or original(p))
    elif fault=='empty':(path/'file.db').write_bytes(b'')
    else:(path/'file.db').unlink()
    result=run(intermediate)
    assert result['retention_status']=='INCOMPLETE'
    assert result['accepted_values']=={} and result['engineering_qualified'] is False
    assert next(r for r in result['inventory'] if r['path']=='file.db')['status']==fault


def test_n8_frozen_mesh_counts_are_used(intermediate):
    case,_,_=intermediate
    assert case['radial_divisions']==8 and case['axial_divisions']==48
    assert len(case['nodes'])==1265 and len(case['elements'])==384
    result=run(intermediate)
    checks={r['check']:r['status'] for r in result['checks']}
    assert checks['model']==checks['state']==checks['case_titles']=='COMPLETE'
