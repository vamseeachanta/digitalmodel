"""Offline authority/adaptation refusals; no executable invocation."""
import json
from pathlib import Path

import pytest

from digitalmodel.ansys.cylinder_adapter import (
    assessment_for_records, reference_table, validate_runtime_profile,
)
from digitalmodel.ansys.cylinder_reference import reference_text


def rows():
    return [{'pressure_mpa': p, 'station_id': f'{radial}_y{y}',
             'values': reference_text(p, r, str(y))}
            for p in ('0', '10') for radial, r in
            [('inner', '750'), ('middle', '780'), ('outer', '810')]
            for y in (60, 120, 180)]


def test_reference_table_refuses_duplicates_missing_and_wrong_units():
    data = {'units': {'stress': 'MPa', 'displacement': 'mm'}, 'rows': rows()}
    assert len(reference_table(data)['10']) == 63
    for bad in [{**data, 'rows': rows()[:-1]}, {**data, 'rows': rows()+rows()[:1]},
                {**data, 'units': {'stress': 'Pa', 'displacement': 'mm'}}]:
        with pytest.raises(ValueError):
            reference_table(bad)


def test_runtime_profile_requires_each_bounded_native_identity():
    profile = {'release': '2026 R1.01', 'build': '26.1', 'update': '20260202', 'platform': 'WINDOWS x64'}
    raw = b' RELEASE= 2026 R1.01           BUILD= 26.1      UP20260202   VERSION=WINDOWS x64 \n'
    validate_runtime_profile(raw, profile)
    with pytest.raises(ValueError):
        validate_runtime_profile(raw.replace(b'26.1', b'126.10'), profile)
    with pytest.raises(ValueError):
        validate_runtime_profile(raw, {})


def test_adapter_keeps_coarse_numerical_failure_diagnostic():
    data = {'units': {'stress': 'MPa', 'displacement': 'mm'}, 'rows': rows()}
    table = reference_table(data)
    records = []
    for case, p in [('ocv-zero-t60-n16', '0'), ('ocv-t60-p10-n4', '10')]:
        values = {s: {q: str(v) for (name, q), v in table[p].items() if name == s}
                  for s in {k[0] for k in table[p]}}
        records.append({'case_id': case, 'values': values, 'rfy_sum': '0', 'evidence_errors': []})
    records[-1]['values']['inner_y120']['sigma_theta'] = '99999'
    result = assessment_for_records(records, table)
    assert result['status'] == 'PASS'  # operator continuation, with failed diagnostic check retained
    assert any(not check['passed'] for check in result['checks'])


@pytest.mark.parametrize('fault',['empty','order','missing_value','nonfinite','stopped'])
def test_assessment_refuses_invalid_or_post_stop_evidence(fault):
    table=reference_table({'units':{'stress':'MPa','displacement':'mm'},'rows':rows()})
    values={station:{q:str(v) for (sid,q),v in table['0'].items() if sid==station}
            for station in {k[0] for k in table['0']}}
    records=[{'case_id':'ocv-zero-t60-n16','values':values,'rfy_sum':'0','evidence_errors':[]}]
    if fault=='empty':records=[]
    if fault=='order':records[0]['case_id']='ocv-t60-p10-n4'
    if fault=='missing_value':del values['inner_y120']['sigma_r']
    if fault=='nonfinite':values['inner_y120']['sigma_r']='NaN'
    if fault=='stopped':
        records[0]['evidence_errors']=['Missing native state']
        records.append({**records[0],'case_id':'ocv-t60-p10-n4'})
    assert assessment_for_records(records,table)['status']=='INCOMPLETE'


def artifact_directory(tmp_path):
    import hashlib
    directory=tmp_path/'attempt';directory.mkdir()
    names=['case.inp','case.out','file.err','file.rst','file.db','file.mntr',
           'model.cdb','station_values.txt','state_values.txt','support_reactions.txt',
           'precision_witness.txt','stdout.bin','stderr.bin']
    for name in names:(directory/name).write_bytes(b'' if name in ('file.err','stdout.bin','stderr.bin') else b'synthetic\n')
    profile={'release':'2026 R1.01','build':'26.1','update':'20260202','platform':'WINDOWS x64'}
    (directory/'case.out').write_bytes(b' RELEASE= 2026 R1.01           BUILD= 26.1      UP20260202   VERSION=WINDOWS x64 \n')
    reference=tmp_path/'reference.json';reference.write_bytes(b'synthetic reference\n')
    execution={'return_code':0,'timed_out':False,'owned_processes_remaining':0,
               'containment_verified':True,'evidence_complete':True,'settlement_required':False,
               'streams_finalized':True,'stdout':b'','stderr':b''}
    return directory,reference,hashlib.sha256(reference.read_bytes()).hexdigest(),profile,execution


def test_extractor_binds_raw_artifacts_and_preserves_incomplete(tmp_path,monkeypatch):
    from digitalmodel.ansys import cylinder_adapter as adapter
    directory,reference,digest,profile,execution=artifact_directory(tmp_path)
    monkeypatch.setattr(adapter,'_frozen_case',lambda case_id:{'case_id':case_id,'deck_bytes':b'synthetic\n'})
    calls=[]
    def parse(**kwargs):
        calls.append(kwargs)
        return {'status':'INCOMPLETE','errors':['Native layout unestablished'],'values':{},'rfy_sum':None}
    monkeypatch.setattr(adapter,'validate_native_evidence',parse)
    row=adapter.extract_record({'case_id':'ocv-zero-t60-n16','deck':'case.inp'},directory,execution,
        reference_path=reference,approved_reference_hash=digest,runtime_profile=profile)
    assert row['evidence_errors']==['Native layout unestablished']
    assert row['rfy_sum'] is None
    assert calls[0]['artifacts']['native.out']==(directory/'case.out').read_bytes()
    assert row['artifacts']['file.rst']['sha256']


@pytest.mark.parametrize('fault',['reference','deck','missing_rst','stream','timeout','profile'])
def test_extractor_refuses_unbound_or_incomplete_capture(tmp_path,monkeypatch,fault):
    from digitalmodel.ansys import cylinder_adapter as adapter
    directory,reference,digest,profile,execution=artifact_directory(tmp_path)
    monkeypatch.setattr(adapter,'_frozen_case',lambda case_id:{'case_id':case_id,'deck_bytes':b'synthetic\n'})
    def prohibited(**kwargs):raise AssertionError('Unbound evidence reached parser')
    monkeypatch.setattr(adapter,'validate_native_evidence',prohibited)
    if fault=='reference':reference.write_bytes(b'changed')
    if fault=='deck':(directory/'case.inp').write_bytes(b'changed')
    if fault=='missing_rst':(directory/'file.rst').unlink()
    if fault=='stream':(directory/'stdout.bin').write_bytes(b'changed')
    if fault=='timeout':execution['timed_out']=True
    if fault=='profile':profile['build']='126.10'
    with pytest.raises(ValueError):adapter.extract_record(
        {'case_id':'ocv-zero-t60-n16','deck':'case.inp'},directory,execution,
        reference_path=reference,approved_reference_hash=digest,runtime_profile=profile)


@pytest.mark.parametrize('fine_failure',[False,True])
def test_four_case_assessment_preserves_final_numeric_verdict(fine_failure):
    from digitalmodel.ansys.cylinder_criteria import CASE_IDS
    table=reference_table({'units':{'stress':'MPa','displacement':'mm'},'rows':rows()})
    records=[]
    for index,case_id in enumerate(CASE_IDS):
        source=table['0' if index==0 else '10']
        values={station:{q:str(v) for (sid,q),v in source.items() if sid==station}
                for station in {k[0] for k in source}}
        records.append({'case_id':case_id,'values':values,'rfy_sum':'0','evidence_errors':[]})
    if fine_failure:records[-1]['values']['inner_y120']['sigma_theta']='99999'
    assert assessment_for_records(records,table)['status']==('FAIL' if fine_failure else 'PASS')


def test_explicit_deck_basename_is_supported_without_deck_locator(tmp_path,monkeypatch):
    from digitalmodel.ansys import cylinder_adapter as adapter
    directory,reference,digest,profile,execution=artifact_directory(tmp_path)
    monkeypatch.setattr(adapter,'_frozen_case',lambda case_id:{'case_id':case_id,'deck_bytes':b'synthetic\n'})
    monkeypatch.setattr(adapter,'validate_native_evidence',lambda **kwargs:
        {'status':'INCOMPLETE','errors':['unestablished'],'values':{},'rfy_sum':None})
    result=adapter.extract_record({'case_id':'ocv-zero-t60-n16','deck_basename':'case.inp'},
        directory,execution,reference_path=reference,approved_reference_hash=digest,runtime_profile=profile)
    assert result['evidence_errors']==['unestablished']



def test_execution_factory_requires_profile_and_rechecks_executable(tmp_path,monkeypatch):
    import hashlib
    from digitalmodel.ansys import cylinder_adapter as adapter
    bundle=tmp_path/'bundle';bundle.mkdir()
    reference=bundle/'reference.json'
    reference.write_text(json.dumps({'units':{'stress':'MPa','displacement':'mm'},'rows':rows()}))
    manifest={'reference':'reference.json'}
    (bundle/'manifest.json').write_text(json.dumps(manifest))
    exe=tmp_path/'solver.exe';exe.write_bytes(b'synthetic nonexecutable')
    approval={'executable_sha256':hashlib.sha256(exe.read_bytes()).hexdigest(),
              'manifest_sha256':hashlib.sha256((bundle/'manifest.json').read_bytes()).hexdigest(),
              'runtime_profile':{'release':'2026 R1.01','build':'26.1','update':'20260202','platform':'WINDOWS x64'}}
    monkeypatch.setattr(adapter,'verify_prepared_reference',lambda path:True)
    calls=[]
    monkeypatch.setattr(adapter,'_launch_case',lambda *args:calls.append(args) or {'mock':True})
    bound=adapter.make_execution_adapters(bundle,exe,approval)
    assert set(bound)=={'launch','extract','assess','verify_reference'}
    case={'case_id':'ocv-zero-t60-n16','deck':'decks/case.inp'}
    from digitalmodel.ansys.cylinder_benchmark import build_case
    (tmp_path/'case.inp').write_bytes(build_case(case['case_id'])['deck_bytes'])
    assert bound['launch'](case,tmp_path,300)=={'mock':True}
    assert calls[0][0]['deck_basename']=='case.inp'
    (tmp_path/'case.inp').write_bytes(b'/EXIT,NOSAVE\n')
    with pytest.raises(ValueError,match='frozen'):
        bound['launch'](case,tmp_path,300)
    assert len(calls)==1
    exe.write_bytes(b'changed')
    with pytest.raises(ValueError):bound['launch'](case,tmp_path,300)
    with pytest.raises(ValueError):adapter.make_execution_adapters(bundle,exe,{})
    invalid={**approval,'runtime_profile':{**approval['runtime_profile'],'build':''}}
    with pytest.raises(ValueError):adapter.make_execution_adapters(bundle,exe,invalid)



@pytest.mark.parametrize('fault',['comments','swapped','duplicate','conflict','split','missing'])
def test_runtime_profile_requires_one_complete_labeled_native_header(fault):
    profile={'release':'2026R1','build':'26.1','update':'20260202','platform':'WINX64'}
    header=b' RELEASE= 2026R1 BUILD= 26.1 UP20260202 VERSION=WINX64\n'
    raw=header
    if fault=='comments':raw=b'/COM, Expected 2026R1 26.1 20260202 WINX64\n'
    if fault=='swapped':raw=b' RELEASE= 26.1 BUILD= 2026R1 UP20260202 VERSION=WINX64\n'
    if fault=='duplicate':raw=header+header
    if fault=='conflict':raw=header.replace(b'BUILD= 26.1',b'BUILD= 26.2')+b'/COM, Expected 26.1\n'
    if fault=='split':raw=header.replace(b' BUILD=',b'\n BUILD=')
    if fault=='missing':raw=b'2026R1 26.1 20260202 WINX64\n'
    with pytest.raises(ValueError):validate_runtime_profile(raw,profile)



def test_utf8_nonprofile_status_text_does_not_break_ascii_profile_fields():
    profile={'release':'2026 R1.01','build':'26.1','update':'20260202','platform':'WINDOWS x64'}
    raw=('Synthetic status: Unicode \u2014 separate from identity.\n'
         ' RELEASE= 2026 R1.01 BUILD= 26.1 UP20260202 VERSION=WINDOWS x64\n').encode('utf-8')
    validate_runtime_profile(raw,profile)
    with pytest.raises(UnicodeDecodeError):validate_runtime_profile(b'\xff'+raw,profile)
