import copy
from hashlib import sha256
import json
import math
from pathlib import Path
from types import SimpleNamespace

import pytest
import yaml

from digitalmodel.workflows import installation_numerical_prepare as prepare


def fixture(tmp_path):
    anchors={}
    for name in ('a','b'):
        root=tmp_path/name;root.mkdir()
        model={'General':{'UnitsSystem':'SI','ImplicitUseVariableTimeStep':False,
            'ImplicitConstantTimeStep':.05,'TargetLogSampleInterval':.1,'StageDuration':[80,600],'LogPrecision':'Single'},
            'Environment':{'WaveHs':1.,'WaveSeed':123},'6DBuoys':[{'Name':'Body','AddedMassCoefficient':[1.,2.,3.]}],
            'Lines':[{'Name':'Wire','LineType, Length, TargetSegmentLength':[['Steel',12.,1.2]],'Connection':'Body'}]}
        (root/'model.yml').write_text(yaml.safe_dump(model));model_sha=sha256((root/'model.yml').read_bytes()).hexdigest()
        request={'model':'model.yml','model_sha256':model_sha,'timeout_seconds':14400,'solver_version':'11.6c',
            'extraction':{'period':[0,600],'supplemental_profile':{'schema_version':1,
                'channels':[{'object':'Body','variable':'Z','units':'m'}],'geometry_lines':['Wire']}}}
        (root/'request.yml').write_text(yaml.safe_dump(request))
        generation={'case_id':name,'model_sha256':model_sha,'request_sha256':sha256((root/'request.yml').read_bytes()).hexdigest(),
            'factor':1.,'added_mass_t':3.,'status':'native_verified_not_run'}
        (root/'generation.json').write_text(json.dumps(generation))
        anchors[name]={'model_path':str(root/'model.yml'),'model_sha256':model_sha,
            'generation_sha256':sha256((root/'generation.json').read_bytes()).hexdigest(),'factor':1.,'added_mass_t':3.}
    cases=[]
    for name in anchors:
        cases.extend([{'id':name+'_log','source_case':name,'compare_to':name,'changes':{'General.TargetLogSampleInterval':.05}},
            {'id':name+'_time','source_case':name,'compare_to':name+'_log','changes':{'General.TargetLogSampleInterval':.05,'General.ImplicitConstantTimeStep':.025}}])
    cases.append({'id':'a_mesh','source_case':'a','compare_to':'a_time','changes':dict(cases[1]['changes'],**{'Lines.Wire.TargetSegmentLength':[.6]})})
    (tmp_path/'comparison.json').write_text('{}');(tmp_path/'runtime.json').write_text('{}')
    plan={'schema_version':1,'source_anchors':anchors,'cases':cases,'solver_version':'11.6c',
          'comparison_report_path':'comparison.json','comparison_report_sha256':sha256(b'{}').hexdigest(),
          'runtime_source':{'path':'runtime.json','sha256':sha256(b'{}').hexdigest()}}
    path=tmp_path/'plan.json';path.write_text(json.dumps(plan))
    dll=tmp_path/'solver.dll';dll.write_bytes(b'fake library')
    return dict(plan_path=path,plan_sha256=sha256(path.read_bytes()).hexdigest(),compact_output=tmp_path/'compact',
        expanded_output=tmp_path/'expanded',solver_identity={'requested':'11.6c','resolved_version':'11.6c','resolved_lib_path':str(dll)})


class NativeObject:
    def __init__(self,data):object.__setattr__(self,'data',data)
    def __getattr__(self,key):
        if key=='TargetSegmentLength':return [r[2] for r in self.data['LineType, Length, TargetSegmentLength']]
        if key=='NumberOfSegments':return [math.ceil(r[1]/r[2]) for r in self.data['LineType, Length, TargetSegmentLength']]
        return self.data[key]
    def __setattr__(self,key,value):
        if key=='TargetSegmentLength':
            for row,target in zip(self.data['LineType, Length, TargetSegmentLength'],value):row[2]=target
        else:self.data[key]=value


class NativeModel:
    def __init__(self,api):
        self.api=api;self.state=0;self.threadCount=1
        self.waveComponents=(SimpleNamespace(WaveTrainIndex=0,Frequency=.1,FrequencyLowerBound=.09,
            FrequencyUpperBound=.11,Amplitude=1.,PhaseLagWrtWaveTrainTime=2.,PhaseLagWrtSimulationTime=2.,
            WaveNumber=.04,Direction=165.,Period=10.),)
    def LoadData(self,path):
        data=yaml.safe_load(Path(path).read_bytes());self.variation='BaseFile' in data
        self.doc=yaml.safe_load((Path(path).parent/data['BaseFile']).read_bytes()) if self.variation else data
        if self.variation:
            self.doc['General'].update(data.get('General',{}))
            for name,fields in data.get('Lines',{}).items():
                for key,value in fields.items():setattr(self[name],key,value)
        self.general=NativeObject(self.doc['General'])
    def __getitem__(self,name):return NativeObject(next(r for r in self.doc['Lines'] if r['Name']==name))
    def SaveDataMem(self,kind):return yaml.safe_dump(self.doc).encode()
    def SaveData(self,path):Path(path).write_bytes(self.SaveDataMem('text'))


class NativeApi:
    ModelState=SimpleNamespace(Reset=0)
    DataFileType=SimpleNamespace(Text='text')
    def __init__(self):self.threads=[]
    def DLLVersion(self):return '11.6c'
    def Model(self,threadCount):self.threads.append(threadCount);return NativeModel(self)


def test_prepares_five_without_solving_and_records_actual_counts(tmp_path):
    args=fixture(tmp_path);api=NativeApi()
    result=prepare.prepare_numerical_checks(api,**args)
    assert result['status']=='native_verified_not_run' and len(result['cases'])==5
    mesh=next(r for r in result['cases'] if r['id']=='a_mesh')
    assert mesh['segment_counts']['Wire']['after']==[20]
    assert mesh['segment_counts']['Wire']['before']==[10]
    assert all(t==1 for t in api.threads)
    assert not list(args['compact_output'].rglob('model.yml'))
    assert not (args['compact_output']/'INCOMPLETE').exists()
    original=yaml.safe_load((tmp_path/'a/request.yml').read_bytes())
    refined=yaml.safe_load((args['expanded_output']/'a_mesh/request.yml').read_bytes())
    assert original['extraction']==refined['extraction']
    assert refined['timeout_seconds']==original['timeout_seconds']


@pytest.mark.parametrize('key,value',[('LogPrecision','Double'),('TargetLogSampleInterval',.075)])
def test_rejects_invalid_logging_premise(key,value):
    model={'General':{'ImplicitConstantTimeStep':.05,'TargetLogSampleInterval':.1,'LogPrecision':'Single'}}
    model['General'][key]=value
    with pytest.raises(ValueError):prepare._timing(model)


def test_changed_plan_evidence_stops_before_outputs(tmp_path):
    args=fixture(tmp_path);(tmp_path/'comparison.json').write_text('{"changed":true}')
    with pytest.raises(ValueError):prepare.prepare_numerical_checks(NativeApi(),**args)
    assert not args['compact_output'].exists()


@pytest.mark.parametrize('defect',['cycle','two_controls','unknown_field','nonfinite','duplicate'])
def test_invalid_plan_stops_before_output(tmp_path,defect):
    args=fixture(tmp_path);plan=json.loads(args['plan_path'].read_bytes())
    if defect=='cycle':plan['cases'][0]['compare_to']='a_time'
    elif defect=='two_controls':plan['cases'][1]['compare_to']='a'
    elif defect=='unknown_field':plan['cases'][0]['changes']['Environment.WaveHs']=2
    elif defect=='nonfinite':plan['cases'][0]['changes']['General.TargetLogSampleInterval']=float('nan')
    else:plan['cases'][1]['id']=plan['cases'][0]['id']
    args['plan_path'].write_text(json.dumps(plan));args['plan_sha256']=sha256(args['plan_path'].read_bytes()).hexdigest()
    with pytest.raises(ValueError):prepare.prepare_numerical_checks(NativeApi(),**args)
    assert not args['compact_output'].exists() and not args['expanded_output'].exists()


def test_source_pin_and_existing_output_rejected(tmp_path):
    args=fixture(tmp_path);(tmp_path/'a/model.yml').write_text('tampered')
    with pytest.raises(ValueError):prepare.prepare_numerical_checks(NativeApi(),**args)
    args['compact_output'].mkdir()
    with pytest.raises(FileExistsError):prepare.prepare_numerical_checks(NativeApi(),**args)


def test_snapshot_guard_rejects_unlisted_physics_change():
    before={'General':{'TargetLogSampleInterval':.1},'Mass':3.}
    after=copy.deepcopy(before);after['General']['TargetLogSampleInterval']=.05
    prepare.verify_changes(before,after,{'General.TargetLogSampleInterval':.05})
    after['Mass']=4.
    with pytest.raises(ValueError):prepare.verify_changes(before,after,{'General.TargetLogSampleInterval':.05})


def test_wave_digest_requires_all_native_component_fields():
    model=NativeModel(NativeApi());del model.waveComponents[0].WaveNumber
    with pytest.raises(ValueError):prepare._components(model)


def test_logging_shorter_than_integration_rejected(tmp_path):
    args=fixture(tmp_path);plan=json.loads(args['plan_path'].read_bytes())
    plan['cases'][0]['changes']['General.TargetLogSampleInterval']=.01
    args['plan_path'].write_text(json.dumps(plan));args['plan_sha256']=sha256(args['plan_path'].read_bytes()).hexdigest()
    with pytest.raises(ValueError):prepare.prepare_numerical_checks(NativeApi(),**args)
    assert not args['compact_output'].exists()


def test_native_actual_thread_count_checked(tmp_path):
    args=fixture(tmp_path)
    class WrongThreads(NativeApi):
        def Model(self,threadCount):
            model=super().Model(threadCount);model.threadCount=2;return model
    with pytest.raises(ValueError):prepare.prepare_numerical_checks(WrongThreads(),**args)


def test_local_master_drift_is_rejected(tmp_path):
    args=fixture(tmp_path)
    class DriftingModel(NativeModel):
        def LoadData(self,path):
            super().LoadData(path)
            if Path(path).parent.name=='masters':Path(path).write_bytes(Path(path).read_bytes()+b'\n')
    class DriftingApi(NativeApi):
        def Model(self,threadCount):return DriftingModel(self)
    with pytest.raises(ValueError,match='digest'):prepare.prepare_numerical_checks(DriftingApi(),**args)


@pytest.mark.parametrize('defect',['anchor_name','generation_status','solver_version'])
def test_anchor_contract_rejected_before_output(tmp_path,defect):
    args=fixture(tmp_path);plan=json.loads(args['plan_path'].read_bytes())
    if defect=='anchor_name':plan['source_anchors']['../escape']=plan['source_anchors'].pop('a')
    else:
        gen_path=tmp_path/'a/generation.json';generation=json.loads(gen_path.read_bytes())
        if defect=='generation_status':generation['status']='unverified'
        else:
            path=tmp_path/'a/request.yml';request=yaml.safe_load(path.read_bytes())
            request['solver_version']='11.5';path.write_text(yaml.safe_dump(request))
            generation['request_sha256']=sha256(path.read_bytes()).hexdigest()
        gen_path.write_text(json.dumps(generation))
        plan['source_anchors']['a']['generation_sha256']=sha256(gen_path.read_bytes()).hexdigest()
    args['plan_path'].write_text(json.dumps(plan));args['plan_sha256']=sha256(args['plan_path'].read_bytes()).hexdigest()
    with pytest.raises(ValueError):prepare.prepare_numerical_checks(NativeApi(),**args)
    assert not args['compact_output'].exists()
