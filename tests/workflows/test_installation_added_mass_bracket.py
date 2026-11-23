import copy
from hashlib import sha256
import json
from pathlib import Path
from types import SimpleNamespace

import pytest
import yaml

from digitalmodel.workflows import installation_added_mass_bracket as bracket


def fixture(tmp_path):
    model = {'General': {'UnitsSystem': 'SI', 'ImplicitUseVariableTimeStep': False, 'ImplicitConstantTimeStep': .05},
             'Environment': {'Density': 1.025, 'WaveTrains': [{'Name': 'Wave1'}]},
             '6DBuoys': [{'Name': 'Body', 'BuoyType': 'Lumped buoy',
                          'LumpedBuoyAddedMassMethod': 'Diagonal values',
                          'HydrodynamicMass': [2., 3., 4.], 'AddedMassCoefficient': [1., 2., 3.],
                          'Mass': 5., 'Volume': 1.}]}
    source = tmp_path / 'source.yml'
    source.write_text(yaml.safe_dump(model))
    digest = sha256(source.read_bytes()).hexdigest()
    settings = dict(buildup_s=80., duration_s=600., sample_interval_s=.1,
                    gamma=1.5, max_time_step_s=.05, components=80,
                    fixed_time_step_s=.05,
                    wave_reference={'WaveDirection': 165, 'WaveOrigin': [0, 0], 'WaveTimeOrigin': 0})
    matrix = tmp_path / 'matrix.json'
    matrix.write_text(json.dumps({'master_sha256': digest, 'settings': settings,
                                 'cases': [dict(hs_m=1+i*.25, tp_s=8+i, seed=123) for i in range(5)]}))
    receipt = tmp_path / 'properties.json'
    receipt.write_text(json.dumps({'source_sha256': digest, 'body_name': 'Body',
        'units': {'added_mass': 'kg', 'HydrodynamicMass':'t'}, 'rho_water_kg_m3':1025.,
        'inputs': {'m_air':5000., 'm_water':1000.}, 'hydrodynamic_reference_mass_kg':4000.,
        'properties': {'translational': {'added_mass': {'z': 12000.}, 'ca': {'z':3.}}}}))
    return dict(source=source, source_sha256=digest, matrix=matrix,
                matrix_sha256=sha256(matrix.read_bytes()).hexdigest(), receipt=receipt,
                receipt_sha256=sha256(receipt.read_bytes()).hexdigest(), output=tmp_path/'new',
                body_name='Body', baseline_added_mass_t=12., factors=[.5, 1., 1.5], case_indices=list(range(5)))


def test_fifteen_lean_changes_preserve_master_and_xy(tmp_path):
    args = fixture(tmp_path)
    result = bracket.prepare_bracket(**args)
    assert result['status'] == 'prepared_not_native_verified'
    assert len(result['cases']) == 15
    assert (args['output']/'master.yml').read_bytes() == args['source'].read_bytes()
    for row in result['cases']:
        change = yaml.safe_load((args['output']/row['change_file']).read_bytes())
        assert change['BaseFile'] == '../master.yml'
        assert set(change) == {'BaseFile', 'General', 'Environment', '6DBuoys'}
        assert change['6DBuoys'] == {'Body': {'AddedMassCoefficient': [1., 2., row['factor']*3.]}}
        assert row['added_mass_t'] == row['factor']*12.
        assert row['status'] == 'prepared_not_native_verified'
    assert len(list(args['output'].rglob('*.yml'))) == 16


@pytest.mark.parametrize('key,value', [('factors',[.5,.5]),('factors',[float('nan')]),
    ('factors',[True]),('case_indices',[0,0]),('case_indices',[99]),
    ('case_indices',[True]),('baseline_added_mass_t',13.),('body_name','Missing')])
def test_invalid_selection_fails_before_output(tmp_path,key,value):
    args=fixture(tmp_path);args[key]=value
    with pytest.raises(ValueError): bracket.prepare_bracket(**args)
    assert not args['output'].exists()


@pytest.mark.parametrize('name', ['source','matrix','receipt'])
def test_tampered_input_rejected(tmp_path,name):
    args=fixture(tmp_path);args[name].write_bytes(args[name].read_bytes()+b' ')
    with pytest.raises(ValueError,match='digest'): bracket.prepare_bracket(**args)
    assert not args['output'].exists()


def test_existing_output_is_untouched(tmp_path):
    args=fixture(tmp_path);args['output'].mkdir()
    with pytest.raises(FileExistsError): bracket.prepare_bracket(**args)
    assert list(args['output'].iterdir()) == []


@pytest.mark.parametrize('defect', ['include','matrix_identity','receipt_identity','receipt_units','zero_reference'])
def test_dependency_or_model_defect_rejected(tmp_path,defect):
    args=fixture(tmp_path)
    if defect in ('include','zero_reference'):
        data=yaml.safe_load(args['source'].read_bytes())
        if defect=='include': data['BaseFile']='missing.yml'
        else: data['6DBuoys'][0]['HydrodynamicMass'][2]=0
        args['source'].write_text(yaml.safe_dump(data));args['source_sha256']=sha256(args['source'].read_bytes()).hexdigest()
        for name in ('matrix','receipt'):
            d=json.loads(args[name].read_bytes());d['master_sha256' if name=='matrix' else 'source_sha256']=args['source_sha256']
            args[name].write_text(json.dumps(d));args[name+'_sha256']=sha256(args[name].read_bytes()).hexdigest()
    else:
        name='matrix' if defect=='matrix_identity' else 'receipt';data=json.loads(args[name].read_bytes())
        if defect=='receipt_units':data['units']['added_mass']='t'
        else:data['master_sha256' if name=='matrix' else 'source_sha256']='wrong'
        args[name].write_text(json.dumps(data));args[name+'_sha256']=sha256(args[name].read_bytes()).hexdigest()
    with pytest.raises(ValueError):bracket.prepare_bracket(**args)
    assert not args['output'].exists()


def test_native_snapshot_guard_rejects_xy_and_other_changes(tmp_path):
    from digitalmodel.workflows.installation_added_mass_materialize import verify_structure
    args=fixture(tmp_path);before=yaml.safe_load(args['source'].read_bytes())
    after=copy.deepcopy(before);after['6DBuoys'][0]['AddedMassCoefficient'][2]=4.5
    verify_structure(before,after,'Body',4.5)
    for field,value in [('Mass',6.),('AddedMassCoefficient',[9.,2.,4.5])]:
        changed=copy.deepcopy(after);changed['6DBuoys'][0][field]=value
        with pytest.raises(ValueError):verify_structure(before,changed,'Body',4.5)
    with pytest.raises(ValueError):verify_structure(before,after,'Body',4.)


@pytest.mark.parametrize('timeout,extraction', [(0,{'period':[0,600]}),(True,{'period':[0,600]}),
    (14400,{'period':[0,100]}),(14400,{'period':[0,600]})])
def test_native_preflight_rejects_bad_timeout_or_profile(timeout,extraction):
    from digitalmodel.workflows.installation_added_mass_materialize import validate_extraction
    with pytest.raises(ValueError):validate_extraction(extraction,{'duration_s':600},timeout)


def test_receipt_reference_units_are_required(tmp_path):
    args=fixture(tmp_path);data=json.loads(args['receipt'].read_bytes())
    data['units']['HydrodynamicMass']='kg'
    args['receipt'].write_text(json.dumps(data));args['receipt_sha256']=sha256(args['receipt'].read_bytes()).hexdigest()
    with pytest.raises(ValueError,match='units'):bracket.prepare_bracket(**args)


def test_receipt_not_rounded_caller_is_mass_basis(tmp_path):
    args=fixture(tmp_path);args['baseline_added_mass_t']=12.+1e-12
    result=bracket.prepare_bracket(**args)
    assert result['baseline_added_mass_t']==12.


def test_native_selection_rejects_noop_change(tmp_path):
    from digitalmodel.workflows.installation_added_mass_materialize import _selected
    args=fixture(tmp_path);result=bracket.prepare_bracket(**args)
    row=result['cases'][0];p=args['output']/row['change_file'];change=yaml.safe_load(p.read_bytes())
    change.pop('6DBuoys');p.write_text(yaml.safe_dump(change))
    row['change_sha256']=sha256(p.read_bytes()).hexdigest()
    (args['output']/'manifest.json').write_text(json.dumps(result))
    digest=sha256((args['output']/'manifest.json').read_bytes()).hexdigest()
    with pytest.raises(ValueError,match='payload'):_selected(args['output'],row['id'],digest)


def test_materialization_preserves_profile_and_qualifies_window(tmp_path,monkeypatch):
    from digitalmodel.workflows import installation_added_mass_materialize as native
    args=fixture(tmp_path);manifest=bracket.prepare_bracket(**args);row=manifest['cases'][0]
    before=yaml.safe_load(args['source'].read_bytes());after=copy.deepcopy(before)
    after['6DBuoys'][0]['AddedMassCoefficient'][2]=row['coefficient_z']
    class Model:
        def SaveData(self,path):
            from pathlib import Path
            Path(path).write_text(yaml.safe_dump(after))
        def LoadData(self,path):pass
    extraction={'period':[0,600],'time_histories':[{'object':'Body','variable':'Z','units':'m'}],
                'supplemental_profile':{'schema_version':1,'channels':[{'object':'Body','variable':'Z','units':'m'}], 'geometry_lines':[]}}
    request=tmp_path/'request.yml';request.write_text(yaml.safe_dump({'extraction':extraction}))
    monkeypatch.setattr(native,'_native_models',lambda *a:(Model(),before,{}))
    monkeypatch.setattr(native,'_snapshot',lambda *a:after)
    calls=[]
    monkeypatch.setattr(native,'_verify',lambda *a:calls.append('verified'))
    monkeypatch.setattr(native,'_objects',lambda *a:None)
    output=tmp_path/'native'
    dll=tmp_path/'solver.dll';dll.write_bytes(b'fixture')
    api=SimpleNamespace(DLLVersion=lambda:'11.6c')
    receipt=native.materialize_bracket_case(api,args['output'],row['id'],output,
        manifest_sha256=sha256((args['output']/'manifest.json').read_bytes()).hexdigest(),
        extraction_request=request,extraction_request_sha256=sha256(request.read_bytes()).hexdigest(),
        solver_version='11.6c',timeout_seconds=14400,
        solver_identity={'requested':'11.6c','resolved_version':'11.6c','resolved_lib_path':str(dll)})
    generated=yaml.safe_load((output/'request.yml').read_bytes())
    assert generated['extraction']==extraction
    assert any('stationarity is unverified' in text for text in generated['limitations'])
    assert not any(text.startswith('Stationary extraction') for text in generated['limitations'])
    assert calls==['verified'] and receipt['status']=='native_verified_not_run'


def test_native_manifest_requires_external_pin(tmp_path):
    from digitalmodel.workflows.installation_added_mass_materialize import _selected
    args=fixture(tmp_path);manifest=bracket.prepare_bracket(**args)
    path=args['output']/'manifest.json';digest=sha256(path.read_bytes()).hexdigest()
    path.write_bytes(path.read_bytes()+b' ')
    with pytest.raises(ValueError,match='digest'):_selected(args['output'],manifest['cases'][0]['id'],digest)


def test_receipt_precise_ca_is_used_instead_of_master(tmp_path):
    args=fixture(tmp_path);data=json.loads(args['receipt'].read_bytes())
    data['properties']['translational']['ca']['z']=3.123456789
    mass=4.*3.123456789
    data['properties']['translational']['added_mass']['z']=mass*1000
    args['receipt'].write_text(json.dumps(data));args['receipt_sha256']=sha256(args['receipt'].read_bytes()).hexdigest()
    args['baseline_added_mass_t']=mass
    result=bracket.prepare_bracket(**args)
    for row in result['cases']:
        assert row['coefficient_z']==row['factor']*3.123456789
        change=yaml.safe_load((args['output']/row['change_file']).read_bytes())
        assert change['6DBuoys']['Body']['AddedMassCoefficient']==[1.,2.,row['factor']*3.123456789]


@pytest.mark.parametrize('defect', ['units','buoy','method','density','reference','ca','fixed_missing','fixed_mismatch'])
def test_engineering_basis_rejects_incompatible_input(tmp_path,defect):
    args=fixture(tmp_path);model=yaml.safe_load(args['source'].read_bytes())
    receipt=json.loads(args['receipt'].read_bytes());matrix=json.loads(args['matrix'].read_bytes())
    if defect=='units':model['General']['UnitsSystem']='US'
    elif defect=='buoy':model['6DBuoys'][0]['BuoyType']='Spar buoy'
    elif defect=='method':model['6DBuoys'][0]['LumpedBuoyAddedMassMethod']='Matrix'
    elif defect=='density':model['Environment']['Density']=1.2
    elif defect=='reference':receipt['hydrodynamic_reference_mass_kg']=4001.
    elif defect=='ca':receipt['properties']['translational']['ca']['z']=4.
    elif defect=='fixed_missing':matrix['settings'].pop('fixed_time_step_s')
    elif defect=='fixed_mismatch':matrix['settings']['fixed_time_step_s']=.025
    args['source'].write_text(yaml.safe_dump(model));args['source_sha256']=sha256(args['source'].read_bytes()).hexdigest()
    receipt['source_sha256']=matrix['master_sha256']=args['source_sha256']
    for name,value in [('receipt',receipt),('matrix',matrix)]:
        args[name].write_text(json.dumps(value));args[name+'_sha256']=sha256(args[name].read_bytes()).hexdigest()
    with pytest.raises(ValueError):bracket.prepare_bracket(**args)
    assert not args['output'].exists()


def test_native_selection_rejects_wrong_declared_mass(tmp_path):
    from digitalmodel.workflows.installation_added_mass_materialize import _selected
    args=fixture(tmp_path);manifest=bracket.prepare_bracket(**args)
    manifest['cases'][0]['added_mass_t']=999.
    path=args['output']/'manifest.json';path.write_text(json.dumps(manifest))
    with pytest.raises(ValueError):_selected(args['output'],manifest['cases'][0]['id'],sha256(path.read_bytes()).hexdigest())


def test_path_escape_rejected(tmp_path):
    from digitalmodel.workflows.installation_added_mass_materialize import _inside
    with pytest.raises(ValueError):_inside(tmp_path,'../outside.yml')
    with pytest.raises(ValueError):_inside(tmp_path,str(tmp_path.parent/'outside.yml'))


@pytest.mark.parametrize('defect',['requested','resolved_version','api_version','missing_library'])
def test_materialization_function_rejects_solver_identity(tmp_path,defect):
    from digitalmodel.workflows import installation_added_mass_materialize as native
    args=fixture(tmp_path);manifest=bracket.prepare_bracket(**args)
    dll=tmp_path/'solver.dll';dll.write_bytes(b'fixture')
    identity={'requested':'11.6c','resolved_version':'11.6c','resolved_lib_path':str(dll)}
    if defect in ('requested','resolved_version'):identity[defect]='11.5'
    if defect=='missing_library':dll.unlink()
    api=SimpleNamespace(DLLVersion=lambda:'11.5' if defect=='api_version' else '11.6c')
    request=tmp_path/'request.yml'
    request.write_text(yaml.safe_dump({'extraction':{'period':[0,600],
        'time_histories':[{'object':'Body','variable':'Z','units':'m'}],
        'supplemental_profile':{'schema_version':1,'channels':[{'object':'Body','variable':'Z','units':'m'}],
                                'geometry_lines':[]}}}))
    with pytest.raises((ValueError,FileNotFoundError)):
        native.materialize_bracket_case(api,args['output'],manifest['cases'][0]['id'],tmp_path/'native',
            manifest_sha256=sha256((args['output']/'manifest.json').read_bytes()).hexdigest(),
            extraction_request=request,extraction_request_sha256=sha256(request.read_bytes()).hexdigest(),
            solver_version='11.6c',timeout_seconds=14400,solver_identity=identity)
    assert not (tmp_path/'native').exists()


class FakeNativeModel:
    def __init__(self, api):
        self.api=api

    def LoadData(self,path):
        self.api.loads.append(str(path))
        data=yaml.safe_load(Path(path).read_bytes())
        self.variation=copy.deepcopy(data) if 'BaseFile' in data else None
        if 'BaseFile' in data:
            self.doc=yaml.safe_load((Path(path).parent/data['BaseFile']).read_bytes())
            self.doc['General'].update(data['General'])
            self.doc['Environment'].update(data['Environment'])
            if not self.api.noop:
                for body in self.doc['6DBuoys']:body.update(data['6DBuoys'].get(body['Name'],{}))
        else:self.doc=data
        self.general=SimpleNamespace(**self.doc['General'])
        wave=self.doc['Environment']['WaveTrains'][0]
        env={k:v for k,v in self.doc['Environment'].items() if k!='WaveTrains'}
        env.update(wave);env.update(NumberOfWaveTrains=1,WaveDirection=165,
            WaveOriginX=0,WaveOriginY=0,WaveTimeOrigin=0)
        self.environment=SimpleNamespace(**env)
        self.objects=[SimpleNamespace(name=b['Name']) for b in self.doc['6DBuoys']]

    def SaveDataMem(self,kind):
        if self.variation is not None:return yaml.safe_dump(self.variation).encode()
        self.doc['General'].update(vars(self.general))
        return yaml.safe_dump(self.doc).encode()

    def __getitem__(self,name):
        return FakeNativeBody(self,next(b for b in self.doc['6DBuoys'] if b['Name']==name))

    def SaveData(self,path):
        Path(path).write_bytes(self.SaveDataMem('text'))


class FakeNativeBody:
    def __init__(self,model,body):
        object.__setattr__(self,'model',model)
        object.__setattr__(self,'body',body)

    def __getattr__(self,name):
        if name.startswith('AddedMassCoefficient') and name[-1] in 'XYZ':
            return self.body['AddedMassCoefficient']['XYZ'.index(name[-1])]
        return self.body[name]

    def __setattr__(self,name,value):
        if name.startswith('AddedMassCoefficient') and name[-1] in 'XYZ':
            self.body['AddedMassCoefficient']['XYZ'.index(name[-1])]=value
        else:self.body[name]=value
        if self.model.api.corrupt and self.model.variation is None:self.body['Mass']+=1


class FakeNativeApi:
    DataFileType=SimpleNamespace(Text='text')

    def __init__(self,corrupt=False,noop=False):
        self.corrupt=corrupt
        self.noop=noop
        self.loads=[]
        self.threads=[]

    def Model(self,threadCount):
        self.threads.append(threadCount)
        return FakeNativeModel(self)


@pytest.mark.parametrize('corrupt',[False,True])
def test_native_models_full_flow_with_fake_api(tmp_path,corrupt):
    from digitalmodel.workflows import installation_added_mass_materialize as native
    args=fixture(tmp_path);manifest=bracket.prepare_bracket(**args);row=manifest['cases'][0]
    extraction={'time_histories':[{'object':'Body','variable':'Z','units':'m'}],
                'supplemental_profile':{'schema_version':1,'channels':[{'object':'Body','variable':'Z','units':'m'}],
                                        'geometry_lines':[]}}
    api=FakeNativeApi(corrupt)
    call=lambda:native._native_models(api,args['output']/'master.yml',args['output']/row['change_file'],manifest,row,extraction)
    if corrupt:
        with pytest.raises(ValueError,match='structural'):call()
    else:
        model,before,reference=call()
        assert model.doc['6DBuoys'][0]['AddedMassCoefficient']==[1.,2.,1.5]
        assert model.variation is None
        assert model.doc['6DBuoys'][0]['Mass']==5.
        assert before['6DBuoys'][0]['AddedMassCoefficient']==[1.,2.,3.]
        assert reference['WaveDirection']==165
        assert model.general.ImplicitConstantTimeStep==.05
    assert api.threads==[1,1] and len(api.loads)==2


def test_native_variation_noop_is_rejected(tmp_path):
    from digitalmodel.workflows import installation_added_mass_materialize as native
    args=fixture(tmp_path);manifest=bracket.prepare_bracket(**args);row=manifest['cases'][0]
    extraction={'time_histories':[{'object':'Body','variable':'Z','units':'m'}],
                'supplemental_profile':{'schema_version':1,'channels':[{'object':'Body','variable':'Z','units':'m'}],
                                        'geometry_lines':[]}}
    with pytest.raises(ValueError):
        native._native_models(FakeNativeApi(noop=True),args['output']/'master.yml',
            args['output']/row['change_file'],manifest,row,extraction)
