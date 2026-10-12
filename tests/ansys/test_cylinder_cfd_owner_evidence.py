"""Synthetic fixed-file owner joins; no production file or process access."""
import base64
from copy import deepcopy
import hashlib
import json
import pytest
from digitalmodel.ansys.analysis_records import canonical_bytes
from digitalmodel.ansys.cylinder_cfd_owner_evidence import resolve_owner_evidence


def sha(raw):
    return hashlib.sha256(raw).hexdigest()


def dump(value):
    return json.dumps(value).encode()


def process(pid,parent,name='python.exe'):
    return dict(pid=pid,parent_pid=parent,creation_time=str(pid),name=name,
                executable_path='C:/bin/'+name,executable_resolved_path='C:/bin/'+name,
                executable_sha256='a'*64,argv=['C:/bin/'+name],cwd='C:/control',script_sources=[])


@pytest.fixture
def facts():
    files={'C:/control/rotation_queue.py':b'controller', 'C:/source/adapter.py':b'adapter',
           'C:/source/guard.py':b'guard','C:/cases/G04-11kn-C/case/trial-config.json':b'{"ignored":1.25}'}
    controller=process(2,1);guard=process(4,3);outer=process(1,0);wrapper=process(3,2)
    controller['argv'] += ['rotation_queue.py','controller-config-r5.json']
    guard['argv'] += ['C:/source/adapter.py','C:/source/guard.py','C:/cases/G04-11kn-C/case/trial-config.json']
    outer['argv']=list(controller['argv']);wrapper['argv']=list(guard['argv'])
    outer['cwd']=wrapper['cwd']='C:/TEMP'
    for r,paths in [(controller,['C:/control/rotation_queue.py']),(guard,['C:/source/adapter.py','C:/source/guard.py'])]:
        r['script_sources']=[dict(argv_index=i+1,path=p,sha256=sha(files[p]),
             resolution_basis='observed_interpreter_cwd' if r is controller else 'absolute_argument') for i,p in enumerate(paths)]
    rows=[process(5,4,'mpiexec.exe'),process(6,5,'smpd.exe')]
    rows[0]['argv'] += ['-n','60','-case','C:/cases/G04-11kn-C/case']
    rows += [process(i,6,'interFoam.exe') for i in range(10,70)]
    rows += [process(i+100,i,'conhost.exe') for i in range(10,70)]
    binding=dict(schema='cfd-process-binding-2',host='synthetic',controller=2,guard=4,
       wrappers=[1,3],mpi=5,helper=6,ranks=list(range(10,70)),console_helpers=list(range(110,170)),
       processes=[controller,guard,outer,wrapper]+rows)
    ancestors=[]
    for r in [guard,wrapper,controller,outer]:
        a={k:r[k] for k in ('pid','parent_pid','creation_time','argv','cwd')}
        a['sources']=[dict(argv_literal=r['argv'][s['argv_index']],cwd_resolved_path=s['path'],
             exists=True,sha256=s['sha256']) for s in r['script_sources']]
        ancestors.append(a)
    job=dict(condition_id='G04_11',case='C:/cases/G04-11kn-C/case',config_sha256=sha(files['C:/cases/G04-11kn-C/case/trial-config.json']))
    config=dict(schema=1,root='C:/control',adapter='C:/source/adapter.py',guard='C:/source/guard.py',
        adapter_sha256=sha(b'adapter'),guard_sha256=sha(b'guard'),evidence_sha256={'rotation_queue.py':sha(b'controller')},jobs=[job],ignored=1.25)
    files['C:/control/controller-config-r5.json']=dump(config)
    receipt=dict(schema='pressure-current-resource-facts-1',snapshot=dict(host='synthetic',rows=[{k:v for k,v in r.items() if k not in ('cwd','executable_resolved_path')} for r in rows]),
      current_cfd_owner_evidence=dict(active_case='G04-11kn-C',ancestors=ancestors,
        owner_source_records=[dict(path='C:/control/controller-config-r5.json',sha256=sha(dump(config))),dict(path='C:/control/controller-state.json',sha256='d'*64)]),
      launch_receipt_bounded_search=dict(child_launch_receipt=dict(wrapper_pid=3,condition_id='G04_11',epoch=1.25),current_controller_state=dict(pid=2,current='G04_11',controller_created_epoch=2.0)))
    return files,binding,receipt,config


def assemble(facts):
    files,binding,receipt,config=facts
    files['C:/control/controller-config-r5.json']=dump(config)
    receipt['current_cfd_owner_evidence']['owner_source_records'][0]['sha256']=sha(dump(config))
    raw=dump(receipt);files['C:/receipt.json']=raw
    ref=dict(id='existing-facts.json',sha256=sha(raw));binding['owner_reference']=ref
    operation=dict(cfd_owner_evidence=dict(ref,path='C:/receipt.json'))
    calls=[]
    def read(path,expected):
        assert 'controller-state' not in path
        calls.append(path)
        value=files[path.replace('\\','/')]
        if sha(value)!=expected: raise ValueError('callback digest changed')
        return value
    return operation,binding,read,calls


@pytest.mark.parametrize('historical_forwarder_sources',[None,'valid'])
def test_resolves_fixed_sources_ignores_forwarder_history_and_state(facts,historical_forwarder_sources):
    for i in (1,3):
        facts[2]['current_cfd_owner_evidence']['ancestors'][i]['sources']=None if historical_forwarder_sources is None else [dict(path='C:/irrelevant.py',sha256='f'*64)]
    operation,binding,read,calls=assemble(facts)
    result=resolve_owner_evidence(operation,binding,read)
    canonical_bytes(result)
    assert base64.b64decode(result['raw_receipt_base64'])==facts[0]['C:/receipt.json']
    assert result['evidence_scope']=='conditional_fixed_source_relationships'
    assert not any('controller-state' in p for p in calls)
    assert len(result['fixed_file_pins'])>=5


@pytest.mark.parametrize('mutation',['reference','host','pid','parent','creation','argv','cwd','source','job','role','case','queue','config_root','source_index'])
def test_missing_or_mismatched_relationship_refuses(facts,mutation):
    files,b,r,c=facts
    if mutation=='host': b['host']='other'
    if mutation=='pid': r['current_cfd_owner_evidence']['ancestors'][2]['pid']=99
    if mutation=='parent': b['processes'][0]['parent_pid']=0
    if mutation=='creation': b['processes'][0]['creation_time']='2.000'
    if mutation=='argv': b['processes'][0]['argv']=list(b['processes'][0]['argv'])+['extra']
    if mutation=='cwd': b['processes'][0]['cwd']='C:/other'
    if mutation=='source': c['adapter_sha256']='f'*64
    if mutation=='job': c['jobs']*=2
    if mutation=='role': b['ranks']=b['ranks'][:-1]
    if mutation=='case': r['current_cfd_owner_evidence']['active_case']='different'
    if mutation=='queue': r['launch_receipt_bounded_search']['child_launch_receipt']['wrapper_pid']=9
    if mutation=='config_root': c['root']='C:/other'
    if mutation=='source_index': b['processes'][0]['script_sources'][0]['argv_index']=2
    operation,binding,read,_=assemble(facts)
    if mutation=='reference': operation['cfd_owner_evidence']['id']='changed'
    with pytest.raises(ValueError): resolve_owner_evidence(operation,binding,read)


@pytest.mark.parametrize('raw',[b'{"schema":1,"schema":2}',b'{"x":NaN}',b'{"x":Infinity}',b'['*40+b'0'+b']'*40])
def test_malformed_json_refuses_even_with_matching_hash(facts,raw):
    operation,b,read,_=assemble(facts);facts[0]['C:/receipt.json']=raw
    operation['cfd_owner_evidence']['sha256']=b['owner_reference']['sha256']=sha(raw)
    with pytest.raises(ValueError): resolve_owner_evidence(operation,b,read)


def test_callback_cannot_substitute_unhashed_bytes(facts):
    operation,b,_,_=assemble(facts)
    with pytest.raises(ValueError): resolve_owner_evidence(operation,b,lambda *_:b'{}')


def test_all_calls_recheck_fixed_bytes(facts):
    operation,b,read,_=assemble(facts)
    resolve_owner_evidence(operation,b,read)
    facts[0]['C:/source/guard.py']=b'changed'
    with pytest.raises(ValueError): resolve_owner_evidence(operation,b,read)


def test_parent_created_after_child_refuses(facts):
    facts[1]['processes'][2]['creation_time']='200'
    facts[2]['current_cfd_owner_evidence']['ancestors'][3]['creation_time']='200'
    operation,binding,read,_=assemble(facts)
    with pytest.raises(ValueError): resolve_owner_evidence(operation,binding,read)


def test_configuration_schema_must_be_existing_integer_one(facts):
    facts[3]['schema']=True
    operation,binding,read,_=assemble(facts)
    with pytest.raises(ValueError): resolve_owner_evidence(operation,binding,read)


def test_redirect_refusal_from_reader_propagates(facts):
    operation,binding,_,_=assemble(facts)
    def denied(*_):
        raise ValueError('preflight path relative or redirected')
    with pytest.raises(ValueError,match='redirected'):
        resolve_owner_evidence(operation,binding,denied)


def test_changed_run_reports_stale_receipt_without_generalizing(facts):
    facts[1]['processes'].pop()
    operation,binding,read,_=assemble(facts)
    with pytest.raises(ValueError,match='stale owner receipt'):
        resolve_owner_evidence(operation,binding,read)


def _actual_binding(receipt):
    """Supply explicitly synthetic missing v2 image pins; no live classification."""
    ancestors=receipt['current_cfd_owner_evidence']['ancestors']
    old=receipt['snapshot']['rows']
    rows=[dict(r,cwd=None,executable_resolved_path=r['executable_path']) for r in old]
    for index,a in enumerate(ancestors):
        row=process(a['pid'],a['parent_pid'])
        row.update({k:deepcopy(a[k]) for k in ('creation_time','argv','cwd')})
        row['script_sources']=[]
        if index in (0,2):
            for source in a['sources']:
                i=row['argv'].index(source['argv_literal'])
                row['script_sources'].append(dict(argv_index=i,path=source['cwd_resolved_path'],
                    sha256=source['sha256'],resolution_basis='observed_interpreter_cwd' if index==2 else 'absolute_argument'))
        rows.append(row)
    def ids(name):
        return [r['pid'] for r in old if r['name'].lower()==name]
    return dict(schema='cfd-process-binding-2',host=receipt['snapshot']['host'],
        processes=rows,controller=ancestors[2]['pid'],guard=ancestors[0]['pid'],
        wrappers=[ancestors[3]['pid'],ancestors[1]['pid']],mpi=ids('mpiexec.exe')[0],
        helper=ids('smpd.exe')[0],ranks=ids('interfoam.exe'),console_helpers=ids('conhost.exe'))


def test_pinned_actual_receipt_fixed_sources_offline():
    """Opt-in private fixed-file joins, deliberately NOT a complete native binding."""
    import os
    from pathlib import Path
    location=os.environ.get('SOLVERS_CFD_OWNER_RECEIPT')
    if not location:
        pytest.skip('private pinned receipt not supplied')
    maximum=4*1024*1024
    calls=[]
    def bounded_read(path,expected):
        assert Path(path).name != 'controller-state.json'
        with Path(path).open('rb') as stream:
            raw=stream.read(maximum+1)
        assert len(raw)<=maximum and sha(raw)==expected
        calls.append(path)
        return raw
    expected='02da4edc0779a3c683b6d9b7b662b19d2344801d2a38b5f32950485290778d88'
    receipt=json.loads(bounded_read(location,expected))
    config_pin=receipt['current_cfd_owner_evidence']['owner_source_records'][0]
    assert config_pin['sha256']=='5eeb5c34e48cad18bb57b1611dc7c8b99ab4ea87106dafe93d7dbea26441912a'
    config=json.loads(bounded_read(config_pin['path'],config_pin['sha256']))
    assert type(config['schema']) is int and config['schema']==1
    binding=_actual_binding(receipt)
    ref=dict(id='SOLVERS-pressure-current-resource-facts.json',sha256=expected)
    binding['owner_reference']=ref
    operation=dict(cfd_owner_evidence=dict(ref,path=location))
    result=resolve_owner_evidence(operation,binding,bounded_read)
    canonical_bytes(result)
    assert result['matched_joins']['branch']['condition_id']=='G04_11'
    assert result['matched_joins']['execution_rows']==122
    assert len(result['fixed_file_pins'])==6
    print(json.dumps(dict(test_scope='actual fixed-file joins with synthetic missing ancestor image pins; no live authority',
        receipt_sha256=expected,config_sha256=config_pin['sha256'],config_schema=config['schema'],config_keys=list(config),
        fixed_file_pins=result['fixed_file_pins'])))
