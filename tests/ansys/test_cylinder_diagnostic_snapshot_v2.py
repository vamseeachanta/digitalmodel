"""Synthetic process adapter tests; no live collector invocation."""
import importlib
from types import SimpleNamespace
import pytest


def tool():
    return importlib.import_module('digitalmodel.ansys.cylinder_diagnostic_snapshot_v2')


def fixture(tmp_path,monkeypatch):
    current=tool();cwd=tmp_path/'controller';cwd.mkdir();temp=tmp_path/'temp';temp.mkdir()
    exe=tmp_path/'python.exe';exe.write_bytes(b'synthetic image');(cwd/'controller.py').write_bytes(b'pass')
    launcher=tmp_path/'launcher.exe';launcher.write_bytes(b'synthetic launcher')
    raw=[dict(pid=1,parent_pid=0,name='python.exe',creation_time='1',executable_path=str(launcher)),
         dict(pid=2,parent_pid=1,name='python.exe',creation_time='2',executable_path=str(exe))]
    processes={}
    for row in raw:
        process=SimpleNamespace(pid=row['pid'])
        for method,key in [('create_time','creation_time'),('ppid','parent_pid'),('name','name'),('exe','executable_path')]:
            setattr(process,method,lambda r=row,k=key: r[k])
        process.cmdline=lambda:['python.exe','controller.py']
        process.cwd=lambda p=row['pid']:str(temp if p==1 else cwd)
        process.is_running=lambda:True;processes[row['pid']]=process
    monkeypatch.setattr(current.psutil,'Process',lambda pid:processes[pid])
    monkeypatch.setattr(current,'_enumerate',lambda:raw)
    monkeypatch.setattr(current,'read_windows_parent_map',lambda:{r['pid']:r['parent_pid'] for r in raw},raising=False)
    monkeypatch.setattr(current.time,'time',lambda:10)
    monkeypatch.setattr(current.socket,'gethostname',lambda:'synthetic')
    def forwarder(candidate,rows,cache):
        return dict(parent_pid=1,parent_creation_time='1',child_pid=2,child_creation_time='2',
            launcher_sha256=rows[1]['executable_sha256'],resource_kind_hex='02',
            embedded_target_literal=str(exe),target_alias_path=str(exe),alias_identity={},
            resolved_target_path=str(exe),resolved_target_sha256=rows[2]['executable_sha256'])
    monkeypatch.setattr(current,'observe_forwarder',forwarder)
    seed=dict(schema='process-discovery-seed-2',host='synthetic',
        selected_processes=[dict(pid=r['pid'],creation_time=r['creation_time']) for r in raw],
        forwarder_candidates=[dict(parent_pid=1,parent_creation_time='1',child_pid=2,
                                  child_creation_time='2',declared_target_alias=str(exe))])
    return current,seed,processes


def test_forwarder_nonexistent_temp_source_is_not_resolved(tmp_path,monkeypatch):
    current,seed,_=fixture(tmp_path,monkeypatch)
    result=current.collect_v2(discovery_seed=seed)
    assert result['errors']==[] and result['coverage']['selected_details_complete'] is True
    parent,child=result['rows'];assert parent['script_sources']==[]
    assert child['script_sources'][0]['argv_index']==1
    assert child['script_sources'][0]['resolution_basis']=='observed_interpreter_cwd'
    assert result['coverage']['selected_count']==2


@pytest.mark.parametrize('fault',['future','changed_argv','missing_source','forwarder'])
def test_partial_errors_preserve_coverage(tmp_path,monkeypatch,fault):
    current,seed,processes=fixture(tmp_path,monkeypatch)
    if fault=='future':processes[2].create_time=lambda:'11'
    elif fault=='changed_argv':
        calls=[]
        def argv():calls.append(1);return ['python.exe','controller.py'] if len(calls)==1 else ['python.exe','other.py']
        processes[2].cmdline=argv
    elif fault=='missing_source':(tmp_path/'controller/controller.py').unlink()
    else:
        def broken(*a):raise ValueError('bad resource')
        monkeypatch.setattr(current,'observe_forwarder',broken)
    result=current.collect_v2(discovery_seed=seed)
    assert result['errors'] and not result['coverage']['selected_details_complete']
    assert len(result['rows'])+len(result['errors'])==result['coverage']['selected_count']
    assert not {r['pid'] for r in result['rows']} & {r['pid'] for r in result['errors']}


def test_dispatch_none_preserves_v1(monkeypatch):
    from digitalmodel.ansys import cylinder_diagnostic_snapshot as old
    monkeypatch.setattr(old,'_enumerate',lambda:[])
    assert old.collect_process_snapshot(None)['schema']=='process-snapshot-1'


def test_conflicting_modes_refuse(tmp_path,monkeypatch):
    current,seed,_=fixture(tmp_path,monkeypatch)
    with pytest.raises(ValueError):current.collect_v2(binding=seed,discovery_seed=seed)


@pytest.mark.parametrize('attribute,value',[('cwd',None),('cmdline',['python.exe','../controller.py']),
    ('cmdline',['python.exe','-m','controller']),('cmdline',['python.exe','-c','pass'])])
def test_interpreter_missing_or_unsupported_source_refuses(tmp_path,monkeypatch,attribute,value):
    current,seed,processes=fixture(tmp_path,monkeypatch)
    setattr(processes[2],attribute,lambda:value)
    result=current.collect_v2(discovery_seed=seed)
    assert any(row['pid']==2 and row['reason_code']=='unresolved_source' for row in result['errors'])


@pytest.mark.parametrize('fault',['host','duplicate_pid','duplicate_forwarder','missing_pid','limit'])
def test_seed_bounds_refuse(tmp_path,monkeypatch,fault):
    current,seed,_=fixture(tmp_path,monkeypatch)
    if fault=='host':seed['host']='other'
    elif fault=='duplicate_pid':seed['selected_processes']*=2
    elif fault=='duplicate_forwarder':seed['forwarder_candidates']*=2
    elif fault=='limit':seed['forwarder_candidates']*=17
    else:seed['selected_processes'].append(dict(pid=999,creation_time='1'))
    with pytest.raises(ValueError):current.collect_v2(discovery_seed=seed)


def test_creation_after_start_becomes_counted_identity_error(tmp_path,monkeypatch):
    current,seed,processes=fixture(tmp_path,monkeypatch)
    rows=current._enumerate();rows[1]['creation_time']='11';seed['selected_processes'][1]['creation_time']='11'
    seed['forwarder_candidates'][0]['child_creation_time']='11'
    result=current.collect_v2(discovery_seed=seed)
    assert dict(pid=2,reason_code='identity_changed') in result['errors']
    assert len(result['rows'])+len(result['errors'])==2


def test_process_change_during_final_file_read_is_detected(tmp_path,monkeypatch):
    current,seed,processes=fixture(tmp_path,monkeypatch)
    original=current.DeclaredReads.verify
    def changed(cache):
        original(cache)
        processes[2].cmdline=lambda:['python.exe','changed.py']
    monkeypatch.setattr(current.DeclaredReads,'verify',changed)
    result=current.collect_v2(discovery_seed=seed)
    assert dict(pid=2,reason_code='identity_changed') in result['errors']


def test_six_maps_and_fresh_process_instances(tmp_path,monkeypatch):
    current,seed,templates=fixture(tmp_path,monkeypatch)
    maps=importlib.import_module('digitalmodel.ansys.cylinder_parent_map')
    acquisitions=[];instances=[]
    def read():acquisitions.append(1);return {1:0,2:1}
    def process(pid):
        value=SimpleNamespace(**vars(templates[pid]))
        value.ppid=lambda:pytest.fail('per-process parent call')
        instances.append(value);return value
    monkeypatch.setattr(maps,'read_windows_parent_map',read)
    monkeypatch.setattr(current,'_enumerate',maps.enumerate_windows_v2)
    monkeypatch.setattr(current.psutil,'Process',process)
    result=current.collect_v2(discovery_seed=seed)
    assert result['errors']==[] and len(acquisitions)==6
    assert len(instances)==10 and len({id(p) for p in instances})==10


@pytest.mark.parametrize('phase', [1, 2])
def test_new_process_during_details_or_stability_refuses(tmp_path, monkeypatch, phase):
    current, seed, _ = fixture(tmp_path, monkeypatch)
    initial = current._enumerate()
    calls = []
    def read():
        calls.append(1)
        added = [dict(initial[1], pid=3, parent_pid=2)]
        return initial + added if len(calls) == phase + 1 else initial
    monkeypatch.setattr(current, '_enumerate', read)
    with pytest.raises(ValueError, match='population'):
        current.collect_v2(discovery_seed=seed)


@pytest.mark.parametrize('phase', [1, 2])
def test_excluded_process_joins_family_during_collection(tmp_path, monkeypatch, phase):
    current, seed, _ = fixture(tmp_path, monkeypatch)
    initial = current._enumerate()
    initial.append(dict(initial[0], pid=3, parent_pid=0, name='unrelated.exe'))
    calls = []
    def read():
        calls.append(1)
        return [dict(r, parent_pid=2) if r['pid']==3 and len(calls)==phase+1
                else dict(r) for r in initial]
    monkeypatch.setattr(current, '_enumerate', read)
    with pytest.raises(ValueError, match='parent transition'):
        current.collect_v2(discovery_seed=seed)


@pytest.mark.parametrize('phase',[1,2])
@pytest.mark.parametrize('fault',['missing','parent','whole_map'])
def test_phase_map_changes_refuse(tmp_path,monkeypatch,phase,fault):
    current,seed,_=fixture(tmp_path,monkeypatch);calls=[]
    initial=current._enumerate()
    def read():
        calls.append(1)
        if len(calls)==phase+1:
            if fault=='whole_map':raise ValueError('whole map unavailable')
            return initial[:1] if fault=='missing' else [initial[0],dict(initial[1],parent_pid=999)]
        return initial
    monkeypatch.setattr(current,'_enumerate',read)
    with pytest.raises(ValueError) as caught:
        current.collect_v2(discovery_seed=seed)
    assert caught.value.evidence['failed_stage']


@pytest.mark.parametrize('changed_phase',[1,2])
def test_creation_reuse_with_same_parent_refuses(tmp_path,monkeypatch,changed_phase):
    current,seed,templates=fixture(tmp_path,monkeypatch);calls={1:0,2:0}
    def process(pid):
        calls[pid]+=1;value=SimpleNamespace(**vars(templates[pid]))
        if pid==2 and calls[pid]==changed_phase:value.create_time=lambda:'3'
        return value
    monkeypatch.setattr(current.psutil,'Process',process)
    result=current.collect_v2(discovery_seed=seed)
    assert dict(pid=2,reason_code='identity_changed') in result['errors']


def test_constructor_vanish_of_pinned_pid_still_refuses_selection(tmp_path,monkeypatch):
    current,seed,templates=fixture(tmp_path,monkeypatch)
    maps=importlib.import_module('digitalmodel.ansys.cylinder_parent_map')
    monkeypatch.setattr(maps,'read_windows_parent_map',lambda:{1:0,2:1})
    monkeypatch.setattr(current,'_enumerate',maps.enumerate_windows_v2)
    def process(pid):
        if pid==2:raise current.psutil.NoSuchProcess(pid)
        return templates[pid]
    monkeypatch.setattr(current.psutil,'Process',process)
    with pytest.raises(ValueError,match='missing.*initial'):current.collect_v2(discovery_seed=seed)
