"""Synthetic refusal-only identity capture; no host queries or solver authority."""
from copy import deepcopy
import importlib
from types import SimpleNamespace
import pytest
from digitalmodel.ansys.analysis_records import canonical_bytes


def module():
    return importlib.import_module('digitalmodel.ansys.cylinder_parent_identities')


def process(pid, created=1.0, name='worker.exe', image='C:/worker.exe'):
    return SimpleNamespace(pid=pid, create_time=lambda: created, name=lambda: name,
        exe=lambda: image, ppid=lambda: pytest.fail('extra parent table read'),
        _proc=SimpleNamespace(exe=lambda: image))


def initial_row(pid=1):
    return dict(pid=pid, parent_pid=0, creation_time='1.0', name='parent.exe', executable_path='C:/parent.exe')


def capture(monkeypatch, factory=None, initial=None, final=None, rows=None, triggers=None):
    current = module(); calls = []
    def read(pid):
        calls.append(pid)
        return (factory or process)(pid)
    monkeypatch.setattr(current.psutil, 'Process', read)
    result = current.capture_parent_identities(initial or {1: 0}, final or {1: 0, 2: 1},
        [initial_row()] if rows is None else rows, [2] if triggers is None else triggers)
    return result, calls


def test_added_identity_and_original_parent_are_distinct(monkeypatch):
    result, calls = capture(monkeypatch)
    assert result['schema']=='parent-change-identities-1'
    assert set(result)=={'schema','status','resource_relevance','table_identity_binding',
        'selection_rule','candidate_count','selected_pids','omitted_candidate_count',
        'attempted_pids','records','canonical_byte_limit','started_monotonic_ns',
        'ended_monotonic_ns','limitation'}
    assert result['canonical_byte_limit']==32768
    assert type(result['started_monotonic_ns']) is type(result['ended_monotonic_ns']) is int
    assert 0 < result['started_monotonic_ns'] <= result['ended_monotonic_ns']
    assert result['selected_pids'] == [2, 1] and calls == [2, 2, 1, 1]
    child, parent = result['records']
    assert set(child)=={'pid','original_row_status','final_table_parent_pid','post_table'}
    assert set(parent)==set(child)|{'initial_identity','initial_image_basis'}
    assert parent['initial_image_basis']=='original_collector_psutil_high_level_may_guess'
    assert set(child['post_table'])=={'status','identity'}
    assert set(child['post_table']['identity'])=={'pid','creation_time','name','executable_path'}
    assert child['original_row_status'] == 'NOT_IN_INITIAL_TABLE'
    assert child['final_table_parent_pid'] == 1
    assert child['post_table']['status'] == 'OBSERVED_AFTER_TABLE'
    assert 'parent_pid' not in child['post_table']['identity']
    assert parent['original_row_status'] == 'RETAINED'
    assert parent['initial_identity'] == initial_row()
    assert result['resource_relevance'] == 'NOT_EVALUATED'
    assert result['table_identity_binding'] == 'NOT_ESTABLISHED'
    assert canonical_bytes(result)


@pytest.mark.parametrize('fault,status', [('gone','DISAPPEARED'), ('denied','UNAVAILABLE'), ('birth','IDENTITY_CHANGED'), ('name','IDENTITY_CHANGED'), ('image','IDENTITY_CHANGED'), ('blank','INCOMPLETE_AFTER_TABLE'), ('huge','INVALID_FIELDS')])
def test_unavailable_changed_and_invalid_identity_states(monkeypatch, fault, status):
    count = []
    def factory(pid):
        count.append(pid)
        if fault == 'gone': raise module().psutil.NoSuchProcess(pid)
        if fault == 'denied': raise module().psutil.AccessDenied(pid)
        return process(pid, created=2 if fault == 'birth' and len(count) == 2 else 1,
            name='x'*4097 if fault == 'huge' else '' if fault == 'blank' else 'changed' if fault == 'name' and len(count) == 2 else 'worker',
            image='changed' if fault == 'image' and len(count) == 2 else 'C:/worker')
    result, _ = capture(monkeypatch, factory, final={2:0}, triggers=[2])
    assert result['records'][0]['post_table']['status'] == status
    assert len(result['attempted_pids']) == 1
    assert len(canonical_bytes(result)) <= 32768


def test_missing_original_and_disappeared_table_have_explicit_states(monkeypatch):
    result, calls = capture(monkeypatch, initial={1:0,2:1}, final={1:0}, rows=[], triggers=[2])
    assert result['records'][0]['original_row_status'] == 'UNOBSERVED_INITIAL'
    assert result['records'][0]['post_table']['status'] == 'NOT_IN_FINAL_TABLE'
    assert 2 not in calls


def test_limit_prioritizes_deduplicated_triggers_then_both_parents(monkeypatch):
    initial = {pid: 100 for pid in range(1, 12)}
    final = {pid: 101 for pid in range(1, 12)}
    result, calls = capture(monkeypatch, initial=initial, final=final, rows=[], triggers=list(reversed(range(1,12))))
    assert result['selected_pids'] == list(range(1,9))
    assert result['candidate_count'] == 13 and result['omitted_candidate_count'] == 5
    assert calls == [pid for pid in range(1,9) for _ in range(2)]
    result, _ = capture(monkeypatch, initial={2:1,1:0}, final={2:3,1:0}, rows=[], triggers=[2,1])
    assert result['selected_pids'] == [1,2,3]


def test_aggregate_canonical_byte_limit_and_copy_isolation(monkeypatch):
    name = '\u6d77'*4096
    rows = [dict(initial_row(pid),name=name,executable_path=name) for pid in range(1,9)]
    before = deepcopy(rows)
    result, _ = capture(monkeypatch, lambda pid: process(pid,name=name,image=name),
        initial={pid:0 for pid in range(1,9)}, final={pid:1 for pid in range(1,9)}, rows=rows, triggers=list(range(1,9)))
    assert len(canonical_bytes(result)) <= 32768
    assert any(r['original_row_status']=='OMITTED_SIZE_LIMIT' for r in result['records'])
    rows[0]['name']='mutated'
    assert rows[1:]==before[1:]
    assert 'mutated' not in canonical_bytes(result).decode()


@pytest.mark.parametrize('method', ['create_time','name','exe'])
def test_read_errors_never_escape_capture(monkeypatch, method):
    def factory(pid):
        value=process(pid)
        def fail(): raise OSError('synthetic error')
        setattr(value._proc if method=='exe' else value,method,fail)
        return value
    result, _=capture(monkeypatch,factory,final={2:0},triggers=[2])
    assert result['records'][0]['post_table']['status'] in {'UNAVAILABLE','INCOMPLETE_AFTER_TABLE'}
    assert result['resource_relevance']=='NOT_EVALUATED'


@pytest.mark.parametrize('broken', [False, True])
def test_numeric_refusal_survives_supplemental_capture(monkeypatch, broken):
    from . import test_cylinder_parent_refusal_evidence as fixture
    current=fixture.maps_fixture.tool()
    calls=[]
    def capture(*args):
        calls.append(args)
        if broken: raise RuntimeError('synthetic capture error')
        return dict(schema='parent-change-identities-1', status='SYNTHETIC_REFUSAL_TEST')
    monkeypatch.setattr(current,'capture_parent_identities',capture,raising=False)
    error=fixture.rejected(monkeypatch,{1:0},{1:0,2:1})
    assert len(calls)==1
    assert error.evidence['rejected_parent_observation']['trigger_pids']==[2]
    expected='CAPTURE_FAILED' if broken else 'SYNTHETIC_REFUSAL_TEST'
    assert error.evidence['rejected_parent_identity']['status']==expected
    if broken:
        failed=error.evidence['rejected_parent_identity']
        assert failed['schema']=='parent-change-identities-error-1'
        assert set(failed)=={'schema','status','error_type','resource_relevance','table_identity_binding'}
        assert failed['error_type']=='RuntimeError'
        assert failed['resource_relevance']=='NOT_EVALUATED'
    assert str(error)=='initial parent identity changed before selection'
    assert canonical_bytes(error.evidence)


@pytest.mark.parametrize('width', [3930,3950,3970,3990])
def test_near_limit_retains_all_trailing_omission_metadata(monkeypatch,width):
    name='\u6d77'*width
    rows=[dict(initial_row(1), name=name, executable_path=name)]
    result,_=capture(monkeypatch,lambda pid: process(pid,name='x'*width,image='y'*width),
        initial={pid:0 for pid in range(1,9)},final={pid:0 for pid in range(1,9)},rows=rows,triggers=list(range(1,9)))
    assert len(result['records'])==8 and result['attempted_pids']==list(range(1,9))
    assert len(canonical_bytes(result))<=32768
    assert sum(r['original_row_status']=='OMITTED_SIZE_LIMIT' for r in result['records'])>=2


@pytest.mark.parametrize('fault', ['missing','creation','text'])
def test_invalid_original_status_is_explicit(monkeypatch,fault):
    row=initial_row()
    if fault=='missing':row.pop('name')
    elif fault=='creation':row['creation_time']=' 1'
    else:row['name']='x'*4097
    result,_=capture(monkeypatch,rows=[row])
    assert result['records'][1]['original_row_status']=='INVALID_FIELDS'


def test_success_never_calls_refusal_capture(monkeypatch):
    from . import test_cylinder_parent_map as fixture
    current=fixture.tool()
    monkeypatch.setattr(current,'read_windows_parent_map',lambda:{1:0})
    monkeypatch.setattr(current.psutil,'Process',fixture.fake_process)
    monkeypatch.setattr(current,'capture_parent_identities',lambda *args:pytest.fail('capture on success'))
    assert current.enumerate_windows_v2()[0]['pid']==1



def test_maximum_changed_samples_are_omitted_without_metadata_loss(monkeypatch):
    calls=[]
    def factory(pid):
        calls.append(pid)
        return process(pid, name='\u6d77'*4096, image=('\u5cb8' if len(calls)%2 else '\u8239')*4096)
    result,_=capture(monkeypatch,factory,final={2:0},triggers=[2])
    assert result['attempted_pids']==[2]
    assert result['records']==[{'pid':2,'original_row_status':'OMITTED_SIZE_LIMIT',
        'post_table':{'status':'OMITTED_SIZE_LIMIT'}}]
    assert len(canonical_bytes(result))<=32768



def test_native_image_accessor_never_uses_high_level_guess(monkeypatch):
    def factory(pid):
        value=process(pid)
        value.exe=lambda:pytest.fail('high-level executable fallback used')
        return value
    result,_=capture(monkeypatch,factory)
    assert all(r['post_table']['status']=='OBSERVED_AFTER_TABLE' for r in result['records'])
