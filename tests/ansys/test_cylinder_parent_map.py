"""Synthetic backend and process tests; no live process enumeration."""
import importlib
from types import SimpleNamespace
import pytest


def tool():
    return importlib.import_module('digitalmodel.ansys.cylinder_parent_map')


def mock_backend(monkeypatch, value):
    current=tool();calls=[]
    def read():calls.append('map');return value
    monkeypatch.setattr(current.backend,'ppid_map',read)
    return current,calls


def test_backend_once_returns_independent_copy(monkeypatch):
    original={0:0,1:0,2:1};current,calls=mock_backend(monkeypatch,original)
    result=current.read_windows_parent_map();result[2]=999
    assert original[2]==1 and calls==['map']


@pytest.mark.parametrize('value',[{},[],{True:0},{1:False},{-1:0},{1:-1},{2**32:0},
    {1:2**32},{'1':0},{i:0 for i in range(65537)}])
def test_invalid_map_refuses(monkeypatch,value):
    current,_=mock_backend(monkeypatch,value)
    with pytest.raises(ValueError):current.read_windows_parent_map()


@pytest.mark.parametrize('fault',['version','init_hash','backend_hash','capability'])
def test_dependency_identity_refuses_before_backend(tmp_path,monkeypatch,fault):
    current,calls=mock_backend(monkeypatch,{1:0})
    if fault=='version':monkeypatch.setattr(current.psutil,'__version__','changed')
    elif fault=='capability':monkeypatch.setattr(current.backend,'ppid_map',None)
    else:
        path=tmp_path/('__init__.py' if fault=='init_hash' else '_pswindows.py');path.write_text('changed')
        monkeypatch.setattr(current.psutil if fault=='init_hash' else current.backend,'__file__',str(path))
    with pytest.raises(ValueError):current.read_windows_parent_map()
    assert calls==[]


def test_missing_map_entry_is_nosuchprocess():
    current=tool()
    with pytest.raises(current.psutil.NoSuchProcess):current.parent_of({1:0},2)


def fake_process(pid):
    return SimpleNamespace(pid=pid,name=lambda:'python.exe',exe=lambda:'synthetic.exe',
        create_time=lambda:1.0,ppid=lambda:pytest.fail('per-process parent query'),
        _proc=SimpleNamespace(exe=lambda:'synthetic.exe'))


def test_initial_population_is_map_keys_and_no_other_enumeration(monkeypatch):
    current=tool();calls=[]
    monkeypatch.setattr(current,'read_windows_parent_map',lambda:{2:1,1:0})
    monkeypatch.setattr(current.psutil,'pids',lambda:pytest.fail('second population'))
    monkeypatch.setattr(current.psutil,'process_iter',lambda:pytest.fail('cached iterator'))
    def process(pid):calls.append(pid);return fake_process(pid)
    monkeypatch.setattr(current.psutil,'Process',process)
    result=current.enumerate_windows_v2()
    assert calls==[1,2] and [r['parent_pid'] for r in result]==[0,1]


@pytest.mark.parametrize('constructor',[True,False])
def test_skip_only_pre_yield_constructor_disappearance(monkeypatch,constructor):
    current=tool();monkeypatch.setattr(current,'read_windows_parent_map',lambda:{1:0})
    def gone():raise current.psutil.NoSuchProcess(1)
    def process(pid):
        if constructor:gone()
        value=fake_process(pid);value.name=gone;return value
    monkeypatch.setattr(current.psutil,'Process',process)
    if constructor:assert current.enumerate_windows_v2()==[]
    else:
        with pytest.raises(ValueError):current.enumerate_windows_v2()


def test_backend_error_refuses(monkeypatch):
    current,_=mock_backend(monkeypatch,{1:0})
    def error():raise OSError('synthetic unavailable backend')
    monkeypatch.setattr(current.backend,'ppid_map',error)
    with pytest.raises(ValueError):current.read_windows_parent_map()


def test_non_windows_dependency_refuses(monkeypatch):
    current,calls=mock_backend(monkeypatch,{1:0})
    monkeypatch.setattr(current.psutil,'WINDOWS',False)
    with pytest.raises(ValueError):current.read_windows_parent_map()
    assert not calls


def test_initial_required_identity_refuses_but_image_can_be_unavailable(monkeypatch):
    current=tool();monkeypatch.setattr(current,'read_windows_parent_map',lambda:{1:0})
    process=fake_process(1)
    def denied():raise current.psutil.AccessDenied(1)
    process.exe=denied;monkeypatch.setattr(current.psutil,'Process',lambda pid:process)
    assert current.enumerate_windows_v2()[0]['executable_path'] is None
    process.create_time=denied
    with pytest.raises(ValueError):current.enumerate_windows_v2()


@pytest.mark.parametrize('final', [{1: 0, 2: 1}, {1: 0}, {1: 0, 2: 99, 3: 2}])
def test_initial_parent_change_or_disappearance_refuses_before_selection(monkeypatch, final):
    current = tool()
    tables = iter([{1: 0, 2: 99}, final])
    monkeypatch.setattr(current, 'read_windows_parent_map', lambda: next(tables))
    monkeypatch.setattr(current.psutil, 'Process', fake_process)
    with pytest.raises(ValueError, match='initial parent'):
        current.enumerate_windows_v2()


@pytest.mark.parametrize('module_name', ['cext', '_common', '_compat'])
def test_transitive_dependency_digest_refuses(tmp_path, monkeypatch, module_name):
    current, calls = mock_backend(monkeypatch, {1: 0})
    module = current.backend.cext if module_name == 'cext' else importlib.import_module('psutil.' + module_name)
    from pathlib import Path
    path = tmp_path / Path(module.__file__).name
    path.write_bytes(b'changed dependency')
    monkeypatch.setattr(module, '__file__', str(path))
    with pytest.raises(ValueError):
        current.read_windows_parent_map()
    assert calls == []
