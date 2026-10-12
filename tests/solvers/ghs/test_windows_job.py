"""Pointer-width/flag contracts are importable on Linux."""
import ctypes
import os
import pytest
from digitalmodel.solvers.ghs import _windows_job as w


def test_native_structure_layout_and_flags():
    assert ctypes.sizeof(w.DWORD)==4
    assert ctypes.sizeof(w.HANDLE)==ctypes.sizeof(ctypes.c_void_p)
    assert w.CREATE_FLAGS == 0x4 | 0x80000 | 0x08000000
    assert w.JOB_LIST_ATTRIBUTE == 0x0002000D
    assert w.JOB_KILL_ON_CLOSE == 0x2000
    if ctypes.sizeof(w.HANDLE)==8:
        assert ctypes.sizeof(w.STARTUPINFOEX)==112
        assert ctypes.sizeof(w.JOBOBJECT_EXTENDED_LIMIT_INFORMATION)==144


def test_no_dll_loading_on_import():
    if os.name!='nt':
        with pytest.raises(RuntimeError):w.kernel_api()


@pytest.mark.parametrize('failure',['close','wait'])
def test_cleanup_attempts_all_handle_releases(failure,monkeypatch):
    from digitalmodel.solvers.ghs import _owned_process as o
    calls=[]
    class Kernel:
        def CloseHandle(self,handle):
            calls.append(handle)
            return not(handle==1 and failure=='close')
        def WaitForSingleObject(self,*args):
            return 0xffffffff if failure=='wait' else 0
    monkeypatch.setattr(w,'identity',lambda *args:{'pid':1,'creation_time':2})
    item=o.Owned(Kernel(),1,2,3,'child',0)
    with pytest.raises(RuntimeError):item.cleanup()
    assert calls==[1,3,2]
    if failure=='close':assert item.job==1
