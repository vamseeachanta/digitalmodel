"""Failure-injected Win32 boundary: no Windows process is created by these tests."""
import ctypes as c
from pathlib import Path
import pytest
from digitalmodel.solvers.ghs import _owned_process as o
from digitalmodel.solvers.ghs import _windows_job as w


class Kernel:
    def __init__(self,failure=None):self.failure=failure;self.calls=[]
    def result(self,name,value=1):
        self.calls.append(name)
        return 0 if self.failure==name else value
    def CreateJobObjectW(self,*args):return self.result('job',10)
    def SetInformationJobObject(self,job,kind,limits,size):
        assert c.cast(limits,c.POINTER(w.JOBOBJECT_EXTENDED_LIMIT_INFORMATION)).contents.basic.flags==0x2000
        return self.result('limits')
    def InitializeProcThreadAttributeList(self,buffer,count,flags,size):
        c.cast(size,c.POINTER(w.SIZE_T)).contents.value=128
        if buffer is None:
            if hasattr(c,'set_last_error'):c.set_last_error(122)
            return self.result('size',0)
        return self.result('init')
    def UpdateProcThreadAttribute(self,buffer,flags,key,value,size,*args):
        assert key==0x2000d and c.cast(value,c.POINTER(w.HANDLE)).contents.value==10
        return self.result('update')
    def DeleteProcThreadAttributeList(self,*args):self.calls.append('delete')
    def CreateProcessW(self,exe,command,pa,ta,inherit,flags,env,cwd,startup,result):
        self.calls.append('create')
        assert not inherit and flags==w.CREATE_FLAGS
        assert str(Path(exe).resolve())==str(Path(o.sys.executable).resolve())
        assert '_sentinel.py' in command.value
        if self.failure=='create':return 0
        info=c.cast(result,c.POINTER(w.PROCESS_INFORMATION)).contents
        info.process=20;info.thread=30;info.pid=40;return 1
    def IsProcessInJob(self,process,job,result):
        c.cast(result,c.POINTER(w.BOOL)).contents.value=self.failure!='membership_value'
        return self.result('membership')
    def ResumeThread(self,*args):return self.result('resume',0xffffffff if self.failure=='resume' else 1) if self.failure!='resume' else 0xffffffff
    def CloseHandle(self,handle):return self.result('close_'+str(handle))
    def TerminateProcess(self,*args):return self.result('terminate')
    def WaitForSingleObject(self,*args):
        self.calls.append('wait');return 0xffffffff if self.failure=='wait' else 0


def setup(monkeypatch,failure=None):
    kernel=Kernel(failure)
    monkeypatch.setattr(o,'trusted_interpreter',lambda:Path(o.sys.executable).resolve())
    monkeypatch.setattr(w,'kernel_api',lambda:kernel)
    monkeypatch.setattr(w,'identity',lambda *args:{'pid':40,'creation_time':5})
    monkeypatch.setattr(o,'source_identity',lambda:{'sentinel_sha256':'a','interpreter_sha256':'b'})
    return kernel


@pytest.mark.parametrize('failure',['job','limits','init','update','create','membership','membership_value','resume'])
def test_failure_never_resumes_unchecked_child(monkeypatch,tmp_path,failure):
    kernel=setup(monkeypatch,failure)
    with pytest.raises((OSError,RuntimeError)):o.launch('child',tmp_path)
    if failure!='resume':assert 'resume' not in kernel.calls
    if failure not in {'job'}:assert 'close_10' in kernel.calls
    if failure in {'membership','membership_value','resume'}:
        assert 'close_20' in kernel.calls and 'close_30' in kernel.calls


def test_suspended_launch_has_no_resume_and_attributes_live_through_create(monkeypatch,tmp_path):
    kernel=setup(monkeypatch)
    item=o.launch('parent',tmp_path,suspended=True)
    assert 'resume' not in kernel.calls
    assert kernel.calls.index('update')<kernel.calls.index('create')<kernel.calls.index('delete')
    item.cleanup()


@pytest.mark.parametrize('failure',['terminate','close_10','wait','close_30'])
def test_failed_launch_cleanup_attempts_every_handle(monkeypatch,failure):
    kernel=setup(monkeypatch,failure)
    with pytest.raises(RuntimeError):o.cleanup_failed_launch(kernel,10,20,30)
    assert 'close_10' in kernel.calls and 'close_20' in kernel.calls and 'close_30' in kernel.calls



@pytest.mark.parametrize('expired',[False,True])
@pytest.mark.parametrize('before',[False,True])
def test_crash_barrier_closes_observer_job_before_controller_termination(monkeypatch,tmp_path,before,expired):
    from types import SimpleNamespace
    from digitalmodel.solvers.ghs import _qualification_scenarios as s
    events=[];identities=[{'pid':n,'creation_time':n} for n in (1,2,3)]
    role='controller_before' if before else 'controller_after'
    controller=SimpleNamespace(identity=identities[0],role=role,started=s.time.monotonic())
    kernel=SimpleNamespace(TerminateProcess=lambda *args:events.append('terminate') or 1)
    r=SimpleNamespace(start=lambda *args:controller,kernel=kernel,retained=[],jobs=[],
                      retain=lambda identity:identity['pid'])
    records=iter([{'role':role,'identity':identities[0],'target':identities[1],'job_handle':7},
                  {'role':'parent','identity':identities[1],'child':identities[2]}])
    monkeypatch.setattr(s,'ready',lambda path,role:next(records))
    monkeypatch.setattr(s.o,'retain',lambda *args,**kwargs:1)
    monkeypatch.setattr(s.o,'duplicate_job',lambda *args:7)
    monkeypatch.setattr(s.w,'member',lambda *args:True)
    monkeypatch.setattr(s.w,'census',lambda *args:{2} if before else {2,3})
    monkeypatch.setattr(s.w,'close',lambda *args:events.append('duplicate_closed'))
    monkeypatch.setattr(s.w,'wait',lambda *args:True)
    monkeypatch.setattr(s,'alive',lambda *args:None)
    monkeypatch.setattr(s,'terminated',lambda *args:events.append('terminated'))
    if expired:
        controller.started -= 11
        with pytest.raises(ValueError, match='watchdog margin'):
            s.crash('controller_before_resume' if before else 'controller_after_readiness',tmp_path,r)
        assert 'terminate' not in events
        return
    result=s.crash('controller_before_resume' if before else 'controller_after_readiness',tmp_path,r)
    assert events==['duplicate_closed','terminate','terminated']
    assert result['resume_called'] is not before
    assert (tmp_path/'resume_ack').exists() is not before



def test_native_image_comes_from_current_process_not_venv_redirector(tmp_path):
    from types import SimpleNamespace
    real=tmp_path/'actual-python.exe';real.write_bytes(b'fixture')
    def image(module,buffer,size):
        assert module is None
        buffer.value=str(real);return len(str(real))
    assert o.native_image(SimpleNamespace(GetModuleFileNameW=image))==real


def test_native_image_truncation_fails_before_launch():
    from types import SimpleNamespace
    with pytest.raises(OSError):o.native_image(SimpleNamespace(GetModuleFileNameW=lambda *args:32768))
