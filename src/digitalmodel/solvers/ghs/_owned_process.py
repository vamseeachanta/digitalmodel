"""Sentinel-only native launch. No caller argv, interpreter or script override."""
import ctypes as c
import hashlib
import os
from pathlib import Path
import subprocess
import sys
import time

from . import _windows_job as w
from ._qualification_state import ROLES,no_reparse
from ._qualification_contract import PROFILE


def native_image(kernel):
    buffer=c.create_unicode_buffer(32768)
    length=kernel.GetModuleFileNameW(None,buffer,len(buffer))
    if not 0<length<len(buffer):raise OSError('Current process image unresolved')
    return no_reparse(Path(buffer.value).resolve())


def trusted_interpreter():
    # Windows venv python.exe can be a redirector creating an extra process.
    return native_image(w.kernel_api()) if os.name=='nt' else Path(sys.executable).resolve()


def source_identity():
    script=Path(__file__).with_name('_sentinel.py').resolve()
    interpreter=trusted_interpreter()
    values={}
    for name,path in [('sentinel',script),('interpreter',interpreter)]:
        no_reparse(path)
        with path.open('rb') as stream:
            digest=hashlib.file_digest(stream,'sha256').hexdigest()
        values[name+'_sha256']=digest
    return values


def launch(role,root,*,suspended=False):
    if type(role) is not str or role not in ROLES:raise ValueError('Unknown fixed sentinel role')
    if type(suspended) is not bool:raise ValueError('Invalid suspended flag')
    root=no_reparse(Path(root).resolve())
    if not root.is_dir():raise ValueError('Fresh scenario directory required')
    expected=source_identity();kernel=w.kernel_api();job=w.new_job(kernel)
    process=thread=None
    try:
        info=w.STARTUPINFOEX();info.startup.cb=c.sizeof(info)
        output=w.PROCESS_INFORMATION()
        with w.Attributes(kernel,job) as attributes:
            info.attributes=c.cast(attributes.buffer,c.c_void_p)
            if source_identity()!=expected:raise ValueError('Launch source changed')
            argv=[str(trusted_interpreter()),'-I','-B',
                  str(Path(__file__).with_name('_sentinel.py').resolve()),role,str(root)]
            command=c.create_unicode_buffer(subprocess.list2cmdline(argv))
            w.checked(kernel.CreateProcessW(argv[0],command,None,None,False,w.CREATE_FLAGS,
                                           None,str(root),c.byref(info),c.byref(output)))
        process,thread=output.process,output.thread
        if not w.member(kernel,process,job):raise OSError('Initial job membership missing')
        result=Owned(kernel,job,process,thread,role,time.monotonic())
        if not suspended:result.resume()
        return result
    except BaseException:
        cleanup_failed_launch(kernel,job,process,thread)
        raise


def cleanup_failed_launch(kernel,job,process,thread):
    errors=[]
    if process:
        try:w.checked(kernel.TerminateProcess(process,1))
        except OSError:errors.append('terminate_failed')
    try:w.close(kernel,job)
    except OSError:errors.append('job_close_failed')
    if process:
        try:
            if not w.wait(kernel,process,PROFILE['cleanup_timeout']*1000):errors.append('termination_unconfirmed')
        except OSError:errors.append('wait_failed')
    for handle in (thread,process):
        try:w.close(kernel,handle)
        except OSError:errors.append('handle_close_failed')
    if errors:raise RuntimeError('Failed launch cleanup uncertain: '+','.join(errors))



class Owned:
    def __init__(self,kernel,job,process,thread,role,started):
        self.kernel=kernel;self.job=job;self.process=process;self.thread=thread
        self.role=role;self.started=started;self.identity=w.identity(kernel,process)
        self.resumed=False;self.closed=False

    def resume(self):
        if self.resumed:raise ValueError('Already resumed')
        if self.kernel.ResumeThread(self.thread)==0xffffffff:raise OSError('Resume failed')
        self.resumed=True

    def close_job(self):
        if self.job:
            w.close(self.kernel,self.job);self.job=None

    def cleanup(self):
        errors=[]
        try:self.close_job()
        except OSError:errors.append('job_close')
        try:
            if not w.wait(self.kernel,self.process,PROFILE['cleanup_timeout']*1000):errors.append('termination_unconfirmed')
        except OSError:errors.append('wait_failed')
        for handle in (self.thread,self.process):
            try:w.close(self.kernel,handle)
            except OSError:errors.append('handle_close')
        self.closed=True
        if errors:raise RuntimeError('Owned cleanup uncertain: '+','.join(errors))


def retain(kernel,record,*,controller=False):
    if (type(record) is not dict or type(record.get('pid')) is not int
            or not 0<record['pid']<=0xffffffff or type(record.get('creation_time')) is not int):
        raise ValueError('Invalid process identity')
    access=0x1000|0x100000|(0x1|0x40 if controller else 0)
    handle=w.checked(kernel.OpenProcess(access,False,record['pid']))
    try:
        if w.identity(kernel,handle)!=record:raise ValueError('Process identity changed')
        return handle
    except BaseException:
        w.close(kernel,handle);raise


def duplicate_job(kernel,controller,job_value):
    duplicate=w.HANDLE()
    w.checked(kernel.DuplicateHandle(controller,w.HANDLE(job_value),kernel.GetCurrentProcess(),
                                     c.byref(duplicate),0,False,2))
    return duplicate.value
