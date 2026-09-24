"""Checked Windows ABI only; import never loads DLLs or starts processes."""
import ctypes as c
import os

DWORD=c.c_uint32
BOOL=c.c_int32
HANDLE=c.c_void_p
SIZE_T=c.c_size_t
WORD=c.c_uint16
CREATE_FLAGS=0x4 | 0x80000 | 0x08000000
JOB_LIST_ATTRIBUTE=0x0002000D
JOB_KILL_ON_CLOSE=0x2000


class FILETIME(c.Structure):
    _fields_=[('low',DWORD),('high',DWORD)]


class STARTUPINFO(c.Structure):
    _fields_=[('cb',DWORD),('reserved',c.c_wchar_p),('desktop',c.c_wchar_p),
        ('title',c.c_wchar_p),('x',DWORD),('y',DWORD),('xsize',DWORD),('ysize',DWORD),
        ('xchars',DWORD),('ychars',DWORD),('fill',DWORD),('flags',DWORD),
        ('show',WORD),('reserved2_count',WORD),('reserved2',c.c_void_p),
        ('stdin',HANDLE),('stdout',HANDLE),('stderr',HANDLE)]


class STARTUPINFOEX(c.Structure):
    _fields_=[('startup',STARTUPINFO),('attributes',c.c_void_p)]


class PROCESS_INFORMATION(c.Structure):
    _fields_=[('process',HANDLE),('thread',HANDLE),('pid',DWORD),('tid',DWORD)]


class BASIC_LIMITS(c.Structure):
    _fields_=[('process_time',c.c_int64),('job_time',c.c_int64),('flags',DWORD),
        ('min_ws',SIZE_T),('max_ws',SIZE_T),('active',DWORD),('affinity',SIZE_T),
        ('priority',DWORD),('scheduling',DWORD)]


class IO_COUNTERS(c.Structure):
    _fields_=[(name,c.c_uint64) for name in ('read_ops','write_ops','other_ops',
                                          'read_bytes','write_bytes','other_bytes')]


class JOBOBJECT_EXTENDED_LIMIT_INFORMATION(c.Structure):
    _fields_=[('basic',BASIC_LIMITS),('io',IO_COUNTERS),('process_memory',SIZE_T),
              ('job_memory',SIZE_T),('peak_process',SIZE_T),('peak_job',SIZE_T)]


DEFINITIONS={
 'GetModuleFileNameW':([HANDLE,c.c_wchar_p,DWORD],DWORD),
 'CreateJobObjectW':([c.c_void_p,c.c_wchar_p],HANDLE),
 'SetInformationJobObject':([HANDLE,c.c_int,c.c_void_p,DWORD],BOOL),
 'QueryInformationJobObject':([HANDLE,c.c_int,c.c_void_p,DWORD,c.POINTER(DWORD)],BOOL),
 'InitializeProcThreadAttributeList':([c.c_void_p,DWORD,DWORD,c.POINTER(SIZE_T)],BOOL),
 'UpdateProcThreadAttribute':([c.c_void_p,DWORD,SIZE_T,c.c_void_p,SIZE_T,c.c_void_p,c.c_void_p],BOOL),
 'DeleteProcThreadAttributeList':([c.c_void_p],None),
 'CreateProcessW':([c.c_wchar_p,c.c_wchar_p,c.c_void_p,c.c_void_p,BOOL,DWORD,
                    c.c_void_p,c.c_wchar_p,c.c_void_p,c.POINTER(PROCESS_INFORMATION)],BOOL),
 'IsProcessInJob':([HANDLE,HANDLE,c.POINTER(BOOL)],BOOL),
 'GetProcessTimes':([HANDLE,c.POINTER(FILETIME),c.POINTER(FILETIME),
                     c.POINTER(FILETIME),c.POINTER(FILETIME)],BOOL),
 'GetProcessId':([HANDLE],DWORD),
 'ResumeThread':([HANDLE],DWORD),
 'CloseHandle':([HANDLE],BOOL),
 'WaitForSingleObject':([HANDLE,DWORD],DWORD),
 'GetExitCodeProcess':([HANDLE,c.POINTER(DWORD)],BOOL),
 'OpenProcess':([DWORD,BOOL,DWORD],HANDLE),
 'TerminateProcess':([HANDLE,DWORD],BOOL),
 'GetCurrentProcess':([],HANDLE),
 'DuplicateHandle':([HANDLE,HANDLE,HANDLE,c.POINTER(HANDLE),DWORD,BOOL,DWORD],BOOL),
}


def kernel_api():
    if os.name!='nt' or c.sizeof(HANDLE)!=8:
        raise RuntimeError('Windows 64-bit qualification required')
    kernel=c.WinDLL('kernel32',use_last_error=True)
    for name,(args,result) in DEFINITIONS.items():
        fn=getattr(kernel,name);fn.argtypes=args;fn.restype=result
    return kernel


def checked(value):
    if not value:raise OSError('Windows API failure')
    return value


def close(kernel,handle):
    if handle:checked(kernel.CloseHandle(handle))


def new_job(kernel):
    job=checked(kernel.CreateJobObjectW(None,None))
    limits=JOBOBJECT_EXTENDED_LIMIT_INFORMATION();limits.basic.flags=JOB_KILL_ON_CLOSE
    try:checked(kernel.SetInformationJobObject(job,9,c.byref(limits),c.sizeof(limits)))
    except BaseException:
        close(kernel,job);raise
    return job


class Attributes:
    """All lpValue storage survives until attribute-list deletion."""
    def __init__(self,kernel,job):
        self.kernel=kernel;self.buffer=None;self.initialized=False
        self.jobs=(HANDLE*1)(job)

    def __enter__(self):
        size=SIZE_T()
        result=self.kernel.InitializeProcThreadAttributeList(None,1,0,c.byref(size))
        error=c.get_last_error() if os.name=='nt' else 122
        if result or error!=122 or not 0<size.value<=65536:
            raise OSError('Unexpected attribute sizing response')
        self.buffer=c.create_string_buffer(size.value)
        checked(self.kernel.InitializeProcThreadAttributeList(self.buffer,1,0,c.byref(size)))
        self.initialized=True
        try:
            checked(self.kernel.UpdateProcThreadAttribute(self.buffer,0,JOB_LIST_ATTRIBUTE,
                c.cast(self.jobs,c.c_void_p),c.sizeof(self.jobs),None,None))
        except BaseException:
            self.__exit__(None,None,None);raise
        return self

    def __exit__(self,*args):
        if self.initialized:
            self.kernel.DeleteProcThreadAttributeList(self.buffer);self.initialized=False


def identity(kernel,process):
    times=[FILETIME() for _ in range(4)]
    checked(kernel.GetProcessTimes(process,*(c.byref(t) for t in times)))
    return {'pid':int(checked(kernel.GetProcessId(process))),
            'creation_time':(times[0].high<<32)|times[0].low}


def member(kernel,process,job):
    value=BOOL();checked(kernel.IsProcessInJob(process,job,c.byref(value)))
    return bool(value.value)


def wait(kernel,process,milliseconds=0):
    result=kernel.WaitForSingleObject(process,milliseconds)
    if result not in (0,258):raise OSError('Process wait failed')
    return result==0


def census(kernel,job):
    class PID_LIST(c.Structure):
        _fields_=[('assigned',DWORD),('count',DWORD),('pids',SIZE_T*16)]
    data=PID_LIST();returned=DWORD()
    checked(kernel.QueryInformationJobObject(job,3,c.byref(data),c.sizeof(data),c.byref(returned)))
    if data.assigned!=data.count or data.count>16:raise OSError('Unexpected job census')
    return set(data.pids[:data.count])
