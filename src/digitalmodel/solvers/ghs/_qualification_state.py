"""Bounded private observations and persistent synthetic-only attempt interlock."""
import hashlib
import json
import os
from pathlib import Path
import stat
import uuid

ROLES={'parent','child','control','controller_before','controller_after','abnormal','breakaway','abnormal_child'}
STAGES={'reserved','creation_may_have_occurred','identities_recorded','cleanup_confirmed'}


def no_reparse(path):
    path=Path(path)
    for part in (path,*path.parents):
        if part.exists() or part.is_symlink():
            info=part.lstat()
            if stat.S_ISLNK(info.st_mode) or getattr(info,'st_file_attributes',0)&0x400:
                raise ValueError('Reparse path refused')
    return path


def encoded(value):
    data=json.dumps(value,sort_keys=True,separators=(',',':'),allow_nan=False).encode()
    if len(data)>65536:raise ValueError('Record exceeds bound')
    return data


def publish(path,value):
    path=no_reparse(path);temporary=path.with_name(path.name+'.'+uuid.uuid4().hex+'.tmp')
    try:
        with temporary.open('xb') as stream:
            stream.write(encoded(value));stream.flush();os.fsync(stream.fileno())
        # Windows rename does not overwrite an existing target.
        if path.exists():raise FileExistsError(path)
        temporary.rename(path)
    finally:
        if temporary.exists():temporary.unlink()


def read_record(path):
    path=no_reparse(path)
    with path.open('rb') as stream:data=stream.read(65537)
    if len(data)>65536:raise ValueError('Record exceeds bound')
    try:value=json.loads(data)
    except (ValueError,UnicodeError):raise ValueError('Invalid readiness record') from None
    if type(value) is not dict or value.get('role') not in ROLES:
        raise ValueError('Invalid readiness role')
    return value


def state_root():
    if os.name!='nt':raise RuntimeError('Windows LocalAppData required')
    import ctypes as c
    from ._windows_job import checked
    # CSIDL_LOCAL_APPDATA resolves the actual current-user shell folder.
    shell=c.WinDLL('shell32',use_last_error=True)
    fn=shell.SHGetFolderPathW
    fn.argtypes=[c.c_void_p,c.c_int,c.c_void_p,c.c_uint32,c.c_wchar_p];fn.restype=c.c_long
    buffer=c.create_unicode_buffer(32768)
    if fn(None,0x1c,None,0,buffer)!=0 or not buffer.value:
        raise OSError('Local state resolution failed')
    return no_reparse(Path(buffer.value)/'digitalmodel'/'ghs-sentinel-qualification')


class Attempt:
    def __init__(self):
        self.root=state_root();self.active=self.root/'active.json'
        self.record={'schema_version':1,'attempt_id':uuid.uuid4().hex,'stage':'reserved'}
        self.reserved=False

    def reserve(self):
        no_reparse(self.root);self.root.mkdir(parents=True,exist_ok=True)
        with self.active.open('xb') as stream:
            stream.write(encoded(self.record));stream.flush();os.fsync(stream.fileno())
        self.reserved=True

    def stage(self,name,**details):
        if not self.reserved or name not in STAGES:raise ValueError('Invalid attempt stage')
        allowed={'reserved':{'creation_may_have_occurred','cleanup_confirmed'},
                 'creation_may_have_occurred':{'identities_recorded','cleanup_confirmed'},
                 'identities_recorded':{'identities_recorded','creation_may_have_occurred','cleanup_confirmed'},
                 'cleanup_confirmed':{'cleanup_confirmed'}}
        if name not in allowed[self.record['stage']]:raise ValueError('Invalid stage transition')
        self.record.update(stage=name,**details)
        # The marker remains present even when a partial update fails.
        with self.active.open('r+b') as stream:
            stream.seek(0);stream.write(encoded(self.record));stream.truncate()
            stream.flush();os.fsync(stream.fileno())

    def finish(self,outcome,*,cleanup_confirmed):
        if cleanup_confirmed is not True or self.record['stage']!='cleanup_confirmed':
            raise ValueError('Unresolved attempt cannot finalize')
        self.stage('cleanup_confirmed',outcome=outcome)
        target=self.root/(self.record['attempt_id']+'.completed.json')
        if target.exists():raise FileExistsError(target)
        self.active.rename(target)
        return target
