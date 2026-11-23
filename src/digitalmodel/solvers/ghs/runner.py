"""Synthetic staging/capture seam. Every live backend remains unavailable."""
import hashlib
import os
import stat
from pathlib import Path

from ._canonical import bytes_hash, fail, integer, shape
from .contracts import RESOURCE_LIMITS, build_argv, preview_runfile, validate_packet

MAX_EVENTS=4096
STREAM_LIMIT=8*1024*1024
TOTAL_LIMIT=32*1024*1024


def run_approved_capture(*args,**kwargs):
    raise RuntimeError('Live GHS disabled: Windows containment, runtime, authority and format qualification required.')


def _is_link(path):
    stat=path.lstat()
    return path.is_symlink() or bool(getattr(stat,'st_file_attributes',0)&0x400)


def stage_synthetic(root,geometry_bytes):
    bytes_hash(geometry_bytes)
    if not isinstance(root,(str,os.PathLike)):fail()
    root=Path(root)
    if '..' in root.parts:fail()
    root=root.absolute()
    for parent in root.parents:
        if parent.exists() and _is_link(parent):fail()
    if root.exists() or root.is_symlink():fail()
    try:
        root.mkdir(mode=0o700);work=root/'work';temp=root/'temp';work.mkdir(mode=0o700);temp.mkdir(mode=0o700)
        for name,data in (('canary.gf',geometry_bytes),('canary.rf',preview_runfile())):
            with (work/name).open('xb') as handle:handle.write(data)
    except OSError as exc:
        raise ValueError('synthetic staging failed; inspect owned residue') from exc
    return {'evidence_kind':'synthetic_staging','containment_qualified':False,'root':str(root),'work':str(work),'temp':str(temp),
            'geometry_sha256':bytes_hash(geometry_bytes),'runfile_sha256':bytes_hash(preview_runfile())}


def _result(failures,hashes,sizes):
    return {'schema_version':1,'state':'capture_failed' if failures else 'capture_unqualified',
        'evidence_kind':'synthetic_process_double','licensed_execution_verified':False,
        'containment_qualified':False,'attempts':1,
        'failure_reasons':failures or ['Native format, diagnostic closure and license evidence unqualified.'],
        'artifact_hashes':{k:v.hexdigest() for k,v in hashes.items()},'byte_counts':sizes,
        'limitations':['Events simulate process behavior; no wall-clock timeout or process termination tested.',
            'No hostile-writer race containment or Windows ACL qualification is established.',
            'Windows reparse checks are observational; O_NOFOLLOW is unavailable there.']}


def inspect_synthetic_events(events):
    hashes={k:hashlib.sha256() for k in ('stdout','stderr','report')};sizes=dict.fromkeys(hashes,0)
    failures=[];terminal=False;total=0
    for count,event in enumerate(_safe_events(events)):
        if count>=MAX_EVENTS:failures.append('event_limit');break
        if terminal:failures.append('post_terminal_event');break
        if type(event) is not dict or type(event.get('kind')) is not str:fail()
        kind=event['kind']
        if kind in hashes:
            shape(event,{'kind','data'});data=event['data']
            if type(data) is not bytes:fail()
            if len(data)+sizes[kind]>STREAM_LIMIT or len(data)+total>TOTAL_LIMIT:
                failures.append('overflow');break
            sizes[kind]+=len(data);total+=len(data);hashes[kind].update(data)
        elif kind=='exit':
            shape(event,{'kind','returncode'});code=integer(event['returncode'],-2147483648,2147483647)
            terminal=True
            if code!=0:failures.append('nonzero_exit')
        elif kind in {'timeout','launch_error','termination_failed','stream_error'}:
            shape(event,{'kind'});terminal=True;failures.append(kind)
        else:fail()
    if not terminal and not failures:failures.append('missing_terminal_event')
    return _result(failures,hashes,sizes)


def _safe_events(events):
    try:
        iterator=iter(events)
        for _ in range(MAX_EVENTS+1):
            try:event=next(iterator)
            except StopIteration:return
            yield event
    except Exception:
        yield {'kind':'stream_error'}


def _read_verified(path,expected):
    before=path.lstat()
    if _is_link(path) or not stat.S_ISREG(before.st_mode) or before.st_size!=len(expected):fail()
    flags=os.O_RDONLY|getattr(os,'O_BINARY',0)|getattr(os,'O_NOFOLLOW',0)
    fd=os.open(path,flags)
    with os.fdopen(fd,'rb') as handle:
        opened=os.fstat(handle.fileno())
        keys=('st_dev','st_ino','st_size','st_mtime_ns')
        if any(getattr(before,k)!=getattr(opened,k) for k in keys):fail()
        data=handle.read(len(expected)+1)
        after=os.fstat(handle.fileno())
        if any(getattr(opened,k)!=getattr(after,k) for k in (*keys,'st_ctime_ns')):fail()
    current=path.lstat()
    if _is_link(path) or any(getattr(before,k)!=getattr(current,k) for k in (*keys,'st_ctime_ns')):fail()
    if data!=expected:fail()


def _verify_stage(stage,geometry):
    root=Path(stage['root']);work=Path(stage['work']);temp=Path(stage['temp'])
    for path in (*root.parents,root,work,temp):
        if not path.is_dir() or _is_link(path):fail()
    if work.parent!=root or temp.parent!=root:fail()
    if {p.name for p in root.iterdir()}!={'work','temp'}:fail()
    for name,expected in (('canary.gf',geometry),('canary.rf',preview_runfile())):
        _read_verified(work/name,expected)
    if {p.name for p in work.iterdir()}!={'canary.gf','canary.rf'}:fail()
    if list(temp.iterdir()):fail()


def run_synthetic_capture(packet,request,runtime_profile,output_policy,geometry_bytes,root,backend):
    """Trusted injected test backend receives exact argv/limits; never a live adapter."""
    validate_packet(packet,request,runtime_profile,output_policy,geometry_bytes)
    stage=stage_synthetic(root,geometry_bytes);_verify_stage(stage,geometry_bytes)
    argv=build_argv(runtime_profile)
    try:events=backend(argv=argv,limits=dict(RESOURCE_LIMITS),stage=dict(stage))
    except Exception:
        result=inspect_synthetic_events([{'kind':'launch_error'}])
    else:result=inspect_synthetic_events(events)
    _verify_stage(stage,geometry_bytes)
    result.update(packet_sha256=packet['packet_sha256'],runtime_profile_sha256=packet['runtime_profile_sha256'],
                  output_policy_sha256=packet['output_policy_sha256'],argv_sha256=packet['argv_sha256'])
    return result
