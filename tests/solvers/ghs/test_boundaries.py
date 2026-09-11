"""Bounds, component binding and a complete synthetic pipeline."""
import copy
import itertools
from pathlib import Path
import pytest
from digitalmodel.solvers.ghs import contracts as c,runner,comparison
from .test_contracts import inputs,packet,receipt
from .test_comparison import normalized


@pytest.mark.parametrize('mutation',['integer','text','nested','depths'])
def test_validate_before_hash(monkeypatch,mutation):
    p=packet()
    if mutation=='integer':p['resource_limits']['wall_timeout_seconds']=10**5000
    elif mutation=='text':p['case_id']='x'*100000
    elif mutation=='nested':p['density_kg_m3']={'unexpected':['x']*1000}
    else:p['depths_m']=['1']*10000
    def forbidden(*a,**k):raise AssertionError('hash called before bounds')
    monkeypatch.setattr(c,'digest',forbidden)
    with pytest.raises(ValueError,match='^invalid, noncanonical or unqualified canary evidence$'):
        c.validate_prepared(p)


def test_synthetic_pipeline_binds_prepared_packet(tmp_path):
    args=inputs();p=c.prepare_canary(*args);calls=[]
    assert c.preview_approval(receipt(p),p,approved_digest=p['packet_sha256'],
                now='2029-01-01T00:00:00Z',consumed_nonces=[])['launch_allowed'] is False
    def backend(**kwargs):
        calls.append(kwargs)
        return [{'kind':'report','data':b'SYNTHETIC REPORT NOT NATIVE'},
                {'kind':'stdout','data':b'SYNTHETIC DIAGNOSTICS'}, {'kind':'exit','returncode':0}]
    result=runner.run_synthetic_capture(p,*args,tmp_path/'stage',backend)
    assert len(calls)==1 and calls[0]['argv']==c.build_argv(args[1])
    assert calls[0]['limits']==c.RESOURCE_LIMITS
    assert result['packet_sha256']==p['packet_sha256']
    assert result['state']=='capture_unqualified'
    rows,evidence=normalized()
    assessed=comparison.compare_normalized(p,rows,evidence)
    assert assessed['packet_sha256']==result['packet_sha256']
    assert assessed['licensed_execution_verified'] is False


@pytest.mark.parametrize('mutate',['geometry','runfile','output','unknown_file','directory_link'])
def test_stage_substitution_rejects(tmp_path,mutate):
    args=inputs();p=c.prepare_canary(*args)
    def backend(**kwargs):
        work=Path(kwargs['stage']['work'])
        if mutate=='geometry':(work/'canary.gf').write_bytes(b'wrong')
        elif mutate=='runfile':(work/'canary.rf').write_bytes(b'wrong')
        elif mutate=='output':(work/'canary.pf').write_bytes(b'stale')
        elif mutate=='unknown_file':(work/'OPEN-RF.RF').write_bytes(b'unknown')
        else:
            target=tmp_path/'replacement';work.rename(target)
            try:work.symlink_to(target,target_is_directory=True)
            except OSError:pytest.skip('symlink unavailable')
        return [{'kind':'exit','returncode':0}]
    with pytest.raises(ValueError):runner.run_synthetic_capture(p,*args,tmp_path/'stage',backend)


def test_swapped_packet_never_calls_backend(tmp_path):
    args=inputs();p=c.prepare_canary(*args);args[1]['executable_sha256']='b'*64
    def forbidden(**kwargs):raise AssertionError('backend called on swapped profile')
    with pytest.raises(ValueError):runner.run_synthetic_capture(p,*args,tmp_path/'stage',forbidden)
    assert not (tmp_path/'stage').exists()


@pytest.mark.parametrize('events',[[{'kind':'exit','returncode':True}],
    [{'kind':'exit','returncode':0},{'kind':'exit','returncode':0}],
    [{'kind':'stdout','data':b'no terminal'}]])
def test_bad_terminal_evidence(events):
    try:result=runner.inspect_synthetic_events(events)
    except ValueError:return
    assert result['state']=='capture_failed'


def test_event_cap_stops_infinite_empty_chunks():
    result=runner.inspect_synthetic_events(itertools.repeat({'kind':'stdout','data':b''}))
    assert 'event_limit' in result['failure_reasons']


def test_total_retention_limit(monkeypatch):
    monkeypatch.setattr(runner,'TOTAL_LIMIT',3)
    result=runner.inspect_synthetic_events([{'kind':'stdout','data':b'aa'}, {'kind':'stderr','data':b'bb'}])
    assert 'overflow' in result['failure_reasons']


def test_iterator_failure_preserves_partial_evidence():
    import hashlib
    def events():
        yield {'kind':'stdout','data':b'partial'}
        raise OSError('synthetic reader failed')
    result=runner.inspect_synthetic_events(events())
    assert result['state']=='capture_failed'
    assert result['artifact_hashes']['stdout']==hashlib.sha256(b'partial').hexdigest()
    assert 'stream_error' in result['failure_reasons']


def test_oversize_substitution_rejected_before_read(tmp_path,monkeypatch):
    args=inputs();p=c.prepare_canary(*args)
    def backend(**kwargs):
        path=Path(kwargs['stage']['work'])/'canary.gf'
        with path.open('wb') as handle:handle.truncate(8*1024*1024+1)
        return [{'kind':'exit','returncode':0}]
    def forbidden(*a,**k):raise AssertionError('unbounded read_bytes forbidden')
    monkeypatch.setattr(Path,'read_bytes',forbidden)
    with pytest.raises(ValueError):runner.run_synthetic_capture(p,*args,tmp_path/'stage',backend)


def test_text_limit_precedes_string_scanning():
    import sys
    from digitalmodel.solvers.ghs._canonical import text
    value=' '*4096+'x'
    scanned=[]
    def observe(frame,event,arg):
        if event=='c_call' and getattr(arg,'__self__',None) is value:
            scanned.append(arg.__name__)
    previous=sys.getprofile()
    try:
        sys.setprofile(observe)
        with pytest.raises(ValueError):text(value,128)
    finally:
        sys.setprofile(previous)
    assert scanned==[]


def test_synthetic_staging_never_claims_race_containment(tmp_path):
    from digitalmodel.solvers.ghs.runner import stage_synthetic, inspect_synthetic_events
    stage=stage_synthetic(tmp_path/'fresh',b'synthetic')
    result=inspect_synthetic_events([{'kind':'exit','returncode':0}])
    assert stage['containment_qualified'] is False
    assert result['containment_qualified'] is False
    assert any('race' in note for note in result['limitations'])
