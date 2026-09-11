"""Explicit synthetic process events and fresh staging; no licensed executable."""
import pytest
from digitalmodel.solvers.ghs import runner
from .test_contracts import inputs


def test_exclusive_staging_and_hashes(tmp_path):
    args=inputs();root=tmp_path/'fresh'
    result=runner.stage_synthetic(root,args[3])
    assert result['evidence_kind']=='synthetic_staging'
    assert (root/'work/canary.gf').read_bytes()==args[3]
    assert not (root/'work/canary.pf').exists()
    with pytest.raises(ValueError):runner.stage_synthetic(root,args[3])


def test_symlink_staging_rejects(tmp_path):
    real=tmp_path/'real';real.mkdir();link=tmp_path/'link'
    try:link.symlink_to(real,target_is_directory=True)
    except OSError:pytest.skip('symlink unavailable')
    with pytest.raises(ValueError):runner.stage_synthetic(link/'child',b'synthetic')


@pytest.mark.parametrize('event,state',[
    ({'kind':'exit','returncode':0},'capture_unqualified'),
    ({'kind':'exit','returncode':2},'capture_failed'),
    ({'kind':'timeout'},'capture_failed'),({'kind':'launch_error'},'capture_failed'),
    ({'kind':'termination_failed'},'capture_failed')])
def test_synthetic_process_states(event,state):
    result=runner.inspect_synthetic_events([event])
    assert result['state']==state
    assert result['evidence_kind']=='synthetic_process_double'
    assert result['attempts']==1
    assert result['licensed_execution_verified'] is False


def test_incremental_overflow_stops_consumption():
    def events():
        yield {'kind':'stdout','data':b'x'*(8*1024*1024+1)}
        raise AssertionError('must stop consuming after overflow')
    result=runner.inspect_synthetic_events(events())
    assert result['state']=='capture_failed'
    assert 'overflow' in result['failure_reasons']


def test_zero_with_fake_completion_is_unqualified():
    events=[{'kind':'stdout','data':b'COMPLETE licensed success'}, {'kind':'exit','returncode':0}]
    result=runner.inspect_synthetic_events(events)
    assert result['state']=='capture_unqualified'


def test_live_never_calls_process(monkeypatch):
    import subprocess
    def forbidden(*a,**k):raise AssertionError('actual process launch')
    monkeypatch.setattr(subprocess,'Popen',forbidden)
    with pytest.raises(RuntimeError):runner.run_approved_capture({}, {'approved':True})
