"""Qualification boundaries independent of licensed software."""
import copy
import json
import os
from pathlib import Path

import pytest

from digitalmodel.solvers.ghs import qualification as q
from digitalmodel.solvers.ghs import _qualification_state as s
from digitalmodel.solvers.ghs import _owned_process as owned


@pytest.mark.parametrize('value',[None,True,1.0,-1,0,10**100, float('nan')])
def test_profile_rejects_invalid_scalar(value):
    profile=dict(q.PROFILE);profile['cleanup_timeout']=value
    with pytest.raises(ValueError):q.validate_profile(profile)


def test_profile_closed_and_fixed():
    assert q.validate_profile(None)==q.PROFILE
    for change in ({'other':1},{'watchdog':31},{'margin':0}):
        with pytest.raises(ValueError):q.validate_profile(dict(q.PROFILE,**change))


@pytest.mark.parametrize('override',[{'argv':['evil']},{'executable':'evil'},
                                     {'script':'evil'},{'shell':True}])
def test_launch_override_has_no_native_side_effect(tmp_path,override):
    with pytest.raises(TypeError):owned.launch('child',tmp_path,**override)
    assert list(tmp_path.iterdir())==[]


def test_unknown_role_fails_before_native_api(tmp_path):
    with pytest.raises(ValueError):owned.launch('vendor',tmp_path)


def test_non_windows_public_call_never_creates_output(tmp_path):
    if os.name=='nt':pytest.skip('Linux platform gate')
    target=tmp_path/'new'
    with pytest.raises(RuntimeError):q.qualify(target)
    assert not target.exists()


def test_timing_margin_rejects_natural_exit_confounds():
    q.check_timing(100.0,101.0)
    for now in (110.1,123.0,130.0,float('nan'),99.0):
        with pytest.raises(ValueError):q.check_timing(100.0,now)


def test_persistent_reservation_and_no_cross_output_bypass(tmp_path,monkeypatch):
    monkeypatch.setattr(s,'state_root',lambda:tmp_path/'state')
    attempt=s.Attempt();attempt.reserve()
    attempt.stage('creation_may_have_occurred')
    with pytest.raises(FileExistsError):s.Attempt().reserve()
    assert json.loads(attempt.active.read_text())['stage']=='creation_may_have_occurred'
    with pytest.raises(ValueError):attempt.finish({'state':'passed'},cleanup_confirmed=False)
    assert attempt.active.exists()


def test_completed_marker_not_removed_without_durable_outcome(tmp_path,monkeypatch):
    monkeypatch.setattr(s,'state_root',lambda:tmp_path/'state')
    attempt=s.Attempt();attempt.reserve();attempt.stage('cleanup_confirmed')
    done=attempt.finish({'state':'failed'},cleanup_confirmed=True)
    assert done.exists() and not attempt.active.exists()
    assert json.loads(done.read_text())['outcome']['state']=='failed'
    s.Attempt().reserve()


def test_public_summary_deidentifies_recursively():
    private={'state':'sentinel_containment_passed','pid':123,'account':'private-user',
             'path':'C:/private','scenarios':[{'name':'timeout','passed':True,'pid':456}],
             'runtime':{'executable':'C:/private/python.exe'}}
    public=q.public_summary(private)
    encoded=json.dumps(public)
    assert all(secret not in encoded for secret in ('private-user','C:/private','456','executable'))
    assert public['ghs_launch_allowed'] is False
    assert public['licensed_execution_verified'] is False
    assert len(public['private_evidence_sha256'])==64


def test_readiness_rejects_oversize_and_unknown_roles(tmp_path):
    path=tmp_path/'ready.json';path.write_bytes(b'x'*65537)
    with pytest.raises(ValueError):s.read_record(path)
    path.write_text('{"role":"vendor"}')
    with pytest.raises(ValueError):s.read_record(path)


@pytest.mark.parametrize('names',[[],['timeout']*8,list(q.SCENARIOS)[:-1]])
def test_incomplete_scenarios_cannot_be_public_success(names):
    observations=[{'name':n,'passed':True,'cleanup_confirmed':True} for n in names]
    assert q.public_summary({'state':'sentinel_containment_passed','scenarios':observations})['state']!='sentinel_containment_passed'


def test_empty_resource_set_never_proves_cleanup(monkeypatch):
    from digitalmodel.solvers.ghs import _qualification_scenarios as scenarios
    monkeypatch.setattr(scenarios.w,'kernel_api',lambda:object())
    resources=scenarios.Resources(lambda *args,**kw:None)
    assert resources.close_all() is False


def test_non_breakaway_parent_requires_observed_child(monkeypatch,tmp_path):
    from digitalmodel.solvers.ghs import _qualification_scenarios as scenarios
    from types import SimpleNamespace
    identity={'pid':1,'creation_time':2}
    item=SimpleNamespace(identity=identity,process=1,job=10,role='parent')
    resources=SimpleNamespace(kernel=object())
    monkeypatch.setattr(scenarios,'ready',lambda *args,**kw:{'role':'parent','identity':identity,'child':None})
    monkeypatch.setattr(scenarios,'alive',lambda *args:None)
    monkeypatch.setattr(scenarios.w,'member',lambda *args:True)
    monkeypatch.setattr(scenarios.w,'census',lambda *args:{1})
    with pytest.raises(ValueError):scenarios.parent_members(resources,item,tmp_path)



def test_failed_finalization_preserves_active_marker(tmp_path,monkeypatch):
    monkeypatch.setattr(s,'state_root',lambda:tmp_path/'state')
    attempt=s.Attempt();attempt.reserve();attempt.stage('cleanup_confirmed')
    def failed(*args,**kwargs):raise OSError('disk-full simulation')
    monkeypatch.setattr(Path,'rename',failed)
    with pytest.raises(OSError):attempt.finish({'state':'failed'},cleanup_confirmed=True)
    assert attempt.active.exists()
    with pytest.raises(FileExistsError):s.Attempt().reserve()


def test_recovery_state_same_in_public_serialization():
    assert q.public_summary({'state':'recovery_required','scenarios':[]})['state']=='recovery_required'


def test_readiness_exact_role_required(tmp_path):
    from digitalmodel.solvers.ghs._qualification_scenarios import ready
    path=tmp_path/'record.json';path.write_text('{"role":"control"}')
    with pytest.raises(ValueError):ready(path,'parent')


def test_pending_creation_cannot_finalize_even_after_empty_cleanup(monkeypatch,tmp_path):
    from digitalmodel.solvers.ghs import _qualification_scenarios as scenarios
    monkeypatch.setattr(scenarios.w,'kernel_api',lambda:object())
    def uncertain(*args,**kw):raise RuntimeError('create-before-record crash')
    monkeypatch.setattr(scenarios.o,'launch',uncertain)
    r=scenarios.Resources(lambda *a,**kw:None)
    with pytest.raises(RuntimeError):r.start('parent',tmp_path)
    assert r.pending is True and r.close_all() is False


def test_ineffective_kill_cannot_pass_after_natural_watchdog_exit(monkeypatch):
    from digitalmodel.solvers.ghs import _qualification_scenarios as scenarios
    calls=[]
    def wait(kernel,handle,milliseconds):
        calls.append(milliseconds)
        return milliseconds>=30000
    monkeypatch.setattr(scenarios.w,'wait',wait)
    with pytest.raises(ValueError):scenarios.terminated(object(),[1])
    assert calls and 0<=calls[0]<=2000


def test_retained_identity_mismatch_closes_handle(monkeypatch):
    from types import SimpleNamespace
    closed=[]
    kernel=SimpleNamespace(OpenProcess=lambda *args:5,CloseHandle=lambda h:closed.append(h) or True)
    monkeypatch.setattr(owned.w,'identity',lambda *args:{'pid':10,'creation_time':200})
    with pytest.raises(ValueError):owned.retain(kernel,{'pid':10,'creation_time':100})
    assert closed==[5]



def test_timeout_has_observed_deadline_and_rejects_early_exit(monkeypatch):
    from digitalmodel.solvers.ghs import _qualification_scenarios as scenarios
    ticks=iter([0.0,0.1,0.5,1.1]);waits=[]
    monkeypatch.setattr(scenarios.time,'monotonic',lambda:next(ticks))
    monkeypatch.setattr(scenarios,'alive',lambda *args:None)
    monkeypatch.setattr(scenarios.w,'wait',lambda *args:waits.append(args[-1]) or False)
    observation=scenarios.observe_timeout(object(),[1],[{'pid':1,'creation_time':2}])
    assert waits and observation['deadline_seconds']==1
    monkeypatch.setattr(scenarios.time,'monotonic',lambda:0.0)
    monkeypatch.setattr(scenarios.w,'wait',lambda *args:True)
    with pytest.raises(ValueError):scenarios.observe_timeout(object(),[1],[{'pid':1,'creation_time':2}])



def test_completed_attempt_cannot_regress_to_creation(tmp_path,monkeypatch):
    monkeypatch.setattr(s,'state_root',lambda:tmp_path/'state')
    attempt=s.Attempt();attempt.reserve();attempt.stage('cleanup_confirmed')
    with pytest.raises(ValueError):attempt.stage('creation_may_have_occurred')


def test_next_scenario_can_begin_after_recorded_identities(tmp_path,monkeypatch):
    monkeypatch.setattr(s,'state_root',lambda:tmp_path/'state')
    attempt=s.Attempt();attempt.reserve();attempt.stage('creation_may_have_occurred')
    attempt.stage('identities_recorded',identities=[{'pid':1,'creation_time':2}])
    attempt.stage('creation_may_have_occurred')
    assert json.loads(attempt.active.read_text())['stage']=='creation_may_have_occurred'


def test_failed_scenario_diagnostics_survive_work_directory_cleanup(tmp_path):
    directory=tmp_path/'scenario';directory.mkdir()
    (directory/'child.json').write_text('{"role":"child","identity":{"pid":1,"creation_time":2}}')
    records=q.preserve_failed_records(directory)
    assert not directory.exists()
    assert records['child.json']['identity']['creation_time']==2
