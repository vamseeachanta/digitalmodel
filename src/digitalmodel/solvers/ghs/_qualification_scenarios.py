"""Eight bounded observed experiments; successful recovery never changes a failed proof."""
import ctypes as c
import time

from . import _windows_job as w
from . import _owned_process as o
from ._qualification_state import read_record
from ._qualification_contract import PROFILE,TIMEOUT_SECONDS

SCENARIOS=('normal_parent_exit','timeout','controller_before_resume','controller_after_readiness',
           'breakaway','unrelated_control','second_handle','abnormal_child_exit')


class ObservationFailure(RuntimeError):
    def __init__(self,name,cleanup_confirmed):
        super().__init__(name);self.cleanup_confirmed=cleanup_confirmed


def ready(path,role=None):
    end=time.monotonic()+PROFILE['readiness_timeout']
    while time.monotonic()<end:
        if path.exists():
            record=read_record(path)
            if role is not None and record.get('role')!=role:raise ValueError('Readiness role mismatch')
            return record
        time.sleep(0.01)
    raise ValueError('Readiness missing')


def check_timing(start,now):
    if (type(start) not in (int,float) or type(now) not in (int,float)
            or not 0<=now-start<=PROFILE['trigger_budget'] or PROFILE['watchdog']-(now-start)<=PROFILE['cleanup_timeout']+PROFILE['margin']):
        raise ValueError('Insufficient independent watchdog margin')


def alive(kernel,handle,identity):
    if w.identity(kernel,handle)!=identity or w.wait(kernel,handle):
        raise ValueError('Expected live identity missing')


def terminated(kernel,handles):
    deadline=time.monotonic()+PROFILE['cleanup_timeout']
    for handle in handles:
        remaining=max(0,int((deadline-time.monotonic())*1000))
        if not w.wait(kernel,handle,remaining):raise ValueError('Cleanup deadline exceeded')


class Resources:
    def __init__(self,journal):
        self.journal=journal;self.identities=[];self.pending=False;self.complete_census=False
        self.owned=[];self.retained=[];self.jobs=[];self.kernel=w.kernel_api()
        self.observations=[]

    def start(self,role,root,*,suspended=False):
        self.pending=True
        self.journal('creation_may_have_occurred',identities=self.identities)
        item=o.launch(role,root,suspended=suspended);self.owned.append(item)
        self.identities.append(dict(item.identity,role=role))
        self.journal('identities_recorded',identities=self.identities)
        self.pending=False
        return item

    def retain(self,identity):
        handle=o.retain(self.kernel,identity);self.retained.append(handle)
        self.identities.append(dict(identity,role='observed_member'))
        self.journal('identities_recorded',identities=self.identities)
        return handle

    def close_all(self):
        errors=[]
        for job in self.jobs:
            try:w.close(self.kernel,job)
            except OSError:errors.append('extra_job')
        self.jobs=[]
        for item in reversed(self.owned):
            try:item.cleanup()
            except (OSError,RuntimeError):errors.append('owned_cleanup')
        for handle in self.retained:
            try:
                if not w.wait(self.kernel,handle,PROFILE['cleanup_timeout']*1000):errors.append('retained_alive')
            except OSError:errors.append('retained_wait')
            finally:
                try:w.close(self.kernel,handle)
                except OSError:errors.append('retained_close')
        return not errors and not self.pending and self.complete_census


def parent_members(resources,item,root):
    record=ready(root/'parent.json',item.role)
    if record.get('identity')!=item.identity:raise ValueError('Parent identity disagreement')
    if not record.get('child') and not (item.role=='breakaway' and
            record.get('breakaway_attempted') is True and record.get('breakaway_error')==5):
        raise ValueError('Required child evidence missing')
    handles=[item.process];identities=[item.identity]
    if record.get('child'):
        child=resources.retain(record['child']);handles.append(child);identities.append(record['child'])
    for handle,identity in zip(handles,identities):
        alive(resources.kernel,handle,identity)
        if not w.member(resources.kernel,handle,item.job):raise ValueError('Member escaped job')
    if w.census(resources.kernel,item.job)!={i['pid'] for i in identities}:
        raise ValueError('Unexpected process census')
    resources.complete_census=True
    return record,handles,identities


def ordinary(name,root,r):
    role='breakaway' if name=='breakaway' else 'abnormal' if name=='abnormal_child_exit' else 'parent'
    item=r.start(role,root)
    record,handles,identities=parent_members(r,item,root)
    control=None
    if name=='unrelated_control':
        control_root=root/'control';control_root.mkdir()
        control=r.start('control',control_root);control_record=ready(control_root/'control.json','control')
        if control_record.get('identity')!=control.identity:raise ValueError('Control identity mismatch')
        alive(r.kernel,control.process,control.identity)
    if name=='normal_parent_exit':
        (root/'finish').touch(exist_ok=False)
        if not w.wait(r.kernel,item.process,PROFILE['cleanup_timeout']*1000):raise ValueError('Parent did not exit normally')
        code=w.DWORD();w.checked(r.kernel.GetExitCodeProcess(item.process,c.byref(code)))
        if code.value!=0:raise ValueError('Normal parent exit was nonzero')
        handles=handles[1:];identities=identities[1:]
        if not handles:raise ValueError('Live child missing')
    if name=='abnormal_child_exit':
        (root/'abnormal_exit').touch(exist_ok=False)
        if len(handles)!=2 or not w.wait(r.kernel,handles[1],PROFILE['cleanup_timeout']*1000):raise ValueError('Abnormal exit missing')
        code=w.DWORD();w.checked(r.kernel.GetExitCodeProcess(handles[1],c.byref(code)))
        if code.value!=23:raise ValueError('Wrong abnormal code')
        handles=handles[:1];identities=identities[:1]
    check_timing(item.started,time.monotonic())
    for handle,identity in zip(handles,identities):alive(r.kernel,handle,identity)
    if name=='breakaway' and (record.get('breakaway_attempted') is not True or
            (not record.get('child') and record.get('breakaway_error')!=5)):
        raise ValueError('Breakaway attempt evidence missing')
    timeout_observation=observe_timeout(r.kernel,handles,identities) if name=='timeout' else None
    check_timing(item.started,time.monotonic())
    if w.census(r.kernel,item.job)!={i['pid'] for i in identities}:
        raise ValueError('Pre-trigger census changed')
    triggered=time.monotonic()
    if control:alive(r.kernel,control.process,control.identity)
    if name=='second_handle':triggered=second_handle(r,item,handles,identities)
    else:item.close_job()
    terminated(r.kernel,handles)
    if control:alive(r.kernel,control.process,control.identity)
    return {'name':name,'passed':True,'identities':identities,'trigger':'last_handle_close',
            'breakaway_attempted':name=='breakaway','breakaway_error':record.get('breakaway_error'),
            'pretrigger_census':record,'elapsed_at_trigger':triggered-item.started,
            'timeout_observation':timeout_observation,
            'wait_signaled_count':len(handles),'cleanup_confirmed':True,
            'earliest_natural_exit_seconds':PROFILE['watchdog'],
            'second_handle_ineffective_kill_observed':name=='second_handle',
            'run_classification':'expected_failed_run' if name=='abnormal_child_exit' else 'experiment_only'}


def observe_timeout(kernel,handles,identities):
    started=time.monotonic();deadline=started+TIMEOUT_SECONDS;polls=0
    while time.monotonic()<deadline:
        for handle,identity in zip(handles,identities):alive(kernel,handle,identity)
        if w.wait(kernel,handles[0],50):raise ValueError('Workload exited before timeout')
        polls+=1
    return {'deadline_seconds':TIMEOUT_SECONDS,'polls':polls,'trigger':'deadline_expired'}


def second_handle(r,item,handles,identities):
    current=r.kernel.GetCurrentProcess();duplicate=w.HANDLE()
    w.checked(r.kernel.DuplicateHandle(current,item.job,current,c.byref(duplicate),0,False,2))
    r.jobs.append(duplicate.value);item.close_job()
    # The sentinel must survive an actual bounded wait while the second handle exists.
    try:terminated(r.kernel,handles)
    except ValueError:pass
    else:raise ValueError('Ineffective kill incorrectly appeared successful')
    for handle,identity in zip(handles,identities):alive(r.kernel,handle,identity)
    triggered=time.monotonic();check_timing(item.started,triggered)
    w.close(r.kernel,duplicate.value);r.jobs.remove(duplicate.value)
    return triggered


def crash(name,root,r):
    before=name=='controller_before_resume'
    controller=r.start('controller_before' if before else 'controller_after',root)
    record=ready(root/'controller.json',controller.role)
    if record.get('identity')!=controller.identity:raise ValueError('Controller identity disagreement')
    controller_handle=o.retain(r.kernel,controller.identity,controller=True)
    r.retained.append(controller_handle)
    target=r.retain(record['target']);handles=[target];identities=[record['target']]
    duplicate=o.duplicate_job(r.kernel,controller_handle,record['job_handle']);r.jobs.append(duplicate)
    if not w.member(r.kernel,target,duplicate):raise ValueError('Suspended membership missing')
    if not before:
        (root/'resume_ack').touch(exist_ok=False)
        parent_record=ready(root/'parent.json','parent')
        if parent_record.get('identity')!=record['target']:raise ValueError('Target identity disagreement')
        identities.append(parent_record['child']);handles.append(r.retain(parent_record['child']))
    for handle,identity in zip(handles,identities):
        alive(r.kernel,handle,identity)
        if not w.member(r.kernel,handle,duplicate):raise ValueError('Crash member escaped')
    if w.census(r.kernel,duplicate)!={i['pid'] for i in identities}:raise ValueError('Crash census mismatch')
    r.complete_census=True
    w.close(r.kernel,duplicate);r.jobs.remove(duplicate)
    # Barrier is observer-owned: no target-job handles remain here before controller death.
    if r.jobs:raise ValueError('Observer job handle retained')
    alive(r.kernel,controller_handle,controller.identity)
    triggered=time.monotonic();check_timing(controller.started,triggered)
    w.checked(r.kernel.TerminateProcess(controller_handle,41))
    if not w.wait(r.kernel,controller_handle,PROFILE['cleanup_timeout']*1000):raise ValueError('Controller death unconfirmed')
    terminated(r.kernel,handles)
    return {'name':name,'passed':True,'identities':identities,'trigger':'controller_terminated',
            'observer_duplicate_closed':True,'resume_called':not before,'cleanup_confirmed':True,
            'elapsed_at_trigger':triggered-controller.started,'pretrigger_census':identities,
            'wait_signaled_count':len(handles),'compatible_enclosing_job_observed':True}


def run_scenario(name,root,journal):
    if name not in SCENARIOS:raise ValueError('Unknown scenario')
    r=Resources(journal);outcome=None;failure=None
    try:outcome=crash(name,root,r) if name.startswith('controller_') else ordinary(name,root,r)
    except BaseException as error:failure=type(error).__name__
    cleanup=r.close_all()
    if failure or not cleanup:raise ObservationFailure(failure or 'cleanup_uncertain',cleanup)
    return outcome
