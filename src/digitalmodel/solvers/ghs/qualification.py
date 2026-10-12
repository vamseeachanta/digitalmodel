"""Operator-invoked harmless Windows containment observations, never launch authority."""
import hashlib
import json
import os
from pathlib import Path
import platform
import shutil
import sys
from datetime import datetime,timezone

from . import _windows_job as w
from . import _owned_process as owned
from . import _qualification_state as storage
from ._qualification_scenarios import SCENARIOS,ObservationFailure,check_timing,run_scenario

from ._qualification_contract import PROFILE,TIMEOUT_SECONDS


def validate_profile(value):
    if value is None:return dict(PROFILE)
    if type(value) is not dict or len(value)!=len(PROFILE) or set(value)!=set(PROFILE):
        raise ValueError('Closed profile required')
    for key,expected in PROFILE.items():
        if type(value[key]) is not int or value[key]!=expected:raise ValueError('Fixed finite profile required')
    return dict(PROFILE)


def runtime_identity():
    result=owned.source_identity()
    result.update(python_version=sys.version,windows_build=platform.version(),
                  architecture=platform.machine(),profile=dict(PROFILE),machine=platform.node(),
                  account=account_name())
    result['helper_hashes']={p.name:hashlib.sha256(p.read_bytes()).hexdigest()
        for p in Path(__file__).parent.glob('*.py')}
    return result


def account_name():
    import ctypes as c
    api=c.WinDLL('advapi32',use_last_error=True)
    fn=api.GetUserNameW;fn.argtypes=[c.c_wchar_p,c.POINTER(w.DWORD)];fn.restype=w.BOOL
    buffer=c.create_unicode_buffer(256);size=w.DWORD(256)
    w.checked(fn(buffer,c.byref(size)))
    return buffer.value


def public_summary(private):
    scenarios=private.get('scenarios',[])
    valid=(type(scenarios) is list and len(scenarios)==len(SCENARIOS)
           and all(type(item) is dict for item in scenarios)
           and {i.get('name') for i in scenarios}==set(SCENARIOS)
           and all(i.get('passed') is True and i.get('cleanup_confirmed') is True for i in scenarios))
    passed=valid and private.get('state')=='sentinel_containment_passed'
    return {'schema_version':1,'state':'sentinel_containment_passed' if passed else ('recovery_required' if private.get('state')=='recovery_required' else 'sentinel_containment_failed'),
            'ghs_launch_allowed':False,'licensed_execution_verified':False,
            'private_evidence_sha256':hashlib.sha256(storage.encoded(private)).hexdigest(),
            'scenarios':[{'name':i['name'],'passed':i.get('passed') is True}
                         for i in scenarios if type(i) is dict and i.get('name') in SCENARIOS],
            'limitations':['Observed harmless sentinels only; no GHS, license, receipt or storage qualification.',
                           'Evidence is not authorization; changed runtime or context requires a new observation.']}


def preserve_failed_records(directory):
    records={}
    for sub in (directory,directory/'control'):
        for name in ('parent.json','child.json','controller.json','control.json'):
            path=sub/name
            if path.exists():records[str(path.relative_to(directory))]=storage.read_record(path)
    storage.publish(directory.parent/'failed-scenario-records.json',records)
    storage.no_reparse(directory);shutil.rmtree(directory)
    return records


def qualify(output_root,profile=None):
    validate_profile(profile)
    if os.name!='nt':raise RuntimeError('Native Windows qualification required')
    kernel=w.kernel_api();runtime=runtime_identity()
    root=storage.no_reparse(Path(output_root).absolute())
    if root.exists():raise FileExistsError(root)
    private={'schema_version':1,'runtime':runtime,'utc_start':datetime.now(timezone.utc).isoformat(),
             'scenarios':[],'enclosing_job':w.member(kernel,kernel.GetCurrentProcess(),None),
             'ghs_launch_allowed':False,'licensed_execution_verified':False}
    root.mkdir(parents=False)
    try:
        attempt=storage.Attempt();attempt.reserve()
    except BaseException:
        root.rmdir();raise
    clean=False
    try:
        for name in SCENARIOS:
            directory=root/name;directory.mkdir()
            private['scenarios'].append(run_scenario(name,directory,attempt.stage))
            shutil.rmtree(directory)
        if runtime_identity()!=runtime:raise ValueError('Runtime identity changed during observation')
        private['state']='sentinel_containment_passed';clean=True
    except ObservationFailure as error:
        private['state']='sentinel_containment_failed';private['failure']=str(error)
        clean=error.cleanup_confirmed
        if clean:
            try:private['failed_records']=preserve_failed_records(directory)
            except (OSError,ValueError):clean=False
    except (OSError,ValueError,RuntimeError):
        private['state']='recovery_required'
    if not clean:private['state']='recovery_required'
    private['utc_end']=datetime.now(timezone.utc).isoformat()
    storage.publish(root/'private-observations.json',private)
    public=public_summary(private)
    if clean:
        attempt.stage('cleanup_confirmed')
        attempt.finish(public,cleanup_confirmed=True)
    storage.publish(root/'public-summary.json',public)
    return public
