"""Finite original Python sentinels. No network, vendor imports or arbitrary commands."""
import os
from pathlib import Path
import subprocess
import sys
import threading
import time


def watchdog():
    time.sleep(PROFILE['watchdog']);os._exit(97)


def await_file(path,seconds=None):
    end=time.monotonic()+(PROFILE['readiness_timeout'] if seconds is None else seconds)
    while time.monotonic()<end:
        if path.exists():return
        time.sleep(0.01)
    raise RuntimeError('Sentinel barrier missing')


def own_identity():
    from digitalmodel_ghs_sentinel import _windows_job as w
    kernel=w.kernel_api()
    return w.identity(kernel,kernel.GetCurrentProcess())


def fixed_child(role,root,breakaway=False):
    if role not in {'child','abnormal_child'}:raise ValueError('Unknown child role')
    from digitalmodel_ghs_sentinel._owned_process import trusted_interpreter
    argv=[str(trusted_interpreter()),'-I','-B',str(Path(__file__).resolve()),role,str(root)]
    flags=subprocess.CREATE_NO_WINDOW|(0x01000000 if breakaway else 0)
    return subprocess.Popen(argv,stdin=subprocess.DEVNULL,stdout=subprocess.DEVNULL,
                            stderr=subprocess.DEVNULL,close_fds=True,creationflags=flags)


def parent(role,root):
    from digitalmodel_ghs_sentinel._qualification_state import publish,read_record
    child_role='abnormal_child' if role=='abnormal' else 'child'
    try:
        child=fixed_child(child_role,root,breakaway=role=='breakaway')
    except OSError as error:
        if role!='breakaway':raise
        publish(root/'parent.json',{'role':role,'identity':own_identity(),
                'breakaway_attempted':True,'breakaway_error':error.winerror,'child':None})
        await_file(root/'finish',PROFILE['watchdog']);return
    await_file(root/'child.json')
    record=read_record(root/'child.json')
    if record.get('role')!=child_role:raise ValueError('Child readiness role mismatch')
    publish(root/'parent.json',{'role':role,'identity':own_identity(),'child':record['identity'],
            'breakaway_attempted':role=='breakaway','breakaway_error':None})
    await_file(root/'finish',PROFILE['watchdog'])


def controller(role,root):
    from digitalmodel_ghs_sentinel._owned_process import launch
    from digitalmodel_ghs_sentinel._qualification_state import publish
    target=launch('parent',root,suspended=True)
    publish(root/'controller.json',{'role':role,'identity':own_identity(),
            'target':target.identity,'job_handle':target.job,'started':target.started})
    if role=='controller_after':
        await_file(root/'resume_ack');target.resume()
    await_file(root/'never_written',PROFILE['watchdog'])


def main():
    if len(sys.argv)!=3:raise ValueError('Fixed role and root required')
    role,root=sys.argv[1],Path(sys.argv[2])
    from digitalmodel_ghs_sentinel._qualification_state import ROLES,publish,no_reparse
    if role not in ROLES or not no_reparse(root).is_dir():raise ValueError('Invalid sentinel input')
    if role in {'controller_before','controller_after'}:return controller(role,root)
    if role in {'parent','abnormal','breakaway'}:return parent(role,root)
    publish(root/('control.json' if role=='control' else 'child.json'),
            {'role':role,'identity':own_identity()})
    if role=='abnormal_child':
        await_file(root/'abnormal_exit');return 23
    await_file(root/'never_written',PROFILE['watchdog'])


if __name__=='__main__':
    # Load the fixed stdlib-only helper package, not the solver registry or scientific stack.
    import importlib.util
    directory=Path(__file__).resolve().parent
    spec=importlib.util.spec_from_file_location('digitalmodel_ghs_sentinel',directory/'__init__.py',
                                               submodule_search_locations=[str(directory)])
    package=importlib.util.module_from_spec(spec)
    sys.modules[spec.name]=package;spec.loader.exec_module(package)
    from digitalmodel_ghs_sentinel._qualification_contract import PROFILE
    threading.Thread(target=watchdog,daemon=True).start()
    sys.exit(main() or 0)
