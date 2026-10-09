"""Owned process supervisors; Windows Job Object or unqualified POSIX group.

No process search by executable name and no broad process termination. POSIX
keeps the unreaped leader to prevent process-group ID reuse during settlement;
session-escaping descendants are not contained, so that route is not B2-qualified.
"""
import os
import signal
import subprocess
import time

import psutil

from digitalmodel.ansys.cylinder_process_windows import WindowsJob


def _resume(pid):
    psutil.Process(pid).resume()


class WindowsSupervisor:
    containment_verified = True

    def __init__(self, job_factory=WindowsJob, popen=subprocess.Popen, resume=_resume):
        self.job = job_factory()
        self.popen, self.resume = popen, resume
        self.process = None
        self.assigned = False
        self.pid = None

    def start(self, argv, cwd, stdout, stderr):
        self.process = self.popen(argv, cwd=str(cwd), stdout=stdout, stderr=stderr,
                                  stdin=subprocess.DEVNULL, close_fds=True,
                                  creationflags=0x08000004)  # NO_WINDOW | SUSPENDED
        self.pid = self.process.pid
        try:
            self.job.assign(self.process._handle)
            self.assigned = True
            self.resume(self.pid)
        except BaseException as original:
            try:
                self.terminate()
            except Exception as cleanup:
                raise OSError(f"{type(original).__name__}: {original}; "
                              f"cleanup {type(cleanup).__name__}: {cleanup}") from original
            raise

    def wait(self, timeout):
        return self.process.wait(timeout=timeout)

    def active(self):
        if self.process is not None and not self.assigned:
            return int(self.process.poll() is None)
        return self.job.active()

    def terminate(self):
        if self.assigned:
            self.job.terminate()
        elif self.process is not None:
            self.process.kill()  # Popen uses the owned Windows handle, not PID search

    def close(self):
        if self.process is not None:
            self.process.wait(timeout=5)
        self.job.close()


class PosixSupervisor:
    containment_verified = False

    def __init__(self, popen=subprocess.Popen, waitid=getattr(os, "waitid", None)):
        self.popen, self.waitid = popen, waitid
        self.process = None
        self.pid = None
        self.exited = False

    def start(self, argv, cwd, stdout, stderr):
        if self.waitid is None or not hasattr(os, "WNOWAIT"):
            raise OSError("unreaped-leader supervision unavailable; no launch")
        self.process = self.popen(argv, cwd=str(cwd), stdout=stdout, stderr=stderr,
                                  stdin=subprocess.DEVNULL, start_new_session=True,
                                  close_fds=True)
        self.pid = self.process.pid

    def wait(self, timeout):
        deadline = time.monotonic() + timeout
        while True:
            state = self.waitid(os.P_PID, self.pid, os.WEXITED | os.WNOHANG | os.WNOWAIT)
            if state is not None:
                self.exited = True
                return state.si_status if state.si_code == os.CLD_EXITED else -state.si_status
            if time.monotonic() >= deadline:
                raise subprocess.TimeoutExpired("owned process group", timeout)
            time.sleep(0.01)

    def active(self):
        if self.pid is None:
            return 0
        count = 0
        for process in psutil.process_iter(["pid", "status"]):
            try:
                if os.getpgid(process.pid) == self.pid and process.status() != psutil.STATUS_ZOMBIE:
                    count += 1
            except (ProcessLookupError, psutil.NoSuchProcess):
                continue
        return count

    def terminate(self):
        if self.pid is not None:
            os.killpg(self.pid, signal.SIGKILL)  # leader has not been reaped

    def close(self):
        if self.process is not None:
            self.process.wait(timeout=5)  # only now release the leader PID


def supervisor_factory():
    return WindowsSupervisor() if os.name == "nt" else PosixSupervisor()
