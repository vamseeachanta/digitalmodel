"""Owned supervisor sequencing without starting any native process."""
import io
import ctypes
import subprocess
from unittest.mock import Mock
from types import SimpleNamespace

import pytest

from digitalmodel.ansys.cylinder_process import WindowsSupervisor, PosixSupervisor
from digitalmodel.ansys.cylinder_process_windows import WindowsJob, ExtendedLimit, Accounting


def test_windows_assignment_precedes_resume_and_is_suspended():
    events = []
    job = Mock()
    job.assign.side_effect = lambda handle: events.append("assign")
    proc = Mock(pid=42, _handle=99)
    popen = Mock(return_value=proc)
    resumer = Mock(side_effect=lambda pid: events.append("resume"))
    supervisor = WindowsSupervisor(job_factory=lambda: job, popen=popen, resume=resumer)
    supervisor.start(["synthetic"], ".", io.BytesIO(), io.BytesIO())
    assert events == ["assign", "resume"]
    assert popen.call_args.kwargs["creationflags"] == 0x08000004
    assert "env" not in popen.call_args.kwargs  # subprocess inherits the bound parent environment.
    job.assign.assert_called_once_with(99)
    supervisor.terminate()
    job.terminate.assert_called_once()


def test_failed_assignment_never_resumes_and_kills_owned_handle():
    job = Mock()
    job.assign.side_effect = OSError("assignment denied")
    proc = Mock(pid=42, _handle=99)
    resume = Mock()
    supervisor = WindowsSupervisor(job_factory=lambda: job,
                                   popen=Mock(return_value=proc), resume=resume)
    with pytest.raises(OSError):
        supervisor.start(["synthetic"], ".", io.BytesIO(), io.BytesIO())
    resume.assert_not_called()
    proc.kill.assert_called_once()


def test_windows_active_count_not_leader_exit_controls_settlement():
    job = Mock()
    job.active.return_value = 2
    supervisor = WindowsSupervisor(job_factory=lambda: job)
    assert supervisor.active() == 2


def test_posix_unsupported_waitid_refuses_before_popen():
    popen = Mock()
    supervisor = PosixSupervisor(popen=popen, waitid=None)
    with pytest.raises(OSError):
        supervisor.start(["synthetic"], ".", io.BytesIO(), io.BytesIO())
    popen.assert_not_called()


def test_job_layout_flags_and_active_query():
    api = Mock()
    api.CreateJobObjectW.return_value = 99
    flags = []
    def configure(handle, kind, pointer, size):
        assert handle == 99 and kind == 9 and size == ctypes.sizeof(ExtendedLimit)
        flags.append(ctypes.cast(pointer, ctypes.POINTER(ExtendedLimit)).contents.basic.flags)
        return 1
    def query(handle, kind, pointer, size, returned):
        assert handle == 99 and kind == 1 and size == 48
        ctypes.cast(pointer, ctypes.POINTER(Accounting)).contents.active = 3
        return 1
    api.SetInformationJobObject.side_effect = configure
    api.QueryInformationJobObject.side_effect = query
    job = WindowsJob(api=api)
    assert flags == [0x2000]  # no breakaway or silent-breakaway
    assert ctypes.sizeof(ExtendedLimit) == (144 if ctypes.sizeof(ctypes.c_void_p) == 8 else 112)
    assert job.active() == 3
    job.close()
    api.CloseHandle.assert_called_once_with(99)


def test_job_setup_failure_closes_handle_without_launch():
    api = Mock()
    api.CreateJobObjectW.return_value = 99
    api.SetInformationJobObject.return_value = 0
    with pytest.raises(OSError, match="SetInformationJobObject"):
        WindowsJob(api=api)
    api.CloseHandle.assert_called_once_with(99)


def test_job_query_failure_is_unknown_not_zero():
    api = Mock()
    api.QueryInformationJobObject.return_value = 0
    job = WindowsJob(api=api)
    with pytest.raises(OSError, match="QueryInformationJobObject"):
        job.active()


def test_windows_resume_failure_terminates_assigned_job():
    job = Mock()
    proc = Mock(pid=42, _handle=99)
    supervisor = WindowsSupervisor(job_factory=lambda: job, popen=Mock(return_value=proc),
                                   resume=Mock(side_effect=OSError("resume failed")))
    with pytest.raises(OSError, match="resume failed"):
        supervisor.start(["synthetic"], ".", io.BytesIO(), io.BytesIO())
    job.terminate.assert_called_once()
    proc.kill.assert_not_called()


def test_start_and_cleanup_errors_are_both_retained():
    job = Mock()
    job.assign.side_effect = OSError("assignment denied")
    proc = Mock(pid=42, _handle=99)
    proc.kill.side_effect = OSError("termination denied")
    supervisor = WindowsSupervisor(job_factory=lambda: job, popen=Mock(return_value=proc))
    with pytest.raises(OSError, match="assignment denied.*termination denied"):
        supervisor.start(["synthetic"], ".", io.BytesIO(), io.BytesIO())


def test_posix_leader_is_not_reaped_before_group_termination(monkeypatch):
    from digitalmodel.ansys import cylinder_process as module
    for name, value in {"P_PID": 1, "WEXITED": 4, "WNOHANG": 1,
                        "WNOWAIT": 16, "CLD_EXITED": 1}.items():
        monkeypatch.setattr(module.os, name, value, raising=False)
    monkeypatch.setattr(module.signal, "SIGKILL", 9, raising=False)
    killpg = Mock()
    monkeypatch.setattr(module.os, "killpg", killpg, raising=False)
    waitid = Mock(return_value=SimpleNamespace(si_status=0, si_code=1))
    proc = Mock(pid=42)
    popen = Mock(return_value=proc)
    supervisor = PosixSupervisor(popen=popen, waitid=waitid)
    supervisor.start(["synthetic"], ".", io.BytesIO(), io.BytesIO())
    assert popen.call_args.kwargs["start_new_session"] is True
    assert supervisor.wait(1) == 0
    proc.wait.assert_not_called()
    proc.poll.assert_not_called()
    assert waitid.call_args.args[-1] & 16
    supervisor.terminate()
    killpg.assert_called_once_with(42, 9)
    supervisor.close()
    proc.wait.assert_called_once()
