"""Binary capture/operator metadata using fake supervisors only."""
import subprocess

import pytest

from digitalmodel.ansys.cylinder_runner import launch_case
from digitalmodel.ansys.cylinder_runner import BinaryANSYSRunner, settle_retained
from digitalmodel.ansys.runner import ANSYSRunner


class FakeSupervisor:
    containment_verified = True

    def __init__(self, timeout=False, leftover=False, unknown=False, start_error=False):
        self.timeout, self.leftover = timeout, leftover
        self.unknown, self.start_error = unknown, start_error
        self.started = self.closed = self.terminated = False
        self.pid = 123

    def start(self, argv, cwd, stdout, stderr):
        self.started = True
        self.argv = argv
        stdout.write(b"raw\r\n\xff")
        stderr.write(b"\x80")
        if self.start_error:
            raise OSError("containment failed")

    def wait(self, timeout):
        if self.timeout:
            raise subprocess.TimeoutExpired("synthetic", timeout)
        return 0

    def active(self):
        if self.unknown:
            raise OSError("query failed")
        return int(self.leftover and not self.terminated)

    def terminate(self):
        self.terminated = True

    def close(self):
        self.closed = True


def setup_case(tmp_path):
    exe = tmp_path / "solver.exe"
    exe.write_bytes(b"synthetic; never executed")
    directory = tmp_path / "attempt"
    directory.mkdir()
    (directory / "case.inp").write_text("synthetic deck", encoding="utf-8")
    return exe, directory


def test_original_bytes_and_dedicated_files(tmp_path):
    exe, directory = setup_case(tmp_path)
    fake = FakeSupervisor()
    result = launch_case({"deck_basename": "case.inp"}, directory, 3, exe,
                         supervisor_factory=lambda: fake)
    assert result["stdout"] == (directory / "stdout.bin").read_bytes() == b"raw\r\n\xff"
    assert result["stderr"] == b"\x80"
    assert result["return_code"] == 0 and result["owned_processes_remaining"] == 0
    assert fake.closed
    assert fake.argv[-3:] == ["-np", "1", "-smp"]


def test_capture_file_collision_never_launches(tmp_path):
    exe, directory = setup_case(tmp_path)
    (directory / "stderr.bin").write_bytes(b"preserve")
    fake = FakeSupervisor()
    with pytest.raises(FileExistsError):
        launch_case({"deck_basename": "case.inp"}, directory, 3, exe,
                    supervisor_factory=lambda: fake)
    assert not fake.started
    assert (directory / "stderr.bin").read_bytes() == b"preserve"


@pytest.mark.parametrize("kwargs", [{"timeout": True}, {"leftover": True},
                                   {"start_error": True}, {"unknown": True}])
def test_timeout_leftover_or_uncertainty_stops(tmp_path, kwargs):
    exe, directory = setup_case(tmp_path)
    fake = FakeSupervisor(**kwargs)
    result = launch_case({"deck_basename": "case.inp"}, directory, 3, exe,
                         supervisor_factory=lambda: fake)
    assert not result["evidence_complete"]
    assert fake.terminated
    assert result["stdout"] == b"raw\r\n\xff"
    if kwargs.get("unknown"):
        assert result["owned_processes_remaining"] is None
        assert result["retained_supervisor_token"]
        assert not fake.closed


@pytest.mark.parametrize("name", ["../case.inp", "sub/case.inp", "case.inp.", "C:case.inp"])
def test_deck_path_cannot_escape_attempt(tmp_path, name):
    exe, directory = setup_case(tmp_path)
    with pytest.raises(ValueError):
        launch_case({"deck_basename": name}, directory, 3, exe)


def test_default_runner_methods_are_unchanged():
    assert BinaryANSYSRunner.run is ANSYSRunner.run
    assert BinaryANSYSRunner.execute is ANSYSRunner.execute


def test_unknown_settlement_can_be_retried_by_owned_token(tmp_path):
    exe, directory = setup_case(tmp_path)
    fake = FakeSupervisor(unknown=True)
    result = launch_case({"deck": "case.inp"}, directory, 3, exe,
                         supervisor_factory=lambda: fake)
    fake.unknown = False
    settled = settle_retained(result["retained_supervisor_token"])
    assert settled["owned_processes_remaining"] == 0 and fake.closed


def test_unqualified_platform_never_reports_complete(tmp_path):
    exe, directory = setup_case(tmp_path)
    fake = FakeSupervisor()
    fake.containment_verified = False
    result = launch_case({"deck": "case.inp"}, directory, 3, exe,
                         supervisor_factory=lambda: fake)
    assert not result["evidence_complete"]


def test_stream_read_error_is_reported_not_raised_after_launch(tmp_path, monkeypatch):
    from pathlib import Path
    exe, directory = setup_case(tmp_path)
    fake = FakeSupervisor()
    monkeypatch.setattr(Path, "read_bytes", lambda self: (_ for _ in ()).throw(OSError("read denied")))
    result = launch_case({"deck": "case.inp"}, directory, 3, exe,
                         supervisor_factory=lambda: fake)
    assert not result["evidence_complete"]
    assert result["stdout"] is None and result["stderr"] is None
    assert result["error"] and fake.closed
