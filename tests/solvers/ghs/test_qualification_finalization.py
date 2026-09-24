# Regression checks for completion ordering and effective kill deadlines.
from types import SimpleNamespace
import ctypes as c
import pytest
from digitalmodel.solvers.ghs import qualification as q
from digitalmodel.solvers.ghs import _qualification_scenarios as s


def test_failed_marker_finalization_never_publishes_success(monkeypatch, tmp_path):
    class Attempt:
        def reserve(self): pass
        def stage(self, *args, **kwargs): pass
        def finish(self, *args, **kwargs): raise OSError("finalization failed")
    monkeypatch.setattr(q, "os", SimpleNamespace(name="nt"))
    monkeypatch.setattr(q.w, "kernel_api", lambda: SimpleNamespace(GetCurrentProcess=lambda: 1))
    monkeypatch.setattr(q.w, "member", lambda *args: False)
    monkeypatch.setattr(q, "runtime_identity", lambda: {})
    monkeypatch.setattr(q.storage, "Attempt", Attempt)
    monkeypatch.setattr(q, "run_scenario", lambda name, *args: {
        "name": name, "passed": True, "cleanup_confirmed": True})
    root = tmp_path / "observation"
    with pytest.raises(OSError, match="finalization failed"):
        q.qualify(root)
    assert (root / "private-observations.json").exists()
    assert not (root / "public-summary.json").exists()


def test_second_handle_rechecks_budget_before_effective_kill(monkeypatch):
    def duplicate(*args):
        c.cast(args[3], c.POINTER(s.w.HANDLE)).contents.value = 7
        return 1
    closed = []
    kernel = SimpleNamespace(GetCurrentProcess=lambda: 1, DuplicateHandle=duplicate)
    resources = SimpleNamespace(kernel=kernel, jobs=[])
    item = SimpleNamespace(job=5, started=100, close_job=lambda: None)
    monkeypatch.setattr(s, "terminated", lambda *args: (_ for _ in ()).throw(ValueError()))
    monkeypatch.setattr(s, "alive", lambda *args: None)
    monkeypatch.setattr(s.time, "monotonic", lambda: 111)
    monkeypatch.setattr(s.w, "close", lambda *args: closed.append(args[1]))
    with pytest.raises(ValueError, match="watchdog margin"):
        s.second_handle(resources, item, [2], [{"pid": 2}])
    assert resources.jobs == [7]
    assert closed == []
