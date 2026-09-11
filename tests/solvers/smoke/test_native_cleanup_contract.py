"""Refuse unconfirmed child cleanup; exercise handle ownership without a solver."""

import os
from pathlib import Path
from types import SimpleNamespace

import pytest


pytestmark = pytest.mark.skipif(os.name != "nt", reason="Windows process API")


@pytest.mark.parametrize("cleanup_wait", [258, 0xffffffff])
def test_unconfirmed_cleanup_cannot_return_success(tmp_path, cleanup_wait):
    path = Path(__file__).resolve().parents[3] / "docs/plans/evidence/issue-2082-native.ps1"
    source = path.read_text().split("$worker = @'", 1)[1].split("\n'@", 1)[0]
    namespace = {}
    exec(source.rsplit("sys.exit(main())", 1)[0], namespace)
    waits = iter([0, 0, cleanup_wait])
    closed = []
    kernel = SimpleNamespace(
        AssignProcessToJobObject=lambda *args: True,
        ResumeThread=lambda *args: 1,
        CloseHandle=lambda handle: closed.append(handle) or True,
    )
    namespace["kernel_api"] = lambda: kernel
    namespace["job_object"] = lambda _: "job"
    namespace["_winapi"] = SimpleNamespace(
        CreateProcess=lambda *args: ("process", "thread", 1, 2),
        WaitForSingleObject=lambda *args: next(waits),
        GetExitCodeProcess=lambda *args: 0,
        CloseHandle=lambda handle: closed.append(handle),
    )
    with pytest.raises(RuntimeError, match="termination"):
        namespace["run_owned"](["fixture"], tmp_path, tmp_path, 1)
    assert closed == ["job", "process", "thread"]
