"""Windows subprocess acceptance harness tests; no native solver imports."""
import ctypes
import json
import os
from pathlib import Path
import subprocess
import sys
import time

import pytest

pytestmark = pytest.mark.skipif(os.name != "nt", reason="Windows harness")
HARNESS = Path(__file__).resolve().parents[3] / "docs/plans/evidence/issue-2082-native.ps1"
FIXTURE = Path(__file__).with_name("native_harness_child.py")


@pytest.fixture
def fake_repo(tmp_path):
    root = tmp_path / "repo with spaces"
    scripts = root / "scripts"
    scripts.mkdir(parents=True)
    (scripts / "solver_smoke_test.py").write_bytes(FIXTURE.read_bytes())
    source = root / "src/digitalmodel/solvers/smoke"
    source.mkdir(parents=True)
    for name in ("probes.py", "workflow.py"):
        (source / name).write_text("# fixture source\n")
    for args in (["init"], ["add", "."], ["-c", "user.name=Fixture", "-c",
                 "user.email=fixture@example.invalid", "commit", "-m", "fixture"]):
        subprocess.run(["git", *args], cwd=root, check=True, capture_output=True)
    return root


def invoke(root, output, mode, timeout=5):
    (root / "mode").write_text(mode)
    return subprocess.run(
        ["powershell", "-NoProfile", "-NonInteractive", "-ExecutionPolicy", "Bypass",
         "-File", str(HARNESS), "-Python", sys.executable, "-RepoRoot", str(root),
         "-OutputDirectory", str(output), "-TimeoutSeconds", str(timeout)],
        capture_output=True, text=True, timeout=25,
    )


def test_success_retains_artifacts_and_sanitizes_proof(fake_repo, tmp_path):
    output = tmp_path / "proof"
    run = invoke(fake_repo, output, "success")
    assert run.returncode == 0, run.stderr + run.stdout
    proof = json.loads((output / "proof.json").read_text())
    assert proof["ok"] is True
    assert proof["python_version"] and len(proof["revision"]) == 40
    assert set(proof["input_hashes"]) == {
        "scripts/solver_smoke_test.py", "src/digitalmodel/solvers/smoke/probes.py",
        "src/digitalmodel/solvers/smoke/workflow.py",
    }
    assert len(proof["output_hashes"]) == 2
    assert len(list(output.glob("scratch-*/orcaflex/smoke.sim"))) == 1
    assert str(fake_repo) not in (output / "proof.json").read_text()


@pytest.mark.parametrize("mode", ["failure", "badproof", "nonfinite"])
def test_failure_retains_diagnostics(fake_repo, tmp_path, mode):
    output = tmp_path / "proof"
    run = invoke(fake_repo, output, mode)
    assert run.returncode != 0
    proof = json.loads((output / "proof.json").read_text())
    assert proof["ok"] is False
    assert (output / "stdout.log").exists()
    assert (output / "stderr.log").exists()
    assert (output / "harness-error.log").read_text()


def process_alive(pid):
    kernel = ctypes.WinDLL("kernel32", use_last_error=True)
    kernel.OpenProcess.restype = ctypes.c_void_p
    kernel.WaitForSingleObject.argtypes = [ctypes.c_void_p, ctypes.c_ulong]
    kernel.CloseHandle.argtypes = [ctypes.c_void_p]
    handle = kernel.OpenProcess(0x100000, False, pid)
    if not handle:
        return False
    try:
        return kernel.WaitForSingleObject(handle, 0) == 258
    finally:
        kernel.CloseHandle(handle)


def test_timeout_kills_owned_descendant_only(fake_repo, tmp_path):
    outsider = subprocess.Popen([sys.executable, "-c", "import time;time.sleep(30)"])
    try:
        output = tmp_path / "proof"
        run = invoke(fake_repo, output, "timeout", timeout=2)
        assert run.returncode != 0
        proof = json.loads((output / "proof.json").read_text())
        assert proof["timed_out"] is True
        pid = int((fake_repo / "descendant.pid").read_text())
        deadline = time.monotonic() + 3
        while process_alive(pid) and time.monotonic() < deadline:
            time.sleep(0.05)
        assert not process_alive(pid)
        assert outsider.poll() is None
    finally:
        outsider.terminate()
        outsider.wait(timeout=5)


def test_existing_output_is_rejected(fake_repo, tmp_path):
    output = tmp_path / "proof"
    output.mkdir()
    marker = output / "keep.txt"
    marker.write_text("untouched")
    assert invoke(fake_repo, output, "success").returncode != 0
    assert marker.read_text() == "untouched"


@pytest.mark.parametrize("name", ["probes.py", "workflow.py"])
def test_missing_proof_input_refuses_before_child_launch(fake_repo, tmp_path, name):
    (fake_repo / "src/digitalmodel/solvers/smoke" / name).unlink()
    output = tmp_path / "proof"
    assert invoke(fake_repo, output, "success").returncode != 0
    proof = json.loads((output / "proof.json").read_text())
    assert proof["ok"] is False
    assert not (output / "stdout.log").exists()
