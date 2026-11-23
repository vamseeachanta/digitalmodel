"""Audited-model harness tests use fake processes, never OrcFxAPI."""
import json
import os
from pathlib import Path
import subprocess
import sys
from types import SimpleNamespace

import pytest

pytestmark = pytest.mark.skipif(os.name != "nt", reason="Windows harness")
HARNESS = Path(__file__).resolve().parents[3] / "docs/plans/evidence/issue-2082-native.ps1"


@pytest.fixture
def fake_repo(tmp_path):
    root = tmp_path / "model repository with spaces"
    (root / "scripts").mkdir(parents=True)
    (root / "src/digitalmodel/solvers/smoke").mkdir(parents=True)
    (root / "fixture.txt").write_text("fake repository")
    for args in (["init"], ["add", "."], ["-c", "user.name=Fixture", "-c",
                 "user.email=fixture@example.invalid", "commit", "-m", "fixture"]):
        subprocess.run(["git", *args], cwd=root, check=True, capture_output=True)
    return root


def worker():
    text = HARNESS.read_text().split("$worker = @'", 1)[1].split("\n'@", 1)[0]
    namespace = {}
    exec(text.rsplit("sys.exit(main())", 1)[0], namespace)
    return namespace


@pytest.mark.parametrize("mode", ["success", "badproof", "failure", "mutate",
                                  "wrongphase", "missing_sim", "simhash", "incomplete", "inputhash"])
def test_model_mode_sequences_only_valid_unchanged_solve(fake_repo, tmp_path, mode):
    fixture = Path(__file__).with_name("native_model_harness_child.py")
    (fake_repo / "scripts/orcaflex_native_model_probe.py").write_bytes(fixture.read_bytes())
    (fake_repo / "src/digitalmodel/solvers/smoke/model_probe.py").write_text("# fake\n")
    (fake_repo / "src/digitalmodel/solvers/smoke/model_manifest.py").write_text("# fake\n")
    (fake_repo / "src/digitalmodel/solvers/smoke/model_data_readback.py").write_text("# fake\n")
    helper = HARNESS.parents[3] / "src/digitalmodel/solvers/smoke/model_proof.py"
    (fake_repo / "src/digitalmodel/solvers/smoke/model_proof.py").write_bytes(helper.read_bytes())
    contract = fake_repo / "docs/benchmarks/mooring_buoy/qualification.yml"
    contract.parent.mkdir(parents=True)
    contract.write_text("# fake contract\n")
    manifest = fake_repo / "manifest.json"
    manifest.write_text(json.dumps({"mode": mode}))
    output = tmp_path / "proof"
    run = subprocess.run(
        ["powershell", "-NoProfile", "-NonInteractive", "-ExecutionPolicy", "Bypass",
         "-File", str(HARNESS), "-Python", sys.executable, "-RepoRoot", str(fake_repo),
         "-OutputDirectory", str(output), "-Mode", "Model", "-Manifest", str(manifest),
         "-TimeoutSeconds", "120"], capture_output=True, text=True, timeout=35,
    )
    proof = json.loads((output / "proof.json").read_text())
    assert (run.returncode == 0) is (mode == "success"), run.stderr
    assert proof["ok"] is (mode == "success")
    assert proof["pythonhashseed"] == "0"
    phases = (fake_repo / "phases").read_text().splitlines()
    assert phases == (["solve", "readback"] if mode == "success" else ["solve"])
    assert str(fake_repo) not in (output / "proof.json").read_text()
    assert "src/digitalmodel/solvers/smoke/model_data_readback.py" in proof["input_hashes"]
    assert "docs/benchmarks/mooring_buoy/qualification.yml" in proof["input_hashes"]


@pytest.mark.parametrize("relative", [
    "src/digitalmodel/solvers/smoke/model_data_readback.py",
    "docs/benchmarks/mooring_buoy/qualification.yml",
])
def test_model_binding_requires_new_readback_and_case_contract(fake_repo, relative):
    ns = worker()
    sources = ns["model_sources"](fake_repo)
    assert fake_repo / relative in sources


@pytest.mark.parametrize("unknown", [False, True])
def test_cleanup_observes_job_before_closing_even_after_root_exit(unknown):
    ns = worker()
    events = []
    counts = iter([1, 0])
    def active(kernel, job):
        events.append("query")
        if unknown:
            raise RuntimeError("query failed")
        return next(counts)
    ns["active_processes"] = active
    ns["_winapi"] = SimpleNamespace(
        WaitForSingleObject=lambda *args: 0,
        CloseHandle=lambda h: events.append("close-" + h),
    )
    kernel = SimpleNamespace(
        TerminateJobObject=lambda *args: events.append("terminate") or True,
        CloseHandle=lambda h: events.append("close-" + h) or True,
    )
    if unknown:
        with pytest.raises(RuntimeError, match="cleanup|query"):
            ns["cleanup_owned"](kernel, "job", "process", "thread")
    else:
        ns["cleanup_owned"](kernel, "job", "process", "thread")
        assert events.index("terminate") < events.index("close-job")
        assert events.count("query") >= 2
    assert events[-3:] == ["close-job", "close-process", "close-thread"]


def test_query_failure_still_requests_unassigned_root_termination():
    ns = worker()
    events = []
    ns["active_processes"] = lambda *args: (_ for _ in ()).throw(RuntimeError("query"))
    ns["_winapi"] = SimpleNamespace(
        WaitForSingleObject=lambda *args: 258,
        TerminateProcess=lambda *args: events.append("terminate-root"),
        CloseHandle=lambda h: events.append("close-" + h),
    )
    kernel = SimpleNamespace(CloseHandle=lambda h: events.append("close-" + h) or True)
    with pytest.raises(RuntimeError, match="query"):
        ns["cleanup_owned"](kernel, "job", "process", "thread")
    assert events.index("terminate-root") < events.index("close-process")
