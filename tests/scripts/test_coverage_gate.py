"""Exercise the public command line with small, independent evidence changes."""

import json
import os
import shutil
import subprocess
import sys
from pathlib import Path
from xml.etree import ElementTree

import pytest


GATE = Path(__file__).resolve().parents[2] / "scripts/ci/coverage_gate.py"
FIXTURES = Path(__file__).parent / "fixtures/coverage_gate"
SHA = "a" * 40


def run(*args, **kwargs):
    return subprocess.run([sys.executable, GATE, *args], **kwargs).returncode


CASES = [
    ("positive", None), ("dropped", "dropped node"), ("failure", "new failure"),
    ("skip", "skip reason changed"), ("lines", "lost covered lines"),
    ("empty", "empty baseline"), ("missing-file", "missing report file"),
    ("missing-env", "missing environment"), ("wrong-commit", "commit mismatch"),
    ("missing-module", "missing required module"), ("nightly", "pre-merge node moved to nightly"),
    ("collection", "collection error"), ("partial", "incomplete run"),
    ("hidden", "hidden baseline failure"), ("pass-to-skip", "passing node now skipped"),
    ("unmapped", "missing passing replacement"), ("mapped", None),
    ("missing-record", "missing commit record"), ("zero-byte", "empty baseline"),
    ("obligation", "obligation changed"), ("selection", "missing closure shard"),
    ("saving", None), ("failed-replacement", "missing passing replacement"),
    ("invalid", "invalid input"),
    ("dirty", "dirty worktree"), ("overlay", None),
    ("partial-overlay", "dirty worktree"), ("missing-blob", "dirty worktree"),
    ("missing-touched-modules", "invalid input"), ("removal-missing-touched-modules", "invalid input"),
    ("unchecked-closure", "pre-merge closure not checked"), ("daily", None),
    ("early-stop", "incomplete run"), ("early-stop-combined", "incomplete run"),
    ("early-stop-maxfail", "incomplete run"), ("replay", "incomplete run"), ("report-chars", None),
    ("missing-changed-paths", "dirty worktree"),
    ("teardown", None), ("duplicate", "invalid input"),
    ("windows-paths", None), ("separator-collision", "invalid input"),
]


@pytest.mark.parametrize(("case", "reason"), CASES, ids=[case for case, _ in CASES])
def test_compare(case, reason, tmp_path):
    for side in ("before", "after"):
        shutil.copytree(FIXTURES / "reference", tmp_path / side / "py")
    folder = FIXTURES / case
    if not (folder / "before.json").exists():
        shutil.copytree(FIXTURES / "defaults", tmp_path, dirs_exist_ok=True)
    shutil.copytree(folder, tmp_path, dirs_exist_ok=True)
    if (folder / "omit.json").exists():
        for path in json.loads((folder / "omit.json").read_text()):
            (tmp_path / path).unlink()
    kind = "removal" if (folder / "mapping.json").exists() else "consolidation"
    before, after = tmp_path / "before", tmp_path / "after"
    if (folder / "before.json").exists():
        kind, before, after = "selection", folder / "before.json", folder / "after.json"
    flags = []
    for name in ("required-envs", "required-modules", "touched-modules", "mapping",
                 "closure", "pr-path-nodes", "nightly-path-nodes"):
        manifest = tmp_path / f"{name}.json"
        if manifest.exists():
            flags.extend(["--" + name, manifest])
    report = tmp_path / "report.md"
    code = run("compare", "--before", before, "--after", after, "--kind", kind,
               "--commit", SHA, "--report", report, *flags)
    assert code == (2 if reason == "invalid input" else int(reason is not None))
    content = report.read_text()
    headline = content.splitlines()[0]
    assert headline.startswith(f"Coverage gate at {SHA[:8]}: ")
    if reason:
        assert ": FAIL (" in headline and reason in headline
    else:
        assert headline.endswith(": PASS")
    if case == "saving":
        assert "intended saving" in content and "large" in content
    if case == "overlay":
        assert "line check: no touched modules" in content
    if case.endswith("missing-touched-modules"):
        assert "--touched-modules" in content
    if case in ("unchecked-closure", "mapped"):
        assert "pre-merge closure: not checked (no closure supplied)" in content
    if case == "teardown":
        assert "| py | 2 | 2 | 0 | 1 | 1 |" in content
    if case == "windows-paths":
        assert "lib/core.py: 0" in content
    if case == "duplicate":
        assert "duplicate node" in content


def test_usage_error(tmp_path):
    report = tmp_path / "report.md"
    report.write_text(f"Coverage gate at {SHA[:8]}: PASS\n")
    assert run("compare", "--kind", "typo", "--report", report, "--commit", SHA) == 2
    assert report.read_text().splitlines()[0] == f"Coverage gate at {SHA[:8]}: FAIL (invalid usage)"


EARLY_STOP = [["-x"], ["-qx"], ["--exitfirst"], ["--maxfail=2"], ["--maxfail", "2"], ["--sw"],
              ["--stepwise-skip"], ["--lf"], ["--last-failed"]]


@pytest.mark.parametrize("addopts", [False, True], ids=["argument", "PYTEST_ADDOPTS"])
@pytest.mark.parametrize("option", EARLY_STOP, ids=[" ".join(o) for o in EARLY_STOP])
def test_collect_refuses_early_stop(option, addopts, tmp_path):
    env = dict(os.environ)
    env.pop("PYTEST_ADDOPTS", None)
    if addopts:
        env["PYTEST_ADDOPTS"] = " ".join(option)
        option = []
    result = subprocess.run([sys.executable, GATE, "collect", "--env", "py", "--out", "reports", "--",
                             "inputs", *option], cwd=tmp_path, env=env, capture_output=True, text=True)
    assert result.returncode == 2
    assert "early-stop or replay option" in result.stderr
    assert not (tmp_path / "reports").exists()


def test_collect(tmp_path):
    subprocess.run(["git", "clone", "-q", "--shared", "--no-checkout", "--single-branch",
                    "--no-tags", str(GATE.parents[2]), str(tmp_path)], check=True)
    subprocess.run(["git", "sparse-checkout", "set", "--no-cone", "/README.md"], cwd=tmp_path, check=True)
    subprocess.run(["git", "checkout", "-q", "HEAD"], cwd=tmp_path, check=True)
    (tmp_path / "README.md").write_text("overlay\n")
    (tmp_path / "inputs").mkdir()
    extra = "inputs/extra config"
    (tmp_path / extra).write_text("test input\n")
    (tmp_path / "inputs/test_pair.py").write_text("def test_one(): pass\ndef test_two(): pass\n")
    paths = ["README.md", extra, "inputs/test_pair.py"]
    assert run("collect", "--env", "py", "--out", "reports", "--overlay", *paths, "--", "inputs/test_pair.py", "-q",
               cwd=tmp_path) == 0
    evidence = tmp_path / "reports/py"
    record = json.loads((evidence / "env.json").read_text())
    head = subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=tmp_path, text=True).strip()
    assert record["commit"] == head and record["clean"] is False
    assert sorted(record["changed_paths"]) == sorted(paths)
    assert record["overlay"] == {p: subprocess.check_output(["git", "hash-object", "--", p],
                                                            cwd=tmp_path, text=True).strip() for p in paths}
    assert record["complete"] is True and record["pytest_exit_code"] == 0
    assert record["obligation"] == "pre-merge" and record["python_version"]
    assert "inputs/test_pair.py" in record["command"]
    assert len(ElementTree.parse(evidence / "junit.xml").findall(".//testcase")) == 2
    # coverage.py writes native separators; compare normalises them (windows-paths case).
    files = json.loads((evidence / "coverage.json").read_text())["files"]
    assert "inputs/test_pair.py" in {key.replace("\\", "/") for key in files}
