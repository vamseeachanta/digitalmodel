"""Exercise the public command line with small, independent evidence changes."""

import json
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
]


@pytest.mark.parametrize(("case", "reason"), CASES, ids=[case for case, _ in CASES])
def test_compare(case, reason, tmp_path):
    for side in ("before", "after"):
        shutil.copytree(FIXTURES / "reference", tmp_path / side / "py")
    folder = FIXTURES / case
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
        if (folder / f"{name}.json").exists():
            flags.extend(["--" + name, folder / f"{name}.json"])
    report = tmp_path / "report.md"
    code = run("compare", "--before", before, "--after", after, "--kind", kind,
               "--commit", SHA, "--report", report, *flags)
    assert code == (2 if case == "invalid" else int(reason is not None))
    content = report.read_text()
    headline = content.splitlines()[0]
    assert headline.startswith(f"Coverage gate at {SHA[:8]}: ")
    if reason:
        assert ": FAIL (" in headline and reason in headline
    else:
        assert headline.endswith(": PASS")
    if case == "saving":
        assert "intended saving" in content and "large" in content


def test_usage_error(tmp_path):
    report = tmp_path / "report.md"
    report.write_text(f"Coverage gate at {SHA[:8]}: PASS\n")
    assert run("compare", "--kind", "typo", "--report", report, "--commit", SHA) == 2
    assert report.read_text().splitlines()[0] == f"Coverage gate at {SHA[:8]}: FAIL (invalid usage)"


def test_collect(tmp_path):
    subprocess.run(["git", "clone", "-q", "--shared", "--no-checkout", "--single-branch",
                    "--no-tags", str(GATE.parents[2]), str(tmp_path)], check=True)
    (tmp_path / "test_pair.py").write_text("def test_one(): pass\ndef test_two(): pass\n")
    assert run("collect", "--env", "py", "--out", "reports", "--", "test_pair.py", "-q",
               cwd=tmp_path) == 0
    evidence = tmp_path / "reports/py"
    record = json.loads((evidence / "env.json").read_text())
    head = subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=tmp_path, text=True).strip()
    assert record["commit"] == head and record["clean"] is False
    assert record["complete"] is True and record["pytest_exit_code"] == 0
    assert record["obligation"] == "pre-merge" and record["python_version"]
    assert "test_pair.py" in record["command"]
    assert len(ElementTree.parse(evidence / "junit.xml").findall(".//testcase")) == 2
    assert "test_pair.py" in json.loads((evidence / "coverage.json").read_text())["files"]
