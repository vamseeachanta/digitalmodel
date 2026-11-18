"""Report evals: contract, comparison rules, run records and the offline evals.

The CP edition eval needs digitalmodel and network access for the BSEE file; the
scheduled report-eval job runs it.
"""

from __future__ import annotations

import copy
import importlib.util
import json
import os
import subprocess
import sys
import tempfile
from pathlib import Path

import pytest

EVALS = Path(__file__).resolve().parents[2] / "examples" / "report-evals"
ALL = sorted(p.name for p in EVALS.iterdir() if (p / "results.json").exists())
# Variables that would point git at the caller's repository instead of a fixture.
GIT_BINDINGS = ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE",
                "GIT_OBJECT_DIRECTORY", "GIT_ALTERNATE_OBJECT_DIRECTORIES", "GIT_NAMESPACE")


def _git(*args: str, **kwargs) -> subprocess.CompletedProcess:
    """git for fixture repositories, with the caller's repository bindings cleared."""
    env = {k: v for k, v in os.environ.items() if k.upper() not in GIT_BINDINGS}
    return subprocess.run(["git", *args], env=env, **kwargs)


def _kit():
    sys.path.insert(0, str(EVALS))
    import evalkit
    return evalkit


def _schema() -> dict:
    return json.loads((EVALS / "results.schema.json").read_text(encoding="utf-8"))


def _manifest(folder: str) -> dict:
    return json.loads((EVALS / folder / "results.json").read_text(encoding="utf-8"))


def _run(folder: str, *args: str) -> subprocess.CompletedProcess:
    return subprocess.run([sys.executable, str(EVALS / folder / "eval.py"), *args],
                          capture_output=True, text=True, encoding="utf-8", timeout=600)


@pytest.mark.parametrize("folder", ALL)
def test_results_json_meets_contract(folder):
    """--validate-only checks the full schema with the standard library."""
    out = _run(folder, "--validate-only")
    assert out.returncode == 0, out.stdout + out.stderr
    assert "valid against results.schema.json" in out.stdout


@pytest.mark.parametrize("folder", ALL)
def test_results_json_passes_jsonschema(folder):
    """Cross-check with the reference validator where it is installed."""
    jsonschema = pytest.importorskip("jsonschema")
    validator = jsonschema.Draft202012Validator(_schema())
    errors = [e.message for e in validator.iter_errors(_manifest(folder))]
    assert not errors, errors


@pytest.mark.parametrize("folder", ALL)
def test_last_run_is_bound_to_current_files(folder):
    records = sorted((EVALS / folder).glob("last-run*.json"))
    assert records, "no run record committed"
    for record in records:
        out = _run(folder, "--check-record", str(record))
        assert out.returncode == 0, (record.name, out.stdout)


@pytest.mark.parametrize("mutate, expected", [
    (lambda d: d.update(results=[]), "non-empty"),
    (lambda d: d["results"][0].update(tolerance={"abs": -1}), ">= 0"),
    (lambda d: d["report"].pop("title"), "title"),
    (lambda d: d["results"][0].update(scope="sometimes"), "scope"),
    (lambda d: d["results"].append(dict(d["results"][0])), "duplicate"),
])
def test_invalid_manifests_are_rejected(mutate, expected):
    kit = _kit()
    doc = copy.deepcopy(_manifest(ALL[0]))
    mutate(doc)
    errors = kit.validate(doc, _schema())
    assert errors and any(expected in e for e in errors), errors
    try:
        jsonschema = __import__("jsonschema")
    except ImportError:
        return
    if expected != "duplicate":  # JSON Schema cannot express unique ids
        assert list(jsonschema.Draft202012Validator(_schema()).iter_errors(doc))


def test_comparison_contract():
    kit = _kit()
    boundary = {"value": 100, "tolerance": {"rel_pct": 29}, "basis": "fixed"}
    assert kit.compare_one(boundary, 129)[0] == "MATCH"      # exact rational bound
    assert kit.compare_one(boundary, 129.0)[0] == "MATCH"
    assert kit.compare_one(boundary, 130)[0] == "MISMATCH"
    assert kit.compare_one(boundary, 10**400)[0] == "MISMATCH"  # beyond float range
    small = {"value": 1000, "tolerance": {"rel_pct": 0.7}, "basis": "fixed"}
    assert kit.compare_one(small, 1007)[0] == "MATCH"
    assert kit.compare_one(small, 1007.0)[0] == "MATCH"
    assert kit.compare_one(small, 1008)[0] == "MISMATCH"
    fixed = {"value": 10, "tolerance": {"abs": 0.5}, "basis": "fixed"}
    live = {"value": 200, "tolerance": {"rel_pct": 1.0}, "basis": "live"}
    zero = {"value": 0, "tolerance": {"rel_pct": 5.0}, "basis": "fixed"}
    flag = {"value": True, "tolerance": {"abs": 0}, "basis": "fixed"}
    assert kit.compare_one(fixed, 10.5)[0] == "MATCH"        # inclusive bound
    assert kit.compare_one(fixed, 10.51)[0] == "MISMATCH"
    assert kit.compare_one(live, 202)[0] == "MATCH"          # 1 % of 200, inclusive
    assert kit.compare_one(live, 202.1)[0] == "DRIFT"
    assert kit.compare_one(zero, 0)[0] == "MATCH"            # zero reference is exact
    assert kit.compare_one(zero, 0.001)[0] == "MISMATCH"
    assert kit.compare_one(zero, 5e-10)[0] == "MISMATCH"     # no floating allowance at zero
    big = {"value": 1_000_000_000, "tolerance": {"abs": 0}, "basis": "fixed"}
    assert kit.compare_one(big, 1_000_000_000)[0] == "MATCH"
    assert kit.compare_one(big, 1_000_000_001)[0] == "MISMATCH"  # integers compare exactly
    assert kit.compare_one(fixed, True)[0] == "MISMATCH"     # boolean for a number
    assert kit.compare_one(flag, 1)[0] == "MISMATCH"         # number for a boolean
    assert kit.compare_one(fixed, float("nan"))[0] == "MISMATCH"
    assert kit.compare_one(fixed, None)[0] == "NOT_EVALUATED"
    rows = [{"status": "MATCH"}, {"status": "NOT_EVALUATED"}, {"status": "MISMATCH"}]
    assert kit.grade(rows) == "FAIL" and kit.grade(rows[:2]) == "PARTIAL" and kit.grade([]) == "PARTIAL"


def _reader(tmp_path, name, items):
    path = tmp_path / name
    path.write_text(json.dumps({"results": items}), encoding="utf-8")
    return str(path)


def test_compare_mode(tmp_path):
    folder = ALL[0]
    graded = [r for r in _manifest(folder)["results"] if r.get("scope", "both") == "both"]
    full = [{"id": r["id"], "value": r["value"]} for r in graded]
    assert _run(folder, "--compare", _reader(tmp_path, "full.json", full)).returncode == 0
    assert _run(folder, "--compare", _reader(tmp_path, "empty.json", [])).returncode == 2
    assert _run(folder, "--compare", _reader(tmp_path, "part.json", full[:1])).returncode == 2
    dup = full + [full[0]]
    assert _run(folder, "--compare", _reader(tmp_path, "dup.json", dup)).returncode == 1
    extra = full + [{"id": "not_a_result", "value": 1}]
    assert _run(folder, "--compare", _reader(tmp_path, "extra.json", extra)).returncode == 1
    out = tmp_path / "odd.json"
    odd = _run(folder, "--compare", _reader(tmp_path, "odd-id.json", [{"id": [], "value": 1}]), "--out", str(out))
    assert odd.returncode == 1 and json.loads(out.read_text(encoding="utf-8"))["grade"] == "FAIL"


def _demo_folder(tmp_path):
    work = tmp_path / "evals" / "demo"
    work.mkdir(parents=True)
    (tmp_path / "evals" / "results.schema.json").write_text(json.dumps(_schema()), encoding="utf-8")
    (tmp_path / "evals" / "evalkit.py").write_text((EVALS / "evalkit.py").read_text(encoding="utf-8"),
                                                   encoding="utf-8")
    (work / "results.json").write_text(json.dumps(_manifest(ALL[0])), encoding="utf-8")
    (work / "eval.py").write_text("# demo\n", encoding="utf-8")
    (work / "PROMPT.md").write_text("demo\n", encoding="utf-8")
    return work


def test_failure_after_final_value_fails_the_run(tmp_path):
    """compute() that fills every value and then raises must not grade PASS."""
    kit = _kit()
    work = _demo_folder(tmp_path)
    results = _manifest(ALL[0])["results"]

    def compute(args, values, notes, context):
        for r in results:
            values[r["id"]] = r["value"]
        raise ValueError("failure after final value")

    out = tmp_path / "run.json"
    assert kit.main(work / "results.json", compute, argv=["--out", str(out)]) == 1
    record = json.loads(out.read_text(encoding="utf-8"))
    assert record["grade"] == "FAIL" and any("failure after final value" in e for e in record["errors"])
    assert {r["status"] for r in record["results"]} == {"MATCH"}


def test_record_written_without_out(tmp_path, monkeypatch):
    kit = _kit()
    work = _demo_folder(tmp_path)
    monkeypatch.setattr(kit.tempfile, "gettempdir", lambda: str(tmp_path))
    runs = Path(tempfile.gettempdir()) / "report-evals-runs"
    before = set(runs.glob("*.json")) if runs.exists() else set()
    kit.main(work / "results.json", lambda args, values, notes, context: None, argv=[])
    assert set(runs.glob("*.json")) - before


@pytest.mark.parametrize("mode, removed", [("rebuild", "eval.py"), ("prompt-level", "PROMPT.md"),
                                          ("rebuild", "../evalkit.py")])
def test_missing_bound_file_makes_record_stale(tmp_path, mode, removed):
    kit = _kit()
    work = _demo_folder(tmp_path)
    names = kit.PROMPT_ARTIFACTS if mode == "prompt-level" else kit.REBUILD_ARTIFACTS
    record = tmp_path / "record.json"
    record.write_text(json.dumps({"mode": mode, "artifacts": kit.artifacts(work, names)}), encoding="utf-8")
    assert kit.check_record(work, record) == []
    (work / removed).unlink()
    assert any("missing" in p for p in kit.check_record(work, record))


def test_failed_compute_still_writes_record(tmp_path):
    """An exception in compute() grades its results ERROR and the record is written."""
    kit = _kit()
    work = tmp_path / "evals" / "demo"
    work.mkdir(parents=True)
    (tmp_path / "evals" / "results.schema.json").write_text(json.dumps(_schema()), encoding="utf-8")
    (work / "results.json").write_text(json.dumps(_manifest(ALL[0])), encoding="utf-8")

    def compute(args, values, notes, context):
        values[_manifest(ALL[0])["results"][0]["id"]] = _manifest(ALL[0])["results"][0]["value"]
        raise OSError("network down")

    out = tmp_path / "run.json"
    assert kit.main(work / "results.json", compute, argv=["--out", str(out)]) == 1
    record = json.loads(out.read_text(encoding="utf-8"))
    assert record["grade"] == "FAIL" and "network down" in record["context"]["compute_error"]
    assert record["results"][0]["status"] == "MATCH"
    assert {r["status"] for r in record["results"][1:]} == {"ERROR"}


def test_same_second_runs_keep_both_records(tmp_path, monkeypatch):
    """Two runs started in the same second write two records; neither replaces the other."""
    kit = _kit()
    work = _demo_folder(tmp_path)
    monkeypatch.setattr(kit.tempfile, "gettempdir", lambda: str(tmp_path))
    monkeypatch.setattr(kit, "utc_now", lambda: "2026-11-06T12:00:00Z")
    results = _manifest(ALL[0])["results"]

    def failing(args, values, notes, context):
        raise ValueError("first run fails")

    def passing(args, values, notes, context):
        for r in results:
            values[r["id"]] = r["value"]

    runs = Path(tempfile.gettempdir()) / "report-evals-runs"
    before = set(runs.glob("*.json")) if runs.exists() else set()
    assert kit.main(work / "results.json", failing, argv=[]) == 1
    kit.main(work / "results.json", passing, argv=[])
    new = set(runs.glob("*.json")) - before
    grades = sorted(json.loads(p.read_text(encoding="utf-8"))["grade"] for p in new)
    assert len(new) == 2 and grades[0] == "FAIL", grades


def test_offline_blob_lookup_never_fetches(tmp_path, monkeypatch):
    """A pinned blob missing from the clone is unavailable offline; git fetch is not run."""
    kit = _kit()
    calls = []
    real_run = kit.subprocess.run

    def recording_run(cmd, *args, **kwargs):
        calls.append(cmd)
        return real_run(cmd, *args, **kwargs)

    monkeypatch.setattr(kit.subprocess, "run", recording_run)
    assert kit.git_blob(EVALS.parents[1], "0" * 40, "README.md", fetch=False) is None
    assert calls and not any("fetch" in c for c in calls)
    args = type("Args", (), {"repo_root": EVALS.parents[1], "pinned_commit": "0" * 40,
                             "revision": "pinned", "offline": True})()
    with pytest.raises(FileNotFoundError):
        kit.repo_file(args, "README.md")
    assert not any("fetch" in c for c in calls)


OFFLINE = ["issue-001-hull-roll-screening", "issue-001-sloshing-frequency"]


@pytest.mark.parametrize("folder", OFFLINE)
@pytest.mark.parametrize("revision", ["candidate", "pinned"])
def test_offline_eval_passes(folder, revision):
    """Candidate regression (this checkout) and historical reproduction (code.commit) both pass."""
    out = _run(folder, "--revision", revision)
    assert out.returncode == 0 and "GRADE: PASS" in out.stdout, out.stdout + out.stderr


@pytest.mark.parametrize("edit", ["duplicate", "period"])
def test_hull_archive_needs_one_to_one_cases(tmp_path, edit):
    """A duplicated case or a changed peak period must not count as reproduced."""
    folder = "issue-001-hull-roll-screening"
    root = EVALS.parents[1]
    archive = json.loads((root / "docs/api/cfd/data/generic_hull_roll_screening.json").read_text(encoding="utf-8"))
    if edit == "duplicate":
        archive["cases"][-1] = copy.deepcopy(archive["cases"][0])
    else:
        archive["cases"][0]["tank_peak_period_s"] = 999.0
    path = tmp_path / "archive.json"
    path.write_text(json.dumps(archive), encoding="utf-8")
    out = _run(folder, "--archive", str(path))
    assert out.returncode == 1 and "MISMATCH" in out.stdout, out.stdout


def _cp_module():
    sys.path.insert(0, str(EVALS))
    spec = importlib.util.spec_from_file_location("_cp_eval", EVALS / "issue-002-cp-anode-editions" / "eval.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_cp_package_must_match_revision(tmp_path):
    """The imported digitalmodel must come from the graded checkout (candidate) or the pin (pinned)."""
    module = _cp_module()
    checkout = tmp_path / "checkout"
    inside = checkout / "src" / "digitalmodel" / "__init__.py"
    outside = tmp_path / "other" / "src" / "digitalmodel" / "__init__.py"
    pin = "0" * 40

    def args(revision):
        return type("Args", (), {"repo_root": checkout, "revision": revision, "pinned_commit": pin})()

    module.check_package(args("candidate"), inside, "1" * 40)
    with pytest.raises(ImportError):
        module.check_package(args("candidate"), outside, pin)  # historical package in a candidate run
    module.check_package(args("pinned"), outside, pin)
    with pytest.raises(ImportError):
        module.check_package(args("pinned"), inside, "1" * 40)


@pytest.mark.parametrize("entry", [None, 3, "invalid", []])
def test_non_object_result_is_rejected(entry):
    doc = copy.deepcopy(_manifest(ALL[0]))
    doc["results"][0] = entry
    assert _kit().validate(doc, _schema())


def test_pinned_files_are_isolated(tmp_path, monkeypatch):
    """Each run writes pinned blobs to its own private directory."""
    kit = _kit()
    monkeypatch.setattr(kit.tempfile, "gettempdir", lambda: str(tmp_path))
    monkeypatch.setattr(kit, "git_blob", lambda *args, **kwargs: b"# fixture\n")

    def args():
        return type("Args", (), {"revision": "pinned", "repo_root": tmp_path, "pinned_commit": "0" * 40})()

    first, second = args(), args()
    path = kit.repo_file(first, "code/script.py")
    neighbor = kit.repo_file(first, "code/data.json")
    assert path.parent == neighbor.parent
    assert path != kit.repo_file(second, "code/script.py")


def test_record_reports_dirty_artifacts(tmp_path):
    kit = _kit()
    work = _demo_folder(tmp_path)
    _git("init", "-q", str(tmp_path), check=True)
    out = tmp_path / "record.json"
    kit.main(work / "results.json", lambda *args: None, argv=["--out", str(out)])
    assert json.loads(out.read_text(encoding="utf-8"))["context"]["artifacts_dirty"] is True


def test_unreadable_reader_and_huge_number_fail(tmp_path):
    """A reader path that is a directory, or a 401-digit integer, grades FAIL with a record."""
    folder = ALL[0]
    out = tmp_path / "dir-run.json"
    run = _run(folder, "--compare", str(tmp_path), "--out", str(out))
    assert run.returncode == 1, run.stderr
    assert json.loads(out.read_text(encoding="utf-8"))["grade"] == "FAIL"
    numeric = next(r for r in _manifest(folder)["results"]
                   if r.get("scope", "both") == "both" and isinstance(r["value"], int)
                   and not isinstance(r["value"], bool))
    reader = tmp_path / "huge.json"
    reader.write_text('{"results": [{"id": "%s", "value": %s}]}' % (numeric["id"], "9" * 401), encoding="utf-8")
    out = tmp_path / "huge-run.json"
    run = _run(folder, "--compare", str(reader), "--out", str(out))
    assert run.returncode == 1, run.stderr
    record = json.loads(out.read_text(encoding="utf-8"))
    assert record["grade"] == "FAIL" and "MISMATCH" in {r["status"] for r in record["results"]}


def test_git_calls_ignore_caller_bindings(tmp_path, monkeypatch):
    """GIT_DIR and related variables must not redirect fixture or evalkit git calls."""
    kit = _kit()
    caller = tmp_path / "caller"
    _git("init", "-q", str(caller), check=True)
    (caller / "keep.txt").write_text("caller file\n", encoding="utf-8")

    def snapshot():
        return {str(f.relative_to(caller)): f.read_bytes() for f in caller.rglob("*") if f.is_file()}

    before = snapshot()
    monkeypatch.setenv("GIT_DIR", str(caller / ".git"))
    monkeypatch.setenv("GIT_WORK_TREE", str(caller))
    monkeypatch.setenv("GIT_COMMON_DIR", str(caller / ".git"))
    fixture = tmp_path / "fixture"
    _git("init", "-q", str(fixture), check=True)
    (fixture / "a.txt").write_bytes(b"a\n")
    _git("-C", str(fixture), "add", "a.txt", check=True)
    _git("-C", str(fixture), "-c", "user.name=fixture", "-c", "user.email=fixture-identity",
         "-c", "commit.gpgsign=false", "commit", "-q", "-m", "fixture", check=True)
    head = kit.git_head(fixture)
    assert head is not None
    assert kit.git_blob(fixture, head, "a.txt", fetch=False) == b"a\n"
    assert snapshot() == before


@pytest.fixture(autouse=True)
def _private_temp(tmp_path, monkeypatch):
    """Runs without --out, pinned blobs and caches go under this test's directory."""
    temp = tmp_path / "system-temp"
    temp.mkdir()
    for name in ("TMPDIR", "TEMP", "TMP"):
        monkeypatch.setenv(name, str(temp))
    monkeypatch.setattr(tempfile, "tempdir", str(temp))
    # git must not find a repository above the test directory
    monkeypatch.setenv("GIT_CEILING_DIRECTORIES", str(tmp_path.parent))


def _commit_all(repo):
    _git("-C", str(repo), "add", "-A", check=True)
    _git(
        "-C",
        str(repo),
        "-c",
        "user.name=fixture",
        "-c",
        "user.email=fixture-identity",
        "-c",
        "commit.gpgsign=false",
        "commit",
        "-q",
        "-m",
        "fixture",
        check=True,
    )


def _record_context(work, tmp_path):
    out = tmp_path / "record.json"
    _kit().main(work / "results.json", lambda *args: None, argv=["--out", str(out)])
    return json.loads(out.read_text(encoding="utf-8"))["context"]


@pytest.mark.parametrize("state", ["clean", "ignored", "no-repository"])
def test_record_dirty_flag_states(tmp_path, state):
    """Clean committed files: false. Files in an ignored folder: true. No repository: null."""
    work = _demo_folder(tmp_path)
    if state != "no-repository":
        _git("init", "-q", str(tmp_path), check=True)
        if state == "ignored":
            (tmp_path / ".gitignore").write_text("evals/\n", encoding="utf-8")
        _commit_all(tmp_path)
    context = _record_context(work, tmp_path)
    expected = {"clean": False, "ignored": True, "no-repository": None}[state]
    assert context["artifacts_dirty"] is expected
    assert (context["artifacts_repo_head"] is None) == (state == "no-repository")


def test_record_ignores_caller_git_bindings(tmp_path, monkeypatch):
    """main() reports the repository that holds the eval files, not one named by GIT_DIR."""
    work = _demo_folder(tmp_path)
    _git("init", "-q", str(tmp_path), check=True)
    _commit_all(tmp_path)
    head = _git(
        "-C", str(tmp_path), "rev-parse", "HEAD", capture_output=True, text=True
    ).stdout.strip()
    caller = tmp_path.parent / (tmp_path.name + "-caller")
    _git("init", "-q", str(caller), check=True)
    monkeypatch.setenv("GIT_DIR", str(caller / ".git"))
    monkeypatch.setenv("GIT_WORK_TREE", str(caller))
    context = _record_context(work, tmp_path)
    assert context["artifacts_repo_head"] == head
    assert context["artifacts_dirty"] is False


def test_scratch_directory_is_removed(tmp_path, monkeypatch):
    """Pinned blobs written during compute() are removed when the run ends."""
    kit = _kit()
    work = _demo_folder(tmp_path)
    monkeypatch.setattr(kit, "git_blob", lambda *args, **kwargs: b"# fixture\n")
    seen = []

    def compute(args, values, notes, context):
        args.repo_root = tmp_path
        seen.append(kit.repo_file(args, "code/script.py"))

    out = tmp_path / "record.json"
    kit.main(
        work / "results.json", compute, argv=["--revision", "pinned", "--out", str(out)]
    )
    assert seen and not seen[0].exists() and not seen[0].parent.parent.exists()

def test_non_finite_reader_value_keeps_record_strict_json(tmp_path):
    """A reader value of 1e400 grades FAIL and the record parses as strict JSON."""
    folder = ALL[0]
    numeric = next(
        r
        for r in _manifest(folder)["results"]
        if r.get("scope", "both") == "both"
        and isinstance(r["value"], (int, float))
        and not isinstance(r["value"], bool)
    )
    reader = tmp_path / "inf.json"
    reader.write_text(
        '{"results": [{"id": "%s", "value": 1e400}]}' % numeric["id"], encoding="utf-8"
    )
    out = tmp_path / "inf-run.json"
    run = _run(folder, "--compare", str(reader), "--out", str(out))
    assert run.returncode == 1, run.stderr

    def reject(token):
        raise ValueError(f"non-standard JSON constant {token}")

    record = json.loads(out.read_text(encoding="utf-8"), parse_constant=reject)
    row = next(r for r in record["results"] if r["id"] == numeric["id"])
    assert record["grade"] == "FAIL"
    assert row["status"] == "MISMATCH" and row["got"] == "inf"
    assert _kit().json_safe({"a": [float("nan"), float("-inf"), 1.5]}) == {
        "a": ["nan", "-inf", 1.5]
    }
