# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Retention boundaries use disposable histories and real inventory evidence."""

from __future__ import annotations

import copy
import io
import json
import subprocess
from pathlib import Path
from urllib.error import URLError

import pytest

from digitalmodel.parametric import historical_provenance as history

ROOT = Path(__file__).resolve().parents[2]
INDEX = "atlases/historical-provenance.json"


@pytest.fixture
def repository(tmp_path, monkeypatch, request):
    # Never let a caller's Git bindings redirect fixture mutations.
    for name in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE",
                 "GIT_OBJECT_DIRECTORY", "GIT_ALTERNATE_OBJECT_DIRECTORIES"):
        monkeypatch.delenv(name, raising=False)
    monkeypatch.delenv("GITHUB_REF", raising=False)
    monkeypatch.delenv("GITHUB_EVENT_PATH", raising=False)
    monkeypatch.setenv("GITHUB_REPOSITORY", "example/repository")
    # Only disposable histories receive policy evidence; real inventories do not.
    monkeypatch.setattr(history, "_integration_policy", lambda *args: {
        "allow_merge_commit": True, "allow_squash_merge": False,
        "allow_rebase_merge": False,
    }, raising=False)
    monkeypatch.setenv("GIT_CONFIG_COUNT", "1")
    monkeypatch.setenv("GIT_CONFIG_KEY_0", "core.autocrlf")
    monkeypatch.setenv("GIT_CONFIG_VALUE_0", "false")
    root = tmp_path / "repository"
    root.mkdir()

    def git(*args, data=None):
        return subprocess.check_output(
            ["git", "-C", str(root), *args], input=data, stderr=subprocess.PIPE,
        ).decode().strip()

    git("init", "-b", "main")
    git("remote", "add", "origin", "https://github.com/example/repository.git")
    # Disposable fixtures shall also run where no contributor identity exists.
    # These local settings affect only the newly initialized test repository.
    git("config", "user.name", "Provenance fixture")
    git("config", "user.email", "fixture")
    row = copy.deepcopy(next(
        row for row in json.loads((ROOT / INDEX).read_text(encoding="utf-8"))["atlases"]
        if row["status"] == "VERIFIED"
        and row["workflow_id"] == getattr(request, "param", "mooring-fatigue")
    ))
    reader = history.git_reader(ROOT, row["source_revision"])
    for name in row["sources"]:
        path = root / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes(reader(name))
    revision = subprocess.check_output(["git", "-C", str(ROOT), "rev-parse", "HEAD"]).decode().strip()
    artifacts = history.git_reader(ROOT, revision)
    for name in row["artifacts"]:
        path = root / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes(artifacts(name))

    def commit(message, *parents):
        # Build both merge styles explicitly, without changing the real checkout.
        git("add", ".")
        tree = git("write-tree")
        arguments = ["commit-tree", tree]
        for parent in parents:
            arguments.extend(["-p", parent])
        return git(*arguments, data=(message + "\n").encode())

    source = commit("Default source")
    row["source_revision"] = source
    wrapper = root / history.WRAPPER
    original = wrapper.read_bytes()
    wrapper.write_bytes(original + b"\n# Feature wrapper snapshot.\n")
    feature_source = commit("Feature source", source)
    feature_row = copy.deepcopy(row)
    feature_row["source_revision"] = feature_source
    feature_row.update(history.source_record(row["workflow_id"], history.archive_reader(root)))
    wrapper.write_bytes(original + b"\n# Unrelated wrapper snapshot.\n")
    unrelated = commit("Unrelated source", source)
    git("update-ref", "refs/heads/unrelated", unrelated)
    wrapper.write_bytes(original)

    def prepare(kind="modified"):
        before = [] if kind == "added" else [feature_row if kind == "unchanged" else row]
        index = root / INDEX
        index.write_text(json.dumps({"atlases": before}), encoding="utf-8", newline="\n")
        base = commit("Default inventory", source)
        git("update-ref", "refs/heads/main", base)
        git("update-ref", "refs/remotes/origin/main", base)
        wrapper.write_bytes(original + b"\n# Feature wrapper snapshot.\n")
        current_row = row if kind == "retained" else feature_row
        index.write_text(json.dumps({"atlases": [current_row]}), encoding="utf-8", newline="\n")
        head = commit("Feature inventory", base, feature_source)
        git("update-ref", "refs/heads/feature", head)
        git("symbolic-ref", "HEAD", "refs/heads/feature")
        return dict(root=root, git=git, commit=commit, base=base, head=head,
                    row=copy.deepcopy(current_row), unrelated=unrelated,
                    source=source, before=before)

    return prepare


def local_ci_merge(case, monkeypatch, *, event=False):
    """Reproduce the workflow's detached merge without fetching a pull ref."""
    git = case["git"]
    retained = git("rev-parse", "origin/main")
    git("checkout", "--detach", retained)
    git("merge", "--no-ff", "--no-edit", case["head"])
    assert git("rev-parse", "--abbrev-ref", "HEAD") == "HEAD"
    assert git("rev-parse", "HEAD^1") == retained
    assert git("rev-parse", "HEAD^2") == case["head"]
    assert git("for-each-ref", "refs/pull/17/head") == ""
    monkeypatch.setenv("GITHUB_REF", "refs/pull/17/merge")
    if event:
        path = case["root"].parent / "event.json"
        path.write_text(json.dumps({"pull_request": {
            "head": {"sha": case["head"]}, "base": {"sha": case["base"],
                "ref": "main", "repo": {"full_name": "example/repository"}},
        }}), encoding="utf-8", newline="\n")
        monkeypatch.setenv("GITHUB_EVENT_PATH", str(path))


@pytest.mark.parametrize("kind", ["added", "retained"])
@pytest.mark.parametrize("event", [False, True])
def test_workflow_local_merge_without_pull_ref(repository, monkeypatch, kind, event):
    case = repository(kind)
    local_ci_merge(case, monkeypatch, event=event)
    result = history.verify_git(case["row"], case["root"])
    assert result["status"] == "VERIFIED"
    assert result["working_tree_drift"] == []


@pytest.mark.parametrize("ci_merge", [False, True])
def test_advancing_main_does_not_relax_untouched_entry(repository, monkeypatch, ci_merge):
    case = repository("unchanged")
    main_row = copy.deepcopy(case["row"])
    main_row["reason"] = "Metadata changed only on main"
    (case["root"] / INDEX).write_text(json.dumps({"atlases": [main_row]}), encoding="utf-8", newline="\n")
    tip = case["commit"]("Main-only inventory change", case["base"])
    case["git"]("update-ref", "refs/remotes/origin/main", tip)
    # Restore the feature's files before testing or building the CI merge.
    (case["root"] / INDEX).write_text(json.dumps({"atlases": [case["row"]]}), encoding="utf-8", newline="\n")
    case["git"]("add", INDEX)
    assert case["git"]("merge-base", tip, case["head"]) == case["base"]
    if ci_merge:
        local_ci_merge(case, monkeypatch, event=True)
    with pytest.raises(history.EvidenceError, match="unchanged.*default branch"):
        history.verify_git(case["row"], case["root"])


@pytest.mark.parametrize("event", [False, True])
def test_local_merge_cannot_substitute_synthetic_head(repository, monkeypatch, event):
    case = repository()
    local_ci_merge(case, monkeypatch, event=event)
    with pytest.raises(history.EvidenceError, match="CI pull request head"):
        history.verify_git(case["row"], case["root"], pr_head="HEAD")


@pytest.mark.parametrize("damage", ["missing", "malformed", "symbolic", "outside"])
def test_invalid_event_head_fails_closed(repository, monkeypatch, damage):
    case = repository()
    local_ci_merge(case, monkeypatch, event=True)
    path = case["root"].parent / "event.json"
    if damage == "missing":
        monkeypatch.setenv("GITHUB_EVENT_PATH", str(path.parent / "absent.json"))
    elif damage == "malformed":
        path.write_text("{", encoding="utf-8", newline="\n")
    else:
        head = "HEAD" if damage == "symbolic" else case["unrelated"]
        event_data = json.loads(path.read_text(encoding="utf-8"))
        event_data["pull_request"]["head"]["sha"] = head
        path.write_text(json.dumps(event_data), encoding="utf-8", newline="\n")
    with pytest.raises(history.EvidenceError, match="event payload|checkout ancestry"):
        history.verify_git(case["row"], case["root"])


def test_event_head_takes_precedence_over_stale_pull_ref(repository, monkeypatch):
    case = repository()
    local_ci_merge(case, monkeypatch, event=True)
    case["git"]("update-ref", "refs/pull/17/head", case["base"])
    assert history.verify_git(case["row"], case["root"])["status"] == "VERIFIED"


@pytest.mark.parametrize("parents", ["wrong-first", "extra"])
def test_local_merge_fallback_requires_exact_retained_parent(repository, monkeypatch, parents):
    case = repository()
    ancestry = [case["head"], case["base"]] if parents == "wrong-first" else [
        case["base"], case["head"], case["unrelated"]]
    synthetic = case["commit"]("Invalid merge shape", *ancestry)
    case["git"]("update-ref", "--no-deref", "HEAD", synthetic)
    monkeypatch.setenv("GITHUB_REF", "refs/pull/17/merge")
    with pytest.raises(history.EvidenceError, match="Missing CI pull request head ref"):
        history.verify_git(case["row"], case["root"])


def test_head_checkout_does_not_infer_its_own_parent(repository, monkeypatch):
    case = repository()
    monkeypatch.setenv("GITHUB_REF", "refs/pull/17/head")
    with pytest.raises(history.EvidenceError, match="Missing CI pull request head ref"):
        history.verify_git(case["row"], case["root"])


@pytest.mark.parametrize("event", [False, True])
def test_local_ci_merge_rejects_synthetic_only_inventory(repository, monkeypatch, event):
    case = repository()
    local_ci_merge(case, monkeypatch, event=event)
    row = copy.deepcopy(case["row"])
    row["source_revision"] = case["unrelated"]
    (case["root"] / INDEX).write_text(json.dumps({"atlases": [row]}), encoding="utf-8", newline="\n")
    synthetic = case["commit"]("Synthetic-only inventory", case["base"], case["head"])
    case["git"]("update-ref", "--no-deref", "HEAD", synthetic)
    with pytest.raises(history.EvidenceError, match="committed inventory"):
        history.verify_git(row, case["root"])


def test_merge_original_head_recovers_squashed_source_retention(repository):
    case = repository()
    squashed = case["commit"]("Squashed inventory", case["base"])
    case["git"]("update-ref", "refs/heads/main", squashed)
    case["git"]("update-ref", "refs/remotes/origin/main", squashed)
    case["git"]("symbolic-ref", "HEAD", "refs/heads/main")
    with pytest.raises(history.EvidenceError, match="squash.*merge commit"):
        history.verify_git(case["row"], case["root"])
    recovered = case["commit"]("Retain original source ancestry", squashed, case["head"])
    case["git"]("update-ref", "refs/heads/main", recovered)
    case["git"]("update-ref", "refs/remotes/origin/main", recovered)
    assert history.verify_git(case["row"], case["root"])["status"] == "VERIFIED"


@pytest.mark.parametrize("kind", ["added", "modified"])
@pytest.mark.parametrize("repository", ["mooring-fatigue", "riser-fatigue"], indirect=True)
def test_open_pull_request_retains_only_its_changed_entry(repository, kind):
    case = repository(kind)
    result = history.verify_git(case["row"], case["root"])
    assert result["status"] == "VERIFIED"
    assert result["working_tree_drift"] == []


def test_unchanged_entry_keeps_default_branch_retention(repository):
    case = repository("unchanged")
    with pytest.raises(history.EvidenceError, match="unchanged.*default branch"):
        history.verify_git(case["row"], case["root"])


@pytest.mark.parametrize("revision", ["unrelated", "unreachable", "default"])
def test_changed_entry_cannot_claim_revision_outside_its_head(repository, revision):
    case = repository()
    row = case["row"]
    if revision == "unreachable":
        row["source_revision"] = case["commit"]("Unreferenced source")
    elif revision == "default":
        # Even a retained default-branch revision must belong to the PR head
        # when the entry requests the PR-specific retention exception.
        row["source_revision"] = case["unrelated"]
        case["git"]("update-ref", "refs/remotes/origin/main", case["unrelated"])
    else:
        row["source_revision"] = case["unrelated"]
    (case["root"] / INDEX).write_text(json.dumps({"atlases": [row]}), encoding="utf-8", newline="\n")
    head = case["commit"]("Invalid provenance claim", case["head"])
    case["git"]("update-ref", "refs/heads/feature", head)
    with pytest.raises(history.EvidenceError, match="pull request head"):
        history.verify_git(row, case["root"])


def test_uncommitted_entry_cannot_obtain_pull_request_retention(repository):
    case = repository()
    row = copy.deepcopy(case["row"])
    row["reason"] = "Uncommitted metadata"
    with pytest.raises(history.EvidenceError, match="committed inventory"):
        history.verify_git(row, case["root"])


@pytest.mark.parametrize("style", ["merge", "squash"])
@pytest.mark.parametrize("repository", ["mooring-fatigue", "riser-fatigue"], indirect=True)
def test_default_branch_after_merge_requires_retained_sources(repository, style):
    case = repository()
    parents = (case["base"], case["head"]) if style == "merge" else (case["base"],)
    merged = case["commit"]("Merged inventory", *parents)
    case["git"]("update-ref", "refs/heads/main", merged)
    case["git"]("update-ref", "refs/remotes/origin/main", merged)
    case["git"]("symbolic-ref", "HEAD", "refs/heads/main")
    # Keeping the old feature and PR refs must never revive a squash orphan.
    case["git"]("update-ref", "refs/pull/17/head", case["head"])
    if style == "merge":
        assert history.verify_git(case["row"], case["root"])["status"] == "VERIFIED"
    else:
        with pytest.raises(history.EvidenceError, match="squash.*merge commit"):
            history.verify_git(case["row"], case["root"], pr_head="refs/pull/17/head")


def test_ci_merge_checkout_uses_pull_request_head(repository, monkeypatch):
    case = repository()
    case["git"]("update-ref", "refs/pull/17/head", case["head"])
    synthetic = case["commit"]("Synthetic check merge", case["base"], case["head"])
    case["git"]("update-ref", "refs/heads/check", synthetic)
    case["git"]("symbolic-ref", "HEAD", "refs/heads/check")
    monkeypatch.setenv("GITHUB_REF", "refs/pull/17/merge")
    assert history.verify_git(case["row"], case["root"])["status"] == "VERIFIED"


def test_ci_requires_the_actual_pull_request_head_ref(repository, monkeypatch):
    case = repository()
    single_parent = case["commit"]("Non-merge checkout", case["base"])
    case["git"]("update-ref", "--no-deref", "HEAD", single_parent)
    monkeypatch.setenv("GITHUB_REF", "refs/pull/17/merge")
    with pytest.raises(history.EvidenceError, match="Missing CI pull request head ref refs/pull/17/head"):
        history.verify_git(case["row"], case["root"])


def test_supplied_head_must_belong_to_current_checkout(repository):
    case = repository()
    with pytest.raises(history.EvidenceError, match="head.*checkout"):
        history.verify_git(case["row"], case["root"], pr_head="unrelated")


@pytest.mark.parametrize("damage", ["sources", "artifacts", "basis", "algorithm"])
def test_pull_request_retention_preserves_evidence_checks(repository, damage):
    case = repository()
    row = case["row"]
    if damage in {"sources", "artifacts"}:
        name = next(iter(row[damage]))
        row[damage][name] = "0" * 64
    elif damage == "basis":
        row[damage]["standards"] = []
    else:
        row[damage] = "unsupported"
    (case["root"] / INDEX).write_text(json.dumps({"atlases": [row]}), encoding="utf-8", newline="\n")
    head = case["commit"]("Damaged evidence", case["head"])
    case["git"]("update-ref", "refs/heads/feature", head)
    with pytest.raises(history.EvidenceError):
        history.verify_git(row, case["root"])


def test_default_branch_stays_strict_with_stale_remote_ref(repository):
    case = repository()
    merged = case["commit"]("Squashed inventory", case["base"])
    case["git"]("update-ref", "refs/heads/main", merged)
    case["git"]("symbolic-ref", "HEAD", "refs/heads/main")
    with pytest.raises(history.EvidenceError, match="squash.*merge commit"):
        history.verify_git(case["row"], case["root"])


def test_ci_cannot_override_its_head_with_the_synthetic_merge(repository, monkeypatch):
    case = repository()
    case["git"]("update-ref", "refs/pull/17/head", case["head"])
    synthetic = case["commit"]("Synthetic check merge", case["base"], case["head"])
    case["git"]("update-ref", "refs/heads/check", synthetic)
    case["git"]("symbolic-ref", "HEAD", "refs/heads/check")
    monkeypatch.setenv("GITHUB_REF", "refs/pull/17/merge")
    with pytest.raises(history.EvidenceError, match="CI pull request head"):
        history.verify_git(case["row"], case["root"], pr_head="HEAD")


def test_ci_rejects_source_only_in_synthetic_merge(repository, monkeypatch):
    case = repository()
    case["git"]("update-ref", "refs/pull/17/head", case["head"])
    row = case["row"]
    row["source_revision"] = case["unrelated"]
    (case["root"] / INDEX).write_text(json.dumps({"atlases": [row]}), encoding="utf-8", newline="\n")
    synthetic = case["commit"]("Synthetic-only entry", case["head"], case["unrelated"])
    case["git"]("update-ref", "refs/heads/check", synthetic)
    case["git"]("symbolic-ref", "HEAD", "refs/heads/check")
    monkeypatch.setenv("GITHUB_REF", "refs/pull/17/merge")
    with pytest.raises(history.EvidenceError, match="CI destination base"):
        history.verify_git(row, case["root"])


def test_verifier_ignores_inherited_repository_bindings(repository, monkeypatch):
    case = repository()
    caller_head = history._git(ROOT, "rev-parse", "HEAD")
    caller_index = (ROOT / INDEX).read_bytes()
    git_dir = history._git(ROOT, "rev-parse", "--absolute-git-dir").decode().strip()
    common_dir = history._git(ROOT, "rev-parse", "--git-common-dir").decode().strip()
    for name, value in (("GIT_DIR", git_dir), ("GIT_WORK_TREE", str(ROOT)),
                        ("GIT_COMMON_DIR", str(ROOT / common_dir)),
                        ("GIT_INDEX_FILE", str(ROOT / ".git"))):
        monkeypatch.setenv(name, value)
    assert history.verify_git(case["row"], case["root"])["status"] == "VERIFIED"
    assert history._git(ROOT, "rev-parse", "HEAD") == caller_head
    assert (ROOT / INDEX).read_bytes() == caller_index


def test_cli_accepts_explicit_pull_request_head(repository, monkeypatch, capsys):
    case = repository()
    verify = history.verify_git

    def in_fixture(record, **options):
        return verify(record, case["root"], **options)

    monkeypatch.setattr(history, "verify_git", in_fixture)
    assert history.main(["--index", str(case["root"] / INDEX), "--pr-head", "feature"]) == 0
    assert json.loads(capsys.readouterr().out)["status"] == "VERIFIED"


def test_detached_default_branch_ci_stays_strict_with_stale_ref(repository, monkeypatch):
    case = repository()
    merged = case["commit"]("Squashed inventory", case["base"])
    # A detached default-branch checkout with an older retained remote ref.
    case["git"]("update-ref", "--no-deref", "HEAD", merged)
    monkeypatch.setenv("GITHUB_REF", "refs/heads/main")
    with pytest.raises(history.EvidenceError, match="squash.*merge commit"):
        history.verify_git(case["row"], case["root"])


@pytest.mark.parametrize("kind", ["unchanged", "default"])
def test_retention_failure_identifies_ref_and_staleness(repository, monkeypatch, kind):
    case = repository("unchanged" if kind == "unchanged" else "modified")
    if kind == "default":
        monkeypatch.setenv("GITHUB_REF", "refs/heads/main")
    with pytest.raises(history.EvidenceError) as error:
        history.verify_git(case["row"], case["root"])
    assert "origin/main" in str(error.value)
    assert case["base"] in str(error.value)
    assert "or the retained ref is stale; fetch it" in str(error.value)


@pytest.mark.parametrize("variable", ["GIT_OBJECT_DIRECTORY", "GIT_ALTERNATE_OBJECT_DIRECTORIES"])
def test_verifier_ignores_inherited_object_directories(repository, monkeypatch, variable):
    case = repository()
    monkeypatch.setenv(variable, str(case["root"].parent / "missing-objects"))
    assert history.verify_git(case["row"], case["root"])["status"] == "VERIFIED"


def test_verifier_ignores_replace_objects(repository, monkeypatch):
    case = repository()
    case["git"]("replace", case["row"]["source_revision"], case["unrelated"])
    monkeypatch.delenv("GIT_NO_REPLACE_OBJECTS", raising=False)
    assert history.verify_git(case["row"], case["root"])["status"] == "VERIFIED"
    assert case["git"]("show-ref", "--hash", f"refs/replace/{case['row']['source_revision']}") == case["unrelated"]


def test_alternate_objects_cannot_supply_source_evidence(repository, monkeypatch):
    case = repository()
    content = case["git"]("cat-file", "-p", case["head"]) + "\nAlternate evidence\n"
    objects = case["root"].parent / "alternate-objects"
    objects.mkdir()
    monkeypatch.setenv("GIT_OBJECT_DIRECTORY", str(objects))
    foreign = case["git"]("hash-object", "-w", "-t", "commit", "--stdin", data=content.encode())
    monkeypatch.delenv("GIT_OBJECT_DIRECTORY")
    monkeypatch.setenv("GIT_ALTERNATE_OBJECT_DIRECTORIES", str(objects))
    with pytest.raises(history.EvidenceError, match="Unavailable Git evidence"):
        history.git_reader(case["root"], foreign)


def test_branch_only_source_rejects_unproven_merge_policy(repository, monkeypatch):
    case = repository()
    policies = [
        {"allow_merge_commit": True, "allow_squash_merge": True, "allow_rebase_merge": False},
        {"allow_merge_commit": True, "allow_squash_merge": False, "allow_rebase_merge": True},
        {"allow_merge_commit": False, "allow_squash_merge": False, "allow_rebase_merge": False},
        {},
        {"allow_merge_commit": True, "allow_squash_merge": "false", "allow_rebase_merge": False},
        {"allow_merge_commit": True, "allow_squash_merge": 0, "allow_rebase_merge": False},
    ]
    for policy in policies:
        monkeypatch.setattr(history, "_integration_policy", lambda *args: policy)
        with pytest.raises(history.EvidenceError, match="merge-only integration"):
            history.verify_git(case["row"], case["root"])


def test_default_retained_source_requires_no_policy_request(repository, monkeypatch):
    case = repository("retained")

    def unavailable(*args):
        raise AssertionError("Default-retained sources shall remain offline")

    monkeypatch.setattr(history, "_integration_policy", unavailable)
    assert history.verify_git(case["row"], case["root"])["status"] == "VERIFIED"


def test_fork_main_cannot_bypass_destination_merge_policy(repository, monkeypatch):
    case = repository()
    local_ci_merge(case, monkeypatch, event=True)
    event = case["root"].parent / "event.json"
    event.write_text(json.dumps({"pull_request": {
        "head": {"sha": case["head"]}, "base": {"sha": case["base"],
            "ref": "main", "repo": {"full_name": "example/repository"}},
    }}), encoding="utf-8", newline="\n")
    case["git"]("update-ref", "refs/remotes/origin/main", case["head"])
    monkeypatch.setattr(history, "_integration_policy", lambda *args: {
        "allow_merge_commit": True, "allow_squash_merge": True,
        "allow_rebase_merge": True,
    })
    with pytest.raises(history.EvidenceError, match="merge-only integration"):
        history.verify_git(case["row"], case["root"])


def test_ci_rejects_invalid_destination_base(repository, monkeypatch):
    case = repository()
    local_ci_merge(case, monkeypatch, event=True)
    path = case["root"].parent / "event.json"
    original = json.loads(path.read_text(encoding="utf-8"))
    for field, value in (("sha", "HEAD"), ("sha", case["unrelated"]),
                         ("ref", "other"), ("repo", {"full_name": "example/other"})):
        event = copy.deepcopy(original)
        event["pull_request"]["base"][field] = value
        path.write_text(json.dumps(event), encoding="utf-8", newline="\n")
        with pytest.raises(history.EvidenceError, match="CI destination base"):
            history.verify_git(case["row"], case["root"])
    event = copy.deepcopy(original)
    del event["pull_request"]["base"]
    path.write_text(json.dumps(event), encoding="utf-8", newline="\n")
    with pytest.raises(history.EvidenceError, match="CI destination base"):
        history.verify_git(case["row"], case["root"])


def test_ci_destination_retained_source_stays_offline(repository, monkeypatch):
    case = repository("retained")
    local_ci_merge(case, monkeypatch, event=True)
    case["git"]("update-ref", "refs/remotes/origin/main", case["head"])

    def unavailable(*args):
        raise AssertionError("Destination-retained sources shall remain offline")

    monkeypatch.setattr(history, "_integration_policy", unavailable)
    assert history.verify_git(case["row"], case["root"])["status"] == "VERIFIED"


def test_ci_without_event_rejects_fork_retained_base(repository, monkeypatch):
    case = repository()
    local_ci_merge(case, monkeypatch)
    case["git"]("remote", "set-url", "origin", "https://github.com/example/fork.git")
    with pytest.raises(history.EvidenceError, match="CI destination base"):
        history.verify_git(case["row"], case["root"])


@pytest.fixture
def policy_request(tmp_path, monkeypatch):
    for name in ("GITHUB_REF", "GITHUB_REPOSITORY", "GITHUB_ACTIONS", "GITHUB_EVENT_PATH",
                 "GH_TOKEN", "GITHUB_TOKEN"):
        monkeypatch.delenv(name, raising=False)
    monkeypatch.setattr(history, "_git", lambda *args: b"https://github.com/example/repository.git")
    requests = []
    metadata = {"full_name": "example/repository", "default_branch": "main",
                "allow_merge_commit": True, "allow_squash_merge": False,
                "allow_rebase_merge": False}

    class Connection:
        def __init__(self, host, *, timeout):
            self.host = host
            self.timeout = timeout

        def request(self, method, path, *, headers):
            assert method == "GET"
            requests.append(("https://" + self.host + path, self.timeout))

        def getresponse(self):
            response = io.BytesIO(json.dumps(metadata).encode("utf-8"))
            response.status = 200
            return response

        def close(self):
            pass

    monkeypatch.setattr(history, "HTTPSConnection", Connection)
    return tmp_path, metadata, requests


def test_policy_reader_binds_response_to_destination(policy_request):
    root, metadata, requests = policy_request
    assert history._integration_policy(root, "origin/main") == metadata
    assert requests == [("https://api.github.com/repos/example/repository", 10)]


@pytest.mark.parametrize("damage", ["identity", "branch", "transport", "json", "shape", "http"])
def test_policy_reader_rejects_unavailable_or_misdirected_evidence(policy_request, monkeypatch, damage):
    root, metadata, _ = policy_request
    if damage == "identity":
        metadata["full_name"] = "example/other"
    elif damage == "branch":
        metadata["default_branch"] = "other"
    elif damage == "transport":
        def unavailable(*args, **kwargs):
            raise URLError("Unavailable policy")
        monkeypatch.setattr(history.HTTPSConnection, "request", unavailable)
    else:
        def invalid_response(self):
            response = io.BytesIO(b"{" if damage == "json" else b"null")
            response.status = 403 if damage == "http" else 200
            return response
        monkeypatch.setattr(history.HTTPSConnection, "getresponse", invalid_response)
    with pytest.raises(history.EvidenceError, match="integration policy"):
        history._integration_policy(root, "origin/main")


def test_ci_policy_uses_base_repository_instead_of_fork_origin(policy_request, monkeypatch):
    root, metadata, requests = policy_request
    monkeypatch.setenv("GITHUB_REF", "refs/pull/17/merge")
    monkeypatch.setenv("GITHUB_REPOSITORY", "example/destination")
    metadata["full_name"] = "example/destination"
    assert history._integration_policy(root, "origin/main") == metadata
    assert requests[0][0] == "https://api.github.com/repos/example/destination"


def test_ci_policy_rejects_mismatched_event_destination(policy_request, monkeypatch):
    root, _, requests = policy_request
    event = root / "event.json"
    event.write_text(json.dumps({"pull_request": {"base": {
        "repo": {"full_name": "example/other"}}}}), encoding="utf-8", newline="\n")
    monkeypatch.setenv("GITHUB_REF", "refs/pull/17/merge")
    monkeypatch.setenv("GITHUB_REPOSITORY", "example/destination")
    monkeypatch.setenv("GITHUB_EVENT_PATH", str(event))
    with pytest.raises(history.EvidenceError, match="integration policy"):
        history._integration_policy(root, "origin/main")
    assert requests == []


def test_local_policy_ignores_inherited_ci_repository(policy_request, monkeypatch):
    root, metadata, requests = policy_request
    monkeypatch.setenv("GITHUB_REPOSITORY", "example/other")
    assert history._integration_policy(root, "origin/main") == metadata
    assert requests[0][0] == "https://api.github.com/repos/example/repository"


def test_ci_policy_requires_destination_identity(policy_request, monkeypatch):
    root, _, requests = policy_request
    monkeypatch.setenv("GITHUB_REF", "refs/pull/17/merge")
    with pytest.raises(history.EvidenceError, match="integration policy"):
        history._integration_policy(root, "origin/main")
    assert requests == []


@pytest.mark.parametrize("remote", [
    "https://github.com/example/repository.git",
    "git" + "@" + "github.com:example/repository.git",
    "ssh://git" + "@" + "github.com/example/repository.git",
])
def test_local_policy_accepts_supported_origin_urls(policy_request, monkeypatch, remote):
    root, metadata, _ = policy_request
    monkeypatch.setattr(history, "_git", lambda *args: remote.encode("utf-8"))
    assert history._integration_policy(root, "origin/main") == metadata


@pytest.mark.parametrize("remote", [
    "https://example.invalid/example/repository.git",
    "https://github.com/example/../repository",
    "https://github.com/example/repository?query=other",
    "file:repository",
])
def test_local_policy_rejects_untrusted_origin_urls(policy_request, monkeypatch, remote):
    root, _, requests = policy_request
    monkeypatch.setattr(history, "_git", lambda *args: remote.encode("utf-8"))
    with pytest.raises(history.EvidenceError, match="integration policy"):
        history._integration_policy(root, "origin/main")
    assert requests == []


@pytest.mark.parametrize("ci_merge", [False, True])
def test_regeneration_selection_survives_retention_integration(repository, monkeypatch, ci_merge):
    case = repository("retained")
    if ci_merge:
        local_ci_merge(case, monkeypatch, event=True)
    row = copy.deepcopy(case["row"])
    source = row.pop("source_revision")
    row["source_publication"] = "first-matching-artifact-publication"
    result = history.verify_git(row, case["root"])
    assert result["status"] == "VERIFIED"
    assert result["fingerprint"] == row["content_fingerprint"]
    assert result["source_revision"] == source
    assert result["working_tree_drift"] == []


def test_regeneration_selection_cannot_bypass_pinned_retention(repository):
    case = repository("unchanged")
    row = copy.deepcopy(case["row"])
    row["source_publication"] = "first-matching-artifact-publication"
    with pytest.raises(history.EvidenceError, match="Ambiguous source publication"):
        history.verify_git(row, case["root"])
