"""Historical provenance uses retained bytes, never the live response code."""

from __future__ import annotations

import copy
import hashlib
import json
import subprocess
import sys
from pathlib import Path

import pytest

from digitalmodel.parametric import historical_provenance as history

ROOT = Path(__file__).resolve().parents[2]
INDEX = ROOT / "atlases" / "historical-provenance.json"


def test_independent_raw_fingerprint_vector():
    """Hand construction of the legacy refresh.py hash, not a response solve.

    Source stream = UTF-8(path) || raw bytes in declared order. Input hash =
    SHA256(raw input). Final hash = SHA256(sorted-key JSON of the five fields).
    The toy bytes and empty standards are test inputs, not engineering values.
    """
    spec = {"physics": "linear", "response": "value", "axes": []}
    basis = {
        "spec": spec,
        "response_kwargs": {},
        "standards": [],
        "input_sha256": hashlib.sha256(b"input\r\n").hexdigest(),
        "source_sha256": hashlib.sha256(b"a.pyx\r\nb.pyy\n").hexdigest(),
    }
    expected = hashlib.sha256(json.dumps(basis, sort_keys=True).encode()).hexdigest()
    assert history.legacy_fingerprint(
        spec, {}, [], b"input\r\n", [("a.py", b"x\r\n"), ("b.py", b"y\n")]
    ) == expected


@pytest.fixture(scope="module")
def inventory():
    return json.loads(INDEX.read_text(encoding="utf-8"))


def test_inventory_covers_every_computed_manifest(inventory):
    import yaml

    expected = set()
    for path in (ROOT / "atlases").glob("*/*/manifest.yaml"):
        provenance = yaml.safe_load(path.read_text(encoding="utf-8"))["provenance"]
        if provenance.get("code_version") and provenance.get("content_fingerprint"):
            expected.add(path.relative_to(ROOT).as_posix())
    assert {row["manifest"] for row in inventory["atlases"]} == expected


def test_all_retained_bases_reconstruct(inventory):
    for row in inventory["atlases"]:
        result = history.verify_git(row, ROOT, retained_ref="origin/main")
        assert result["status"] == row["status"], row["manifest"]
        if result["status"] == "VERIFIED":
            assert result["fingerprint"] == row["content_fingerprint"]
        else:
            assert result["reason"] == row["reason"]


def test_historical_viv_standards_are_not_current_defaults(inventory):
    row = next(r for r in inventory["atlases"] if "/862a28080fc9/" in r["manifest"])
    assert row["basis"]["standards"] == [{"id": "DNV-RP-F105", "edition": "2017-06"}]
    assert history.verify_git(row, ROOT)["status"] == "VERIFIED"


@pytest.fixture
def retained_record(inventory):
    return copy.deepcopy(next(r for r in inventory["atlases"] if r["status"] == "VERIFIED"))


def test_archive_reconstruction_without_git(retained_record, tmp_path):
    row = retained_record
    reader = history.git_reader(ROOT, row["source_revision"])
    for name in row["sources"]:
        path = tmp_path / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes(reader(name))
    assert history.verify_sources(row, history.archive_reader(tmp_path)) == row["content_fingerprint"]


@pytest.mark.parametrize("damage", ["missing", "changed", "crlf", "symlink"])
def test_archive_rejects_unavailable_or_mismatched_bytes(retained_record, tmp_path, damage):
    row = retained_record
    reader = history.git_reader(ROOT, row["source_revision"])
    for name in row["sources"]:
        path = tmp_path / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes(reader(name))
    name = next(iter(row["sources"]))
    path = tmp_path / name
    if damage == "missing":
        # Exercise an absent archive without deleting retained evidence.
        archive = tmp_path / "absent"
    elif damage == "symlink":
        archive = tmp_path / "redirected"
        archive.symlink_to(tmp_path, target_is_directory=True)
    else:
        archive = tmp_path
        path.write_bytes(path.read_bytes() + (b"\r\n" if damage == "crlf" else b"changed"))
    with pytest.raises(history.EvidenceError):
        history.verify_sources(row, history.archive_reader(archive))


@pytest.mark.parametrize("field", ["source_revision", "content_fingerprint", "basis", "sources"])
def test_git_rejects_mismatched_evidence(retained_record, field):
    row = retained_record
    if field == "source_revision":
        row[field] = "0" * 40  # Synthetic unavailable object ID, not an engineering limit.
    elif field == "content_fingerprint":
        row[field] = "0" * 64
    elif field == "basis":
        row[field]["standards"] = [{"id": "unavailable", "edition": "unknown"}]
    else:
        row[field].pop(next(iter(row[field])))
    with pytest.raises(history.EvidenceError):
        history.verify_git(row, ROOT)


def test_git_rejects_unretained_revision(retained_record):
    with pytest.raises(history.EvidenceError, match="retained"):
        history.verify_git(retained_record, ROOT, retained_ref="missing-retention-ref")


@pytest.fixture
def regeneration_record(retained_record):
    import yaml

    row = retained_record
    read = history.git_reader(ROOT, row.pop("source_revision"))
    row["artifacts"] = {name: history.sha256(read(name)) for name in row["artifacts"]}
    row["recorded_code_version"] = yaml.safe_load(read(row["manifest"]))["provenance"]["code_version"]
    row["source_publication"] = "first-matching-artifact-publication"
    return row


def test_regeneration_resolves_matching_committed_publication(regeneration_record):
    row = regeneration_record
    revision = history._regeneration_revision(row, ROOT)
    assert len(revision) == 40
    history.verify_artifacts(row, ROOT, revision=revision)
    assert history.verify_sources(row, history.git_reader(ROOT, revision)) == row["content_fingerprint"]


@pytest.mark.parametrize("field", ["sources", "artifacts", "content_fingerprint"])
def test_regeneration_rejects_unpublished_or_changed_evidence(regeneration_record, field):
    row = regeneration_record
    if field == "content_fingerprint":
        row[field] = "0" * 64
    else:
        row[field][next(iter(row[field]))] = "0" * 64
    with pytest.raises(history.EvidenceError, match="No committed publication matches"):
        history.verify_git(row, ROOT)


def test_regeneration_selection_cannot_replace_pinned_revision(retained_record):
    row = retained_record
    row["source_publication"] = "first-matching-artifact-publication"
    with pytest.raises(history.EvidenceError, match="Ambiguous source publication"):
        history.verify_git(row, ROOT)


@pytest.mark.parametrize("path", ["../escape", "/absolute", "a/../b", "a\\b"])
def test_archive_paths_are_relative_and_unredirected(path, tmp_path):
    with pytest.raises(history.EvidenceError):
        history.archive_reader(tmp_path)(path)


def test_archive_allows_symlink_above_root(tmp_path):
    parent = tmp_path / "parent"
    root = parent / "archive"
    root.mkdir(parents=True)
    (root / "evidence.txt").write_bytes(b"retained bytes\r\n")
    linked_parent = tmp_path / "linked-parent"
    linked_parent.symlink_to(parent, target_is_directory=True)
    assert history.archive_reader(linked_parent / "archive")("evidence.txt") == b"retained bytes\r\n"


@pytest.mark.parametrize("location", ["root", "directory", "file"])
def test_archive_rejects_links_at_or_below_root(tmp_path, location):
    original = tmp_path / "original"
    original.mkdir()
    (original / "evidence.txt").write_bytes(b"retained bytes\n")
    root = tmp_path / "archive"
    if location == "root":
        root.symlink_to(original, target_is_directory=True)
        name = "evidence.txt"
    else:
        root.mkdir()
        if location == "directory":
            (root / "nested").symlink_to(original, target_is_directory=True)
            name = "nested/evidence.txt"
        else:
            (root / "evidence.txt").symlink_to(original / "evidence.txt")
            name = "evidence.txt"
    with pytest.raises(history.EvidenceError, match="Redirected evidence"):
        history.archive_reader(root)(name)


@pytest.mark.parametrize("location", ["root", "directory"])
def test_archive_rejects_resolved_redirection(tmp_path, monkeypatch, location):
    """Model a directory junction on platforms without native junctions."""
    root = tmp_path / "archive"
    redirected = root if location == "root" else root / "nested"
    redirected.mkdir(parents=True)
    (redirected / "evidence.txt").write_bytes(b"external evidence\n")
    target = tmp_path / "external"
    target.mkdir()
    resolve = Path.resolve

    def resolve_redirected(path, *args, **kwargs):
        if path == redirected or redirected in path.parents:
            path = target / path.relative_to(redirected)
        return resolve(path, *args, **kwargs)

    monkeypatch.setattr(Path, "resolve", resolve_redirected)
    name = "evidence.txt" if location == "root" else "nested/evidence.txt"
    with pytest.raises(history.EvidenceError, match="Redirected evidence"):
        history.archive_reader(root)(name)


@pytest.mark.skipif(sys.platform != "win32", reason="Requires Windows directory junctions")
@pytest.mark.parametrize("location", ["root", "directory"])
def test_archive_rejects_windows_junctions(tmp_path, location):
    target = tmp_path / "external"
    target.mkdir()
    (target / "evidence.txt").write_bytes(b"external evidence\n")
    root = tmp_path / "archive"
    if location == "root":
        junction = root
        name = "evidence.txt"
    else:
        root.mkdir()
        junction = root / "nested"
        name = "nested/evidence.txt"
    subprocess.run(
        ["cmd", "/c", "mklink", "/J", str(junction), str(target)],
        check=True, capture_output=True,
        creationflags=getattr(subprocess, "CREATE_NO_WINDOW", 0),
    )
    assert not junction.is_symlink()
    with pytest.raises(history.EvidenceError, match="Redirected evidence"):
        history.archive_reader(root)(name)


def test_live_artifact_tampering_is_rejected(retained_record, tmp_path):
    row = retained_record
    for name in row["artifacts"]:
        path = tmp_path / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes((ROOT / name).read_bytes())
    manifest = tmp_path / row["manifest"]
    manifest.write_bytes(manifest.read_bytes() + b"# changed\n")
    with pytest.raises(history.EvidenceError, match="digest"):
        history.verify_artifacts(row, tmp_path)


@pytest.fixture(params=["VERIFIED", "NOT_EVALUATED"])
def artifact_checkout(inventory, tmp_path, monkeypatch, request):
    """Share read-only Git objects while keeping artifact edits temporary."""
    for key in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"):
        monkeypatch.delenv(key, raising=False)
    # Explicit defaults make raw-CRLF drift tests independent of global config.
    monkeypatch.setenv("GIT_CONFIG_COUNT", "1")
    monkeypatch.setenv("GIT_CONFIG_KEY_0", "core.autocrlf")
    monkeypatch.setenv("GIT_CONFIG_VALUE_0", "false")
    row = copy.deepcopy(next(r for r in inventory["atlases"] if r["status"] == request.param))
    git_dir = subprocess.check_output(
        ["git", "-C", str(ROOT), "rev-parse", "--absolute-git-dir"], text=True,
    ).strip()
    (tmp_path / ".git").write_text(f"gitdir: {git_dir}\n", encoding="utf-8")
    for name in row["artifacts"]:
        path = tmp_path / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes(subprocess.check_output(["git", "-C", str(ROOT), "show", f"HEAD:{name}"]))
    return row, tmp_path


@pytest.mark.parametrize("damage", ["crlf", "changed", "missing", "symlink"])
def test_git_verifies_committed_artifacts_and_reports_checkout_drift(artifact_checkout, damage):
    row, root = artifact_checkout
    if damage == "crlf":
        changed = []
        for name in row["artifacts"]:
            if Path(name).suffix in {".yaml", ".json"}:
                path = root / name
                path.write_bytes(path.read_bytes().replace(b"\n", b"\r\n"))
                changed.append(name)
    else:
        name = row["manifest"]
        changed = [name]
        path = root / name
        if damage == "changed":
            path.write_bytes(path.read_bytes() + b"# changed\n")
        else:
            retained = path.with_name("retained-" + path.name)
            path.rename(retained)
            if damage == "symlink":
                path.symlink_to(retained)
    result = history.verify_git(row, root)
    assert result["status"] == row["status"]
    assert result["working_tree_drift"] == sorted(changed)


def test_git_reports_no_drift_for_exact_checkout(artifact_checkout):
    row, root = artifact_checkout
    result = history.verify_git(row, root)
    assert result["status"] == row["status"]
    assert result["working_tree_drift"] == []


@pytest.mark.parametrize("autocrlf", ["true", "input"])
@pytest.mark.parametrize("edited", [False, True])
def test_git_distinguishes_autocrlf_conversion_from_content_edits(
    artifact_checkout, monkeypatch, edited, autocrlf,
):
    row, root = artifact_checkout
    # Process-local configuration leaves the shared repository config untouched.
    monkeypatch.setenv("GIT_CONFIG_COUNT", "1")
    monkeypatch.setenv("GIT_CONFIG_KEY_0", "core.autocrlf")
    monkeypatch.setenv("GIT_CONFIG_VALUE_0", autocrlf)
    for name in row["artifacts"]:
        if Path(name).suffix in {".yaml", ".json"}:
            path = root / name
            path.write_bytes(path.read_bytes().replace(b"\n", b"\r\n"))
    if edited:
        path = root / row["manifest"]
        path.write_bytes(path.read_bytes() + b"# changed\r\n")
    result = history.verify_git(row, root)
    assert result["status"] == row["status"]
    assert result["working_tree_drift"] == ([row["manifest"]] if edited else [])


def test_git_rejects_committed_artifact_digest_mismatch(artifact_checkout):
    row, root = artifact_checkout
    row["artifacts"][row["manifest"]] = "0" * 64
    with pytest.raises(history.EvidenceError, match="Artifact digest mismatch"):
        history.verify_git(row, root)


def test_wrapper_is_bound_even_when_legacy_hash_omits_it(retained_record):
    row = retained_record
    original = history.git_reader(ROOT, row["source_revision"])

    def changed(name):
        data = original(name)
        return data + b"# changed wrapper\n" if name == history.WRAPPER else data

    with pytest.raises(history.EvidenceError, match="Source digest mismatch"):
        history.verify_sources(row, changed)


def test_missing_wrapper_digest_is_rejected(retained_record):
    row = retained_record
    row["sources"].pop(history.WRAPPER)
    with pytest.raises(history.EvidenceError, match="inventory mismatch"):
        history.verify_sources(row, history.git_reader(ROOT, row["source_revision"]))


def test_unsupported_algorithm_is_rejected(retained_record):
    retained_record["algorithm"] = "unknown"
    with pytest.raises(history.EvidenceError, match="Unsupported"):
        history.verify_sources(retained_record, lambda name: b"")


def test_declared_gap_does_not_hide_source_corruption(inventory):
    row = next(r for r in inventory["atlases"] if r["status"] == "NOT_EVALUATED")
    row = copy.deepcopy(row)
    name = next(iter(row["sources"]))
    row["sources"][name] = "0" * 64
    with pytest.raises(history.EvidenceError, match="Source digest mismatch"):
        history.verify_git(row, ROOT)


@pytest.mark.parametrize("status", ["VERIFIED", "NOT_EVALUATED", "REJECTED"])
def test_cli_reports_status_and_refuses_incomplete_audit(
    retained_record, tmp_path, monkeypatch, capsys, status,
):
    index = tmp_path / "index.json"
    index.write_text(json.dumps({"atlases": [retained_record]}), encoding="utf-8")

    def verify(*args, **kwargs):
        if status == "REJECTED":
            raise history.EvidenceError("Unavailable evidence")
        return {"status": status}

    monkeypatch.setattr(history, "verify_git", verify)
    assert history.main(["--index", str(index)]) == int(status != "VERIFIED")
    assert json.loads(capsys.readouterr().out)["status"] == status


def test_cli_exits_nonzero_on_checkout_content_drift(
    artifact_checkout, monkeypatch, capsys,
):
    row, root = artifact_checkout
    path = root / row["manifest"]
    path.write_bytes(path.read_bytes() + b"# changed\n")
    index = root / "index.json"
    index.write_text(json.dumps({"atlases": [row]}), encoding="utf-8")
    verify = history.verify_git

    def verify_checkout(record, *, retained_ref):
        return verify(record, root, retained_ref=retained_ref)

    monkeypatch.setattr(history, "verify_git", verify_checkout)
    assert history.main(["--index", str(index)]) == 1
    result = json.loads(capsys.readouterr().out)
    assert result["status"] == row["status"]
    assert result["working_tree_drift"] == [row["manifest"]]


def test_cli_reads_utf8_index_under_legacy_default(retained_record, tmp_path, monkeypatch, capsys):
    index = tmp_path / "index.json"
    index.write_text(
        json.dumps({"atlases": [retained_record], "description": "\u00e9vidence"}, ensure_ascii=False),
        encoding="utf-8",
    )
    read_text = Path.read_text

    def legacy_read_text(path, encoding=None, errors=None):
        return read_text(path, encoding=encoding or "ascii", errors=errors)

    monkeypatch.setattr(Path, "read_text", legacy_read_text)
    assert history.main(["--index", str(index)]) == 0
    assert json.loads(capsys.readouterr().out)["status"] == "VERIFIED"


def test_documented_worked_example(monkeypatch):
    document = ROOT / "docs/plans/parametric-refresh/historical-provenance.md"
    text = document.read_text(encoding="utf-8")
    example = text.split("```python\n", 1)[1].split("```", 1)[0]
    monkeypatch.chdir(ROOT)
    namespace = {}
    exec(compile(example, document.name, "exec"), namespace)
    assert namespace["answer"]["fingerprint"] in text
