"""Exercise the frozen migration through its public reader, without the archive."""
import hashlib
import json
import os
import shutil
import subprocess
from pathlib import Path

import pytest

from digitalmodel import legacy_evidence as evidence

ROOT = Path(__file__).resolve().parents[2]


def test_manifest_is_frozen(tmp_path):
    target = tmp_path / evidence.MANIFEST
    target.parent.mkdir(parents=True)
    target.write_bytes((ROOT / evidence.MANIFEST).read_bytes() + b' ')
    with pytest.raises(ValueError, match="manifest digest"):
        evidence.legacy_files(tmp_path, "a" * 40)


def test_stored_atlas_contains_only_the_published_producer():
    revision = "c12624942c025e3d68182fa157cb479616a95f5d"
    path = "scripts/python/digitalmodel/ocimf/build_coefficient_explorer.py"
    entry = evidence.legacy_files(ROOT, revision)[path]
    assert entry["storage"] == "redacted-blob"
    assert entry["archived_blob"] == "08e84f7083ca298b914dc5396d3f182f10a1f44e"
    with pytest.raises(ValueError, match="redacted"):
        evidence.read_evidence(ROOT, revision, path)
    source = evidence.read_evidence(ROOT, revision, path, allow_redacted=True)
    assert hashlib.sha256(source).hexdigest() == "c37be3500043b51731ca59680ad07def74b91288123875197e410ae970234849"

    manifest = json.loads((ROOT / evidence.MANIFEST).read_bytes())
    published = {entry["sha256"] for paths in manifest["files"].values() for entry in paths.values()
                 if entry["storage"] != "same-as-root"}
    store = (ROOT / evidence.MANIFEST).parent / "blobs"
    assert {blob.name for blob in store.iterdir()} == published


def test_archived_ref_evidence_uses_publication_refs():
    manifest = json.loads((ROOT / evidence.MANIFEST).read_bytes())
    for refs in manifest["archived_refs"].values():
        assert refs
        assert all(not ref.startswith("refs/heads/backup/") for ref in refs)


def test_exists_claim_is_limited_to_the_cited_revision_and_path():
    revision = "3256bab95f84da3d9fadce8d5dcf8458d66d1eb4"
    path = "docs/registry/implementation-map.yaml"
    manifest = json.loads((ROOT / evidence.MANIFEST).read_bytes())
    assert manifest["files"][revision][path]["rule"] == "exists"
    assert manifest["archived_refs"][revision] == [
        "refs/heads/codex/implmap-work", "refs/pull/2545/head",
    ]
    assert evidence.read_evidence(ROOT, revision, path)
    # Archived main contains the identical blob at this unlisted commit.
    with pytest.raises(ValueError, match="No frozen evidence"):
        evidence.read_evidence(ROOT, "38d3f79a2efc4796e95ef932f8e224873b7398a8", path)
    with pytest.raises(ValueError, match="No frozen evidence"):
        evidence.read_evidence(ROOT, revision, "README.md")


def test_corrupt_stored_bytes_are_rejected(tmp_path):
    manifest = ROOT / evidence.MANIFEST
    records = json.loads(manifest.read_bytes())["files"]
    revision, path, entry = next(
        (rev, path, entry) for rev, paths in records.items() for path, entry in paths.items()
        if entry["storage"] == "blob"
    )
    target = tmp_path / evidence.MANIFEST
    target.parent.mkdir(parents=True)
    shutil.copyfile(manifest, target)
    blob = target.parent / "blobs" / entry["sha256"]
    blob.parent.mkdir()
    blob.write_bytes(b"damaged evidence")
    with pytest.raises(ValueError, match="digest mismatch"):
        evidence.read_legacy(tmp_path, revision, path)


def test_root_storage_checks_pinned_bytes_and_exact_path(monkeypatch):
    revision = "b4c10284fa7d28398e6fa02c262c219c0fec8fef"
    path = "src/digitalmodel/materials/grades.py"
    entry = evidence.legacy_files(ROOT, revision)[path]
    assert entry["storage"] == "same-as-root"
    with pytest.raises(ValueError, match="No frozen evidence"):
        evidence.read_legacy(ROOT, revision, "README.md")
    monkeypatch.setattr(evidence, "git_output", lambda *args: b"corrupt object")
    with pytest.raises(ValueError, match="digest mismatch"):
        evidence.read_legacy(ROOT, revision, path)
    monkeypatch.setitem(entry, "mode", "120000")
    with pytest.raises(ValueError, match="Missing regular file"):
        evidence.read_legacy(ROOT, revision, path)


@pytest.mark.parametrize("rule", ["exists", "main-ancestry"])
def test_new_sources_require_main_ancestry(tmp_path, monkeypatch, rule):
    from digitalmodel.parametric.historical_provenance import EvidenceError, git_reader

    env = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}

    def git(*args):
        return subprocess.check_output(
            ["git", "-c", "user.name=Fixture", "-c", "user.email=fixture@example.invalid",
             "-c", "commit.gpgsign=false", "-C", str(tmp_path), *args], text=True, env=env,
        ).strip()

    git("init", "-q", "-b", "main")
    (tmp_path / "source").write_bytes(b"published")
    (tmp_path / "uncited").write_bytes(b"published")
    git("add", "source", "uncited")
    git("commit", "-qm", "published")
    assert evidence.read_evidence(tmp_path, git("rev-parse", "HEAD"), "source") == b"published"
    git("checkout", "-qb", "topic")
    git("commit", "--allow-empty", "-qm", "unmerged")
    with pytest.raises(ValueError, match="main"):
        evidence.read_evidence(tmp_path, git("rev-parse", "HEAD"), "source")
    with pytest.raises(ValueError):
        evidence.read_evidence(tmp_path, "0" * 40, "source")

    # A side branch can retain a pre-reset object outside current main.
    listed = git("rev-parse", "HEAD")
    entry = {"rule": rule, "storage": "same-as-root", "mode": "100644",
             "blob": git("rev-parse", "HEAD:source"),
             "sha256": hashlib.sha256(b"published").hexdigest()}
    monkeypatch.setattr(evidence, "_manifest", lambda root: {
        "files": {listed: {"source": entry}},
    })
    git("checkout", "-q", "main")
    (tmp_path / "source").write_bytes(b"revised after publication")
    git("commit", "-qam", "revise source")
    assert evidence.read_evidence(tmp_path, listed, "source") == b"published"
    with pytest.raises(ValueError, match="main"):
        evidence.read_evidence(tmp_path, listed, "uncited")
    if rule == "main-ancestry":
        assert git_reader(tmp_path, listed)("source") == b"published"
        with pytest.raises(EvidenceError):
            git_reader(tmp_path, listed)("uncited")
    else:
        with pytest.raises(EvidenceError):
            git_reader(tmp_path, listed)("source")
    git("checkout", "-q", "topic")
    git("commit", "--allow-empty", "-qm", "unlisted with identical tree")
    with pytest.raises(ValueError, match="main"):
        evidence.read_evidence(tmp_path, git("rev-parse", "HEAD"), "source")
