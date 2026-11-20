# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent value checks for repository-only provenance evidence."""
from __future__ import annotations

import hashlib
import importlib.util
import json
import os
from pathlib import Path
import subprocess

import pytest

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/riser_capability/build_orcaflex_riser_provenance_evidence.py"
R = "docs/domains/orcaflex/risers/"
E = "docs/domains/orcaflex/examples/"
L = "docs/domains/orcaflex/library/"


@pytest.fixture
def builder():
    spec = importlib.util.spec_from_file_location("provenance_builder", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def git(root, *args, input=None):
    env = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
    return subprocess.run(["git", "-C", str(root), *args], input=input,
                          stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                          check=True, env=env).stdout


def snapshot(root, files, parent=None):
    """Build disposable history without the checkout's index or configuration."""
    git(root, "init", "--quiet")
    stream = b"commit refs/heads/fixture\ncommitter Fixture <fixture> 1700000000 +0000\n"
    subject = b"Synthetic repository import"
    stream += b"data " + str(len(subject)).encode() + b"\n" + subject + b"\n"
    if parent:
        stream += b"from " + parent.encode() + b"\n"
    for path, data in files.items():
        stream += (b"M 100644 inline " + json.dumps(path, ensure_ascii=False).encode("utf-8", "surrogateescape") + b"\ndata "
                   + str(len(data)).encode() + b"\n" + data + b"\n")
    git(root, "fast-import", "--quiet", input=stream + b"\n")
    return git(root, "rev-parse", "refs/heads/fixture").decode().strip()


def test_inventory_tracks_every_yaml_and_only_the_pinned_blobs(builder, tmp_path, monkeypatch):
    files = {R + name + "/model.yml": b"General: {UnitsSystem: SI}\n"
             for name in builder.FOLDERS}
    files[R + "production/config.yaml"] = b"basename: analysis\n"
    files[R + "production/notes.txt"] = b"not a model"
    first = snapshot(tmp_path, files)
    snapshot(tmp_path, {R + "production/model.yml": b"changed: true\n"}, first)
    monkeypatch.setenv("GIT_DIR", str(tmp_path / "unavailable"))
    monkeypatch.setenv("GIT_WORK_TREE", str(tmp_path / "unavailable"))
    monkeypatch.setenv("GIT_COMMON_DIR", str(tmp_path / "unavailable"))
    data = builder.build(tmp_path, first)
    rows = {r["path"]: r for r in data["records"]}
    assert set(rows) == {p for p in files if p.endswith((".yml", ".yaml"))}
    for path, row in rows.items():
        assert row["sha256"] == hashlib.sha256(files[path]).hexdigest()
        assert row["bytes"] == len(files[path])
        assert row["first_appearance"]["commit"] == first
        assert row["first_appearance"]["date"] == "2023-11-14T22:13:20+00:00"
        assert row["first_appearance"]["subject"]["sha256"] == hashlib.sha256(
            b"Synthetic repository import").hexdigest()
        assert row["proposed_class"] == "origin not established"
    assert rows[R + "production/config.yaml"]["file_kind"] == "analysis configuration"


def test_public_byte_and_normalized_matches_are_reproducible(builder, tmp_path):
    a = b"General: {UnitsSystem: SI}\nTitle: generic\n"
    b = b"# comment\nTitle: generic\nGeneral:\n  UnitsSystem: SI\n"
    files = {R + "production/model.yml": a, E + "generic.yml": a,
             E + "nested/reordered.yaml": b, L + "copy.yml": a}
    rev = snapshot(tmp_path, files)
    row = builder.build(tmp_path, rev)["records"][0]
    assert row["proposed_class"] == "matches public example"
    assert row["example_matches"] == [
        {"path": E + "generic.yml", "kind": "byte and normalized"},
        {"path": E + "nested/reordered.yaml", "kind": "normalized"}]
    assert row["library_matches"] == [{"path": L + "copy.yml", "kind": "byte and normalized"}]
    # Independent canonical representation for this literal mapping.
    scalar = lambda v: ["tag:yaml.org,2002:str", v]
    mapping = "tag:yaml.org,2002:map"
    expected = [mapping, [[scalar("General"), [mapping, [[scalar("UnitsSystem"), scalar("SI")]]]],
                          [scalar("Title"), scalar("generic")]]]
    encoded = json.dumps(expected, ensure_ascii=True, separators=(",", ":")).encode()
    assert row["normalized_sha256"] == hashlib.sha256(encoded).hexdigest()


@pytest.mark.parametrize("other", [b"x: 2", b"x: '1'", b"x: true", b"x: [2, 1]"])
def test_values_types_and_sequence_order_are_not_erased(builder, other):
    original = b"x: [1, 2]" if b"[" in other else b"x: 1"
    assert builder.normalize(original)["sha256"] != builder.normalize(other)["sha256"]


@pytest.mark.parametrize("raw", [b"x: 1\nx: 2", b"x: !unsupported text",
                                    b"x: &cycle [*cycle]", b"x: [", b"x: !!set {a: null}",
                                    b"a: 1\ntrue: 2\n1: 3", b"title: \xb0", b"",
                                    b"x: &base {a: 1}\ny: {<<: *base}"])
def test_unavailable_normalization_is_explicit(builder, raw):
    result = builder.normalize(raw)
    assert result["sha256"] is None
    assert result["status"].startswith("unavailable:")


def test_library_only_and_invalid_yaml_are_never_public(builder, tmp_path):
    raw = b"x: 1\nx: 2\n"
    rev = snapshot(tmp_path, {R + "drilling/config.yml": raw, L + "copy.yml": raw})
    row = builder.build(tmp_path, rev)["records"][0]
    assert row["proposed_class"] == "origin not established"
    assert row["example_matches"] == []
    assert row["library_matches"] == [{"path": L + "copy.yml", "kind": "byte"}]
    assert row["normalization_status"].startswith("unavailable:")
    assert row["missing_evidence"]


def test_text_is_located_and_withheld_instead_of_published(builder, tmp_path):
    local = tmp_path / "private" / "model.yml"
    raw = f"# Source: {local}\nGeneral:\n  Title: identifying text\nTitle: another title\n".encode()
    rev = snapshot(tmp_path, {R + "production/model.yml": raw})
    row = builder.build(tmp_path, rev)["records"][0]
    assert {x["locator"] for x in row["source_text"]} == {"lines 1-1", "General", "Title"}
    assert all(x["status"].startswith("withheld:") and len(x["sha256"]) == 64
               for x in row["source_text"])
    rendered = json.dumps(row)
    assert str(local) not in rendered and "identifying text" not in rendered


@pytest.mark.parametrize("output_name", ["JSON_OUTPUT", "MD_OUTPUT"])
def test_check_detects_missing_and_changed_outputs_without_writing(builder, tmp_path, output_name):
    rev = snapshot(tmp_path, {R + "production/model.yml": b"x: 1\n"})
    data = builder.build(tmp_path, rev)
    assert not builder.write_outputs(tmp_path, data, check=True)
    assert not (tmp_path / builder.JSON_OUTPUT).exists()
    assert builder.write_outputs(tmp_path, data)
    assert builder.write_outputs(tmp_path, data, check=True)
    output = tmp_path / getattr(builder, output_name)
    output.write_bytes(b"drift\n")
    before = output.read_bytes()
    assert not builder.write_outputs(tmp_path, data, check=True)
    assert output.read_bytes() == before


def test_shallow_history_and_missing_revision_fail_closed(builder, tmp_path):
    rev = snapshot(tmp_path, {R + "production/model.yml": b"x: 1"})
    with pytest.raises(subprocess.CalledProcessError):
        builder.build(tmp_path, "refs/heads/unavailable")
    (tmp_path / ".git/shallow").write_text(rev + "\n")
    with pytest.raises(ValueError, match="shallow history"):
        builder.build(tmp_path, rev)


def test_repository_rows_matches_and_input_bytes_are_reproducible(builder):
    paths = git(ROOT, "ls-tree", "-r", "--name-only", "-z", "HEAD", "--",
                *(R + name for name in builder.FOLDERS)).split(b"\0")
    candidates = {p.decode() for p in paths if p.endswith((b".yml", b".yaml"))}
    before = {p: hashlib.sha256((ROOT / p).read_bytes()).hexdigest() for p in candidates}
    data = builder.build(ROOT, "HEAD")
    assert {r["path"] for r in data["records"]} == candidates
    for row in data["records"]:
        assert row["sha256"] == before[row["path"]]
        for field in ("example_matches", "library_matches"):
            for match in row[field]:
                blob = git(ROOT, "show", "HEAD:" + match["path"])
                if "byte" in match["kind"]:
                    assert hashlib.sha256(blob).hexdigest() == row["sha256"]
                if "normalized" in match["kind"]:
                    assert builder.normalize(blob)["sha256"] == row["normalized_sha256"]
    assert before == {p: hashlib.sha256((ROOT / p).read_bytes()).hexdigest() for p in candidates}
    assert builder.write_outputs(ROOT, data, check=True)


def test_byte_safe_paths_remain_enumerated_and_renderable(builder, tmp_path):
    path = R + "production/space " + bytes([128]).decode("utf-8", "surrogateescape") + ".yml"
    rev = snapshot(tmp_path, {path: b"x: 1"})
    data = builder.build(tmp_path, rev)
    assert data["records"][0]["path"] == path
    assert builder.write_outputs(tmp_path, data)
    stored = json.loads((tmp_path / builder.JSON_OUTPUT).read_text())
    assert stored["records"][0]["path"] == path


def test_outputs_cannot_escape_the_requested_repository(builder, tmp_path):
    root = tmp_path / "repo"
    root.mkdir()
    rev = snapshot(root, {R + "production/model.yml": b"x: 1"})
    data = builder.build(root, rev)
    target = tmp_path / "external"
    target.mkdir()
    (root / "docs").symlink_to(target, target_is_directory=True)
    with pytest.raises(ValueError, match="output path"):
        builder.write_outputs(root, data)
    assert list(target.iterdir()) == []
