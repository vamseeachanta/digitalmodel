# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Portable imports and explicit refusal of unsupported descriptor walks."""

from __future__ import annotations

import json
import os
from pathlib import Path
import subprocess
import sys
import textwrap
from types import SimpleNamespace

import pytest

from digitalmodel.solvers.openfoam import artifact_index as index


_DESCRIPTOR_TESTS = {
    "test_select_roots_returns_disjoint_completed_selections",
    "test_field_tree_contains_only_numeric_time_roots",
    "test_select_roots_rejects_duplicate_numeric_time_spellings",
    "test_snapshot_tree_rejects_symlink",
    "test_snapshot_tree_rejects_fifo",
    "test_snapshot_tree_returns_exact_sorted_records",
    "test_verify_unchanged_detects_same_size_modification",
    "test_verify_unchanged_detects_added_file",
    "test_verify_unchanged_detects_removed_file",
    "test_verify_unchanged_detects_rename",
    "test_build_index_emits_no_absolute_host_path",
    "test_completed_index_excludes_processor_and_diagnostic_roots",
}
_PORTABLE_TESTS = {
    "test_frame_prepends_uint64_big_endian_length",
    "test_tree_domain_frame_golden_vector",
    "test_single_record_tree_digest_golden_vector",
    "test_tree_digest_is_independent_of_input_order",
    "test_tree_digest_changes_when_only_size_changes",
    "test_artifact_id_is_stable_and_kind_sensitive",
    "test_host_local_locator_has_exact_opaque_shape",
    "test_numeric_time_name_accepts_canonical_nonnegative_values",
    "test_numeric_time_name_rejects_invalid_values",
}
_CAPABILITIES = [
    ("O_DIRECTORY", None),
    ("O_NOFOLLOW", None),
    ("supports_dir_fd", "open"),
    ("supports_dir_fd", "stat"),
    ("supports_follow_symlinks", "stat"),
    ("supports_fd", "listdir"),
    ("supports_dir_fd", None),
    ("supports_follow_symlinks", None),
    ("supports_fd", None),
]


@pytest.mark.parametrize("attribute,member", _CAPABILITIES)
def test_each_missing_capability_is_unsupported(monkeypatch, attribute, member):
    if member is None:
        monkeypatch.delattr(os, attribute, raising=False)
    else:
        remaining = getattr(os, attribute) - {getattr(os, member)}
        monkeypatch.setattr(os, attribute, remaining)
    assert index.descriptor_walk_supported() is False


@pytest.mark.parametrize("entry", ["snapshot_tree", "verify_unchanged", "select_roots", "build_index"])
def test_unsupported_entry_rejects_before_input_access(monkeypatch, entry):
    def touched(*args, **kwargs):
        pytest.fail("unsupported operation inspected its input")

    monkeypatch.setattr(index, "descriptor_walk_supported", lambda: False, raising=False)
    monkeypatch.setattr(index, "os", SimpleNamespace(open=touched, stat=touched, listdir=touched))
    if entry == "verify_unchanged":
        monkeypatch.setattr(index, "snapshot_tree", touched)
    if entry == "build_index":
        monkeypatch.setattr(index, "select_roots", touched)
    kwargs = dict.fromkeys(
        ("run_identity_sha256", "input_sha256", "source_sha256", "tool_sha256", "generation_id"),
        "1" * 64,
    ) if entry == "build_index" else {}
    args = (object(), []) if entry == "verify_unchanged" else (object(),)
    with pytest.raises(index.ArtifactIndexError, match="descriptor"):
        getattr(index, entry)(*args, **kwargs)


@pytest.mark.skipif(sys.platform != "linux", reason="positive Linux capability sentinel")
def test_linux_support_is_positive_and_walks_real_tree(tmp_path):
    assert index.descriptor_walk_supported() is True
    (tmp_path / "field").write_bytes(b"x")
    records = index.snapshot_tree(tmp_path)
    assert records == [index.FileRecord("field", 1, index.sha256(b"x").hexdigest())]
    index.verify_unchanged(tmp_path, records)


_CHILD = """
import importlib.abc
import json
import os
import sys
import pytest

class NoLicensedImports(importlib.abc.MetaPathFinder):
    def find_spec(self, fullname, path=None, target=None):
        if fullname == 'OrcFxAPI' or fullname.startswith('ansys.mapdl'):
            raise AssertionError('licensed API import forbidden')

sys.meta_path.insert(0, NoLicensedImports())
for flag in json.loads(sys.argv[1]):
    if hasattr(os, flag):
        delattr(os, flag)
from digitalmodel.solvers.openfoam import artifact_index as index
assert index.descriptor_walk_supported() is False
record = index.FileRecord('0/U', 1, index.sha256(b'x').hexdigest())
assert index.frame(b'x') == bytes(7) + b'\\x01x'
assert index.tree_digest([record]) == '15749b5cd2b684b587d2d7da29accbe736aeb6736d6afa2a0500fe5fd01e6c73'
assert index.is_numeric_time_name('0.5') is True
assert index.is_numeric_time_name('nan') is False
assert len(index.artifact_id(run_identity_sha256='1'*64, generation_id='2'*64,
    kind='field_tree', selection='0', size_bytes=1, file_count=1,
    content_sha256=record.sha256)) == 64
for entry in (index.snapshot_tree, index.select_roots):
    try:
        entry(object())
    except index.ArtifactIndexError:
        pass
    else:
        raise AssertionError('unsupported walk accepted')

class Results:
    def __init__(self):
        self.collected, self.passed, self.skipped = [], [], []
    def pytest_collection_finish(self, session):
        self.collected = [item.name for item in session.items]
    def pytest_runtest_logreport(self, report):
        if report.when == 'call' and report.passed:
            self.passed.append(report.nodeid.rsplit('::', 1)[-1])
        if report.skipped:
            self.skipped.append(report.nodeid.rsplit('::', 1)[-1])

results = Results()
args = [sys.argv[2], '-q', '-p', 'no:cacheprovider', '--basetemp', sys.argv[3]]
if sys.argv[4] == 'collect':
    args.append('--collect-only')
code = pytest.main(args, plugins=[results])
print(json.dumps(vars(results)))
sys.exit(code)
"""


@pytest.mark.parametrize("flags", [("O_DIRECTORY",), ("O_NOFOLLOW",), ("O_DIRECTORY", "O_NOFOLLOW")])
@pytest.mark.parametrize("mode", ["collect", "execute"])
def test_missing_flags_in_fresh_process(flags, mode, tmp_path):
    repo = Path(__file__).resolve().parents[3]
    env = os.environ.copy()
    env["PYTHONPATH"] = os.pathsep.join((str(repo / "src"), env.get("PYTHONPATH", "")))
    env["PYTHONDONTWRITEBYTECODE"] = "1"
    command = [sys.executable, "-c", textwrap.dedent(_CHILD), json.dumps(flags),
               "tests/solvers/openfoam/test_artifact_index.py", str(tmp_path / "child"), mode]
    result = subprocess.run(
        command, cwd=repo, env=env, capture_output=True, text=True, timeout=120,
        creationflags=subprocess.CREATE_NO_WINDOW if os.name == "nt" else 0,
    )
    assert result.returncode == 0, result.stdout + result.stderr
    reports = json.loads(result.stdout.splitlines()[-1])
    assert set(reports["collected"]) == _PORTABLE_TESTS | _DESCRIPTOR_TESTS
    if mode == "execute":
        assert set(reports["passed"]) == _PORTABLE_TESTS
        assert set(reports["skipped"]) == _DESCRIPTOR_TESTS
