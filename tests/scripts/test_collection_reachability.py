# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Exercise collection observation using disposable, solver-free trees."""

import hashlib
import importlib.util
import json
from pathlib import Path, PureWindowsPath
import shutil
import subprocess
import sys

import pytest


TOOL = Path(__file__).resolve().parents[2] / "scripts/ci/collection_reachability.py"


def write(root, name, text):
    path = root / name
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8")


@pytest.fixture
def tree(tmp_path):
    root = tmp_path / "fixture"
    root.mkdir()
    write(root, "pytest.ini", "[pytest]\ntestpaths = tests\n")
    write(root, "tests/control/test_control.py", "def test_control(): pass\n")
    write(root, "conftest.py", '''import sys

def deny_external(event, args):
    if event.startswith("socket."):
        raise RuntimeError("network prohibited in synthetic collection")
    if event == "import" and args[0].startswith(("OrcFxAPI", "ansys")):
        raise RuntimeError("licensed binding prohibited in synthetic collection")

sys.addaudithook(deny_external)
''')
    return root


def snapshot(root):
    return {
        path.relative_to(root).as_posix(): hashlib.sha256(path.read_bytes()).hexdigest()
        for path in root.rglob("*") if path.is_file() and not path.is_symlink()
    }


def observe(root, **options):
    output = options.pop("output", root.parent / "report.json")
    command = [sys.executable, str(TOOL), "--root", str(root),
               "--boundary", str(options.pop("boundary", root.parent)),
               "--output", str(output)]
    for name, value in options.items():
        command.extend(["--" + name, str(value)])
    result = subprocess.run(command, capture_output=True, text=True, timeout=20)
    assert output.is_file(), result.stderr
    report = json.loads(output.read_text(encoding="utf-8"))
    assert str(root.parent) not in json.dumps(report)
    return result, report


def test_reachable_tree_produces_bound_evidence_without_mutation(tree):
    before = snapshot(tree)
    result, report = observe(tree)
    assert result.returncode == 0, report
    assert report["status"] == "pass"
    assert report["candidates"] == ["tests/control"]
    assert report["missing"] == []
    assert report["nodes"] == [{"nodeid": "tests/control/test_control.py::test_control",
                                "path": "tests/control/test_control.py"}]
    assert report["configuration"]["testpaths"] == ["tests"]
    assert report["source_sha256"] == report["source_sha256_after"]
    assert len(report["source_sha256"]) == 64
    assert snapshot(tree) == before


@pytest.mark.parametrize("source,options", [
    ("def test_control(): pass\n", {}),
    ("import missing_synthetic_dependency\n", {}),
    ("import os\nos._exit(0)\n", {}),
    ("import time\ntime.sleep(2)\n", {"timeout": 0.5}),
])
def test_child_scratch_is_removed_after_collection(tree, source, options):
    write(tree, "tests/control/test_control.py", source)
    observe(tree, **options)
    assert not list(tree.parent.glob("collection-*"))


@pytest.mark.parametrize("mechanism,config,extra", [
    ("ignore", "", "collect_ignore = ['tests/hidden']\n"),
    ("glob", "", "collect_ignore_glob = ['tests/hid*']\n"),
    ("path-pattern", "norecursedirs = tests/hidden\n", ""),
    ("testpaths", "testpaths = tests/control\n", ""),
])
def test_hidden_directory_is_named(tree, mechanism, config, extra):
    write(tree, "tests/hidden/test_hidden.py", "def test_hidden(): pass\n")
    write(tree, "pytest.ini", "[pytest]\n" + config)
    with (tree / "conftest.py").open("a", encoding="utf-8") as stream:
        stream.write(extra)
    result, report = observe(tree)
    assert result.returncode == 1
    assert report["missing"] == ["tests/hidden"]
    assert "unreachable_directories" in report["reasons"]


@pytest.mark.parametrize("source,reason,has_error", [
    ("import missing_synthetic_dependency\n", "collection_errors", True),
    ("import pytest\npytest.skip('synthetic', allow_module_level=True)\n",
     "unreachable_directories", False),
])
def test_incomplete_module_cannot_be_called_covered(tree, source, reason, has_error):
    write(tree, "tests/hidden/test_hidden.py", source)
    result, report = observe(tree)
    assert result.returncode == 1
    assert report["missing"] == ["tests/hidden"]
    assert reason in report["reasons"]
    assert bool(report["errors"]) == has_error
    assert bool(report["skips"]) != has_error
    assert report["nodes"]  # A partial collect must still fail.


@pytest.mark.parametrize("empty,reason", [(True, "empty_candidates"),
                                         (False, "zero_nodes")])
def test_vacuous_collection_fails(tree, empty, reason):
    write(tree, "pytest.ini", "[pytest]\npython_files = absent_*.py\n" if empty
          else "[pytest]\npython_functions = absent_*\n")
    result, report = observe(tree)
    assert result.returncode == 1
    assert reason in report["reasons"]


def test_item_paths_survive_duplicate_ids_separators_and_deselection(tree):
    write(tree, "pytest.ini", "[pytest]\naddopts = -k absent\n")
    write(tree, "tests/control/test_control.py", '''import pytest
@pytest.mark.parametrize("value", [1, 2], ids=["a/b::c", "a/b::c"])
def test_control(value): pass
''')
    result, report = observe(tree)
    assert result.returncode == 0, report
    assert len(report["nodes"]) == 2
    assert {node["path"] for node in report["nodes"]} == {
        "tests/control/test_control.py"}
    assert report["missing"] == []


def test_repeated_node_ids_are_not_a_count_threshold(tree):
    with (tree / "conftest.py").open("a", encoding="utf-8") as stream:
        stream.write('''
def pytest_itemcollected(item):
    item._nodeid = "duplicate"
''')
    write(tree, "tests/other/test_other.py", "def test_other(): pass\n")
    result, report = observe(tree)
    assert result.returncode == 0, report
    assert len(report["nodes"]) == 2
    assert report["missing"] == []


def test_child_clears_inherited_selection_and_git_bindings(tree, monkeypatch):
    before = snapshot(tree)
    for name in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "PYTEST_ADDOPTS",
                 "PYTEST_PLUGINS", "PYTEST_DEBUG"):
        monkeypatch.setenv(name, "invalid-inherited-binding")
    with (tree / "conftest.py").open("a", encoding="utf-8") as stream:
        stream.write('''
import os
assert not any(key in os.environ for key in (
    "GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "PYTEST_ADDOPTS",
    "PYTEST_PLUGINS", "PYTEST_DEBUG"))
''')
    write(tree, ".git/HEAD", "synthetic metadata sentinel\n")
    original = snapshot(tree)
    result, report = observe(tree)
    assert result.returncode == 0, report
    assert snapshot(tree) == original
    assert before["tests/control/test_control.py"] == original[
        "tests/control/test_control.py"]


@pytest.mark.parametrize("source,reason,options", [
    ("import os\nos._exit(0)\n", "missing_evidence", {}),
    ("import time\ntime.sleep(2)\n", "child_timeout", {"timeout": 0.5}),
])
def test_failed_child_cannot_reuse_stale_output(tree, source, reason, options):
    if reason == "missing_evidence":
        with (tree / "conftest.py").open("a", encoding="utf-8") as stream:
            stream.write("\nimport time\ntime.sleep(0.75)\n")
    output = tree.parent / "report.json"
    output.write_text('{"status":"pass","stale":true}', encoding="utf-8")
    write(tree, "tests/control/test_control.py", source)
    result, report = observe(tree, output=output, **options)
    assert result.returncode == 1
    assert report["status"] == "fail"
    assert reason in report["reasons"]
    assert "stale" not in report


def test_collection_source_mutation_fails(tree):
    write(tree, "tests/control/test_control.py", '''from pathlib import Path
Path(__file__).write_text("def test_changed(): pass\\n")
def test_control(): pass
''')
    result, report = observe(tree)
    assert result.returncode == 1
    assert "source_changed" in report["reasons"]
    assert report["source_sha256"] != report["source_sha256_after"]


@pytest.mark.parametrize("kind", ["directory", "file"])
def test_external_links_are_incomplete_and_never_imported(tree, kind):
    outside = tree.parent / "outside"
    outside.mkdir()
    sentinel = outside / "test_external.py"
    sentinel.write_text("raise RuntimeError('external code imported')\n", encoding="utf-8")
    target = outside if kind == "directory" else sentinel
    link = tree / "tests" / ("linked" if kind == "directory" else "test_link.py")
    link.symlink_to(target, target_is_directory=kind == "directory")
    before = snapshot(outside)
    result, report = observe(tree)
    assert result.returncode == 1
    assert report["incomplete_links"] == [link.relative_to(tree).as_posix()]
    assert "untraversed_links" in report["reasons"]
    assert report["nodes"] == []
    assert snapshot(outside) == before


@pytest.mark.parametrize("which", ["absent", "root-outside", "output-outside",
                                    "output-inside", "linked-output", "timeout"])
def test_invalid_boundary_or_output_is_rejected_without_writes(tree, which):
    boundary, root, output = tree.parent, tree, tree.parent / "report.json"
    options = []
    if which == "absent":
        root = tree.parent / "absent"
    elif which == "root-outside":
        boundary = tree / "tests"
    elif which == "output-outside":
        output = tree.parent.parent / "forbidden.json"
    elif which == "output-inside":
        output = tree / "report.json"
    elif which == "linked-output":
        output.symlink_to(tree / "pytest.ini")
    else:
        options = ["--timeout", "nan"]
    before = snapshot(tree)
    result = subprocess.run([
        sys.executable, str(TOOL), "--root", str(root), "--boundary", str(boundary),
        "--output", str(output), *options,
    ], capture_output=True, text=True, timeout=20)
    assert result.returncode == 2
    assert snapshot(tree) == before
    assert not output.exists() or output.is_relative_to(tree) or output.is_symlink()


@pytest.mark.parametrize("replacement", [
    "not JSON", "{}", '{"completed":false}',
])
def test_malformed_evidence_fails_closed(tree, replacement):
    with (tree / "conftest.py").open("a", encoding="utf-8") as stream:
        stream.write(f'''
from pathlib import Path
def pytest_unconfigure(config):
    Path(sys.argv[-1]).write_text({replacement!r})
''')
    result, report = observe(tree)
    assert result.returncode == 1
    assert "invalid_evidence" in report["reasons"]
    assert not list(tree.parent.glob("collection-*"))
    assert report["nodes"] == []


def test_nonzero_child_cannot_pass_with_complete_item_evidence(tree):
    with (tree / "conftest.py").open("a", encoding="utf-8") as stream:
        stream.write('''
import pytest
@pytest.hookimpl(trylast=True)
def pytest_sessionfinish(session, exitstatus):
    session.exitstatus = 3
''')
    result, report = observe(tree)
    assert result.returncode == 1
    assert report["nodes"]
    assert "child_failed" in report["reasons"]
    assert not list(tree.parent.glob("collection-*"))


def test_custom_filename_patterns_are_observed(tree):
    write(tree, "pytest.ini", "[pytest]\npython_files = check_*.py\n")
    write(tree, "tests/alternate/check_sample.py", "def test_sample(): pass\n")
    result, report = observe(tree)
    assert result.returncode == 0, report
    assert report["candidates"] == ["tests/alternate"]
    assert report["configuration"]["config_file"] == "pytest.ini"


def test_absolute_configuration_values_are_not_shared(tree):
    write(tree, "pytest.ini", f"[pytest]\ntestpaths = {(tree / 'tests').as_posix()}\n")
    result, report = observe(tree)
    assert result.returncode == 0, report
    assert report["configuration"]["testpaths"] == ["[root]/tests"]


def test_digest_changes_when_source_changes_between_observations(tree):
    _, before = observe(tree)
    write(tree, "tests/control/test_control.py", "def test_renamed(): pass\n")
    result, after = observe(tree)
    assert result.returncode == 0
    assert before["source_sha256"] != after["source_sha256"]


def load_tool():
    spec = importlib.util.spec_from_file_location("collection_observer", TOOL)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_inventory_does_not_silently_ignore_read_errors(tree, monkeypatch):
    tool = load_tool()

    def unavailable(path):
        raise PermissionError("synthetic denied read")

    monkeypatch.setattr(tool.os, "scandir", unavailable)
    with pytest.raises(PermissionError):
        tool.inventory(tree)
    output = tree.parent / "report.json"
    monkeypatch.setattr(sys, "argv", [str(TOOL), "--root", str(tree),
                                     "--boundary", str(tree.parent),
                                     "--output", str(output)])
    assert tool.main() == 1
    assert json.loads(output.read_text())["reasons"] == ["source_io_error"]


def test_child_start_failure_is_reported(tree, monkeypatch):
    tool = load_tool()

    def unavailable(*args, **kwargs):
        raise OSError("synthetic unavailable interpreter")

    monkeypatch.setattr(tool.subprocess, "run", unavailable)
    report = tool.observe(tree, tree.parent / "report.json", 1)
    assert report["status"] == "fail"
    assert "child_start_failed" in report["reasons"]
    assert not list(tree.parent.glob("collection-*"))


@pytest.mark.parametrize("error", [PermissionError, FileNotFoundError])
def test_cleanup_retries_without_losing_child_timeout(tree, monkeypatch, error):
    tool = load_tool()
    remove = shutil.rmtree
    attempts = []

    def transient_lock(path, *args, **kwargs):
        attempts.append(path)
        if len(attempts) == 1:
            raise error("synthetic transient removal failure")
        return remove(path, *args, **kwargs)

    def timeout(*args, **kwargs):
        kwargs["stderr"].write(b"synthetic child output")
        raise subprocess.TimeoutExpired(args[0], kwargs["timeout"])

    monkeypatch.setattr(shutil, "rmtree", transient_lock)
    monkeypatch.setattr(tool.subprocess, "run", timeout)
    report = tool.observe(tree, tree.parent / "report.json", 0.5)
    assert "child_timeout" in report["reasons"]
    assert "scratch_cleanup_failed" not in report["reasons"]
    assert len(attempts) == 2
    assert not list(tree.parent.glob("collection-*"))


@pytest.mark.parametrize("error", [PermissionError, FileNotFoundError])
@pytest.mark.parametrize("timed_out", [True, False])
def test_persistent_cleanup_failure_is_explicit_and_fails_closed(tree, monkeypatch,
                                                               timed_out, error):
    tool = load_tool()
    attempts = []

    def locked(path, *args, **kwargs):
        attempts.append(path)
        raise error("synthetic persistent removal failure")

    def timeout(*args, **kwargs):
        raise subprocess.TimeoutExpired(args[0], kwargs["timeout"])

    monkeypatch.setattr(shutil, "rmtree", locked)
    if timed_out:
        monkeypatch.setattr(tool.subprocess, "run", timeout)
    report = tool.observe(tree, tree.parent / "report.json", 0.5 if timed_out else 30)
    assert report["status"] == "fail"
    assert "scratch_cleanup_failed" in report["reasons"]
    assert ("child_timeout" in report["reasons"]) == timed_out
    assert len(attempts) == 3
    assert len(list(tree.parent.glob("collection-*"))) == 1
    if not timed_out:
        assert report["nodes"]


def test_cleanup_of_already_removed_folder_succeeds_without_retry(tree, monkeypatch):
    tool = load_tool()

    def unexpected_sleep(delay):
        pytest.fail("cleanup retried an absent folder")

    monkeypatch.setattr(tool.time, "sleep", unexpected_sleep)
    assert tool.remove_scratch(tree.parent / "already_removed")


def test_parameter_ids_with_absolute_paths_are_redacted(tree):
    write(tree, "tests/control/test_control.py", '''import pytest
from pathlib import Path
@pytest.mark.parametrize("value", [1], ids=[str(Path(__file__).resolve().parents[3])])
def test_control(value): pass
''')
    result, report = observe(tree)
    assert result.returncode == 0, report
    assert "[path removed]" in report["nodes"][0]["nodeid"]


@pytest.mark.parametrize("locator", [
    "C:synthetic_data", r"C:\synthetic_data\tests", "C:/synthetic_data/tests",
])
def test_windows_drive_locators_are_redacted(tree, locator):
    tool = load_tool()
    report = {"configuration": {"testpaths": [locator]},
              "nodes": [{"nodeid": f"tests/test_sample.py::test_sample[{locator}]"}]}
    shared = tool.shareable(report, tree)
    assert shared["configuration"]["testpaths"] == ["[path removed]"]
    expected = locator if locator == "C:synthetic_data" else "[path removed]"
    assert shared["nodes"][0]["nodeid"] == (
        f"tests/test_sample.py::test_sample[{expected}]")


@pytest.mark.parametrize("nodeid", [
    "tests/test_a.py::B::test_x", "test_p[a:1]", "test_p[a:2]",
])
def test_node_identifiers_are_preserved_outside_configuration(tree, nodeid):
    tool = load_tool()
    report = {"nodes": [{"nodeid": nodeid}], "errors": [nodeid], "skips": [nodeid]}
    assert tool.shareable(report, tree) == report


@pytest.mark.parametrize("suffix", ["", "/tests", "2/tests", "_other/tests"])
def test_root_redaction_respects_path_boundaries(tree, suffix):
    tool = load_tool()
    locator = tree.as_posix() + suffix
    expected = "[root]" + suffix if suffix in ("", "/tests") else "[path removed]"
    assert tool.shareable({"configuration": {"testpaths": [locator]}}, tree) == {
        "configuration": {"testpaths": [expected]}}


@pytest.mark.parametrize("suffix", ["", r"\tests", r"2\tests"])
def test_windows_root_redaction_respects_path_boundaries(suffix):
    tool = load_tool()
    root = PureWindowsPath(r"C:\synthetic\tree")
    expected = "[root]" + suffix if suffix in ("", r"\tests") else "[path removed]"
    assert tool.shareable(str(root) + suffix, root) == expected


def test_ancestor_configuration_is_rejected_before_collection(tree):
    nested = tree / "nested"
    write(nested, "tests/test_nested.py", "raise RuntimeError('must not import')\n")
    result, report = observe(nested, boundary=tree.parent,
                             output=tree.parent / "report.json")
    assert result.returncode == 1
    assert "configuration_outside_root" in report["reasons"]
    assert report["nodes"] == []
