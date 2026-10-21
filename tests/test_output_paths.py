"""Canonical destination expectations derived by hand for issue #2315.

``child/../report`` and ``report`` address the same fixture file; a symlink
directory addresses its target directory. These are path-identity assertions,
not numerical acceptance tests. Source: Python 3.11 pathlib documentation,
Path.resolve and Path.samefile (linked in the helper's docstring).
"""

import errno
from hashlib import sha256
from pathlib import Path
import runpy

import pytest

from digitalmodel._output_paths import validate_output_paths


def test_normalized_nonexistent_destinations_collide_without_creating_directories(tmp_path):
    output = tmp_path / "new" / "report"
    alias = tmp_path / "new" / "child" / ".." / "report"
    with pytest.raises(ValueError, match="distinct"):
        validate_output_paths(output, alias)
    assert not output.parent.exists()


def test_directory_symlink_aliases_are_rejected(tmp_path):
    directory = tmp_path / "details"
    directory.mkdir()
    alias = tmp_path / "alias"
    try:
        alias.symlink_to(directory, target_is_directory=True)
    except OSError as exc:
        if exc.errno not in (errno.EPERM, errno.EACCES) and getattr(exc, "winerror", None) != 1314:
            raise
        pytest.skip("Symlink creation is unavailable")
    with pytest.raises(ValueError, match="distinct"):
        validate_output_paths(directory / "report", alias / "report")
    assert not list(directory.iterdir())


def test_distinct_existing_files_are_not_changed(tmp_path):
    first, second = tmp_path / "first", tmp_path / "second"
    for path in (first, second):
        path.write_bytes(b"retained input")
    before = {path: sha256(path.read_bytes()).digest() for path in (first, second)}
    validate_output_paths(first, second)
    assert {path: sha256(path.read_bytes()).digest() for path in before} == before


def test_empty_output_set_is_valid():
    validate_output_paths()


@pytest.mark.parametrize("existing", [False, True])
def test_case_only_destinations_are_rejected(tmp_path, existing):
    first, second = tmp_path / "analysis.HTML", tmp_path / "analysis.html"
    if existing:
        first.write_bytes(b"retained input")
    with pytest.raises(ValueError, match="distinct"):
        validate_output_paths(first, second)
    if existing:
        assert first.read_bytes() == b"retained input"
    else:
        assert not first.exists()
        assert not second.exists()


@pytest.mark.parametrize("module,function,kwargs,fail_on", [
    ("test_output_paths.py", "test_directory_symlink_aliases_are_rejected", {}, 1),
    ("reporting/test_output_destinations.py",
     "test_colliding_bundle_outputs_preserve_all_files",
     {"pair": ("analysis.html", "analysis_citations.json"), "alias": "symlink"}, 1),
    ("reporting/test_output_destinations.py", "test_disabled_pdf_is_not_a_destination", {}, 1),
    ("reporting/test_output_destinations.py",
     "test_cross_directory_artifacts_keep_resolvable_manifest_links", {}, 1),
    ("reporting/test_output_destinations.py",
     "test_cross_directory_artifacts_keep_resolvable_manifest_links", {}, 2),
    ("subsea/catenary_riser/test_report_destinations.py",
     "test_aliased_destinations_preserve_existing_bytes", {"alias": "symlink"}, 1),
    ("visualization/test_export_destinations.py",
     "test_all_destinations_validated_before_first_export", {"pair": ("html", "json")}, 1),
    ("visualization/test_export_destinations.py",
     "test_unselected_formats_do_not_create_false_collisions", {}, 1),
])
@pytest.mark.parametrize("error_number,windows_error,should_skip", [
    (errno.EPERM, None, True),
    (errno.EACCES, None, True),
    (None, 1314, True),
    (errno.EEXIST, None, False),
    (errno.ENOENT, None, False),
    (errno.EIO, None, False),
], ids=["eperm", "eacces", "windows-privilege", "exists", "missing", "io"])
def test_symlink_fixture_skips_only_privilege_errors(
    tmp_path, monkeypatch, module, function, kwargs, fail_on,
    error_number, windows_error, should_skip,
):
    """Exercise every symlink handler with privilege and fixture failures."""
    target = runpy.run_path(str(Path(__file__).parent / module))[function]
    failure = OSError(error_number, "Synthetic symlink failure")
    if windows_error is not None:
        failure.winerror = windows_error
    calls = 0

    def fail_symlink(*args, **options):
        nonlocal calls
        calls += 1
        if calls == fail_on:
            raise failure
        # Earlier calls need no link: execution stops at the injected error.

    monkeypatch.setattr(Path, "symlink_to", fail_symlink)
    arguments = {"tmp_path": tmp_path, **kwargs}
    if function == "test_colliding_bundle_outputs_preserve_all_files":
        arguments["monkeypatch"] = monkeypatch
    if should_skip:
        with pytest.raises(pytest.skip.Exception, match="Symlink creation is unavailable"):
            target(**arguments)
    else:
        with pytest.raises(OSError) as caught:
            try:
                target(**arguments)
            except pytest.skip.Exception:
                pytest.fail("An unexpected symlink error was converted to a skip")
        assert caught.value is failure
    assert calls == fail_on
