# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Regression cases for the artifact reconciliation in issue 1903.

Expected bytes are literal fixtures, independent of generator computations.
An unchanged HTML file cannot establish that its JSON sidecar is current.
"""

import pytest

from scripts import check_generated_html as checker


def test_issue_sidecars_have_registered_owners():
    expected = {
        "scripts/capabilities/build_wall_thickness_explorer.py":
            "docs/api/structural/wall-thickness-explorer.json",
        "scripts/capabilities/build_dynacard_troubleshooting.py":
            "docs/api/artificial-lift/dynacard-troubleshooting.json",
        "scripts/capabilities/build_sloshing_explorer.py":
            "docs/api/structural/sloshing-explorer.json",
    }
    entries = {entry.script: entry for entry in checker.GENERATORS}
    for script, sidecar in expected.items():
        assert sidecar in entries[script].sidecars


@pytest.fixture
def artifact_pair(tmp_path, monkeypatch):
    entry = checker.Generator(
        "scripts/build_page.py", ("docs/api/page.html",),
        sidecars=("docs/api/page.json",),
    )
    monkeypatch.setattr(checker, "GENERATORS", (entry,))
    repo, shadow = tmp_path / "repo", tmp_path / "shadow"
    for root in (repo, shadow):
        (root / "docs/api").mkdir(parents=True)
        (root / "docs/api/page.html").write_text("<html>same</html>\n")
        (root / "docs/api/page.json").write_text('{"status":"current"}\n')
    return repo, shadow, entry


@pytest.mark.parametrize("fault", ["changed", "missing", "uncommitted"])
def test_sidecar_drift_is_detected_with_unchanged_html(artifact_pair, fault):
    repo, shadow, _entry = artifact_pair
    if fault == "changed":
        (shadow / "docs/api/page.json").write_text('{"status":"stale"}\n')
    elif fault == "missing":
        (shadow / "docs/api/page.json").unlink()
    else:
        (repo / "docs/api/page.json").unlink()
    assert [path for path, _diff in checker.compare_outputs(repo, shadow)] == [
        "docs/api/page.json"
    ]


def test_unchanged_pair_has_no_drift(artifact_pair):
    repo, shadow, _entry = artifact_pair
    assert checker.compare_outputs(repo, shadow) == []
    # HTML-only consumers such as the brand guard keep their existing scope.
    assert checker.all_registered_outputs(repo) == {repo / "docs/api/page.html"}


def test_missing_sidecar_cannot_reuse_copied_bytes(artifact_pair, monkeypatch):
    _repo, shadow, entry = artifact_pair
    checker.clear_generated_html(shadow, entry)
    assert not (shadow / "docs/api/page.json").exists()

    def write_html_only(_shadow, _command):
        from subprocess import CompletedProcess

        (shadow / "docs/api/page.html").write_text("<html>same</html>\n")
        return CompletedProcess([], 0, "", "")

    monkeypatch.setattr(checker, "run_script", write_html_only)
    error = checker.run_generator(shadow, entry)
    assert error is not None
    assert "output" in error


def test_registered_artifacts_include_sidecars(artifact_pair):
    repo, _shadow, _entry = artifact_pair
    assert checker.all_registered_artifacts(repo) == {
        repo / "docs/api/page.html", repo / "docs/api/page.json",
    }


def test_clear_tolerates_uncommitted_sidecar(artifact_pair):
    _repo, shadow, entry = artifact_pair
    (shadow / "docs/api/page.json").unlink()
    checker.clear_generated_html(shadow, entry)
    assert not (shadow / "docs/api/page.html").exists()


def test_shadow_keeps_source_version_metadata(tmp_path, monkeypatch):
    """An uninstalled source import reads its version from pyproject.toml."""
    repo, shadow = tmp_path / "repo", tmp_path / "shadow"
    for relative in ("scripts", "docs/api", "src"):
        (repo / relative).mkdir(parents=True)
    metadata = '[project]\nname = "example"\nversion = "0.0.0"\n'
    (repo / "pyproject.toml").write_text(metadata)
    monkeypatch.setattr(checker, "GENERATORS", ())
    checker.prepare_shadow(repo, shadow)
    assert (shadow / "pyproject.toml").read_text() == metadata
    (shadow / "pyproject.toml").write_text("changed only in shadow")
    assert (repo / "pyproject.toml").read_text() == metadata


def test_child_drops_inherited_git_bindings(tmp_path, monkeypatch):
    """The shadow cwd cannot override repository bindings inherited from Git."""
    bindings = ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE")
    for name in bindings:
        monkeypatch.setenv(name, "caller-binding")

    def capture(_command, **kwargs):
        from subprocess import CompletedProcess

        assert all(name not in kwargs["env"] for name in bindings)
        assert kwargs["cwd"] == tmp_path
        return CompletedProcess([], 0, "", "")

    monkeypatch.setattr(checker.subprocess, "run", capture)
    checker.run_script(tmp_path, ["synthetic-command"])
    import os

    assert all(os.environ[name] == "caller-binding" for name in bindings)
