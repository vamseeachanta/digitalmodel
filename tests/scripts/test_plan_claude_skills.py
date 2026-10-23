# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Post-clone setup contract from issue #2009 and the #469 repository plan.

Expected paths are derived independently: from checkout/.claude/skills, ascend
through .claude and checkout to the shared parent, then enter workspace-hub.
The three names and YYYYMMDD marker format come from #2009's Scope section.
Dates below are explicit synthetic calendar inputs, not engineering constants.
"""

import datetime
import json
import os
import shutil
import subprocess
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/setup-claude-skills.sh"
NAMES = ("guidelines", "meta", "workflows")
DATE = "20261014"


def fixture(tmp_path):
    repo = tmp_path / "checkout with spaces"
    hub = tmp_path / "workspace-hub"
    repo.mkdir()
    for name in NAMES:
        (hub / ".claude/skills/_internal" / name).mkdir(parents=True)
    return repo, hub


def command(repo, hub):
    return [
        "bash",
        str(SCRIPT),
        "--repo",
        str(repo),
        "--workspace-hub",
        str(hub),
        "--date",
        DATE,
    ]


def run(args, **kwargs):
    return subprocess.run(args, capture_output=True, text=True, **kwargs)


def assert_artifacts(repo, hub, date=DATE, destination=None):
    """Compare actual links with independently specified source directories."""
    destination = destination or repo / ".claude/skills"
    for name in NAMES:
        pointer = destination / name
        assert pointer.is_symlink()
        assert not Path(os.readlink(pointer)).is_absolute()
        assert pointer.resolve(strict=True) == (
            hub / ".claude/skills/_internal" / name
        ).resolve(strict=True)
    assert (destination / ".ecosystem-propagated").read_text() == date + "\n"


def test_all_four_relative_artifacts_and_idempotence(tmp_path):
    repo, hub = fixture(tmp_path)
    first = run(command(repo, hub))
    assert first.returncode == 0, first.stderr
    assert_artifacts(repo, hub)
    second = run(command(repo, hub))
    assert second.returncode == 0, second.stderr
    assert second.stdout == first.stdout
    plan = json.loads(first.stdout)
    assert plan["mode"] == "apply"
    assert {row["path"] for row in plan["artifacts"]} == {
        f".claude/skills/{name}" for name in (*NAMES, ".ecosystem-propagated")
    }
    for row in plan["artifacts"][:-1]:
        assert row["kind"] == "symlink"
        assert row["target"] == (
            "../../../workspace-hub/.claude/skills/_internal/" + Path(row["path"]).name
        )
    assert plan["artifacts"][-1]["content"] == DATE + "\n"


def test_dry_run_prints_recipe_without_writes(tmp_path):
    repo, hub = fixture(tmp_path)
    first = run(command(repo, hub) + ["--dry-run"])
    second = run(command(repo, hub) + ["--dry-run"])
    assert first.returncode == second.returncode == 0
    assert first.stdout == second.stdout
    assert json.loads(first.stdout)["mode"] == "plan"
    assert list(repo.iterdir()) == []


def test_absent_sibling_fails_without_writes(tmp_path):
    repo = tmp_path / "checkout"
    repo.mkdir()
    result = run(["bash", str(SCRIPT), "--repo", str(repo)])
    assert result.returncode != 0
    assert "Clone workspace-hub" in result.stderr
    assert "sibling" in result.stderr
    assert "--workspace-hub" in result.stderr
    assert "Traceback" not in result.stderr
    assert list(repo.iterdir()) == []


@pytest.mark.parametrize("missing", NAMES)
def test_missing_one_target_fails_without_writes(tmp_path, missing):
    repo, hub = fixture(tmp_path)
    (hub / ".claude/skills/_internal" / missing).rmdir()
    result = run(command(repo, hub))
    assert result.returncode != 0
    assert missing in result.stderr
    assert list(repo.iterdir()) == []


def test_missing_source_preserves_existing_artifacts(tmp_path):
    repo, hub = fixture(tmp_path)
    assert run(command(repo, hub)).returncode == 0
    output = repo / ".claude/skills"
    before = {name: os.readlink(output / name) for name in NAMES}
    (hub / ".claude/skills/_internal/meta").rmdir()
    result = run(command(repo, hub) + ["--date", "20261015"])
    assert result.returncode != 0
    assert {name: os.readlink(output / name) for name in NAMES} == before
    assert (output / ".ecosystem-propagated").read_text() == DATE + "\n"


def test_arbitrary_checkout_depth_has_no_absolute_target(tmp_path):
    """A nested checkout needs four ascents rather than the sibling's three."""
    repo, hub = fixture(tmp_path)
    deeper = tmp_path / "trees/library"
    deeper.mkdir(parents=True)
    result = run(command(deeper, hub))
    assert result.returncode == 0, result.stderr
    assert_artifacts(deeper, hub)
    for row in json.loads(result.stdout)["artifacts"][:-1]:
        assert row["target"] == (
            "../../../../workspace-hub/.claude/skills/_internal/"
            + Path(row["path"]).name
        )
    assert list(repo.iterdir()) == []


def test_apply_alias_and_explicit_destination(tmp_path):
    repo, hub = fixture(tmp_path)
    output = repo / "pointers"
    args = command(repo, hub) + ["--apply", "--skills-dir", str(output)]
    first = run(args)
    assert first.returncode == 0, first.stderr
    assert_artifacts(repo, hub, destination=output)
    second = run(args)
    assert second.returncode == 0, second.stderr
    assert second.stdout == first.stdout


@pytest.mark.parametrize("old_target_exists", [False, True])
def test_stale_symlinks_are_replaced_without_changing_their_targets(
    tmp_path,
    old_target_exists,
):
    repo, hub = fixture(tmp_path)
    output = repo / ".claude/skills"
    output.mkdir(parents=True)
    old = tmp_path / "old-skills"
    if old_target_exists:
        old.mkdir()
        (old / "retained").write_text("original data\n")
    for name in NAMES:
        (output / name).symlink_to(old, target_is_directory=True)
    (output / ".ecosystem-propagated").write_text("old date\n")
    result = run(command(repo, hub))
    assert result.returncode == 0, result.stderr
    assert_artifacts(repo, hub)
    if old_target_exists:
        assert (old / "retained").read_text() == "original data\n"
    else:
        assert not old.exists()
    assert set(path.name for path in output.iterdir()) == {
        *NAMES,
        ".ecosystem-propagated",
    }


def test_matching_links_are_retained_and_marker_is_refreshed(tmp_path):
    repo, hub = fixture(tmp_path)
    assert run(command(repo, hub)).returncode == 0
    output = repo / ".claude/skills"
    before = {name: (output / name).lstat().st_ino for name in NAMES}
    result = run(command(repo, hub) + ["--date", "20261015"])
    assert result.returncode == 0, result.stderr
    assert_artifacts(repo, hub, date="20261015")
    assert {name: (output / name).lstat().st_ino for name in NAMES} == before


@pytest.mark.parametrize(
    ("entry", "kind"),
    [
        ("meta", "file"),
        ("meta", "directory"),
        (".ecosystem-propagated", "file"),
        (".ecosystem-propagated", "directory"),
        (".ecosystem-propagated", "symlink"),
        (".ecosystem-propagated", "dangling-link"),
    ],
)
def test_conflicts_are_detected_before_any_write(tmp_path, entry, kind):
    repo, hub = fixture(tmp_path)
    output = repo / ".claude/skills"
    output.mkdir(parents=True)
    path = output / entry
    target = tmp_path / "existing data"
    if kind == "file":
        path.write_text("preserved\n")
    elif kind == "directory":
        path.mkdir()
        (path / "retained").write_text("preserved\n")
    else:
        if kind == "symlink":
            target.write_text("preserved\n")
        path.symlink_to(target)
    # A regular marker is refreshable. Keep a later conflicting real pointer.
    if entry == ".ecosystem-propagated" and kind == "file":
        (output / "workflows").write_text("preserved workflow\n")
    result = run(command(repo, hub))
    assert result.returncode != 0
    assert "manual review" in result.stderr
    assert not (output / "guidelines").exists()
    if kind == "file":
        assert path.read_text() == "preserved\n"
    elif kind == "directory":
        assert (path / "retained").read_text() == "preserved\n"
    else:
        assert path.is_symlink()
        assert os.readlink(path) == str(target)
        if kind == "symlink":
            assert target.read_text() == "preserved\n"


@pytest.mark.parametrize("parent", [".claude", ".claude/skills"])
def test_symlinked_destination_parents_do_not_write_outside_checkout(tmp_path, parent):
    repo, hub = fixture(tmp_path)
    outside = tmp_path / "outside"
    outside.mkdir()
    path = repo / parent
    path.parent.mkdir(parents=True, exist_ok=True)
    path.symlink_to(outside, target_is_directory=True)
    result = run(command(repo, hub))
    assert result.returncode != 0
    assert "manual review" in result.stderr
    assert list(outside.iterdir()) == []
    assert path.is_symlink()


@pytest.mark.parametrize("parent", [".claude", ".claude/skills"])
def test_regular_file_destination_parents_are_preserved(tmp_path, parent):
    repo, hub = fixture(tmp_path)
    path = repo / parent
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text("preserved\n")
    result = run(command(repo, hub))
    assert result.returncode != 0
    assert "manual review" in result.stderr
    assert path.read_text() == "preserved\n"


@pytest.mark.parametrize("date", ["20260230", "20261301", "2026101", "abcdefgh"])
def test_invalid_date_fails_without_writes(tmp_path, date):
    repo, hub = fixture(tmp_path)
    result = run(command(repo, hub) + ["--date", date])
    assert result.returncode != 0
    assert "date" in result.stderr
    assert list(repo.iterdir()) == []


def test_default_clone_command_uses_script_location_and_leaves_git_clean(tmp_path):
    """Use actual ignore rules; no fixture-specific ignore rule hides artifacts."""
    repo, hub = fixture(tmp_path)
    scripts = repo / "scripts"
    scripts.mkdir()
    for name in ("setup-claude-skills.sh", "plan_claude_skills.py"):
        shutil.copyfile(ROOT / "scripts" / name, scripts / name)
    output = repo / ".claude/skills"
    output.mkdir(parents=True)
    shutil.copyfile(ROOT / ".gitignore", repo / ".gitignore")
    shutil.copyfile(ROOT / ".claude/skills/.gitignore", output / ".gitignore")
    subprocess.run(["git", "init", "-q", str(repo)], check=True)
    (repo / ".git/info/exclude").write_text(
        ".gitignore\n.claude/skills/.gitignore\nscripts/\n",
    )
    status = ["git", "status", "--porcelain", "--untracked-files=all"]
    assert run(status, cwd=repo).stdout == ""
    args = ["bash", str(scripts / "setup-claude-skills.sh")]
    before = datetime.date.today().strftime("%Y%m%d")
    first = run(args, cwd=tmp_path)
    after = datetime.date.today().strftime("%Y%m%d")
    assert first.returncode == 0, first.stderr
    marker = (output / ".ecosystem-propagated").read_text().strip()
    assert marker in {before, after}
    assert_artifacts(repo, hub, date=marker)
    assert run(args, cwd=tmp_path).returncode == 0
    assert run(status, cwd=repo).stdout == ""
    paths = [f".claude/skills/{name}" for name in (*NAMES, ".ecosystem-propagated")]
    ignored = run(
        ["git", "check-ignore", "--stdin"], cwd=repo, input="\n".join(paths) + "\n"
    )
    assert set(ignored.stdout.splitlines()) == set(paths)


def test_real_destination_names_remain_ignored():
    help_result = run(["bash", str(SCRIPT), "--help"])
    assert help_result.returncode == 0
    assert "--apply" in help_result.stdout
    assert "--dry-run" in help_result.stdout
    paths = [f".claude/skills/{name}" for name in (*NAMES, ".ecosystem-propagated")]
    result = run(
        ["git", "check-ignore", "--stdin"], cwd=ROOT, input="\n".join(paths) + "\n"
    )
    assert set(result.stdout.splitlines()) == set(paths)


def test_later_conflict_preserves_earlier_stale_link_and_marker(tmp_path):
    repo, hub = fixture(tmp_path)
    output = repo / ".claude/skills"
    output.mkdir(parents=True)
    (output / "guidelines").symlink_to("old-target")
    (output / "workflows").write_text("retained workflow\n")
    (output / ".ecosystem-propagated").write_text("old marker\n")
    result = run(command(repo, hub))
    assert result.returncode != 0
    assert os.readlink(output / "guidelines") == "old-target"
    assert not (output / "meta").is_symlink()
    assert (output / "workflows").read_text() == "retained workflow\n"
    assert (output / ".ecosystem-propagated").read_text() == "old marker\n"


@pytest.mark.parametrize("destination", ["outside", "repository-root"])
def test_explicit_destination_must_be_inside_repository(tmp_path, destination):
    repo, hub = fixture(tmp_path)
    output = tmp_path / "outside" if destination == "outside" else repo
    result = run(command(repo, hub) + ["--skills-dir", str(output)])
    assert result.returncode != 0
    assert "--skills-dir must be a subdirectory" in result.stderr
    assert list(repo.iterdir()) == []
    if destination == "outside":
        assert not output.exists()


def test_absent_repository_fails_without_writes(tmp_path):
    repo, hub = fixture(tmp_path)
    absent = repo / "absent"
    result = run(command(absent, hub))
    assert result.returncode != 0
    assert "repository directory is absent" in result.stderr
    assert list(repo.iterdir()) == []


def test_dry_run_and_apply_cannot_be_combined(tmp_path):
    repo, hub = fixture(tmp_path)
    result = run(command(repo, hub) + ["--dry-run", "--apply"])
    assert result.returncode != 0
    assert "not allowed" in result.stderr
    assert list(repo.iterdir()) == []
