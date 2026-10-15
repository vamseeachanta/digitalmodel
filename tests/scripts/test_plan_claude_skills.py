# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Read-only pointer plans use relative targets and preserve existing files."""

import json
import subprocess
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/setup-claude-skills.sh"


def fixture(tmp_path):
    repo = tmp_path / "checkout"
    hub = tmp_path / "workspace-hub"
    repo.mkdir()
    for name in ("guidelines", "meta", "workflows"):
        (hub / ".claude/skills/_internal" / name).mkdir(parents=True)
    return repo, hub


def command(repo, hub):
    return ["bash", str(SCRIPT), "--repo", str(repo), "--workspace-hub", str(hub),
            "--date", "20261014"]


def test_all_four_relative_artifacts_and_idempotence(tmp_path):
    repo, hub = fixture(tmp_path)
    first = subprocess.run(command(repo, hub), capture_output=True, text=True)
    assert first.returncode == 0, first.stderr
    second = subprocess.run(command(repo, hub), capture_output=True, text=True)
    assert second.stdout == first.stdout
    plan = json.loads(first.stdout)
    assert len(plan["artifacts"]) == 4
    for row in plan["artifacts"][:3]:
        assert row["kind"] == "symlink"
        assert row["target"].startswith("../../../workspace-hub/")
        assert (repo / row["path"]).parent.joinpath(row["target"]).resolve() == (
            hub / ".claude/skills/_internal" / Path(row["path"]).name
        ).resolve()
    assert plan["artifacts"][3]["content"] == "20261014\n"
    assert list(repo.iterdir()) == []


def test_absent_sibling_fails_without_writes(tmp_path):
    repo = tmp_path / "checkout"
    repo.mkdir()
    result = subprocess.run(command(repo, tmp_path / "workspace-hub"),
                            capture_output=True, text=True)
    assert result.returncode != 0
    assert "Clone workspace-hub" in result.stderr
    assert list(repo.iterdir()) == []


def test_missing_one_target_fails_without_writes(tmp_path):
    repo, hub = fixture(tmp_path)
    (hub / ".claude/skills/_internal/meta").rmdir()
    result = subprocess.run(command(repo, hub), capture_output=True, text=True)
    assert result.returncode != 0
    assert "meta" in result.stderr
    assert list(repo.iterdir()) == []


def test_arbitrary_checkout_depth_has_no_absolute_target(tmp_path):
    repo, hub = fixture(tmp_path)
    deeper = repo / "nested/checkouts/library"
    deeper.mkdir(parents=True)
    result = subprocess.run(command(deeper, hub), capture_output=True, text=True)
    assert result.returncode == 0
    for row in json.loads(result.stdout)["artifacts"][:3]:
        assert not Path(row["target"]).is_absolute()
        assert (deeper / row["path"]).parent.joinpath(row["target"]).resolve().is_dir()


def test_apply_creates_four_artifacts_and_reruns_cleanly(tmp_path):
    repo, hub = fixture(tmp_path)
    output = repo / "pointers"
    subprocess.run(["git", "init", "-q", str(repo)], check=True)
    (repo / ".gitignore").write_text(".gitignore\npointers/\n")
    assert subprocess.check_output(["git", "status", "--porcelain"], cwd=repo) == b""
    args = command(repo, hub) + ["--apply", "--skills-dir", str(output)]
    first = subprocess.run(args, capture_output=True, text=True)
    assert first.returncode == 0, first.stderr
    for name in ("guidelines", "meta", "workflows"):
        assert (output / name).is_symlink()
        assert (output / name).resolve() == (hub / ".claude/skills/_internal" / name).resolve()
    assert (output / ".ecosystem-propagated").read_text() == "20261014\n"
    second = subprocess.run(args, capture_output=True, text=True)
    assert second.returncode == 0
    assert second.stdout == first.stdout
    assert subprocess.check_output(["git", "status", "--porcelain"], cwd=repo) == b""


def test_apply_refuses_existing_regular_file_before_any_write(tmp_path):
    repo, hub = fixture(tmp_path)
    output = repo / "pointers"
    output.mkdir()
    (output / "meta").write_text("preserved\n")
    result = subprocess.run(command(repo, hub) + ["--apply", "--skills-dir", str(output)],
                            capture_output=True, text=True)
    assert result.returncode != 0
    assert "existing pointer entry requires manual review: meta" in result.stderr
    assert (output / "meta").read_text() == "preserved\n"
    assert sorted(p.name for p in output.iterdir()) == ["meta"]


def test_real_destination_names_are_ignored_without_generating_them():
    help_result = subprocess.run(["bash", str(SCRIPT), "--help"],
                                 capture_output=True, text=True)
    assert "--apply" in help_result.stdout
    paths = [f".claude/skills/{name}" for name in
             ("guidelines", "meta", "workflows", ".ecosystem-propagated")]
    result = subprocess.run(["git", "check-ignore", "--stdin"], cwd=ROOT,
                            input="\n".join(paths) + "\n", capture_output=True, text=True)
    assert set(result.stdout.splitlines()) == set(paths)
