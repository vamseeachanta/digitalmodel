"""Contracts for Bandit startup and changed-source-file reporting."""

from pathlib import Path

import yaml


REPO = Path(__file__).resolve().parents[2]


def bandit_entry():
    config = yaml.safe_load((REPO / ".pre-commit-config.yaml").read_text())
    return next(
        hook
        for repo in config["repos"]
        for hook in repo["hooks"]
        if hook["id"] == "bandit"
    )


def test_bandit_entry_declares_its_runtime_dependency():
    assert "pbr>=2.0.0" in bandit_entry().get("additional_dependencies", [])


def test_bandit_entry_scans_changed_files_without_narrowing():
    entry = bandit_entry()
    assert entry.get("pass_filenames", True) is True
    assert "pbr>=2.0.0" in entry.get("additional_dependencies", [])
    assert entry.get("files") == "^src/"
    assert not {"exclude", "stages", "skips", "skip"}.intersection(entry)
    forbidden = {
        "--skip", "-s", "--baseline", "-b", "--severity-level",
        "--level", "-l", "-ll", "-lll", "--confidence-level",
        "--confidence", "-i", "-ii", "-iii", "--exit-zero",
        "-x", "--exclude", "-c", "--configfile",
    }
    args = entry.get("args", [])
    for arg in args:
        option = arg.partition("=")[0]
        assert option not in forbidden, f"Reporting must retain every class: {arg}"
        if arg.startswith("-") and not arg.startswith("--"):
            assert not any(arg.startswith(flag) for flag in ("-s", "-b", "-l", "-i", "-x", "-c"))
    assert not ("-r" in args and "src/" in args)
    config_text = (REPO / ".pre-commit-config.yaml").read_text()
    assert ".bandit-baseline.json" not in config_text
    config = yaml.safe_load(config_text)
    repo = next(repo for repo in config["repos"] if entry in repo["hooks"])
    assert repo["rev"] == "1.7.5"


def test_bandit_revision_is_pinned():
    """Regression pin: retain the existing Bandit revision."""
    config = yaml.safe_load((REPO / ".pre-commit-config.yaml").read_text())
    repo = next(
        repo for repo in config["repos"]
        if any(hook["id"] == "bandit" for hook in repo["hooks"])
    )
    assert repo["rev"] == "1.7.5"
