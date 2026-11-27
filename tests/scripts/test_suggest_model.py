"""Repository tiers follow public exact names and explicit private configuration."""

import os
import subprocess
from pathlib import Path

import pytest


SUGGEST_MODEL = Path(__file__).resolve().parents[2] / "scripts/monitoring/suggest_model.sh"


@pytest.fixture
def suggest(tmp_path, monkeypatch):
    monkeypatch.setenv("HOME", str(tmp_path))
    monkeypatch.delenv("ACE_PRIVATE_REPOS", raising=False)
    monkeypatch.setenv("ACE_PRIVATE_CONFIG", str(tmp_path / "private-repos.txt"))

    def run(repo):
        result = subprocess.run(
            ["bash", str(SUGGEST_MODEL), repo, "review architecture"],
            cwd=tmp_path,
            input="n\n",
            text=True,
            capture_output=True,
            check=True,
        )
        tier = next(line.strip() for line in result.stdout.splitlines() if line.startswith("  Tier: "))
        return tier.removeprefix("Tier: "), result.stderr.splitlines()

    return run


@pytest.mark.parametrize(("repo", "expected"), [
    ("workspace-hub", "Work Tier 1 (Production)"),
    ("digitalmodel", "Work Tier 1 (Production)"),
    ("assetutilities", "Work Tier 2 (Active)"),
    ("worldenergydata", "Work Tier 2 (Active)"),
    ("aceengineer-website", "Personal (Active)"),
    ("hobbies", "Personal (Experimental)"),
])
def test_static_tiers_match_whole_names(suggest, repo, expected):
    assert suggest(repo) == (expected, [])
    assert suggest(f"prefix-{repo}-suffix") == ("Unknown", [])


def test_missing_config_adds_no_private_tier(suggest):
    assert suggest("sample-one") == ("Unknown", [])


def test_file_matches_complete_private_names(suggest, tmp_path):
    (tmp_path / "private-repos.txt").write_text(" sample-one \nsample-two\n", encoding="utf-8")
    assert suggest("sample-one") == ("Work Tier 3 (Maintenance)", [])
    assert suggest("prefix-sample-one") == ("Unknown", [])


def test_environment_takes_precedence_over_file(suggest, tmp_path, monkeypatch):
    (tmp_path / "private-repos.txt").write_text("file-only\n", encoding="utf-8")
    monkeypatch.setenv("ACE_PRIVATE_REPOS", " sample-one ,sample-two")
    assert suggest("sample-one") == ("Work Tier 3 (Maintenance)", [])
    assert suggest("file-only") == ("Unknown", [])


@pytest.mark.parametrize("failure", ["unreadable", "directory", "invalid-utf8"])
def test_config_error_warns_once_and_adds_no_private_tier(suggest, tmp_path, failure):
    config = tmp_path / "private-repos.txt"
    if failure == "directory":
        config.mkdir()
    else:
        config.write_bytes(b"sample-one\n" + (b"\xff" if failure == "invalid-utf8" else b""))
        if failure == "unreadable":
            config.chmod(0)
            if os.access(config, os.R_OK):
                pytest.skip("current user can read mode-000 files")

    tier, warnings = suggest("sample-one")
    assert tier == "Unknown"
    assert len(warnings) == 1
    assert "warning" in warnings[0].lower()
