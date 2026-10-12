"""Pin configuration and provenance restored after redaction."""
from pathlib import Path
import re
import subprocess

ROOT = Path(__file__).resolve().parents[2]


def test_status_defaults_agree() -> None:
    """The collector and renderer share their unconfigured cache location."""
    status = ROOT / "scripts/cfd/status"
    collector = (status / "cfd_status_collect.sh").read_text()
    renderer = (status / "cfd_statusline.sh").read_text()
    collected = re.search(r'cfg.get\("cache", "([^"]+)"\)', collector).group(1)
    rendered = re.search(r"configured_cache:-([^}]+)", renderer).group(1)
    assert collected.replace("~", "$HOME", 1) == rendered


def test_deployment_records_runtime_identity() -> None:
    """Deployment provenance is evaluated on the deploying host."""
    source = (ROOT / "scripts/cfd/deploy_lane.sh").read_text()
    line = next(line for line in source.splitlines() if "scripts/cfd from" in line)
    assert "$(hostname -s)" in line


def test_package_public_metadata_matches_main() -> None:
    """Published author and repository links retain the upstream metadata."""
    main = subprocess.check_output(
        ["git", "show", "origin/main:pyproject.toml"], cwd=ROOT, text=True,
    )
    current = (ROOT / "pyproject.toml").read_text()
    keys = ('    {name =', '"Homepage" =', '"Bug Tracker" =', '"Documentation" =')
    expected = [line for line in main.splitlines() if line.startswith(keys)]
    actual = [line for line in current.splitlines() if line.startswith(keys)]
    assert len(expected) == 4
    assert actual == expected
