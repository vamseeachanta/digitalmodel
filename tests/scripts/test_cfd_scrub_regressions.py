"""Pin configuration and provenance restored after redaction."""
from pathlib import Path
import re
import importlib.util
import sys

ROOT = Path(__file__).resolve().parents[2]


def test_status_defaults_agree() -> None:
    """The collector and renderer share their unconfigured cache location."""
    status = ROOT / "scripts/cfd/status"
    collector = (status / "cfd_status_collect.sh").read_text()
    renderer = (status / "cfd_statusline.sh").read_text()
    collected = re.search(r'cfg.get\("cache", "([^"]+)"\)', collector).group(1)
    rendered = re.search(r"configured_cache:-([^}]+)", renderer).group(1)
    assert collected.replace("~", "$HOME", 1) == rendered


def test_deployment_records_revision_and_parity() -> None:
    """The deployment stamp identifies verified content without machine identity."""
    source = (ROOT / "scripts/cfd/deploy_lane.sh").read_text()
    line = next(line for line in source.splitlines() if ">> cfd/" in line)
    assert "digitalmodel@$rev" in line
    assert "parity=verified" in line


def test_package_public_metadata_matches_main() -> None:
    """Published author and repository links retain the upstream metadata."""
    current = (ROOT / "pyproject.toml").read_text()
    keys = ('    {name =', '"Homepage" =', '"Bug Tracker" =', '"Documentation" =')
    expected = [
        '    {name = "Development Team", email = "dev@example.com"}',
        '"Homepage" = "https://github.com/vamseeachanta/digitalmodel"',
        '"Bug Tracker" = "https://github.com/vamseeachanta/digitalmodel/issues"',
        '"Documentation" = "https://github.com/vamseeachanta/digitalmodel#readme"',
    ]
    actual = [line for line in current.splitlines() if line.startswith(keys)]
    assert len(expected) == 4
    assert actual == expected


def test_dispatch_default_repo_is_home_relative() -> None:
    """The default remote repository retains its home-relative location."""
    spec = importlib.util.spec_from_file_location(
        "dispatch_default_regression", ROOT / "scripts/setup/dispatch-cfd-run.py",
    )
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    assert module.build_parser().parse_args([]).repo_dir == "~/digitalmodel"
