"""Security hooks retain their scanners and repository-local configuration."""

from pathlib import Path, PureWindowsPath
import tomllib

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]


def _hooks():
    config = yaml.safe_load((ROOT / ".pre-commit-config.yaml").read_text())
    return {
        hook["id"]: hook
        for repo in config["repos"]
        for hook in repo["hooks"]
    }


def _argument_stays_in_repository(argument, root):
    value = argument.split("=", 1)[1] if argument.startswith("--") and "=" in argument else argument
    if value.startswith("-"):
        return True
    windows_path = PureWindowsPath(value)
    if value.startswith("..") or windows_path.drive or windows_path.root:
        return False
    path = Path(value.replace("\\", "/"))
    return not path.is_absolute() and (root / path).resolve().is_relative_to(root.resolve())


def _gitleaks_config_path():
    args = _hooks()["gitleaks"].get("args", [])
    for index, argument in enumerate(args):
        if argument == "--config":
            assert index + 1 < len(args), "Gitleaks --config requires a path"
            return args[index + 1]
        if argument.startswith("--config="):
            return argument.split("=", 1)[1]
    pytest.fail("Gitleaks must name a repository-owned configuration")


def test_all_hook_argument_paths_stay_in_repository():
    for identifier, hook in _hooks().items():
        args = hook.get("args", [])
        for index, argument in enumerate(args):
            if index and args[index - 1] == "-c":
                continue  # Inline Python programs are not path arguments.
            assert _argument_stays_in_repository(argument, ROOT), (
                f"{identifier}: hook argument leaves the repository: {argument}"
            )


def test_gitleaks_configuration_exists_and_extends_defaults():
    argument = _gitleaks_config_path()
    assert _argument_stays_in_repository(argument, ROOT), "Gitleaks configuration must be repository-local"
    path = ROOT / argument
    assert path.is_file(), "Gitleaks configuration must exist in the repository"
    config = tomllib.loads(path.read_text())
    assert config["extend"]["useDefault"] is True
    assert "path" not in config["extend"]
    assert "rules" not in config
    assert "allowlist" not in config


def test_security_hook_identifiers_regression_pin():
    """Regression pin: both scanners are present on the unchanged base."""
    assert {"bandit", "gitleaks"}.issubset(_hooks())


@pytest.mark.parametrize(
    ("argument", "expected"),
    [
        ("../config.toml", False),
        ("--config=../config.toml", False),
        ("nested/../../config.toml", False),
        ("..\\config.toml", False),
        (str(Path(Path.cwd().anchor) / "external.toml"), False),
        (str(PureWindowsPath("Z:") / "external.toml"), False),
        ("src/", True),
        ("--config=.gitleaks.toml", True),
        ("--verbose", True),
    ],
)
def test_argument_containment_cases(argument, expected, tmp_path):
    assert _argument_stays_in_repository(argument, tmp_path) is expected


def test_argument_resolved_escape(tmp_path):
    root = tmp_path / "repository"
    root.mkdir()
    outside = tmp_path / "external.toml"
    outside.write_text("title = 'external'\n")
    (root / "config.toml").symlink_to(outside)
    assert not _argument_stays_in_repository("config.toml", root)
