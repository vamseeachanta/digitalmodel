"""Contracts for Markdown link-check startup and unchanged link selection."""

import json
from pathlib import Path, PureWindowsPath
import re

import yaml

ROOT = Path(__file__).resolve().parents[2]
PROBE_LINKS = (
    "https://example.com/documentation",
    "http://example.org/tool",
    "guide.md",
    "../README.md",
    "#section",
)


def _entries():
    config = yaml.safe_load((ROOT / ".pre-commit-config.yaml").read_text())
    return [
        (repo, hook)
        for repo in config["repos"]
        for hook in repo["hooks"]
        if hook["id"] == "markdown-link-check"
    ]


def _config_path():
    _, entry = _entries()[0]
    args = entry["args"]
    argument = args[args.index("--config") + 1]
    windows_path = PureWindowsPath(argument)
    assert not windows_path.drive and not windows_path.root
    relative_path = Path(argument.replace("\\", "/"))
    assert not relative_path.is_absolute()
    path = (ROOT / relative_path).resolve()
    assert path.is_relative_to(ROOT.resolve())
    assert path.is_file(), "Markdown link-check configuration must exist"
    return path


def test_markdown_link_check_config_exists():
    assert _config_path().is_file()


def test_markdown_link_check_config_is_a_json_object():
    config = json.loads(_config_path().read_text())
    assert isinstance(config, dict)


def test_markdown_link_check_config_is_not_loosened():
    config = json.loads(_config_path().read_text())
    for entry in config.get("ignorePatterns", []):
        pattern = re.compile(entry["pattern"])
        assert all(pattern.search(link) is None for link in PROBE_LINKS)
    assert all(int(code) < 400 for code in config.get("aliveStatusCodes", []))
    assert "httpHeaders" not in config


def test_markdown_link_check_entry_is_not_narrowed():
    """Regression pin: retain the hook revision and file selection at BASE."""
    entries = _entries()
    assert len(entries) == 1
    repo, entry = entries[0]
    assert repo["rev"] == "v3.11.2"
    assert not {"exclude", "files", "stages", "pass_filenames"}.intersection(entry)
    assert len(entry["args"]) == 2
    assert entry["args"][0] == "--config"
