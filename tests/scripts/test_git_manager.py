"""Private checkouts are discovered only when configured and present."""

import pytest

from scripts.git.git_manager import GitManager


@pytest.fixture
def checkouts(tmp_path, monkeypatch):
    home = tmp_path / "home"
    monkeypatch.setenv("HOME", str(home))
    monkeypatch.setenv("USERPROFILE", str(home))
    monkeypatch.delenv("ACE_PRIVATE_REPOS", raising=False)
    monkeypatch.delenv("ACE_PRIVATE_CONFIG", raising=False)
    checkouts = tmp_path / "checkouts"
    checkouts.mkdir()
    for name in ("digitalmodel", "client-c29b", "sample-one", "sample-two", "file-only"):
        (checkouts / name).mkdir()
    (checkouts / "plain-file").touch()
    return checkouts


def test_environment_overrides_config(checkouts, tmp_path, monkeypatch):
    config = tmp_path / "private-repos.txt"
    config.write_text("file-only\n", encoding="utf-8")
    monkeypatch.setenv("ACE_PRIVATE_CONFIG", str(config))
    monkeypatch.setenv(
        "ACE_PRIVATE_REPOS",
        f" sample-one ,,sample-two,sample-one,digitalmodel,missing,plain-file,..,{tmp_path}",
    )
    assert GitManager(str(checkouts)).repos == ["digitalmodel", "sample-one", "sample-two"]


@pytest.mark.parametrize("use_default", [False, True])
def test_file_supplies_one_name_per_line(checkouts, tmp_path, monkeypatch, use_default):
    if use_default:
        config = tmp_path / "home" / ".config" / "ace" / "private-repos.txt"
    else:
        config = tmp_path / "configured.txt"
        monkeypatch.setenv("ACE_PRIVATE_CONFIG", str(config))
    config.parent.mkdir(parents=True, exist_ok=True)
    config.write_text("sample-two\n\n sample-one ", encoding="utf-8")
    assert GitManager(str(checkouts)).repos == ["digitalmodel", "sample-two", "sample-one"]


def test_absent_configuration_adds_no_private_checkouts(checkouts):
    assert GitManager(str(checkouts)).repos == ["digitalmodel"]
