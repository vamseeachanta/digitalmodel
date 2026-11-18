"""Offline discovery contracts; synthetic launchers are never executed."""

from pathlib import Path

import pytest

from digitalmodel.ansys import runner as module
from digitalmodel.ansys.runner import ANSYSRunConfig, ANSYSRunner

ENV_NAMES = ("ANSYS_MAPDL_PATH", "MAPDL_PATH", "ANSYS_EXECUTABLE")


def _unexpected(*args, **kwargs):
    pytest.fail("discovery reached an unexpected fallback or process boundary")


@pytest.fixture(autouse=True)
def isolated_discovery(monkeypatch):
    for name in ENV_NAMES:
        monkeypatch.delenv(name, raising=False)
    monkeypatch.setattr(module.platform, "system", lambda: "Linux")
    monkeypatch.setattr(module.shutil, "which", lambda name: None)
    monkeypatch.setattr(module, "_run_solver_process", _unexpected)


@pytest.fixture
def installation_root(tmp_path, monkeypatch):
    root = tmp_path / "installations"
    root.mkdir()

    def synthetic_path(value):
        if value == r"C:\Program Files\ANSYS Inc":
            return root
        return Path(value)

    monkeypatch.setattr(module, "Path", synthetic_path)
    monkeypatch.setattr(module.platform, "system", lambda: "Windows")
    return root


def _launcher(root, release):
    executable = root / release / "ansys" / "bin" / "winx64" / "MAPDL.exe"
    executable.parent.mkdir(parents=True)
    executable.write_text("synthetic launcher; never executed", encoding="utf-8")
    return executable


def test_standard_releases_use_numeric_descending_order(installation_root):
    expected = [_launcher(installation_root, name) for name in ("v261", "v100", "v99")]
    assert module._standard_paths() == expected


def test_malformed_release_names_are_ignored(installation_root):
    expected = _launcher(installation_root, "v261")
    for name in ("v", "v262beta", "v263.0", "v264-old", "v+265", "v٢٦٦"):
        _launcher(installation_root, name)
    assert module._standard_paths() == [expected]


def test_equal_numeric_releases_have_stable_name_tie_break(installation_root):
    padded = _launcher(installation_root, "v099")
    plain = _launcher(installation_root, "v99")
    assert module._standard_paths() == [plain, padded]
    assert module._standard_paths() == [plain, padded]


def test_standard_candidates_must_be_files(installation_root):
    expected = _launcher(installation_root, "v99")
    directory = installation_root / "v261" / "ansys" / "bin" / "winx64" / "MAPDL.exe"
    directory.mkdir(parents=True)
    (installation_root / "v100").mkdir()
    (installation_root / "v999").write_text("not a directory", encoding="utf-8")
    assert module._standard_paths() == [expected]


def test_missing_standard_root_returns_empty(installation_root, monkeypatch):
    monkeypatch.setattr(module, "Path", lambda value: installation_root / "absent")
    assert module._standard_paths() == []


@pytest.mark.parametrize("system", ["Linux", "Darwin"])
def test_non_windows_does_not_probe_filesystem(monkeypatch, system):
    monkeypatch.setattr(module.platform, "system", lambda: system)
    monkeypatch.setattr(module, "Path", _unexpected)
    assert module._standard_paths() == []


def test_explicit_configuration_outranks_all_discovery(tmp_path, monkeypatch):
    executable = _launcher(tmp_path, "configured")
    monkeypatch.setenv(ENV_NAMES[0], str(_launcher(tmp_path, "environment")))
    monkeypatch.setattr(module, "_standard_paths", _unexpected)
    monkeypatch.setattr(module.shutil, "which", _unexpected)
    runner = ANSYSRunner(ANSYSRunConfig(executable_path=executable))
    assert runner._detect_executable() == executable.resolve()


def test_missing_explicit_configuration_refuses_fallback(tmp_path, monkeypatch):
    monkeypatch.setenv(ENV_NAMES[0], str(_launcher(tmp_path, "environment")))
    monkeypatch.setattr(module, "_standard_paths", _unexpected)
    monkeypatch.setattr(module.shutil, "which", _unexpected)
    runner = ANSYSRunner(ANSYSRunConfig(executable_path=tmp_path / "missing"))
    assert runner._detect_executable() is None


@pytest.mark.parametrize("selected", range(len(ENV_NAMES)))
def test_environment_precedence_is_preserved(tmp_path, monkeypatch, selected):
    expected = None
    for index, name in enumerate(ENV_NAMES):
        executable = _launcher(tmp_path, name)
        monkeypatch.setenv(name, str(tmp_path / "absent") if index < selected else str(executable))
        if index == selected:
            expected = executable
    monkeypatch.setattr(module, "_standard_paths", _unexpected)
    monkeypatch.setattr(module.shutil, "which", _unexpected)
    assert ANSYSRunner()._detect_executable() == expected


def test_newest_standard_installation_outranks_stale_path(installation_root, monkeypatch):
    _launcher(installation_root, "v99")
    expected = _launcher(installation_root, "v261")
    calls = []

    def stale_path(name):
        calls.append(name)
        return str(installation_root / "stale" / "mapdl")

    monkeypatch.setattr(module.shutil, "which", stale_path)
    assert ANSYSRunner()._detect_executable() == expected
    assert calls == []
    assert ANSYSRunner()._detect_executable() == expected


@pytest.mark.parametrize("selected", range(4))
def test_missing_standards_preserve_path_fallback_names(installation_root, monkeypatch, selected):
    names = ("mapdl", "ansys", "MAPDL.exe", "ansys.exe")
    expected = installation_root / "generic-launcher"
    expected.write_text("synthetic launcher; never executed", encoding="utf-8")
    calls = []

    def lookup(name):
        calls.append(name)
        return str(expected) if name == names[selected] else None

    monkeypatch.setattr(module.shutil, "which", lookup)
    assert ANSYSRunner()._detect_executable() == expected
    assert calls == list(names[:selected + 1])


def test_invalid_environment_falls_through_to_standards(installation_root, monkeypatch):
    monkeypatch.setenv(ENV_NAMES[0], str(installation_root / "missing"))
    monkeypatch.setenv(ENV_NAMES[1], str(installation_root))
    monkeypatch.setenv(ENV_NAMES[2], "")
    expected = _launcher(installation_root, "v261")
    monkeypatch.setattr(module.shutil, "which", _unexpected)
    assert ANSYSRunner()._detect_executable() == expected


def test_no_candidates_returns_none():
    assert ANSYSRunner()._detect_executable() is None
