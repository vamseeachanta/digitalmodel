"""Python 3.11-compatible directory-junction probe (``Path.is_junction`` is 3.12+)."""
import os
import pathlib
import subprocess
import sys

import pytest

from digitalmodel.ansys import _path_compat
from digitalmodel.ansys._path_compat import is_junction


def _without_native(monkeypatch):
    monkeypatch.delattr(pathlib.Path, 'is_junction', raising=False)


def test_regular_file_and_directory_are_not_junctions(tmp_path):
    file = tmp_path / 'file.txt'
    file.write_text('x')
    assert is_junction(tmp_path) is False
    assert is_junction(file) is False
    assert is_junction(str(file)) is False


def test_missing_path_is_not_a_junction(tmp_path):
    assert is_junction(tmp_path / 'absent') is False


def test_fallback_without_native_method_on_plain_paths(tmp_path, monkeypatch):
    _without_native(monkeypatch)
    assert not hasattr(pathlib.Path(tmp_path), 'is_junction')
    assert is_junction(tmp_path) is False
    assert is_junction(tmp_path / 'absent') is False


def test_non_windows_fallback_never_reports_a_junction(tmp_path, monkeypatch):
    _without_native(monkeypatch)
    monkeypatch.setattr(_path_compat, '_WINDOWS', False)
    monkeypatch.setattr(_path_compat.os, 'lstat', lambda _: pytest.fail('POSIX has no junctions'))
    assert is_junction(tmp_path) is False


def test_symlink_is_not_reported_as_junction(tmp_path):
    target = tmp_path / 'target'
    target.mkdir()
    link = tmp_path / 'link'
    try:
        link.symlink_to(target, target_is_directory=True)
    except OSError as error:
        pytest.skip(f'symlink creation unavailable: {error}')
    assert is_junction(link) is False


@pytest.mark.skipif(sys.platform != 'win32', reason='directory junctions exist only on Windows')
@pytest.mark.parametrize('native', [True, False], ids=['native', 'fallback'])
def test_real_windows_junction_is_detected(tmp_path, monkeypatch, native):
    target = tmp_path / 'target'
    target.mkdir()
    junction = tmp_path / 'junction'
    subprocess.run(['cmd', '/c', 'mklink', '/J', str(junction), str(target)],
                   check=True, capture_output=True)
    try:
        if not native:
            _without_native(monkeypatch)
        assert is_junction(junction) is True
        assert is_junction(target) is False
    finally:
        os.rmdir(junction)
