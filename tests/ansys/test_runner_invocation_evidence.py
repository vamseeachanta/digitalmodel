"""Offline regressions for relative invocation and timeout evidence."""
from pathlib import Path
from types import SimpleNamespace
import subprocess

import pytest

from digitalmodel.ansys.runner import ANSYSRunner, ANSYSRunStatus, run_ansys


def test_relative_paths_resolve_before_solver_changes_directory(tmp_path, monkeypatch):
    monkeypatch.chdir(tmp_path)
    Path('model.inp').write_text('FINISH')
    monkeypatch.setattr(ANSYSRunner, '_detect_executable', lambda self: Path('fake'))

    def simulate(argv, **kwargs):
        cwd = Path(kwargs['cwd'])
        source = cwd / argv[argv.index('-i') + 1]
        target = cwd / argv[argv.index('-o') + 1]
        assert source.is_file()
        target.write_text('completed')
        return SimpleNamespace(returncode=0, stdout='', stderr='')

    monkeypatch.setattr('digitalmodel.ansys.runner.subprocess.run', simulate)
    result = run_ansys('model.inp')
    assert result.status == ANSYSRunStatus.COMPLETED
    assert result.log_file == tmp_path / 'output/model.out'


@pytest.mark.parametrize('streams', [(b'partial output', b'partial error'), ('partial output', 'partial error')])
def test_timeout_preserves_partial_evidence(tmp_path, monkeypatch, streams):
    script = tmp_path / 'model.inp'
    script.write_text('FINISH')
    output = tmp_path / 'output'
    monkeypatch.setattr(ANSYSRunner, '_detect_executable', lambda self: Path('fake'))

    def simulate(argv, **kwargs):
        (output / 'model.out').write_text('partial analysis')
        (output / 'digest.csv').write_text('partial,value')
        raise subprocess.TimeoutExpired(argv, 1, output=streams[0], stderr=streams[1])

    monkeypatch.setattr('digitalmodel.ansys.runner.subprocess.run', simulate)
    result = run_ansys(script, output_dir=output)
    assert result.status == ANSYSRunStatus.FAILED
    assert 'timeout' in result.error_message.lower()
    assert result.return_code is None  # No observed exit code may be invented.
    assert result.stdout == 'partial output'
    assert result.stderr == 'partial error'
    assert result.log_file == output / 'model.out'
    assert output / 'digest.csv' in result.result_files


def test_prepare_failure_returns_failed_record(tmp_path):
    output = tmp_path / 'occupied'
    output.write_text('file')
    result = run_ansys(tmp_path / 'model.inp', output_dir=output)
    assert result.status == ANSYSRunStatus.FAILED
    assert result.return_code is None
    assert result.error_message


def test_longest_error_marker_with_minimal_cross_boundary_tail(tmp_path, monkeypatch):
    import io
    marker = 'SOLUTION NOT CONVERGED'
    text = 'x' * (65536 - len(marker) + 1) + marker + 'x' * 70000

    class BoundedStream(io.StringIO):
        def read(self, size=-1):
            assert 0 < size <= 65536, 'log reads must be bounded'
            return super().read(size)

    monkeypatch.setattr(Path, 'open', lambda *a, **k: BoundedStream(text))
    assert marker in ANSYSRunner()._detect_error(0, tmp_path / 'model.out')
