"""Native thread budget must reach the model, not just the process pool."""
from types import SimpleNamespace

import pytest

from digitalmodel.solvers.orcaflex import orcaflex_parallel_analysis as parallel
from digitalmodel.workflows import orcaflex_reproduce as reproduce
from digitalmodel.workflows import orcaflex_run_batch as batch


def test_installation_reproduction_explicitly_caps_native_threads(tmp_path):
    config = reproduce.batch_config(tmp_path / 'model.yml', tmp_path)
    assert config['orcaflex_run_batch']['run_batch']['solver_threads'] == 1


@pytest.mark.parametrize('threads', [1, 2])
def test_leaf_passes_explicit_native_thread_count(monkeypatch, tmp_path, threads):
    seen = []
    def model(**kwargs):
        seen.append(kwargs)
        return SimpleNamespace(LoadData=lambda path: None, threadCount=threads)
    monkeypatch.setattr(parallel, 'ORCAFLEX_AVAILABLE', True)
    monkeypatch.setattr(parallel, 'OrcFxAPI', SimpleNamespace(Model=model), raising=False)
    result = parallel.OrcaFlexParallelAnalysis(1).process_single_file({
        'file_path': str(tmp_path / 'model.yml'), 'config': {
            'solver_threads': threads, 'static': False, 'dynamic': False,
            'save_sim': False, 'output_dir': str(tmp_path)}})
    assert result['status'] == 'success'
    assert seen == [{'threadCount': threads}]


def test_batch_forwards_native_budget(monkeypatch, tmp_path):
    captured = []
    monkeypatch.setattr(batch, '_license_available', lambda: True)
    def stop(self, files, config):
        captured.append(config)
        raise RuntimeError('capture complete')
    monkeypatch.setattr(batch.OrcaFlexParallelAnalysis, 'process_files_parallel', stop)
    cfg = reproduce.batch_config(tmp_path / 'model.yml', tmp_path)
    with pytest.raises(RuntimeError, match='capture complete'):
        batch.router(cfg)
    assert captured[0]['solver_threads'] == 1


@pytest.mark.parametrize('threads', [0, -1, True, 1.5])
def test_invalid_native_budget_fails_before_model(monkeypatch, tmp_path, threads):
    def model(**kwargs):
        pytest.fail('invalid budget reached native model')
    monkeypatch.setattr(parallel, 'ORCAFLEX_AVAILABLE', True)
    monkeypatch.setattr(parallel, 'OrcFxAPI', SimpleNamespace(Model=model), raising=False)
    result = parallel.OrcaFlexParallelAnalysis(1).process_single_file({
        'file_path': str(tmp_path / 'model.yml'), 'config': {'solver_threads': threads}})
    assert result['status'] == 'failed'
    assert 'positive integer' in result['error']


def test_native_budget_mismatch_prevents_solving(monkeypatch, tmp_path):
    model = SimpleNamespace(LoadData=lambda path: None, threadCount=64)
    monkeypatch.setattr(parallel, 'ORCAFLEX_AVAILABLE', True)
    monkeypatch.setattr(parallel, 'OrcFxAPI', SimpleNamespace(Model=lambda **kw: model), raising=False)
    result = parallel.OrcaFlexParallelAnalysis(1).process_single_file({
        'file_path': str(tmp_path / 'model.yml'), 'config': {'solver_threads': 1}})
    assert result['status'] == 'failed'
    assert 'differs from requested' in result['error']


def test_observed_native_budget_survives_batch_receipt(monkeypatch, tmp_path):
    monkeypatch.setattr(batch, '_license_available', lambda: True)
    def pool(self, files, config):
        return {'results': [{'file_path': files[0], 'status': 'success',
                            'solver_threads_requested': 1, 'solver_threads': 1}]}
    monkeypatch.setattr(batch.OrcaFlexParallelAnalysis, 'process_files_parallel', pool)
    cfg = batch.router(reproduce.batch_config(tmp_path / 'model.yml', tmp_path))
    import json
    receipt = json.loads((tmp_path / 'results/batch_summary.json').read_text())
    assert receipt['native_thread_budget'] == [{'index': 0, 'requested': 1, 'observed': 1}]
    assert cfg['orcaflex_run_batch']['cases'][0]['solver_threads'] == 1


def test_parallel_import_keeps_solver_version_gate_available():
    import subprocess
    import sys
    result = subprocess.run([sys.executable, '-c',
        'import sys; from digitalmodel.solvers.orcaflex import orcaflex_parallel_analysis; '
        'assert "OrcFxAPI" not in sys.modules'], capture_output=True, text=True)
    assert result.returncode == 0, result.stderr
