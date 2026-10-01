"""Native progress logging must preserve solver callback/cancellation semantics."""
from types import SimpleNamespace as NS

import pytest

from digitalmodel.solvers.orcaflex.dynamics_progress import dynamics_progress


def test_rate_limit_and_completion_preserve_existing_cancellation():
    clock_values = iter([0., 0., 10., 30., 31.])
    logs, calls = [], []
    def previous(*args):
        calls.append(args[1])
        return args[1] == 600
    model = NS(dynamicsProgressHandler=previous)
    logger = NS(info=lambda *args: logs.append(args))
    with dynamics_progress(model, 30, logger, 'case.yml', clock=lambda: next(clock_values)):
        for simulation_time in (-80, 0, 100, 600):
            assert model.dynamicsProgressHandler(model, simulation_time, -80, 600) == (simulation_time == 600)
    assert calls == [-80, 0, 100, 600]
    assert len(logs) == 3
    assert model.dynamicsProgressHandler is previous
    assert logs[-1][3:] == (600., 680., -80., 600.)


def test_disabled_does_not_touch_callback():
    previous = object()
    model = NS(dynamicsProgressHandler=previous)
    with dynamics_progress(model, None, None, 'case'):
        assert model.dynamicsProgressHandler is previous


def test_solver_exception_restores_handler_and_propagates():
    model = NS(dynamicsProgressHandler=None)
    with pytest.raises(RuntimeError, match='solver failure'):
        with dynamics_progress(model, 30, NS(info=lambda *a: None), 'case'):
            raise RuntimeError('solver failure')
    assert model.dynamicsProgressHandler is None


def test_existing_callback_exception_is_not_suppressed():
    def previous(*args):
        raise RuntimeError('prior callback')
    model = NS(dynamicsProgressHandler=previous)
    with pytest.raises(RuntimeError, match='prior callback'):
        with dynamics_progress(model, 30, NS(info=lambda *a: None), 'case'):
            model.dynamicsProgressHandler(model, 0, 0, 600)
    assert model.dynamicsProgressHandler is previous


@pytest.mark.parametrize('interval', [0, -1, float('nan'), float('inf')])
def test_invalid_interval_rejected(interval):
    model = NS(dynamicsProgressHandler=None)
    with pytest.raises(ValueError):
        with dynamics_progress(model, interval, None, 'case'):
            pass


def test_reproduction_config_enables_progress():
    from digitalmodel.workflows.orcaflex_reproduce import batch_config
    cfg = batch_config('model.yml', 'output')
    assert cfg['orcaflex_run_batch']['run_batch']['progress_interval_seconds'] == 30


@pytest.mark.parametrize('fail', [False, True])
def test_worker_installs_callback_and_keeps_failure_state(tmp_path, monkeypatch, fail):
    from digitalmodel.solvers.orcaflex import orcaflex_parallel_analysis as worker
    calls = []
    model = NS(dynamicsProgressHandler=None, state=NS(name='SimulationStopped'),
               LoadData=lambda p: None)
    def simulate():
        assert callable(model.dynamicsProgressHandler)
        assert model.dynamicsProgressHandler(model, 5., -80., 600.) is False
        calls.append('dynamics')
        if fail: raise RuntimeError('solver failure')
    model.RunSimulation = simulate
    monkeypatch.setattr(worker, 'OrcFxAPI', NS(Model=lambda: model), raising=False)
    monkeypatch.setattr(worker, 'ORCAFLEX_AVAILABLE', True)
    analyzer = worker.OrcaFlexParallelAnalysis.__new__(worker.OrcaFlexParallelAnalysis)
    result = analyzer.process_single_file({'file_path': str(tmp_path/'model.yml'), 'config': {
        'static': False, 'dynamic': True, 'save_sim': False, 'progress_interval_seconds': 30}})
    assert calls == ['dynamics']
    assert result['status'] == ('failed' if fail else 'success')
    assert model.dynamicsProgressHandler is None


def test_batch_router_passes_optional_progress_config(tmp_path, monkeypatch):
    from digitalmodel.workflows import orcaflex_run_batch as batch
    captured = []
    original = batch._MockOrcaFlexBatch.process_files_parallel
    def capture(self, files, config):
        captured.append(config)
        return original(self, files, config)
    monkeypatch.setattr(batch._MockOrcaFlexBatch, 'process_files_parallel', capture)
    source = tmp_path/'model.yml'
    source.write_text('General: {}\n')
    batch.router({'_config_dir_path': str(tmp_path), 'orcaflex_run_batch': {
        'models': {'files': [str(source)]}, 'analysis': {'type': 'both'},
        'run_batch': {'mock': True, 'workers': 1, 'progress_interval_seconds': 30}}})
    assert captured[0]['progress_interval_seconds'] == 30
