"""Fail-closed contracts for a single licensed reproduction, without a solver."""
from pathlib import Path
import json
import yaml

import pytest

from digitalmodel.workflows import orcaflex_reproduce as repro


def complete_metadata():
    return dict(simulation_complete=True, run_status="SimulationStopped",
                start_time=-10.0, stop_time=11.0, current_time=11.0)


def test_completed_metadata_requires_original_endpoints():
    result = repro.validate_completion(complete_metadata(), [-10.0, 11.0])
    assert result["simulation_stop"] == 11.0


@pytest.mark.parametrize("key,value", [
    ("simulation_complete", False), ("run_status", None),
    ("run_status", "SimulationStoppedUnstable"), ("current_time", 10.0),
    ("stop_time", 10.0), ("start_time", 0.0), ("current_time", float("nan")),
])
def test_incomplete_or_unknown_evidence_is_rejected(key, value):
    metadata = complete_metadata()
    metadata[key] = value
    with pytest.raises(ValueError):
        repro.validate_completion(metadata, [-10.0, 11.0])


def test_sample_coverage_requires_finite_monotonic_complete_series():
    repro.validate_samples([-10.0, -9.9, -9.8], [-10.0, -9.8], 0.1)
    for samples in ([], [-9.9], [-10.0, float("nan"), -9.8],
                    [-10.0, -10.0, -9.8], [-10.0, -9.8]):
        with pytest.raises(ValueError):
            repro.validate_samples(samples, [-10.0, -9.8], 0.1)


def test_batch_configuration_has_one_unmodified_real_case(tmp_path):
    cfg = repro.batch_config(tmp_path / "source" / "model.yml", tmp_path)
    settings = cfg["orcaflex_run_batch"]
    assert len(settings["models"]["files"]) == 1
    assert settings["analysis"] == {"type": "both"}
    assert settings["run_batch"]["workers"] == 1
    assert settings["run_batch"]["mock"] is False
    assert "variants" not in settings


@pytest.mark.parametrize("update", [
    {"completed": 0}, {"failed": 1}, {"mock": True},
    {"total_cases": 2}, {"analysis_type": "statics"},
])
def test_batch_success_cannot_hide_mock_partial_or_static_run(update):
    summary = dict(completed=1, failed=0, mock=False, total_cases=1,
                   analysis_type="both")
    summary.update(update)
    with pytest.raises(ValueError):
        repro.validate_batch(summary)


def test_snapshot_reuses_hash_and_preserves_original(tmp_path):
    source = tmp_path / "original.yml"
    source.write_bytes(b"%YAML 1.1\n---\nGeneral: {}\n")
    output = tmp_path / "run"
    output.mkdir()
    digest = repro.compute_hash(source)
    copied = repro.snapshot_model(source, output, digest)
    assert copied.read_bytes() == source.read_bytes()
    assert repro.compute_hash(copied) == digest
    with pytest.raises(ValueError, match="digest"):
        repro.snapshot_model(source, tmp_path / "wrong", "0" * 64)


def test_child_environment_removes_git_bindings(monkeypatch):
    for name in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"):
        monkeypatch.setenv(name, "foreign-fixture")
    env = repro.child_environment("vendor.dll")
    assert not any(name in env for name in
                   ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"))
    assert env["_OrcFxAPIlib"] == "vendor.dll"
    assert Path(env["PYTHONPATH"].split(__import__('os').pathsep)[0]).name == "src"


def test_child_preserves_custom_api_search_path(monkeypatch):
    monkeypatch.setenv('PYTHONPATH', 'vendor-python')
    assert repro.child_environment('vendor.dll')['PYTHONPATH'].endswith('vendor-python')


def test_scalar_limitations_rejected_before_solve(tmp_path):
    request = tmp_path / 'request.yml'
    request.write_text(yaml.safe_dump(dict(model='model.yml', model_sha256='a'*64,
        extraction={'period': [0, 1]}, limitations='diagnostic')))
    with pytest.raises(ValueError, match='limitations'):
        repro._read_config(request)


def test_dependency_model_rejected_before_copy(tmp_path):
    source = tmp_path / 'dependent.yml'
    source.write_text('BaseFile: other.yml\n')
    with pytest.raises(ValueError, match='standalone'):
        repro.snapshot_model(source, tmp_path / 'run', repro.compute_hash(source))


@pytest.fixture
def resume_case(tmp_path, monkeypatch):
    output = tmp_path / 'run'
    source = output / 'source/model.yml'
    source.parent.mkdir(parents=True)
    source.write_text('model')
    sim = output / 'batch_runs/sims/model.sim'
    sim.parent.mkdir(parents=True)
    sim.write_text('simulation')
    solver = {'resolved_version': '11.6c', 'resolved_lib_path': 'original.dll'}
    previous = {'model_sha256': repro.compute_hash(source), 'simulation_sha256': repro.compute_hash(sim),
                'code': {'git_revision': 'solve-revision'}, 'solver': solver, 'solver_version': '11.6c'}
    (output / 'run.json').write_text(json.dumps(previous))
    config = {'model': 'model.yml', 'model_sha256': previous['model_sha256'],
              'solver_version': '11.6c', 'extraction': {'period': [0, 11]}, 'limitations': ['diagnostic']}
    request = tmp_path / 'request.yml'
    request.write_text(yaml.safe_dump(config))
    monkeypatch.setattr(repro, '_code_identity', lambda: {'git_revision': 'postprocess-revision'})
    monkeypatch.setattr(repro, '_load_api', lambda cfg: (object(), dict(solver)))
    monkeypatch.setattr(repro, '_readback', lambda *a: sim)
    monkeypatch.setattr(repro.DataProvenance, 'save_alongside', lambda *a: None)
    return request, output, config, previous


@pytest.mark.parametrize('failure', ['source', 'simulation', 'solver'])
def test_rejected_resume_preserves_original_receipt(resume_case, monkeypatch, failure):
    request, output, cfg, previous = resume_case
    before = (output / 'run.json').read_bytes()
    if failure == 'source': (output / 'source/model.yml').write_text('tampered')
    if failure == 'simulation': (output / 'batch_runs/sims/model.sim').write_text('tampered')
    if failure == 'solver':
        monkeypatch.setattr(repro, '_load_api', lambda cfg: (object(), {'resolved_version': '12.0', 'resolved_lib_path': 'new.dll'}))
    with pytest.raises(ValueError):
        repro.reproduce(request, output, postprocess_only=True)
    assert (output / 'run.json').read_bytes() == before


def test_resume_keeps_solver_code_and_separate_postprocess_identity(resume_case):
    request, output, cfg, previous = resume_case
    result = repro.reproduce(request, output, postprocess_only=True)
    assert result['code'] == previous['code']
    assert result['solver'] == previous['solver']
    assert result['solver_version'] == previous['solver_version']
    assert result['postprocess']['code']['git_revision'] == 'postprocess-revision'
    assert result['postprocess']['request_sha256'] == repro.compute_hash(request)


def test_code_identity_includes_current_workflow_bytes():
    identity = repro._code_identity()
    hashes = identity['workflow_sha256']
    for name in ('orcaflex_reproduce.py', 'orcaflex_reproduce_results.py'):
        path = Path(repro.__file__).parent / name
        assert hashes[name] == repro.compute_hash(path)


def test_resume_wrong_requested_version_preserves_receipt(resume_case):
    request, output, cfg, previous = resume_case
    before = (output / 'run.json').read_bytes()
    cfg['solver_version'] = '12.0'
    request.write_text(yaml.safe_dump(cfg))
    with pytest.raises(ValueError, match='requested solver'):
        repro.reproduce(request, output, postprocess_only=True)
    assert (output / 'run.json').read_bytes() == before


@pytest.mark.parametrize('resume', [False, True])
def test_readback_metadata_is_detached_from_parent_receipt(tmp_path, monkeypatch, resume):
    from types import SimpleNamespace as NS
    import sys
    sim = tmp_path / 'batch_runs/sims/model.sim'
    sim.parent.mkdir(parents=True)
    sim.write_text('simulation')
    receipt = {'preflight': {'endpoints': [-10., 11.], 'logging_interval': 0.9}}
    if resume:
        receipt['postprocess'] = {'started_at': 'now'}
    existing = tmp_path / 'extracted/report.html'
    existing.parent.mkdir()
    existing.write_text('original report')
    model = NS(SampleTimes=lambda p: list(range(-10, 12)), warnings=[],
               general=NS(ActualLogSampleInterval=1.))
    metadata = {**complete_metadata(), 'model': model}
    utilities = NS(OrcaflexUtilities=lambda: NS(get_model_and_metadata=lambda p: metadata))
    monkeypatch.setitem(sys.modules, 'digitalmodel.solvers.orcaflex.orcaflex_utilities', utilities)
    from digitalmodel.workflows import orcaflex_reproduce_results
    monkeypatch.setattr(orcaflex_reproduce_results, 'extract_results', lambda m, c, o, r: {'run_metadata': r, 'output': str(o)})
    api = NS(Period=lambda p: p, pnWholeSimulation=0)
    repro._readback(api, Path('model.yml'), tmp_path, receipt, {'extraction': {'period': [0, 11]}})
    assert receipt['extraction']['run_metadata'] is not receipt
    name = Path(receipt['extraction']['output']).name
    assert name.startswith('extracted-postprocess-') if resume else name == 'extracted'
    assert existing.read_text() == 'original report'
    json.dumps(receipt)


def test_batch_child_configures_api_before_importing_router(tmp_path, monkeypatch):
    import builtins
    from types import SimpleNamespace as NS
    request = tmp_path / 'input.yml'
    request.write_text(yaml.safe_dump(repro.batch_config(tmp_path / 'model.yml', tmp_path)))
    events = []
    monkeypatch.setattr(repro, '_load_api', lambda cfg: events.append('api'))
    original_import = builtins.__import__
    def intercept(name, *args, **kwargs):
        if name == 'digitalmodel.workflows.orcaflex_run_batch':
            assert events == ['api']
            events.append('import')
            return NS(router=lambda cfg: events.append(cfg))
        return original_import(name, *args, **kwargs)
    monkeypatch.setattr(builtins, '__import__', intercept)
    repro._batch_child(request)
    assert events[:2] == ['api', 'import']
    assert events[2]['_config_dir_path'] == str(tmp_path)
    assert events[2]['orcaflex_run_batch']['run_batch']['mock'] is False


def test_solve_launches_pinned_batch_child(tmp_path, monkeypatch):
    from types import SimpleNamespace as NS
    commands = []
    monkeypatch.setattr(repro.subprocess, 'Popen', lambda command, **kwargs: (commands.append(command) or NS(wait=lambda **k: 0)))
    (tmp_path / 'results').mkdir()
    (tmp_path / 'results/batch_summary.json').write_text(json.dumps({
        'total_cases': 1, 'completed': 1, 'failed': 0, 'mock': False, 'analysis_type': 'both'}))
    repro._solve(tmp_path / 'input.yml', tmp_path, 'pinned.dll', 10)
    assert commands[0][1:3] == ['-m', 'digitalmodel.workflows.orcaflex_reproduce']
    assert commands[0][-1] == '--batch-child'
