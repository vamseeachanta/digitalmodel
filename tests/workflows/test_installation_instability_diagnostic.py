"""Single-case instability diagnostic with a fake OrcFxAPI; no licensed solver calls."""
import json
from types import SimpleNamespace as NS

import numpy as np
import pytest

from digitalmodel.workflows import installation_instability_diagnostic as diag

PNG = object()


class Line(NS):
    def TimeHistory(self, variable, period=None, objectExtra=None):
        self.model.calls.append((self.name, variable, period, objectExtra))
        values = self.model.series[(self.name, objectExtra)]
        return np.asarray(values[:self.model.samples])


class Model:
    def __init__(self, state, stop, raise_run=False):
        self.general = NS(ImplicitConstantTimeStep=0.05, StageDuration=[80.0, 600.0])
        self.state_name, self.raise_run = state, raise_run
        self.samples = int(stop / 0.5) + 1
        self.simulationTimeStatus = NS(CurrentTime=stop)
        self.warnings = ('Line Sling2 compression',)
        self.series = {('Sling2', 'A'): list(range(2000)), ('Sling2', 'B'): [5] * 2000,
                       ('Crane', 'A'): [100.0] * 2000, ('Crane', 'B'): []}
        self.objects = [NS(name='Environment', typeName='Environment'),
                        Line(name='Sling2', typeName='Line', model=self),
                        Line(name='Crane', typeName='Line', model=self)]
        self.calls, self.views = [], []
        self.defaultViewParameters = NS(ViewSize=0, Width=0, Height=0, ViewAzimuth=0, ViewElevation=0, FileFormat=None)

    def LoadData(self, path): pass
    def RunSimulation(self):
        if self.raise_run: raise RuntimeError('solver error')
    def SaveSimulation(self, path): open(path, 'wb').write(b'sim')
    def SaveModelView(self, path, vp):
        assert vp.FileFormat is PNG and (vp.Width, vp.Height) == (1280, 960)
        self.views.append(vp.ViewAzimuth); open(path, 'wb').write(b'img')
    def SampleTimes(self, period=None): return np.arange(self.samples) * 0.5


@pytest.fixture
def fake(monkeypatch, tmp_path):
    made, kwargs = [], []
    def factory(state='SimulationStoppedUnstable', stop=4.0, raise_run=False):
        def model(**kw):
            kwargs.append(kw); made.append(Model(state, stop, raise_run)); return made[-1]
        return NS(Model=model, oeEndA='A', oeEndB='B', pnWholeSimulation='whole', BitmapFileFormat=NS(PNG=PNG))
    monkeypatch.setattr(diag, '_state', lambda model: model.state_name)
    monkeypatch.setattr(diag, '_pin', lambda cpus: list(cpus))
    model = tmp_path / 'model.yml'
    model.write_text('General: {}\n')
    return factory, made, kwargs, model


def test_unstable_run_keeps_partial_simulation_views_and_histories(fake, tmp_path):
    factory, made, kwargs, model = fake
    out = tmp_path / 'run'
    record = diag.run_case(model, out, cpus=[60], api=factory(), time_step=0.025)
    assert kwargs == [{'threadCount': 1}] and record['cpu_affinity'] == [60]
    assert made[0].general.ImplicitConstantTimeStep == 0.025 and record['time_step_s'] == 0.025
    assert record['outcome'] == 'unstable' and record['stop_time_s'] == 4.0 and record['end_time_s'] == 600.0
    assert record['simulation'] == 'unstable.sim' and (out / 'unstable.sim').exists()
    assert set(record['views']) == {'elevation', 'plan', 'perspective'}
    sling = record['line_ends']['Sling2']['End A']
    assert sling['maximum'] == 8 and sling['time_of_maximum_s'] == 4.0 and sling['samples'] == 9
    assert ('Sling2', 'Effective tension', 'whole', 'A') in made[0].calls
    assert 'history Crane|End B' in record['stage_errors']
    arrays = np.load(out / 'line_end_tension.npz')
    assert 'Crane|End B' not in arrays and list(arrays['time']) == list(np.arange(9) * 0.5)
    assert json.loads((out / 'diagnostic.json').read_text()) == record


@pytest.mark.parametrize('state,stop,outcome', [('SimulationStopped', 600.0, 'completed'),
                                                ('SimulationStopped', 300.0, 'incomplete'),
                                                (None, 300.0, 'unknown'), ('Reset', 0.0, 'incomplete')])
def test_outcome_distinguishes_completed_incomplete_and_unknown(fake, tmp_path, state, stop, outcome):
    factory, _, _, model = fake
    record = diag.run_case(model, tmp_path / 'r', cpus=[60], api=factory(state, stop))
    assert record['outcome'] == outcome
    assert (tmp_path / 'r' / ('complete.sim' if outcome == 'completed' else f'{outcome}.sim')).exists()


def test_solver_exception_still_writes_manifest_with_available_evidence(fake, tmp_path):
    factory, _, _, model = fake
    record = diag.run_case(model, tmp_path / 'x', cpus=[60], api=factory(raise_run=True))
    assert 'solver error' in record['stage_errors']['run']
    assert json.loads((tmp_path / 'x' / 'diagnostic.json').read_text())['stage_errors']['run']


def test_view_failure_is_recorded_not_fatal(fake, tmp_path, monkeypatch):
    factory, _, _, model = fake
    monkeypatch.setattr(Model, 'SaveModelView', lambda self, p, vp: (_ for _ in ()).throw(RuntimeError('no graphics')))
    record = diag.run_case(model, tmp_path / 'nv', cpus=[60], api=factory())
    assert record['views'] == {} and 'no graphics' in record['view_errors']['elevation']
    assert record['line_ends']['Sling2']


def test_inputs_rejected_before_output(fake, tmp_path):
    factory, _, _, model = fake
    (tmp_path / 'exists').mkdir()
    with pytest.raises(FileExistsError):
        diag.run_case(model, tmp_path / 'exists', cpus=[60], api=factory())
    for step in (0, -1, float('nan')):
        with pytest.raises(ValueError):
            diag.run_case(model, tmp_path / f'bad{step}', cpus=[60], api=factory(), time_step=step)
    with pytest.raises(ValueError):
        diag.run_case(model, tmp_path / 'nocpu', cpus=[], api=factory())
    assert not (tmp_path / 'nocpu').exists()


def test_setup_failure_is_recorded_in_manifest(fake, tmp_path, monkeypatch):
    factory, _, _, model = fake
    monkeypatch.setattr(Model, 'LoadData', lambda self, p: (_ for _ in ()).throw(RuntimeError('licence')))
    with pytest.raises(RuntimeError):
        diag.run_case(model, tmp_path / 's', cpus=[60], api=factory())
    manifest = json.loads((tmp_path / 's' / 'diagnostic.json').read_text())
    assert 'licence' in manifest['stage_errors']['setup']


def test_model_removed_during_run_still_writes_manifest(fake, tmp_path, monkeypatch):
    factory, _, _, model = fake
    def vanish(self): model.unlink()
    monkeypatch.setattr(Model, 'RunSimulation', vanish)
    record = diag.run_case(model, tmp_path / 'g', cpus=[60], api=factory())
    assert 'model_integrity' in record['stage_errors']
    assert (tmp_path / 'g' / 'diagnostic.json').exists()
