"""Offline acceptance contract with explicit fake API, never a native Model."""
import json
from enum import IntEnum
from types import SimpleNamespace

import pytest

from digitalmodel.solvers.smoke import model_probe


class ObjectType(IntEnum):
    Line = 1
    Buoy6D = 7
    General = 100
    Environment = 101
    Vessel = 102


class FakeObject:
    def __init__(self, name, kind, api):
        self.name, self.type, self.api = name, kind, api
        self.Mass = 4

    def StaticResult(self, variable, extra=None):
        return 1.0

    def RangeGraph(self, variable, period):
        return SimpleNamespace(X=[0.0, 620.0], Mean=[1.0, 2.0])

    def TimeHistory(self, variable, period, extra=None):
        if self.api.fail == "unsupported":
            raise ValueError("unsupported result")
        value = float("nan") if self.api.fail == "nonfinite" else 1.0
        if self.api.fail == "mutation" and self.api.loaded:
            value = 2.0
        return [value] * 1001


class FakeModel:
    def __init__(self, api, threadCount):
        assert threadCount == 1
        self.api = api
        self.threadCount = 1
        self.general = SimpleNamespace(StageDuration=[11.5, 100.0])
        self.environment = SimpleNamespace(WaterDepth=500.0)
        self.objects = [FakeObject("Mooring", ObjectType.Line, api),
                        FakeObject("Metocean buoy", ObjectType.Buoy6D, api)]
        self.state = "complete"
        self.simulationComplete = True
        self.warnings = ()

    def __getitem__(self, name):
        return next(o for o in self.objects if o.name == name)

    def LoadData(self, path):
        if self.api.fail == "load":
            raise ValueError("load failed")
        if self.api.fail == "thread":
            self.threadCount = 2
        if self.api.fail == "settings":
            self.general.StageDuration = [1.0, 2.0]

    def CalculateStatics(self):
        if self.api.fail == "statics":
            raise ValueError("statics failed")

    def RunSimulation(self):
        if self.api.fail == "dynamics":
            raise ValueError("dynamics failed")
        if self.api.fail == "incomplete":
            self.simulationComplete = False

    def SampleTimes(self, period):
        count = 1000 if self.api.fail == "grid" else 1001
        return [i / 10 for i in range(count)]

    def SaveSimulation(self, path):
        if self.api.fail != "missing_save":
            path.write_bytes(b"fake simulation")

    def LoadSimulation(self, path):
        self.api.loaded = True

    def SaveData(self, path):
        path.write_text('6DBuoys:\n  - Name: Metocean buoy\n    CentreOfMass: [0.001, 0, 0]\n')


@pytest.fixture
def setup_probe(tmp_path, monkeypatch):
    contract = {
        "expected_settings": {"general": {"StageDuration": [11.5, 100.0]},
                              "environment": {"WaterDepth": 500.0},
                              "objects": {"Metocean buoy": {"Mass": 4}}},
        "expected_types": {"Mooring": "Line", "Metocean buoy": "Buoy6D"},
        "inventory_policy": {"allowed_system_types": ["General", "Environment"]},
        "period": {"start": 0, "end": 100, "interval": 0.1, "grid_atol": 1e-9},
    }
    manifest = {"_root": tmp_path, "master": "master.yml", "contract": contract}
    (tmp_path / "manifest.json").write_text("{}")
    (tmp_path / "master.yml").write_text("General: {}\n")
    monkeypatch.setattr(model_probe, "verify_manifest", lambda _: manifest)
    api = SimpleNamespace(fail=None, loaded=False, closed=0, ObjectType=ObjectType,
                          ModelState=SimpleNamespace(SimulationStopped="complete"),
                          Period=lambda n: n, pnStaticState=-1, oeEndA="A", oeEndB="B",
                          oeBuoy=lambda x, y, z: (x, y, z), DLLVersion=lambda: "fake")
    api.Model = lambda **kw: FakeModel(api, **kw)
    return tmp_path / "manifest.json", tmp_path / "output", api


def test_solve_and_independent_readback_match(setup_probe):
    manifest, out, api = setup_probe
    solve = model_probe.run_phase(manifest, out, "solve", api)
    assert solve["ok"], solve
    assert solve == json.loads((out / "solve/results.json").read_text())
    read = model_probe.run_phase(manifest, out, "readback", api)
    assert read["ok"] and read["fidelity_verified"]
    assert read["results"] == solve["results"]
    assert len(solve["results"]["histories"]) == 6
    assert len(solve["results"]["times"]) == 1001


@pytest.mark.parametrize("failure", ["load", "statics", "dynamics", "thread", "settings",
                                      "nonfinite", "grid", "unsupported", "incomplete", "missing_save"])
def test_failures_are_written_as_failed_proof(setup_probe, failure):
    manifest, out, api = setup_probe
    api.fail = failure
    result = model_probe.run_phase(manifest, out, "solve", api)
    assert result["ok"] is False
    assert result == json.loads((out / "solve/results.json").read_text())
    assert result["stage"] != "complete"


def test_readback_numeric_mutation_fails(setup_probe):
    manifest, out, api = setup_probe
    assert model_probe.run_phase(manifest, out, "solve", api)["ok"]
    api.fail = "mutation"
    assert not model_probe.run_phase(manifest, out, "readback", api)["ok"]


def test_corrupt_simulation_rejected_before_load(setup_probe):
    manifest, out, api = setup_probe
    assert model_probe.run_phase(manifest, out, "solve", api)["ok"]
    (out / "solve/model.sim").write_bytes(b"changed")
    assert not model_probe.run_phase(manifest, out, "readback", api)["ok"]
    assert not api.loaded


def test_existing_phase_directory_never_overwritten(setup_probe):
    manifest, out, api = setup_probe
    first = model_probe.run_phase(manifest, out, "solve", api)
    with pytest.raises(FileExistsError):
        model_probe.run_phase(manifest, out, "solve", api)
    assert json.loads((out / "solve/results.json").read_text()) == first


def test_manifest_rechecked_after_solve(setup_probe, monkeypatch):
    manifest, out, api = setup_probe
    initial = model_probe.verify_manifest
    calls = []
    def verify(path):
        calls.append(path)
        if len(calls) > 1:
            raise ValueError("bundle changed")
        return initial(path)
    monkeypatch.setattr(model_probe, "verify_manifest", verify)
    assert not model_probe.run_phase(manifest, out, "solve", api)["ok"]
    assert len(calls) == 2


def test_readback_requires_same_manifest_as_solve(setup_probe):
    manifest, out, api = setup_probe
    assert model_probe.run_phase(manifest, out, "solve", api)["ok"]
    manifest.write_text('{"changed":true}')
    result = model_probe.run_phase(manifest, out, "readback", api)
    assert not result["ok"]
    assert not api.loaded


def test_failed_preflight_never_constructs_api_model(setup_probe, monkeypatch):
    manifest, out, api = setup_probe
    def reject(_):
        raise ValueError("invalid manifest")
    def forbidden(**_):
        pytest.fail("native constructor reached before preflight")
    monkeypatch.setattr(model_probe, "verify_manifest", reject)
    api.Model = forbidden
    assert not model_probe.run_phase(manifest, out, "solve", api)["ok"]


def test_thread_drift_after_dynamics_fails(setup_probe):
    manifest, out, api = setup_probe
    def create(**kw):
        model = FakeModel(api, **kw)
        model.RunSimulation = lambda: setattr(model, "threadCount", 2)
        return model
    api.Model = create
    assert not model_probe.run_phase(manifest, out, "solve", api)["ok"]


def test_incomplete_history_fails_even_with_full_times(setup_probe):
    manifest, out, api = setup_probe
    def create(**kw):
        model = FakeModel(api, **kw)
        model["Mooring"].TimeHistory = lambda *args: [1.0]
        return model
    api.Model = create
    assert not model_probe.run_phase(manifest, out, "solve", api)["ok"]


@pytest.mark.parametrize("mutation", ["missing", "type", "static_nonfinite"])
def test_inventory_and_static_contract_failures(setup_probe, mutation):
    manifest, out, api = setup_probe
    def create(**kw):
        model = FakeModel(api, **kw)
        if mutation == "missing":
            model.objects.pop()
        elif mutation == "type":
            model["Mooring"].type = ObjectType.Buoy6D
        else:
            model["Mooring"].StaticResult = lambda *args: float("inf")
        return model
    api.Model = create
    assert not model_probe.run_phase(manifest, out, "solve", api)["ok"]


def test_readback_without_solve_proof_fails_before_model(setup_probe):
    manifest, out, api = setup_probe
    api.Model = lambda **kw: pytest.fail("reader started without solve proof")
    assert not model_probe.run_phase(manifest, out, "readback", api)["ok"]


def test_allowed_system_objects_do_not_change_required_inventory(setup_probe):
    manifest, out, api = setup_probe
    def create(**kw):
        model = FakeModel(api, **kw)
        model.objects.extend([FakeObject("General", ObjectType.General, api),
                              FakeObject("Environment", ObjectType.Environment, api)])
        return model
    api.Model = create
    result = model_probe.run_phase(manifest, out, "solve", api)
    assert result["ok"], result
    assert {row["name"] for row in result["observed_inventory"]} == {
        "General", "Environment", "Mooring", "Metocean buoy"}


@pytest.mark.parametrize("kind", ["unexpected_physical", "duplicate", "environment_drift"])
def test_system_policy_does_not_hide_other_drift(setup_probe, kind):
    manifest, out, api = setup_probe
    def create(**kw):
        model = FakeModel(api, **kw)
        if kind == "unexpected_physical":
            model.objects.append(FakeObject("Extra vessel", ObjectType.Vessel, api))
        elif kind == "duplicate":
            model.objects.append(FakeObject("Mooring", ObjectType.Line, api))
        else:
            model.environment.WaterDepth = 50.0
        return model
    api.Model = create
    assert not model_probe.run_phase(manifest, out, "solve", api)["ok"]


@pytest.mark.parametrize("change", ["valid", "missing", "different"])
def test_exported_physics_readback(setup_probe, change):
    manifest, out, api = setup_probe
    contract = model_probe.verify_manifest(manifest)["contract"]
    contract["expected_export"] = {
        "6DBuoys": {"Metocean buoy": {"CentreOfMass": [0.001, 0, 0]}}}
    def create(**kw):
        model = FakeModel(api, **kw)
        if change == "missing":
            model.SaveData = lambda path: path.write_text('6DBuoys: []\n')
        elif change == "different":
            model.SaveData = lambda path: path.write_text(
                '6DBuoys:\n - Name: Metocean buoy\n   CentreOfMass: [0, 0, 0]\n')
        return model
    api.Model = create
    result = model_probe.run_phase(manifest, out, "solve", api)
    assert result["ok"] is (change == "valid")
    if change == "valid":
        assert result["input_readback_verified"]
        assert (out / "solve/loaded.yml").exists()
        assert model_probe.run_phase(manifest, out, "readback", api)["input_readback_verified"]
