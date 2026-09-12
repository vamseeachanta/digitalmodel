"""Contained-process mooring-buoy probe; no API import before manifest checks.

The public native Model API has no Close/context-manager method. References
are dropped in finally; process exit and the parent harness's observed whole
job drain provide release evidence before another phase starts.
"""
import hashlib
import importlib
import json
import traceback
from pathlib import Path

from .model_results import extract_results, static_results
from .model_data_readback import verify_export


def verify_manifest(path):
    from .model_manifest import verify_manifest as verify
    return verify(path)


def digest(path):
    if not path.is_file() or path.stat().st_size == 0:
        raise ValueError("required artifact missing or empty")
    return hashlib.sha256(path.read_bytes()).hexdigest()


def plain(value):
    if hasattr(value, "tolist"):
        return value.tolist()
    if isinstance(value, (tuple, list)):
        return [plain(item) for item in value]
    return value


def check_settings(model, api, contract, proof):
    count = model.threadCount
    if isinstance(count, bool) or count != 1:
        raise ValueError("observed solver thread count must equal one")
    proof["thread_count_observed"] = int(count)
    expected_types = contract["expected_types"]
    objects = {obj.name: obj for obj in model.objects}
    if not set(expected_types).issubset(objects) or len(objects) != len(model.objects):
        raise ValueError("native object inventory differs from contract")
    allowed_system = set(contract["inventory_policy"]["allowed_system_types"])
    proof["observed_inventory"] = [
        {"name": name, "type": obj.type.name} for name, obj in sorted(objects.items())]
    for name, obj in objects.items():
        if name not in expected_types and obj.type.name not in allowed_system:
            raise ValueError("unexpected native object outside system inventory policy")
    for name, kind in expected_types.items():
        if objects[name].type != api.ObjectType[kind]:
            raise ValueError("native object type differs from contract")
    settings = contract["expected_settings"]
    groups = [(model.general, settings["general"]),
              (model.environment, settings["environment"])]
    groups.extend((objects[name], attrs) for name, attrs in settings["objects"].items())
    for obj, attrs in groups:
        for name, expected in attrs.items():
            if plain(getattr(obj, name)) != expected:
                raise ValueError(f"native setting differs from contract: {name}")
    proof["settings_verified"] = True


def completed(model, api):
    if model.simulationComplete is not True or model.state != api.ModelState.SimulationStopped:
        raise ValueError("simulation did not complete source stages")


def load_baseline(output_dir):
    path = output_dir / "solve/results.json"
    data = json.loads(path.read_text(encoding="utf-8"))
    if data.get("ok") is not True or data.get("stage") != "complete" or data.get("phase") != "solve":
        raise ValueError("successful solve proof required before readback")
    if digest(output_dir / "solve/model.sim") != data.get("simulation_sha256"):
        raise ValueError("simulation hash differs from solve proof")
    return data


def run_model(manifest, output_dir, phase, api, proof):
    contract, model = manifest["contract"], None
    try:
        baseline = load_baseline(output_dir) if phase == "readback" else None
        if baseline and baseline.get("manifest_sha256") != proof["manifest_sha256"]:
            raise ValueError("readback manifest differs from solve manifest")
        proof["stage"] = "construct"
        model = api.Model(threadCount=1)
        proof["stage"] = "load"
        if phase == "solve":
            model.LoadData(manifest["_root"] / manifest["master"])
        else:
            model.LoadSimulation(output_dir / "solve/model.sim")
        proof["stage"] = "settings"
        check_settings(model, api, contract, proof)
        if "expected_export" in contract:
            proof["stage"] = "input_readback"
            proof["loaded_data_sha256"] = verify_export(
                model, output_dir / phase, contract["expected_export"])
            proof["input_readback_verified"] = True
        proof["stage"] = "statics"
        if phase == "solve":
            model.CalculateStatics()
        static = static_results(model, api)
        proof["stage"] = "dynamics"
        if phase == "solve":
            model.RunSimulation()
        completed(model, api)
        check_settings(model, api, contract, proof)
        proof["stage"] = "extract"
        proof["results"] = extract_results(model, api, contract, static)
        proof["warnings"] = list(model.warnings)
        proof["stage"] = "save" if phase == "solve" else "compare"
        if phase == "solve":
            model.SaveSimulation(output_dir / "solve/model.sim")
        elif proof["results"] != baseline["results"]:
            raise ValueError("saved-result numeric fidelity mismatch")
        proof["simulation_sha256"] = digest(output_dir / "solve/model.sim")
        proof["fidelity_verified"] = phase == "readback"
        proof["simulation_complete"] = True
    finally:
        model = None


def run_phase(manifest_path: Path, output_dir: Path, phase: str, api=None) -> dict:
    if phase not in {"solve", "readback"}:
        raise ValueError("phase must be solve or readback")
    phase_dir = output_dir / phase
    phase_dir.mkdir(parents=True, exist_ok=False)
    proof = {"ok": False, "phase": phase, "stage": "preflight",
             "thread_count_requested": 1}
    try:
        manifest = verify_manifest(manifest_path)
        proof["manifest_sha256"] = digest(manifest_path)
        proof["stage"] = "import"
        api = api if api is not None else importlib.import_module("OrcFxAPI")
        proof["version"] = api.DLLVersion()
        run_model(manifest, output_dir, phase, api, proof)
        proof["stage"] = "postflight"
        verify_manifest(manifest_path)
        if digest(manifest_path) != proof["manifest_sha256"]:
            raise ValueError("manifest changed during phase")
        proof.update(ok=True, stage="complete")
    except Exception as exc:
        traceback.print_exc()  # Parent harness captures this in private stderr.
        proof["error_type"] = type(exc).__name__
        # API errors can include private paths; detailed raw logs stay private.
        proof["error"] = "phase failed; inspect private runtime diagnostics"
    (phase_dir / "results.json").write_text(
        json.dumps(proof, indent=2, allow_nan=False) + "\n", encoding="utf-8")
    return proof
