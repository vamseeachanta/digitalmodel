"""Reproduce one unchanged OrcaFlex case through existing repository workflows.

Use ``python -m digitalmodel.workflows.orcaflex_reproduce CONFIG --output DIR``.
The output directory must not exist. This diagnostic does not qualify engineering
limits. A failed extraction can be retried with --postprocess-only without solving.
"""
from __future__ import annotations

import argparse
import copy
import gc
import json
import math
import os
import re
from pathlib import Path
import shutil
import signal
import subprocess
import sys
from datetime import datetime, timezone

import yaml

from digitalmodel.infrastructure.persistence.provenance import (
    DataProvenance, compute_hash,
)


def validate_completion(metadata, endpoints):
    """Reject incomplete, unknown, unstable or truncated saved simulations."""
    if metadata.get("simulation_complete") is not True:
        raise ValueError("Saved simulation is not complete")
    if metadata.get("run_status") != "SimulationStopped":
        raise ValueError("Saved simulation state is not recognized as completed")
    for key, expected in zip(("start_time", "stop_time", "current_time"),
                             (endpoints[0], endpoints[1], endpoints[1])):
        value = metadata.get(key)
        if value is None or not math.isfinite(float(value)):
            raise ValueError(f"Missing or nonfinite {key}")
        if abs(float(value) - expected) > 1e-6:
            raise ValueError(f"Incomplete {key}: {value} != {expected}")
    return {"simulation_start": endpoints[0], "simulation_stop": endpoints[1]}


def validate_samples(samples, endpoints, interval):
    values = [float(value) for value in samples]
    if len(values) < 2 or not all(math.isfinite(value) for value in values):
        raise ValueError("Missing or nonfinite simulation samples")
    if interval <= 0 or not math.isfinite(interval):
        raise ValueError("Invalid logging interval")
    if abs(values[0] - endpoints[0]) > interval + 1e-6:
        raise ValueError("Missing start sample coverage")
    if abs(values[-1] - endpoints[1]) > interval + 1e-6:
        raise ValueError("Missing stop sample coverage")
    steps = [b - a for a, b in zip(values, values[1:])]
    if any(step <= 0 or step > interval + 1e-6 for step in steps):
        raise ValueError("Nonmonotonic or incomplete sample coverage")
    return {"count": len(values), "first": values[0], "last": values[-1],
            "min_interval": min(steps), "max_interval": max(steps)}


def validate_batch(summary):
    expected = {"total_cases": 1, "completed": 1, "failed": 0,
                "mock": False, "analysis_type": "both"}
    if any(summary.get(key) != value for key, value in expected.items()):
        raise ValueError("Batch did not complete exactly one real dynamic case")


def snapshot_model(source, output, expected_hash):
    source, output = Path(source).resolve(), Path(output).resolve()
    if compute_hash(source) != expected_hash:
        raise ValueError("Source model digest mismatch")
    if re.search(r'(?im)^\s*(?:-\s*)?(?:BaseFile|IncludeFile)\s*:',
                 source.read_text(encoding='utf-8-sig')):
        raise ValueError('Only standalone model YAML is supported; flatten dependencies first')
    target = output / "source" / source.name
    target.parent.mkdir(parents=True, exist_ok=True)
    shutil.copyfile(source, target)
    if compute_hash(target) != expected_hash:
        raise ValueError("Copied model digest mismatch")
    return target


def batch_config(model_path, output):
    return {"basename": "orcaflex_run_batch", "orcaflex_run_batch": {
        "models": {"files": [str(Path(model_path).resolve())]},
        "analysis": {"type": "both"}, "run_batch": {
            "workers": 1, "solver_threads": 1, "mock": False, "save_sim": True, "progress_interval_seconds": 30,
            "work_dir": str(Path(output).resolve() / "batch_runs"),
            "output_dir": str(Path(output).resolve() / "results")}},
        "default": {"log_level": "INFO"}}


def child_environment(dll_path):
    env = dict(os.environ)
    for name in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"):
        env.pop(name, None)
    source_root = str(Path(__file__).resolve().parents[2])
    env["PYTHONPATH"] = os.pathsep.join(filter(None, (source_root, env.get('PYTHONPATH'))))
    env["PYTHONDONTWRITEBYTECODE"] = "1"
    env["_OrcFxAPIlib"] = str(dll_path)
    return env


def _write_json(path, content):
    Path(path).write_text(json.dumps(content, indent=2, allow_nan=False,
                                   default=str) + "\n", encoding="utf-8")


def _code_identity():
    root = Path(__file__).resolve().parents[3]
    env = dict(os.environ)
    for key in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"):
        env.pop(key, None)
    result = subprocess.run(["git", "-C", str(root), "rev-parse", "HEAD"],
                            capture_output=True, text=True, env=env, check=True)
    return {"git_revision": result.stdout.strip(), "python": sys.version,
            "executable": sys.executable, "source_root": str(root),
            "workflow_sha256": {name: compute_hash(Path(__file__).parent / name)
                                for name in ('orcaflex_reproduce.py', 'orcaflex_reproduce_results.py')}}


def _load_api(config):
    from digitalmodel.solvers.orcaflex import orcaflex_api
    if not orcaflex_api.is_configured():
        orcaflex_api.configure(config.get("solver_version"))
    api, identity = orcaflex_api.api(), orcaflex_api.record()
    if not identity.get("resolved_lib_path") or not identity.get("resolved_version"):
        raise ValueError("Solver library identity was not established")
    return api, identity


def _preflight(api, model_path):
    model = api.Model(threadCount=1)
    model.LoadData(str(model_path))
    stages = [float(value) for value in model.general.StageDuration]
    if len(stages) < 2 or not all(math.isfinite(x) and x > 0 for x in stages):
        raise ValueError("Positive build-up and dynamic stages are required")
    if model.general.UnitsSystem != "SI":
        raise ValueError("First-run extraction currently requires SI model units")
    result = {"stages": stages, "endpoints": [-stages[0], sum(stages[1:])],
              "logging_interval": float(model.general.TargetLogSampleInterval),
              "objects": [obj.name for obj in model.objects], "units_system": "SI"}
    del model
    gc.collect()
    return result


def _terminate(process):
    if os.name == "nt":
        subprocess.run(["taskkill", "/PID", str(process.pid), "/T", "/F"],
                       stdout=subprocess.PIPE, stderr=subprocess.PIPE, check=False)
    else:
        os.killpg(process.pid, signal.SIGTERM)
    try:
        process.wait(timeout=10)
    except subprocess.TimeoutExpired:
        if os.name != "nt":
            os.killpg(process.pid, signal.SIGKILL)
        else:
            process.kill()
        process.wait(timeout=10)


def _solve(input_path, output, dll_path, timeout):
    command = [sys.executable, "-m", "digitalmodel.workflows.orcaflex_reproduce",
               str(input_path), "--batch-child"]
    options = {"start_new_session": True} if os.name != "nt" else {
        "creationflags": subprocess.CREATE_NEW_PROCESS_GROUP | subprocess.CREATE_NO_WINDOW}
    with (output / "solver.log").open("w", encoding="utf-8") as log:
        process = subprocess.Popen(command, cwd=output, env=child_environment(dll_path),
                                   stdout=log, stderr=subprocess.STDOUT, **options)
        try:
            code = process.wait(timeout=timeout)
        except subprocess.TimeoutExpired as error:
            _terminate(process)
            raise TimeoutError(f"Solver exceeded {timeout} s resource cap") from error
    if code:
        raise RuntimeError(f"Solver process exited {code}; see solver.log")
    summary = json.loads((output / "results/batch_summary.json").read_text())
    validate_batch(summary)
    return summary


def _readback(api, model_path, output, receipt, config):
    from digitalmodel.solvers.orcaflex.orcaflex_utilities import OrcaflexUtilities
    from digitalmodel.workflows.orcaflex_reproduce_results import extract_results
    sim = output / "batch_runs/sims" / (model_path.stem + ".sim")
    metadata = OrcaflexUtilities().get_model_and_metadata(str(sim))
    endpoints = receipt["preflight"]["endpoints"]
    receipt.update(validate_completion(metadata, endpoints))
    model = metadata["model"]
    samples = model.SampleTimes(api.Period(api.pnWholeSimulation))
    interval = float(model.general.ActualLogSampleInterval)
    receipt['actual_logging_interval'] = interval
    receipt["samples"] = validate_samples(samples, endpoints, interval)
    receipt["simulation_sha256"] = compute_hash(sim)
    receipt["warnings"] = list(model.warnings)
    _write_json(output / "run.json", receipt)
    extraction = dict(config["extraction"])
    period = extraction["period"]
    if len(period) != 2 or not (endpoints[0] <= period[0] < period[1] <= endpoints[1]):
        raise ValueError("Extraction period is outside the solved interval")
    extract_dir = output / 'extracted'
    if receipt.get('postprocess'):
        stamp = datetime.now(timezone.utc).strftime('%Y%m%dT%H%M%S%fZ')
        extract_dir = output / f'extracted-postprocess-{stamp}'
        extract_dir.mkdir(exist_ok=False)
    receipt["extraction"] = extract_results(model, extraction, extract_dir, copy.deepcopy(receipt))
    return sim


def _read_config(path):
    config = yaml.safe_load(path.read_text(encoding="utf-8"))
    if not isinstance(config, dict):
        raise ValueError("Expected reproduction configuration mapping")
    for key in ("model", "model_sha256", "extraction", "limitations"):
        if not config.get(key):
            raise ValueError(f"Required configuration: {key}")
    if not isinstance(config['limitations'], list) or not all(
            isinstance(item, str) and item.strip() for item in config['limitations']):
        raise ValueError('limitations must be a nonempty list of text')
    timeout = float(config.get("timeout_seconds", 900))
    if not math.isfinite(timeout) or timeout <= 0:
        raise ValueError("A finite positive timeout is required")
    return config


def _prepare(config_path, output, config):
    output.mkdir(parents=True, exist_ok=False)
    shutil.copyfile(config_path, output / "request.yml")
    model_path = snapshot_model(config_path.parent / config["model"], output,
                                config["model_sha256"])
    input_path = output / "input.yml"
    input_path.write_text(yaml.safe_dump(batch_config(model_path, output)), encoding="utf-8")
    return model_path, input_path


def _resume(output, config, receipt):
    previous = json.loads((output / "run.json").read_text())
    if previous.get("model_sha256") != config["model_sha256"]:
        raise ValueError("Resume source identity mismatch")
    receipt.update(previous)
    model_path = output / "source" / Path(config["model"]).name
    if compute_hash(model_path) != config["model_sha256"]:
        raise ValueError("Resume source bytes changed")
    sim = output / "batch_runs/sims" / (model_path.stem + ".sim")
    if not previous.get("simulation_sha256") or compute_hash(sim) != previous["simulation_sha256"]:
        raise ValueError("Resume requires a previously fingerprinted simulation")
    return model_path


def _resume_identity(config, identity, receipt, config_path):
    original = receipt.get('solver', {}).get('resolved_version')
    resolved = identity.get('resolved_version')
    requested = config.get('solver_version')
    if not original or resolved != original:
        raise ValueError('Resume solver resolved version differs from original solve')
    if not requested or not (resolved == requested or
                            (requested[-1].isdigit() and resolved.startswith(requested) and
                             resolved[len(requested):].isalpha())):
        raise ValueError('Resume requested solver version does not match resolved version')
    receipt['postprocess'] = {'code': _code_identity(), 'solver': identity,
                             'request_sha256': compute_hash(config_path),
                             'started_at': datetime.now(timezone.utc).isoformat()}


def reproduce(config_path, output, postprocess_only=False):
    config_path, output = Path(config_path).resolve(), Path(output).resolve()
    config = _read_config(config_path)
    receipt = {"run_id": output.name, "status": "started", "engineering_qualified": False,
               "engineering_acceptance": "NOT EVALUATED", "model_sha256": config["model_sha256"],
               "limitations": config["limitations"], "started_at": datetime.now(timezone.utc).isoformat()}
    if output.exists() and not postprocess_only:
        raise FileExistsError(f"Run directory already exists: {output}")
    may_write_receipt = not postprocess_only
    try:
        if postprocess_only:
            model_path = _resume(output, config, receipt)
        else:
            model_path, input_path = _prepare(config_path, output, config)
        receipt["status"] = "preflight"
        api, identity = _load_api(config)
        if postprocess_only:
            _resume_identity(config, identity, receipt, config_path)
            may_write_receipt = True
        else:
            receipt["code"] = _code_identity()
            receipt["solver"] = identity
            receipt["solver_version"] = identity["resolved_version"]
            receipt["request_sha256"] = compute_hash(config_path)
        if not postprocess_only:
            receipt["preflight"] = _preflight(api, model_path)
            receipt["status"] = "solving"
            _write_json(output / "run.json", receipt)
            receipt["batch"] = _solve(input_path, output, identity["resolved_lib_path"],
                                      float(config.get("timeout_seconds", 900)))
        receipt["status"] = "postprocessing"
        sim = _readback(api, model_path, output, receipt, config)
        if compute_hash(model_path) != config["model_sha256"]:
            raise ValueError("Frozen source bytes changed during run")
        receipt["status"] = "completed"
        receipt.pop('error', None)
        provenance = DataProvenance(str(model_path), receipt["code"]["git_revision"],
                                    config["model_sha256"], metadata=receipt)
        provenance.save_alongside(sim)
    except Exception as error:
        receipt.update(status="failed", error=f"{type(error).__name__}: {error}")
        raise
    finally:
        receipt["finished_at"] = datetime.now(timezone.utc).isoformat()
        if output.is_dir() and may_write_receipt:
            _write_json(output / "run.json", receipt)
    return receipt


def _batch_child(config_path):
    """Bind the parent-selected DLL before loading the existing batch workflow."""
    _load_api({})
    from digitalmodel.workflows.orcaflex_run_batch import router
    config_path = Path(config_path).resolve()
    config = yaml.safe_load(config_path.read_text(encoding='utf-8'))
    if not isinstance(config, dict):
        raise ValueError('Expected batch configuration mapping')
    config['_config_dir_path'] = str(config_path.parent)
    return router(config)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("config", type=Path)
    parser.add_argument("--output", type=Path)
    parser.add_argument("--postprocess-only", action="store_true")
    parser.add_argument("--batch-child", action="store_true", help=argparse.SUPPRESS)
    args = parser.parse_args()
    if args.batch_child:
        if args.output or args.postprocess_only:
            parser.error('--batch-child cannot be combined with output/resume options')
        _batch_child(args.config)
        return
    if args.output is None:
        parser.error('--output is required')
    result = reproduce(args.config, args.output, args.postprocess_only)
    print(json.dumps({"status": result["status"], "run_id": result["run_id"]}))


if __name__ == "__main__":
    main()
