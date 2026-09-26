"""Single-case instability diagnostic: keep the evidence a failed campaign run discards.

The campaign run path raises on an unstable state before anything is saved. This tool runs one
prepared model outside that path. Whatever happens, it retains what can be captured:
- the (partial) simulation;
- the final state and simulation time;
- solver warnings;
- line-end effective-tension histories up to the stop;
- model-view images at the stop.

Each capture stage is independent and a failure is recorded, not propagated, so
``diagnostic.json`` is always written. The outputs feed a physical-realism review. Nothing here
is a finding, and engineering acceptance remains NOT EVALUATED.
"""
from __future__ import annotations

import argparse
from hashlib import sha256
import json
import math
from pathlib import Path

import numpy as np

VIEWS = {'elevation': (0, 0), 'plan': (0, 90), 'perspective': (315, 30)}


def _state(model):
    from digitalmodel.solvers.orcaflex.run_state import state_name
    return state_name(model)


def _pin(cpus):
    import psutil
    process = psutil.Process()
    process.cpu_affinity(list(cpus))
    observed = process.cpu_affinity()
    if set(observed) != set(cpus):
        raise RuntimeError('Requested CPU affinity was not applied')
    return observed


def _outcome(state, stop, end):
    if state == 'SimulationStoppedUnstable':
        return 'unstable'
    if state == 'SimulationStopped' and stop is not None and end is not None and stop >= end - 1e-6:
        return 'completed'
    if state is None:
        return 'unknown'
    return 'incomplete'


def _stage(record, name, action):
    try:
        return action()
    except Exception as error:  # evidence capture is best effort; the manifest records every gap
        record['stage_errors'][name] = f'{type(error).__name__}: {error}'
        return None


def _views(api, model, out, view_size):
    saved, errors = {}, {}
    png = getattr(getattr(api, 'BitmapFileFormat', None), 'PNG', None)
    for name, (azimuth, elevation) in VIEWS.items():
        path = out / f'view-{name}.png'
        try:
            vp = model.defaultViewParameters
            vp.ViewSize, vp.Width, vp.Height = view_size, 1280, 960
            vp.ViewAzimuth, vp.ViewElevation = azimuth, elevation
            if png is None:
                raise RuntimeError('PNG bitmap format not available in this API')
            vp.FileFormat = png
            model.SaveModelView(str(path), vp)
            saved[name] = path.name
        except Exception as error:
            errors[name] = f'{type(error).__name__}: {error}'
    return saved, errors


def _line_ends(api, model, out, record):
    times = np.asarray(model.SampleTimes(api.pnWholeSimulation), dtype=float)
    arrays, summary = {'time': times}, {}
    for obj in model.objects:
        if getattr(obj, 'typeName', None) != 'Line':
            continue
        for label, extra in (('End A', api.oeEndA), ('End B', api.oeEndB)):
            key = f'{obj.name}|{label}'
            try:
                values = np.asarray(obj.TimeHistory('Effective tension', api.pnWholeSimulation, objectExtra=extra), dtype=float)
                if not len(values) or len(values) != len(times):
                    raise ValueError(f'history has {len(values)} samples for {len(times)} sample times')
            except Exception as error:
                record['stage_errors'][f'history {key}'] = f'{type(error).__name__}: {error}'
                continue
            arrays[key] = values
            peak = int(np.argmax(values))
            summary.setdefault(obj.name, {})[label] = dict(
                maximum=float(values[peak]), time_of_maximum_s=float(times[peak]),
                minimum=float(values.min()), final=float(values[-1]), samples=len(values))
    np.savez_compressed(out / 'line_end_tension.npz', **arrays)
    return summary


def run_case(model_path, output_dir, *, cpus, api=None, time_step=None, view_size=60.0):
    """Run one prepared model on the given CPUs and retain its evidence in diagnostic.json."""
    model_path, out = Path(model_path).resolve(), Path(output_dir).resolve()
    if out.exists():
        raise FileExistsError('Diagnostic output must be new')
    if time_step is not None and (isinstance(time_step, bool) or not math.isfinite(time_step) or time_step <= 0):
        raise ValueError('time_step must be a positive finite number')
    if not cpus:
        raise ValueError('Explicit CPU set required')
    observed = _pin(cpus)
    if api is None:
        import OrcFxAPI as api  # noqa: N813 - licensed solver, imported only for a real run
    model_sha = sha256(model_path.read_bytes()).hexdigest()
    out.mkdir(parents=True)
    record = dict(schema_version=1, model=str(model_path), model_sha256=model_sha, cpu_affinity=list(observed),
                  engineering_acceptance='NOT EVALUATED', stage_errors={},
                  purpose='Evidence for a physical-realism review; not a finding.')
    try:
        try:
            model = api.Model(threadCount=1)
            model.LoadData(str(model_path))
            if time_step is not None:
                model.general.ImplicitConstantTimeStep = time_step
        except Exception as error:
            record['stage_errors']['setup'] = f'{type(error).__name__}: {error}'
            raise
        record['time_step_s'] = _stage(record, 'time_step', lambda: float(model.general.ImplicitConstantTimeStep))
        stages = _stage(record, 'stage_duration', lambda: list(model.general.StageDuration)) or []
        record['end_time_s'] = float(sum(stages[1:])) if len(stages) > 1 else None
        _stage(record, 'run', model.RunSimulation)
        record['state'] = _stage(record, 'state', lambda: _state(model))
        record['stop_time_s'] = _stage(record, 'stop_time', lambda: float(model.simulationTimeStatus.CurrentTime))
        record['outcome'] = _outcome(record['state'], record['stop_time_s'], record['end_time_s'])
        record['warnings'] = _stage(record, 'warnings', lambda: list(model.warnings)) or []
        name = 'complete.sim' if record['outcome'] == 'completed' else f"{record['outcome']}.sim"
        if _stage(record, 'save_simulation', lambda: model.SaveSimulation(str(out / name))) is None \
                and 'save_simulation' not in record['stage_errors']:
            record['simulation'] = name
        views = _stage(record, 'views', lambda: _views(api, model, out, view_size)) or ({}, {})
        record['views'], record['view_errors'] = views
        record['line_ends'] = _stage(record, 'line_ends', lambda: _line_ends(api, model, out, record)) or {}
    finally:
        try:
            if sha256(model_path.read_bytes()).hexdigest() != model_sha:
                record['stage_errors']['model_integrity'] = 'Model file changed during the diagnostic'
        except Exception as error:
            record['stage_errors']['model_integrity'] = f'{type(error).__name__}: {error}'
        (out / 'diagnostic.json').write_text(json.dumps(record, indent=2), encoding='utf-8')
    return record


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--model', required=True, type=Path)
    parser.add_argument('--output', required=True, type=Path, help='Parent directory; one subdirectory per run')
    parser.add_argument('--cpus', required=True, help='Explicit comma-separated CPU IDs, e.g. 60')
    parser.add_argument('--time-step', type=float, action='append',
                        help='Override; repeat for several runs. Omit to run the model unchanged.')
    args = parser.parse_args()
    cpus = [int(c) for c in args.cpus.split(',')]
    for step in args.time_step or [None]:
        name = 'as-prepared' if step is None else f'dt-{step:g}'
        record = run_case(args.model, args.output / name, cpus=cpus, time_step=step)
        print(json.dumps({k: record.get(k) for k in ('time_step_s', 'state', 'outcome', 'stop_time_s', 'views', 'stage_errors')}))


if __name__ == '__main__':
    main()
