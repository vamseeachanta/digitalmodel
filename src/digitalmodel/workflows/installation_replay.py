"""Fresh single-case replay by composing existing installation workflows."""
import argparse
from datetime import datetime, timezone
from hashlib import sha256
from importlib.metadata import version
from importlib.util import find_spec
import json
import math
import os
from pathlib import Path
import platform
import subprocess

import yaml

REQUIRED_INPUTS = ('master', 'matrix', 'change', 'extraction', 'criteria',
                   'demo_config', 'reference_summary', 'report_config')


def _json(path, value):
    text = json.dumps(value, indent=2, allow_nan=False)
    Path(path).write_text(text, encoding='utf-8', newline='\n')
    if json.loads(Path(path).read_bytes()) != value:
        raise ValueError('JSON readback mismatch')


def artifact_record(path, root):
    path, root = Path(path).resolve(), Path(root).resolve()
    if not path.is_relative_to(root):
        raise ValueError('Artifact outside replay root')
    raw = path.read_bytes()
    return dict(path=path.relative_to(root).as_posix(), sha256=sha256(raw).hexdigest(), bytes=len(raw))


def _git(root):
    env = {k: v for k, v in os.environ.items() if k not in ('GIT_DIR', 'GIT_WORK_TREE', 'GIT_COMMON_DIR')}
    return subprocess.run(['git', '-C', str(root), 'rev-parse', 'HEAD'], env=env,
                          text=True, capture_output=True, check=True).stdout.strip()


def _clean_source(root, paths):
    env = {k: v for k, v in os.environ.items() if k not in ('GIT_DIR', 'GIT_WORK_TREE', 'GIT_COMMON_DIR')}
    result = subprocess.run(['git', '-C', str(root), 'status', '--porcelain', '--untracked-files=all', '--', *paths],
                            env=env, text=True, capture_output=True, check=True)
    if result.stdout.strip():
        raise ValueError(f'Runtime source tree is dirty: {root}')


def _environment(manifest, base):
    root = Path(__file__).resolve().parents[3]
    code, runtime = manifest['code'], manifest['runtime']
    if _git(root) != code['git_revision']:
        raise ValueError('digitalmodel revision mismatch')
    _clean_source(root, ['src', 'config'])
    if not code.get('files'):
        raise ValueError('Workflow file digests required')
    for name, digest in code['files'].items():
        path = (root / name).resolve()
        if not path.is_relative_to(root) or sha256(path.read_bytes()).hexdigest() != digest:
            raise ValueError('Workflow file digest mismatch')
    if platform.python_version() != runtime['python_version']:
        raise ValueError('Python version mismatch')
    for name, expected in runtime['dependencies'].items():
        if version(name) != expected:
            raise ValueError(f'Package version mismatch: {name}')
    asset_root = (base / runtime['assetutilities_root']).resolve()
    if _git(asset_root) != runtime['assetutilities_git_revision']:
        raise ValueError('assetutilities revision mismatch')
    spec = find_spec('assetutilities')
    if spec is None or not spec.origin or not Path(spec.origin).resolve().is_relative_to(asset_root / 'src/assetutilities'):
        raise ValueError('Imported assetutilities differs from configured source root')
    _clean_source(asset_root, ['src', 'config'])
    if not isinstance(runtime.get('solver_library_sha256'), str) or len(runtime['solver_library_sha256']) != 64:
        raise ValueError('Pinned solver_library_sha256 required')
    if not runtime.get('cpu_affinity') or any(type(x) is not int or x < 0 for x in runtime['cpu_affinity']):
        raise ValueError('Explicit processor affinity required')
    for entry in runtime.get('external_files', []):
        external = (base / entry['path']).resolve()
        if not external.is_file() or sha256(external.read_bytes()).hexdigest() != entry['sha256']:
            raise ValueError('External runtime file digest mismatch')


def validate_manifest(path, output, *, check_environment=True):
    path, output = Path(path).resolve(), Path(output).resolve()
    if output.exists():
        raise FileExistsError('Replay requires a new output root, including no empty directory')
    manifest = json.loads(path.read_bytes())
    if manifest.get('version') != 1 or type(manifest.get('case_index')) is not int:
        raise ValueError('Version 1 and explicit integer case index required')
    if manifest.get('retention') != {'simulation': False}:
        raise ValueError('Explicit no-simulation-retention policy required')
    inputs = {}
    for name in REQUIRED_INPUTS:
        entry = manifest.get('inputs', {}).get(name, {})
        source = (path.parent / entry.get('path', '__missing__')).resolve()
        if not source.is_file() or sha256(source.read_bytes()).hexdigest() != entry.get('sha256'):
            raise ValueError(f'Missing or changed input: {name}')
        inputs[name] = source
    _tolerances(manifest['comparison'])
    _validate_execution(manifest)
    _validate_selection(manifest, inputs)
    if check_environment:
        _environment(manifest, path.parent)
    return manifest, inputs


def _validate_execution(manifest):
    timeout = manifest.get('timeout_seconds')
    if type(timeout) not in (int, float) or not math.isfinite(timeout) or timeout <= 0:
        raise ValueError('Explicit positive finite numeric timeout_seconds required')
    solver = manifest.get('runtime', {}).get('solver_version')
    if not isinstance(solver, str) or not solver.strip():
        raise ValueError('Explicit nonempty solver_version required')


def _validate_selection(manifest, inputs):
    matrix = json.loads(inputs['matrix'].read_bytes())
    index = manifest['case_index']
    if not 0 <= index < len(matrix['cases']):
        raise ValueError('Case index outside matrix')
    case = matrix['cases'][index]
    if matrix['master_sha256'] != manifest['inputs']['master']['sha256'] or case['change_sha256'] != manifest['inputs']['change']['sha256']:
        raise ValueError('Selected case dependency hashes differ')
    limitations = manifest.get('pilot_limitations')
    if not isinstance(limitations, list) or not limitations or any(not isinstance(x, str) or not x.strip() for x in limitations):
        raise ValueError('Explicit nonempty pilot_limitations required')


def _tolerances(settings):
    values = [settings['absolute_tolerance'], settings['relative_tolerance']]
    if any(isinstance(v, bool) or not math.isfinite(v) or v < 0 for v in values):
        raise ValueError('Explicit finite nonnegative tolerances required')
    return values


def compare_metrics(fresh, reference, settings):
    absolute, relative = _tolerances(settings)
    left, right = fresh['channels'], reference['channels']
    if not left or set(left) != set(right):
        raise ValueError('Comparison channels differ')
    differences = []
    for key, channel in left.items():
        other = right[key]
        for name in ('units', 'object', 'variable', 'position'):
            if channel.get(name) != other.get(name):
                raise ValueError(f'Channel identity differs: {key}/{name}')
        numbers, expected_numbers = _numeric_fields(channel), _numeric_fields(other)
        if not {'minimum', 'maximum'} <= numbers.keys() or not {'minimum', 'maximum'} <= expected_numbers.keys():
            raise ValueError(f'Numeric minimum and maximum required: {key}')
        if set(numbers) != set(expected_numbers):
            raise ValueError(f'Channel numeric metric coverage differs: {key}')
        for metric, actual in numbers.items():
            expected = expected_numbers[metric]
            if not all(math.isfinite(v) for v in (actual, expected)):
                raise ValueError('Nonfinite comparison metric')
            allowed = absolute + relative * abs(expected)
            delta = abs(actual - expected)
            differences.append(dict(channel=key, metric=metric, actual=actual,
                                    reference=expected, absolute_difference=delta,
                                    tolerance=allowed, passed=delta <= allowed))
    return dict(passed=bool(differences) and all(row['passed'] for row in differences), settings=settings, metrics=differences)


def _numeric_fields(value, prefix=''):
    if isinstance(value, dict):
        result = {}
        for key, item in value.items():
            result.update(_numeric_fields(item, prefix + ('.' if prefix else '') + str(key)))
        return result
    if isinstance(value, list):
        result = {}
        for index, item in enumerate(value):
            result.update(_numeric_fields(item, f'{prefix}[{index}]'))
        return result
    return {prefix: value} if type(value) in (int, float) else {}


def dispose_simulation(path, root):
    path, root = Path(path).resolve(), Path(root).resolve()
    if not path.is_relative_to(root / 'run') or path.suffix.lower() != '.sim':
        raise ValueError('Only a fresh-run simulation may be disposed')
    record = artifact_record(path, root)
    path.unlink()
    if path.exists():
        raise ValueError('Simulation removal failed')
    return dict(**record, disposition='removed_after_verified_derivation')


def _snapshot_inputs(root, inputs, manifest):
    frozen = root / 'inputs'
    frozen.mkdir()
    copied = {}
    for name, source in inputs.items():
        target = frozen / (name + source.suffix)
        target.write_bytes(source.read_bytes())
        if sha256(target.read_bytes()).hexdigest() != manifest['inputs'][name]['sha256']:
            raise ValueError('Input changed during snapshot')
        copied[name] = target
    matrix = json.loads(copied['matrix'].read_bytes())
    index = manifest['case_index']
    if not 0 <= index < len(matrix['cases']):
        raise ValueError('Case index outside matrix')
    case = matrix['cases'][index]
    if matrix['master_sha256'] != manifest['inputs']['master']['sha256'] or case['change_sha256'] != manifest['inputs']['change']['sha256']:
        raise ValueError('Matrix dependency hashes differ from manifest')
    study = root / 'study'
    study.mkdir()
    for path, name in [('matrix.json', 'matrix'), (matrix['master_file'], 'master'), (case['change_file'], 'change')]:
        target = (study / path).resolve()
        if not target.is_relative_to(study):
            raise ValueError('Matrix path escapes fresh study')
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_bytes(copied[name].read_bytes())
    return copied, matrix


def _native(root, frozen, matrix, manifest):
    from digitalmodel.workflows.installation_seastates import materialize_case
    from digitalmodel.workflows.orcaflex_reproduce import reproduce, _load_api
    from digitalmodel.workflows.installation_campaign import _verify_run, _ensure_traces
    import psutil
    runtime = manifest['runtime']
    psutil.Process().cpu_affinity(runtime['cpu_affinity'])
    api, identity = _load_api({'solver_version': runtime['solver_version']})
    if identity['resolved_version'] != runtime['solver_version']:
        raise ValueError('Resolved native solver mismatch')
    identity['library_sha256'] = sha256(Path(identity['resolved_lib_path']).read_bytes()).hexdigest()
    if runtime['solver_library_sha256'] != identity['library_sha256']:
        raise ValueError('Resolved solver library digest mismatch')
    extraction = yaml.safe_load(frozen['extraction'].read_text(encoding='utf-8'))
    if 'extraction' in extraction:
        extraction = extraction['extraction']
    materialize_case(api, root / 'study', manifest['case_index'], root / 'prepared',
                     extraction=extraction, solver_version=runtime['solver_version'],
                     timeout_seconds=manifest['timeout_seconds'])
    receipt = reproduce(root / 'prepared/request.yml', root / 'run', postprocess_only=False)
    _verify_run(root / 'run', root / 'prepared/generation.json', matrix,
                manifest['case_index'], runtime['solver_version'])
    _ensure_traces(root / 'run', receipt)
    return identity


def run_replay(manifest_path, output):
    manifest, inputs = validate_manifest(manifest_path, output)
    manifest_raw = Path(manifest_path).read_bytes()
    if json.loads(manifest_raw) != manifest:
        raise ValueError('Manifest changed during preflight')
    root = Path(output).resolve()
    root.mkdir(parents=True, exist_ok=False)
    record = dict(status='started', started_utc=datetime.now(timezone.utc).isoformat(),
                  manifest=manifest, manifest_sha256=sha256(manifest_raw).hexdigest(),
                  source_campaign_mutation=False, force_fresh=True, stages=[], simulation_disposition='not_created')
    record['external_files'] = [dict(entry, resolved_path=str((Path(manifest_path).resolve().parent / entry['path']).resolve()))
                                for entry in manifest['runtime'].get('external_files', [])]
    _json(root / 'lineage.json', record)
    try:
        frozen, matrix = _snapshot_inputs(root, inputs, manifest)
        record['stages'].append(dict(stage='inputs_verified', artifacts=[artifact_record(p, root) for p in frozen.values()]))
        _json(root / 'lineage.json', record)
        record['solver'] = _native(root, frozen, matrix, manifest)
        _environment(manifest, Path(manifest_path).resolve().parent)
        record['stages'].append(dict(stage='fresh_solve_and_extraction_completed'))
        from digitalmodel.workflows.installation_replay_report import build_reports
        derived = build_reports(root, frozen, matrix, manifest)
        record['comparison'] = derived['comparison']
        record['stages'].append(dict(stage='reports_verified', artifacts=derived['artifacts']))
        record['simulation_disposition'] = 'verified_pending_disposal'
        _json(root / 'lineage.json', record)
        simulations = list((root / 'run').rglob('*.sim'))
        if len(simulations) != 1:
            raise ValueError('Expected one fresh simulation for disposal')
        record['removed_simulation'] = dispose_simulation(simulations[0], root)
        record.update(status='completed', simulation_disposition='removed_after_verified_derivation')
    except Exception as error:
        record.update(status='failed', error=f'{type(error).__name__}: {error}',
                      simulation_disposition='retained_if_created_for_diagnosis')
        raise
    finally:
        record['finished_utc'] = datetime.now(timezone.utc).isoformat()
        _json(root / 'lineage.json', record)
    return record


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('manifest', type=Path)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--validate-only', action='store_true')
    args = parser.parse_args()
    if args.validate_only:
        validate_manifest(args.manifest, args.output)
        print('Pinned replay inputs and environment verified; no native solve performed')
    else:
        print(json.dumps({'status': run_replay(args.manifest, args.output)['status']}))


if __name__ == '__main__':
    main()
