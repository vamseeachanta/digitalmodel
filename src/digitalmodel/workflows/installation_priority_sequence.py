"""Run pinned five-case priority batches sequentially through the existing runner."""
import argparse
from contextlib import contextmanager
import copy
from datetime import datetime, timezone
from hashlib import sha256
import json
from pathlib import Path

import psutil

from digitalmodel.workflows import installation_priority_requests as runner

INPUT_KEYS = ('manifest_path', 'manifest_sha256', 'artifact_manifest', 'artifact_manifest_sha256')
REQUIRED = {*INPUT_KEYS, 'output_root', 'baseline_root', 'identities', 'cpus'}
OPTIONAL = {'workers', 'poll_seconds', 'wait_timeout_seconds'}
PATH_KEYS = ('manifest_path', 'artifact_manifest', 'output_root', 'baseline_root')


def _pinned(path, digest):
    raw = Path(path).read_bytes()
    if not isinstance(digest, str) or sha256(raw).hexdigest() != digest:
        raise ValueError('Pinned input digest mismatch')
    return json.loads(raw.decode('utf-8-sig'))


def _config(path, digest):
    config = _pinned(path, digest)
    if not isinstance(config, dict) or not REQUIRED <= config.keys() or config.keys() - REQUIRED - OPTIONAL:
        raise ValueError('Unsupported or missing priority runner fields')
    config = copy.deepcopy(config)
    for key in PATH_KEYS:
        config[key] = str((path.parent / config[key]).resolve())
    return config


def _validate_config(config):
    inputs = {key: config[key] for key in INPUT_KEYS}
    runner._validate_options(Path(config['output_root']), Path(config['baseline_root']), inputs,
        config['identities'], config.get('workers', 3), config['cpus'],
        config.get('poll_seconds', 15), config.get('wait_timeout_seconds'))
    rows = runner._inputs(**inputs)
    if len(rows) != 5:
        raise ValueError('Expected five verified cases per priority batch')
    return rows


def _overlap(left, right):
    return left.is_relative_to(right) or right.is_relative_to(left)


def _isolate(root, jobs, spec):
    outputs = [root] + [Path(job['config_args']['output_root']) for job in jobs]
    for index, path in enumerate(outputs):
        if path.exists():
            raise FileExistsError('Sequence and batch outputs must be new')
        if any(_overlap(path, other) for other in outputs[:index]):
            raise ValueError('Sequence and batch outputs overlap')
        for job in jobs:
            config = job['config_args']
            sources = [Path(config['baseline_root']), Path(config['artifact_manifest']).parent]
            if any(_overlap(path, source) for source in sources):
                raise ValueError('Output must be disjoint from every baseline/input tree')
            for source in [spec, job['config_path'], Path(config['manifest_path'])]:
                if source.is_relative_to(path):
                    raise ValueError('Output overlaps pinned source file')


def _preflight(spec, digest, root):
    data = _pinned(spec, digest)
    if (not isinstance(data, dict) or set(data) != {'schema_version', 'jobs'}
            or type(data['schema_version']) is not int or data['schema_version'] != 1
            or not isinstance(data['jobs'], list) or not data['jobs']):
        raise ValueError('Sequence schema version 1 and nonempty jobs required')
    jobs = []
    for row in data['jobs']:
        if not isinstance(row, dict) or set(row) != {'config', 'sha256'}:
            raise ValueError('Each job requires config and sha256')
        relative = Path(row['config'])
        path = (spec.parent / relative).resolve()
        if relative.is_absolute() or not path.is_relative_to(spec.parent):
            raise ValueError('Job config must be relative within spec directory')
        if any(job['config_path'] == path for job in jobs):
            raise ValueError('Duplicate config path')
        config = _config(path, row['sha256'])
        _validate_config(config)
        jobs.append(dict(config_path=path, relative_path=relative.as_posix(), sha256=row['sha256'], config_args=config))
    _isolate(root, jobs, spec)
    return jobs


def _save(root, summary):
    raw = json.dumps(summary, indent=2, allow_nan=False)
    temporary = root/'sequence.pending.json'
    temporary.write_text(raw, encoding='utf-8')
    if json.loads(temporary.read_bytes()) != summary:
        raise ValueError('Sequence readback mismatch')
    temporary.replace(root/'sequence.json')


@contextmanager
def _ownership(root, owner):
    root.mkdir(parents=True, exist_ok=False)
    path = root/'sequence.lock'
    with path.open('x', encoding='utf-8') as stream:
        def update(active_batch):
            owner['active_batch'] = active_batch
            stream.seek(0)
            stream.write(json.dumps(owner, allow_nan=False))
            stream.truncate()
            stream.flush()
        update(None)
        try:
            yield update
        finally:
            stream.close()
            path.unlink()


def _module_hashes():
    paths = [Path(__file__), Path(runner.__file__), Path(runner.parallel.__file__)]
    return {path.name: sha256(path.read_bytes()).hexdigest() for path in paths}


def _disk_check(config, index, summary):
    parent = Path(config['output_root']).parent
    while not parent.exists():
        if parent.parent == parent:
            raise ValueError('No accessible volume for batch output')
        parent = parent.parent
    free = psutil.disk_usage(str(parent)).free
    minimum = 10 * 1024**3
    summary['disk_checks'].append(dict(batch=index, checked_parent=str(parent), free_bytes=free,
        required_free_bytes=minimum, observed_utc=datetime.now(timezone.utc).isoformat(),
        basis='Operational free-space floor; not a prediction of required storage'))
    if free < minimum:
        raise RuntimeError('Insufficient disk free space for next batch: 10 GiB floor')


def _dispatch(spec, digest, root, jobs, summary, update_owner):
    for index, job in enumerate(jobs):
        _pinned(spec, digest)
        config = _config(job['config_path'], job['sha256'])
        if config != job['config_args']:
            raise ValueError('Verified config changed before dispatch')
        _validate_config(config)
        update_owner(index)
        summary.update(status='running', active_batch=index)
        _disk_check(config, index, summary)
        _save(root, summary)
        # The runner drains its process pool and waits for native children before return.
        result = runner.run_priority_requests(**copy.deepcopy(config))
        summary['batch_results'].append(dict(index=index, result=result))
        cases = result.get('cases', [])
        if result.get('status') != 'completed' or len(cases) != 5 or any(r.get('status') != 'completed' for r in cases):
            summary.update(status='failed', error='Batch did not complete all five cases')
            _save(root, summary)
            return
        _save(root, summary)
    summary.update(status='completed', active_batch=None)


def run_sequence(spec, spec_sha256, output):
    spec, root = Path(spec).resolve(), Path(output).resolve()
    if root.exists():
        raise FileExistsError('Sequence output already exists; no reclaim or overwrite')
    jobs = _preflight(spec, spec_sha256, root)
    process = psutil.Process()
    owner = dict(pid=process.pid, create_time=process.create_time(), spec_sha256=spec_sha256)
    summary = dict(status='prepared', coordinator=copy.deepcopy(owner), spec_sha256=spec_sha256,
        started_utc=datetime.now(timezone.utc).isoformat(), active_batch=None, batch_results=[], disk_checks=[],
        jobs=[dict(config=j['relative_path'], sha256=j['sha256'], config_args=j['config_args']) for j in jobs],
        module_sha256=_module_hashes(), engineering_acceptance='NOT EVALUATED')
    with _ownership(root, owner) as update_owner:
        try:
            _save(root, summary)
            _dispatch(spec, spec_sha256, root, jobs, summary, update_owner)
        except BaseException as error:
            summary.update(status='failed', error=f'{type(error).__name__}: {error}')
            raise
        finally:
            summary['finished_utc'] = datetime.now(timezone.utc).isoformat()
            _save(root, summary)
    return summary


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('spec', type=Path)
    parser.add_argument('--spec-sha256', required=True)
    parser.add_argument('--output', required=True, type=Path)
    args = parser.parse_args()
    result = run_sequence(args.spec, args.spec_sha256, args.output)
    print(json.dumps({'status': result['status']}))
    return 0 if result['status'] == 'completed' else 1


if __name__ == '__main__':
    raise SystemExit(main())
