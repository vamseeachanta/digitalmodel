"""Derive a sea-state study from an existing one with declared, verified differences only.

Supported differences are single-line General master fields (numerical settings) and the
irregular-wave seed. Every other master field and every change file is proved unchanged, so a
variant campaign compares like with like. The output also carries an all-MISSING handover that
the explicit-subset parallel launcher accepts as its frozen source.
"""
from __future__ import annotations

import argparse
import json
import math
from pathlib import Path
import re
import shutil

import yaml

from digitalmodel.workflows.installation_seastates import compute_hash, prepare_matrix

GENERAL_FIELDS = {'ImplicitConstantTimeStep'}


def _load(path):
    from digitalmodel.solvers.orcaflex.yaml_utils import OrcaFlexLoader
    return yaml.load(Path(path).read_text(encoding='utf-8-sig'), Loader=OrcaFlexLoader)


def _source(study):
    matrix_path = study / 'matrix.json'
    matrix = json.loads(matrix_path.read_text(encoding='utf-8'))
    if compute_hash(study / matrix['master_file']) != matrix['master_sha256']:
        raise ValueError('Source master differs from its matrix digest')
    for case in matrix['cases']:
        if compute_hash(study / case['change_file']) != case['change_sha256']:
            raise ValueError('Source change file differs from its matrix digest')
    seeds = {case['seed'] for case in matrix['cases']}
    if len(seeds) != 1:
        raise ValueError('Source study must use one wave seed')
    return matrix, seeds.pop()


def _validate(general, seed, source_seed):
    general = dict(general or {})
    if not general and seed is None:
        raise ValueError('At least one declared difference is required')
    for key, value in general.items():
        if key not in GENERAL_FIELDS:
            raise ValueError(f'Unsupported master field: {key}')
        if isinstance(value, bool) or not isinstance(value, (int, float)) or not math.isfinite(value) or value <= 0:
            raise ValueError(f'{key} must be a positive finite number')
    if seed is not None and (isinstance(seed, bool) or not isinstance(seed, int) or seed == source_seed):
        raise ValueError('Seed must be an integer different from the source seed')
    return general


def _edit_master(text, general):
    deltas = {}
    for key, value in general.items():
        pattern = re.compile(rf'^(  {key}: )(\S+)(?=\r?$)', re.MULTILINE)
        matches = pattern.findall(text)
        if len(matches) != 1:
            raise ValueError(f'{key} must appear exactly once in the master General block')
        before = float(matches[0][1])
        if before == value:
            raise ValueError(f'{key} already equals {value}')
        text = pattern.sub(lambda m: m.group(1) + repr(float(value)), text)
        deltas[f'General.{key}'] = {'before': before, 'after': float(value)}
    return text, deltas


def _verify_master(source, derived, general):
    before, after = _load(source), _load(derived)
    for key, value in general.items():
        if after['General'].pop(key) != value:
            raise ValueError(f'{key} not applied')
        before['General'].pop(key)
    if before != after:
        raise ValueError('Derived master differs beyond the declared fields')


def _verify_settings(source_study, source_matrix, matrix, general):
    master = _load(source_study / source_matrix['master_file'])
    declared = source_matrix['settings'].get('fixed_time_step_s')
    if declared is not None and declared != master['General'].get('ImplicitConstantTimeStep'):
        raise ValueError('Source fixed_time_step_s differs from its master')
    before, after = dict(source_matrix['settings']), dict(matrix['settings'])
    if 'ImplicitConstantTimeStep' in general:
        if after.pop('fixed_time_step_s', None) != general['ImplicitConstantTimeStep']:
            raise ValueError('Derived fixed_time_step_s differs from the declared value')
        before.pop('fixed_time_step_s', None)
    if before != after:
        raise ValueError('Derived study settings differ beyond the declared fields')


def _verify_changes(source_study, source_matrix, output, matrix, seed):
    identity = lambda cases: [(c['hs_m'], c['tp_s']) for c in cases]
    if identity(source_matrix['cases']) != identity(matrix['cases']):
        raise ValueError('Derived case coverage differs from the source matrix')
    for old, new in zip(source_matrix['cases'], matrix['cases']):
        if (old['hs_m'], old['tp_s']) != (new['hs_m'], new['tp_s']):
            raise ValueError('Derived case order differs from source')
        old_raw = (source_study / old['change_file']).read_bytes()
        new_raw = (output / new['change_file']).read_bytes()
        if seed is None:
            if old_raw != new_raw:
                raise ValueError('Change file differs without a declared seed change')
            continue
        a, b = yaml.safe_load(old_raw), yaml.safe_load(new_raw)
        if b['Environment']['WaveTrains'][0].pop('WaveSeed') != seed:
            raise ValueError('Seed not applied')
        a['Environment']['WaveTrains'][0].pop('WaveSeed')
        if a != b:
            raise ValueError('Change file differs beyond the wave seed')


def prepare_variant_study(source_study, output_dir, *, general=None, seed=None, purpose):
    """Write a derived study plus handover.json and variant.json; nothing is solved."""
    source_study, output = Path(source_study).resolve(), Path(output_dir).resolve()
    if output.exists():
        raise FileExistsError('Variant study output must be new')
    source_matrix, source_seed = _source(source_study)
    general = _validate(general, seed, source_seed)
    # Byte-exact decode: text-mode reads would normalise CRLF and alter unrelated bytes.
    master_text = (source_study / source_matrix['master_file']).read_bytes().decode('utf-8')
    derived_text, deltas = _edit_master(master_text, general)
    staging = output.parent / f'{output.name}.source-master.yml'
    partial = output.parent / f'{output.name}.partial'
    if staging.exists() or partial.exists():
        raise FileExistsError('Staging paths must be new')
    output.parent.mkdir(parents=True, exist_ok=True)
    try:
        staging.write_bytes(derived_text.encode('utf-8'))
        _verify_master(source_study / source_matrix['master_file'], staging, general)
        settings = source_matrix['settings']
        matrix = prepare_matrix(staging, compute_hash(staging), partial,
                                seed=source_seed if seed is None else seed,
                                buildup=settings['buildup_s'], duration=settings['duration_s'],
                                sample_interval=settings['sample_interval_s'], gamma=settings['gamma'],
                                components=settings['components'], max_time_step=settings['max_time_step_s'])
        _verify_settings(source_study, source_matrix, matrix, general)
        _verify_changes(source_study, source_matrix, partial, matrix, seed)
        partial.rename(output)
    except BaseException:
        # Derived files carry no evidence of their own; publish only a fully verified study.
        shutil.rmtree(partial, ignore_errors=True)
        raise
    finally:
        staging.unlink(missing_ok=True)
    handover = {'status': 'prepared', 'matrix_sha256': compute_hash(output / 'matrix.json'),
                'master_sha256': matrix['master_sha256'], 'engineering_acceptance': 'NOT EVALUATED',
                'cases': [{'index': i, 'hs_m': c['hs_m'], 'tp_s': c['tp_s'], 'seed': c['seed'],
                           'status': 'MISSING', 'engineering_acceptance': 'NOT EVALUATED'}
                          for i, c in enumerate(matrix['cases'])]}
    (output / 'handover.json').write_text(json.dumps(handover, indent=2), encoding='utf-8')
    record = {'schema_version': 1, 'purpose': purpose, 'source_study': str(source_study),
              'source_matrix_sha256': compute_hash(source_study / 'matrix.json'),
              'source_master_sha256': source_matrix['master_sha256'],
              'matrix_sha256': handover['matrix_sha256'], 'master_sha256': matrix['master_sha256'],
              'master_deltas': deltas, 'seed': {'before': source_seed, 'after': seed} if seed is not None else None,
              'unchanged': 'All other master fields and all change files verified identical'
                           + ('' if seed is None else ' apart from WaveSeed'),
              'engineering_acceptance': 'NOT EVALUATED'}
    (output / 'variant.json').write_text(json.dumps(record, indent=2), encoding='utf-8')
    return record


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('source_study', type=Path)
    parser.add_argument('output_dir', type=Path)
    parser.add_argument('--purpose', required=True)
    parser.add_argument('--timestep', type=float)
    parser.add_argument('--seed', type=int)
    args = parser.parse_args()
    general = {'ImplicitConstantTimeStep': args.timestep} if args.timestep is not None else None
    print(json.dumps(prepare_variant_study(args.source_study, args.output_dir, general=general,
                                           seed=args.seed, purpose=args.purpose), indent=2))


if __name__ == '__main__':
    main()
