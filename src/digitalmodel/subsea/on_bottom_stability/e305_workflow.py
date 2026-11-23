# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Isolated two-case segment-table workflow; no licensed program interface."""

from __future__ import annotations

import csv
import hashlib
import io
import json
from dataclasses import asdict
from pathlib import Path
from typing import Any

from digitalmodel.citations.schema import Citation

from digitalmodel.subsea.on_bottom_stability.e305_simplified import (
    Case, Pipe, WeightResult, diameter_averaged_current, dimensionless_ratios, evaluate_case,
    wave_from_sea_state, weight_build_up,
)
from digitalmodel.subsea.on_bottom_stability.e305_tables import TableSet, load_tables


BASENAME = 'on_bottom_stability_e305'


def _cases(raw_cases: Any) -> list[dict[str, Any]]:
    if not isinstance(raw_cases, list) or len(raw_cases) != 2:
        raise ValueError('both distinct load-case roles are required')
    if any(not isinstance(case, dict) for case in raw_cases):
        raise ValueError('load-case rows must be objects')
    if {case.get('role') for case in raw_cases} != {'extreme_wave', 'extreme_current'}:
        raise ValueError('both distinct load-case roles are required')
    return raw_cases


def _make_case(raw: dict[str, Any], *, weight: WeightResult, tables: TableSet | None,
               g: float) -> tuple[Case, dict[str, Any]]:
    values = dict(raw)
    lookups = {}
    if 'sea_state' in values:
        if 'wave_velocity_m_s' in values or 'period_s' in values:
            raise ValueError('sea_state and typed wave kinematics are mutually exclusive')
        velocity, period = wave_from_sea_state(**values.pop('sea_state'), g=g)
        values.update(wave_velocity_m_s=velocity, period_s=period)
    if tables is None:
        return Case(**values), lookups
    if 'R' in values or 'Fw' in values:
        raise ValueError('table mode requires table factors rather than typed R or Fw')
    if 'reference_current' in values:
        if 'current_m_s' in values:
            raise ValueError('reference and averaged current inputs are mutually exclusive')
        current = values.pop('reference_current')
        roughness = tables.lookup('roughness', grain_size_m=current['grain_size_m'])
        lookups['roughness'] = asdict(roughness)
        values['current_m_s'] = diameter_averaged_current(
            current['velocity_m_s'], reference_height_m=current['height_m'],
            diameter_m=weight.hydrodynamic_diameter_m, roughness_m=roughness.value)
    if 'R_coordinates' not in values:
        raise ValueError('table mode requires R_coordinates')
    reduction = tables.lookup('R', **values.pop('R_coordinates'))
    lookups['R'] = asdict(reduction)
    K, M = dimensionless_ratios(values['wave_velocity_m_s'], values['period_s'],
                                values['current_m_s'], weight.hydrodynamic_diameter_m)
    if M is None:
        raise ValueError('zero-wave table mode has undefined M; a supported basis is required')
    calibration = tables.lookup('Fw', K=K, M=M)
    lookups['Fw'] = asdict(calibration)
    if 'clay_coordinates' in values:
        if 'mu' in values:
            raise ValueError('typed friction and clay lookup are mutually exclusive')
        friction = tables.lookup('clay_friction', **values.pop('clay_coordinates'))
        values['mu'] = friction.value
        lookups['clay_friction'] = asdict(friction)
    values.update(R=reduction.value, Fw=calibration.value)
    return Case(**values), lookups


def _segment(raw: dict[str, Any], *, mode: str, g: float, tables: TableSet | None) -> dict[str, Any]:
    if not isinstance(raw, dict) or not isinstance(raw.get('id'), str) or not raw['id'].strip():
        raise ValueError('each segment requires a nonempty identifier')
    pipe = Pipe(**raw['pipe'])
    weight = weight_build_up(pipe, g=g)
    results = []
    for raw_case in _cases(raw['cases']):
        case, lookups = _make_case(raw_case, weight=weight, tables=tables, g=g)
        citations = tuple(Citation(**lookup['citation']) for lookup in lookups.values())
        result = evaluate_case(case, diameter_m=weight.hydrodynamic_diameter_m,
                               rho_water_kg_m3=pipe.water_density_kg_m3,
                               available_weight_n_m=weight.submerged_weight_n_m,
                               citations=citations,
                               factor_source='licensed_tables' if tables else 'typed_parity')
        results.append(asdict(result) | {'inputs': asdict(case), 'table_lookups': lookups})
    return {'id': raw['id'], 'pipe': asdict(pipe), 'weight': asdict(weight), 'cases': results}


def _json_bytes(value: Any) -> bytes:
    return (json.dumps(value, indent=2, sort_keys=True, allow_nan=False) + '\n').encode('utf-8')


def _csv_bytes(payload: dict[str, Any]) -> bytes:
    stream = io.StringIO(newline='')
    columns = ('segment_id', 'role', 'required_weight_n_m', 'available_weight_n_m',
               'utilization', 'safety_factor', 'phase_rad', 'R', 'Fw', 'K', 'M',
               'os_f101_status', 'qualified', 'gamma_st_requested', 'notes')
    writer = csv.DictWriter(stream, fieldnames=columns, lineterminator='\n')
    writer.writeheader()
    for segment in payload['segments']:
        for case in segment['cases']:
            writer.writerow({'segment_id': segment['id']} | {key: case[key] for key in columns[1:] if key != 'notes'}
                            | {'notes': 'gamma_st is recorded only and does not enter the demand'})
    return stream.getvalue().encode('utf-8')


def _write_outputs(payload: dict[str, Any], directory: Path) -> dict[str, str]:
    directory.mkdir(parents=True, exist_ok=True)
    outputs = {'e305.json': _json_bytes(payload), 'e305.csv': _csv_bytes(payload)}
    digests = {name: hashlib.sha256(data).hexdigest() for name, data in outputs.items()}
    outputs['e305.manifest.json'] = _json_bytes({'schema_version': 1, 'outputs': digests.copy()})
    for name, data in outputs.items():
        target = directory / name
        target.write_bytes(data)
        if target.read_bytes() != data:
            raise ValueError('output readback differs from the generated bytes')
        digests[name] = hashlib.sha256(data).hexdigest()
    return digests


def router(cfg: dict[str, Any]) -> dict[str, Any]:
    """Evaluate a table of segments in explicit parity or external-table mode."""
    params = cfg[BASENAME]
    mode = params.get('mode')
    if mode not in ('parity', 'tables'):
        raise ValueError('an explicit parity or tables mode is required')
    g = params['g']
    tables = load_tables(params.get('tables_file'), wiki_root=params.get('wiki_root')) if mode == 'tables' else None
    raw_segments = params['segments']
    if not isinstance(raw_segments, list) or not raw_segments:
        raise ValueError('segments must be a nonempty table')
    segments = [_segment(raw, mode=mode, g=g, tables=tables) for raw in raw_segments]
    if len({segment['id'] for segment in segments}) != len(segments):
        raise ValueError('segment identifiers must be unique')
    payload = {'schema_version': 1, 'mode': mode, 'g': g, 'qualified': False,
               'os_f101_status': 'not_established',
               'limitations': ['edition-specific E305 equivalence is not established',
                               'OS-F101 comparator is not implemented',
                               'gamma_st is recorded only and does not enter the demand',
                               'private workbook reproduction is not established'],
               'table_source_sha256': tables.source_sha256 if tables else None,
               'citations': [asdict(table.citation) for table in tables.tables] if tables else [],
               'segments': segments}
    output = params.get('outputs')
    result = dict(payload)
    if output is not None:
        directory = Path(output['directory'])
        if not directory.is_absolute():
            if '_config_dir_path' not in cfg:
                raise ValueError('an absolute output directory is required without _config_dir_path')
            directory = Path(cfg['_config_dir_path']) / directory
        result['output_sha256'] = _write_outputs(payload, directory)
    params['result'] = result
    cfg[BASENAME + '_result'] = result
    return cfg
