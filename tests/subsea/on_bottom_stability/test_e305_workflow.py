# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Synthetic workflow registration, batch, table and deterministic-output tests."""

import ast
import copy
import hashlib
import json
import os
import subprocess
import sys
from pathlib import Path

import pytest
import yaml

from tests.subsea.on_bottom_stability.synthetic_e305_tables import make_synthetic_tables

ROOT = Path(__file__).resolve().parents[3]


def configuration(tmp_path):
    template = ROOT / 'examples/workflows/on-bottom-stability-e305/input.yml'
    cfg = yaml.safe_load(template.read_text())
    cfg['on_bottom_stability_e305']['outputs'] = {'directory': str(tmp_path / 'results')}
    return cfg


def test_e305_registration_and_registered_batch_are_reproducible(tmp_path):
    registry = yaml.safe_load((ROOT / 'docs/registry/workflows.yaml').read_text())
    rows = [row for row in registry['workflows'] if row['id'] == 'on-bottom-stability-e305']
    assert len(rows) == 1
    assert rows[0]['basename'] == 'on_bottom_stability_e305'
    # The shared engine eagerly imports licensed interfaces. Inspect its new
    # registration without importing those interfaces; execute the pure router.
    source = ast.parse((ROOT / 'src/digitalmodel/engine.py').read_text())
    branches = [node for node in ast.walk(source) if isinstance(node, ast.If)
                and isinstance(node.test, ast.Compare)
                and ast.unparse(node.test) == "basename == 'on_bottom_stability_e305'"]
    assert len(branches) == 1
    assert any(isinstance(node, ast.ImportFrom) and node.module ==
               'digitalmodel.subsea.on_bottom_stability.e305_workflow'
               for node in ast.walk(branches[0]))
    from digitalmodel.subsea.on_bottom_stability.e305_workflow import router

    cfg = configuration(tmp_path)
    first = router(copy.deepcopy(cfg))
    directory = tmp_path / 'results'
    outputs = {name: (directory / name).read_bytes() for name in ('e305.json', 'e305.csv', 'e305.manifest.json')}
    second = router(copy.deepcopy(cfg))
    assert all((directory / name).read_bytes() == data for name, data in outputs.items())
    result = first['on_bottom_stability_e305']['result']
    assert result == second['on_bottom_stability_e305']['result']
    assert first[rows[0]['result']['key']] == result
    assert result['qualified'] is False
    assert len(result['segments']) == 2
    assert all({case['role'] for case in row['cases']} == {'extreme_wave', 'extreme_current'}
               for row in result['segments'])
    for name, digest in result['output_sha256'].items():
        assert hashlib.sha256((directory / name).read_bytes()).hexdigest() == digest
    assert json.loads(outputs['e305.json'])['qualified'] is False
    input_path = tmp_path / 'synthetic_input.yml'
    input_path.write_text(yaml.safe_dump(cfg))
    env = {key: value for key, value in os.environ.items()
           if key not in ('GIT_DIR', 'GIT_WORK_TREE', 'GIT_COMMON_DIR')}
    subprocess.run([sys.executable, str(ROOT / 'examples/workflows/on-bottom-stability-e305/run.py'),
                    '--input', str(input_path)], check=True, capture_output=True, text=True, env=env)
    assert all((directory / name).read_bytes() == data for name, data in outputs.items())


def test_e305_batch_requires_two_roles_and_never_falls_back_from_tables(tmp_path, monkeypatch):
    from digitalmodel.subsea.on_bottom_stability.e305_workflow import router
    from digitalmodel.subsea.on_bottom_stability.e305_tables import TableError

    cfg = configuration(tmp_path)
    cfg['on_bottom_stability_e305']['segments'][0]['cases'].pop()
    with pytest.raises(ValueError, match='roles'):
        router(cfg)
    cfg = configuration(tmp_path)
    cfg['on_bottom_stability_e305']['mode'] = 'tables'
    monkeypatch.delenv('E305_TABLES_FILE', raising=False)
    with pytest.raises(TableError):
        router(cfg)


def test_e305_table_batch_retains_four_lookup_sidecars(tmp_path):
    from digitalmodel.subsea.on_bottom_stability.e305_workflow import router

    path, _, _ = make_synthetic_tables(tmp_path)
    cfg = configuration(tmp_path)
    params = cfg['on_bottom_stability_e305']
    params.update(mode='tables', tables_file=str(path), wiki_root=str(tmp_path))
    params['segments'] = params['segments'][:1]
    for case in params['segments'][0]['cases']:
        for key in ('R', 'Fw', 'mu', 'current_m_s'):
            case.pop(key)
        case.update(wave_velocity_m_s=0.1, period_s=1.0,
                    reference_current=dict(velocity_m_s=0.01, height_m=1.0, grain_size_m=1.0),
                    R_coordinates={'synthetic_coordinate': 1.0},
                    clay_coordinates={'synthetic_coordinate': 1.0})
    result = router(copy.deepcopy(cfg))['on_bottom_stability_e305']['result']
    assert result['table_source_sha256'] == hashlib.sha256(path.read_bytes()).hexdigest()
    for case in result['segments'][0]['cases']:
        assert set(case['table_lookups']) == {'roughness', 'R', 'Fw', 'clay_friction'}
        assert case['factor_source'] == 'licensed_tables'
        assert case['qualified'] is False
