# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""External workbook conversion and parity regression, with synthetic schema data."""

import copy
import hashlib
import json
import math
import os
from pathlib import Path
import subprocess
import sys

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[3]


def synthetic_workbook_and_map(tmp_path):
    from openpyxl import Workbook

    config = yaml.safe_load((ROOT / 'examples/workflows/on-bottom-stability-e305/input.yml').read_text())
    segment = copy.deepcopy(config['on_bottom_stability_e305']['segments'][0])
    steel_mass = math.pi / 4 * (1 - 0.8**2) * 8
    # Synthetic added mass sets available weight to exactly 24 N/m at g=9.81.
    segment['pipe'].update(concrete_m=0, added_mass_kg_m=24/9.81-steel_mass+math.pi/4)
    for case, current in zip(segment['cases'], (2, math.sqrt(6))):
        case.update(wave_velocity_m_s=0, current_m_s=current, Fw=1, cl=0,
                    target_safety_factor=1)
    wb = Workbook()
    sheet = wb.active
    sheet.title = 'Synthetic'
    # FD=1.5*U**2 and mu=0.5 give demands 12 and 18 N/m.
    for address, number in {'A1': 9.81, 'A2': 12.0, 'A3': 24.0, 'A4': 0.5,
                            'B2': 18.0, 'B3': 24.0, 'B4': 0.75}.items():
        sheet[address] = number
    workbook = tmp_path / 'synthetic_workbook.xlsx'
    wb.save(workbook)
    expected = {segment['id']: {}}
    for role, column in [('extreme_wave', 'A'), ('extreme_current', 'B')]:
        expected[segment['id']][role] = {name: {'sheet': 'Synthetic', 'cell': f'{column}{row}'}
            for name, row in [('required_weight_n_m', 2), ('available_weight_n_m', 3), ('utilization', 4)]}
    mapping = dict(g={'sheet': 'Synthetic', 'cell': 'A1'},
                   segments=[{'literal': segment}], expected=expected)
    map_file = tmp_path / 'synthetic_cell_map.json'
    map_file.write_text(json.dumps(mapping))
    return workbook, map_file, mapping, segment['id']


def test_synthetic_workbook_conversion_is_external_cached_and_reproducible(tmp_path):
    from digitalmodel.subsea.on_bottom_stability.e305_fixture import load_fixture, convert_workbook, compare_fixture

    workbook, map_file, mapping, identifier = synthetic_workbook_and_map(tmp_path)
    output = tmp_path / 'synthetic_fixture.json'
    env = {key: value for key, value in os.environ.items()
           if key not in ('GIT_DIR', 'GIT_WORK_TREE', 'GIT_COMMON_DIR')}
    subprocess.run([sys.executable, str(ROOT / 'scripts/subsea/convert_e305_workbook.py'),
                    str(workbook), '--map', str(map_file), '--output', str(output)],
                   check=True, capture_output=True, text=True, env=env)
    first = output.read_bytes()
    convert_workbook(workbook, map_file, output, repository_root=ROOT)
    assert output.read_bytes() == first
    fixture = load_fixture(output)
    comparisons = compare_fixture(fixture)
    assert len(comparisons) == 6
    assert max(row['relative_error'] for row in comparisons) < 1e-12
    fixture['expected'][identifier]['extreme_wave']['required_weight_n_m'] *= 1.01
    assert max(row['relative_error'] for row in compare_fixture(fixture)) > 0.001
    assert fixture['source']['workbook_sha256'] == hashlib.sha256(workbook.read_bytes()).hexdigest()
    assert fixture['source']['cell_map_sha256'] == hashlib.sha256(map_file.read_bytes()).hexdigest()
    with pytest.raises(ValueError, match='outside'):
        convert_workbook(workbook, map_file, ROOT / 'e305_private_fixture_forbidden.json', repository_root=ROOT)
    original_map = map_file.read_bytes()
    mapping['expected'][identifier]['extreme_wave']['required_weight_n_m'] = {'literal': 12.0}
    map_file.write_text(json.dumps(mapping))
    tampered = hashlib.sha256(map_file.read_bytes()).hexdigest()
    try:
        with pytest.raises(ValueError, match='cached'):
            convert_workbook(workbook, map_file, output, repository_root=ROOT)
    finally:
        map_file.write_bytes(original_map)
    restored = hashlib.sha256(map_file.read_bytes()).hexdigest()
    before = fixture['source']['cell_map_sha256']
    assert before == restored and before != tampered
    assert output.read_bytes() == first
    proof = {'before': before, 'tampered': tampered, 'restored': restored}
    (tmp_path / 'fixture_probe_hashes.json').write_text(json.dumps(proof, sort_keys=True)+'\n')


def test_present_malformed_private_fixture_is_not_a_skip(tmp_path):
    from digitalmodel.subsea.on_bottom_stability.e305_fixture import load_fixture

    path = tmp_path / 'synthetic_malformed_fixture.json'
    path.write_text('{"schema_version":1,"g":9.81,"segments":[],"expected":{},"source":{}}')
    with pytest.raises(ValueError):
        load_fixture(path)


def test_private_goby_both_cases_with_g_9_81_within_0_1_percent():
    from digitalmodel.subsea.on_bottom_stability.e305_fixture import load_fixture, compare_fixture

    configured = os.environ.get('E305_GOBY_FIXTURE')
    if not configured:
        pytest.skip('private fixture absent: E305_GOBY_FIXTURE is not configured; reproduction not observed')
    path = Path(configured)
    if not path.exists():
        pytest.skip('configured private fixture is absent; reproduction not observed')
    fixture = load_fixture(path)
    comparisons = compare_fixture(fixture)
    assert comparisons
    assert all(row['relative_error'] <= 0.001 for row in comparisons)
