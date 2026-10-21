# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Preparation reporting uses independently checked arithmetic, never a solver."""

import builtins
import csv
import importlib.util
import json
import shutil
from pathlib import Path

import pytest
from bs4 import BeautifulSoup

from digitalmodel.drilling_riser import generic_campaign

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / 'scripts/reporting/build_generic_riser_preparation_report.py'


@pytest.fixture(autouse=True)
def forbid_solver(monkeypatch):
    original = builtins.__import__

    def guarded(name, *args, **kwargs):
        if name.split('.')[0] == 'OrcFxAPI':
            raise AssertionError('Licensed interface must not be imported')
        return original(name, *args, **kwargs)

    monkeypatch.setattr(builtins, '__import__', guarded)


@pytest.fixture
def builder():
    spec = importlib.util.spec_from_file_location('preparation_report', SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@pytest.fixture
def campaign(tmp_path):
    destination = tmp_path / 'inputs'
    shutil.copytree(ROOT / generic_campaign.DEST, destination)
    return destination


def test_preparation_report_without_results_or_runtime_criteria(builder):
    spec = builder.build_spec(ROOT / generic_campaign.DEST)
    assert [s.key for s in spec.sections][:2] == ['summary', 'revision-history']
    tables = {block.title: block for s in spec.sections for block in s.blocks
              if hasattr(block, 'columns')}
    assert len(tables['Offset and current cases'].rows) == 24
    assert len(tables['Analytical column screens'].rows) == 24
    pending = tables['Pending connected-mode comparisons'].rows
    assert len(pending) == 24
    assert all(row[1:] == ['NOT_EVALUATED'] * 5 for row in pending)
    prose = ' '.join(block.markdown for s in spec.sections for block in s.blocks
                     if hasattr(block, 'markdown'))
    assert 'No operating window is established' in prose
    assert 'Geometric extension is not mechanical tensioner stroke' in prose
    assert 'assumed, based on previous experience' in prose


def test_worked_offset_example_is_independent(builder):
    """Case-table assumptions: depth 1020 m, offset 5% => 1020*5/100 = 51 m.

    For the specified main-column length 1020 m, a straight chord extends by
    sqrt(1020**2 + 51**2) - 1020 = 1.2742041195 m, displayed as 1.3 m.
    This Euclidean hand calculation is geometric, not a stroke prediction.
    """
    spec = builder.build_spec(ROOT / generic_campaign.DEST)
    cases = next(block for s in spec.sections for block in s.blocks
                 if getattr(block, 'title', '') == 'Offset and current cases')
    assert next(row for row in cases.rows if row[0] == 'o5-c0')[2] == '51.0'
    screens = next(block for s in spec.sections for block in s.blocks
                   if getattr(block, 'title', '') == 'Analytical column screens')
    assert next(row for row in screens.rows if row[0] == 'o5-c0')[-1] == '1.3'


def test_html_is_deterministic_and_example_is_current(builder):
    first = builder.render(ROOT / generic_campaign.DEST)
    assert first == builder.render(ROOT / generic_campaign.DEST)
    expected = ROOT / generic_campaign.DEST / 'preparation-report.html'
    assert first.rstrip('\n') + '\n' == expected.read_text(encoding='utf-8')
    soup = BeautifulSoup(first, 'html.parser')
    sections = soup.select('main.main > section')
    assert [s['id'] for s in sections][:2] == ['summary', 'revision-history']
    assert 'NOT_EVALUATED' in soup.get_text()
    assert 'No external references cited' not in soup.get_text()
    assert not soup.select('script[src]')


@pytest.mark.parametrize('target', ['case-table.yml', 'screens/column-screen.csv'])
def test_rejects_changed_input_bytes(builder, campaign, target):
    path = campaign / target
    path.write_text(path.read_text() + '\n', encoding='utf-8')
    with pytest.raises(ValueError, match='identity'):
        builder.build_spec(campaign)


def test_rejects_changed_calculation_fingerprint(builder, campaign):
    path = campaign / 'screens/basis.json'
    basis = json.loads(path.read_text())
    basis['calculation_fingerprints']['stackup.py'] = 'stale'
    path.write_text(json.dumps(basis), encoding='utf-8')
    with pytest.raises(ValueError, match='identity'):
        builder.build_spec(campaign)


@pytest.mark.parametrize('mutation', [
    'duplicate', 'missing', 'nan', 'negative', 'offset', 'missing_column',
])
def test_rejects_invalid_screen_even_with_updated_digest(builder, campaign, mutation):
    path = campaign / 'screens/column-screen.csv'
    with path.open(newline='') as stream:
        reader = csv.DictReader(stream)
        fields, rows = reader.fieldnames, list(reader)
    if mutation == 'duplicate':
        rows[-1] = rows[0]
    elif mutation == 'missing':
        rows.pop()
    elif mutation == 'nan':
        rows[0]['axial_plus_bending_stress_mpa'] = 'nan'
    elif mutation == 'negative':
        rows[0]['geometric_extension_m'] = '-1'
    elif mutation == 'offset':
        rows[0]['offset_pct'] = '99'
    else:
        fields.remove('case_id')
        for row in rows:
            row.pop('case_id')
    with path.open('w', newline='') as stream:
        writer = csv.DictWriter(stream, fieldnames=fields, lineterminator='\n')
        writer.writeheader()
        writer.writerows(rows)
    basis_path = campaign / 'screens/basis.json'
    basis = json.loads(basis_path.read_text())
    basis['screen_csv_sha256'] = generic_campaign.digest(path)
    basis_path.write_text(json.dumps(basis), encoding='utf-8')
    with pytest.raises(ValueError, match='screen'):
        builder.build_spec(campaign)


def test_rejects_changed_basis_even_if_its_digest_is_updated(builder, campaign):
    import yaml

    path = campaign / 'case-table.yml'
    table = yaml.safe_load(path.read_text())
    table['cases'][0]['current_mps']['value'] = 99
    path.write_text(yaml.safe_dump(table), encoding='utf-8')
    basis_path = campaign / 'screens/basis.json'
    basis = json.loads(basis_path.read_text())
    basis['case_table_sha256'] = generic_campaign.digest(path)
    basis_path.write_text(json.dumps(basis), encoding='utf-8')
    with pytest.raises(ValueError, match='design basis'):
        builder.build_spec(campaign)
