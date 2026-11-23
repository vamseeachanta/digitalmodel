"""Numeric regression for ISO 10424-1:2004, Table 14 (all 23 rows)."""
from copy import deepcopy

import pytest
import yaml

from digitalmodel.bha_database.loader import BhaDatabaseError
from digitalmodel.bha_database import iso_table14

# designation, OD (mm), bore (mm), lengths (m), bevel (mm), reference BSR (1).
# Source: ISO 10424-1:2004, Table 14; decimal commas normalized.
EXPECTED = [
    ('NC23-31', 79.4, 31.8, [9.14], 76.2, 2.57),
    ('NC26-35', 88.9, 38.1, [9.14], 82.9, 2.42),
    ('NC31-41', 104.8, 50.8, [9.14, 9.45], 100.4, 2.43),
    ('NC35-47', 120.6, 50.8, [9.14, 9.45], 114.7, 2.58),
    ('NC38-50', 127.0, 57.2, [9.14, 9.45], 121.0, 2.38),
    ('NC44-60', 152.4, 57.2, [9.14, 9.45], 144.5, 2.49),
    ('NC44-60', 152.4, 71.4, [9.14, 9.45], 144.5, 2.84),
    ('NC44-62', 158.8, 57.2, [9.14, 9.45], 149.2, 2.91),
    ('NC46-62', 158.8, 71.4, [9.14, 9.45], 150.0, 2.63),
    ('NC46-65', 165.1, 57.2, [9.14, 9.45], 154.8, 2.76),
    ('NC46-65', 165.1, 71.4, [9.14, 9.45], 154.8, 3.05),
    ('NC46-67', 171.4, 57.2, [9.14, 9.45], 159.5, 3.18),
    ('NC50-70', 177.8, 57.2, [9.14, 9.45], 164.7, 2.54),
    ('NC50-70', 177.8, 71.4, [9.14, 9.45], 164.7, 2.73),
    ('NC50-72', 184.2, 71.4, [9.14, 9.45], 169.5, 3.12),
    ('NC56-77', 196.8, 71.4, [9.14, 9.45], 185.3, 2.70),
    ('NC56-80', 203.2, 71.4, [9.14, 9.45], 190.1, 3.02),
    ('6 5/8 REG', 209.6, 71.4, [9.14, 9.45], 195.6, 2.93),
    ('NC61-90', 228.6, 71.4, [9.14, 9.45], 212.7, 3.17),
    ('7 5/8 REG', 241.3, 76.2, [9.14, 9.45], 223.8, 2.81),
    ('NC70-97', 247.6, 76.2, [9.14, 9.45], 232.6, 2.57),
    ('NC70-100', 254.0, 76.2, [9.14, 9.45], 237.3, 2.81),
    ('8 5/8 REG', 279.4, 76.2, [9.14, 9.45], 266.7, 2.84),
]
FIELDS = ('designation', 'od', 'id', 'length', 'bevel_diameter', 'reference_bsr')


def test_all_table14_cells():
    data = iso_table14.load_table14()
    actual = [tuple(row[f]['value'] for f in FIELDS) for row in data['components']]
    assert actual == EXPECTED
    for row in data['components']:
        weight = 2.67 * ((row['od']['value'] / 25.4) ** 2
                         - (row['id']['value'] / 25.4) ** 2)
        assert row['weight_per_length']['value'] == pytest.approx(weight, abs=1e-9)
    assert 'ISO 10424-1:2004, Table 14' in iso_table14.load_table14.__doc__
    assert iso_table14.DEFAULT_PATH.read_text(encoding='utf-8').startswith(
        '# Source: ISO 10424-1:2004, Table 14')


@pytest.mark.parametrize('field', (*FIELDS, 'weight_per_length'))
@pytest.mark.parametrize('key', ('unit', 'source_key', 'basis'))
def test_rejects_relabelled_quantity(tmp_path, field, key):
    data = deepcopy(iso_table14.load_table14())
    data['components'][0][field][key] = 'wrong'
    saved = tmp_path / 'rows.yml'
    saved.write_text(yaml.safe_dump(data))
    with pytest.raises(BhaDatabaseError):
        iso_table14.load_table14(saved)


@pytest.mark.parametrize('corruption', ('count', 'duplicate', 'source', 'edition',
                                       'locator', 'nonfinite', 'schema'))
def test_rejects_invalid_table(tmp_path, corruption):
    data = deepcopy(iso_table14.load_table14())
    if corruption == 'count':
        data['components'].pop()
    elif corruption == 'duplicate':
        data['components'][1] = deepcopy(data['components'][0])
    elif corruption in ('source', 'edition', 'locator'):
        key = 'key' if corruption == 'source' else corruption
        data['sources'][0][key] = 'wrong'
    elif corruption == 'nonfinite':
        data['components'][0]['od']['value'] = float('nan')
    else:
        data['schema_version'] = True
    saved = tmp_path / 'rows.yml'
    saved.write_text(yaml.safe_dump(data))
    with pytest.raises(BhaDatabaseError):
        iso_table14.load_table14(saved)


def test_bounded_reader_rejects_duplicate_yaml(tmp_path):
    saved = tmp_path / 'rows.yml'
    saved.write_text('schema_version: 1\nschema_version: 1\n')
    with pytest.raises(BhaDatabaseError, match='duplicate YAML key'):
        iso_table14.load_table14(saved)


def test_reference_rows_are_not_assembly_components():
    from pydantic import ValidationError
    from digitalmodel.bha_database.schema import Component

    with pytest.raises(ValidationError) as caught:
        Component.model_validate(iso_table14.load_table14()['components'][0])
    missing = {e['loc'][0] for e in caught.value.errors() if e['type'] == 'missing'}
    assert {'source_key', 'od_max', 'top_connection'} <= missing


def test_explicit_path_and_detached_return(tmp_path, monkeypatch):
    saved = tmp_path / 'rows.yml'
    saved.write_bytes(iso_table14.DEFAULT_PATH.read_bytes())
    monkeypatch.chdir(tmp_path)
    data = iso_table14.load_table14(saved)
    data['components'][0]['od']['value'] = -1
    assert iso_table14.load_table14(saved)['components'][0]['od']['value'] == 79.4


def test_missing_file_reports_explicit_path(tmp_path):
    with pytest.raises(BhaDatabaseError, match='pass an explicit YAML path'):
        iso_table14.load_table14(tmp_path / 'absent.yml')


def test_supplied_strings_reconcile_with_numeric_cells():
    for row in iso_table14.load_table14()['components']:
        for field in FIELDS:
            cell = row[field]
            supplied = cell['as_published']
            if field == 'designation':
                parsed = supplied
            elif field == 'length':
                parsed = [float(s.replace(',', '.')) for s in supplied.split(' or ')]
            else:
                parsed = float(supplied.removesuffix(':1').replace(',', '.'))
            assert parsed == cell['value'], (row['component_id'], field)


def test_metadata_mutations_and_later_row(tmp_path):
    original = iso_table14.load_table14()
    mutations = [('sources', 0, 'sha256'), ('sources', 0, 'title'),
                 ('sources', 0, 'type')]
    for section, index, field in mutations:
        data = deepcopy(original)
        data[section][index][field] = 'wrong'
        saved = tmp_path / 'rows.yml'
        saved.write_text(yaml.safe_dump(data))
        with pytest.raises(BhaDatabaseError):
            iso_table14.load_table14(saved)
    for field, key, value in [('od', 'unit', 'inch'),
                              ('id', 'value', 999.0),
                              ('bevel_diameter', 'value', 999.0),
                              ('od', 'value', float('inf')),
                              ('weight_per_length', 'formula', 'wrong'),
                              ('designation', 'as_published', '')]:
        data = deepcopy(original)
        data['components'][-1][field][key] = value
        saved.write_text(yaml.safe_dump(data))
        with pytest.raises(BhaDatabaseError):
            iso_table14.load_table14(saved)


def test_supplied_bevel_remains_pending():
    """Issues #2716 and #2629 require a licensed printed-page comparison.

    The existing numeric regression pins the supplied transcription of ISO
    10424-1:2004, Table 14; it does not verify the disputed bevel value.
    """
    data = iso_table14.load_table14()
    row = next(row for row in data['components']
               if row['designation']['value'] == '6 5/8 REG')
    cell = row['bevel_diameter']
    assert cell['verification'] == 'pending'
    assert 'licensed' in cell['verification_reason']
    assert cell['verification_issue'] == '#2629'
    assert data['pending_cells'] == [{
        'component_id': row['component_id'],
        'field': 'bevel_diameter',
        'source_key': cell['source_key'],
        'reason': cell['verification_reason'],
        'issue': cell['verification_issue'],
    }]


def write_verification_case(tmp_path, **metadata):
    """Attach synthetic status metadata to an otherwise unchanged table."""
    data = yaml.safe_load(iso_table14.DEFAULT_PATH.read_text(encoding='utf-8'))
    data['components'][0]['od'].update(metadata)
    saved = tmp_path / 'rows.yml'
    saved.write_text(yaml.safe_dump(data), encoding='utf-8')
    return saved


@pytest.mark.parametrize('status', ['unknown', '', 'PENDING', None, True, [], {}])
def test_rejects_invalid_verification(tmp_path, status):
    """The status vocabulary is specified by issue #2716, not a numeric limit."""
    saved = write_verification_case(tmp_path, verification=status)
    with pytest.raises(BhaDatabaseError, match='od verification'):
        iso_table14.load_table14(saved)


@pytest.mark.parametrize('key', ['verification_reason', 'verification_issue'])
@pytest.mark.parametrize('value', [None, '', '   '])
def test_pending_requires_context(tmp_path, key, value):
    metadata = dict(verification='pending', verification_reason='Synthetic review',
                    verification_issue='https://example.org/review')
    metadata[key] = value
    saved = write_verification_case(tmp_path, **metadata)
    with pytest.raises(BhaDatabaseError, match=f'od {key}'):
        iso_table14.load_table14(saved)


@pytest.mark.parametrize('locator', [None, '', '   '])
def test_verified_requires_comparison_locator(tmp_path, locator):
    """A status alone cannot record the printed-page comparison in #2629."""
    saved = write_verification_case(tmp_path, verification='verified',
                                    verification_locator=locator)
    with pytest.raises(BhaDatabaseError, match='od verification_locator'):
        iso_table14.load_table14(saved)


def test_verified_status_is_retained_without_clearing_other_pending_cells(tmp_path):
    """Synthetic locator tests metadata handling, not source authenticity."""
    saved = write_verification_case(tmp_path, verification='verified',
                                    verification_locator='Synthetic printed-page locator')
    data = iso_table14.load_table14(saved)
    assert data['components'][0]['od']['verification'] == 'verified'
    assert [cell['field'] for cell in data['pending_cells']] == ['bevel_diameter']
    assert 'verification' not in data['components'][0]['id']


def test_reports_all_pending_cells_and_recomputes_summary(tmp_path, monkeypatch):
    saved = write_verification_case(
        tmp_path, verification='pending', verification_reason='Synthetic review',
        verification_issue='https://example.org/review')
    raw = yaml.safe_load(saved.read_text(encoding='utf-8'))
    raw['pending_cells'] = []  # A saved summary must not hide pending quantities.
    saved.write_text(yaml.safe_dump(raw), encoding='utf-8')
    monkeypatch.chdir(tmp_path)
    data = iso_table14.load_table14(saved)
    assert [(cell['component_id'], cell['field']) for cell in data['pending_cells']] == [
        (raw['components'][0]['component_id'], 'od'),
        (next(row['component_id'] for row in raw['components']
              if row['designation']['value'] == '6 5/8 REG'), 'bevel_diameter'),
    ]
    data['pending_cells'].clear()
    assert iso_table14.load_table14(saved)['pending_cells']


def test_unmarked_table_does_not_imply_verified(tmp_path):
    data = yaml.safe_load(iso_table14.DEFAULT_PATH.read_text(encoding='utf-8'))
    for row in data['components']:
        for field in FIELDS:
            for key in ('verification', 'verification_reason', 'verification_issue'):
                row[field].pop(key, None)
    saved = tmp_path / 'rows.yml'
    saved.write_text(yaml.safe_dump(data), encoding='utf-8')
    loaded = iso_table14.load_table14(saved)
    assert loaded['pending_cells'] == []
    assert loaded['components'] == data['components']
