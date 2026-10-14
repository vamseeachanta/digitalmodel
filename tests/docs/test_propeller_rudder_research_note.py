"""Offline validation of the issue research-note transcription.

Known note anomalies (retained, not resolved):
- Located, not read says 16; its top-level semicolon-separated groups number 11.
  Compound groups remain intact; no absent bibliographic details are inferred.
"""

import copy
import math
import re
from pathlib import Path

import pytest
import yaml

DOC_DIR = Path(__file__).resolve().parents[2] / 'docs/domains/hydrodynamics'
STEM = 'propeller-rudder-research-note-2026-10'
REQUIRED = {
    'schema_version', 'document', 'issue', 'source_of_entries', 'checked',
    'sources_read', 'located_not_read', 'equations', 'benchmark_values',
    'unconfirmed_constants', 'counts',
}


def validate_sources(data, markdown):
    """Check source identity, order, citation text and retained access standing."""
    all_ids = []
    for key, prefix, status, text_key in (
        ('sources_read', 'R', 'read', 'citation'),
        ('located_not_read', 'L', 'located_not_read', 'text'),
    ):
        entries = data[key]
        assert [e['id'] for e in entries] == [
            f'{prefix}{i}' for i in range(1, len(entries) + 1)
        ]
        for entry in entries:
            assert entry['status'] == status
            assert f"[{entry['id']}] {entry[text_key]}" in markdown
            all_ids.append(entry['id'])
    assert len(all_ids) == len(set(all_ids))
    assert set(re.findall(r'\b[RL]\d+\b', markdown)) == set(all_ids)
    assert data['counts'] == {
        'read': 9, 'located_not_read': 16,
        'enumerated_read': 9, 'enumerated_located_not_read': 11,
    }
    assert len(data['sources_read']) == data['counts']['enumerated_read']
    assert len(data['located_not_read']) == data['counts']['enumerated_located_not_read']


def validate_equations(data, markdown):
    """Compare every complete fenced block, including punctuation and spacing."""
    blocks = re.findall(r'^```[^\n]*\n(.*?)\n```$', markdown, re.MULTILINE | re.DOTALL)
    assert blocks == [e['text'] for e in data['equations']]
    assert [e['number'] for e in data['equations']] == [
        'eq. 1', 'eq. 2', 'eq. 13', 'eq. 14', 'eq. 15', 'eq. 16', 'eqs. 11, 12',
    ]
    for equation in data['equations']:
        assert {'number', 'name', 'text', 'printed_in', 'attributed_to',
                'primary_opened'} <= equation.keys()
        assert equation['primary_opened'] is False
        assert equation['number'] in markdown
        assert equation['name'] in markdown
        assert equation['printed_in'] in markdown
        assert equation['attributed_to'] in markdown
    assert 'equations are images on that page and were not recoverable' in markdown


def validate_values(data, markdown):
    """Reject symbol assignments to the two constants with lost labels."""
    constants = data['unconfirmed_constants']
    assert len(constants) == 2
    for entry in constants:
        assert entry['symbol'] is None, 'Unconfirmed constant has a symbol'
        assert entry['status'] == 'to be confirmed against the primary paper before use'
    assert [e['value'] for e in constants] == [1.09, 0.50]
    block = markdown.split('### To be confirmed against the primary paper before use\n', 1)[1]
    block = block.split('\nSoding', 1)[0]
    assert 'to be confirmed against the primary paper before use' in block.lower()
    assert 'symbol labels were lost in text extraction' in block
    assert 'must not be used in any calculation until confirmed' in block
    # Twenty labelled entries, counting each printed conditional value separately.
    assert len(data['benchmark_values']) == 20
    for entry in data['benchmark_values']:
        assert {'symbol', 'value', 'unit', 'condition', 'status'} <= entry.keys()
        assert isinstance(entry['symbol'], str) and entry['symbol']
        assert type(entry['value']) in (int, float) and math.isfinite(entry['value'])
        assert entry['value'] not in [e['value'] for e in constants]
        assert entry['status'] == 'as reproduced in the secondary paper; primary not opened'


def validate_note(data, markdown):
    """Validate structure without fetching any URL or evaluating an equation."""
    assert isinstance(data, dict) and REQUIRED <= data.keys()
    assert data['schema_version'] == 1
    assert data['issue'] == 143
    assert data['checked'] == '2026-10-11'
    assert data['source_of_entries'] == 'research note on the issue'
    assert data['document'] == f'{STEM}.md'
    assert re.findall(r'^## (\d)\.', markdown, re.MULTILINE) == list('12345678')
    assert 'Relation to the existing documents (differences listed, not resolved)' in markdown
    assert 'Observation by the research note\'s author, not from a source' in markdown
    validate_sources(data, markdown)
    validate_equations(data, markdown)
    validate_values(data, markdown)


@pytest.fixture(scope='module')
def note():
    data = yaml.safe_load((DOC_DIR / f'{STEM}.sources.yaml').read_text(encoding='utf-8'))
    markdown = (DOC_DIR / f'{STEM}.md').read_text(encoding='utf-8')
    return data, markdown


def test_note_structure_and_content(note):
    validate_note(*note)


def test_unconfirmed_symbol_is_rejected(note):
    data, markdown = note
    broken = copy.deepcopy(data)
    broken['unconfirmed_constants'][0]['symbol'] = 'invented_symbol'
    with pytest.raises(AssertionError, match='Unconfirmed constant has a symbol'):
        validate_note(broken, markdown)
