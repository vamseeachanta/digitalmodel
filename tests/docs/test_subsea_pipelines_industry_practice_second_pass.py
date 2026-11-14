"""Offline evidence and disposition checks for issue 1815's second pass."""
from copy import deepcopy
import importlib.util
import json
from pathlib import Path
import re

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
BASE = ROOT / 'docs/research/subsea-pipelines-industry-practice.md'
DOC = BASE.with_name(BASE.stem + '-second-pass.md')
SOURCES = DOC.with_suffix('.sources.yaml')
spec = importlib.util.spec_from_file_location('first_pass', Path(__file__).with_name(
    'test_subsea_pipelines_industry_practice.py'))
first = importlib.util.module_from_spec(spec)
spec.loader.exec_module(first)
section, table_rows, validate_url = first.section, first.table_rows, first.validate_url
KEYS = ['papers', 'contractors', 'tools', 'jips', 'convergence']
CRITERIA = [
    '5+ recent OMAE/ISOPE/OPT papers summarized',
    'Contractor + vessel capability matrix (Subsea7, Allseas, Saipem, TechnipFMC, McDermott, Heerema)',
    'Tool capability matrix (OrcaFlex, SAGE Profile, ABAQUS, StableLines)',
    'JIP landscape (SAFEBUCK, HOTPIPE, POSIT) documented',
    'Method convergence/divergence flagged',
]
REQUIRED = [
    ['OMAE', 'ISOPE', 'OPT'],
    ['Subsea7', 'Allseas', 'Saipem', 'TechnipFMC', 'McDermott', 'Heerema'],
    ['OrcaFlex', 'SAGE Profile', 'ABAQUS', 'StableLines'],
    ['SAFEBUCK', 'HOTPIPE', 'POSIT'],
    ['Converging', 'Diverging or unresolved'],
]
FALLBACK = ['met', 'partly met', 'partly met', 'partly met', 'met']


def source_ids(text):
    return first.source_ids(text) + re.findall(r'\[(S\d+)\]', text)


def load():
    return DOC.read_text(), yaml.safe_load(SOURCES.read_text())


def catalog(data):
    entries = yaml.safe_load(BASE.with_suffix('.sources.yaml').read_text())['sources']
    entries += data['sources']
    result = {e['id']: e for e in entries}
    assert len(result) == len(entries)
    return result


def read_evidence(ids, sources):
    assert ids and all(i in sources and sources[i]['status'] == 'read' for i in ids)


def verified_rows(text, key):
    block = section(text, {'contractors': '1. Contractor matrix updates',
                          'tools': '2. Tool matrix updates',
                          'jips': '3. JIP landscape updates'}[key])
    block = section(block, 'Verified', 3)
    if len([line for line in block.splitlines() if line.startswith('|')]) == 2:
        assert re.search(r'^\|(?:-+\|)+$', block, re.M)
        return []  # A header-only table records no verified additions.
    return table_rows(block)


def validate_document(text, data, contents):
    sources = catalog(data)
    assert [e['id'] for e in data['sources']] == [f'S{i}' for i in range(1, len(data['sources']) + 1)]
    for e in data['sources']:
        assert all(isinstance(e.get(k), str) and e[k] for k in
                   ('title', 'publisher', 'year', 'url', 'checked', 'status', 'basis'))
        assert e['status'] in {'read', 'located_not_read'}
        assert re.fullmatch(r'\d{4}-\d{2}-\d{2}', e['checked'])
        validate_url(e['url'])
    assert set(source_ids(text)) <= sources.keys()
    assert {e['id'] for e in data['sources']} <= set(source_ids(text))
    for url in re.findall(r'https?://[^\s)>]+', text):
        validate_url(url)
    for key in ('contractors', 'tools', 'jips'):
        for row in verified_rows(text, key):
            read_evidence(source_ids(' '.join(row)), sources)
            assert not re.search(r'unverified|not reached', ' '.join(row), re.I)
    first.validate_line_counts(contents)


def validate_solitaire(text, data):
    row, = [r for r in verified_rows(text, 'contractors') if r[0] == 'Allseas: Solitaire']
    joined = ' '.join(row)
    vessel = json.loads((ROOT / 'assets/data/vessel_specs_pipelay.json').read_text())[0]
    anchors = re.findall(r'^\| Solitaire\s*\| (.*)$',
                         (ROOT / 'assets/data/vessel_specs_README.md').read_text(), re.M)
    lower, = [int(m[1]) for a in anchors if (m := re.search(r'(\d+)–60 inches', a))]
    figures = re.findall(r'\d[\d,]*', re.sub(r'DP-3|2026 filename edition|\[[PS]\d+\]', '', joined))
    assert sorted(map(lambda n: int(n.replace(',', '')), figures)) == sorted([
        vessel['tensioner_capacity_tonnes'], lower, vessel['max_pipe_diameter_inches'],
        vessel['record_water_depth_m']])
    assert lower == 2 and vessel['dp_class'] + ' [S1]' in joined
    assert vessel['lay_methods'] == ['S-lay'] and 'S-lay [S2]' in joined
    for phrase in ('1,050 t total installed tension capacity [S1]',
                   '2 to 60 in outside diameter [S1]', 'achieved record 2,775 m [S2]',
                   'not an operating limit', '2026 filename edition',
                   'publication date not established', 'demonstration reference facts only'):
        assert phrase in joined
    manifest = json.loads((ROOT / 'assets/data/vessel_specs_provenance.json').read_text())
    for sid, fields in [('S1', ['tensioner_capacity_tonnes', 'max_pipe_diameter_inches', 'dp_class']),
                        ('S2', ['lay_methods', 'record_water_depth_m'])]:
        entry = catalog(data)[sid]
        assert all(entry['url'] == vessel['field_sources'][f] for f in fields)
        original, = [e for e in manifest['sources'] if e['source_url'] == entry['url']]
        assert original['status'] == 'retrieved'
        assert all(entry[k] == original[k] for k in ('sha256', 'retrieved_utc'))
        assert entry['basis'] == 'repository readback, assets/data/vessel_specs_README.md Table 2'


def derive(text, data):
    sources, coverage, results = catalog(data), data['coverage'], []
    assert list(coverage) == KEYS
    for key, required in zip(KEYS, REQUIRED):
        items = coverage[key]
        assert len({e['item'] for e in items}) == len(items)
        for e in items:
            read_evidence(e['evidence'], sources)
            assert e['pass'] in {'first', 'second'}
            content = BASE.read_text() if e['pass'] == 'first' else text
            location = e['location']
            block = section(content, location['section'])
            if key == 'convergence':
                assert e['pass'] == 'first' and e['item'] in required
                block = block.split('**' + e['item'] + '**\n', 1)[1].split('\n**', 1)[0]
                rows = re.findall(r'^\d+\. .*$', block, re.M)
            elif key == 'papers':
                rows = re.findall(r'^\d+\. .*$', block, re.M)
                assert len(e['evidence']) == 1 and e['item'] == e['evidence'][0]
                citation = sources[e['item']].get('citation', '')
                assert re.search(r'(OMAE|ISOPE|OPT)\s*202[4-9]', citation)
                assert e['item'] not in {'P6', 'P7'} and 'listing' not in citation.lower()
            else:
                if e['pass'] == 'second':
                    rows = table_rows(section(block, 'Verified', 3))
                else:
                    if key == 'contractors':
                        block = section(block, 'Rows sourced to pages read', 3)
                    rows = table_rows(block)
                row = rows[location['row']]
                column = row[2] if key == 'tools' and e['pass'] == 'first' else row[0]
                assert e['item'] in required and e['item'] in column
                if key == 'contractors':
                    assert column.startswith(e['item'] + ':')
                rows = [' | '.join(r) for r in rows]
            row = rows[location['row']]
            if key != 'convergence':
                assert e['item'] in row
            assert set(e['evidence']) <= set(source_ids(row))
            read_evidence(source_ids(row), sources)
        count = len(items)
        target = 5 if key == 'papers' else len(required)
        results.append('met' if count >= target else 'partly met' if count else 'not met')
    return results


def validate_status(text, data):
    rows = table_rows(section(text, '5. Acceptance status'))
    assert [r[0] for r in rows] == CRITERIA
    assert [r[1] for r in rows] == [', '.join(r) for r in REQUIRED]
    assert [r[3] for r in rows] == derive(text, data)
    for key, row in zip(KEYS, rows):
        for e in data['coverage'][key]:
            assert e['item'] in row[2] and set(e['evidence']) <= set(source_ids(row[2]))
    missing = set(REQUIRED[1]) - {e['item'] for e in data['coverage']['contractors']}
    assert all(name + ' missing' in rows[1][4] for name in missing)


def test_second_pass_document():
    text, data = load()
    validate_document(text, data, [text, SOURCES.read_text(), Path(__file__).read_text()])


def test_solitaire_row_matches_repository_data():
    validate_solitaire(*load())


@pytest.mark.parametrize('change', ['three 350 t tensioners', '300 m length', '1,000'])
def test_solitaire_verified_row_rejects_unsupported_figure(change):
    text, data = load()
    changed = text.replace('1,050', change if change == '1,000' else '1,050 ' + change, 1)
    with pytest.raises(AssertionError):
        validate_solitaire(changed, data)


def test_status_table_bound_to_criteria_and_evidence():
    validate_status(*load())


@pytest.fixture
def offline_fallback():
    text, data = load()
    for key in KEYS:
        data['coverage'][key] = [e for e in data['coverage'][key]
                                 if e['pass'] == 'first' or e['item'] == 'Allseas']
    rows = table_rows(section(text, '5. Acceptance status'))
    for key, row, disposition in zip(KEYS, rows, FALLBACK):
        row[2] = '; '.join(e['item'] + ' ' + ''.join('[' + i + ']' for i in e['evidence'])
                           for e in data['coverage'][key])
        row[3] = disposition
    rows[1][4] = 'TechnipFMC missing.'
    header = '| Criterion | Required items | Covered items with evidence ids | Disposition | Note |\n|---|---|---|---|---|\n'
    text = text.split('## 5. Acceptance status')[0] + '## 5. Acceptance status\n\n' + header
    text += '\n'.join('| ' + ' | '.join(row) + ' |' for row in rows) + '\n'
    return text, data


def test_offline_fallback_keeps_open_criteria_open(offline_fallback):
    text, data = offline_fallback
    validate_status(text, data)
    assert derive(text, data) == FALLBACK
    assert 'TechnipFMC missing' in table_rows(section(text, '5. Acceptance status'))[1][4]


@pytest.mark.parametrize('mutation', [
    'false-met', 'criterion', 'order', 'unread', 'unknown', 'item', 'url', 'lines',
])
def test_validator_rejects_broken_evidence(offline_fallback, mutation):
    text, data = deepcopy(offline_fallback)
    if mutation == 'false-met':
        rows = section(text, '5. Acceptance status').splitlines()
        target = next(r for r in rows if r.startswith('| Contractor +'))
        text = text.replace(target, target.replace('| partly met |', '| met |'))
    elif mutation == 'criterion':
        text = text.replace(CRITERIA[0], 'Changed criterion')
    elif mutation == 'order':
        lines = text.splitlines()
        indices = [i for i, line in enumerate(lines) if any(line.startswith('| ' + c + ' |') for c in CRITERIA)]
        lines[indices[0]], lines[indices[1]] = lines[indices[1]], lines[indices[0]]
        text = '\n'.join(lines)
    elif mutation in {'unread', 'unknown', 'item'}:
        entry = data['coverage']['contractors'][-1]
        if mutation == 'unread':
            data['sources'][0]['status'] = 'located_not_read'
        elif mutation == 'unknown':
            entry['evidence'] = ['S99']
        else:
            entry['item'] = 'TechnipFMC'
    elif mutation == 'url':
        data['sources'][0]['url'] = 'https:///missing-host'
    with pytest.raises(AssertionError):
        validate_document(text, data, ['line\n' * 401] if mutation == 'lines' else [text])
        validate_status(text, data)
