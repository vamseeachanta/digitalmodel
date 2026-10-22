"""Rebuild and independently check the spectral hydrodynamics report."""
import importlib.util
import json
import re
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.hydrodynamics.seakeeping import operability_analysis

ROOT = Path(__file__).resolve().parents[2]
REPORT = ROOT / 'docs/reports/modules/hydrodynamics'


def load_builder():
    spec = importlib.util.spec_from_file_location('hydrodynamics_report', REPORT / 'generate.py')
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def public_example():
    spec = importlib.util.spec_from_file_location(
        'public_seakeeping_example', ROOT / 'tests/hydrodynamics/test_seakeeping.py')
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    example = module.TestOperabilityAnalysis()
    frequencies, rao = example.unity_raos.__wrapped__(example)
    scatter = example.scatter_diagram.__wrapped__(example)
    return frequencies, rao, scatter


def test_rebuild_and_every_worked_value(tmp_path):
    builder = load_builder()
    builder.build(tmp_path)
    for name in ('index.html', 'changes.json'):
        assert (tmp_path / name).read_bytes() == (REPORT / name).read_bytes()
    page = (tmp_path / 'index.html').read_text()
    payload = json.loads(re.search(
        r'<script id="example-data" type="application/json">(.*?)</script>', page, re.S)[1])
    omega, rao, scatter = public_example()
    actual = operability_analysis(omega, rao, scatter, {'heave': 2.0})
    assert payload['result'] == actual
    assert payload['inputs']['scatter'] == scatter
    assert payload['inputs']['omega'] == omega.tolist()
    assert payload['inputs']['rao'] == rao.tolist()
    assert payload['inputs']['criterion'] == 2.0
    values = builder.display_values(payload)
    rendered = re.findall(r'data-value="([^"]+)">([^<]+)</', page)
    assert set(key for key, _ in rendered) == set(values)
    for key, text in rendered:
        assert text == builder.number(values[key], key), key
    assert payload['result']['operability_pct'] == pytest.approx(90.0)
    assert [r['operable'] for r in actual['sea_state_results']] == [True, True, True, False]
    # The plotted demands and limit are the same values as the calculation.
    for index, row in enumerate(actual['sea_state_results']):
        assert f'data-demand="{row["significant_amplitude"]}"' in page
    assert f'data-limit="{payload["inputs"]["criterion"]}"' in page


def test_structure_sources_and_offline_review():
    page = (REPORT / 'index.html').read_text()
    assert '<body class="hs-doc">' in page
    assert len(re.findall(r'<h2>', page)) == 10
    assert 'Draft for client review' in page
    assert 'Revision history' in page
    assert 'ASSUMED - to be confirmed' in page
    assert '<script src=' not in page and '<link rel="stylesheet"' not in page
    assert 'showSaveFilePicker' in page and 'comments.json' in page
    assert 'changes-panel' in page and 'localStorage' in page
    assert '<title>' in page and 'tabindex="0"' in page
    assert '<b>' not in page and '<strong>' not in page
    ids = set(re.findall(r'id="([^"]+)"', page))
    for target in re.findall(r'href="#([^"]+)"', page):
        assert target in ids
    assert np.isfinite(load_builder().calculate()['result']['operability_pct'])


def test_analytical_reference_and_record_links():
    from html.parser import HTMLParser
    from urllib.parse import unquote

    class Links(HTMLParser):
        def __init__(self):
            super().__init__()
            self.targets = []

        def handle_starttag(self, tag, attrs):
            if tag == 'a':
                self.targets.extend(v for k, v in attrs if k == 'href')

    data = load_builder().calculate()
    for row in data['result']['sea_state_results']:
        assert row['m0'] == pytest.approx(row['hs'] ** 2 / 16)
        assert row['significant_amplitude'] == pytest.approx(row['hs'] / 2)
    weights = data['inputs']['scatter']
    limit = data['inputs']['criterion']
    expected = 100 * sum(r['probability'] for r in weights if r['hs'] / 2 <= limit)
    expected /= sum(r['probability'] for r in weights)
    assert data['result']['operability_pct'] == pytest.approx(expected)
    parser = Links()
    parser.feed((REPORT / 'index.html').read_text())
    for target in parser.targets:
        if not target.startswith('#'):
            assert (REPORT / unquote(target)).is_file(), target


def test_review_javascript_syntax():
    import shutil
    import subprocess

    node = shutil.which('node')
    if node is None:
        pytest.skip('JavaScript syntax check requires Node')
    page = (REPORT / 'index.html').read_text()
    scripts = re.findall(r'<script>(.*?)</script>', page, re.S)
    assert len(scripts) == 1
    subprocess.run([node, '--check'], input=scripts[0], text=True, check=True,
                   capture_output=True)


def test_offline_comments_and_change_panel():
    """Execute review controls with a small DOM stand-in, without a browser."""
    import shutil
    import subprocess

    node = shutil.which('node')
    if node is None:
        pytest.skip('Review control check requires Node')
    page = (REPORT / 'index.html').read_text()
    script = re.search(r'<script>(.*?)</script>', page, re.S)[1]
    changes = re.search(r'<script id="changes-data"[^>]*>(.*?)</script>', page, re.S)[1]
    # Data must precede the script because the page executes inline scripts in order.
    assert page.index('id="changes-data"') < page.index('<script>')
    harness = r'''
const assert = require('node:assert/strict');
const element = () => ({textContent:'', value:'', children:[],
  append(child) { this.children.push(child); },
  replaceChildren() { this.children = []; }});
const ids = ['comment-status','comment-list','comment-section','comment-text',
  'add-comment','save-comments','import-comments','changes-text','changes-data'];
const elements = Object.fromEntries(ids.map(id => [id, element()]));
elements['changes-data'].textContent = CHANGES;
const document = {getElementById: id => elements[id] || null,
  createElement: element, querySelectorAll: () => [{textContent:'Executive Summary'}]};
const saved = new Map();
const localStorage = {getItem: key => saved.get(key), setItem: (key, value) => saved.set(key, value)};
let exported = '';
const window = {showSaveFilePicker: async () => ({createWritable: async () => ({
  write: async text => { exported = text; }, close: async () => {}})})};
SCRIPT
assert.match(elements['changes-text'].textContent, /spectral motion/);
elements['comment-section'].value = 'Executive Summary';
elements['comment-text'].value = '<img src=x onerror=alert(1)>';
elements['add-comment'].onclick();
assert.equal(elements['comment-list'].children.length, 1);
assert.match(elements['comment-list'].children[0].textContent, /<img/);
(async () => {
  await elements['save-comments'].onclick();
  assert.equal(JSON.parse(exported)[0].section, 'Executive Summary');
  await elements['import-comments'].onchange({target:{files:[{text:async () =>
    JSON.stringify([{section:'Limitations',text:'Imported comment'}])}]}});
  assert.equal(elements['comment-list'].children[0].textContent, 'Limitations: Imported comment');
  await elements['import-comments'].onchange({target:{files:[{text:async () => '{}'}]}});
  assert.match(elements['comment-status'].textContent, /not a comments array/);
})().catch(error => { console.error(error); process.exitCode = 1; });
'''
    harness = harness.replace('CHANGES', json.dumps(changes)).replace('SCRIPT', script)
    subprocess.run([node], input=harness, text=True, check=True, capture_output=True)
