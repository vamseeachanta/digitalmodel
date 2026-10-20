"""Public forecast artifact disclosure and integrity contract."""
import hashlib
import json
from html.parser import HTMLParser
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PAGE = ROOT / 'docs/api/installation/installation-forecast.html'
LABEL = 'SIMULATED DEMO, not connected offshore'
CRITERIA = ('Acceptance criteria come from a workbook whose applicability is '
            'unresolved; alert levels are provisional.')


class Resources(HTMLParser):
    def handle_starttag(self, tag, attrs):
        if tag in {'script', 'style', 'link', 'img', 'iframe', 'object', 'embed'}:
            assert not {'src', 'href', 'data'}.intersection(dict(attrs))


def test_disclosures_and_self_containment():
    text = PAGE.read_text(encoding='utf-8')
    assert f'<title>{LABEL}</title>' in text
    assert f'<h1>{LABEL}</h1>' in text
    assert CRITERIA in text
    Resources().feed(text)
    assert len(PAGE.read_bytes()) < 5_000_000
    assert 'window_probability' in text
    assert 'Engineering acceptance: NOT EVALUATED' in text


def test_manifest_and_navigation():
    manifest = json.loads(PAGE.with_suffix('.manifest.json').read_text(encoding='utf-8'))
    assert manifest['sha256'] == hashlib.sha256(PAGE.read_bytes()).hexdigest()
    assert manifest['size_bytes'] == PAGE.stat().st_size
    assert manifest['sanitization']['findings'] == 0
    assert manifest['sanitization']['terms_checked'] > 0
    nav = (ROOT / 'docs/capability-map/capabilities-sections.yml').read_text(encoding='utf-8')
    assert '../installation/installation-forecast.html' in nav


def test_no_source_paths_or_internal_decisions():
    for path in (PAGE, PAGE.with_suffix('.manifest.json')):
        text = path.read_text(encoding='utf-8').casefold()
        for term in ('data/structure-', 'data\\\\structure-', 'owner decision',
                     'monitoring-config.json', 'payload.json', '.xlsm'):
            assert term not in text


def test_all_private_scan_terms_absent():
    manifest_path = PAGE.with_suffix('.manifest.json')
    manifest = json.loads(manifest_path.read_text(encoding='utf-8'))
    fingerprints = manifest['sanitization']['substring_fingerprints']
    assert len(fingerprints) == manifest['sanitization']['terms_checked']
    assert fingerprints
    for path in (PAGE, manifest_path):
        text = path.read_text(encoding='utf-8').casefold()
        for item in fingerprints:
            size = item['length']
            assert size > 0
            assert all(hashlib.sha256(text[i:i + size].encode('utf-8')).hexdigest()
                       != item['sha256'] for i in range(len(text) - size + 1))


if __name__ == '__main__':
    test_disclosures_and_self_containment()
    test_manifest_and_navigation()
    test_no_source_paths_or_internal_decisions()
    test_all_private_scan_terms_absent()
    print('4 artifact checks passed')
