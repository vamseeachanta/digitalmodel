# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Public forecast artifact disclosure and integrity contract."""
import hashlib
import json
import os
import re
import subprocess
from html.parser import HTMLParser
from pathlib import Path

import pytest

from scripts.capabilities import build_installation_forecast_page as builder
from scripts.capabilities.build_capabilities_inventory import registered_site_paths

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


def test_manifest_integrity():
    manifest = json.loads(PAGE.with_suffix('.manifest.json').read_text(encoding='utf-8'))
    assert manifest['sha256'] == hashlib.sha256(PAGE.read_bytes()).hexdigest()
    assert manifest['size_bytes'] == PAGE.stat().st_size
    assert manifest['sanitization']['findings'] == 0
    assert manifest['sanitization']['terms_checked'] > 0


def test_navigation_is_registered():
    assert str(Path('installation/installation-forecast.html')) in registered_site_paths(ROOT)


def test_commented_navigation_is_not_registered(tmp_path, monkeypatch):
    nav = tmp_path / 'docs/capability-map/capabilities-sections.yml'
    nav.parent.mkdir(parents=True)
    nav.write_text('sections: []\n# ../installation/installation-forecast.html\n', encoding='utf-8')
    monkeypatch.setitem(globals(), 'ROOT', tmp_path)
    with pytest.raises(AssertionError):
        test_navigation_is_registered()


def embedded_scenario(text):
    return json.loads(re.search(
        r'<script type="application/json" id="forecast-data">(.*?)</script>',
        text, re.S,
    ).group(1))


def render_embedded_scenario():
    scenario = embedded_scenario(PAGE.read_text(encoding='utf-8'))
    threshold = next(channel['exceedance']['alert_probability']
                     for frame in scenario['frames'] for channel in frame['channels']
                     if 'exceedance' in channel)
    config = {'scenarios': [scenario], 'exceedance': {'alert_probability': threshold}}
    return builder.render({'demo': {'scenarios': [scenario]}}, config)


def test_display_values_have_no_float_artifacts():
    text = re.sub(r'<script\b[^>]*>.*?</script>', '', PAGE.read_text(encoding='utf-8'), flags=re.S)
    assert not re.search(r'\d+\.\d{4,}', text)


def test_render_formats_values_without_rounding_embedded_data():
    text = render_embedded_scenario()
    assert 'Provisional load limit: 106.831 kN.' in text
    assert 'Provisional exceedance probability: 0.027; alert threshold: 0.200;' in text
    assert embedded_scenario(text) == embedded_scenario(PAGE.read_text(encoding='utf-8'))


def test_embedded_data_reproduces_page():
    assert render_embedded_scenario().encode('utf-8') == PAGE.read_bytes()


def test_autocrlf_checkout_preserves_manifest_integrity(tmp_path):
    env = {key: value for key, value in os.environ.items() if not key.startswith('GIT_')}
    repo, checkout = tmp_path / 'repo', tmp_path / 'checkout'
    repo.mkdir()
    paths = ('.gitattributes', PAGE.relative_to(ROOT), PAGE.with_suffix('.manifest.json').relative_to(ROOT))
    for path in paths:
        target = repo / path
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_bytes((ROOT / path).read_bytes())

    def git(*args):
        return subprocess.run(['git', '-c', 'core.autocrlf=true', '-c', 'core.safecrlf=false', *args],
                              cwd=repo, env=env, check=True, capture_output=True)

    git('init', '--quiet')
    git('add', '--', *map(str, paths))
    git('checkout-index', '--all', '--prefix=' + str(checkout) + os.sep)
    page = checkout / PAGE.relative_to(ROOT)
    manifest_bytes = page.with_suffix('.manifest.json').read_bytes()
    manifest = json.loads(manifest_bytes)
    assert manifest['sha256'] == hashlib.sha256(page.read_bytes()).hexdigest()
    assert manifest['size_bytes'] == page.stat().st_size
    assert b'\r' not in manifest_bytes


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
    raise SystemExit(pytest.main([__file__]))
