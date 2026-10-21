# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Public forecast artifact disclosure and integrity contract."""
import hashlib
import json
import os
import re
import subprocess
from copy import deepcopy
from decimal import Decimal
from html.parser import HTMLParser
from pathlib import Path

import pytest

from scripts.capabilities import build_installation_forecast_page as builder
from scripts.capabilities.build_capabilities_inventory import registered_site_paths

ROOT = Path(__file__).resolve().parents[2]
PAGE = ROOT / 'docs/api/installation/installation-forecast.html'
LABEL = 'SIMULATED DEMO, not connected offshore'
CRITERIA = ('Acceptance criteria are provisional demonstration assumptions; '
            'engineering applicability is unresolved.')


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
    assert manifest['numerical_basis'] == builder.NUMERICAL_BASIS


def test_numerical_basis_explains_published_limits_and_alerts():
    text = PAGE.read_text(encoding='utf-8')
    basis = json.loads(PAGE.with_suffix('.manifest.json').read_text())['numerical_basis']
    assert basis['status'] == 'assumed, based on previous experience'
    conversion = Decimal(str(basis['kN_per_Te']))
    assert conversion == Decimal('9.81')
    loads = basis['load_limits_Te']
    expected = {'Master-link load-path proxy: TopRigging End B': Decimal('173.637'),
                'Sling2 End B tension': Decimal('106.8309')}
    assert set(loads) == set(expected)
    assert {label: Decimal(str(value)) * conversion for label, value in loads.items()} == expected
    cutoff = Decimal(str(basis['alert_probability']))
    assert cutoff == Decimal(1) / 5
    for frame in embedded_scenario(text)['frames']:
        for channel in frame['channels']:
            if channel['assumed_limit'] is None:
                continue
            assert channel['assumed_limit'] == pytest.approx(float(expected[channel['label']]))
            alert = channel['exceedance']
            assert Decimal(str(alert['alert_probability'])) == cutoff
            assert alert['alert'] is (Decimal(str(alert['window_probability'])) >= cutoff)
    assert 'assumed, based on previous experience' in text
    assert 'Calibration error trajectories are not published' in text
    assert 'Revision history' in text


def test_independent_worked_probability_example():
    basis = json.loads(PAGE.with_suffix('.manifest.json').read_text())['numerical_basis']
    example = basis['worked_example']
    assert example['forecast_kN'] == [100, 110]
    # Hand-worked maxima: two equal the limit, three exceed it.
    maxima = [max(f + e for f, e in zip(example['forecast_kN'], errors, strict=True))
              for errors in example['error_trajectories_kN']]
    assert maxima == [110, 110, 111, 111, 112]
    assert example['limit_kN'] == 110
    assert sum(value > example['limit_kN'] for value in maxima) == 3
    assert example['window_probability'] == 3 / 5
    assert example['alert'] is True
    text = PAGE.read_text(encoding='utf-8')
    assert '3 / 5 = 60.0 %' in text
    assert '20.0 %' in text


@pytest.mark.parametrize('channel_index', [1, 2])
def test_render_rejects_load_limit_inconsistent_with_basis(channel_index):
    scenario = embedded_scenario(PAGE.read_text(encoding='utf-8'))
    scenario['frames'][0]['channels'][channel_index]['assumed_limit'] += 1
    config = {'scenarios': [scenario], 'exceedance': {'alert_probability': 0.2}}
    with pytest.raises(ValueError, match='numerical basis'):
        builder.render({'demo': {'scenarios': [scenario]}}, config)


def test_render_rejects_alert_cutoff_inconsistent_with_basis():
    scenario = embedded_scenario(PAGE.read_text(encoding='utf-8'))
    for frame in scenario['frames']:
        for channel in frame['channels']:
            if 'exceedance' in channel:
                channel['exceedance']['alert_probability'] = 0.5
    config = {'scenarios': [scenario], 'exceedance': {'alert_probability': 0.5}}
    with pytest.raises(ValueError, match='numerical basis'):
        builder.render({'demo': {'scenarios': [scenario]}}, config)


@pytest.mark.parametrize(('limit', 'probability', 'alert'), [(100, 0.0, False), (99, 1.0, True)])
def test_constant_history_has_hand_worked_exceedance(limit, probability, alert):
    from digitalmodel.workflows.installation_forecast import exceedance_forecast

    # All forecasts and all back-test errors are exactly 100 and 0 respectively.
    result = exceedance_forecast(list(range(15)), [100] * 15, 14, limit,
                                 horizon_s=2, order=1, stride=1, alert_probability=0.2)
    assert result['calibration_windows'] == 11
    assert result['window_probability'] == probability
    assert result['alert'] is alert


def test_builder_publishes_numerical_basis(tmp_path):
    scenario = embedded_scenario(PAGE.read_text(encoding='utf-8'))
    config = {'scenarios': [scenario], 'exceedance': {'alert_probability': 0.2}}
    config_raw = json.dumps(config).encode('utf-8')
    payload = {'demo': {'scenarios': [deepcopy(scenario)]},
               'provenance': {'demo_config': {'sha256': hashlib.sha256(config_raw).hexdigest()}}}
    for name, raw in (('payload', json.dumps(payload).encode('utf-8')), ('config', config_raw),
                      ('terms', b'["reserved-example-term"]')):
        (tmp_path / name).write_bytes(raw)
    output = tmp_path / 'forecast.html'
    builder.build(tmp_path / 'payload', tmp_path / 'config', output, tmp_path / 'terms')
    manifest = json.loads(output.with_suffix('.manifest.json').read_text())
    assert manifest['numerical_basis'] == builder.NUMERICAL_BASIS
    assert manifest['sha256'] == hashlib.sha256(output.read_bytes()).hexdigest()


def test_load_sources_have_one_consistent_declaration():
    text = render_embedded_scenario()
    assert 'workbook' not in text.casefold()
    assert 'original derivation is not established' not in text
    basis = builder.NUMERICAL_BASIS
    for label, limit in basis['load_limits_Te'].items():
        source = basis['load_limit_sources'][label]
        assert source == 'assumed, based on previous experience'
        assert (f'<td>{limit:.3f}</td><td>{limit * 9.81:.3f}</td>'
                f'<td>{source}</td>') in text


def test_forecast_settings_explain_calibration_counts():
    text = render_embedded_scenario()
    settings = builder.NUMERICAL_BASIS['forecast_settings']
    assert settings['order'] == 40
    assert settings['ridge'] == 0.001
    assert settings['sample_interval_s'] == 0.5
    assert settings['stride_samples'] == 10
    assert settings['min_calibration_windows'] == 10
    assert settings['horizon_s'] == 120
    assert 'N = floor((NOW / 0.5 - 359) / 10) + 1' in text
    assert 'minimum of 10 complete calibration windows' in text
    assert 'fixed order 40, ridge 0.001' in text
    for frame, count in zip(embedded_scenario(text)['frames'], [13, 25, 37, 49, 61], strict=True):
        for channel in frame['channels']:
            assert 'fixed order 40, ridge 0.001' in channel['method']
            if 'exceedance' in channel:
                assert channel['exceedance']['calibration_windows'] == count


def test_probability_displays_integer_window_counts():
    text = render_embedded_scenario()
    for numerator, denominator in [(0, 13), (3, 25), (1, 37), (14, 49), (23, 61)]:
        assert f'({numerator} of {denominator} calibration windows)' in text


@pytest.mark.parametrize(('field', 'value'), [
    ('calibration_windows', 14), ('window_probability', 0.1),
])
def test_render_rejects_inconsistent_calibration_counts(field, value):
    scenario = embedded_scenario(PAGE.read_text(encoding='utf-8'))
    scenario['frames'][0]['channels'][1]['exceedance'][field] = value
    config = {'scenarios': [scenario], 'exceedance': {'alert_probability': 0.2}}
    with pytest.raises(ValueError, match='calibration'):
        builder.render({'demo': {'scenarios': [scenario]}}, config)


def test_alert_explanation_compares_peak_with_limit():
    text = render_embedded_scenario()
    assert 'At NOW 420 s' in text
    assert '108.897 kN' in text
    assert '64.7 kN below the 173.637 kN limit' in text
    assert 'alert comes from the back-test error spread' in text


def test_manifest_bytes_follow_builder_order_with_disclosed_carry_over():
    raw = PAGE.with_suffix('.manifest.json').read_bytes()
    manifest = json.loads(raw)
    ordered = {key: manifest[key] for key in (
        'sha256', 'size_bytes', 'source_sha256', 'numerical_basis',
        'sanitization', 'reduction', 'provenance_carry_over',
    )}
    assert raw == (json.dumps(ordered, indent=2) + '\n').encode('utf-8')
    carry_over = manifest['provenance_carry_over']
    assert carry_over['fields'] == ['source_sha256', 'sanitization']
    assert 'not re-read' in carry_over['reason']
    assert raw == builder.manifest_bytes(PAGE.read_bytes(), manifest['source_sha256'],
                                         manifest['sanitization'], carry_over)


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
