# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Public forecast artifact disclosure and integrity contract."""
import hashlib
import json
import os
import re
import subprocess
import sys
from copy import deepcopy
from decimal import Decimal
from html.parser import HTMLParser
from pathlib import Path, PurePath, PurePosixPath, PureWindowsPath

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
    assert basis['status'] == 'provisional; no basis recorded'
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
    assert basis['status'] in text
    assert 'assumed, based on previous experience' not in text
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
        assert 'original derivation is unverified' in source
        assert (f'<td>{limit:.3f}</td><td>{limit * 9.81:.3f}</td>'
                f'<td>{source}</td>') in text


def test_revision_five_preserves_published_history_and_appends_one_row():
    text = PAGE.read_text(encoding='utf-8')
    assert builder.NUMERICAL_BASIS['revision'] == 5
    table = re.search(r'<h2>Revision history</h2>.*?<tbody>(.*?)</tbody>', text, re.S).group(1)
    rows = re.findall(r'<tr>.*?</tr>', table, re.S)
    published_rows = [
        '<tr><td>0</td><td>2026-10-20</td><td>Selected simulated forecast traces and provisional alerts are published for demonstration.</td></tr>',
        '<tr><td>1</td><td>2026-10-20</td><td>Numerical assumptions and a worked example are added to make the arithmetic inspectable.</td></tr>',
        '<tr><td>2</td><td>2026-10-20</td><td>Limit sources, forecast settings and empirical window counts are clarified without changing the retained series.</td></tr>',
        '<tr><td>3</td><td>2026-10-21</td><td>Unsupported basis claims are removed, the conversion source is cited, and calibration arithmetic and alert explanations are derived from their inputs.</td></tr>',
        '<tr><td>4</td><td>2026-11-06</td><td>Published revision history is preserved; input and written-page integrity checks remain active under optimized Python.</td></tr>',
    ]
    assert rows[:5] == published_rows
    assert len(rows) == len(published_rows) + 1
    assert rows[5].startswith('<tr><td>5</td><td>2026-11-06</td>')


@pytest.mark.parametrize('optimized', [False, True], ids=['normal', 'optimized'])
@pytest.mark.parametrize(('fault', 'message'), [
    ('history_end', 'History must end at NOW'),
    ('forecast_start', 'Forecast must start after NOW'),
    ('history_length', 'Series times and values must have equal lengths'),
    ('forecast_length', 'Series times and values must have equal lengths'),
    ('empty_history', 'Series must contain times and values'),
    ('empty_forecast', 'Series must contain times and values'),
    ('alert_cutoff', 'Alert result differs from the numerical basis'),
    ('horizon', 'Forecast horizon differs from the exceedance horizon'),
])
def test_input_integrity_checks_survive_optimization(tmp_path, fault, message, optimized):
    scenario = embedded_scenario(PAGE.read_text(encoding='utf-8'))
    frame = scenario['frames'][0]
    channel = frame['channels'][0]
    if fault == 'history_end':
        channel['history']['times'][-1] -= 0.5
    elif fault == 'forecast_start':
        channel['forecast']['times'][0] = frame['now_s']
    elif fault in {'history_length', 'forecast_length'}:
        channel[fault.removesuffix('_length')]['values'].pop()
    elif fault in {'empty_history', 'empty_forecast'}:
        channel[fault.removeprefix('empty_')] = {'times': [], 'values': []}
    else:
        # Use a channel with a load limit for cutoff and horizon validation.
        channel = frame['channels'][1]
        channel['exceedance'] = deepcopy(channel['exceedance'])
        if fault == 'alert_cutoff':
            channel['exceedance']['alert_probability'] = 0.5
        else:
            channel['exceedance']['horizon_s'] += 1
    config = {'scenarios': [scenario], 'exceedance': {'alert_probability': 0.2}}
    request = tmp_path / 'request.json'
    request.write_text(json.dumps({'payload': {'demo': {'scenarios': [scenario]}},
                                  'config': config}), encoding='utf-8')
    command = [sys.executable, *(['-O'] if optimized else []), '-c', '''
import json
import sys
from scripts.capabilities import build_installation_forecast_page as builder
request = json.loads(open(sys.argv[1], encoding='utf-8').read())
try:
    builder.render(request['payload'], request['config'])
except ValueError as error:
    if str(error) != sys.argv[2]:
        raise
else:
    raise RuntimeError('Malformed forecast input was accepted')
''', str(request), message]
    result = subprocess.run(command, cwd=ROOT, capture_output=True, text=True)
    assert result.returncode == 0, result.stderr


@pytest.mark.parametrize('target', ['page', 'manifest'])
@pytest.mark.parametrize('optimized', [False, True], ids=['normal', 'optimized'])
def test_written_page_integrity_check_survives_optimization(tmp_path, optimized, target):
    scenario = embedded_scenario(PAGE.read_text(encoding='utf-8'))
    config = {'scenarios': [scenario], 'exceedance': {'alert_probability': 0.2}}
    config_raw = json.dumps(config).encode('utf-8')
    payload = {'demo': {'scenarios': [scenario]},
               'provenance': {'demo_config': {'sha256': hashlib.sha256(config_raw).hexdigest()}}}
    for name, raw in (('payload', json.dumps(payload).encode('utf-8')), ('config', config_raw),
                      ('terms', b'["reserved-example-term"]')):
        (tmp_path / name).write_bytes(raw)
    command = [sys.executable, *(['-O'] if optimized else []), '-c', '''
import sys
from pathlib import Path
from unittest.mock import patch
from scripts.capabilities import build_installation_forecast_page as builder
root = Path(sys.argv[1])
output = root / 'forecast.html'
manifest = output.with_suffix('.manifest.json')
output.write_bytes(b'previous page')
manifest.write_bytes(b'previous manifest')
target = output if sys.argv[2] == 'page' else manifest
write_bytes = Path.write_bytes

def corrupt_page(path, data):
    return write_bytes(path, data + b'corrupted' if path.name == target.name else data)

with patch.object(Path, 'write_bytes', corrupt_page):
    try:
        builder.build(root / 'payload', root / 'config', output, root / 'terms')
    except ValueError as error:
        expected = ('Written page differs from the rendered bytes' if sys.argv[2] == 'page'
                    else 'Written manifest differs from the serialized bytes')
        if str(error) != expected:
            raise
    else:
        raise RuntimeError('Corrupted written output was accepted')
if output.read_bytes() != b'previous page' or manifest.read_bytes() != b'previous manifest':
    raise RuntimeError('Failed verification replaced published outputs')
''', str(tmp_path), target]
    result = subprocess.run(command, cwd=ROOT, capture_output=True, text=True)
    assert result.returncode == 0, result.stderr


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


@pytest.mark.parametrize(('now', 'peak', 'margin'), [
    (420, '108.897', '64.740'), (480, '97.641', '75.996'),
])
def test_alert_explanation_compares_peak_with_limit(now, peak, margin):
    text = render_embedded_scenario()
    assert (f'At NOW {now} s the plotted peak, {peak} kN, is '
            f'{margin} kN below the 173.637 kN limit; '
            'the alert comes from the back-test error spread.') in text


def test_alert_explanation_is_absent_when_alert_is_false():
    scenario = embedded_scenario(PAGE.read_text(encoding='utf-8'))
    alert = scenario['frames'][3]['channels'][1]['exceedance']
    alert.update(window_probability=0, alert=False)
    config = {'scenarios': [scenario], 'exceedance': {'alert_probability': 0.2}}
    text = builder.render({'demo': {'scenarios': [scenario]}}, config)
    assert 'At NOW 420 s the plotted peak' not in text
    assert 'At NOW 480 s the plotted peak' in text


@pytest.mark.parametrize('excess', [0, 1])
def test_alert_explanation_allows_peak_at_or_above_limit(excess):
    scenario = embedded_scenario(PAGE.read_text(encoding='utf-8'))
    channel = scenario['frames'][4]['channels'][1]
    channel['forecast']['values'][0] = channel['assumed_limit'] + excess
    config = {'scenarios': [scenario], 'exceedance': {'alert_probability': 0.2}}
    text = builder.render({'demo': {'scenarios': [scenario]}}, config)
    assert 'At NOW 480 s the plotted peak' not in text
    assert 'alert: true' in text
    assert embedded_scenario(text)['frames'][4]['channels'][1] == channel


def test_alert_explanation_covers_other_load_channels():
    scenario = embedded_scenario(PAGE.read_text(encoding='utf-8'))
    channel = scenario['frames'][4]['channels'][2]
    channel['exceedance'].update(window_probability=23 / 61, alert=True)
    config = {'scenarios': [scenario], 'exceedance': {'alert_probability': 0.2}}
    text = builder.render({'demo': {'scenarios': [scenario]}}, config)
    assert text.count('At NOW 480 s the plotted peak') == 2
    assert f'below the {channel["assumed_limit"]:.3f} kN limit' in text


def test_conversion_is_disclosed_as_rounded_demonstration_convention():
    text = render_embedded_scenario()
    assert 'rounded demonstration convention' in text
    assert '9.806 65 kN/Te' in text
    assert '3rd CGPM (1901), Declaration 2' in text
    assert 'conventional value of standard gravity' in text
    # Standard gravity gives the exact tonne-force conversion; 9.81 is retained.
    relative_increase = (Decimal('9.81') / Decimal('9.80665') - 1) * 100
    assert 0 < relative_increase < Decimal('0.1')
    assert 'less than 0.1 %' in text


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


def checkout_prefix(path: PurePath) -> str:
    """Build the forward-slash prefix required by Git checkout-index."""
    return path.as_posix() + '/'


@pytest.mark.parametrize(('path', 'expected'), [
    (PurePosixPath('checkout'), 'checkout/'),
    (PurePosixPath('checkout/output'), 'checkout/output/'),
    (PureWindowsPath(r'C:\checkout'), 'C:/checkout/'),
    (PureWindowsPath(r'checkout\output'), 'checkout/output/'),
])
def test_checkout_prefix_uses_posix_separators(path, expected):
    assert checkout_prefix(path) == expected


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
    git('checkout-index', '--all', '--prefix=' + checkout_prefix(checkout))
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


@pytest.mark.parametrize('time_origin', [0, 10])
def test_calibration_text_is_derived_when_settings_change(monkeypatch, time_origin):
    settings = deepcopy(builder.NUMERICAL_BASIS['forecast_settings'])
    settings.update(order=20, ridge=0.002, horizon_s=60, sample_interval_s=1,
                    stride_samples=5, min_calibration_windows=3, time_origin_s=time_origin)
    monkeypatch.setitem(builder.NUMERICAL_BASIS, 'forecast_settings', settings)
    frames = [{'now_s': 240}, {'now_s': 300}]
    text = builder.numerical_basis_html(frames)
    assert 'fixed order 20, ridge 0.002' in text
    assert 'sample 59' in text
    assert '60 s horizon contains 60 samples' in text
    assert '119 = 59 + 60' in text
    assert 'forecast horizon: 60 s' in text
    assert 'calibration stride: 5 samples' in text
    assert 'minimum of 3 complete calibration windows' in text
    clock = 'NOW' if time_origin == 0 else '(NOW - 10)'
    assert f'N = floor(({clock} / 1 - 119) / 5) + 1' in text
    counts = '25 and 37' if time_origin == 0 else '23 and 35'
    assert f'At NOW 240 and 300 s, N is {counts} respectively.' in text
    assert 'fixed order 40' not in text


@pytest.mark.parametrize('alert', [False, True])
def test_exceedance_without_load_limit_has_explicit_error(alert):
    scenario = embedded_scenario(PAGE.read_text(encoding='utf-8'))
    frame = scenario['frames'][0]
    channel = frame['channels'][0]
    channel['exceedance'] = deepcopy(frame['channels'][1]['exceedance'])
    channel['exceedance'].update(alert=alert, window_probability=int(alert))
    config = {'scenarios': [scenario], 'exceedance': {'alert_probability': 0.2}}
    with pytest.raises(ValueError, match='Exceedance requires a load limit'):
        builder.render({'demo': {'scenarios': [scenario]}}, config)


if __name__ == '__main__':
    raise SystemExit(pytest.main([__file__]))
