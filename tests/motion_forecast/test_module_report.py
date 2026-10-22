"""Rebuild and numerical contract for the motion reconstruction report."""
import importlib.util
import json
from pathlib import Path

import numpy as np

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / 'scripts/build_motionforecast_report.py'


def builder():
    spec = importlib.util.spec_from_file_location('motion_report', SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_example_matches_public_module_and_closed_form():
    from digitalmodel.motion_forecast import (
        AnalyticRAO, WaveComponent, WaveForecast, reconstruct_motion,
    )
    data = builder().example()
    p = data['inputs']
    wave = WaveForecast([WaveComponent(p['omega'], p['amplitude'], p['phase'])],
                        horizon=p['horizon'])
    motion = reconstruct_motion(wave, AnalyticRAO({
        'heave': lambda w, b: p['gain'] * np.exp(1j * p['response_phase']),
    }), dt=p['dt'])
    assert data['time'] == motion.t.tolist()
    assert data['heave'] == motion.dof['heave'].tolist()
    assert data['peak'] == motion.max_abs('heave')
    assert data['significant'] == motion.significant('heave')
    expected = p['amplitude'] * p['gain'] * np.cos(
        p['omega'] * motion.t + p['phase'] + p['response_phase'])
    np.testing.assert_allclose(data['heave'], expected, rtol=1e-12, atol=1e-12)


def test_rebuild_is_exact_and_contains_numerical_payload(tmp_path):
    module = builder()
    module.build(tmp_path)
    target = ROOT / 'docs/reports/modules/motionforecast'
    for name in ('index.html', 'changes.json'):
        assert (tmp_path / name).read_bytes() == (target / name).read_bytes()
    page = (tmp_path / 'index.html').read_text()
    payload = page.split('<script id="example-data" type="application/json">')[1].split('</script>')[0]
    assert json.loads(payload) == module.example()
    assert page.count('<section id="section-') == 10
    assert 'class="hs-doc"' in page
    assert '<script src=' not in page
    assert 'showDirectoryPicker' in page
    assert 'textContent' in page
    assert 'innerHTML' not in page
    for key in ('peak', 'significant'):
        assert f'data-result="{key}">{module.example()[key]:.3f}' in page
    assert 'Forecast interval boundary' in page


def test_rendered_input_values_and_source_links():
    import re
    from html import unescape
    page = (ROOT / 'docs/reports/modules/motionforecast/index.html').read_text()
    values = re.findall(r'<tr><td>[^<]+</td><td>([0-9.]+)</td><td>[^<]+</td><td>Regular-wave', page)
    assert values == [f'{value:.3f}' for value in builder().example()['inputs'].values()]
    for link in re.findall(r'href="(\.\./[^"#]+)"', page):
        assert (ROOT / 'docs/reports/modules/motionforecast' / unescape(link)).is_file()


def test_generator_is_classified_in_html_census():
    from scripts.generated_html_ownership import DISCOVERY_FALSE_POSITIVES
    assert 'scripts/build_motionforecast_report.py' in DISCOVERY_FALSE_POSITIVES
