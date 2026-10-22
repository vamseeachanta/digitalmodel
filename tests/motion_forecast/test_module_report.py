"""Rebuild and numerical contract for the motion reconstruction report."""
import importlib.util
import json
import math
import os
from pathlib import Path
import statistics
import subprocess
import sys

import numpy as np
import pytest

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
    assert data['time'] == [round(value, 12) for value in motion.t.tolist()]
    assert data['heave'] == [round(value, 12) for value in motion.dof['heave'].tolist()]
    assert data['peak'] == round(motion.max_abs('heave'), 12)
    assert data['significant'] == round(motion.significant('heave'), 12)
    expected = p['amplitude'] * p['gain'] * np.cos(
        p['omega'] * motion.t + p['phase'] + p['response_phase'])
    np.testing.assert_allclose(data['heave'], expected, rtol=1e-12, atol=1e-12)
    closed_form = [1.3 * 0.8 * math.cos(0.7 * i / 10 + 0.9) for i in range(601)]
    independent_significant = 2 * statistics.pstdev(closed_form)
    assert independent_significant == pytest.approx(1.452253, abs=5e-7)
    assert data['significant'] == pytest.approx(independent_significant, abs=1e-12)
    assert data['peak'] <= p['amplitude'] * p['gain']


def test_payload_is_stable_under_last_bit_roundoff(monkeypatch):
    module = builder()
    expected = module.example()
    reconstruct = module.reconstruct_motion

    def reconstruct_with_roundoff(*args, **kwargs):
        motion = reconstruct(*args, **kwargs)
        motion.t = np.nextafter(motion.t, np.inf)
        motion.dof['heave'] = np.nextafter(motion.dof['heave'], np.inf)
        return motion

    monkeypatch.setattr(module, 'reconstruct_motion', reconstruct_with_roundoff)
    assert module.example() == expected


def test_rebuild_uses_utf8_under_ascii_locale(tmp_path):
    environment = os.environ.copy()
    environment.update(PYTHONUTF8='0', PYTHONCOERCECLOCALE='0', LC_ALL='C')
    result = subprocess.run(
        [sys.executable, '-X', 'warn_default_encoding', '-W', 'error::EncodingWarning',
         str(SCRIPT), str(tmp_path)],
        cwd=ROOT, env=environment, capture_output=True,
    )
    assert result.returncode == 0, result.stderr.decode('utf-8', errors='replace')
    assert (tmp_path / 'index.html').read_bytes().decode('utf-8').startswith('<!DOCTYPE html>')


def test_rebuild_writes_lf_with_windows_default(monkeypatch, tmp_path):
    write_text = Path.write_text

    def write_with_windows_default(path, data, *args, **kwargs):
        kwargs.setdefault('newline', '\r\n')
        return write_text(path, data, *args, **kwargs)

    monkeypatch.setattr(Path, 'write_text', write_with_windows_default)
    builder().build(tmp_path)
    for name in ('index.html', 'changes.json'):
        content = (tmp_path / name).read_bytes()
        assert b'\r' not in content
        assert content.endswith(b'\n')


def test_rebuild_is_exact_and_contains_numerical_payload(tmp_path):
    module = builder()
    module.build(tmp_path)
    target = ROOT / 'docs/reports/modules/motionforecast'
    for name in ('index.html', 'changes.json'):
        assert (tmp_path / name).read_bytes() == (target / name).read_bytes()
    page = (tmp_path / 'index.html').read_text(encoding='utf-8')
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
    page = (ROOT / 'docs/reports/modules/motionforecast/index.html').read_text(encoding='utf-8')
    values = re.findall(r'<tr><td>[^<]+</td><td>([0-9.]+)</td><td>[^<]+</td><td>Regular-wave', page)
    assert values == [f'{value:.3f}' for value in builder().example()['inputs'].values()]
    for link in re.findall(r'href="(\.\./[^"#]+)"', page):
        assert (ROOT / 'docs/reports/modules/motionforecast' / unescape(link)).is_file()


def test_generator_is_classified_in_html_census():
    from scripts.generated_html_ownership import DISCOVERY_FALSE_POSITIVES
    assert 'scripts/build_motionforecast_report.py' in DISCOVERY_FALSE_POSITIVES
