# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Rebuild and independently check the RAO workflow module analysis."""

import importlib.util
import json
import math
from pathlib import Path
import re
import subprocess
import sys

import numpy as np
import pytest

ROOT = Path(__file__).resolve().parents[3]
BUILDER = ROOT / 'scripts/reporting/build_rao_analysis_report.py'
REPORT = ROOT / 'docs/reports/modules/rao_analysis'


@pytest.fixture
def builder():
    spec = importlib.util.spec_from_file_location('rao_report_builder', BUILDER)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def calculation_from_html(text):
    embedded = re.search(r'<script type="application/json" id="calculation">(.*?)</script>', text, re.S)
    assert embedded is not None
    return json.loads(embedded.group(1))


def assert_same_calculation(actual, expected):
    """Allow floating arithmetic differences across architectures.

    Relative 1e-12 and absolute 1e-14 are numerical reproduction tolerances,
    not engineering acceptance limits. Source identities and structure remain
    exact. The independent hand and Simpson checks supply the value evidence.
    """
    if isinstance(expected, dict):
        assert actual.keys() == expected.keys()
        for key in expected:
            if key in {'inputs', 'source_digests'}:
                assert actual[key] == expected[key], (
                    'Rebuild with PYTHONPATH=src python '
                    'scripts/reporting/build_rao_analysis_report.py'
                )
            else:
                assert_same_calculation(actual[key], expected[key])
    elif isinstance(expected, list):
        assert len(actual) == len(expected)
        for a, b in zip(actual, expected):
            assert_same_calculation(a, b)
    elif isinstance(expected, int):
        assert actual == expected
    elif isinstance(expected, float):
        assert actual == pytest.approx(expected, rel=1e-12, abs=1e-14)
    else:
        assert actual == expected


def test_rebuild_is_reproducible(tmp_path):
    subprocess.run([sys.executable, str(BUILDER), '--output', str(tmp_path)], check=True)
    actual = (tmp_path/'report.html').read_text(encoding='utf-8')
    expected = (REPORT/'report.html').read_text(encoding='utf-8')
    assert_same_calculation(calculation_from_html(actual), calculation_from_html(expected))
    # Numerical curve digests are checked against their own retained arrays;
    # source digests and all presentation content must still match exactly.
    def presentation(text):
        text = re.sub(r'(<script type="application/json" id="calculation">).*?(</script>)', r'\1\2', text, flags=re.S)
        return re.sub(r'data-(?:x-)?digest="[a-f0-9]+"', 'data-digest=""', text)

    assert presentation(actual) == presentation(expected)
    assert (tmp_path/'changes.json').read_bytes() == (REPORT/'changes.json').read_bytes()


def test_worked_values_independent_quadrature(builder):
    r"""Independent Simpson quadrature, with scalar math and no production calls.

    Source: Waves in Ocean Engineering (1991), pp107-108, spectrum reproduced
    by Orcina, Waves: Wave spectra and Data for JONSWAP and ISSC spectra.
    sigma below/above peak = 0.07/0.09 from that public data documentation.
    Synthetic inputs: Hs=4, Tp=5, gamma=3, peak RAO=2, width=0.3 rad/s.
    R=2 exp[-((w-wp)/0.3)^2] is an assumed heave-like shape, not a solver RAO.
    The normalization cancels unknown alpha: m0r=(16/16)*int(R²F)/int(F).
    Composite Simpson with 20000 intervals is compared with the builder's
    40000-interval trapezoid result at relative tolerance 1e-5, a numerical
    cross-quadrature allowance only. No operating acceptance is implied.
    """
    wp = 2 * math.pi / 5
    lo, hi, count = wp/100, 10*wp, 20000
    step = (hi-lo) / count
    total = response = 0.0
    for j in range(count+1):
        w = lo+j*step
        sigma = 0.07 if w <= wp else 0.09
        shape = w**-5 * math.exp(-(5/4)*(wp/w)**4) * 3**math.exp(-((w/wp-1)/sigma)**2/2)
        rao = 2*math.exp(-((w-wp)/0.3)**2)
        weight = 1 if j in (0, count) else (4 if j % 2 else 2)
        total += weight*shape
        response += weight*shape*rao**2
    expected_m0 = response/total
    result = builder.calculate()
    assert result['constant']['amplitude'] == 6  # Hand: R*a=3*2.
    assert result['peak']['m0'] == pytest.approx(expected_m0, rel=1e-5)
    assert result['peak']['significant_amplitude'] == pytest.approx(2*math.sqrt(expected_m0), rel=1e-5)
    assert result['peak']['rms'] == pytest.approx(math.sqrt(expected_m0), rel=1e-5)
    assert result['unit']['m0'] == pytest.approx(1, abs=1e-12)
    assert result['unit']['significant_amplitude'] == pytest.approx(2, abs=1e-12)


def test_report_binds_all_displayed_values(builder):
    data = builder.calculate()
    text = (REPORT / 'report.html').read_text(encoding='utf-8')
    bindings = re.findall(r'<span data-value="([\w.]+)">([^<]+)</span>', text)
    assert len(bindings) >= 15
    for key, visible in bindings:
        value = data
        for part in key.split('.'):
            value = value[part]
        assert visible == builder.format_value(value)
    retained = calculation_from_html(text)
    assert retained.keys() == data.keys()
    assert_same_calculation(retained, data)


def test_figures_are_bound_to_computed_arrays(builder):
    data = builder.calculate()
    text = (REPORT / 'report.html').read_text(encoding='utf-8')
    retained = calculation_from_html(text)
    assert_same_calculation(retained['curves'], data['curves'])
    for chart, names in [('regular', ['time', 'motion']), ('spectra', ['omega', 'wave', 'response']), ('rao', ['omega', 'rao'])]:
        assert f'id="figure-{chart}"' in text
        for name in names:
            assert builder.curve_digest(retained['curves'][name]) in text, (
                'Rebuild with PYTHONPATH=src python '
                'scripts/reporting/build_rao_analysis_report.py'
            )
    # Changing a synthetic peak input must alter the computed values and plot.
    modified = builder.calculate(peak_gain=1)
    assert modified['peak']['m0'] == pytest.approx(data['peak']['m0']/4)
    assert builder.curve_digest(modified['curves']['response']) != builder.curve_digest(data['curves']['response'])


def test_report_structure_sources_and_limits():
    text = (REPORT / 'report.html').read_text(encoding='utf-8')
    assert text.index('id="revision-history"') > text.index('id="summary"')
    assert text.index('id="revision-history"') < text.index('id="scope"')
    assert len(re.findall(r'<section id=', text)) == 10
    for term in ['NOT_EVALUATED', 'narrow-band', 'finite-grid', 'lag', 'rad/s', '1991', '#3222', '#3227', '#3233']:
        assert term in text
    assert not re.search(r'<(?:script|link|img)[^>]+(?:src|href)="https?://', text)
    for relative in re.findall(r'href="(\.\./[^"]+)"', text):
        assert (REPORT / relative).resolve().is_file()
    ids = re.findall(r'\bid="([^"]+)"', text)
    assert len(ids) == len(set(ids))
    json.loads((REPORT / 'changes.json').read_text(encoding='utf-8'))


def test_figure_point_count_matches_data(builder):
    """Each sampled plot point is generated from the attached arrays."""
    curves = builder.calculate()['curves']
    assert len(curves['omega']) == len(curves['rao']) == len(curves['wave']) == len(curves['response'])
    np.testing.assert_allclose(np.array(curves['response']), np.array(curves['rao'])**2*np.array(curves['wave']), rtol=1e-12, atol=0)


def test_regular_figure_includes_negative_response():
    """Hand: 6*cos(t) ranges from -6 to +6; both extrema must fit the SVG."""
    text = (REPORT/'report.html').read_text(encoding='utf-8')
    plot = re.search(r'<figure id="figure-regular">(.*?)</figure>', text, re.S).group(1)
    points = re.search(r'points="([^"]+)"', plot).group(1)
    y_values = [float(point.split(',')[1]) for point in points.split()]
    assert min(y_values) == 30
    assert max(y_values) == 270


def test_report_generator_has_reasoned_ownership():
    """Keep the report outside the API census with an explicit rationale."""
    from scripts import check_generated_html as checker

    relative = BUILDER.relative_to(ROOT).as_posix()
    assert relative in checker.discover_candidate_generators(ROOT)
    assert checker.DISCOVERY_FALSE_POSITIVES.get(relative, '').strip()


@pytest.mark.parametrize('filename', ['report.html', 'changes.json'])
def test_rebuilt_text_is_utf8_lf_without_trailing_whitespace(builder, tmp_path, filename):
    """Both generated files retain portable text and clean line endings."""
    builder.build(tmp_path)
    content = (tmp_path / filename).read_bytes()

    assert b'\r' not in content
    assert content.endswith(b'\n')
    assert all(line == line.rstrip() for line in content.decode('utf-8').splitlines())
