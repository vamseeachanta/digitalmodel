"""Response coverage and partial-result regressions for the second review."""
import json
import subprocess
from pathlib import Path

import numpy as np
import pytest
import yaml

from digitalmodel.marine_ops.operation_envelope import (
    DEFAULT_TP_RANGE_S, operation_envelope, plot_envelopes,
)
from tests.marine_ops.test_envelope_validation import full_rao


def band_rao(*, decay=False):
    rao = full_rao()
    frequencies = np.array([.1, .2, .4, .8, 1.2, 1.8, 2.5])
    for name in ('surge', 'sway', 'heave', 'roll', 'pitch', 'yaw'):
        component = getattr(rao, name)
        component.frequencies.values = frequencies.copy()
        gain = 3 * np.exp(-(frequencies / 1.2)**4) if decay else np.ones(7)
        component.magnitude = np.repeat(gain[:, None], 5, axis=1)
        if name != 'heave':
            component.magnitude[:] = 0.
        component.phase = np.zeros((7, 5))
    return rao


def test_realistic_decaying_band_on_default_periods():
    result = operation_envelope(band_rao(decay=True), 'transit')
    assert [p.tp_s for p in result.points] == DEFAULT_TP_RANGE_S
    assert all(p.status == 'evaluated' and p.evaluated for p in result.points)
    assert all(0 <= p.extrapolated_energy_fraction <= .01 for p in result.points)


def test_bad_period_does_not_abort_good_periods():
    result = operation_envelope(band_rao(), 'transit')
    first, last = result.points[0], result.points[-1]
    assert first.status == 'insufficient_coverage'
    assert first.evaluated is False
    assert first.hs_limit_m is None
    assert first.hs_forecast_limit_m is None
    assert first.governing_heading_deg is None
    assert first.extrapolated_energy_fraction > .01
    assert last.status == 'evaluated' and last.evaluated
    assert last.hs_limit_m == pytest.approx(5.4)
    assert result.hs_limit_at(4.) is None
    assert result.hs_limit_at(20.) == pytest.approx(5.4)
    data = result.to_dict()
    assert data['points'][0]['hs_forecast_limit_m'] is None
    json.dumps(data, allow_nan=False)
    with pytest.raises(ValueError, match='unevaluated'):
        result.require_evaluated()


@pytest.mark.parametrize('outside', ['low', 'high', 'both'])
def test_response_fraction_matches_independent_held_edge_integral(outside):
    from digitalmodel.marine_ops.envelope_numerics import extrapolated_fraction
    component = full_rao().heave
    component.frequencies.values = np.array([1., 2.])
    omega = np.array([.5, 1., 1.5, 2., 2.5])
    density = np.ones(5)
    gain = {'low': [2., 2., 1., 0., 0.],
            'high': [0., 0., 1., 2., 2.],
            'both': [2., 2., 1., 2., 2.]}[outside]
    # Covered response = 1.5 for one tail, 2.5 for both; each tail = 2.
    expected = 4 / 6.5 if outside == 'both' else 2 / 3.5
    assert extrapolated_fraction(component, omega, density, np.array(gain)) == pytest.approx(expected)


def test_coverage_uses_boundary_cells():
    from digitalmodel.marine_ops.envelope_numerics import extrapolated_fraction
    component = full_rao().heave
    component.frequencies.values = np.array([.75, 2.25])
    omega = np.array([.5, 1., 1.5, 2., 2.5])
    assert extrapolated_fraction(component, omega, np.ones(5), np.ones(5)) == pytest.approx(.25)


def test_boundary_cells_use_exact_held_edge_magnitude():
    from digitalmodel.marine_ops.envelope_numerics import extrapolated_fraction
    component = full_rao().heave
    component.frequencies.values = np.array([.75, 2.25])
    omega = np.array([.5, 1., 1.5, 2., 2.5])
    gain = np.interp(omega, [.75, 2.25], [0., 1.])
    # Covered trapezoids sum to 25/48; held upper tail = 1/4, lower = 0.
    assert extrapolated_fraction(component, omega, np.ones(5), gain) == pytest.approx(12 / 37)


@pytest.mark.parametrize('zero', [False, True])
def test_no_band_overlap_has_explicit_response_status(zero):
    rao = band_rao()
    rao.heave.frequencies.values += 200.
    if zero:
        rao.heave.magnitude[:] = 0.
    point = operation_envelope(rao, 'transit', tp_range_s=[4.]).points[0]
    assert point.status == ('capped' if zero else 'insufficient_coverage')
    assert point.evaluated is zero
    assert point.hs_limit_m == (20. if zero else None)


def test_any_required_heading_can_invalidate_coverage():
    rao = band_rao(decay=True)
    rao.heave.magnitude[:, -1] = 1.
    point = operation_envelope(rao, 'transit', tp_range_s=[4.]).points[0]
    assert point.status == 'insufficient_coverage'
    assert not point.evaluated and point.hs_limit_m is None
    selected = operation_envelope(rao, 'transit', tp_range_s=[4.], headings=[0.]).points[0]
    assert selected.status == 'evaluated'


def test_plot_leaves_gap_at_unavailable_period(monkeypatch, tmp_path):
    from matplotlib.axes import Axes
    result = operation_envelope(band_rao(), 'transit', tp_range_s=[4., 12., 20.])
    calls = []
    original = Axes.plot
    def record(self, x, y, *args, **kwargs):
        calls.append((x, y))
        return original(self, x, y, *args, **kwargs)
    monkeypatch.setattr(Axes, 'plot', record)
    assert plot_envelopes({'transit': result}, tmp_path / 'partial.png').is_file()
    assert calls[0][0] == [4., 12., 20.]
    assert np.isnan(calls[0][1][0])
    assert calls[0][1][-1] == pytest.approx(5.4)


def test_pamphlet_carries_partial_and_capped_status(monkeypatch):
    from digitalmodel.installation.installation_pamphlet import calculations
    monkeypatch.setattr(calculations, '_build_rao_set', lambda *a: (band_rao(), False))
    env = calculations.compute_envelopes('synthetic', ['transit'], [4., 20.])['transit']
    assert env['hs_by_tp'] == {'4.0': None, '20.0': 5.4}
    assert env['status_by_tp'] == {'4.0': 'insufficient_coverage', '20.0': 'evaluated'}
    assert env['criterion_type'] == 'forecast_limit'
    assert env['alpha_source']['status'] == 'unverified_screening'
    zero = band_rao()
    zero.heave.magnitude[:] = 0.
    monkeypatch.setattr(calculations, '_build_rao_set', lambda *a: (zero, False))
    capped = calculations.compute_envelopes('synthetic', ['transit'], [4.])['transit']
    assert capped['hs_by_tp']['4.0'] == 20.
    assert capped['status_by_tp']['4.0'] == 'capped'
    json.dumps(capped, allow_nan=False)


def test_pamphlet_renders_unavailable_and_cap(monkeypatch):
    from digitalmodel.installation.installation_pamphlet import calculations, render
    from tests.installation.test_installation_pamphlet import _assemble
    monkeypatch.setattr(calculations, '_build_rao_set', lambda *a: (band_rao(), False))
    info = {'name': 'synthetic', 'type': 'synthetic', 'dp_class': '2',
            'crane_radii_m': [10., 40.], 'crane_caps_te': [100., 50.],
            'max_hook_te': 100., 'deck_area_m2': 100., 'deck_strength_t_per_m2': 5.}
    result = _assemble(info, [], 'barge')
    for env in result['envelopes'].values():
        first = str(float(result['tp_grid_s'][0]))
        env['hs_by_tp'][first] = None
        env['status_by_tp'][first] = 'insufficient_coverage'
        last = str(float(result['tp_grid_s'][-1]))
        env['hs_by_tp'][last] = 20.
        env['status_by_tp'][last] = 'capped'
    html = render.render_pamphlet_html(result, 'synthetic')
    assert 'insufficient coverage' in html
    assert '20.00 (capped display bound)' in html
    assert '<td>nan' not in html.lower()
    assert 'cy="nan"' not in html.lower()


def test_browser_lookup_cannot_interpolate_gaps_or_caps():
    from digitalmodel.installation.installation_pamphlet.render import LIVE_WEATHER_MONITOR_WIDGET
    function = LIVE_WEATHER_MONITOR_WIDGET.split('function hsLimitAt', 1)[1].split('const SAMPLE', 1)[0]
    data = {'transit': {'hs_by_tp': {'4.0': None, '12.0': 5.4, '20.0': 20., '24.0': 8.},
                        'status_by_tp': {'4.0': 'insufficient_coverage', '12.0': 'evaluated',
                                         '20.0': 'capped', '24.0': 'evaluated'}}}
    script = ('const ENV=' + json.dumps(data) + '; const TPGRID=[4,12,20,24];\n'
              + 'function hsLimitAt' + function
              + "console.log(JSON.stringify([4,8,12,16,20,24].map(t=>hsLimitAt('transit',t))));")
    output = subprocess.check_output(['node', '-e', script], text=True)
    assert json.loads(output) == [None, None, 5.4, None, None, 8.]


def test_incomplete_damping_band_never_publishes_one_sided_bound(monkeypatch):
    from digitalmodel.installation.installation_pamphlet import calculations
    monkeypatch.setattr(calculations, '_build_rao_set', lambda *a: (band_rao(), False))
    band = calculations.compute_envelope_band('synthetic', ['transit'], [4., 20.], None, None)
    assert band['transit']['hs_by_tp_low'] == {'4.0': None, '20.0': 5.4}
    assert band['transit']['hs_by_tp_high'] == {'4.0': None, '20.0': 5.4}


@pytest.mark.parametrize('limits', [{'4.0': None}, {'20.0': 5.4}])
def test_combined_lowering_limit_preserves_missing_motion_limit(monkeypatch, limits):
    from digitalmodel.installation.installation_pamphlet import calculations
    monkeypatch.setattr(calculations, 'compute_splashzone_envelope',
                        lambda *a: {'hs_by_tp': {'4.0': 2.}})
    record = {'hydrodynamic_coefficients': {}, 'projected_areas': {},
              'mass_properties': {'displaced_volume_m3': 1.}}
    results = calculations.compute_splashzone_set(
        [{'catalog_record': record}], [4.], heavy_lift_env={'hs_by_tp': limits})
    assert results[0]['combined_hs_by_tp'] == {'4.0': None}


def test_hosted_gate_and_domain_map_include_all_envelope_tests():
    root = Path(__file__).resolve().parents[2]
    gate = yaml.safe_load((root / '.claude/quality-gates.yaml').read_text())
    # Gate entries live under the quality-gates mapping.
    gates = gate.get('gates', gate)
    entry = gates['tests-marine-ops-other']
    for path in ('tests/marine_ops/test_operation_envelope.py', 'tests/marine_ops/test_envelope_*.py'):
        assert path in entry['test_roots']
        assert path in entry['command']
        row = next(row for row in (root / 'tests/DOMAINS.md').read_text().splitlines()
                   if row.startswith('| marine-ops-other'))
        assert path in row
    # The detector treats file roots literally, so retain concrete routes too.
    from scripts.ci.detect_touched_domains import parse_domains, touched_domains
    domains = parse_domains(root / 'tests/DOMAINS.md')
    for path in (root / 'tests/marine_ops').glob('test_envelope_*.py'):
        selected = {d.name for d in touched_domains([path.relative_to(root).as_posix()], domains)}
        assert selected == {'marine-ops-other'}
