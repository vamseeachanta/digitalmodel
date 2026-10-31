# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent hand calculations for the probability and margin wrapper.

Two-layer example: D=10 m, zero unit weights, H=2 m, upper su=80 kPa,
lower su=s in [10,30] kPa. Circular area A=25*pi. The open weak-layer
paper (2015), equation 6, with the inherited 6.34 reference gives
qpeak=min(507.2, 6.34*s+48), qtrough=6.34*s. Thus at P=174.8*A,
punch-through occurs exactly when s<=20; its uniform probability is 1/2.
No expected resistance is obtained from a production calculation.
"""

import copy
import csv
import hashlib
import importlib.util
import json
import math
import random
from pathlib import Path

import pytest

from digitalmodel.geotechnical.soil_sensitivity import (
    evaluate,
    monte_carlo,
    nominal_values,
    realize,
    tornado,
    validate_case,
    wilson_interval,
)


@pytest.fixture
def hand_case():
    return {
        'name': 'hand_clay', 'basis': 'assumed, based on previous experience',
        'input': {
            'profile': {'layers': [
                {'kind': 'clay', 'top_m': 0, 'bottom_m': 2,
                 'submerged_weight_kn_m3': 0, 'su_kpa': 80},
                {'kind': 'clay', 'top_m': 2, 'bottom_m': 20,
                 'submerged_weight_kn_m3': 0, 'su_kpa': 20}],
                'sand_method': 'load_spread', 'spread_ratio': 3},
            'spudcan': {'diameter_m': 10, 'tip_to_bearing_m': 0},
            'tip_interval_m': [0, 20], 'preload_kn': 174.8 * (25 * math.pi),
        },
        'parameters': {
            'layers.0.su_kpa': {'low': 60, 'high': 100, 'distribution': 'uniform'},
            'layers.1.su_kpa': {'low': 10, 'high': 30, 'distribution': 'uniform'},
            'top_thickness_m': {'low': 1, 'high': 3, 'distribution': 'uniform'},
        },
    }


def test_margin_and_boundary_from_hand_equation(hand_case):
    # At nominal s=20, P=qpeak*A: equality reaches the descending peak.
    values = nominal_values(hand_case)
    result = evaluate(hand_case, values)
    assert result['punch_through'] is True
    assert result['margin_kn'] == pytest.approx(0, abs=1e-9)
    assert result['loss_threshold_kn'] == pytest.approx(174.8 * 25 * math.pi)
    values['layers.1.su_kpa'] = 30
    assert evaluate(hand_case, values)['margin_kn'] == pytest.approx(63.4 * 25 * math.pi)
    assert evaluate(hand_case, values)['punch_through'] is False


def test_oat_deltas_are_hand_differences_and_do_not_mutate_input(hand_case):
    before = copy.deepcopy(hand_case)
    rows, cases = tornado(hand_case)
    rows = {row['parameter']: row for row in rows}
    # Changing upper strength by +/-20 changes 3*su*H/D by +/-12 kPa;
    # changing lower strength by +/-10 gives +/-63.4 kPa;
    # changing H by +/-1 changes punching by +/-24 kPa.
    for name, delta in [('layers.0.su_kpa', 12), ('layers.1.su_kpa', 63.4),
                        ('top_thickness_m', 24)]:
        assert rows[name]['low_delta_kn'] == pytest.approx(-delta * 25 * math.pi)
        assert rows[name]['high_delta_kn'] == pytest.approx(delta * 25 * math.pi)
    assert hand_case == before
    assert set(cases) == {'nominal'} | {
        f'{name}.{bound}' for name in hand_case['parameters'] for bound in ('low', 'high')}
    low = cases['top_thickness_m.low']['input']['profile']['layers']
    assert low[0]['bottom_m'] == low[1]['top_m'] == 1
    assert low[1]['bottom_m'] == 20  # deeper boundary remains fixed


@pytest.mark.parametrize('successes,total,expected', [
    (0, 100, (0, 3.841458820694124 / 103.84145882069412)),
    (100, 100, (100 / 103.84145882069412, 1)),
    # Wilson score, z=1.959963984540054: for p=.5 the half width is
    # z*sqrt(25+z*z/4)/(100+z*z), evaluated independently here.
    (50, 100, (0.4038315303659956, 0.5961684696340044)),
])
def test_wilson_hand_values(successes, total, expected):
    assert wilson_interval(successes, total) == pytest.approx(expected)


@pytest.mark.parametrize('successes,total', [(-1, 10), (11, 10), (0, 0), (True, 10), (1, 2.5)])
def test_invalid_counts(successes, total):
    with pytest.raises(ValueError):
        wilson_interval(successes, total)


def test_seeded_mc_count_uses_independent_bernoulli_hand_equation(hand_case, tmp_path):
    # Only s varies; the other uniform intervals are degenerate by declaration.
    hand_case['parameters']['layers.0.su_kpa'].update(low=80, high=80)
    hand_case['parameters']['top_thickness_m'].update(low=2, high=2)
    # Sample order is lexical parameter order. Every input consumes one draw.
    rng = random.Random(17)
    count = 0
    for _ in range(10000):
        rng.random()
        count += rng.random() <= 0.5
        rng.random()
    output = tmp_path / 'samples.csv'
    result = monte_carlo(hand_case, n=10000, seed=17, sample_path=output)
    assert result['punch_through_count'] == count
    assert result['probability'] == count / 10000
    assert abs(result['probability'] - 0.5) < 0.02  # two percentage points
    assert result['interval_95'][0] < 0.5 < result['interval_95'][1]
    assert result['n'] == 10000 and result['seed'] == 17
    with output.open(encoding='utf-8') as stream:
        rows = list(csv.DictReader(stream))
    assert len(rows) == 10000
    assert sum(row['punch_through'] == 'True' for row in rows) == count
    assert monte_carlo(hand_case, n=10000, seed=17) == result


def test_control_has_no_loss_margin_but_can_be_unsupported(hand_case):
    hand_case['input']['profile']['layers'] = [
        {'kind': 'clay', 'top_m': 0, 'bottom_m': 20,
         'submerged_weight_kn_m3': 0, 'su_kpa': 10}]
    hand_case['parameters'] = {'layers.0.su_kpa': {
        'low': 10, 'high': 30, 'distribution': 'uniform'}}
    result = evaluate(hand_case, nominal_values(hand_case))
    assert result['punch_through'] is False
    assert result['margin_kn'] is None
    assert result['loss_threshold_kn'] is None
    assert result['unsupported'] is True  # 63.4 < 174.8 kPa throughout
    rows, _ = tornado(hand_case)
    assert rows[0]['low_delta_kn'] is None and rows[0]['high_delta_kn'] is None


def test_joint_sampling_matches_hand_punching_inequality(hand_case):
    # For all bounds in this case, 6.34*s + 3*t*H/10 < 6.34*t,
    # so punching controls the surface peak; zero weights ensure a drop.
    # Varying t, s and H together is therefore an independent three-input
    # Bernoulli oracle, rather than a repeat of a production curve call.
    rng = random.Random(29)
    expected = 0
    for _ in range(10000):
        upper = 60 + 40 * rng.random()
        lower = 10 + 20 * rng.random()
        thickness = 1 + 2 * rng.random()
        expected += 6.34 * lower + 3 * upper * thickness / 10 <= 174.8
    result = monte_carlo(hand_case, n=10000, seed=29)
    assert result['punch_through_count'] == expected
    assert result['no_loss_count'] == 0


def test_sand_friction_bound_from_independent_punching_expression(hand_case):
    hand_case['input']['profile'] = {
        'layers': [
            {'kind': 'sand', 'top_m': 0, 'bottom_m': 5,
             'submerged_weight_kn_m3': 10, 'friction_deg': 30},
            {'kind': 'clay', 'top_m': 5, 'bottom_m': 20,
             'submerged_weight_kn_m3': 7, 'su_kpa': 10}],
        'sand_method': 'punching_shear', 'spread_ratio': 3, 'punching_coefficient': 1.5}
    hand_case['parameters'] = {
        'layers.0.friction_deg': {'low': 28, 'high': 36, 'distribution': 'uniform'},
        'layers.1.su_kpa': {'low': 10, 'high': 30, 'distribution': 'uniform'},
        'top_thickness_m': {'low': 3, 'high': 5, 'distribution': 'uniform'}}
    # q(z)=63.4+3*(25-z*z)*tan(phi)+10z; peak z=10/(6*tan(phi)).
    # Homogeneous sand cap is higher throughout these peaks.
    for phi in (28, 36):
        values = nominal_values(hand_case)
        values['layers.0.friction_deg'] = phi
        tangent = math.tan(math.radians(phi))
        z = 10 / (6 * tangent)
        expected = (63.4 + 3 * (25 - z*z) * tangent + 10*z) * 25 * math.pi
        result = evaluate(hand_case, values)
        assert result['loss_threshold_kn'] == pytest.approx(expected)


@pytest.mark.parametrize('change', ['missing', 'extra', 'reverse', 'nan', 'distribution', 'overlap'])
def test_invalid_distributions_rejected_before_sampling(hand_case, change):
    if change == 'missing':
        del hand_case['parameters']['layers.1.su_kpa']
    elif change == 'extra':
        hand_case['parameters']['unknown'] = {'low': 0, 'high': 1, 'distribution': 'uniform'}
    elif change == 'reverse':
        hand_case['parameters']['top_thickness_m']['low'] = 4
    elif change == 'nan':
        hand_case['parameters']['top_thickness_m']['high'] = float('nan')
    elif change == 'distribution':
        hand_case['parameters']['top_thickness_m']['distribution'] = 'normal'
    else:
        hand_case['parameters']['top_thickness_m']['high'] = 21
    with pytest.raises(ValueError):
        validate_case(hand_case)


def test_realization_rejects_out_of_range_and_incomplete_values(hand_case):
    for values in ({}, dict(nominal_values(hand_case), top_thickness_m=50)):
        with pytest.raises(ValueError):
            realize(hand_case, values)


def load_example():
    spec = importlib.util.spec_from_file_location(
        'sensitivity_example', 'examples/domains/geotechnical/jackup_soil_sensitivity.py')
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_named_inputs_and_offline_exports_resume_and_detect_tampering(tmp_path, monkeypatch):
    module = load_example()
    # Small runs exercise the exporter; production CLI enforces at least 10000.
    module.write_study(tmp_path, n=12, seed=17)
    first = {p.name: p.read_bytes() for p in tmp_path.iterdir()}
    assert all(b'\r' not in value for name, value in first.items() if name.endswith('.csv'))
    def unexpected_recalculation(*args, **kwargs):
        pytest.fail('completed cases must be reused without recalculation')
    monkeypatch.setattr(module, 'monte_carlo', unexpected_recalculation)
    module.write_study(tmp_path, n=12, seed=17)
    assert first == {p.name: p.read_bytes() for p in tmp_path.iterdir()}
    probability = json.loads((tmp_path / 'probability.figure.json').read_text(encoding='utf-8'))
    tornado_spec = json.loads((tmp_path / 'tornado.figure.json').read_text(encoding='utf-8'))
    assert probability['data'][0]['type'] == 'bar'
    assert probability['data'][0]['error_y']['symmetric'] is False
    assert tornado_spec['layout']['xaxis']['title']['text'] == 'Change in margin from nominal (MN)'
    for path in tmp_path.glob('*.cases.json'):
        cases = json.loads(path.read_text(encoding='utf-8'))['cases']
        assert 'nominal' in cases
        assert all('input' in payload and 'values' in payload for payload in cases.values())
    control = json.loads((tmp_path / 'uniform_soft.summary.json').read_text(encoding='utf-8'))
    assert control['result']['punch_through_count'] == 0  # monotone uniform clay
    with (tmp_path / 'sand_over_soft.samples.csv').open(encoding='utf-8') as stream:
        spread = list(csv.DictReader(stream))
    with (tmp_path / 'sand_over_soft_punching.samples.csv').open(encoding='utf-8') as stream:
        punching = list(csv.DictReader(stream))
    for a, b in zip(spread, punching):
        for parameter in ('layers.0.friction_deg', 'layers.1.su_kpa', 'top_thickness_m'):
            assert a[parameter] == b[parameter]  # common random soil draws
    sample = tmp_path / 'uniform_soft.samples.csv'
    sample.write_text(sample.read_text(encoding='utf-8') + '\n', encoding='utf-8', newline='\n')
    with pytest.raises(ValueError, match='digest'):
        module.write_study(tmp_path, n=12, seed=17)


@pytest.mark.parametrize('field,value', [
    (('result', 'n'), 13),
    (('result', 'seed'), 18),
    (('result', 'punch_through_count'), 12),
    (('result', 'probability'), 1.0),
    (('result', 'interval_95'), [1.0, 1.0]),
    (('result', 'unsupported_count'), -1),
    (('result', 'no_loss_count'), 0),
    (('result',), {}),
    (('tornado', 0, 'low_delta_kn'), 0.0),
    (('tornado',), []),
    (('preload_kn',), 1.0),
    (('name',), 'changed'),
    (('basis',), 'changed'),
    (('schema_version',), 0),
])
def test_reuse_rejects_summary_corruption_before_rewriting_outputs(tmp_path, field, value):
    # Uniform clay has 0/12 descending peaks and 12/12 no-loss samples.
    # Changing only the summary must never publish a different interpretation.
    module = load_example()
    module.write_study(tmp_path, n=12, seed=17)
    outputs = {name: (tmp_path / name).read_bytes() for name in (
        'probabilities.csv', 'tornado.csv', 'probability.figure.json', 'tornado.figure.json')}
    path = tmp_path / 'uniform_soft.summary.json'
    summary = json.loads(path.read_text(encoding='utf-8'))
    target = summary
    for key in field[:-1]:
        target = target[key]
    target[field[-1]] = value
    module.write_json(path, summary)
    with pytest.raises(ValueError, match='summary'):
        module.write_study(tmp_path, n=12, seed=17)
    assert outputs == {name: (tmp_path / name).read_bytes() for name in outputs}


def test_reuse_requires_every_expected_artifact_digest(tmp_path):
    module = load_example()
    module.write_study(tmp_path, n=12, seed=17)
    path = tmp_path / 'uniform_soft.summary.json'
    summary = json.loads(path.read_text(encoding='utf-8'))
    del summary['artifacts']['uniform_soft.samples.csv']
    module.write_json(path, summary)
    with pytest.raises(ValueError, match='artifact'):
        module.write_study(tmp_path, n=12, seed=17)


def test_reference_digests_bind_current_sources_inputs_and_outputs():
    root = Path(__file__).resolve().parents[2]
    inputs = root / 'examples/data/jackup_soil_sensitivity'
    reference = inputs / 'reference'
    manifest = json.loads((reference / 'manifest.json').read_text(encoding='utf-8'))
    rebuild = ('PYTHONPATH=src python examples/domains/geotechnical/'
               'jackup_soil_sensitivity.py OUTPUT --samples 10000 --seed 20261028')
    for group, directory in (('sources', root), ('inputs', inputs), ('outputs', reference)):
        for name, expected in manifest[group].items():
            actual = hashlib.sha256((directory / name).read_bytes()).hexdigest()
            assert actual == expected, f'{group}/{name}: rebuild with {rebuild} and refresh the manifest'


def test_cli_refuses_fewer_than_required_samples(tmp_path):
    with pytest.raises(SystemExit):
        load_example().main([str(tmp_path), '--samples', '9999'])
