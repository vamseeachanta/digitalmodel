"""Public surface-to-pump worked report: reproducibility and reader inputs."""
import importlib.util
import json
import sys
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
HERE = ROOT / 'examples/report-evals/dynacard-pump-card'
FIXTURE = ROOT / 'tests/marine_ops/artificial_lift/test_data/cleansed_well_001.json'


def module(name):
    sys.path.insert(0, str(HERE))
    spec = importlib.util.spec_from_file_location('dynacard_report_' + name, HERE / (name + '.py'))
    loaded = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(loaded)
    return loaded


@pytest.fixture(scope='module')
def analysis():
    return module('analysis').run(FIXTURE)


def test_public_example_uses_package_results(analysis):
    from digitalmodel.marine_ops.artificial_lift.dynacard.data_loader import load_from_json_file
    from digitalmodel.marine_ops.artificial_lift.dynacard.calculations import calculate_pump_fillage
    ctx = load_from_json_file(FIXTURE)
    card = module('analysis').solve_default(ctx)
    fillage = calculate_pump_fillage(card)
    assert analysis['key_results']['pump_stroke_in'] == pytest.approx(fillage.gross_stroke)
    assert analysis['key_results']['fillage_pct'] == pytest.approx(fillage.fillage)


def test_reader_layout_reproduces_fixture(analysis):
    got = module('analysis').run(HERE / 'reader-card-template.json')
    assert got['key_results'] == pytest.approx(analysis['key_results'], rel=1e-10, abs=1e-8)
    assert got['solver_cross_check']['default_reference_load_nrmse_pct'] is None
    assert analysis['solver_cross_check']['default_reference_load_nrmse_pct'] is not None


@pytest.mark.parametrize('field,value,message', [
    ('strokes_per_minute', 0, 'strokes_per_minute'),
    ('rod_string', [], 'rod_string'),
    ('surface_card', {'position_in': [0, 1], 'load_lbf': [1]}, 'surface_card'),
    ('fluid_density_lb_ft3', float('nan'), 'fluid_density_lb_ft3'),
    ('tubing_anchored', False, 'tubing'),
])
def test_reader_input_errors(tmp_path, field, value, message):
    data = json.loads((HERE / 'reader-card-template.json').read_text())
    data[field] = value
    path = tmp_path / 'card.json'
    path.write_text(json.dumps(data))
    with pytest.raises(ValueError, match=message):
        module('analysis').run(path)


def test_sensitivity_covers_steps_and_central_factors(analysis):
    table = analysis['sensitivity']
    assert len(table) == 8
    for row in table:
        assert [r['input_change_pct'] for r in row['steps']] == [-5, -1, 1, 5]
        for output in ('pump_stroke_in', 'fluid_load_lbf', 'fillage_pct', 'maximum_pump_load_lbf'):
            changes = [r['output_change_pct'][output] for r in row['steps']]
            assert row['factor_1pct'][output] == pytest.approx((changes[2] - changes[1]) / 2)
            assert row['factor_5pct'][output] == pytest.approx((changes[3] - changes[0]) / 10)
    density = next(r for r in table if r['input'] == 'fluid_density')
    assert abs(density['steps'][0]['output_change_pct']['maximum_pump_load_lbf']) > 0


def test_assumptions_and_hypotheses_are_never_pass(analysis):
    assert len(analysis['assumptions']) >= 8
    assert {r['status'] for r in analysis['assumptions']} == {'ASSUMED'}
    shape = analysis['shape_hypothesis']
    assert shape['status'] == 'HYPOTHESIS'
    assert len(shape['top_three']) == len(shape['nearest_three']) == 3
    for row in shape['top_three'] + shape['nearest_three']:
        assert row['status'] == 'HYPOTHESIS'
        assert row['plate_score']['literal'] == {'correct': 5, 'count': 8}
        assert row['plate_score']['including_assumed'] == {'correct': 6, 'count': 10}
    assert analysis['validation']['published_example']['status'] == 'NOT_EVALUATED'


def test_repeated_closing_pair_is_compared(tmp_path):
    data = json.loads((HERE / 'reader-card-template.json').read_text())
    for name in ('position_in', 'load_lbf'):
        data['surface_card'][name].append(data['surface_card'][name][0])
    path = tmp_path / 'closed.json'
    path.write_text(json.dumps(data))
    result = module('analysis').run(path, include_sensitivity=False)
    opened = module('analysis').run(HERE / 'reader-card-template.json', include_sensitivity=False)
    assert result['key_results'] == pytest.approx(opened['key_results'], rel=1e-12, abs=1e-10)
    assert result['solver_cross_check'] == opened['solver_cross_check']
    for name in ('surface', 'default', 'gibbs'):
        assert result['cards'][name] == opened['cards'][name]
        assert len(result['cards'][name]['position_in']) == 187


def test_report_equals_fresh_build(analysis):
    builder = module('build_report')
    report = ROOT / 'docs/domains/artificial_lift/dynacard-pump-card-report.md'
    assert report.read_text() == builder.build_report(analysis)
    assert report.read_text().split('## ')[1:3][1].startswith('Revision history\n')
    prompt = (HERE / 'PROMPT.md').read_text()
    assert prompt == builder.build_prompt(analysis)
    for row in analysis['missing_data']:
        assert row['what'] in prompt and row['effect'] in prompt
    for text in analysis['cannot_do']:
        assert text in prompt


def test_candidate_eval_rebuilds_and_grades(tmp_path):
    import subprocess
    out = tmp_path / 'run.json'
    for args in (['--validate-only'], ['--out', str(out)]):
        result = subprocess.run([sys.executable, str(HERE / 'eval.py'), *args],
                                capture_output=True, text=True, timeout=120)
        assert result.returncode == 0, result.stdout + result.stderr
    record = json.loads(out.read_text())
    assert record['grade'] == 'PASS'
    assert len(record['results']) >= 16
    assert record['context']['digitalmodel_file'] == 'src/digitalmodel/__init__.py'


def test_eval_rejects_package_outside_checkout(tmp_path):
    evaluator = module('eval')
    args = type('Args', (), {'repo_root': tmp_path, 'revision': 'candidate', 'pinned_commit': '0' * 40})()
    with pytest.raises(ImportError, match='checkout being graded'):
        evaluator.check_package(args, ROOT / 'src/digitalmodel/__init__.py', '0' * 40)


@pytest.mark.parametrize('change,message', [
    (lambda d: d['surface_card'].update(position_in=[0, 1], load_lbf=[10, 20]), '30 usable'),
    (lambda d: d['rod_string'][0].update(material='unknown'), 'material'),
    (lambda d: d.update(unknown_input=3), 'unsupported'),
    (lambda d: d.pop('strokes_per_minute'), 'strokes_per_minute'),
])
def test_reader_rejects_incomplete_or_unsupported_data(tmp_path, change, message):
    data = json.loads((HERE / 'reader-card-template.json').read_text())
    change(data)
    path = tmp_path / 'card.json'
    path.write_text(json.dumps(data))
    with pytest.raises(ValueError, match=message):
        module('analysis').run(path, include_sensitivity=False)


def test_omitted_reader_properties_are_assumed(tmp_path):
    data = json.loads((HERE / 'reader-card-template.json').read_text())
    data.pop('fluid_density_lb_ft3')
    data.pop('tubing_pressure_psi')
    for rod in data['rod_string']:
        for key in ('material', 'modulus_psi', 'weight_lb_ft', 'wave_speed_ft_s'):
            rod.pop(key)
    path = tmp_path / 'card.json'
    path.write_text(json.dumps(data))
    result = module('analysis').run(path, include_sensitivity=False)
    assumptions = {r['id']: r for r in result['assumptions']}
    assert assumptions['fluid_density_lb_ft3']['value'] == 62.4
    from digitalmodel.marine_ops.artificial_lift.dynacard.constants import STEEL_YOUNGS_MODULUS_PSI
    assert assumptions['steel_modulus_psi']['value'] == [STEEL_YOUNGS_MODULUS_PSI] * 2
    assert assumptions['rod_material']['value'] == 'steel'
    assert {r['status'] for r in assumptions.values()} == {'ASSUMED'}


def test_stale_shape_score_is_rejected(tmp_path):
    from digitalmodel.marine_ops.artificial_lift.dynacard.models import CardData
    basis = module('model_basis')
    path = tmp_path / basis.SCORE
    path.parent.mkdir(parents=True)
    score = json.loads((ROOT / basis.SCORE).read_text())
    score['models']['shape_only']['sha256'] = '0' * 64
    path.write_text(json.dumps(score))
    with pytest.raises(ValueError, match='does not match'):
        basis.shape_evidence(CardData(position=[0, 1, 2], load=[1, 2, 1]), tmp_path)


def test_committed_manifest_equals_fresh_build(analysis):
    assert json.loads((HERE / 'results.json').read_text()) == module('build_results').manifest(analysis)
    for row in module('build_results').numerical_results(analysis):
        rid, _, _, _, digits = row
        result = next(r for r in json.loads((HERE / 'results.json').read_text())['results'] if r['id'] == rid)
        assert result['tolerance'] == {'abs': 0.5 * 10 ** -digits}


@pytest.mark.parametrize('field,value,message', [
    ('material', None, 'rod_string.material'),
    ('material', 123, 'rod_string.material'),
    ('load_sample', True, 'load_lbf'),
    ('position_sample', '3.5', 'position_in'),
])
def test_reader_does_not_coerce_invalid_measurements(tmp_path, field, value, message):
    data = json.loads((HERE / 'reader-card-template.json').read_text())
    if field == 'material':
        data['rod_string'][0]['material'] = value
    else:
        key = 'load_lbf' if field == 'load_sample' else 'position_in'
        data['surface_card'][key][0] = value
    path = tmp_path / 'card.json'
    path.write_text(json.dumps(data))
    with pytest.raises(ValueError, match=message):
        module('analysis').run(path, include_sensitivity=False)


def test_rod_endpoint_geometry_gap_is_registered(analysis):
    assert analysis['geometry_basis']['rod_length_ft'] == pytest.approx(4750)
    assert analysis['geometry_basis']['pump_depth_ft'] == 4844
    assert analysis['geometry_basis']['unrepresented_length_ft'] == pytest.approx(94)
    assert next(r for r in analysis['assumptions'] if r['id'] == 'pump_boundary')['status'] == 'ASSUMED'


def test_pinned_eval_checks_consumed_shape_assets(monkeypatch):
    evaluator = module('eval')
    pin = '1' * 40
    args = type('Args', (), {'repo_root': ROOT, 'revision': 'pinned', 'pinned_commit': pin})()
    monkeypatch.setattr(evaluator.evalkit, 'git_head', lambda path: pin)
    def blob(root, commit, rel, fetch=False):
        assert fetch is False
        if rel.endswith('plate_classifier_score.json'):
            return b'changed score bytes'
        return (root / rel).read_bytes()
    monkeypatch.setattr(evaluator.evalkit, 'git_blob', blob)
    with pytest.raises(ImportError, match='unmodified source and inputs'):
        evaluator.provenance(args, {})


def test_manifest_metadata_precision_preserves_material_changes(analysis):
    import copy
    builder = module('build_results')
    original = builder.manifest(analysis)
    changed = copy.deepcopy(analysis)
    distance = changed['shape_hypothesis']['nearest_three'][0]
    distance['distance'] += 1e-15
    assert builder.manifest(changed) == original
    distance['distance'] += 1e-6
    assert builder.manifest(changed) != original
    changed = copy.deepcopy(analysis)
    changed['sources'][0]['sha256'] = 'f' * 64
    assert builder.manifest(changed) != original
    changed = copy.deepcopy(analysis)
    changed['shape_hypothesis']['nearest_three'][0]['label'] += ' changed'
    assert builder.manifest(changed) != original
    assert builder.METADATA_DECIMAL_PLACES == 12


@pytest.mark.parametrize('positions,stroke,travel,fillage,work', [
    ([2, 2, 14, 14], 12, 12, 100, 200),
    ([2, 2, 12, 8], 10, 6, 60, 400 / 3),
])
def test_hand_calculated_card_metrics(positions, stroke, travel, fillage, work):
    from math import pi
    from digitalmodel.marine_ops.artificial_lift.dynacard.models import CardData
    report = module('analysis')
    ctx, _ = report.load_card(HERE / 'reader-card-template.json')
    ctx.surface_card = CardData(position=[0, 0, 20, 20], load=[500, 900, 900, 500])
    ctx.pump.diameter = 2
    ctx.spm = 10
    # Rectangle and trapezoid: upper/lower loads 300/100 lbf. The trapezoid
    # has bases 10/6 in: area = (10 + 6) * 200 / 2 = 1600 in-lbf.
    # A 2 in plunger has area pi in2; a barrel contains 9702 in3.
    card = CardData(position=positions, load=[100, 300, 300, 100])
    assert report.key_results(ctx, card) == pytest.approx({
        'surface_stroke_in': 20, 'peak_polished_rod_load_lbf': 900,
        'minimum_polished_rod_load_lbf': 500, 'pump_stroke_in': stroke,
        'stroke_ratio': stroke / 20, 'maximum_pump_load_lbf': 300,
        'minimum_pump_load_lbf': 100, 'fluid_load_lbf': 200,
        'fillage_pct': fillage, 'effective_travel_in': travel,
        'pump_displacement_bbl_day': pi * travel * 10 * 1440 / 9702,
        'gross_displacement_bbl_day': pi * stroke * 10 * 1440 / 9702,
        'pump_card_area_ft_lbf': work,
    })


def test_analytic_sensitivity_for_plunger_area():
    report = module('analysis')
    sensitivity = module('sensitivity')
    ctx, _ = report.load_card(HERE / 'reader-card-template.json')
    ctx.pump.diameter = 2
    # Independent analytic model: only volume depends on diameter, as D squared.
    # Other metrics are constant; no wave solver or report metric function runs.
    base = dict(zip(sensitivity.OUTPUTS, (12, 200, 75, 300, 400)))
    def analytic(changed):
        return dict(base, pump_displacement_bbl_day=100 * changed.pump.diameter ** 2)
    rows = sensitivity.sensitivity_table(ctx, base, analytic)
    row = next(r for r in rows if r['input'] == 'pump_diameter')
    for step, expected in zip(row['steps'], (-9.75, -1.99, 2.01, 10.25)):
        assert step['output_change_pct']['pump_displacement_bbl_day'] == pytest.approx(expected)
        for key in sensitivity.OUTPUTS[:-1]:
            assert step['output_change_pct'][key] == 0
    for size in (1, 5):
        assert row[f'factor_{size}pct']['pump_displacement_bbl_day'] == pytest.approx(2)
    assert row['stepped_outputs'] == []
    assert all(all(v == 0 for v in r['factor_1pct'].values())
               for r in rows if r['input'] != 'pump_diameter')


def test_periodic_reference_resampling_by_hand():
    from digitalmodel.marine_ops.artificial_lift.dynacard.models import CardData
    source = CardData(position=[0, 6, 0, 0], load=[100, 100, 400, 100])
    got = module('analysis').resample_phases(source, 6)
    assert got.position == pytest.approx([0, 3, 6, 3, 0, 0])
    assert got.load == pytest.approx([100, 100, 100, 250, 400, 250])
