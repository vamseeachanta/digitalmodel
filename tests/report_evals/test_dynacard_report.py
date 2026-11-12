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
    from digitalmodel.marine_ops.artificial_lift.dynacard.everitt_jennings.adapter import EverittJenningsContextSolver
    from digitalmodel.marine_ops.artificial_lift.dynacard.calculations import calculate_pump_fillage
    ctx = load_from_json_file(FIXTURE)
    card = EverittJenningsContextSolver(ctx).solve().downhole_card
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
    assert result['solver_cross_check']['gibbs_default_load_nrmse_pct'] >= 0


def test_report_equals_fresh_build(analysis):
    builder = module('build_report')
    report = ROOT / 'docs/domains/artificial_lift/dynacard-pump-card-report.md'
    assert report.read_text() == builder.build_report(analysis)
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
