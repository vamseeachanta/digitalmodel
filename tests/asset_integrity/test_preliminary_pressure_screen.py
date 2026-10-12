# ABOUTME: Verify bounded named pressure screens retain factors and applicability.
# ABOUTME: Preliminary thresholds cannot become API 579 or asset acceptance.
import json
import pytest

from digitalmodel.asset_integrity.ffs_acceptance_curves import pipe_pressure_wall_screen


def screen(**overrides):
    inputs = dict(D=20., t=.5, grade='X52', method='modified_b31g',
                  axial_length_in=8., pressure_psi=1310.4, safety_factor=1/.72)
    inputs.update(overrides)
    return pipe_pressure_wall_screen(**inputs)


@pytest.mark.parametrize('method', ['b31g', 'modified_b31g', 'rstreng'])
def test_threshold_brackets_pressure_and_retains_scope(method):
    row = screen(method=method)
    assert row['status'] == 'PRELIMINARY_THRESHOLD'
    assert row['api579_allowable_remaining_wall_in'] is None
    assert row['asset_acceptance'] is None
    assert row['evidence_status'] == 'PRELIMINARY_ARITHMETIC_ONLY'
    assert row['inputs']['safety_factor'] == 1/.72
    assert row['passing_bracket']['safe_pressure_psi'] >= 1310.4
    assert row['failing_bracket']['safe_pressure_psi'] < 1310.4
    assert row['passing_bracket']['remaining_wall_in'] - row['failing_bracket']['remaining_wall_in'] <= 1e-6
    assert all(sample['applicability']['ok'] for sample in row['monotonicity_samples'])
    json.dumps(row, allow_nan=False)


def test_censoring_and_no_solution_are_distinct():
    censored = screen(pressure_psi=1)
    assert censored['status'] == 'LOWER_BOUND_CENSORED'
    assert censored['preliminary_remaining_wall_in'] is None
    assert censored['failing_bracket'] is None
    assert censored['passing_bracket']['remaining_wall_in'] == pytest.approx(.1)
    assert censored['passing_bracket']['pressure_margin_psi'] >= 0
    row = screen(pressure_psi=1e6)
    assert row['status'] == 'NO_PRESSURE_SOLUTION'
    assert row['preliminary_remaining_wall_in'] is None


@pytest.mark.parametrize('overrides', [dict(flaw_orientation='circumferential'),
    dict(load_case='pressure-plus-compression'), dict(max_depth_fraction=.85)])
def test_unsupported_cases_have_no_threshold(overrides):
    row = screen(**overrides)
    assert row['status'] == 'INAPPLICABLE'
    assert row['preliminary_remaining_wall_in'] is None
    assert row['reason_codes']


def test_explicit_factor_changes_threshold():
    assert screen(safety_factor=2)['preliminary_remaining_wall_in'] > screen()['preliminary_remaining_wall_in']


def test_rectangular_rstreng_threshold_and_depth_boundary_against_raw_method():
    from digitalmodel.asset_integrity.corroded_pipe import rstreng_effective_area
    for wall in (.375, .625, .5):
        row = screen(t=wall, method='rstreng', pressure_psi=.7*2*52000*.72*wall/20)
        passing = row['passing_bracket']
        depth = wall-passing['remaining_wall_in']
        raw = rstreng_effective_area(20., wall, [0., 8.], [depth, depth], 52000.,
                                    safety_factor=1/.72)
        assert raw.safe_pressure_psi == pytest.approx(passing['safe_pressure_psi'])
        assert raw.area_ratio == pytest.approx(depth/wall)
        endpoint = row['monotonicity_samples'][0]
        assert (wall-endpoint['remaining_wall_in'])/wall <= .8
        assert endpoint['applicability']['ok']


@pytest.mark.parametrize('overrides', [dict(pressure_psi=float('nan')), dict(safety_factor=0), dict(axial_length_in=0)])
def test_invalid_input_rejected(overrides):
    with pytest.raises(ValueError):
        screen(**overrides)


def test_four_size_sweep_retains_all_dispositions_and_withholds_acceptance():
    from digitalmodel.asset_integrity.uniform_loss_diagnostic import run_preliminary_pressure_study
    result = run_preliminary_pressure_study()
    assert len(result['cases']) == 84
    from collections import Counter
    assert Counter(row['status'] for row in result['cases']) == {
        'PRELIMINARY_THRESHOLD': 42, 'LOWER_BOUND_CENSORED': 42}
    assert result['meta']['qualified_cases'] == 0
    assert result['meta']['source_sha256']
    for row in result['cases']:
        assert row['api579_allowable_remaining_wall_in'] is None
        assert row['asset_acceptance'] is None
        assert row['inputs']['safety_factor'] == 1/.72
        assert row['status'] in {'PRELIMINARY_THRESHOLD', 'LOWER_BOUND_CENSORED'}
        assert all(sample['applicability']['ok'] for sample in row['monotonicity_samples'])
    json.dumps(result, allow_nan=False)


@pytest.mark.parametrize('wall', [.3, .7, .1875, .375, .625, .5, .1234567, .913])
def test_endpoint_construction_preserves_depth_bounds_and_exact_nominal(wall):
    row = screen(t=wall, pressure_psi=.7*2*52000*.72*wall/20)
    assert all(sample['applicability']['ok'] for sample in row['monotonicity_samples'])
    assert row['monotonicity_samples'][0]['depth_in']/wall <= .8
    assert row['monotonicity_samples'][-1]['depth_in'] == 0
    assert row['monotonicity_samples'][-1]['remaining_wall_in'] == wall


def test_unknown_grade_is_explicit_input_error():
    with pytest.raises(ValueError, match='Unknown pipe grade'):
        screen(grade='unknown')


def test_pressure_reversal_rejects_inversion(monkeypatch):
    from dataclasses import replace
    from digitalmodel.asset_integrity import ffs_acceptance_curves as curves
    raw = curves.modified_b31g
    def reverse(*args, **kwargs):
        row = raw(*args, **kwargs)
        return replace(row, safe_pressure_psi=-100.) if abs(args[2]-.2) < 1e-10 else row
    monkeypatch.setattr(curves, 'modified_b31g', reverse)
    row = screen()
    assert row['status'] == 'INAPPLICABLE'
    assert row['reason_codes'] == ['NONMONOTONIC_PRESSURE']
    assert row['preliminary_remaining_wall_in'] is None


def test_bisection_flag_cannot_become_threshold(monkeypatch):
    from dataclasses import replace
    from digitalmodel.asset_integrity import ffs_acceptance_curves as curves
    from digitalmodel.asset_integrity.applicability import flagged
    raw, calls = curves.modified_b31g, []
    def flag_after_sampling(*args, **kwargs):
        calls.append(1)
        row = raw(*args, **kwargs)
        return replace(row, applicability=flagged('TEST_FLAG', 'injected midpoint flag')) if len(calls)>101 else row
    monkeypatch.setattr(curves, 'modified_b31g', flag_after_sampling)
    row = screen()
    assert row['status'] == 'INAPPLICABLE'
    assert row['flagged_evaluation']['applicability']['flags'] == ['TEST_FLAG']
    assert row['preliminary_remaining_wall_in'] is None


def test_cli_can_record_missing_git_without_host_identifier(tmp_path, monkeypatch):
    import subprocess
    from digitalmodel.asset_integrity import uniform_loss_diagnostic as diagnostic
    output = tmp_path/'pressure.json'
    def missing_git(*args, **kwargs):
        raise subprocess.CalledProcessError(1, 'git')
    monkeypatch.setattr(diagnostic.subprocess, 'run', missing_git)
    monkeypatch.setattr('sys.argv', ['study', '--preliminary-pressure', '--output', str(output)])
    diagnostic.main()
    runtime = json.loads(output.read_text())['meta']['runtime']
    assert runtime['executing_revision'] is None
    assert runtime['revision_status'] == 'GIT_PROVENANCE_UNAVAILABLE'
    assert 'host' not in runtime


def test_plot_rejects_hidden_unsupported_dispositions(tmp_path):
    from digitalmodel.asset_integrity.uniform_loss_diagnostic import plot_preliminary_pressure_study
    with pytest.raises(ValueError, match='inspect other dispositions'):
        plot_preliminary_pressure_study({'cases': [screen(load_case='external-pressure')]}, tmp_path/'plot.png')
