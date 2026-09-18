"""Conditional envelopes preserve unknowns, physical mapping and sampled gaps."""
import copy
import json

import pytest

from digitalmodel.workflows.installation_assumed_envelope import build_envelope


@pytest.fixture
def inputs():
    rows = [dict(index=i, hs_m=hs, tp_s=tp, status='COMPLETED')
            for i, (hs, tp) in enumerate((h, t) for h in [.25, .5, .75, 1.] for t in [4, 5])]
    cases = [dict(row, status='VERIFIED', channels={
        'rope_end_B': dict(units='kN', variable='Effective tension', maximum=50., minimum=20.,
                           static_tension_kN=100.),
        'turnbuckle_end_A': dict(units='kN', variable='Effective tension', maximum=60.)})
        for row in rows]
    summary = dict(campaign_snapshot={'cases': rows}, cases=cases)
    criteria = dict(status='project_assumption', provenance={'source': 'test project'},
        force_conversion=dict(kN_per_Te=10., provenance='fixture conversion'),
        unresolved_operational_checks=['snap', 'clamp'], checks=[
            dict(id='rope', kind='maximum_tension', channels=['rope_end_B'],
                 limit=10., value_units='Te', provenance='fixture capacity'),
            dict(id='hoist', kind='minimum_static_ratio', channels=['rope_end_B'],
                 limit=.1, value_units='1', provenance='fixture assumption')])
    return summary, criteria


def test_all_pass_is_censored_conditional_only(inputs):
    before = copy.deepcopy(inputs)
    result = build_envelope(*inputs)
    assert all(c['status'] == 'PASS' for c in result['cells'])
    assert all(c['max_utilization'] == .5 for c in result['cells'])
    assert all(b['highest_contiguous_pass_hs_m'] == 1. for b in result['boundaries'])
    assert all(b['upper_edge_censored'] for b in result['boundaries'])
    assert result['operational_acceptance'] == 'NOT_EVALUATED'
    assert result['unresolved_operational_checks'] == ['snap', 'clamp']
    assert inputs == before
    json.dumps(result, allow_nan=False)


@pytest.mark.parametrize('bad', [None, float('nan'), float('inf'), '50', True])
def test_nonfinite_or_missing_demand_does_not_pass(inputs, bad):
    inputs[0]['cases'][0]['channels']['rope_end_B']['maximum'] = bad
    result = build_envelope(*inputs)
    assert result['cells'][0]['status'] == 'NOT_EVALUATED'
    json.dumps(result, allow_nan=False)


@pytest.mark.parametrize('static', [0, -1, None, float('inf')])
def test_invalid_static_denominator_not_evaluated(inputs, static):
    inputs[0]['cases'][0]['channels']['rope_end_B']['static_tension_kN'] = static
    assert build_envelope(*inputs)['cells'][0]['status'] == 'NOT_EVALUATED'


def test_known_fail_dominates_missing_other_check(inputs):
    inputs[0]['cases'][0]['channels']['rope_end_B'].update(maximum=101., static_tension_kN=0)
    cell = build_envelope(*inputs)['cells'][0]
    assert cell['status'] == 'FAIL'
    assert cell['governing_check'] == 'rope'
    assert cell['max_utilization'] == 1.01


@pytest.mark.parametrize('minimum', [0., -10.])
def test_nonpositive_hoist_tension_fails_without_infinity(inputs, minimum):
    inputs[0]['cases'][0]['channels']['rope_end_B']['minimum'] = minimum
    result = build_envelope(*inputs)
    assert result['cells'][0]['status'] == 'FAIL'
    assert result['cells'][0]['governing_check'] == 'hoist'
    json.dumps(result, allow_nan=False)


def test_unknown_check_and_wrong_units_fail_closed(inputs):
    inputs[1]['checks'][0]['kind'] = 'unrecognized'
    assert build_envelope(*inputs)['cells'][0]['status'] == 'NOT_EVALUATED'
    inputs[1]['checks'][0]['kind'] = 'maximum_tension'
    inputs[0]['cases'][0]['channels']['rope_end_B']['units'] = 'Te'
    assert build_envelope(*inputs)['cells'][0]['status'] == 'NOT_EVALUATED'


def test_composite_endpoint_mapping_is_exact(inputs):
    inputs[0]['cases'][0]['channels']['turnbuckle_end_A']['maximum'] = 5000.
    assert build_envelope(*inputs)['cells'][0]['status'] == 'PASS'
    inputs[1]['checks'].append(dict(id='turnbuckle', kind='maximum_tension',
        channels=['turnbuckle_end_A'], limit=20., value_units='Te', provenance='test'))
    cell = build_envelope(*inputs)['cells'][0]
    assert cell['status'] == 'FAIL'
    assert cell['governing_check'] == 'turnbuckle'


def test_missing_run_stops_contiguous_pass_preserves_island(inputs):
    del inputs[0]['cases'][2]
    result = build_envelope(*inputs)
    boundary = result['boundaries'][0]
    assert boundary['highest_contiguous_pass_hs_m'] == .25
    assert boundary['pass_islands'] == [[.25], [.75, 1.]]
    assert boundary['upper_edge_censored'] is True
    assert boundary['first_nonpass_hs_m'] == .5
    assert boundary['first_nonpass_status'] == 'NOT_EVALUATED'


def test_nonmonotonic_pass_fail_pass_is_not_interpolated(inputs):
    inputs[0]['cases'][2]['channels']['rope_end_B']['maximum'] = 120.
    boundary = build_envelope(*inputs)['boundaries'][0]
    assert boundary['highest_contiguous_pass_hs_m'] == .25
    assert boundary['pass_islands'] == [[.25], [.75, 1.]]
    assert boundary['nonmonotonic_observed'] is True


def test_lowest_tested_failure_yields_no_contiguous_window(inputs):
    inputs[0]['cases'][0]['channels']['rope_end_B']['maximum'] = 120.
    assert build_envelope(*inputs)['boundaries'][0]['highest_contiguous_pass_hs_m'] is None


def test_unknown_provenance_or_conversion_does_not_pass(inputs):
    inputs[1]['force_conversion']['provenance'] = None
    assert build_envelope(*inputs)['cells'][0]['status'] == 'NOT_EVALUATED'
    inputs[1]['force_conversion']['provenance'] = 'test'
    inputs[1]['checks'][0]['provenance'] = None
    assert build_envelope(*inputs)['cells'][0]['status'] == 'NOT_EVALUATED'


def test_unverified_case_is_unknown_even_with_metrics(inputs):
    inputs[0]['cases'][0]['status'] = 'RUNNING'
    assert build_envelope(*inputs)['cells'][0]['status'] == 'NOT_EVALUATED'


@pytest.mark.parametrize('defect', ['duplicate', 'coordinate', 'extra'])
def test_ambiguous_evidence_identity_rejected(inputs, defect):
    cases = inputs[0]['cases']
    if defect == 'duplicate':
        cases.append(copy.deepcopy(cases[0]))
    elif defect == 'coordinate':
        cases[0]['hs_m'] = 99
    else:
        cases.append(dict(cases[0], index=99))
    with pytest.raises(ValueError):
        build_envelope(*inputs)


def test_empty_criteria_never_vacuously_pass(inputs):
    inputs[1]['checks'] = []
    assert all(c['status'] == 'NOT_EVALUATED' for c in build_envelope(*inputs)['cells'])


def test_null_conversion_and_nonfinite_limit_are_unknown(inputs):
    inputs[1]['force_conversion'] = None
    result = build_envelope(*inputs)
    assert result['cells'][0]['status'] == 'NOT_EVALUATED'
    inputs[1]['checks'][0]['limit'] = float('nan')
    result = build_envelope(*inputs)
    assert result['cells'][0]['status'] == 'NOT_EVALUATED'
    json.dumps(result, allow_nan=False)


def test_missing_channel_in_group_prevents_group_pass(inputs):
    inputs[1]['checks'][0]['channels'].append('missing_end_B')
    assert build_envelope(*inputs)['cells'][0]['status'] == 'NOT_EVALUATED'
    inputs[0]['cases'][0]['channels']['rope_end_B']['maximum'] = 101.
    assert build_envelope(*inputs)['cells'][0]['status'] == 'FAIL'


def test_exact_limit_passes_and_seed_mismatch_rejects(inputs):
    inputs[0]['cases'][0]['channels']['rope_end_B'].update(maximum=100., minimum=10.)
    assert build_envelope(*inputs)['cells'][0]['status'] == 'PASS'
    inputs[0]['cases'][0]['seed'] = 999
    with pytest.raises(ValueError, match='seed'):
        build_envelope(*inputs)
