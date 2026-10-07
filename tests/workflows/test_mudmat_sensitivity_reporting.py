"""Composite summary (one cell at a different time step) and the sensitivity appendix."""
import copy
from hashlib import sha256
from io import BytesIO
import json

import pytest
from pypdf import PdfReader

from digitalmodel.workflows import installation_composite_summary as composite
from digitalmodel.workflows import vessel_capability_layout as layout
from digitalmodel.workflows import vessel_capability_report as mudmat
from digitalmodel.workflows.installation_full_report_pdf import render_full_pdf

import tests.workflows.test_vessel_capability_screening as screening_fixture


def _row(index, status, hs, tp, peak=50.0, step=0.05):
    row = dict(index=index, status=status, hs_m=hs, tp_s=tp, seed=7)
    if status == 'VERIFIED':
        row.update(settings=dict(duration_s=600, fixed_time_step_s=step, hs_m=hs, tp_s=tp, seed=7,
                                 buildup_s=80, sample_interval_s=.1, gamma=3.3, components=200, max_time_step_s=.1),
                   peak_tension_kN=peak, maximum_low_tension_duration_s=1.0, run_dir='run', simulation_sha256='a',
                   trace_sha256='b', metadata_sha256='d', channels={'load': dict(position='End A', units='kN', minimum=0., maximum=peak,
                                                            variable='Effective tension', object='Sling')})
    return row


def _summaries(tmp_path):
    base = dict(created_utc='t0', campaign_sha256='c' * 64, matrix_sha256='m' * 64, design_basis={'mass_t': 5.},
                engineering_acceptance='NOT EVALUATED',
                campaign_snapshot={'status': 'stopped', 'cases': [dict(index=0, hs_m=1., tp_s=8, seed=7, status='COMPLETED'),
                                                                   dict(index=1, hs_m=1., tp_s=10, seed=7, status='FAILED')]},
                cases=[_row(0, 'VERIFIED', 1., 8), _row(1, 'FAILED', 1., 10)], counts={'VERIFIED': 1, 'FAILED': 1})
    base['campaign_snapshot']['master_sha256'] = 'M0'
    base['cases'][1]['settings'] = copy.deepcopy(base['cases'][0]['settings'])
    base['cases'][1]['settings']['tp_s'] = 10
    supplement = copy.deepcopy(base)
    supplement['campaign_snapshot']['master_sha256'] = 'M1'
    supplement['cases'] = [_row(0, 'MISSING', 1., 8), _row(1, 'VERIFIED', 1., 10, peak=60.0, step=0.0125)]
    supplement['campaign_snapshot']['cases'][1] = dict(index=1, hs_m=1., tp_s=10, seed=7, status='COMPLETED', run_dir='quarter')
    supplement['counts'] = {'MISSING': 1, 'VERIFIED': 1}
    supplement['cases'][1]['run_dir'] = 'quarter'
    for data in (base, supplement):
        data['campaign_snapshot']['matrix_sha256'] = data['matrix_sha256']
        data['event_audits'] = [dict(index=row['index'], status='VERIFIED', errors=[], channels_verified=len(row['channels']),
                                    trace_sha256=row['trace_sha256'], metadata_sha256=row['metadata_sha256'])
                                for row in data['cases'] if row['status'] == 'VERIFIED']
    paths = {}
    variant = dict(source_master_sha256='M0', master_sha256='M1', seed=None,
                   source_matrix_sha256=base['matrix_sha256'], matrix_sha256=supplement['matrix_sha256'],
                   master_deltas={'General.ImplicitConstantTimeStep': {'before': 0.05, 'after': 0.0125}})
    for name, data in (('base', base), ('supplement', supplement), ('variant', variant)):
        paths[name] = tmp_path / f'{name}.json'
        paths[name].write_text(json.dumps(data))
    return paths


def _sub(paths):
    digest = lambda p: sha256(p.read_bytes()).hexdigest()
    return dict(summary=paths['supplement'], sha256=digest(paths['supplement']), index=1, time_step_s=0.0125, reason='R02',
                variant=paths['variant'], variant_sha256=digest(paths['variant']))


def test_composite_replaces_one_cell_and_records_its_time_step(tmp_path):
    paths = _summaries(tmp_path)
    result = composite.build_composite(paths['base'], sha256(paths['base'].read_bytes()).hexdigest(),
        [_sub(paths)])
    row = result['cases'][1]
    assert row['status'] == 'VERIFIED' and row['solved_time_step_s'] == 0.0125 and row['peak_tension_kN'] == 60.0
    assert result['counts'] == {'VERIFIED': 2}
    assert result['composite']['substitutions'][0]['index'] == 1
    assert result['campaign_snapshot']['cases'][1]['status'] == 'COMPLETED'
    assert result['campaign_snapshot']['cases'][1]['solved_time_step_s'] == 0.0125
    assert result['engineering_acceptance'] == 'NOT EVALUATED'


@pytest.mark.parametrize('defect', ['digest', 'unverified', 'coordinates', 'duplicate', 'basis', 'seed', 'settings', 'step',
                                    'no_variant', 'variant_master', 'variant_delta'])
def test_composite_rejects_unbound_substitution(tmp_path, defect):
    paths = _summaries(tmp_path)
    supplement = json.loads(paths['supplement'].read_text())
    if defect == 'unverified': supplement['cases'][1]['status'] = 'FAILED'
    if defect == 'coordinates': supplement['cases'][1]['tp_s'] = 11
    if defect == 'basis': supplement['design_basis'] = {'mass_t': 6.}
    if defect == 'seed': supplement['cases'][1]['seed'] = 8
    if defect == 'settings': supplement['cases'][1]['settings']['duration_s'] = 900
    if defect == 'step': supplement['cases'][1]['settings']['fixed_time_step_s'] = 0.025
    paths['supplement'].write_text(json.dumps(supplement))
    digest = '0' * 64 if defect == 'digest' else sha256(paths['supplement'].read_bytes()).hexdigest()
    if defect in ('variant_master', 'variant_delta'):
        variant = json.loads(paths['variant'].read_text())
        if defect == 'variant_master': variant['source_master_sha256'] = 'other'
        if defect == 'variant_delta': variant['master_deltas']['General.WaveHs'] = {'before': 1, 'after': 2}
        paths['variant'].write_text(json.dumps(variant))
    subs = [dict(_sub(paths), sha256=digest)]
    if defect == 'no_variant': subs[0].pop('variant')
    if defect == 'duplicate': subs = subs * 2
    with pytest.raises(ValueError):
        composite.build_composite(paths['base'], sha256(paths['base'].read_bytes()).hexdigest(), subs)


def sensitivity():
    return dict(master_link_limit_kN=173.637, engineering_acceptance='NOT EVALUATED',
        time_step={'3': dict(hs_m=3., tp_s=10, master_link_utilization={'0.05': 1.038, '0.025': 1.069},
                             sling3_end_b_peak_kN={'0.05': 52.5, '0.025': 75.9})},
        seeds={'3': dict(hs_m=3., tp_s=10, baseline_utilization_0_05=1.038,
                         seed_utilization_0_025={'1': 1.010, '2': 1.098}, minimum=1.010, maximum=1.098)},
        case_135=dict(hs_m=2.75, tp_s=9, stop_time_s=377.575, time_step_s=0.025, completes_at_time_step_s=0.0125,
                      master_link_mean_kN=53.5, payload_submerged_weight_kN=51.0, slack_fraction_master_link_below_10kN=0.23,
                      trace=dict(time=[375.575, 376.575, 377.575], master_link_kN=[0.5, 140.0, 2.0], sling3_end_b_kN=[0.0, 41.0, 0.1]),
                      physical_expectation='Slack then snap <expected>.', comparator_class='closed-form',
                      verdict='not_implausible', verdict_basis='Cycles between zero and 140 kN.'),
        judgement='Engineering judgement on the step.')


def test_appendix_d_rendered_with_verdict_trace_and_tables():
    html = mudmat.render_html(screening_fixture.summary(), config={}, screening=screening_fixture.payload(),
                              sensitivity=sensitivity())
    appendix = html[html.index('id="appendix-d"'):]
    for text in ('Appendix D', '1.038', '1.069', '75.9', '1.010', '1.098', 'not_implausible', 'closed-form',
                 'Slack then snap &lt;expected&gt;.', '377.575', '0.0125', 'aria-label="Case 135'):
        assert text in appendix


def test_no_sensitivity_no_appendix_d():
    html = mudmat.render_html(screening_fixture.summary(), config={}, screening=screening_fixture.payload())
    assert 'id="appendix-d"' not in html


def test_implausible_verdict_rejected_from_report():
    data = sensitivity(); data['case_135']['verdict'] = 'implausible'
    with pytest.raises(ValueError, match='review page'):
        mudmat.render_html(screening_fixture.summary(), config={}, screening=screening_fixture.payload(), sensitivity=data)


def test_cells_solved_at_other_step_are_flagged():
    data = screening_fixture.summary(); data['cases'][2]['solved_time_step_s'] = 0.0125
    html = mudmat.render_html(data, config={}, screening=screening_fixture.payload())
    assert 'solved at 0.0125 s' in html


def test_pdf_appendix_d_tables():
    data, screen, raw = screening_fixture.pdf_inputs(); stream = BytesIO()
    sens = sensitivity()
    render_full_pdf(data, screen, stream, dict(report_title='Mudmat installation analysis'), summary_bytes=raw,
                    sensitivity=sens, sensitivity_bytes=json.dumps(sens).encode())
    text = ' '.join(' '.join(p.extract_text() for p in PdfReader(stream).pages).split())
    assert 'Appendix D' in text and '1.069' in text and '1.098' in text and 'not_implausible' in text


def test_substituted_cell_is_screened_by_envelope(tmp_path):
    from digitalmodel.workflows.installation_assumed_envelope import build_envelope
    paths = _summaries(tmp_path)
    result = composite.build_composite(paths['base'], sha256(paths['base'].read_bytes()).hexdigest(),
        [_sub(paths)])
    criteria = {'checks': []}
    cells = {c['index']: c for c in build_envelope(result, criteria)['cells']}
    assert cells[1]['reason'] != 'Completed verified case evidence unavailable'


@pytest.mark.parametrize('defect', ['empty', 'mismatch', 'nan', 'order', 'early'])
def test_invalid_trace_rejected(defect):
    data = sensitivity(); trace = data['case_135']['trace']
    if defect == 'empty': trace.update(time=[], master_link_kN=[], sling3_end_b_kN=[])
    if defect == 'mismatch': trace['sling3_end_b_kN'] = [0.0]
    if defect == 'nan': trace['master_link_kN'][1] = float('nan')
    if defect == 'order': trace['time'] = [377.575, 376.575, 375.575]
    if defect == 'early': trace['time'] = [340.0, 341.0, 342.0]
    with pytest.raises(ValueError):
        mudmat.render_html(screening_fixture.summary(), config={}, screening=screening_fixture.payload(), sensitivity=data)


def test_pdf_sensitivity_must_match_pinned_bytes():
    data, screen, raw = screening_fixture.pdf_inputs()
    sens = sensitivity(); sens_raw = json.dumps(sens).encode()
    altered = copy.deepcopy(sens); altered['time_step']['3']['master_link_utilization']['0.025'] = 0.5
    with pytest.raises(ValueError):
        render_full_pdf(data, screen, BytesIO(), {}, summary_bytes=raw, sensitivity=altered, sensitivity_bytes=sens_raw)
    receipt = render_full_pdf(data, screen, BytesIO(), {}, summary_bytes=raw, sensitivity=sens, sensitivity_bytes=sens_raw)
    assert receipt['sensitivity_sha256'] == sha256(sens_raw).hexdigest()
