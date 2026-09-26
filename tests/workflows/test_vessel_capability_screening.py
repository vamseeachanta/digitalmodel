"""Pinned provisional screening and causal forecast evidence in the mudmat report."""
import copy
from hashlib import sha256
from io import BytesIO
import json

import pytest
from pypdf import PdfReader

from digitalmodel.workflows import vessel_capability_layout as layout
from digitalmodel.workflows import vessel_capability_report as mudmat
from digitalmodel.workflows import vessel_capability_snapshot as snapshot
from digitalmodel.workflows.installation_full_report_pdf import render_full_pdf


def summary():
    cases=[dict(index=0,hs_m=1.,tp_s=8,status='VERIFIED',peak_tension_kN=50.),
           dict(index=1,hs_m=1.,tp_s=10,status='FAILED'),
           dict(index=2,hs_m=3.,tp_s=8,status='VERIFIED',peak_tension_kN=180.),
           dict(index=3,hs_m=3.,tp_s=10,status='VERIFIED',peak_tension_kN=90.)]
    for row in cases:
        if row['status']=='VERIFIED':
            row.update(run_dir='run',simulation_sha256='a',trace_sha256='b',
                       channels={'load':dict(position='End A',units='kN',minimum=0.,maximum=row['peak_tension_kN'])})
    return dict(created_utc='2026-01-01T00:00:00Z',counts={'VERIFIED':3,'FAILED':1},cases=cases,
        envelopes=[],critical_periods=[],design_basis={'mass_t':5.},matrix_sha256='a'*64,campaign_sha256='b'*64,
        engineering_acceptance='NOT EVALUATED')


def metrics(ar,persistence,mean):
    return {'autoregression':{'rmse':ar,'bias':0.,'sample_count':240},
            'persistence':{'rmse':persistence,'bias':1.,'sample_count':240},
            'history_mean':{'rmse':mean,'bias':0.,'sample_count':240}}


def payload():
    check=lambda util,status:dict(id='Master-link proxy',status=status,utilization=util,governing_channel='profile_005')
    cases=[dict(index=0,hs_m=1.,tp_s=8,status='WITHIN_ASSUMPTIONS',checks=[check(.3,'PASS')]),
           dict(index=1,hs_m=1.,tp_s=10,status='NOT_EVALUATED',checks=[]),
           dict(index=2,hs_m=3.,tp_s=8,status='EXCEEDS_ASSUMPTIONS',checks=[check(1.043,'FAIL')]),
           dict(index=3,hs_m=3.,tp_s=10,status='WITHIN_ASSUMPTIONS',checks=[check(.6,'PASS')])]
    load=dict(id='profile_005',label='Master-link proxy',units='kN',assumed_limit=173.637,
              history=dict(times=[240,360],values=[1,2]),forecast=dict(times=[361,480],values=[2,1]),
              fit_status='fitted',method='History-only ridge autoregression; fixed order 40, ridge 0.001',training_end_s=360,
              metrics=metrics(28.022,55.697,28.106))
    sling=dict(load,id='profile_007',label='Sling2 End B',metrics=metrics(8.553,16.821,8.448))
    frames=[dict(now_s=360,forecast_horizon_s=120,operational_validation='NOT ESTABLISHED',channels=[load,sling])]
    return dict(title='Mudmat installation provisional envelope',created_utc='2026-01-02T00:00:00Z',
        engineering_acceptance='NOT EVALUATED',
        criteria=[dict(id='Master-link proxy',label='Master-link proxy',limit=17.7,units='Te',status='project_assumption')],
        cases=cases,boundaries=[dict(tp_s=8,highest_contiguous_pass_hs_m=1.,first_nonpass_hs_m=3.,
            first_nonpass_status='FAIL',upper_edge_censored=False,nonmonotonic_observed=False)],
        demo=dict(default_mode='history_only',scenarios=[dict(case_index=0,hs_m=1.,tp_s=8,
            source_label='SIMULATED replay',frames=frames)]),
        limitations=['SIMULATED irregular-wave replay; no offshore sensor connection.'],
        provenance={},snapshot=dict(hs_m=1.,tp_s=8,now_s=360))


def test_screening_replaces_placeholders_with_bounded_results():
    data,screen=summary(),payload();before=(copy.deepcopy(data),copy.deepcopy(screen))
    html=mudmat.render_html(data,config={},screening=screen)
    assert (data,screen)==before
    assert 'Not established.</strong> A future Hs' not in html
    assert 'Mudmat forecast validation remains pending' not in html
    for text in ('WITHIN_ASSUMPTIONS: 2','EXCEEDS_ASSUMPTIONS: 1','NOT_EVALUATED: 1','1.043',
                 'project assumption','NOT EVALUATED','28.022','28.106','55.697','8.553','8.448',
                 'lower than the history-mean baseline in 1 of 2','not an approved operating limit'):
        assert text in html


def test_placeholders_retained_without_screening():
    html=mudmat.render_html(summary(),config={})
    assert 'Not established.</strong>' in html and 'forecast validation remains pending' in html


@pytest.mark.parametrize('defect',['coverage','coordinates','unverified','acceptance','noncausal'])
def test_unbound_screening_rejected(defect):
    screen=payload()
    if defect=='coverage':screen['cases'].pop()
    if defect=='coordinates':screen['cases'][0]['tp_s']=9
    if defect=='unverified':screen['cases'][1]['status']='WITHIN_ASSUMPTIONS'
    if defect=='acceptance':screen['engineering_acceptance']='ACCEPTED'
    if defect=='noncausal':screen['demo']['default_mode']='wave_preview'
    with pytest.raises(ValueError):mudmat.render_html(summary(),config={},screening=screen)


def pinned(tmp_path,bind=True):
    data=summary()
    for row in data['cases']:
        if 'run_dir' in row:row['run_dir']=str(tmp_path/'run')
    source=tmp_path/'source.json';source.write_text(json.dumps(data))
    source_sha=sha256(source.read_bytes()).hexdigest()
    screen=payload()
    screen['provenance']['summary']=dict(filename='source.json',sha256=source_sha if bind else '0'*64)
    screening=tmp_path/'payload.json';screening.write_text(json.dumps(screen))
    config=tmp_path/'config.json';config.write_text(json.dumps({'revision':'R6'}))
    return dict(source=source,source_sha256=source_sha,config=config,
        config_sha256=sha256(config.read_bytes()).hexdigest(),output=tmp_path/'report.html',
        screening=screening,screening_sha256=sha256(screening.read_bytes()).hexdigest())


def test_snapshot_pins_screening_payload(tmp_path):
    args=pinned(tmp_path)
    record=snapshot.render_snapshot(**args)
    assert record['screening']['sha256']==args['screening_sha256']
    assert 'vessel_capability_layout' in record['dependency_sha256']
    assert 'EXCEEDS_ASSUMPTIONS: 1' in args['output'].read_text(encoding='utf-8')


@pytest.mark.parametrize('defect',['digest','summary_binding','missing_digest'])
def test_snapshot_rejects_unpinned_screening_before_output(tmp_path,defect):
    args=pinned(tmp_path,bind=defect!='summary_binding')
    if defect=='digest':args['screening_sha256']='0'*64
    if defect=='missing_digest':args['screening_sha256']=None
    with pytest.raises(ValueError):snapshot.render_snapshot(**args)
    assert not args['output'].exists()


def pdf_inputs():
    data,screen=summary(),payload()
    for row in data['cases']:
        row.setdefault('settings',{});row.setdefault('peak_tension_kN',None)
    raw=json.dumps(data).encode()
    screen['provenance']['summary']=dict(sha256=sha256(raw).hexdigest())
    return data,screen,raw


def test_full_pdf_uses_configured_structure_and_payload_reference():
    data,screen,raw=pdf_inputs();stream=BytesIO()
    config=dict(report_title='Mudmat installation analysis',revision='R6',
                structure_description='suspended mudmat and its rigging')
    receipt=render_full_pdf(data,screen,stream,config,summary_bytes=raw)
    text=' '.join(' '.join(p.extract_text() for p in PdfReader(stream).pages).split())
    assert 'jumper' not in text.lower()
    assert 'Mudmat installation analysis | R6' in text and 'suspended mudmat and its rigging' in text
    assert receipt['snapshot']['case_index']==0 and receipt['snapshot']['now_s']==360


@pytest.mark.parametrize('defect',['boundary_on_failed','scenario_failed_case','training_after_now','horizon_beyond'])
def test_screening_evidence_reconciled_with_cases(defect):
    screen=payload()
    channels=screen['demo']['scenarios'][0]['frames'][0]['channels']
    if defect=='boundary_on_failed':
        screen['boundaries']=[dict(tp_s=10,highest_contiguous_pass_hs_m=3.,first_nonpass_hs_m=None,first_nonpass_status=None)]
    if defect=='scenario_failed_case':screen['demo']['scenarios'][0].update(case_index=1,hs_m=1.,tp_s=10)
    if defect=='training_after_now':channels[0]['training_end_s']=480
    if defect=='horizon_beyond':channels[0]['forecast']['times'][-1]=481
    with pytest.raises(ValueError):mudmat.render_html(summary(),config={},screening=screen)


def test_boundaries_recomputed_from_bound_cases():
    screen=payload();screen['boundaries']=[]
    html=mudmat.render_html(summary(),config={},screening=screen)
    assert '<td>8</td><td>1</td><td>3</td><td>EXCEEDS_ASSUMPTIONS</td>' in html
    assert '<td>10</td><td>-</td><td>1</td><td>NOT_EVALUATED</td>' in html


@pytest.mark.parametrize('defect',['zero_mean','missing_baseline','nonfinite'])
def test_forecast_metrics_validated(defect):
    screen=payload();metrics_=screen['demo']['scenarios'][0]['frames'][0]['channels'][0]['metrics']
    if defect=='zero_mean':
        metrics_['history_mean']['rmse']=0.
        html=mudmat.render_html(summary(),config={},screening=screen)
        assert 'undefined' in html and 'in 0 of 1' in html
        return
    if defect=='missing_baseline':metrics_.pop('persistence')
    if defect=='nonfinite':metrics_['autoregression']['rmse']=float('nan')
    with pytest.raises(ValueError):mudmat.render_html(summary(),config={},screening=screen)


def test_late_screening_change_publishes_no_final_output(tmp_path,monkeypatch):
    args=pinned(tmp_path);original=snapshot.render_html
    def tamper(*a,**k):
        value=original(*a,**k);args['screening'].write_text('{}');return value
    monkeypatch.setattr(snapshot,'render_html',tamper)
    with pytest.raises(ValueError):snapshot.render_snapshot(**args)
    assert not args['output'].exists() and not args['output'].with_suffix('.json').exists()


def test_full_pdf_history_only_method_does_not_claim_supplied_future_wave():
    data,screen,raw=pdf_inputs();stream=BytesIO()
    render_full_pdf(data,screen,stream,dict(report_title='Mudmat installation analysis'),summary_bytes=raw)
    text=' '.join(' '.join(p.extract_text() for p in PdfReader(stream).pages).split())
    assert 'simulated future wave trace is supplied input' not in text
    assert 'supplies a simulated future irregular-wave record' not in text
    assert 'only samples available at NOW' in text


def test_boundary_flags_follow_producer_for_coverage_gaps():
    from digitalmodel.workflows.installation_assumed_envelope import _boundary
    screen=payload();screen['boundaries']=[]
    cells=[dict(tp_s=10,hs_m=1.,status='NOT_EVALUATED'),dict(tp_s=10,hs_m=3.,status='PASS')]
    expected=_boundary(10,cells)
    assert expected['upper_edge_censored'] and not expected['nonmonotonic_observed']
    html=mudmat.render_html(summary(),config={},screening=screen)
    assert '<td>10</td><td>-</td><td>1</td><td>NOT_EVALUATED</td><td>yes</td><td>no</td>' in html
    screen['boundaries']=[dict(expected,nonmonotonic_observed=True)]
    with pytest.raises(ValueError):mudmat.render_html(summary(),config={},screening=screen)


def test_scored_channel_without_forecast_times_rejected():
    screen=payload();channel=screen['demo']['scenarios'][0]['frames'][0]['channels'][0]
    channel.pop('forecast');channel['training_end_s']=480
    with pytest.raises(ValueError):mudmat.render_html(summary(),config={},screening=screen)


def test_full_pdf_configuration_uses_verified_rows_only():
    data,screen=summary(),payload()
    for row in data['cases']:
        if row['status']=='VERIFIED':row.update(settings=dict(duration_s=600),heading_degrees=165)
    raw=json.dumps(data).encode()
    screen['provenance']['summary']=dict(sha256=sha256(raw).hexdigest())
    stream=BytesIO()
    render_full_pdf(data,screen,stream,dict(report_title='Mudmat installation analysis'),summary_bytes=raw)
    text=' '.join(' '.join(p.extract_text() for p in PdfReader(stream).pages).split())
    assert 'Heading 165 deg' in text and 'CASE-001' in text


def test_full_pdf_accepts_plain_string_references():
    data,screen,raw=pdf_inputs();stream=BytesIO()
    render_full_pdf(data,screen,stream,dict(report_title='Mudmat installation analysis',
        references=['Baseline audit: audit.json; SHA-256 abc']),summary_bytes=raw)
    text=' '.join(' '.join(p.extract_text() for p in PdfReader(stream).pages).split())
    assert 'Baseline audit: audit.json; SHA-256 abc' in text


def test_full_pdf_carries_configured_findings_and_supplements():
    data,screen,raw=pdf_inputs();stream=BytesIO()
    config=dict(report_title='Mudmat installation analysis',summary_findings=['Seven cells stopped unstable <x>'],
                supplements=['Added-mass bracket factor 1.5 governs'],decisions=['Causal demonstration only'])
    render_full_pdf(data,screen,stream,config,summary_bytes=raw)
    text=' '.join(' '.join(p.extract_text() for p in PdfReader(stream).pages).split())
    for phrase in ('Seven cells stopped unstable <x>','Added-mass bracket factor 1.5 governs','Causal demonstration only'):
        assert phrase in text


def _table2_text(statuses_headings):
    data,screen=summary(),payload()
    for row,(status,heading) in zip(data['cases'],statuses_headings):
        row['status']=status
        if heading is not None:row.update(settings=dict(duration_s=600),heading_degrees=heading)
        else:row.pop('settings',None);row.pop('heading_degrees',None)
    for case in screen['cases']:case.update(status='NOT_EVALUATED',checks=[])
    screen['demo']['scenarios']=[];screen['boundaries']=[]
    raw=json.dumps(data).encode();screen['provenance']['summary']=dict(sha256=sha256(raw).hexdigest())
    stream=BytesIO()
    from digitalmodel.workflows import installation_full_report_pdf as pdf
    story=[];pdf._design(story,data,dict(screen,_report_config={}),pdf._mapped_cases(data,screen))
    return ' '.join(' '.join(getattr(item,'text','') for item in _flatten(story)).split())


def _flatten(items):
    for item in items:
        if hasattr(item,'_content'):yield from _flatten(item._content)
        elif hasattr(item,'_cellvalues'):
            for row in item._cellvalues:yield from _flatten(row)
        else:yield item


def test_table2_discloses_verified_scope_and_excluded_differences():
    text=_table2_text([('VERIFIED',180),('FAILED',165),('VERIFIED',180),('MISSING',None)])
    assert 'Verified source cases (2 of 4)' in text
    assert 'Excluded non-verified rows' in text and '165' in text


def test_table2_zero_verified_scope_is_explicit():
    text=_table2_text([('FAILED',165),('FAILED',165),('MISSING',None),('MISSING',None)])
    assert 'No verified source cases; all 4 rows shown' in text


def test_replay_qualifies_inherited_campaign_findings():
    from digitalmodel.workflows.installation_replay_report import _qualify_inherited
    config=_qualify_inherited(dict(summary_findings=['Peak 151 kN'],decisions=['D'],supplements=['S'],disclosures=['X']))
    prefix='Historical full-campaign context; not fresh pilot findings: '
    assert config['summary_findings']==[prefix+'Peak 151 kN'] and config['decisions']==[prefix+'D']
    assert config['supplements']==[prefix+'S'] and config['disclosures']==[prefix+'X']


def _with_alerts(screen, outcome_override=None):
    scenario = screen['demo']['scenarios'][0]
    scenario['selection_basis'] = 'Selected after inspection because the window contains an exceedance'
    load = scenario['frames'][0]['channels'][0]
    load['truth'] = dict(times=[361, 480], values=[170.0, 180.0])
    load['exceedance'] = dict(status='calibrated', limit=173.637, window_probability=0.12, alert=False, alert_probability=0.2,
                              observed_exceedance=True, outcome=outcome_override or 'miss',
                              calibration_windows=25, first_band_crossing_s=None,
                              scoring='withheld truth used only for post-hoc scoring')
    load['wave_preview'] = dict(times=[361, 480], values=[175.0, 150.0])
    load['wave_preview_metrics'] = {'oracle_wave_fir': {'rmse': 5.5}, 'autoregression': {'rmse': 28.022},
                                    'persistence': {'rmse': 55.7}, 'history_mean': {'rmse': 28.106}}
    return screen


def test_alert_scoring_and_conditional_benchmark_rendered():
    html = mudmat.render_html(summary(), config={}, screening=_with_alerts(payload()))
    for text in ('Selected after inspection', '0.120', 'miss', 'hits 0, misses 1, false alarms 0, correct negatives 0',
                 'conditional', 'supplied future waves', '5.500', 'crosses'):
        assert text in html


@pytest.mark.parametrize('defect', ['outcome', 'observed', 'probability'])
def test_inconsistent_alert_record_rejected(defect):
    screen = _with_alerts(payload(), 'hit' if defect == 'outcome' else None)
    alert = screen['demo']['scenarios'][0]['frames'][0]['channels'][0]['exceedance']
    if defect == 'observed': alert['observed_exceedance'] = False; alert['outcome'] = 'correct_negative'
    if defect == 'probability': alert['window_probability'] = 1.5
    with pytest.raises(ValueError):
        mudmat.render_html(summary(), config={}, screening=screen)


def test_main_body_operating_envelope_and_allowable_table():
    html = mudmat.render_html(summary(), config={}, screening=payload())
    assert 'aria-label="Hs–Tp operating envelope' in html and 'Figure 5-1' in html
    assert 'Allowable Hs (m) by Tp' in html
    body = html[:html.index('id="appendix-a"')]
    assert 'Combined provisional criteria' in body and 'Master-link proxy' in body
    assert 'conditional' in body.lower()


def test_appendix_per_cell_verdicts_are_conditional_and_complete():
    html = mudmat.render_html(summary(), config={}, screening=payload())
    appendix = html[html.index('id="appendix-c"'):]
    assert appendix.count('<tr>') - 1 == 4
    assert 'Acceptable against Master-link proxy' in appendix
    assert 'Not acceptable against Master-link proxy' in appendix
    assert 'Not evaluated' in appendix
    assert 'conditional on confirmation of component capacities' in appendix
    assert 'id="appendix-c"' in html and 'href="#appendix-c"' in html or 'Appendix C' in html


def test_no_screening_means_no_verdicts():
    html = mudmat.render_html(summary(), config={})
    assert 'Acceptable against' not in html and 'id="appendix-c"' not in html


def test_allowable_rows_shared_by_html_and_pdf():
    rows = layout.allowable_rows(payload()['cases'], payload()['criteria'])
    assert rows[0][0] == 'Combined provisional criteria' and rows[1][0] == 'Master-link proxy'
    assert rows[0][1:] == ['1', 'none']
    data, screen, raw = pdf_inputs(); stream = BytesIO()
    render_full_pdf(data, screen, stream, dict(report_title='Mudmat installation analysis',
                    recommendations=['Extended-period RAOs from additional diffraction analysis']), summary_bytes=raw)
    text = ' '.join(' '.join(p.extract_text() for p in PdfReader(stream).pages).split())
    assert 'Allowable Hs (m) by Tp' in text and 'Extended-period RAOs from additional diffraction analysis' in text


def test_configured_recommendations_in_html_section_7():
    html = mudmat.render_html(summary(), config={'recommendations': ['Extended-period RAOs <x>']}, screening=payload())
    section = html[html.index('id="section-7"'):html.index('id="section-8"')]
    assert 'Extended-period RAOs &lt;x&gt;' in section


@pytest.mark.parametrize('defect', ['threshold', 'empty_times', 'truncated', 'misaligned'])
def test_alert_decision_and_truth_coverage_validated(defect):
    screen = _with_alerts(payload())
    load = screen['demo']['scenarios'][0]['frames'][0]['channels'][0]
    alert = load['exceedance']; alert['alert_probability'] = 0.2
    if defect == 'threshold': alert['window_probability'] = 0.9
    if defect == 'empty_times': load['truth']['times'] = []
    if defect == 'truncated': load['truth'] = dict(times=[361], values=[170.0])
    if defect == 'misaligned': load['truth']['times'] = [362, 480]
    with pytest.raises(ValueError):
        mudmat.render_html(summary(), config={}, screening=screen)


def test_criterion_status_independent_of_combined_not_evaluated():
    case = dict(index=9, hs_m=1., tp_s=8, status='NOT_EVALUATED',
                checks=[dict(id='A', status='PASS', utilization=.5)])
    assert layout._criterion_status(case, 'A') == 'WITHIN_ASSUMPTIONS'
    assert layout._criterion_status(case, 'B') == 'NOT_EVALUATED'
