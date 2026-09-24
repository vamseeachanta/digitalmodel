"""Engineering presentation of an unchanged partial structure-demand snapshot."""
from html import escape
import json
import math
import re

from digitalmodel.workflows.installation_partial_report import STYLE, _table, _grid, _envelope_table, _case_details
from digitalmodel.workflows.installation_report_layout import report_cover


def _section(number,title,content):
    return f'<section id="section-{number}"><h2>{number} {title}</h2>{content}</section>'


def _metadata(config,key):
    items=config.get(key,[])
    if not isinstance(items,list) or any(not isinstance(item,str) for item in items):
        raise ValueError('Report contextual metadata requires lists of plain strings')
    return ''.join('<p>'+escape(item)+'</p>' for item in items)


def _design(summary,config):
    keys=('parameter','value','unit','source','status','effect_if_changed')
    rows=config.get('design_data')
    if rows is None:
        rows=[dict(parameter=k,value=v,source='Retained source snapshot',status='Recorded basis')
              for k,v in summary['design_basis'].items()]
    else:
        if not isinstance(rows,list) or any(not isinstance(row,dict) or not set(keys)<=set(row) for row in rows):
            raise ValueError('Design data requires dictionaries with six required fields')
        for row in rows:
            if 'basis_key' in row:
                key=row['basis_key']
                if key not in summary['design_basis'] or str(row['value'])!=str(summary['design_basis'][key]):
                    raise ValueError('Configured design value differs from linked source basis')
    table=_table(['Parameter','Value','Unit','Source','Status','Effect if changed'],
        [[escape(str(row.get(k,'Not recorded'))) for k in keys] for row in rows])
    return _section(3,'Design data and assumed criteria',table+
        '<p class="caption">Table 3-1. Recorded design basis; missing fields remain explicit.</p>'
        '<p>Source-linked values are checked against the retained design basis. Other entries are report-author interpretations; '
        'their sources and qualification status are shown in the table.</p>'
        '<p>Approved capacities and slack/re-tension/interference criteria are not established by this snapshot. '
        'No implicit allowable-compression criterion is introduced.</p>')


def _intro_summary(summary,config):
    counts=', '.join(f'{key}: {value}' for key,value in summary['counts'].items())
    intro='<p>The assessment addresses vessel-specific installation demand for structures of varying sizes. '
    intro+='This partial issue presents the retained baseline deep-submerged irregular-wave responses. '
    intro+='Execution coverage is separate from engineering acceptance.</p>'
    findings='<p>'+escape(counts)+'</p><p class="notice"><strong>No operating window has been established.</strong> '
    findings+='Load and low-tension maxima govern only verified sampled cases. Component capacities, geometric clearance '
    findings+='and slack/re-tension criteria remain unresolved.</p><p>Negative signed tension can reflect the model line '
    findings+='properties during unloading. It is not a physically sustainable sling compression load. '
    findings+='Tension ≤0 is a diagnostic, not evidence of an allowable physical slack distance.</p>'
    findings+=_metadata(config,'summary_findings')
    return _section(1,'Introduction',intro)+_section(2,'Summary and conclusions',findings)


def _method(summary):
    diagram=_workflow()
    audit=('The source audit records completed generation, model, simulation and trace checks and recomputed tension-event durations. '
        if summary.get('event_audits') else 'Verification by a source event audit is not established in the supplied snapshot. ')
    return _section(4,'Analysis methodology','<p>The retained source report contains the campaign snapshot and numerical demand. '+audit+
        'This presentation reuses that snapshot and does not repeat the audit or invoke a solver.</p>'+diagram+
        '<p class="caption">Figure 4-1. Evidence workflow; qualification remains separate from recorded demand.</p>'
        '<p>The imported vessel RAO condition, heading and one wave seed define the represented scope. '
        'The source records wave components below the shortest displacement-RAO period; extrapolation requires assessment. '
        'Numerical convergence and repeated-seed evidence remain qualification requirements.</p>')


def _workflow():
    steps=[('Pinned inputs','Master + sea states'),('Native responses','Simulation + receipts'),
           ('Verified demand','Traces + components'),('Qualification pending','Criteria + model checks')]
    nodes=[]
    for index,(title,label) in enumerate(steps):
        x=10+index*250
        nodes.append(f'<rect x="{x}" y="20" width="225" height="85" rx="8" fill="#edf4f8" stroke="#536c7e"/>'
            f'<text x="{x+112}" y="50" text-anchor="middle" style="font-size:15px">{escape(title)}</text>'
            f'<text x="{x+112}" y="78" text-anchor="middle" style="font-size:13px">{escape(label)}</text>')
        if index<3:nodes.append(f'<path d="M{x+225} 63h23l-6 -5m6 5l-6 5" fill="none" stroke="#536c7e"/>')
    return '<svg viewBox="0 0 1000 125" role="img" aria-label="Pinned inputs to pending engineering qualification" style="width:100%;max-width:none;height:auto">'+''.join(nodes)+'</svg>'


_PRODUCER_STATUS={'WITHIN_ASSUMPTIONS':'PASS','EXCEEDS_ASSUMPTIONS':'FAIL','NOT_EVALUATED':'NOT_EVALUATED'}
_BOUNDARY_KEYS=('highest_contiguous_pass_hs_m','first_nonpass_hs_m','first_nonpass_status',
                'upper_edge_censored','contiguous_upper_edge_censored','nonmonotonic_observed')


def _boundaries(cases):
    """Sampled per-Tp boundaries recomputed with the producer's rule; no interpolation."""
    from digitalmodel.workflows.installation_assumed_envelope import _boundary
    cells=[dict(tp_s=case['tp_s'],hs_m=case['hs_m'],status=_PRODUCER_STATUS[case['status']]) for case in cases]
    return [_boundary(period,cells) for period in sorted({cell['tp_s'] for cell in cells})]

def _finite(value):
    return isinstance(value,(int,float)) and not isinstance(value,bool) and math.isfinite(value)


def _bind_forecast(screened,screening):
    for scenario in screening['demo']['scenarios']:
        case=screened.get(scenario['case_index'])
        if case is None or case['status']=='NOT_EVALUATED' or any(case[k]!=scenario[k] for k in ('hs_m','tp_s')):
            raise ValueError('Forecast scenario must reference a verified screened case')
        for frame in scenario['frames']:
            now,horizon=frame['now_s'],frame.get('forecast_horizon_s')
            if horizon!=120:raise ValueError('Forecast horizon must be 120 s')
            for channel in frame['channels']:
                if any(t>now for t in channel.get('history',{}).get('times',[])):
                    raise ValueError('History samples after NOW')
                times=channel.get('forecast',{}).get('times',[])
                metrics=channel.get('metrics')
                if (times or metrics is not None) and (not times or not _finite(channel.get('training_end_s'))
                        or channel['training_end_s']>now or any(not now<t<=now+horizon for t in times)):
                    raise ValueError('Forecast is not causal within the 120 s horizon')
                if metrics is not None and not all(_finite(metrics.get(k,{}).get('rmse'))
                        for k in ('autoregression','persistence','history_mean')):
                    raise ValueError('Forecast metrics require finite RMSE for all comparators')


def bind_screening(summary,screening):
    """Reject screening evidence that is not case-for-case bound to the source snapshot."""
    if screening.get('engineering_acceptance')!='NOT EVALUATED':
        raise ValueError('Screening payload must retain engineering acceptance NOT EVALUATED')
    if screening.get('demo',{}).get('default_mode')!='history_only':
        raise ValueError('Screening forecast must be causal history-only')
    source={row['index']:row for row in summary['cases']};screened={row['index']:row for row in screening['cases']}
    if len(source)!=len(summary['cases']) or len(screened)!=len(screening['cases']) or not source or set(source)!=set(screened):
        raise ValueError('Screening and source case coverage differ')
    for index,case in screened.items():
        if any(case[key]!=source[index][key] for key in ('hs_m','tp_s')):
            raise ValueError('Screening and source case coordinates differ')
        if case['status']!='NOT_EVALUATED' and source[index]['status']!='VERIFIED':
            raise ValueError('Screened classification requires a verified source case')
    cases=[screened[index] for index in sorted(screened)]
    recomputed={row['tp_s']:row for row in _boundaries(cases)}
    for row in screening.get('boundaries',[]):
        expected=recomputed.get(row['tp_s'])
        if expected is None or any(row.get(k,expected[k])!=expected[k] for k in _BOUNDARY_KEYS):
            raise ValueError('Screening boundary differs from bound case classifications')
    _bind_forecast(screened,screening)
    return cases


def _screening_envelope(cases,screening):
    counts={}
    for case in cases:counts[case['status']]=counts.get(case['status'],0)+1
    text='<p><strong>Provisional screen only; not an approved operating limit.</strong> Each verified case is compared with '
    text+='project assumption endpoint limits. Engineering acceptance: NOT EVALUATED. Numerical failures and unrun cases are NOT_EVALUATED, not exceedances.</p>'
    text+='<p>'+escape(', '.join(f'{key}: {value}' for key,value in sorted(counts.items())))+'</p>'
    text+=_table(['Criterion','Limit','Units','Status'],[[escape(str(c.get('label',c['id']))),escape(str(c['limit'])),
        escape(str(c['units'])),'project assumption'] for c in screening['criteria']])
    text+='<p class="caption">Table 5-5. Provisional endpoint criteria from the recorded project assumption basis.</p>'
    rows=[]
    for case in cases:
        if case['status']!='EXCEEDS_ASSUMPTIONS':continue
        for check in case['checks']:
            if check.get('status')=='FAIL':
                rows.append([f"{case['index']:03d}",f"{case['hs_m']:g}",f"{case['tp_s']:g}",escape(str(check['id'])),
                             f"{check['utilization']:.3f}",escape(str(check.get('governing_channel','Not recorded')))])
    text+=_table(['Case','Hs (m)','Tp (s)','Assumed criterion','Utilization (-)','Governing channel'],rows or [['-']*6])
    text+='<p class="caption">Table 5-6. Cases exceeding a provisional criterion; unity is the screening threshold.</p>'
    fmt=lambda value:'-' if value is None else f'{value:g}'
    label={value:key for key,value in _PRODUCER_STATUS.items()}
    text+=_table(['Tp (s)','Highest contiguous pass Hs (m)','First non-pass Hs (m)','First non-pass state','Highest sampled Hs passes','Pass above observed exceedance'],
        [[fmt(b['tp_s']),fmt(b['highest_contiguous_pass_hs_m']),fmt(b['first_nonpass_hs_m']),
          escape(label.get(b['first_nonpass_status'],'-')),'yes' if b['upper_edge_censored'] else 'no',
          'yes' if b['nonmonotonic_observed'] else 'no'] for b in _boundaries(cases)])
    text+='<p class="caption">Table 5-7. Sampled boundary per Tp, recomputed with the screening rule from the bound case classifications. A NOT_EVALUATED first non-pass state is a coverage gap, not a limit; '
    text+='a column whose highest sampled Hs passes is censored by the study range. No interpolation or extrapolation is applied.</p>'
    return text


def _screening_forecast(screening):
    rows,better,total,undefined=[],0,0,0
    for scenario in screening['demo']['scenarios']:
        for frame in scenario['frames']:
            for channel in frame['channels']:
                m=channel.get('metrics')
                if not m:continue
                ar,pers,mean=(m[k]['rmse'] for k in ('autoregression','persistence','history_mean'))
                ratio='undefined (zero baseline)' if mean==0 else f'{ar/mean:.3f}'
                if channel['id']!='wave_elevation':
                    if mean==0:undefined+=1
                    else:total+=1;better+=ar<mean
                rows.append([f"{frame['now_s']:g}",escape(str(channel.get('label',channel['id']))),escape(str(channel['units'])),
                             f'{ar:.3f}',f'{pers:.3f}',f'{mean:.3f}',ratio])
    scenario=screening['demo']['scenarios'][0]
    text=f"<p>The causal demonstration replays the SIMULATED reference case {scenario['case_index']:03d} (Hs {scenario['hs_m']:g} m, Tp {scenario['tp_s']:g} s). "
    text+='At each origin a history-only autoregression predicts the next 120 s using samples at or before NOW; withheld samples are used only for scoring. '
    text+=f'For load channels, autoregression RMSE is lower than the history-mean baseline in {better} of {total} channel-origin pairs'
    text+=f' ({undefined} pairs with a zero baseline error are undefined). ' if undefined else '. '
    text+='Forecast skill over naive baselines is therefore reported as measured, not assumed. Offshore forecast validation is not established.</p>'
    text+=_table(['NOW (s)','Channel','Units','Autoregression RMSE','Persistence RMSE','History-mean RMSE','RMSE ratio to history mean (-)'],rows)
    text+='<p class="caption">Table 5-8. Held-out 120 s forecast errors at the preselected origins. A ratio below 1.000 indicates lower error than the history-mean baseline.</p>'
    text+=''.join('<p>'+escape(str(item))+'</p>' for item in screening.get('limitations',[]))
    return text


def _results(summary,screening=None):
    from digitalmodel.workflows.vessel_capability_report import _critical_table
    text='<h3>5.1 Execution coverage and sampled periods</h3>'+_grid(summary['cases'])
    text+='<p class="caption">Table 5-1. Coverage at the source snapshot: ● verified, ◐ running, — missing, ! other state. No acceptance verdict is implied.</p>'
    text+=_critical_table(summary['critical_periods'])
    text+='<p class="caption">Table 5-2. Governing sampled periods. Duration is accumulated tension ≤0 time over the recorded analysis interval, '
    text+='not the longest continuous event. Different components can govern duration and load; incomplete rows cannot establish a full-range critical period.</p>'
    text+='<h3>5.2 Component and body response</h3>'+_envelope_table(summary['envelopes'],True)
    text+='<p class="caption">Table 5-3. Signed line-end extrema and maximum accumulated tension ≤0 duration per component, with governing cases.</p>'
    text+=_envelope_table(summary['envelopes'],False)
    text+='<p class="caption">Table 5-4. Native body and winch quantities. Body reference-point Z is not clearance of the lowest rotated point.</p>'
    text+='<h3>5.3 Unloading and re-tension diagnostics</h3><p>Accumulated duration and longest continuous events are distinct quantities. '
    text+='Endpoint chord deficit is unstretched length minus endpoint separation; sag and extension contribute, so it is not physical slack. '
    text+='Post-exit tension peaks do not alone establish physical snap loads or an allowable slack distance.</p>'
    if screening is not None:
        cases=bind_screening(summary,screening)
        text+='<h3>5.4 Provisional installation envelope</h3>'+_screening_envelope(cases,screening)
        text+='<h3>5.5 Two-minute forecasting</h3>'+_screening_forecast(screening)
        return _section(5,'Results — conditional screening',text)
    text+='<h3>5.4 Provisional installation envelope</h3><p><strong>Not established.</strong> A future Hs–Tp envelope requires declared criteria, '
    text+='matched component checks, numerical/model qualification and documented boundaries. No boundary is inferred from execution coverage.</p>'
    text+='<h3>5.5 Two-minute forecasting</h3><p>Mudmat forecast validation remains pending. The intended 120 s demonstration will separate '
    text+='history from prediction with a clear NOW line and quantify held-out performance against naive baselines. '
    text+='A causal prediction uses history available at the forecast origin; supplied future-wave input, if used, will be labelled conditional.</p>'
    return _section(5,'Results — conditional screening',text)


def _validation_conclusions(summary,config,screened=False):
    from digitalmodel.workflows.vessel_capability_report import _pending
    text=_pending(summary['design_basis'],summary.get('campaign_snapshot'),summary.get('sensitivity_campaign_snapshot'),screened)
    text+='<p class="caption">Table 6-1. Evidence status at the retained source snapshot.</p>'
    notes=_metadata(config,'decisions')+_metadata(config,'supplements')
    if notes:text+='<h3>6.1 Subsequent review context</h3><p>The supplied notes do not update source coverage or constitute completed qualification.</p>'+notes
    conclusions='<p>The retained cases support component-demand screening for the recorded arrangement. '
    conclusions+='They do not establish an approved installation envelope. Completion of source-supported size selection, '
    conclusions+='hydrodynamic/numerical checks, RAO assessment, capacity/clearance criteria and statistical coverage is recommended '
    conclusions+='before operating-window qualification. Mudmat forecasting requires a separate validated demonstration.</p>'
    return _section(6,'Validation status',text)+_section(7,'Conclusions and recommendations',conclusions)


def _appendices(summary,base,config):
    revision=_table(['Revision','Description'],[[escape(str(config.get('revision','Not recorded'))),
        escape(str(config.get('revision_description','Presentation of retained source snapshot'))) ]])
    text=_section(8,'References and revision history',_metadata(config,'references')+revision+
        '<p class="caption">Table 8-1. Current issue; no external approval history is inferred.</p>')
    details=[]
    for case in summary['cases']:
        if case['status']!='VERIFIED':continue
        detail=_case_details([case],base)
        if detail.count('</table>')!=1:raise ValueError('One detailed channel table required per verified case')
        detail=detail.replace('</table>',f'</table><p class="caption">Table A-{case["index"]+1:03d}. Detailed native channel results.</p>',1)
        details.append(detail)
    text+='<section id="appendix-a"><h2>Appendix A Detailed results</h2>'
    text+=f'<p>{len(details)} verified cases; {len(summary["cases"])-len(details)} excluded cases. Only VERIFIED cases receive detailed tables; excluded states remain in execution coverage.</p>'
    text+=''.join(details)+'</section>'
    provenance={k:summary[k] for k in ('created_utc','matrix_sha256','campaign_sha256','design_basis')}
    text+='<section id="appendix-b"><h2>Appendix B Provenance and unresolved checks</h2><p>This raw basis is frozen historical source context, not current status. The pinned source JSON retains numerical results and '
    text+='original basis. A presentation rerender does not extend source verification or update campaign status.</p><pre>'
    return text+escape(json.dumps(provenance,indent=2,allow_nan=False))+'</pre></section>'


def render_layout(summary,base,config,screening=None):
    config=dict(config)
    config.setdefault('title','Vessel capability for mudmat installation')
    config.setdefault('document_id','Structure installation engineering assessment')
    config.setdefault('revision','Not recorded')
    config.setdefault('subtitle','Retained simulated irregular-wave demand; qualification and forecasting pending')
    created=config.get('rendered_utc',summary['created_utc'])
    cover=(report_cover(config,created)+'<section><p>Source snapshot UTC: '+escape(summary['created_utc'])+
        '</p><p>Presentation generated UTC: '+escape(str(created))+'</p></section>')
    content=cover+_intro_summary(summary,config)+_design(summary,config)+_method(summary)+_results(summary,screening)
    content+=_validation_conclusions(summary,config,screening is not None)+_appendices(summary,base,config)
    text='<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">'
    text+='<title>'+escape(config['title'])+'</title><style>'+STYLE
    text+='pre{white-space:pre-wrap;overflow-wrap:anywhere}p,td,a{overflow-wrap:anywhere}</style></head><body><main>'+content
    text+='<footer>Private engineering review record. Operational acceptance: NOT EVALUATED.</footer></main></body></html>'
    ids=re.findall(r'\bid="([^"]+)"',text)
    if len(ids)!=len(set(ids)):raise ValueError('Duplicate HTML element IDs')
    return text