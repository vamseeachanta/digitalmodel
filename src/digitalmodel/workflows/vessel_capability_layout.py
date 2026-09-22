"""Engineering presentation of an unchanged partial structure-demand snapshot."""
from html import escape
import json
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


def _results(summary):
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
    text+='<h3>5.4 Provisional installation envelope</h3><p><strong>Not established.</strong> A future Hs–Tp envelope requires declared criteria, '
    text+='matched component checks, numerical/model qualification and documented boundaries. No boundary is inferred from execution coverage.</p>'
    text+='<h3>5.5 Two-minute forecasting</h3><p>Mudmat forecast validation remains pending. The intended 120 s demonstration will separate '
    text+='history from prediction with a clear NOW line and quantify held-out performance against naive baselines. '
    text+='A causal prediction uses history available at the forecast origin; supplied future-wave input, if used, will be labelled conditional.</p>'
    return _section(5,'Results — conditional screening',text)


def _validation_conclusions(summary,config):
    from digitalmodel.workflows.vessel_capability_report import _pending
    text=_pending(summary['design_basis'],summary.get('campaign_snapshot'),summary.get('sensitivity_campaign_snapshot'))
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


def render_layout(summary,base,config):
    config=dict(config)
    config.setdefault('title','Vessel capability for mudmat installation')
    config.setdefault('document_id','Structure installation engineering assessment')
    config.setdefault('revision','Not recorded')
    config.setdefault('subtitle','Retained simulated irregular-wave demand; qualification and forecasting pending')
    created=config.get('rendered_utc',summary['created_utc'])
    cover=(report_cover(config,created)+'<section><p>Source snapshot UTC: '+escape(summary['created_utc'])+
        '</p><p>Presentation generated UTC: '+escape(str(created))+'</p></section>')
    content=cover+_intro_summary(summary,config)+_design(summary,config)+_method(summary)+_results(summary)
    content+=_validation_conclusions(summary,config)+_appendices(summary,base,config)
    text='<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">'
    text+='<title>'+escape(config['title'])+'</title><style>'+STYLE
    text+='pre{white-space:pre-wrap;overflow-wrap:anywhere}p,td,a{overflow-wrap:anywhere}</style></head><body><main>'+content
    text+='<footer>Private engineering review record. Operational acceptance: NOT EVALUATED.</footer></main></body></html>'
    ids=re.findall(r'\bid="([^"]+)"',text)
    if len(ids)!=len(set(ids)):raise ValueError('Duplicate HTML element IDs')
    return text
