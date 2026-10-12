"""Engineering report sections and explicit outstanding-result placeholders."""
from html import escape
from digitalmodel.workflows.installation_workflow_schematic import workflow_schematic


def _table(headers, rows):
    head = ''.join(f'<th>{escape(cell)}</th>' for cell in headers)
    body = ''.join('<tr>' + ''.join(f'<td>{escape(str(cell))}</td>' for cell in row)
                   + '</tr>' for row in rows)
    return f'<div class="scroll"><table><thead><tr>{head}</tr></thead><tbody>{body}</tbody></table></div>'


def _values(cases, key, settings=False):
    values = {str((case.get('settings', {}) if settings else case).get(key, 'Evidence required'))
              for case in cases if case.get('status') == 'VERIFIED'}
    return ', '.join(sorted(values)) if values else 'Pending analysis'


def design_data(summary):
    cases = summary['cases']
    rows = [(label, _values(cases, key, settings), unit, 'Verified completed-case records')
            for label, key, unit, settings in (
                ('Wave height Hs', 'hs_m', 'm', False), ('Peak period Tp', 'tp_s', 's', False),
                ('Wave heading', 'heading_degrees', 'deg', False), ('Random seed', 'seed', '—', False),
                ('Build-up duration', 'buildup_s', 's', True), ('Dynamic duration', 'duration_s', 's', True),
                ('Sampling interval', 'sample_interval_s', 's', True),
                ('Maximum solver time step', 'max_time_step_s', 's', True),
                ('JONSWAP gamma', 'gamma', 'dimensionless', True))]
    rows += [(name, 'Evidence required', 'Not assigned', basis) for name, basis in (
        ('Arrangement and dimensions', 'Matching source workbook is linked in Section 6; controlled design-data schedule pending'),
        ('Equipment capacities and factors', 'Documented workbook candidates require component mapping and factor reconciliation'),
        ('Pipe material and acceptance basis', 'Source material-property discrepancy requires resolution'),
        ('Governing lifting-code edition', 'Historical DNV comparison is available; project applicability remains unverified'))]
    return '<section id="design"><h2>3 · Design data</h2>' + _table(
        ['Parameter', 'Value / status', 'Unit', 'Source / qualification'], rows) + (
        '<p class="caption">Table D1. Recorded values cover completed cases only; the planned grid is in Section 5.1. '
        'Evidence required identifies an unresolved controlled design basis, not absence of all source documents.</p></section>')


def front_sections(summary):
    verified = summary['counts'].get('VERIFIED', 0)
    planned = len(summary['cases'])
    return f'''<section id="introduction"><h2>1 · Introduction</h2>
<p>The analysis evaluates simulated irregular-wave demand during jumper installation. The objectives are to quantify
crane-wire, sling, jumper and connector response; assess intentional sling unloading and re-tension; and develop a
qualified Hs–Tp operating window. A separate two-minute forecasting demonstration will support assessment of installation monitoring.</p>
<p>The engineering acceptance assessment remains incomplete. Pending results retain their designated sections for later population.</p></section>
<section id="conclusions"><h2>2 · Summary and conclusions</h2>
<p>{verified} of {planned} planned cases have verified completed evidence at the stated snapshot. Verification refers to
provenance and data consistency; it does not establish compliance with engineering acceptance criteria.</p>
<ul><li>Component demand envelopes in Section 5 cover completed cases only and retain each governing case and location.</li>
<li>Signed zero/negative sling tension is reported as a diagnostic; it is not a permissible compression or physical slack limit.</li>
<li>No approved operating limits are established. The outstanding acceptance checks are retained in Section 5.5.</li>
<li>Forecast results are simulation demonstrations; offshore prediction accuracy and operational benefit remain unvalidated.</li></ul>
<p><strong>Final conclusions: Pending analysis and acceptance assessment.</strong> Final limiting sea states, governing failure modes,
restrictions and required operating controls will be populated after the outstanding calculations and evidence checks.</p></section>
{design_data(summary)}
<section id="methodology"><h2>4 · Analysis methodology</h2>
{workflow_schematic()}
<h3>4.1 Model and environmental cases</h3><p>A common master model and hashed change files define the Hs–Tp cases.
The recorded solver version, model identity and case settings are retained with each run. Static equilibrium precedes
time-domain irregular-wave dynamics; the build-up interval is excluded from the reported dynamic record.</p>
<h3>4.2 Extraction and verification</h3><p>Saved simulations and extracted traces are checked against recorded hashes.
Channel extrema and signed low-tension durations are checked against time histories. Both sling and crane-wire ends
are retained separately. Governing-arc selections are diagnostic results from the full record, not causal forecast locations.</p>
<h3>4.3 Acceptance and operating-window assessment</h3><p>Demand will be compared with traceable component capacities,
applicable factors and slack/snap/interference criteria. Duration, seed, time-step, mesh and heading sensitivities will
qualify candidate boundaries. Missing criteria remain Not evaluated; missing runs are not interpolated into passing cells.</p>
<h3>4.4 Forecasting method</h3><p>History-only predictions are compared with recorded subsequent response and simple baselines.
Prescribed future-wave assistance, if presented, will be labelled illustrative oracle input and kept separate from causal forecasts.</p></section>'''


def pending_sections(summary):
    pending = sum(summary['counts'].get(k, 0) for k in ('MISSING', 'RUNNING', 'INCOMPLETE', 'FAILED'))
    rows = [
        ('Full sea-state matrix', 'Pending analysis' if pending or not summary['cases'] else 'Runs complete; acceptance Not evaluated',
         f'{pending} cases require completion or evidence resolution; regenerate the verified report snapshot.'),
        ('Operating window: limiting Hs versus Tp', 'Not evaluated',
         'Populate limiting cells, governing component/criterion, tested bounds and restrictions after criterion checks.'),
        ('Capacity utilisation', 'Evidence required / Not evaluated',
         'Map rope, turnbuckle, shackle, crane, clamp, connector and pipe criteria; retain each included factor once.'),
        ('Snap loads and interference', 'Pending analysis',
         'Populate physical slack, re-tension peaks, contact/interference and modelling-sensitivity assessment.'),
        ('Convergence and statistical qualification', 'Pending analysis',
         'Populate time-step, mesh, duration, seed, operation-duration extremes, RAO and heading sensitivity.'),
        ('Forecast validation', 'Not evaluated for live use',
         'Populate per-horizon error, baseline comparison, uncertainty, warning lead time and independent validation.')]
    return '<section id="pending"><h2>5.5 · Outstanding results and acceptance placeholders</h2>' + _table(
        ['Result section', 'Current status', 'Required population / evidence'], rows) + (
        '<p class="caption">Table P1. Sections remain in every regenerated report. Run completion populates demand results; '
        'it does not automatically resolve engineering acceptance, sensitivity or evidence requirements.</p></section>')


def marketing_section():
    return (
        '<section id="marketing"><h2>7 · Pamphlet evidence and communication readiness</h2>' + _table(
            ['Proposed subject', 'Supporting evidence', 'Current disposition'], [
                ('Simulated installation response', 'Verified demand results, Sections 5.1–5.4 and Appendix A',
                 'May describe the analysed simulated cases with their scope and limitations.'),
                ('Hs–Tp operating limits', 'Section 5.5 operating-window assessment',
                 'No approved operating limits; claim withheld pending qualification.'),
                ('Two-minute prediction benefit', 'Section 6 forecast demonstration and future validation',
                 'Illustrative simulation only; no validated offshore accuracy or live capability claim.')]) +
        '<p class="caption">Table M1. The pamphlet will be updated from reviewed report findings; this report does not update or distribute it.</p></section>')
