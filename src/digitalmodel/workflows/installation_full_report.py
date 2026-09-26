"""Integrate verified demand, assumed criteria and monitoring in one engineering report."""
from collections import Counter
from datetime import datetime, timezone
from hashlib import sha256
from html import escape
import json
import os
from pathlib import Path, PurePosixPath
import re

from digitalmodel.workflows.installation_envelope_dashboard import render_dashboard, _criteria_html
from digitalmodel.workflows.installation_partial_report import (
    _grid, _envelope_table, _case_details, _sling_rows, _table,
)
from digitalmodel.workflows.installation_workflow_schematic import workflow_schematic


def dashboard_parts(document):
    styles = re.findall(r'<style>(.*?)</style>', document, re.S)
    sections = re.findall(r'<section>(.*?)</section>', document, re.S)
    scripts = re.search(r'(<script id="payload".*)</body>', document, re.S)
    if len(styles) != 1 or len(sections) != 4 or scripts is None:
        raise ValueError('Unrecognized dashboard markup')
    if sections[1].count('id="envelope"') != 1 or sections[2].count('id="charts"') != 1:
        raise ValueError('Dashboard section identity mismatch')
    grid = sections[1].replace('2 · Hs–Tp envelope under assumptions', '5.4 Hs–Tp envelope under assumptions')
    grid = grid.replace('Figure 1.', 'Figure 5-1.')
    live = sections[2].replace('3 · Simulated near-real-time review', '5.5 Simulated monitoring and two-minute forecasts')
    script = scripts[1]
    if script.count('return holder;') != 1:
        raise ValueError('Dashboard chart function changed')
    caption = ("const figureCaption=document.createElement('p');figureCaption.className='caption';"
               "figureCaption.textContent=`Figure 5-${frame.channels.findIndex(c=>c.id===channel.id)+2}. ${channel.label}: "
               "recorded history and selected 120 s preview/forecast mode, with NOW divider.`;holder.append(figureCaption);return holder;")
    script = script.replace('return holder;', caption)
    return styles[0], '<section>' + grid + '</section><section>' + live + '</section>', script


def _validate_sources(summary, payload, digest):
    if payload.get('provenance', {}).get('summary', {}).get('sha256') != digest:
        raise ValueError('Payload source-summary digest mismatch')
    actual = {row['index']: row for row in summary['cases']}
    cells = {row['index']: row for row in payload['cases']}
    if len(actual) != len(summary['cases']) or len(cells) != len(payload['cases']) or set(actual) != set(cells):
        raise ValueError('Case coverage differs between summary and envelope')
    for index, row in actual.items():
        if any(row[key] != cells[index][key] for key in ('hs_m', 'tp_s')):
            raise ValueError('Case coordinates differ between summary and envelope')
        if row['status'] != 'VERIFIED' and cells[index]['status'] != 'NOT_EVALUATED':
            raise ValueError('Unverified case cannot receive an assumed-criteria disposition')


def _case_label(case):
    return f"case {case['index']:03d}, Hs {case['hs_m']:g} m / Tp {case['tp_s']:g} s"


def _findings(payload):
    result = []
    checks = [(case, check) for case in payload['cases'] for check in case.get('checks', [])
              if case.get('status') in ('WITHIN_ASSUMPTIONS', 'EXCEEDS_ASSUMPTIONS')
              and check.get('status') in ('PASS', 'FAIL')]
    loads = [(case, check) for case, check in checks if 'limit_kN' in check and check.get('utilization') is not None]
    if loads:
        case, check = max(loads, key=lambda pair: pair[1]['utilization'])
        result.append(f"The governing assumed tensile-capacity utilisation is {100 * check['utilization']:.3f}% "
                      f"for {check['governing_channel']}: {check['demand_kN']:.3f} kN compared with "
                      f"{check['limit_kN']:.3f} kN, {_case_label(case)}. This is an assumed-criterion result.")
    hoists = [(case, check) for case, check in checks if check.get('minimum_static_ratio') is not None]
    if hoists:
        case, check = min(hoists, key=lambda pair: pair[1]['minimum_static_ratio'])
        relation = 'meets or exceeds' if check['minimum_static_ratio'] >= check['required_ratio'] else 'is below'
        result.append(f"The minimum hoist tension/static ratio is {check['minimum_static_ratio']:.4f}, "
                      f"which {relation} the assumed minimum {check['required_ratio']:.4f}, at "
                      f"{check['governing_channel']}, {_case_label(case)}.")
    boundaries = payload.get('boundaries', [])
    if boundaries and all(row.get('upper_edge_censored') for row in boundaries):
        top = max(row['hs_m'] for row in payload['cases'])
        result.append(f"The tested upper Hs edge is {top:g} m. Every assessed Tp column remains edge-censored: "
                      "a failure boundary is not reached. No higher-wave capability is established.")
    return result


def _forecast_rows(payload):
    demo, snapshot = payload['demo'], payload.get('snapshot', {})
    scenarios = demo.get('scenarios', [demo])
    scenario = next((s for s in scenarios if all(s[k] == snapshot.get(k) for k in ('hs_m', 'tp_s'))), scenarios[0])
    frame = next((f for f in scenario['frames'] if f['now_s'] == snapshot.get('now_s')), scenario['frames'][0])
    rows = []
    for channel in frame['channels']:
        metrics = channel.get('wave_preview_metrics')
        if channel['id'] == 'wave_elevation' or not metrics:
            continue
        methods = metrics.get('120', metrics)
        value = methods['oracle_wave_fir']['rmse']
        baseline = min(methods['persistence']['rmse'], methods['history_mean']['rmse'])
        rows.append([escape(channel['label']), f"{scenario['hs_m']:g} / {scenario['tp_s']:g}",
                     f"{frame['now_s']:g}", f'{value:.3f}', f'{baseline:.3f}', escape(channel['units']),
                     'Lower observed RMSE' if value < baseline else 'No advantage over best naive baseline'])
    return rows


def _cover(config, created):
    from digitalmodel.workflows.installation_report_layout import report_cover
    return report_cover(dict({'revision':'R7'},**config), created)


def _introduction_summary(summary, payload):
    counts = Counter(row['status'] for row in payload['cases'])
    findings = ''.join(f'<li>{escape(item)}</li>' for item in _findings(payload))
    forecast = _table(['Load channel', 'Hs (m) / Tp (s)', 'Forecast origin (s)', 'Conditional RMSE',
                       'Best naive RMSE', 'RMSE unit', 'Comparison'], _forecast_rows(payload))
    return f'''<section id="section-1"><h2>1 Introduction</h2><p>The analysis evaluates installation demand for the modelled jumper arrangement
and establishes sampled Hs–Tp envelopes against declared project assumptions. A simulated two-minute monitoring demonstration is integrated
with the engineering results. The deep-zone arrangement and prescribed vessel-motion condition define the scope; other installation stages remain unqualified.</p></section>
<section id="section-2"><h2>2 Summary and conclusions</h2><p>{summary['counts'].get('VERIFIED', 0)} completed cases have verified source records.
{counts.get('WITHIN_ASSUMPTIONS', 0)} cells are within the configured assumptions, {counts.get('EXCEEDS_ASSUMPTIONS', 0)} exceed them,
and {counts.get('NOT_EVALUATED', 0)} are not evaluated. These classifications concern the enumerated checks only.</p><ul>{findings}</ul>
<p><strong>Operational acceptance remains NOT EVALUATED.</strong> Assumed capacities, unresolved component checks and statistical/model qualification
prevent issue of an approved operational envelope.</p>{forecast}<p class="caption">Table 2-1. Default review snapshot: 120 s conditional load RMSE
compared with the better persistence/history-mean baseline, using withheld simulated loads. Future JONSWAP waves are supplied inputs;
the table does not quantify offshore wave-prediction accuracy.</p></section>'''


def _design(summary, payload, config):
    rows = []
    for label, key, unit, setting in [('Hs', 'hs_m', 'm', False), ('Tp', 'tp_s', 's', False),
            ('Wave heading', 'heading_degrees', 'deg', False), ('Random seed', 'seed', 'dimensionless', False),
            ('Build-up', 'buildup_s', 's', True), ('Dynamics', 'duration_s', 's', True),
            ('Logging', 'sample_interval_s', 's', True), ('Time step', 'max_time_step_s', 's', True),
            ('JONSWAP gamma', 'gamma', 'dimensionless', True), ('Wave components', 'components', 'count', True)]:
        values = sorted({str((c.get('settings', {}) if setting else c).get(key, 'Not recorded')) for c in summary['cases']})
        rows.append([label, escape(', '.join(values)), unit, 'Pinned verified-case records', 'Recorded analysis input', 'Demand, sampling or scenario scope can change'])
    for row in config.get('design_data', []):
        rows.append([escape(str(row.get(k, 'Not recorded'))) for k in ('parameter', 'value', 'unit', 'source', 'status', 'effect_if_changed')])
    disclosures = ''.join(f'<p>{escape(str(value))}</p>' for value in config.get('disclosures', []))
    return ('<section id="section-3"><h2>3 Design data and assumed criteria</h2>' +
            _table(['Parameter', 'Value', 'Unit', 'Source', 'Status', 'Effect if changed'], rows) +
            '<p class="caption">Table 3-1. Modelled environmental and numerical inputs.</p><h3>3.1 Assumptions carried</h3>' +
            _criteria_html(payload) + '<p class="caption">Table 3-2. Assumed endpoint capacity and minimum/static criteria.</p>' + disclosures +
            '''<p>The capacities are project assumptions rather than an approved equipment register. Changing them changes cell utilisation and classification.
            The crane axial proxy does not establish radius-dependent crane capacity. Intentional sling slack is not rejected by a zero-crossing rule.
            The specified force conversion and component mappings are retained in Appendix B.</p>
            <p>Controlled arrangement dimensions, vessel draught/RAO provenance, material properties and the governing lifting-code applicability
            require reconciliation with the retained source records before operational acceptance.</p></section>''')


def _engineering_schematic():
    diagram = workflow_schematic()
    for original, replacement in [('Pamphlet evidence', 'Engineering review record'),
            ('Reviewed supported claims', 'Assumptions / results'), ('workflow-pamphlet', 'workflow-record'),
            ('data-to="pamphlet"', 'data-to="record"'), ('Oracle input labelled if used', 'Supplied wave preview input'),
            ('Pending qualified boundaries', 'Sampled assumed-criteria grid'), ('Figure M1.', 'Figure 4-1.')]:
        if original not in diagram:
            raise ValueError('Workflow schematic source changed')
        diagram = diagram.replace(original, replacement)
    return diagram


def _methodology(config):
    solver = escape(str(config.get('solver_version', 'Not supplied; see native run receipts')))
    return ('<section id="section-4"><h2>4 Analysis methodology</h2><p>Recorded solver version: ' + solver + '</p>' + _engineering_schematic() + '''
<h3>4.1 Dynamic analysis and extraction</h3><p>A shared master and hashed case changes define the irregular-wave runs. Static equilibrium precedes
the dynamic record; the build-up interval is excluded. Native solver/version receipts and exact inputs remain with each case. Signed effective tension
is retained independently at both ends. Positive tension denotes loading; non-positive tension is a model-response diagnostic, not allowable compression.</p>
<h3>4.2 Assumed-criteria screening</h3><p>Maximum recorded endpoint tension is divided by its assumed capacity. Hoist minimum tension is divided by
the corresponding static tension and compared with the assumed minimum ratio. Complete checks are required for a within-assumptions cell.
Only contiguous sampled passing heights are retained; gaps are not interpolated and the tested upper edge is not extrapolated.</p>
<h3>4.3 Monitoring demonstration</h3><p>The default display supplies the future simulated random JONSWAP wave record to a linear load model fitted
using past wave/load samples. Load truth is held out. This is a conditional load forecast with oracle wave input, not causal offshore wave prediction.
The separate history-only autoregression mode uses history through NOW. RMSE is compared with persistence and history-mean predictions over 120 s.</p>
<p>Marker coordinates are prescribed Hs/Tp values. Playback advances forecast origins without inventing measured sea-state evolution.
Uncertainty intervals and live sensor assimilation are not established.</p></section>''')


def _results(summary, dashboard):
    return ('<section id="section-5"><h2>5 Results — conditional screening</h2><h3>5.1 Completed-case coverage</h3>' + _grid(summary['cases']) +
            '<p class="caption">Table 5-1. Execution coverage; verified completion does not itself denote operational acceptance.</p></section>'
            '<section><h3>5.2 Component response envelopes</h3>' + _envelope_table(summary['envelopes'], True) +
            '<p class="caption">Table 5-2. Signed tension extrema and total ≤0 duration, with governing case and endpoint. Units: kN and s.</p>' +
            _envelope_table(summary['envelopes'], False) +
            '<p class="caption">Table 5-3. Native jumper/connector response; units and governing positions are retained per channel.</p></section>'
            '<section><h3>5.3 Intentional unloading and geometric interpretation</h3>' + _sling_rows(summary['cases']) +
            '<p class="caption">Table 5-4. Sling 5 End B diagnostic events by case. Total ≤0 duration differs from the longest continuous event.</p>'
            '''<p>Endpoint chord deficit includes sag and extension effects and is not physical slack length. Negative tension is not a physically sustainable
            sling compression capacity. Re-tension peaks, interference and geometric slack remain separate acceptance checks.</p></section>''' + dashboard)


def _validation_recommendations(payload):
    items = ''.join(f'<li>{escape(item)}</li>' for item in payload.get('limitations', []))
    return '''<section id="section-6"><h2>6 Validation status</h2><p>Completed-case provenance and numerical extraction are verified in the
retained source assessment. This integration checks its digest and case correspondence; it does not rerun native simulations.
Configured assumption checks and simulated forecast errors are calculated. Independent field validation, calibrated uncertainty and operational
acceptance are not established.</p><ul>''' + items + '''</ul></section>
<section id="section-7"><h2>7 Conclusions and recommendations</h2><p>The calculated envelope supports comparison against the declared assumptions
within the sampled grid. The upper tested edge cannot be interpreted as a maximum workable wave height without a limiting case and qualification.</p>
<ol><li>Reconcile capacities, factors, connection details and governing code edition against the approved equipment/design register.</li>
<li>Complete slack/re-tension, snap, interference, pipe/connector and crane operating-radius checks.</li>
<li>Qualify wave seeds, duration, time step, RAO coverage, vessel heading and installation-stage sensitivities.</li>
<li>Validate measured wave-preview accuracy, latency and conditional load errors before any operational use of the monitoring display.</li></ol></section>'''


def _appendix_case_details(cases, base):
    details = _case_details(cases, base)
    def caption(match):
        return match[1] + f'<p class="caption">Table A-{int(match[2]) + 1:03d}. Recorded channel extrema for case {int(match[2]):03d}; units and locations are retained in the table.</p>'
    result, count = re.subn(r'(<details><summary>Case (\d+).*?</table></div>)', caption, details, flags=re.S)
    if count != sum(case['status'] == 'VERIFIED' for case in cases):
        raise ValueError('Detailed-case caption coverage mismatch')
    return result


def _display_cases(summary, config, base):
    """Resolve explicit portable link mappings without changing pinned source rows."""
    if 'case_run_dirs' not in config:
        return summary['cases']
    mapping = config['case_run_dirs']
    expected = {str(row['index']) for row in summary['cases']}
    if not isinstance(mapping, dict) or set(mapping) != expected:
        raise ValueError('Complete case_run_dirs mapping required')
    locator = config.get('dataset_root')
    if not isinstance(locator, str) or not locator or locator.startswith(('/', '\\')) or ':' in locator:
        raise ValueError('dataset_root must be an explicit relative locator')
    root = (Path(base) / locator).resolve()
    rows = []
    for row in summary['cases']:
        entry = mapping[str(row['index'])]
        if (not isinstance(entry, str) or not entry or '\\' in entry or ':' in entry
                or entry.startswith('/') or '..' in PurePosixPath(entry).parts):
            raise ValueError('Case mapping entries must be relative without traversal')
        resolved = (root / entry).resolve()
        if not resolved.is_relative_to(root):
            raise ValueError('Case mapping resolves outside dataset root')
        rows.append(dict(row, run_dir=str(resolved)))
    return rows


def _references_appendices(summary, payload, config, evidence, base, created):
    rows = [[escape(name), escape(value['path']), escape(value['sha256'])] for name, value in evidence.items()]
    references = _table(['Source', 'Owner-relative / report-relative locator', 'SHA-256'], rows)
    for row in config.get('references', []):
        locator = str(row['path'])
        if ':' in locator.split('/')[0] or locator.startswith(('/', '\\')):
            raise ValueError('Report reference links must be relative paths')
    links = ''.join(f'<li><a href="{escape(str(row["path"]), quote=True)}">{escape(str(row["label"]))}</a></li>'
                    for row in config.get('references', []))
    history = _table(['Revision', 'Generated UTC', 'Description'], [[escape(str(config.get('revision', 'R7'))), escape(created),
                    escape(str(config.get('revision_description', 'Integrated engineering results and simulated monitoring')))]])
    return ('<section id="section-8"><h2>8 References and revision history</h2>' + references +
            '<p class="caption">Table 8-1. Pinned source artifacts used for this revision.</p><ul>' + links + '</ul>' + history +
            '<p class="caption">Table 8-2. Current review revision; no external approval is recorded.</p>'
            '''<p>This revision integrates previously verified demand results with calculated assumed-criteria screening and the random-wave preview demonstration.
            Earlier source snapshots remain separate evidence; no external issue or approval history is inferred.</p></section>'''
            '<section id="appendix-a"><h2>Appendix A Detailed results by case</h2>' + _appendix_case_details(_display_cases(summary, config, base), base) + '</section>'
            '''<section id="appendix-b"><h2>Appendix B Provenance and unresolved checks</h2><p>The JSON sidecar retains the full demand summary,
            assumption payload, case checks, forecast frames and pinned sources. Operationally unresolved checks remain explicit below.</p><pre>''' +
            escape(json.dumps(payload.get('criteria_basis', {}), indent=2)) + '</pre><h3>B.1 Provenance</h3><pre>' +
            escape(json.dumps(payload.get('provenance', {}), indent=2)) + '</pre></section>')


def render_report(summary, payload, config, evidence, created, base):
    styles, dashboard, scripts = dashboard_parts(render_dashboard(payload))
    content = (_cover(config, created) + _introduction_summary(summary, payload) + _design(summary, payload, config) +
               _methodology(config) + _results(summary, dashboard) + _validation_recommendations(payload) +
               _references_appendices(summary, payload, config, evidence, base, created))
    document = ('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">'
                '<title>' + escape(config.get('title', 'Jumper installation engineering report')) + '</title><style>' + styles +
                '</style></head><body><main>' + content + '<footer>Private engineering review record. Operational acceptance: NOT EVALUATED.</footer></main>' + scripts + '</body></html>')
    ids = re.findall(r'\bid="([^"]+)"', document)
    if len(ids) != len(set(ids)):
        raise ValueError('Duplicate HTML element IDs')
    return document


def generate_report(summary_path, payload_path, output, config=None):
    summary_path, payload_path, output = map(Path, (summary_path, payload_path, output))
    sidecar = output.with_suffix('.json')
    if output.exists() or sidecar.exists() or output == sidecar:
        raise FileExistsError('New full-report destinations required')
    source, raw_payload = summary_path.read_bytes(), payload_path.read_bytes()
    summary, payload = json.loads(source), json.loads(raw_payload)
    _validate_sources(summary, payload, sha256(source).hexdigest())
    evidence = {name: dict(path=os.path.relpath(path, output.parent).replace('\\', '/'), sha256=sha256(raw).hexdigest())
                for name, path, raw in [('Demand summary', summary_path, source), ('Envelope and monitoring payload', payload_path, raw_payload)]}
    created = datetime.now(timezone.utc).isoformat()
    if isinstance(config, (str, Path)):
        config_path = Path(config)
        raw_config = config_path.read_bytes()
        evidence['Report configuration'] = dict(path=os.path.relpath(config_path, output.parent).replace('\\', '/'), sha256=sha256(raw_config).hexdigest())
        config = json.loads(raw_config)
    config = config or {}
    rendered = render_report(summary, payload, config, evidence, created, output.parent)
    record = dict(created_utc=created, document_control=config, evidence=evidence, demand_summary=summary,
                  assessment_payload=payload, engineering_acceptance='NOT EVALUATED')
    output.parent.mkdir(parents=True, exist_ok=True)
    for path, text in [(output, rendered), (sidecar, json.dumps(record, indent=2, allow_nan=False))]:
        with path.open('x', encoding='utf-8', newline='\n') as stream:
            stream.write(text)
        if path.read_text(encoding='utf-8') != text:
            raise ValueError('Full report readback mismatch')
    return record
