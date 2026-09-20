"""Evidence-checked partial vessel capability report for structure installations."""
from collections import Counter
from datetime import datetime, timezone
from hashlib import sha256
from html import escape
import json
from pathlib import Path
from digitalmodel.workflows.installation_event_audit import audit_case
from digitalmodel.workflows.installation_trace_extract import verify_profile_metadata, verify_profile_arrays

from digitalmodel.workflows.installation_partial_report import (
    STYLE, collect_cases, component_envelopes, _grid, _envelope_table,
    _case_details, _table,
)


def critical_periods(cases):
    """Report governing sampled periods; partial rows never imply full coverage."""
    result = []
    for height in sorted({row['hs_m'] for row in cases}):
        planned = [row for row in cases if row['hs_m'] == height]
        verified = [row for row in planned if row['status'] == 'VERIFIED']
        if not verified:
            continue
        record = dict(hs_m=height, verified_periods=[row['tp_s'] for row in verified],
                      complete_tp_row=len(verified) == len(planned))
        for name, metric in [('peak_tension', 'peak_tension_kN'),
                             ('low_duration', 'maximum_low_tension_duration_s')]:
            governing = max(verified, key=lambda row: row[metric])
            record[name] = {key: governing[key] for key in ('index', 'tp_s', metric)}
        result.append(record)
    return result


def _critical_table(rows):
    values = [[str(row['hs_m']), ', '.join(map(str, row['verified_periods'])),
               'Complete planned row' if row['complete_tp_row'] else 'Partial row',
               f"{row['peak_tension']['tp_s']} / {row['peak_tension']['peak_tension_kN']:.3f}",
               f"{row['low_duration']['tp_s']} / {row['low_duration']['maximum_low_tension_duration_s']:.3f}"]
              for row in rows]
    return _table(['Hs (m)', 'Verified Tp (s)', 'Coverage',
                   'Peak-load Tp (s) / tension (kN)', 'Low-tension Tp (s) / total ≤0 duration (s)'], values)


def _campaign_status(captured):
    if not isinstance(captured, dict):
        return 'No campaign snapshot supplied; execution status unknown'
    snapshot = captured.get('snapshot', captured)
    if not isinstance(snapshot, dict):
        return 'Invalid campaign snapshot; execution status unknown'
    status = str(snapshot.get('status', 'unknown')).lower()
    counts = Counter(str(row.get('status', 'unknown')).lower()
                     for row in snapshot.get('cases', []) if isinstance(row, dict))
    if status == 'completed' and (not counts or set(counts) != {'completed'}):
        status = 'Inconsistent snapshot: recorded completed'
    text = status + '; ' + ', '.join(f'{key}: {count}' for key, count in sorted(counts.items()))
    if captured.get('read_utc'):
        text += '; read ' + str(captured['read_utc'])
    if captured.get('captured_utc'):
        text += '; captured ' + str(captured['captured_utc'])
    return escape(text)


def _pending(basis, campaign=None, sensitivity=None):
    rows = [
        ['Baseline size', f"{escape(str(basis.get('dry_mass_t', 'Not recorded')))} t; {_campaign_status(campaign)}"],
        ['Smaller structure', 'Geometry, mass, buoyancy, drag/added mass and rigging basis pending'],
        ['Larger structure', 'Source-supported design selection and rigging/vessel compatibility pending'],
        ['Splash-zone and lowering stages', 'Not evaluated by this deep-submerged campaign'],
        ['Hydrodynamic sensitivity', _campaign_status(sensitivity)],
        ['Hs–Tp operating envelopes', 'Not established; capacity and slack/snap/interference criteria pending'],
        ['Two-minute forecasting', 'Mudmat forecast and holdout skill not evaluated; history/prediction divider and uncertainty pending'],
        ['Multiple random seeds', 'Not evaluated; single seed does not establish extreme-load statistics'],
    ]
    rows.extend([[escape(str(name)), escape(str(value))]
                 for name, value in basis.get('additional_sizes', {}).items()])
    return _table(['Assessment', 'Current evidence / placeholder'], rows)


def _front_html(summary):
    counts = ', '.join(f'{key}: {value}' for key, value in summary['counts'].items())
    basis = _table(['Parameter', 'Recorded basis'], [[escape(str(k)), escape(str(v))]
                   for k, v in summary['design_basis'].items()])
    return f'''<!doctype html><html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1"><title>Vessel capability — mudmat installation</title>
<style>{STYLE}</style></head><body><main><header><div class="tag">Engineering assessment / partial results</div>
<h1>Vessel capability for mudmat installation</h1><p>Simulated irregular-wave demand study · {summary['created_utc']}</p></header>
<section><h2>1 · Introduction</h2><p>The assessment will establish vessel-specific installation envelopes for
structures of varying sizes. This partial issue presents completed baseline deep-submerged responses.
Execution coverage and load demand are recorded separately from engineering acceptance.</p></section>
<section><h2>2 · Summary and conclusions</h2><p>{escape(counts)}</p>
<p class="notice"><strong>No operating window has been established.</strong> Load and low-tension maxima below govern only
the verified sampled cases. Component capacities, geometric clearance and slack/re-tension criteria remain unresolved.</p>
<p>Negative signed tension can reflect the model's line properties during unloading. It is not a physically sustainable
sling compression load or an allowable-compression criterion. A tension ≤0 event is a diagnostic; actual slack,
re-tensioning loads and interference require separate interpretation.</p></section>
<section><h2>3 · Design data</h2>{basis}<p class="caption">Table 1. Baseline model and environmental basis.</p></section>
<section><h2>4 · Analysis methodology</h2><p>An immutable campaign snapshot is compared with the input matrix.
Completed case generation, model, simulation and trace hashes are checked using digitalmodel's existing installation
report workflow. Tension event durations are recomputed from recorded traces. Running cases are excluded.
No solver is invoked by this report. Native sampled extrema are reported separately for each line end.</p>
<p>Only the imported vessel RAO condition, heading and single wave seed are represented. The native solver reports
wave components below the shortest displacement-RAO period; extrapolation requires assessment before acceptance.
Hydrodynamic coefficient sensitivities, time-step/duration convergence and repeated seeds remain required evidence.</p></section>
'''


def render_html(summary, base=Path('.')):
    return _front_html(summary) + f'''<section><h2>5 · Results</h2><h3>5.1 Execution coverage</h3>{_grid(summary['cases'])}
<p class="caption">Table 2. ● verified completed; ◐ running at snapshot; — missing; ! other state. These symbols are not acceptance verdicts.</p>
<h3>5.2 Governing sampled periods</h3>{_critical_table(summary['critical_periods'])}
<p class="caption">Table 3. Governing sampled Tp by Hs; different components can govern load and total low-tension duration.
Duration is accumulated tension ≤0 time over the 600 s record, not the longest continuous event.
Incomplete rows cannot establish a critical period over the full planned range.</p></section>
<section><h3>5.3 Line-end demand</h3>{_envelope_table(summary['envelopes'], True)}
<p class="caption">Table 4. Signed tension extrema and longest accumulated tension ≤0 duration, with governing case.</p></section>
<section><h3>5.4 Body and winch response</h3>{_envelope_table(summary['envelopes'], False)}
<p class="caption">Table 5. Native response channels. Body reference-point Z is not seabed clearance of the lowest rotated point.</p></section>
<section><h2>6 · Pending installation envelopes and qualification</h2>{_pending(summary['design_basis'], summary.get('campaign_snapshot'), summary.get('sensitivity_campaign_snapshot'))}
<p class="caption">Table 6. Placeholders retained for subsequent evidence and sizes.</p>
<p>Endpoint chord deficit is unstretched length minus endpoint separation; sag and extension contribute, so it is not physical slack.
Re-tension peaks over a specified diagnostic window do not alone qualify snap loads.</p></section>
<section><h2>Appendix A · Detailed results</h2>{_case_details(summary['cases'], base)}</section>
<section><h2>Appendix B · Provenance</h2><p class="hash">Matrix SHA-256: {summary['matrix_sha256']}<br>
Campaign snapshot SHA-256: {summary['campaign_sha256']}</p><p>The JSON sidecar retains the captured campaign,
verified channels, case hashes and supplied design basis. Verification failures are excluded from demand summaries.</p></section>
<footer>Reusable results owner: private digitalmodel-data. Local retention does not establish remote backup.</footer></main></body></html>'''


def _audit_profile(row):
    try:
        run = Path(row['run_dir'])
        receipt = json.loads((run / 'run.json').read_text())
        metadata_raw = (run / 'installation_traces/metadata.json').read_bytes()
        if sha256(metadata_raw).hexdigest() != row['metadata_sha256']:
            raise ValueError('Profile metadata changed after verification')
        metadata = json.loads(metadata_raw)
        verify_profile_metadata(run, receipt, metadata)
        verify_profile_arrays(run / 'installation_traces/traces.npz', receipt, metadata)
        channels = [key for key, channel in row['channels'].items()
                    if channel['variable'] == 'Effective tension']
        if not channels:
            raise ValueError('No effective tension channels')
        return audit_case(row, expected_channels=channels)
    except (OSError, ValueError, KeyError, TypeError) as error:
        return dict(index=row['index'], status='FAILED', channels_verified=0, errors=[str(error)])


def _pinned_json(path, digest):
    raw = Path(path).read_bytes()
    if sha256(raw).hexdigest() != digest:
        raise ValueError('Sensitivity dependency digest mismatch')
    return json.loads(raw)


def _sensitivity_source(matrix_path, matrix):
    source_path = (matrix_path.parent / matrix['source_manifest']).resolve()
    source = _pinned_json(source_path, matrix['source_manifest_sha256'])
    entries = {row['path']: row['sha256'] for row in source['files']}
    if len(entries) != len(source['files']):
        raise ValueError('Duplicate sensitivity source artifact')
    master = source_path.parent / 'master.yml'
    if sha256(master.read_bytes()).hexdigest() != entries.get('master.yml'):
        raise ValueError('Sensitivity source master digest mismatch')
    for case in matrix['cases']:
        if entries.get(f"matched-pilot/{case['id']}/model.yml") != case['model_sha256']:
            raise ValueError('Sensitivity model differs from source manifest')
    return dict(source_manifest_sha256=matrix['source_manifest_sha256'],
                source_master_sha256=entries['master.yml'])


def _capture_sensitivity(campaign, matrix_path, campaign_sha, matrix_sha):
    from digitalmodel.workflows.installation_priority_requests import _inputs
    if not matrix_path or not campaign_sha or not matrix_sha:
        raise ValueError('Pinned sensitivity campaign and matrix required')
    observed = datetime.now(timezone.utc).isoformat()
    snapshot = _pinned_json(campaign, campaign_sha)
    matrix_path = Path(matrix_path).resolve()
    matrix = _pinned_json(matrix_path, matrix_sha)
    pinned = snapshot['pinned_inputs']
    if pinned['manifest_sha256'] != matrix_sha:
        raise ValueError('Sensitivity campaign matrix binding mismatch')
    artifact = (Path(campaign).parent / pinned['artifact_manifest']).resolve()
    expected = _inputs(matrix_path, matrix_sha, artifact, pinned['artifact_manifest_sha256'])
    cases = {row['id']: row for row in snapshot['cases']}
    if len(cases) != len(snapshot['cases']) or set(cases) != {row['id'] for row in expected}:
        raise ValueError('Sensitivity campaign case identities differ')
    for row in expected:
        if any(cases[row['id']].get(k) != row[k] for k in ('model_sha256', 'request_sha256')):
            raise ValueError('Sensitivity campaign case dependency mismatch')
    provenance = _sensitivity_source(matrix_path, matrix)
    _pinned_json(campaign, campaign_sha)
    result = dict(read_utc=observed, sha256=campaign_sha, snapshot=snapshot,
                  matrix_sha256=matrix_sha, artifact_manifest_sha256=pinned['artifact_manifest_sha256'],
                  **provenance)
    if snapshot.get('captured_utc'):
        result['captured_utc'] = snapshot['captured_utc']
    return result


def generate_report(campaign, matrix, output, design_basis, *, sensitivity_campaign=None,
                    sensitivity_matrix=None, sensitivity_campaign_sha256=None,
                    sensitivity_matrix_sha256=None):
    output = Path(output)
    sidecar = output.with_suffix('.json')
    if output.exists() or sidecar.exists() or output == sidecar:
        raise FileExistsError('Report destination must be new')
    created = datetime.now(timezone.utc).isoformat()
    raw, matrix_raw = Path(campaign).read_bytes(), Path(matrix).read_bytes()
    snapshot, grid = json.loads(raw), json.loads(matrix_raw)
    if snapshot['matrix_sha256'] != sha256(matrix_raw).hexdigest():
        raise ValueError('Campaign matrix digest mismatch')
    cases = collect_cases(snapshot, grid, workers=1)
    event_audits = []
    for row in cases:
        if row['status'] != 'VERIFIED':
            continue
        audit = _audit_profile(row)
        event_audits.append(audit)
        if audit['status'] != 'VERIFIED':
            row.update(status='FAILED', reason='Full tension-event audit failed')
            row.pop('channels', None)
    summary = dict(created_utc=created, campaign_sha256=sha256(raw).hexdigest(),
                   matrix_sha256=sha256(matrix_raw).hexdigest(), campaign_snapshot=snapshot,
                   engineering_acceptance='NOT EVALUATED', design_basis=design_basis,
                   counts=dict(Counter(row['status'] for row in cases)), cases=cases,
                   verification_workers=1, envelopes=component_envelopes(cases),
                   critical_periods=critical_periods(cases), event_audits=event_audits)
    if sensitivity_campaign is not None:
        summary['sensitivity_campaign_snapshot'] = _capture_sensitivity(
            sensitivity_campaign, sensitivity_matrix, sensitivity_campaign_sha256,
            sensitivity_matrix_sha256)
    rendered = render_html(summary, output.parent)
    output.parent.mkdir(parents=True, exist_ok=True)
    with output.open('x', encoding='utf-8') as stream:
        stream.write(rendered)
    with sidecar.open('x', encoding='utf-8') as stream:
        json.dump(summary, stream, indent=2, allow_nan=False)
    if json.loads(sidecar.read_text(encoding='utf-8')) != summary:
        raise ValueError('JSON readback mismatch')
    if output.read_text(encoding='utf-8') != rendered:
        raise ValueError('HTML readback mismatch')
    return summary
