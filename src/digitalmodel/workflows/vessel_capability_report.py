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
               ('No governing Tp; no positive-duration nonpositive-tension events' if row['low_duration']['maximum_low_tension_duration_s']==0 else
                f"{row['low_duration']['tp_s']} / {row['low_duration']['maximum_low_tension_duration_s']:.3f}")]
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


def render_html(summary, base=Path('.'), config=None):
    """Render pinned findings without changing their numerical evidence."""
    from digitalmodel.workflows.vessel_capability_layout import render_layout
    return render_layout(summary, base, config or {})


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
