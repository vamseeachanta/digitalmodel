"""Build single-case replay derivatives using the established report modules."""
from datetime import datetime, timezone
from hashlib import sha256
import json

from digitalmodel.workflows.installation_partial_report import _collect_case, component_envelopes
from digitalmodel.workflows.installation_event_audit import audit_case
from digitalmodel.workflows.installation_monitoring_demo import prepare_payload
from digitalmodel.workflows.installation_full_report import generate_report
from digitalmodel.workflows.installation_full_report_pdf import render_full_pdf
from digitalmodel.workflows.installation_replay import _json, artifact_record, compare_metrics


def _summary(root, matrix, manifest):
    index = manifest['case_index']
    case = matrix['cases'][index]
    mapped = dict(index=index, status='COMPLETED', run_dir=str(root / 'run'),
                  generation_file=str(root / 'prepared/generation.json'),
                  **{k: case[k] for k in ('hs_m', 'tp_s', 'seed')})
    row = _collect_case(index, case, mapped, matrix)
    if row['status'] != 'VERIFIED':
        raise ValueError(f'Fresh evidence verification failed: {row}')
    channels = [key for key, value in row['channels'].items() if value['variable'] == 'Effective tension']
    audit = audit_case(row, expected_channels=channels)
    if not channels or audit['status'] != 'VERIFIED':
        raise ValueError('Fresh event audit failed')
    return dict(created_utc=datetime.now(timezone.utc).isoformat(), counts={'VERIFIED': 1},
                cases=[row], envelopes=component_envelopes([row]),
                campaign_snapshot={'cases': [mapped], 'master_sha256': matrix['master_sha256'],
                                   'selected_case_index': index},
                campaign_sha256=_campaign_identity(mapped, matrix),
                matrix_sha256=sha256((root / 'study/matrix.json').read_bytes()).hexdigest(),
                engineering_acceptance='NOT EVALUATED', full_event_audit=audit)


def _qualify_inherited(config):
    """Mark inherited campaign-level statements as historical context for a single-case replay."""
    config = dict(config)
    for key in ('disclosures', 'summary_findings', 'decisions', 'supplements'):
        config[key] = ['Historical full-campaign context; not fresh pilot findings: ' + text
                       for text in config.get(key, [])]
    return config


def _demo_configuration(frozen, summary, manifest):
    demo = json.loads(frozen['demo_config'].read_bytes())
    case = summary['cases'][0]
    matches = [s for s in demo['scenarios'] if all(s[k] == case[k] for k in ('hs_m', 'tp_s'))]
    if len(matches) != 1:
        raise ValueError('Pinned demo config must contain selected replay case')
    demo['scenarios'] = matches
    demo['snapshot'].update(hs_m=case['hs_m'], tp_s=case['tp_s'])
    demo['limitations'] = list(manifest['pilot_limitations'])
    return demo


def build_reports(root, frozen, matrix, manifest):
    summary = _summary(root, matrix, manifest)
    _json(root / 'summary.json', summary)
    reference = json.loads(frozen['reference_summary'].read_bytes())
    _reference_identity(summary, reference)
    matches = [row for row in reference['cases'] if row['index'] == manifest['case_index']]
    if len(matches) != 1 or matches[0]['status'] != 'VERIFIED':
        raise ValueError('One verified retained reference case required')
    _compare_configuration(summary['cases'][0], matches[0])
    comparison = compare_metrics(summary['cases'][0], matches[0], manifest['comparison'])
    _json(root / 'comparison.json', comparison)
    if not comparison['passed']:
        raise ValueError('Fresh metrics exceed explicit comparison tolerances')
    demo = _demo_configuration(frozen, summary, manifest)
    _json(root / 'selected-demo-config.json', demo)
    criteria = json.loads(frozen['criteria'].read_bytes())
    payload = prepare_payload(summary, criteria, demo)
    payload['title'] = 'Single-case jumper installation replay and simulated monitoring'
    payload['provenance'] = {'summary': artifact_record(root / 'summary.json', root),
                             'criteria': artifact_record(frozen['criteria'], root),
                             'demo_config': artifact_record(root / 'selected-demo-config.json', root)}
    _json(root / 'payload.json', payload)
    config = json.loads(frozen['report_config'].read_bytes())
    config.update(title=payload['title'], revision='Replay pilot', dataset_root='.',
                  case_run_dirs={str(manifest['case_index']): 'run'}, references=[],
                  revision_description='One fresh input-to-solver-to-report replay; historical campaign evidence unchanged.')
    config['inherited_design_source'] = config.get('design_source')
    config['design_source'] = artifact_record(root / 'prepared/model.yml', root)
    for row in config.get('design_data', []):
        row['source'] = 'Inherited design basis; original source: ' + str(row.get('source', 'Not recorded'))
    config = _qualify_inherited(config)
    config.setdefault('disclosures', []).append('Design-data qualifications are inherited from the pinned source report; the replay does not independently qualify drawings or material certificates. Fresh model identity is retained separately.')
    _json(root / 'pilot-config.json', config)
    generate_report(root / 'summary.json', root / 'payload.json', root / 'pilot-report.html', root / 'pilot-config.json')
    render_full_pdf(summary, payload, root / 'pilot-report.pdf', config,
                    summary_bytes=(root / 'summary.json').read_bytes())
    names = ['comparison.json', 'summary.json', 'selected-demo-config.json', 'payload.json',
             'pilot-config.json', 'pilot-report.html', 'pilot-report.json', 'pilot-report.pdf',
             'run/installation_traces/metadata.json', 'run/installation_traces/traces.npz', 'run/run.json']
    return dict(comparison=comparison, artifacts=[artifact_record(root / name, root) for name in names])


def _campaign_identity(mapped, matrix):
    identity = {key: mapped[key] for key in ('index', 'hs_m', 'tp_s', 'seed')}
    identity.update(master_sha256=matrix['master_sha256'],
                    change_sha256=matrix['cases'][mapped['index']]['change_sha256'])
    return sha256(json.dumps(identity, sort_keys=True).encode()).hexdigest()


def _reference_identity(fresh, reference):
    for key in ('matrix_sha256',):
        if not fresh.get(key) or fresh[key] != reference.get(key):
            raise ValueError('Retained reference matrix digest differs')
    master = fresh.get('campaign_snapshot', {}).get('master_sha256')
    if not master or master != reference.get('campaign_snapshot', {}).get('master_sha256'):
        raise ValueError('Retained reference master digest differs')


def _compare_configuration(fresh, reference):
    if any(reference[k] != fresh[k] for k in ('hs_m', 'tp_s', 'seed', 'heading_degrees')):
        raise ValueError('Retained comparison case coordinates/seed/heading differ')
    for key in ('buildup_s', 'duration_s', 'sample_interval_s', 'gamma', 'components',
                'max_time_step_s', 'fixed_time_step_s'):
        if fresh['settings'].get(key) != reference['settings'].get(key):
            raise ValueError(f'Retained numerical setting differs: {key}')
