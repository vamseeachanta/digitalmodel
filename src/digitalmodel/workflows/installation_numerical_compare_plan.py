"""Bind production numerical comparisons to a pinned plan and native preparation."""
from hashlib import sha256
import json
from pathlib import Path

from digitalmodel.workflows.installation_numerical_compare import _read_pinned, compare_pair


def _rows(document):
    rows = document['cases']
    result = {row['id']: row for row in rows}
    if len(result) != len(rows):
        raise ValueError('Duplicate case identity')
    return result


def _documents(config, consumed):
    plan_path = Path(config['plan_path']).resolve()
    prepared_path = Path(config['preparation_manifest']).resolve()
    plan = json.loads(_read_pinned(plan_path, config['plan_sha256'], consumed))
    prepared = json.loads(_read_pinned(prepared_path, config['preparation_sha256'], consumed))
    if prepared.get('status') != 'native_verified_not_run' or prepared.get('plan_sha256') != config['plan_sha256']:
        raise ValueError('Preparation is not verified against pinned plan')
    if prepared.get('source_anchors') != plan['source_anchors']:
        raise ValueError('Preparation anchor identity mismatch')
    rows, actual = _rows(plan), _rows(prepared)
    if set(rows) != set(actual) or config['case_id'] not in rows:
        raise ValueError('Plan/preparation case coverage mismatch')
    for key, row in rows.items():
        if any(actual[key].get(field) != row[field] for field in ('id', 'source_case', 'compare_to', 'changes')):
            raise ValueError('Preparation comparison mapping differs from plan')
    return plan, prepared_path, rows, actual


def _selected(plan, rows, case_id):
    child = rows[case_id]
    anchors = plan['source_anchors']
    source = child['source_case']
    if source not in anchors:
        raise ValueError('Missing selected source anchor')
    visited, current = set(), child
    while current['compare_to'] in rows:
        if current['id'] in visited:
            raise ValueError('Cyclic comparison chain')
        visited.add(current['id'])
        current = rows[current['compare_to']]
        if current['source_case'] != source:
            raise ValueError('Cross-anchor comparison forbidden')
    if current['compare_to'] != source:
        raise ValueError('Comparison chain must terminate at its source anchor')
    parent_changes = rows[child['compare_to']]['changes'] if child['compare_to'] in rows else {}
    if not set(parent_changes) <= set(child['changes']):
        raise ValueError('Cumulative controls cannot silently revert')
    changes = {k: v for k, v in child['changes'].items() if parent_changes.get(k) != v}
    if len(changes) != 1:
        raise ValueError('Plan must change one immediate numerical control')
    field = next(iter(changes))
    kind = ('logging' if field == 'General.TargetLogSampleInterval' else
            'time' if field == 'General.ImplicitConstantTimeStep' else 'mesh')
    return child, kind, changes


def _preparation_row(row, base, consumed):
    if row.get('status') != 'native_verified_not_run':
        raise ValueError('Native verified preparation case required')
    model = (base / row['model_path']).resolve()
    _read_pinned(model, row['model_sha256'], consumed)
    _read_pinned(model.parent/'request.yml', row['request_sha256'], consumed)
    return row['model_sha256'], row['request_sha256']


def _anchor_row(name, anchor, consumed):
    model = Path(anchor['model_path']).resolve()
    _read_pinned(model, anchor['model_sha256'], consumed)
    generation = json.loads(_read_pinned(model.parent/'generation.json', anchor['generation_sha256'], consumed))
    if (generation.get('case_id') != name or generation.get('status') != 'native_verified_not_run'
            or generation.get('model_sha256') != anchor['model_sha256']):
        raise ValueError('Anchor generation binding mismatch')
    _read_pinned(model.parent/'request.yml', generation['request_sha256'], consumed)
    return anchor['model_sha256'], generation['request_sha256']


def _bind_run(root, receipt_sha, expected, consumed):
    receipt = json.loads(_read_pinned(Path(root)/'run.json', receipt_sha, consumed))
    if (receipt.get('model_sha256'), receipt.get('request_sha256')) != expected:
        raise ValueError('Run model/request does not belong to selected preparation')


def compare_planned_pair(config):
    if 'kind' in config or 'expected_changes' in config:
        raise ValueError('Production comparison controls must derive from pinned plan')
    consumed = {}
    plan, prepared_path, rows, actual = _documents(config, consumed)
    child, kind, changes = _selected(plan, rows, config['case_id'])
    candidate = _preparation_row(actual[child['id']], prepared_path.parent, consumed)
    parent_id = child['compare_to']
    parent = (_preparation_row(actual[parent_id], prepared_path.parent, consumed) if parent_id in actual else
              _anchor_row(parent_id, plan['source_anchors'][parent_id], consumed))
    for name, expected in [('parent', parent), ('candidate', candidate)]:
        _bind_run(config[name], config[name+'_receipt_sha256'], expected, consumed)
    keys = ('parent', 'candidate', 'parent_receipt_sha256', 'candidate_receipt_sha256',
            'parent_metadata_sha256', 'candidate_metadata_sha256')
    result = compare_pair(**{key: config[key] for key in keys}, kind=kind, expected_changes=changes)
    result.update(planned_case_id=child['id'], planned_parent_id=parent_id,
                  plan_sha256=config['plan_sha256'], preparation_sha256=config['preparation_sha256'])
    result['plan_sources'] = [{'path': str(p), 'sha256': h} for p, h in consumed.items()]
    modules = ['installation_numerical_compare', 'installation_numerical_compare_plan',
               'installation_numerical_prepare', 'installation_response_metrics', 'installation_trace_extract']
    result['dependency_sha256'] = {name: sha256(Path(__file__).with_name(name+'.py').read_bytes()).hexdigest()
                                   for name in modules}
    for path, digest in consumed.items():
        if sha256(path.read_bytes()).hexdigest() != digest:
            raise ValueError('Plan/preparation changed during comparison')
    return result
