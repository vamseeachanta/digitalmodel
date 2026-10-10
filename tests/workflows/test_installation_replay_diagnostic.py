import json

import pytest
from pypdf import PdfReader

from digitalmodel.workflows.installation_replay_diagnostic import render_diagnostic
import digitalmodel.workflows.installation_replay_diagnostic as diagnostic


def evidence(tmp_path, count=3):
    root = tmp_path / 'pilot'
    root.mkdir()
    lineage = dict(status='failed', error='Fresh metrics exceed explicit comparison tolerances',
                   simulation_disposition='retained_if_created_for_diagnosis',
                   manifest=dict(case_index=97, code={'git_revision': 'a' * 40},
                                 inputs={'reference_summary': {'path': '../reports/reference.json', 'sha256': 'b' * 64}},
                                 runtime={'solver_version': '11.6c'},
                                 comparison={'absolute_tolerance': 1e-6, 'relative_tolerance': 1e-5}),
                   stages=[{'stage': 'fresh_solve_and_extraction_completed'}])
    summary = dict(engineering_acceptance='NOT EVALUATED', cases=[dict(
        index=97, hs_m=2, tp_s=10, seed=123, heading_degrees=180, status='VERIFIED', run_dir='studies/pilot-097/run',
        channels={'sling': dict(units='kN', variable='Effective tension')})])
    rows = [dict(channel='sling', metric='maximum', actual=12, reference=10,
                 absolute_difference=2, tolerance=.000101, passed=False) for _ in range(count)]
    for i, row in enumerate(rows): row['metric'] = f'events.duration_{i}_s'
    comparison = dict(passed=False, settings=lineage['manifest']['comparison'], metrics=rows)
    for name, value in [('lineage', lineage), ('summary', summary), ('comparison', comparison)]:
        (root / f'{name}.json').write_text(json.dumps(value))
    return [root / f'{name}.json' for name in ['lineage', 'summary', 'comparison']]


def test_failed_diagnostic_lists_every_failure_but_pdf_is_bounded(tmp_path):
    paths = evidence(tmp_path, 121)
    before = [p.read_bytes() for p in paths]
    result = render_diagnostic(*paths, tmp_path / 'diagnostic')
    html = result['html'].read_text(encoding='utf-8')
    assert '121 of 121' in html and 'FAILED numerical reproduction' in html
    assert all(f'events.duration_{i}_s' in html for i in range(121))
    reader = PdfReader(result['pdf'])
    assert 1 <= len(reader.pages) <= 4
    text = '\n'.join(page.extract_text() for page in reader.pages)
    assert 'NOT EVALUATED' in text and '121 of 121' in text
    assert 'retained_if_created_for_diagnosis' in html
    assert 'href="../pilot/comparison.json"' in html
    assert [p.read_bytes() for p in paths] == before
    assert 'reference.json' in text and 'b' * 64 in text
    assert 'pilot-097' in text
    receipt = json.loads((result['html'].parent / 'render-receipt.json').read_bytes())
    assert receipt['status'] == 'COMPLETED'
    assert not (result['html'].parent / '.incomplete').exists()


def test_mixed_verdicts_include_only_failed_rows(tmp_path):
    paths = evidence(tmp_path, 313)
    data = json.loads(paths[2].read_bytes())
    for row in data['metrics'][121:]: row['passed'] = True
    paths[2].write_text(json.dumps(data))
    result = render_diagnostic(*paths, tmp_path / 'diagnostic')
    text = result['html'].read_text(encoding='utf-8')
    assert '121 of 313' in text
    assert text.count('<td>events.duration_') == 121
    assert 'events.duration_121_s' not in text


def test_pdf_ranking_selects_exact_largest_normalized_twelve(tmp_path):
    paths = evidence(tmp_path, 15)
    data = json.loads(paths[2].read_bytes())
    for i, row in enumerate(data['metrics']):
        row.update(absolute_difference=100-i, tolerance=(100-i)/(i+2))
    paths[2].write_text(json.dumps(data))
    result = render_diagnostic(*paths, tmp_path / 'diagnostic')
    text = '\n'.join(p.extract_text() for p in PdfReader(result['pdf']).pages)
    expected = [f'events.duration_{i}_s' for i in range(14, 2, -1)]
    assert all(name in text for name in expected)
    assert [text.index(name) for name in expected] == sorted(text.index(name) for name in expected)
    assert 'events.duration_2_s' not in text


@pytest.mark.parametrize('failure', ['pdf', 'source_changed'])
def test_failed_render_retains_incomplete_marker_without_completed_receipt(tmp_path, monkeypatch, failure):
    paths = evidence(tmp_path)
    original = diagnostic._pdf
    def broken(*args):
        if failure == 'pdf': raise RuntimeError('PDF failed')
        original(*args)
        paths[2].write_text(paths[2].read_text() + ' ')
    monkeypatch.setattr(diagnostic, '_pdf', broken)
    out = tmp_path / 'diagnostic'
    with pytest.raises((ValueError, RuntimeError)):
        render_diagnostic(*paths, out)
    assert (out / '.incomplete').exists()
    assert not (out / 'render-receipt.json').exists()


@pytest.mark.parametrize('defect', ['success', 'empty', 'nonfinite', 'case', 'settings', 'passed', 'acceptance', 'channel', 'negative'])
def test_inconsistent_evidence_fails_before_creating_outputs(tmp_path, defect):
    paths = evidence(tmp_path)
    which = 0 if defect == 'success' else 1 if defect in {'case', 'acceptance'} else 2
    data = json.loads(paths[which].read_bytes())
    if defect == 'success': data['status'] = 'completed'
    if defect == 'case': data['cases'][0]['index'] = 98
    if defect == 'empty': data['metrics'] = []
    if defect == 'nonfinite': data['metrics'][0]['actual'] = float('nan')
    if defect == 'settings': data['settings']['relative_tolerance'] = .1
    if defect == 'passed': data['passed'] = True
    if defect == 'acceptance': data['engineering_acceptance'] = 'ACCEPTED'
    if defect == 'channel': data['metrics'][0]['channel'] = 'unknown'
    if defect == 'negative': data['metrics'][0]['tolerance'] = -1
    paths[which].write_text(json.dumps(data))
    with pytest.raises(ValueError): render_diagnostic(*paths, tmp_path / 'diagnostic')
    assert not (tmp_path / 'diagnostic').exists()


def test_output_inside_pilot_is_rejected(tmp_path):
    paths = evidence(tmp_path)
    with pytest.raises(ValueError): render_diagnostic(*paths, paths[0].parent / 'diagnostic')
    assert not (paths[0].parent / 'diagnostic').exists()
