"""Opt-in retained-evidence report integration; never starts the solver."""
import copy
import json
import os
from pathlib import Path
import shutil

import pytest

from digitalmodel.workflows import installation_replay_report as reports


@pytest.mark.skipif(not os.environ.get('INSTALLATION_REPLAY_QA_DATA'), reason='Private retained fixture not configured')
def test_report_stage_with_retained_fixture(tmp_path, monkeypatch):
    data = Path(os.environ['INSTALLATION_REPLAY_QA_DATA'])
    reference_path = data / 'reports/installation-engineering-report-20260918-r6.json'
    reference = json.loads(reference_path.read_bytes())
    row = copy.deepcopy(next(r for r in reference['cases'] if r['index'] == 97))
    original = Path(row['run_dir'])
    root = tmp_path / 'retained-evidence-report-only-qa'
    root.mkdir()
    shutil.copytree(original / 'installation_traces', root / 'run/installation_traces')
    shutil.copyfile(original / 'run.json', root / 'run/run.json')
    (root / 'prepared').mkdir()
    shutil.copyfile(original / 'source/model.yml', root / 'prepared/model.yml')
    row['run_dir'] = str(root / 'run')
    monkeypatch.setattr(reports, '_collect_case', lambda *args: copy.deepcopy(row))
    (root / 'study').mkdir()
    shutil.copyfile(data / 'inputs/matrix.json', root / 'study/matrix.json')
    frozen = {'reference_summary': reference_path,
              'criteria': data / 'assumed-envelope-r2/assumed-project-criteria.json',
              'demo_config': data / 'assumed-envelope-r2/monitoring-demo-config.json',
              'report_config': data / 'reports/full-report-config-r7.json'}
    # build_reports requires frozen inputs inside the isolated output.
    (root / 'inputs').mkdir()
    for key, path in frozen.items():
        target = root / 'inputs' / (key + '.json')
        shutil.copyfile(path, target)
        frozen[key] = target
    matrix = json.loads((data / 'inputs/matrix.json').read_bytes())
    manifest = {'case_index': 97, 'comparison': {'absolute_tolerance': 1e-6, 'relative_tolerance': 1e-5},
                'pilot_limitations': ['RETAINED-EVIDENCE REPORT-ONLY QA; no fresh solve performed.']}
    result = reports.build_reports(root, frozen, matrix, manifest)
    assert result['comparison']['passed']
    assert (root / 'pilot-report.pdf').stat().st_size > 1000
    assert 'RETAINED-EVIDENCE REPORT-ONLY QA' in (root / 'pilot-report.html').read_text(encoding='utf-8')
    assert len(json.loads((root / 'summary.json').read_bytes())['cases']) == 1
    config = json.loads((root / 'pilot-config.json').read_bytes())
    inherited = json.loads(frozen['report_config'].read_bytes()).get('disclosures', [])
    for index, disclosure in enumerate(inherited):
        assert config['disclosures'][index] == 'Historical full-campaign context; not fresh pilot findings: ' + disclosure
