# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Check the retained reliability benchmark against its declared criteria."""
import hashlib
import importlib.util
import json
from datetime import datetime
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
DATA = ROOT / 'docs/domains/asset-integrity/reliability-benchmark'


def load_builder():
    spec = importlib.util.spec_from_file_location('benchmark', DATA / 'build.py')
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_published_and_fatigue_agreement():
    builder = load_builder()
    result = json.loads((DATA / 'results.json').read_text())
    criteria = json.loads((DATA / 'criteria.json').read_text())
    assert builder.validate(result, criteria) == []


def test_criteria_precedes_run_and_matches_bytes():
    record = json.loads((DATA / 'results.json').read_text())
    assert record['criteria_sha256'] == hashlib.sha256(
        (DATA / 'criteria.json').read_bytes()).hexdigest()
    assert datetime.fromisoformat(record['criteria_recorded_at']) < datetime.fromisoformat(
        record['started_at'])
    log = [json.loads(line) for line in (DATA / 'run-record.jsonl').read_text().splitlines()]
    assert log[0]['criteria_sha256'] == record['criteria_sha256']
    assert log[0]['criteria_recorded_at'] == record['criteria_recorded_at']
    assert log[1]['started_at'] == record['started_at']


def test_report_rebuild_is_identical_and_matches_results():
    builder = load_builder()
    criteria = json.loads((DATA / 'criteria.json').read_text())
    result = json.loads((DATA / 'results.json').read_text())
    first = builder.render(result, criteria)
    assert first == builder.render(result, criteria)
    assert first.encode() == (DATA / 'report.html').read_bytes()
    for row in result['fatigue']:
        for key in ('reference_pf', 'native_pf', 'openturns_pf', 'native_mc_pf', 'openturns_mc_pf'):
            assert builder.number(row[key]) in first


def test_revision_history_immediately_after_summary():
    page = (DATA / 'report.html').read_text()
    assert page.index('Summary') < page.index('Revision history') < page.index('1. Introduction')
    criteria = json.loads((DATA / 'criteria.json').read_text())
    for entry in criteria['revision_history']:
        assert entry['date'] in page
        assert entry['change'] in page


def test_manifest_digests():
    manifest = json.loads((DATA / 'manifest.json').read_text())
    for name, digest in manifest['sha256'].items():
        assert hashlib.sha256((DATA / name).read_bytes()).hexdigest() == digest


def test_validator_rejects_altered_result():
    builder = load_builder()
    criteria = json.loads((DATA / 'criteria.json').read_text())
    result = json.loads((DATA / 'results.json').read_text())
    for field in ('pf', 'beta'):
        changed = json.loads(json.dumps(result))
        changed['published'][field] += 0.1
        assert builder.validate(changed, criteria)
    for method in ('native', 'openturns'):
        for field in ('pf', 'beta', 'mc_pf'):
            changed = json.loads(json.dumps(result))
            changed['fatigue'][0][method+'_'+field] += 0.1
            assert builder.validate(changed, criteria)
        changed = json.loads(json.dumps(result))
        changed['sobol'][method]['sn'] += 0.1
        assert builder.validate(changed, criteria)
    changed = json.loads(json.dumps(result))
    changed['fatigue'][0]['reference_pf'] += 0.1
    assert builder.validate(changed, criteria)
    changed = json.loads(json.dumps(result))
    changed['sobol']['native']['sn'] = float('nan')
    assert builder.validate(changed, criteria)


def test_report_has_no_external_resources():
    from html.parser import HTMLParser

    class Resources(HTMLParser):
        def handle_starttag(self, tag, attrs):
            if tag in ('script', 'link', 'img', 'iframe', 'source'):
                assert not any(key in ('src', 'href') for key, _ in attrs)

    Resources().feed((DATA / 'report.html').read_text())
