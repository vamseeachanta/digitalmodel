# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.
"""Exercise evidence boundaries using synthetic, repository-local sources."""

import copy
import hashlib
import importlib.util
import json
from pathlib import Path
from urllib.parse import urlsplit

import pytest

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / 'scripts/reporting/capability_report_inventory.py'
spec = importlib.util.spec_from_file_location('capability_report_inventory', SCRIPT)
inv = importlib.util.module_from_spec(spec)
spec.loader.exec_module(inv)


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


@pytest.fixture
def sample(tmp_path, monkeypatch):
    root = tmp_path / 'checkout'
    contents = {
        'docs/reports/alpha.html': '<h1>Indicative analysis</h1>',
        'scripts/alpha.py': '# synthetic generator\n',
        'tests/test_alpha.py': '# synthetic test\n',
        'data/alpha-result.json': '{"qualification": "INDICATIVE"}',
        'docs/criteria.txt': 'An accessible result record; no engineering qualification.',
    }
    for name, text in contents.items():
        target = root / name
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(text, encoding='utf-8', newline='\n')
    monkeypatch.setattr(inv, 'tracked_files', lambda _: set(contents))
    monkeypatch.setattr(inv, 'checkout_revision', lambda _: 'a' * 40)
    row = {
        'id': 'alpha', 'report_path': 'docs/reports/alpha.html',
        'generator_path': 'scripts/alpha.py', 'test_paths': ['tests/test_alpha.py'],
        'capability': 'Synthetic flow', 'evidence_status': 'INDICATIVE',
        'qualification': 'INDICATIVE',
        'evidence_refs': [{'path': 'data/alpha-result.json', 'kind': 'result'},
                          {'path': 'docs/criteria.txt', 'kind': 'criterion'}],
        'criterion': 'Accessible record only; engineering accuracy is not assessed.',
        'observation': 'The result declares an indicative basis.',
        'limitations': ['Synthetic evidence; no engineering qualification.'],
        'proposed_card_link': 'docs/reports/alpha.html',
        'source_digests': {name: digest(root / name) for name in contents},
    }
    observations = {
        'source_revision': 'a' * 40, 'observed_at': '2026-10-24T00:00:00Z',
        'scope_roots': ['docs/reports'], 'reports': [row],
        'revision_history': [{'revision': '0', 'date': '2026-10-24',
                              'change': 'Initial synthetic review.'}],
    }
    return root, observations


def test_missing_evidence_is_a_named_gap(sample):
    root, observations = sample
    row = observations['reports'][0]
    row['evidence_status'] = 'MISSING'
    row['qualification'] = 'UNKNOWN'
    row['evidence_refs'] = []
    row['source_digests'] = {key: val for key, val in row['source_digests'].items()
                             if key not in ('data/alpha-result.json', 'docs/criteria.txt')}
    result = inv.build_inventory(root, observations)
    assert result['generated']['counts']['MISSING'] == 1
    assert any(g['report_id'] == 'alpha' and g['kind'] == 'missing-evidence'
               for g in result['gaps'])


@pytest.mark.parametrize('field', ['report_path', 'generator_path', 'proposed_card_link'])
def test_dangling_links_rejected(sample, field):
    root, observations = sample
    observations['reports'][0][field] = 'docs/reports/absent.html'
    with pytest.raises(inv.InventoryError, match='unavailable'):
        inv.build_inventory(root, observations)


@pytest.mark.parametrize('path', ['../outside.html', '/outside.html',
                                  'docs/../outside.html', 'docs\\outside.html',
                                  'https://invalid.example/report.html'])
def test_path_escape_rejected(sample, path):
    root, observations = sample
    observations['reports'][0]['report_path'] = path
    with pytest.raises(inv.InventoryError, match='path'):
        inv.build_inventory(root, observations)


def test_symlink_escape_rejected(sample, tmp_path):
    root, observations = sample
    outside = tmp_path / 'outside.html'
    outside.write_text('outside', encoding='utf-8', newline='\n')
    (root / 'docs/reports/alpha.html').unlink()
    (root / 'docs/reports/alpha.html').symlink_to(outside)
    with pytest.raises(inv.InventoryError, match='path'):
        inv.build_inventory(root, observations)


def test_duplicate_ids_rejected(sample):
    root, observations = sample
    observations['reports'].append(copy.deepcopy(observations['reports'][0]))
    with pytest.raises(inv.InventoryError, match='duplicate'):
        inv.build_inventory(root, observations)


@pytest.mark.parametrize('content', ['{broken', '[]', '{"observations": []}',
                                    '{"reports": [], "reports": []}'])
def test_malformed_json_rejected(tmp_path, content):
    path = tmp_path / 'observations.json'
    path.write_text(content, encoding='utf-8', newline='\n')
    with pytest.raises(inv.InventoryError):
        inv.read_observations(path)


def test_mismatched_digest_rejected(sample):
    root, observations = sample
    observations['reports'][0]['source_digests']['scripts/alpha.py'] = '0' * 64
    with pytest.raises(inv.InventoryError, match='digest'):
        inv.build_inventory(root, observations)


def test_source_change_rejected(sample):
    root, observations = sample
    (root / 'data/alpha-result.json').write_text(
        '{"qualification":"UNKNOWN"}', encoding='utf-8', newline='\n')
    with pytest.raises(inv.InventoryError, match='stale'):
        inv.build_inventory(root, observations)


@pytest.mark.parametrize('status,qualification', [('QUALIFIED', 'INDICATIVE'),
                                                 ('RETAINED', 'QUALIFIED')])
def test_indicative_result_cannot_be_promoted_to_qualified(sample, status, qualification):
    root, observations = sample
    observations['reports'][0].update(evidence_status=status, qualification=qualification)
    with pytest.raises(inv.InventoryError, match='status|qualification'):
        inv.build_inventory(root, observations)


def test_retained_requires_result_and_criterion(sample):
    root, observations = sample
    row = observations['reports'][0]
    row['evidence_status'] = 'RETAINED'
    row['evidence_refs'] = [row['evidence_refs'][1]]
    with pytest.raises(inv.InventoryError, match='result'):
        inv.build_inventory(root, observations)


def test_retained_is_record_presence_not_qualification(sample):
    root, observations = sample
    observations['reports'][0]['evidence_status'] = 'RETAINED'
    result = inv.build_inventory(root, observations)
    assert result['reports'][0]['qualification'] == 'INDICATIVE'
    assert result['generated']['counts']['RETAINED'] == 1


def test_implementation_is_not_an_independent_reference(sample):
    root, observations = sample
    row = observations['reports'][0]
    row['evidence_refs'][1] = {'path': 'scripts/alpha.py', 'kind': 'reference'}
    with pytest.raises(inv.InventoryError, match='circular'):
        inv.build_inventory(root, observations)


def test_missing_digest_rejected(sample):
    root, observations = sample
    del observations['reports'][0]['source_digests']['tests/test_alpha.py']
    with pytest.raises(inv.InventoryError, match='digest'):
        inv.build_inventory(root, observations)


def test_stable_order_and_observation_round_trip(sample, tmp_path):
    root, observations = sample
    second = copy.deepcopy(observations['reports'][0])
    second['id'] = 'beta'
    observations['reports'].insert(0, second)
    first = inv.build_inventory(root, observations)
    inv.write_outputs(first, tmp_path / 'staging')
    saved = inv.read_observations(tmp_path / 'staging/capability-report-inventory.json')
    rebuilt = inv.build_inventory(root, saved)
    assert first == rebuilt
    assert [r['id'] for r in first['reports']] == ['alpha', 'beta']
    assert rebuilt['observations']['reports'][0]['observation'] == second['observation']


def test_relative_output_links_and_escaped_text(sample, tmp_path):
    root, observations = sample
    observations['reports'][0]['limitations'] = ['<script>synthetic</script>']
    result = inv.build_inventory(root, observations)
    page = inv.render_html(result)
    assert '../../domains/' not in page
    assert '../../scripts/alpha.py' in page
    assert '../reports/alpha.html' in page
    assert '<script>synthetic</script>' not in page
    assert '&lt;script&gt;synthetic&lt;/script&gt;' in page
    from html.parser import HTMLParser

    class Links(HTMLParser):
        def handle_starttag(self, tag, attrs):
            for key, value in attrs:
                if key == 'href':
                    assert not urlsplit(value).scheme
                    assert not Path(value).is_absolute()
                    assert (root / 'docs/audits' / value).resolve().is_file()

    Links().feed(page)
    inv.write_outputs(result, tmp_path / 'first')
    inv.write_outputs(result, tmp_path / 'second')
    assert (tmp_path / 'first/capability-report-inventory.html').read_bytes() == (
        tmp_path / 'second/capability-report-inventory.html').read_bytes()


def test_untracked_private_and_audit_inputs_excluded(sample, monkeypatch):
    root, observations = sample
    paths = ['docs/reports/untracked.html', 'docs/reports/private/secret.html',
             'docs/audits/capability-report-inventory.html']
    for path in paths:
        target = root / path
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text('excluded', encoding='utf-8', newline='\n')
    tracked = set(observations['reports'][0]['source_digests']) | set(paths[1:])
    monkeypatch.setattr(inv, 'tracked_files', lambda _: tracked)
    observations['scope_roots'].append('docs/audits')
    result = inv.build_inventory(root, observations)
    assert result['generated']['discovered_report_count'] == 1
    assert not any('secret' in path for path in result['generated']['source_digests'])


def test_changed_discovery_is_a_gap(sample, monkeypatch):
    root, observations = sample
    (root / 'docs/reports/beta.html').write_text(
        'unreviewed', encoding='utf-8', newline='\n')
    paths = set(observations['reports'][0]['source_digests']) | {'docs/reports/beta.html'}
    monkeypatch.setattr(inv, 'tracked_files', lambda _: paths)
    result = inv.build_inventory(root, observations)
    assert result['generated']['unreviewed_report_count'] == 1
    assert any(g['kind'] == 'coverage' for g in result['gaps'])


def test_saved_inventory_rebuilds_and_counts_agree():
    saved = ROOT / 'docs/audits/capability-report-inventory.json'
    result = inv.build_inventory(ROOT, inv.read_observations(saved))
    discovered = inv.discover_reports(
        ROOT, result['observations']['scope_roots'], inv.tracked_files(ROOT))
    assert 'docs/reports/modules/capping_stack/index.html' in discovered
    rebuild = ('Run scripts/reporting/capability_report_inventory.py --root . '
               '--observations docs/audits/capability-report-inventory.json '
               '--out-dir docs/audits')
    assert result == json.loads(saved.read_text(encoding='utf-8')), rebuild
    assert inv.render_html(result) == saved.with_suffix('.html').read_text(
        encoding='utf-8'), rebuild
    assert sum(result['generated']['counts'].values()) == len(result['reports'])
    cfd = next(r for r in result['reports'] if r['id'] == 'cfd-calm-water')
    assert cfd['evidence_status'] == 'INDICATIVE'
    assert cfd['qualification'] == 'INDICATIVE'


@pytest.mark.parametrize('kind', ['reference', 'criterion'])
def test_calculation_module_cannot_be_an_independent_reference(sample, monkeypatch, kind):
    root, observations = sample
    path = 'src/calculation.py'
    (root / 'src').mkdir()
    (root / path).write_text('# synthetic calculation', encoding='utf-8', newline='\n')
    tracked = set(observations['reports'][0]['source_digests']) | {path}
    monkeypatch.setattr(inv, 'tracked_files', lambda _: tracked)
    row = observations['reports'][0]
    row['evidence_refs'].append({'path': path, 'kind': kind})
    row['source_digests'][path] = digest(root / path)
    with pytest.raises(inv.InventoryError, match='circular'):
        inv.build_inventory(root, observations)


def test_retained_indicative_record_still_has_a_next_action(sample):
    root, observations = sample
    observations['reports'][0]['evidence_status'] = 'RETAINED'
    result = inv.build_inventory(root, observations)
    assert any(g['report_id'] == 'alpha' and g['kind'] == 'qualification'
               for g in result['gaps'])


@pytest.mark.parametrize('path', ['docs/reports/untracked.html', 'docs/private/result.json',
                                  'docs/audits/result.json'])
def test_evidence_must_be_tracked_and_public(sample, monkeypatch, path):
    root, observations = sample
    target = root / path
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_text('unavailable', encoding='utf-8', newline='\n')
    row = observations['reports'][0]
    tracked = set(row['source_digests'])
    if 'untracked' not in path:
        tracked.add(path)
    monkeypatch.setattr(inv, 'tracked_files', lambda _: tracked)
    row['evidence_refs'].append({'path': path, 'kind': 'result'})
    row['source_digests'][path] = digest(target)
    with pytest.raises(inv.InventoryError, match='unavailable'):
        inv.build_inventory(root, observations)


def test_cli_rejection_writes_no_outputs(sample, tmp_path):
    root, observations = sample
    observations['reports'][0]['source_digests']['scripts/alpha.py'] = '0' * 64
    source = tmp_path / 'review.json'
    source.write_text(json.dumps(observations), encoding='utf-8', newline='\n')
    output = tmp_path / 'rejected-output'
    with pytest.raises(SystemExit) as exc:
        inv.main(['--root', str(root), '--observations', str(source), '--out-dir', str(output)])
    assert exc.value.code == 2
    assert not output.exists()


def test_cli_discovery_never_guesses_qualification(sample, tmp_path):
    root, _ = sample
    output = tmp_path / 'discovery-output'
    assert inv.main(['--root', str(root), '--out-dir', str(output)]) == 0
    saved = json.loads((output / 'capability-report-inventory.json').read_text(
        encoding='utf-8'))
    assert saved['generated']['counts']['UNKNOWN'] == 1
    assert saved['reports'][0]['qualification'] == 'UNKNOWN'
    assert saved['reports'][0]['generator_path'] is None


def test_cli_reads_observations_before_replacing_outputs(sample, tmp_path):
    root, observations = sample
    output = tmp_path / 'same-output'
    inv.write_outputs(inv.build_inventory(root, observations), output)
    source = output / 'capability-report-inventory.json'
    expected = source.read_bytes()
    assert inv.main(['--root', str(root), '--observations', str(source),
                     '--out-dir', str(output)]) == 0
    assert source.read_bytes() == expected


def test_observation_revision_survives_later_commits(sample, monkeypatch):
    root, observations = sample
    first = inv.build_inventory(root, observations)
    monkeypatch.setattr(inv, 'checkout_revision', lambda _: 'b' * 40)
    assert inv.build_inventory(root, observations) == first


def test_output_symlink_cannot_overwrite_other_files(sample, tmp_path):
    root, observations = sample
    output = tmp_path / 'output'
    output.mkdir()
    protected = tmp_path / 'protected.json'
    protected.write_text('preserve', encoding='utf-8', newline='\n')
    (output / 'capability-report-inventory.json').symlink_to(protected)
    with pytest.raises(inv.InventoryError, match='symlink'):
        inv.write_outputs(inv.build_inventory(root, observations), output)
    assert protected.read_text(encoding='utf-8') == 'preserve'
