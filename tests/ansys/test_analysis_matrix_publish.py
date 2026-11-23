"""Synthetic publication tests; no native solver or engineering acceptance."""
import copy
from pathlib import Path

import pytest

from digitalmodel.ansys.analysis_evidence import build_package, load_package, response_csv, publish_package
from digitalmodel.ansys.analysis_records import canonical_bytes, digest_bytes
from digitalmodel.ansys.analysis_matrix_publish import publish_matrix


@pytest.fixture
def records(tmp_path):
    from tests.ansys.test_analysis_evidence import intake
    study, resolver = intake.__wrapped__(tmp_path)
    baseline = build_package(study, resolver)
    next_study = copy.deepcopy(study)
    next_study.update(revision='r2', previous_package_hash=baseline['package_hash'])
    package = build_package(next_study, resolver)
    manifest = tmp_path/'canonical'/'manifest.json'
    manifest.parent.mkdir()
    manifest.write_bytes(canonical_bytes(baseline))
    publish_package(baseline, tmp_path/'owner')
    return package, baseline, manifest, tmp_path/'owner', resolver


def test_publish_and_idempotent_refresh_preserve_baseline(records):
    package, baseline, manifest, root, resolver = records
    path = publish_matrix(package, baseline, manifest, root, resolver)
    assert load_package(path) == load_package(manifest) == package
    assert load_package(root/'dataset-1'/'r1.json') == baseline
    assert manifest.with_name('responses.csv').read_text() == response_csv(package)
    assert publish_matrix(*records) == path
    assert sorted(p.name for p in (root/'dataset-1').glob('*.json')) == ['r1.json','r2.json']


def test_changed_baseline_is_not_replaced(records):
    package, baseline, manifest, root, resolver = records
    manifest.write_text('changed')
    with pytest.raises(ValueError):
        publish_matrix(*records)
    assert manifest.read_text() == 'changed'


def test_changed_evidence_blocks_publication(records):
    package, baseline, manifest, root, resolver = records
    resolver['capture'].write_text('changed')
    with pytest.raises(ValueError, match='evidence changed'):
        publish_matrix(*records)
    assert load_package(manifest) == baseline


def test_existing_integration_lock_is_preserved(records):
    package, baseline, manifest, root, resolver = records
    directory = root/'dataset-1'
    directory.mkdir(parents=True, exist_ok=True)
    lock = directory/'.integration.lock'
    lock.write_text('other owner')
    with pytest.raises(FileExistsError):
        publish_matrix(*records)
    assert lock.read_text() == 'other owner'


def test_crash_after_revision_publish_resumes_without_new_revision(records, monkeypatch):
    import digitalmodel.ansys.analysis_matrix_publish as module
    original = module._replace
    monkeypatch.setattr(module, '_replace', lambda *a: (_ for _ in ()).throw(OSError('crash')))
    with pytest.raises(OSError, match='crash'):
        publish_matrix(*records)
    package, baseline, manifest, root, resolver = records
    assert load_package(manifest) == baseline
    assert load_package(root/'dataset-1'/'r2.json') == package
    monkeypatch.setattr(module, '_replace', original)
    publish_matrix(*records)
    assert load_package(manifest) == package


def test_historical_row_mutation_is_refused(records):
    package, baseline, manifest, root, resolver = records
    study = copy.deepcopy(package)
    study.pop('package_hash')
    for case in study['cases']:
        case.pop('row_hash')
    study['cases'][0]['author'] = 'changed'
    damaged = build_package(study, resolver)
    with pytest.raises(ValueError, match='historical'):
        publish_matrix(damaged, baseline, manifest, root, resolver)


def test_failed_temporary_fsync_does_not_leave_partial(tmp_path, monkeypatch):
    import digitalmodel.ansys.analysis_matrix_publish as module
    monkeypatch.setattr(module.os, 'fsync', lambda *a: (_ for _ in ()).throw(OSError('fsync')))
    with pytest.raises(OSError, match='fsync'):
        module._replace(tmp_path/'target.json', b'bytes')
    assert list(tmp_path.iterdir()) == []


def test_csv_failure_retains_committed_manifest_and_repairs(records, monkeypatch):
    import digitalmodel.ansys.analysis_matrix_publish as module
    original = module._replace
    def fail_csv(target, raw):
        if target.name == 'responses.csv':
            raise OSError('csv failure')
        original(target, raw)
    monkeypatch.setattr(module, '_replace', fail_csv)
    with pytest.raises(OSError, match='csv failure'):
        publish_matrix(*records)
    package, baseline, manifest, root, resolver = records
    assert load_package(manifest) == package
    assert load_package(root/'dataset-1'/'r1.json') == baseline
    monkeypatch.setattr(module, '_replace', original)
    publish_matrix(*records)
    assert manifest.with_name('responses.csv').read_text() == response_csv(package)


def test_empty_owner_root_is_not_an_authoritative_store(records, tmp_path):
    package, baseline, manifest, root, resolver = records
    with pytest.raises(ValueError, match='existing owner'):
        publish_matrix(package, baseline, manifest, tmp_path/'other-owner', resolver)
    assert not (tmp_path/'other-owner'/'dataset-1'/'r1.json').exists()


@pytest.mark.parametrize('key,value', [
    ('coverage', {'qualified_responses': 999}), ('code_revision', 'forged'),
    ('source_revision', 'a' * 40), ('generated_at', '2099-01-01T00:00:00Z'),
])
def test_unchanged_cases_cannot_publish_forged_study_metadata(records, key, value):
    package, baseline, manifest, root, resolver = records
    study = copy.deepcopy(package)
    study.pop('package_hash')
    for case in study['cases']:
        case.pop('row_hash')
    study[key] = value
    damaged = build_package(study, resolver)
    with pytest.raises(ValueError, match='unchanged cases'):
        publish_matrix(damaged, baseline, manifest, root, resolver)
    assert load_package(manifest) == baseline
