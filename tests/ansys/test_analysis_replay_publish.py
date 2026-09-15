"""Synthetic twelve-case replay/publication integration; no native or provider calls."""
from copy import deepcopy
import pytest

from digitalmodel.ansys.analysis_evidence import publish_package, load_package, build_package
from digitalmodel.ansys.analysis_records import canonical_bytes, validate_case
from digitalmodel.ansys.analysis_replay import derive_replay_case, build_replay_package
from digitalmodel.ansys.analysis_matrix_publish import publish_matrix
from tests.ansys.analysis_replay_fixture import full_fixture


def prepared(data):
    case = derive_replay_case(data['baseline'], data['replay_reference'], data['resolver'],
                             review_sha256=data['review_sha256'])
    path = data['root']/'deterministic-checks'
    path.write_bytes(canonical_bytes(case['replay_checks']))
    data['resolver'][case['check_reference']['id']] = path
    return build_replay_package(data['baseline'], data['resolver'],
        replay_reference=data['replay_reference'], revision='synthetic-replayed',
        code_revision='synthetic-reviewed', review_sha256=data['review_sha256'])


def publication_paths(data):
    owner = data['root']/'immutable'
    publish_package(data['baseline'], owner)
    manifest = data['root']/'canonical.json'
    manifest.write_bytes(canonical_bytes(data['baseline']))
    return owner, manifest


def test_full_synthetic_package_preserves_case_history_and_64_checks(tmp_path, monkeypatch):
    data = full_fixture(tmp_path, monkeypatch)
    package = prepared(data)
    assert package['coverage']['total_responses'] == 350
    assert package['coverage']['qualified_responses'] == 0
    case = package['cases'][8]
    assert case['capture_role'] == 'diagnostic_replay'
    assert len(case['replay_checks']['checks']) == 64
    assert case['original_assessment']['status'] == 'INCOMPLETE'
    assert all(row['value'] == '0' for row in case['responses'])
    for i in [*range(8), 9, 10, 11]:
        assert canonical_bytes(package['cases'][i]) == canonical_bytes(data['baseline']['cases'][i])


def test_validate_case_rejects_forged_replay_state(tmp_path, monkeypatch):
    case = deepcopy(full_fixture(tmp_path, monkeypatch)['baseline']['cases'][8])
    case.update(capture_role='diagnostic_replay', assessment_status='complete')
    with pytest.raises(ValueError, match='replay'):
        validate_case(case)


def test_publish_matrix_accepts_explicit_pinned_review_and_real_replay(tmp_path, monkeypatch):
    data = full_fixture(tmp_path, monkeypatch)
    package = prepared(data)
    owner, manifest = publication_paths(data)
    path = publish_matrix(package, data['baseline'], manifest, owner, data['resolver'],
                          review_sha256=data['review_sha256'])
    assert load_package(path) == load_package(manifest) == package


def test_publish_matrix_refuses_raw_tamper_after_builder(tmp_path, monkeypatch):
    data = full_fixture(tmp_path, monkeypatch)
    package = prepared(data)
    owner, manifest = publication_paths(data)
    before = manifest.read_bytes()
    reference = data['receipt']['raw']['ocv-zero-t60-n16/station_values.txt']
    data['resolver'][reference['id']].write_bytes(b'tampered after builder')
    with pytest.raises(ValueError):
        publish_matrix(package, data['baseline'], manifest, owner, data['resolver'],
                       review_sha256=data['review_sha256'])
    assert manifest.read_bytes() == before


def test_publish_matrix_refuses_wrong_external_review_pin(tmp_path, monkeypatch):
    data = full_fixture(tmp_path, monkeypatch)
    package = prepared(data)
    owner, manifest = publication_paths(data)
    with pytest.raises(ValueError, match='review'):
        publish_matrix(package, data['baseline'], manifest, owner, data['resolver'], review_sha256='a'*64)


def test_replay_preserves_six_historical_failed_responses(tmp_path, monkeypatch):
    data = full_fixture(tmp_path, monkeypatch)
    study = deepcopy(data['baseline'])
    study.pop('package_hash')
    for case in study['cases']:
        case.pop('row_hash')
    for response in study['cases'][0]['responses']:
        response['calculation_status'] = 'failed'
    data['baseline'] = build_package(study, data['resolver'])
    historical = deepcopy(data['baseline']['cases'][0])
    package = prepared(data)
    owner, manifest = publication_paths(data)
    publish_matrix(package, data['baseline'], manifest, owner, data['resolver'],
                   review_sha256=data['review_sha256'])
    published = load_package(manifest)
    assert published['cases'][0] == historical
    assert published['coverage']['assessment_failed_responses'] == 6
    assert published['coverage']['assessment_incomplete_cases'] == 0
    assert all(r['value'] is None for r in published['cases'][0]['responses'])
    assert published['cases'][8]['engineering_qualified'] is False
