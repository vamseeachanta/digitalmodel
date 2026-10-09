"""Synthetic reference-lineage mutations; no native or reference generation."""
import json
import pytest
from digitalmodel.ansys import analysis_replay as replay
from digitalmodel.ansys import analysis_replay_inputs as inputs
from digitalmodel.ansys.analysis_records import canonical_bytes, digest_bytes
from tests.ansys.test_analysis_replay import synthetic_evaluation_inputs, review_fixture


def bound_inputs():
    resolved = synthetic_evaluation_inputs()
    raw = resolved['documents']['reference']
    resolved['documents']['runtime_manifest'] = canonical_bytes(dict(reference='reference.json',
        artifacts=[dict(path='reference.json', sha256=digest_bytes(raw), bytes=len(raw))]))
    resolved['raw']['outcome.json'] = canonical_bytes(dict(records=[dict(
        case_id=inputs.CASE_ID, reference_sha256=digest_bytes(raw))]))
    return resolved


@pytest.mark.parametrize('fault', ['missing_manifest_reference', 'changed_manifest_reference',
    'missing_artifact', 'duplicate_artifact', 'changed_artifact_hash', 'wrong_size',
    'missing_outcome_reference', 'changed_outcome_reference', 'wrong_case', 'changed_document'])
def test_evaluator_refuses_reference_binding_faults(fault):
    resolved = bound_inputs()
    manifest = json.loads(resolved['documents']['runtime_manifest'])
    outcome = json.loads(resolved['raw']['outcome.json'])
    if fault == 'missing_manifest_reference':
        manifest.pop('reference')
    elif fault == 'changed_manifest_reference':
        manifest['reference'] = 'other.json'
    elif fault == 'missing_artifact':
        manifest['artifacts'] = []
    elif fault == 'duplicate_artifact':
        manifest['artifacts'] *= 2
    elif fault == 'changed_artifact_hash':
        manifest['artifacts'][0]['sha256'] = 'a' * 64
    elif fault == 'wrong_size':
        manifest['artifacts'][0]['bytes'] += 1
    elif fault == 'missing_outcome_reference':
        outcome['records'][0].pop('reference_sha256')
    elif fault == 'changed_outcome_reference':
        outcome['records'][0]['reference_sha256'] = 'a' * 64
    elif fault == 'wrong_case':
        outcome['records'][0]['case_id'] = 'other'
    else:
        resolved['documents']['reference'] += b' '
    resolved['documents']['runtime_manifest'] = canonical_bytes(manifest)
    resolved['raw']['outcome.json'] = canonical_bytes(outcome)
    with pytest.raises(ValueError, match='reference'):
        replay._evaluate(resolved)


def test_evaluator_passes_observed_byte_hash_to_native_validator(monkeypatch):
    resolved = bound_inputs()
    seen = []
    original = replay.validate_native_evidence
    def capture(case, artifacts, approved, observed):
        seen.append((approved, observed))
        return original(case, artifacts, approved, observed)
    monkeypatch.setattr(replay, 'validate_native_evidence', capture)
    values, checks = replay._evaluate(resolved)
    assert len(values) == len(checks) == 64
    assert seen == [(inputs.REFERENCE_HASH, digest_bytes(resolved['documents']['reference']))]


@pytest.mark.parametrize('files', [None, {}, [None], [[]], [{}],
    [{'path': 1, 'sha256': 'a' * 64}], [{'path': 'x', 'sha256': None}]])
def test_malformed_receipt_files_raise_value_error(files):
    review = review_fixture()
    review['files'] = files
    with pytest.raises(ValueError):
        inputs.validate_review(review, {'src/digitalmodel/ansys/example.py': 'a' * 64})
