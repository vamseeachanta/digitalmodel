"""Pinned r5-to-r6 diagnostic annotation; no numerical adoption or native calls."""
from copy import deepcopy
from decimal import Decimal
from pathlib import Path

from digitalmodel.ansys.analysis_evidence import build_package, validate_package
from digitalmodel.ansys.analysis_records import canonical_bytes, digest_bytes, parse_json, verify_reference

CASE_ID = 'ocv-t60-p10-n4'
KIND = 'n4-recovery-note-1'
BASELINE_HASH = 'd8960b943de200287daeb1c9a0af88d659373dac9bbd73852c3a5dba3b6a2ec6'
OWNER_REVISION = '6bc9bf842643f928b297437e585cfc7d84081c8f'
REFERENCES = {
    'observation': dict(id='digitalmodel-data/reports/ansys-n4-recovery-2026-09-16.json',
        sha256='d38703f10f5cdd190802b57d1374ddf9f7b81d628e36ae77030a0e1418881c5f'),
    'report': dict(id='digitalmodel-data/reports/ansys-n4-recovery-2026-09-16.html',
        sha256='4269bf86501b787f2cb16563c36f5ef0f415fdabc4bca84f731bfed0c11a2316'),
    'review_archive': dict(id='digitalmodel-data/reports/reviews/ansys-n4-recovery-2026-09-16/evidence.zip',
        sha256='d519be855f8b069770598b5e79f968696ae76db6493fc7aebd2e816213fa1310'),
}
SOURCE_NAMES = ('analysis_recovery_note', 'analysis_matrix_publish', 'analysis_evidence', 'analysis_records')


def require(condition, message):
    if not condition:
        raise ValueError(message)


def source_inventory():
    return {'src/digitalmodel/ansys/'+n+'.py': digest_bytes(Path(__file__).with_name(n+'.py').read_bytes())
            for n in SOURCE_NAMES}


def _observation(baseline, resolver):
    for ref in REFERENCES.values():
        verify_reference(ref, resolver)
    report = parse_json(verify_reference(REFERENCES['observation'], resolver))
    case = baseline['cases'][9]
    require(report['case_id'] == CASE_ID == case['case_id'], 'observation case differs')
    require(report['run_id'] == case['observed_capture']['run_id'], 'observation run differs')
    evidence_hashes = {r['sha256'] for r in case['evidence']}
    require(report['source_evidence']['native.out']['sha256'] in evidence_hashes,
            'native output absent from baseline evidence')
    require(report['reference_sha256'] in evidence_hashes, 'reference absent from baseline evidence')
    require(report['recovery_status'] == 'COMPLETE' and report['recovery_errors'] == [], 'recovery differs')
    require(type(report['native_launches']) is int and report['native_launches'] == 0
            and report['engineering_qualified'] is False, 'diagnostic scope differs')
    assessment = report['assessment']; checks = assessment['checks']
    require(assessment['status'] == 'CONTINUE' and assessment['engineering_qualified'] is False,
            'assessment is not unqualified CONTINUE')
    require(len(checks) == 22 and sum(c['passed'] is True for c in checks) == 21
            and sum(c['passed'] is False for c in checks) == 1, 'assessment check coverage differs')
    failure = next(c for c in checks if c['passed'] is False)
    require(failure == dict(criterion='expected_zero', response=['outer_y120','sigma_r'],
            residual='0.01059709996930703', limit='0.010', passed=False), 'failed criterion differs')
    values = report['values']
    require(len(values) == 63 and sum(v['origin'] == 'native_export' for v in values) == 54
            and sum(v['origin'] == 'derived_von_mises' for v in values) == 9, 'recovery origin split differs')
    return report


def _supplement(baseline, report):
    return dict(kind=KIND, case_id=CASE_ID, previous_case_hash=baseline['cases'][9]['row_hash'],
        observed_utc=report['observed_utc'], run_id=report['run_id'], recovery_status='COMPLETE',
        assessment=deepcopy(report['assessment']), check_scope='21 primary y120 checks plus axial equilibrium; auxiliary/refinement not evaluated',
        auxiliary_unchecked_exceedances=[deepcopy(v) for v in report['values']
            if v['station_id'] in ('outer_y60','outer_y180') and v['quantity']=='sigma_r'
            and Decimal(v['value']) > Decimal('0.010')],
        auxiliary_comparator=dict(value='0.010', unit='MPa', scope='numeric comparison only; not governing N4 acceptance'),
        numerical_adoption='NOT_ADOPTED', engineering_qualified=False, native_launches=0,
        recovery_counts=dict(native_export=54, derived_von_mises=9), limits=deepcopy(report['limits']),
        evidence=deepcopy(REFERENCES), evidence_owner_revision=OWNER_REVISION,
        historical_snapshot_claims={k:report[k] for k in ('replay_driver_sha256','owner_manifest_sha256')},
        historical_snapshot_scope='as observed by pinned recovery report; not current runtime or owner-manifest identity',
        transformation_source_scope='n4-note-build-entry-only-1', transformation_source_files=source_inventory(),
        transformation_source_canonicalization='raw-sha256-v1',
        transformation_source_limits='Four build entry files only; transitive validators/imports are excluded; not dependency closure.')


def _finding():
    return dict(finding='n4-primary-radial-zero-limit', disposition='failed-diagnostic-not-adopted',
        affected_responses=[CASE_ID+':outer_y120.sigma_r'], blocks_engineering_qualification=True,
        finding_basis='Primary radial stress 0.01059709996930703 MPa exceeds 0.010 MPa; CONTINUE is not PASS.')


def build_recovery_note(baseline, resolver):
    validate_package(baseline)
    require(baseline['package_hash'] == BASELINE_HASH and baseline['revision'] == 'r5', 'fixed r5 baseline required')
    report = _observation(baseline, resolver)
    study = deepcopy(baseline); study.pop('package_hash')
    study.update(revision='r6', previous_package_hash=baseline['package_hash'],
                 diagnostic_supplement=_supplement(baseline, report))
    existing = {r['id'] for r in study['review_sources']}
    require(not existing.intersection(r['id'] for r in REFERENCES.values()), 'note evidence identity collision')
    study['review_sources'].extend(deepcopy(list(REFERENCES.values())))
    study['finding_ledger'].append(_finding())
    for case in study['cases']:
        case.pop('row_hash')
    result = build_package(study, resolver)
    require(canonical_bytes(result['cases']) == canonical_bytes(baseline['cases']), 'historical cases changed')
    validate_package(result)
    return result


def validate_recovery_note_transition(package, baseline, resolver):
    expected = build_recovery_note(baseline, resolver)
    require(canonical_bytes(package) == canonical_bytes(expected), 'recovery supplement transition differs')
