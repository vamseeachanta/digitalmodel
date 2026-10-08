"""Bounded unattempted cylinder coverage in the existing diagnostic dataset."""
from copy import deepcopy

from digitalmodel.ansys.analysis_evidence import build_package, validate_package
from digitalmodel.ansys.analysis_records import canonical_bytes, nonempty


PENDING_CASE_IDS = ('ocv-zero-t60-n16', 'ocv-t60-p10-n4',
                    'ocv-t60-p10-n8', 'ocv-t60-p10-n16')
PENDING_RESPONSE_UNITS = {
    f'{radius}_y{height}.{quantity}': 'mm' if quantity.startswith('u_') else 'MPa'
    for radius in ('inner', 'middle', 'outer') for height in (60, 120, 180)
    for quantity in ('sigma_r', 'sigma_theta', 'sigma_z', 'tau_rz', 'sigma_vm', 'u_r', 'u_z')
}
PENDING_RESPONSE_UNITS['support.RFY'] = 'N'


def _validate_pending(cases):
    if not isinstance(cases, list) or len(cases) != 4:
        raise ValueError('exactly four pending cases required')
    if [case.get('case_id') for case in cases] != list(PENDING_CASE_IDS):
        raise ValueError('pending case membership/order differs from frozen sequence')
    for case in cases:
        if case.get('capture_role') != 'pending_native':
            raise ValueError('pending native capture role required')
        rows = case.get('responses', [])
        if len(rows) != 64 or len({row.get('name') for row in rows}) != 64:
            raise ValueError('exactly 64 unique pending responses required')
        if {row.get('name'): row.get('unit') for row in rows} != PENDING_RESPONSE_UNITS:
            raise ValueError('pending response identities or units differ')


def build_pending_package(existing, pending_cases, resolver, *, revision,
                          code_revision, source_revision, code_files=None, method_revision=None):
    """Return a validated 12-case package; do not publish or infer native evidence."""
    validate_package(existing)
    if (existing['dataset_id'] != 'ansys-retained-evidence'
            or len(existing['cases']) != 8
            or sum(len(case['responses']) for case in existing['cases']) != 94):
        raise ValueError('expected existing eight-case, 94-response dataset')
    if revision == existing['revision']:
        raise ValueError('new explicit revision required')
    _validate_pending(pending_cases)
    study = deepcopy(existing)
    study.pop('package_hash')
    study['previous_package_hash'] = existing['package_hash']
    study['revision'] = revision
    study['code_revision'] = nonempty(code_revision, 'code revision')
    if 'source_revision' in existing:
        study['baseline_source_revision'] = existing['source_revision']
    study['source_revision'] = nonempty(source_revision, 'source revision')
    if 'code_files' in study:
        study['baseline_code_files'] = study.pop('code_files')
    if code_files is not None:
        study['code_files'] = deepcopy(code_files)
    study['baseline_method_revision'] = existing['method_revision']
    study['method_revision'] = nonempty(
        'pending-native-matrix-1' if method_revision is None else method_revision,
        'method revision')
    study.setdefault('coverage', {}).update(
        pending_cases=4, pending_responses=256, total_cases=12, total_responses=350)
    study['cases'].extend(deepcopy(pending_cases))
    study['expected_cases'].extend(PENDING_CASE_IDS)
    for case in study['cases']:
        case.pop('row_hash', None)
    package = build_package(study, resolver)
    if canonical_bytes(package['cases'][:8]) != canonical_bytes(existing['cases']):
        raise ValueError('historical case bytes changed')
    validate_package(package)
    return package
