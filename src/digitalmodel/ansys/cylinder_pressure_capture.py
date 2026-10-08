"""Read-only fixed N4/N8 capture checks; no pressure numerical acceptance or launch."""
from pathlib import Path
import stat

from digitalmodel.ansys import cylinder_adapter as helpers
from digitalmodel.ansys.cylinder_benchmark import build_case
from digitalmodel.ansys.cylinder_results_audit import parse_configuration
from digitalmodel.ansys.cylinder_results_cdb import parse_model_cdb, verify_model
from digitalmodel.ansys.cylinder_results_diagnostics import classify_diagnostics
from digitalmodel.ansys.cylinder_results_native_status import verify_case_titles
from digitalmodel.ansys.cylinder_results_state import parse_state_values, validate_precision_witness

CASE_ID = 'ocv-t60-p10-n4'
INTERMEDIATE_CASE_ID = 'ocv-t60-p10-n8'
EXPECTED = ('file.db', 'file.rst', 'file.mntr', 'file.err', 'model.cdb',
    CASE_ID+'.inp', CASE_ID+'.out', 'state_values.txt', 'station_values.txt',
    'support_reactions.txt', 'precision_witness.txt', 'stdout.bin', 'stderr.bin')
ALLOW_EMPTY = {'file.err', 'stdout.bin', 'stderr.bin'}
PARSED_FILES = {'model.cdb', CASE_ID+'.inp', CASE_ID+'.out', 'state_values.txt',
                'precision_witness.txt', 'file.err', 'stdout.bin', 'stderr.bin'}
UNVERIFIED_EVIDENCE = tuple(sorted(set(EXPECTED) - PARSED_FILES)) + (
    'pressure_loads_and_support_conditions', 'stress_and_reaction_recovery')
EXECUTION_FIELDS = ('return_code', 'timed_out', 'owned_processes_remaining',
    'containment_verified', 'evidence_complete', 'settlement_required', 'streams_finalized')


def _capture_files(case_id):
    if not isinstance(case_id, str) or case_id not in (CASE_ID, INTERMEDIATE_CASE_ID):
        raise ValueError('Only exact fixed N4/N8 pressure capture identities are supported')
    names = {CASE_ID+'.inp': case_id+'.inp', CASE_ID+'.out': case_id+'.out'}
    return tuple(names.get(name, name) for name in EXPECTED), {
        names.get(name, name) for name in PARSED_FILES}


def _redirected(path):
    info = path.lstat()
    return stat.S_ISLNK(info.st_mode) or bool(getattr(info, 'st_file_attributes', 0) & 0x400)


def _inventory_row(root, name, expected=EXPECTED, parsed=PARSED_FILES):
    row = {'path': name, 'role': 'expected_capture' if name in expected else 'auxiliary'}
    try:
        path = root/name
        if _redirected(path):
            return dict(row, status='redirected'), None
        link_count = path.stat().st_nlink
        if link_count > 1 and path.is_file():
            return dict(row, status='hardlinked', link_count=link_count), None
        retain = name in parsed
        raw, metadata = helpers._read_owned(root, name, retain=retain)
        status = 'empty' if metadata['bytes'] == 0 and name in expected and name not in ALLOW_EMPTY else 'retained'
        return dict(row, **metadata, status=status), raw if retain else None
    except FileNotFoundError:
        return dict(row, status='missing'), None
    except OSError as error:
        return dict(row, status='read_error', error_type=type(error).__name__), None
    except ValueError as error:
        return dict(row, status='invalid', error_type=type(error).__name__), None


def inventory_capture(directory, deck_basename=CASE_ID+'.inp'):
    """Return per-file rows and readable bytes, including incomplete/extra files."""
    if deck_basename not in (CASE_ID+'.inp', INTERMEDIATE_CASE_ID+'.inp'):
        raise ValueError('Only exact fixed N4/N8 deck basenames are in capture scope')
    expected, parsed = _capture_files(deck_basename[:-4])
    root = Path(directory).absolute()
    try:
        for parent in (root, *root.parents):
            if _redirected(parent):
                raise ValueError('Capture root or ancestor is redirected')
        if not root.is_dir():
            raise ValueError('Existing capture directory required')
        root = root.resolve(strict=True)
    except OSError as error:
        raise ValueError('Capture root unavailable: '+type(error).__name__) from error
    names = set(expected)
    rows, artifacts = [], {}
    try:
        names.update(path.name for path in root.iterdir())
    except OSError as error:
        rows.append(dict(path='.', role='directory', status='read_error', error_type=type(error).__name__))
    for name in sorted(names):
        row, raw = _inventory_row(root, name, expected, parsed)
        rows.append(row)
        if raw is not None:
            artifacts[name] = raw
    return rows, artifacts


def _check(checks, name, function, *args, **kwargs):
    try:
        result = function(*args, **kwargs)
        checks.append(dict(check=name, status='COMPLETE'))
        return result
    except (ValueError, OSError, TypeError, KeyError, UnicodeError) as error:
        checks.append(dict(check=name, status='INCOMPLETE', error_type=type(error).__name__,
                           error_class='dependency_or_contract_error' if isinstance(error, (TypeError, KeyError))
                           else 'evidence_error',
                           reason=str(error)))
        return None


def _equal_deck(raw, expected):
    if raw != expected:
        raise ValueError('Retained input differs from selected fixed pressure deck')


def _model(raw, case):
    verify_model(parse_model_cdb(raw), case)


def _diagnostics(output, error, stdout, stderr, nmerr):
    result = classify_diagnostics(output, error, stdout, stderr, nerr_nmerr=nmerr)
    if result['status'] != 'COMPLETE':
        raise ValueError('; '.join(result['errors']))
    return result


def _independent_checks(case, artifacts, execution, profile):
    checks = []
    native = artifacts.get(case['case_id']+'.out')
    _check(checks, 'deck', _equal_deck, artifacts.get(case['case_id']+'.inp'), case['deck_bytes'])
    if all(isinstance(artifacts.get(name+'.bin'), bytes) for name in ('stdout', 'stderr')):
        streams = {name: artifacts[name+'.bin'] for name in ('stdout', 'stderr')}
        _check(checks, 'execution', helpers._execution_bytes, execution, streams)
    else:
        checks.append(dict(check='execution', status='INCOMPLETE', reason='Missing retained process streams'))
    _check(checks, 'runtime_profile', helpers.validate_runtime_profile, native, profile)
    if isinstance(native, bytes):
        _check(checks, 'case_titles', verify_case_titles, native, case['case_token'])
    else:
        checks.append(dict(check='case_titles', status='INCOMPLETE', reason='Missing native output'))
    config = _check(checks, 'configuration', parse_configuration, native)
    if config is not None:
        _check(checks, 'diagnostics', _diagnostics, native, artifacts.get('file.err'),
               artifacts.get('stdout.bin'), artifacts.get('stderr.bin'), config['nerr_nmerr'])
    else:
        checks.append(dict(check='diagnostics', status='INCOMPLETE', reason='Native configuration unestablished'))
    _check(checks, 'model', _model, artifacts.get('model.cdb'), case)
    _check(checks, 'state', parse_state_values, artifacts.get('state_values.txt'), case['case_token'],
           len(case['nodes']), len(case['elements']))
    _check(checks, 'precision_witness', validate_precision_witness,
           artifacts.get('precision_witness.txt'), case['case_token'])
    return checks


def capture_pressure(case, directory, execution, *, runtime_profile, capture_case_id=CASE_ID):
    """Capture status is independent of numerical NOT_EVALUATED and attempt ledger."""
    _capture_files(capture_case_id)
    if not isinstance(case, dict) or case.get('case_id') != capture_case_id:
        raise ValueError('Case identity must match the explicit fixed pressure capture selection')
    if not isinstance(execution, dict):
        raise ValueError('Execution claim must be a dictionary')
    fixed = build_case(capture_case_id)
    rows, artifacts = inventory_capture(directory, capture_case_id+'.inp')
    checks = _independent_checks(fixed, artifacts, execution, runtime_profile)
    retained = all(row['status'] == 'retained' for row in rows if row['role'] == 'expected_capture')
    checked = all(check['status'] == 'COMPLETE' for check in checks)
    execution_status = next(check['status'] for check in checks if check['check'] == 'execution')
    return dict(case_id=capture_case_id, capture_status='INCOMPLETE',
        retention_status='COMPLETE' if retained else 'INCOMPLETE',
        independent_check_status='COMPLETE' if checked else 'INCOMPLETE',
        unverified_evidence=list(UNVERIFIED_EVIDENCE),
        auxiliary_anomalies=[row for row in rows if row['role'] != 'expected_capture'
                             and row['status'] != 'retained'],
        execution_status=execution_status,
        execution_claim={key: execution.get(key) for key in EXECUTION_FIELDS},
        checks=checks, inventory=rows, accepted_values={}, numerical_assessment='NOT_EVALUATED',
        reason='PRESSURE_PARSER_NOT_VALIDATED' if capture_case_id == CASE_ID
            else 'PRESSURE_NUMERICAL_ASSESSMENT_NOT_EVALUATED', engineering_qualified=False,
        scope='Capture-only checks; station/reaction grammar has a deliberate scope exclusion, '
              'not a claim of parsing impossibility. Admission, claims and launches remain operator-owned')
