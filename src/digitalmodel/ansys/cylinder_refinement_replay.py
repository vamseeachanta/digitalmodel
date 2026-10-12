"""Read-only replay of one pinned historical N4 predecessor; never admission.

The supplied dataset directory resolves bytes, not authenticated ownership. The
fixed observation identifies this one history. Its source table is intentionally
strict; changed parser sources require reviewed revalidation, not repinning by a
caller. The new wrapper/helper are covered by current review, not that old table.
Unit and origin labels are wrapper-assigned from the frozen benchmark convention;
label equality is not an independent measurement of physical units or provenance.
"""
from pathlib import Path, PurePosixPath
import stat
import sys

from digitalmodel.ansys.analysis_records import canonical_bytes, digest_bytes, parse_json
from digitalmodel.ansys.cylinder_criteria import evaluate_attempt
from digitalmodel.ansys.cylinder_results_validation import REQUIRED, validate_native_evidence

CASE_ID = 'ocv-t60-p10-n4'
OBSERVATION_SHA256 = 'd38703f10f5cdd190802b57d1374ddf9f7b81d628e36ae77030a0e1418881c5f'


def _path(value, *, directory=False):
    path = Path(value)
    if not path.is_absolute() or path.absolute() != path.resolve():
        raise ValueError('replay path must be absolute and unredirected')
    for item in (path, *path.parents):
        info = item.lstat()
        if stat.S_ISLNK(info.st_mode) or getattr(info, 'st_file_attributes', 0) & 0x400:
            raise ValueError('redirected replay path')
    info = path.stat()
    if directory:
        if not stat.S_ISDIR(info.st_mode):
            raise ValueError('replay directory required')
    elif not stat.S_ISREG(info.st_mode) or info.st_nlink != 1:
        raise ValueError('single-link regular replay file required')
    return path


def _relative(root, name):
    if not isinstance(name, str) or '\\' in name or ':' in name:
        raise ValueError('invalid replay locator')
    part = PurePosixPath(name)
    if part.is_absolute() or not part.parts or '..' in part.parts or str(part) != name:
        raise ValueError('invalid replay locator')
    target = _path(root / name)
    if not target.is_relative_to(root):
        raise ValueError('replay locator escapes root')
    return target


def _read(path, sha, size=None):
    raw = _path(path).read_bytes()
    if digest_bytes(raw) != sha:
        raise ValueError('replay file digest differs')
    if size is not None and (type(size) is not int or size < 0 or len(raw) != size):
        raise ValueError('replay byte count differs')
    return raw


def _sources(report):
    root = Path(__file__).resolve().parents[3]
    sources = report['source_files']
    if not isinstance(sources, dict) or not sources:
        raise ValueError('historical source pins required')
    for name, sha in sources.items():
        if not name.startswith('src/digitalmodel/ansys/') or not name.endswith('.py'):
            raise ValueError('source outside recorded package')
        path = _relative(root, name)
        _read(path, sha)
        module_name = name[4:-3].replace('/', '.')
        if module_name.endswith('.__init__'):
            module_name = module_name[:-9]
        module = sys.modules.get(module_name)
        if module is not None and Path(getattr(module, '__file__', '')).resolve() != path:
            raise ValueError('loaded historical source origin differs')
    return dict(sources)


def _inputs(runtime, report):
    raw = _read(runtime / 'manifest.json', report['runtime_sha256'])
    manifest = parse_json(raw)
    rows = manifest['artifacts']
    by_name = {row['path']: row for row in rows}
    if len(by_name) != len(rows):
        raise ValueError('duplicate historical runtime artifact')
    pins = {'manifest.json': report['runtime_sha256'],
        f'prepared/{CASE_ID}.json': report['case_metadata_sha256'],
        'reference.json': report['reference_sha256']}
    values = {}
    for name, expected in list(pins.items())[1:]:
        entry = by_name[name]
        if entry['sha256'] != expected:
            raise ValueError('runtime and observation artifact pins differ')
        values[name] = parse_json(_read(_relative(runtime, name), expected, entry['bytes']))
    case = values[f'prepared/{CASE_ID}.json']
    if case['case_id'] != CASE_ID:
        raise ValueError('historical predecessor case differs')
    return case, values['reference.json'], pins


def _raw(dataset, report):
    evidence = report['source_evidence']
    if not isinstance(evidence, dict) or set(evidence) != set(REQUIRED):
        raise ValueError('exact nine historical raw roles required')
    result = {}
    for role, row in evidence.items():
        if row['verified'] is not True or row['sha256'] != row['expected_sha256']:
            raise ValueError('historical raw attribution differs')
        result[role] = _read(_relative(dataset, row['owner_relative_path']),
                            row['sha256'], row['bytes'])
    return result


def _value_rows(values):
    return [dict(station_id=s, quantity=q, value=str(v),
        unit='mm' if q.startswith('u_') else 'MPa',
        origin='derived_von_mises' if q == 'sigma_vm' else 'native_export')
        for (s, q), v in sorted(values.items())]


def _compare(case, reference, raw, report):
    sha = report['reference_sha256']
    result = validate_native_evidence(case, raw, sha, sha)
    if result['status'] != 'COMPLETE' or result['errors']:
        raise ValueError('historical native recovery is incomplete')
    values = _value_rows(result['values'])
    force = dict(value=str(result['rfy_sum']), unit='N')
    expected = {(row['station_id'], q): value for row in reference['rows']
        if row['pressure_mpa'] == case['pressure_mpa'] for q, value in row['values'].items()}
    assessment = evaluate_attempt(CASE_ID, result['values'], expected, result['rfy_sum'], [])
    observed = dict(values=values, force=force, assessment=assessment)
    historical = dict(values=report['values'], force=report['support_rfy_sum'],
                      assessment=report['assessment'])
    if canonical_bytes(observed) != canonical_bytes(historical):
        raise ValueError('replayed quantities or full assessment differ from history')
    if assessment['status'] != 'CONTINUE' or assessment['engineering_qualified'] is not False:
        raise ValueError('expected diagnostic CONTINUE without qualification')
    return observed


def _replay(dataset_root, runtime_root, observation_path):
    dataset = _path(dataset_root, directory=True)
    runtime = _path(runtime_root, directory=True)
    report = parse_json(_read(observation_path, OBSERVATION_SHA256))
    if (report['schema'] != 'ansys-diagnostic-recovery-observation-1'
            or report['case_id'] != CASE_ID or report['recovery_status'] != 'COMPLETE'
            or report['recovery_errors'] or report['engineering_qualified'] is not False):
        raise ValueError('historical diagnostic observation differs')
    sources = _sources(report)
    case, reference, pins = _inputs(runtime, report)
    raw = _raw(dataset, report)
    observed = _compare(case, reference, raw, report)
    return dict(schema='cylinder-n4-predecessor-replay-1', case_id=CASE_ID,
        run_id=report['run_id'], replay_status='COMPLETE', native_admission='NOT_EVALUATED',
        native_launches=0, engineering_qualified=False, observation_sha256=OBSERVATION_SHA256,
        source_files=sources, runtime_pins=pins, source_evidence=report['source_evidence'],
        non_discriminating_roles=sorted(role for role, data in raw.items() if not data),
        values=observed['values'], support_rfy_sum=observed['force'],
        assessment=observed['assessment'],
        failed_checks=[row for row in observed['assessment']['checks'] if not row['passed']],
        historical_limits=report['limits'], historical_matrix_adoption=report['matrix_adoption'],
        label_basis='Unit and origin are wrapper-assigned frozen-benchmark labels; '
                    'label agreement is not an independent units/provenance measurement.')


def replay_n4_predecessor(dataset_root, runtime_root, observation_path):
    """Reproduce fixed N4 bytes and assessment without writing or authorizing N8.

    Zero-control replay, ledger lineage, source/config review and live admission
    remain caller obligations. No report field grants authority or qualification.
    Missing/structurally malformed pinned evidence refuses with ValueError.
    Arbitrarily repinned inputs and Decimal extremes are outside this fixed scope.
    """
    try:
        return _replay(dataset_root, runtime_root, observation_path)
    except (KeyError, TypeError, IndexError, AttributeError, OSError) as error:
        raise ValueError('historical replay evidence unavailable or malformed') from error
