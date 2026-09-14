"""Four-attempt operator; external B2 authority and live preflight are mandatory.

Callbacks are explicit trust boundaries, not metadata flags authenticating themselves.
The production caller must supply the approved authority/host evidence adapters.
No solver discovery, native opt-in, retry or default affirmative adapter exists here.
"""
from pathlib import Path, PurePosixPath
import os
import sys
import re
import time

from digitalmodel.ansys.analysis_records import canonical_bytes, digest_bytes, parse_json
from digitalmodel.ansys.cylinder_benchmark import validate_deck

ORDER = ('ocv-zero-t60-n16', 'ocv-t60-p10-n4',
         'ocv-t60-p10-n8', 'ocv-t60-p10-n16')
PROFILE = {'cores': 1, 'parallel': 'smp', 'timeout_seconds': 300}


def runtime_sources():
    """Inventory this loaded package's source files, never a caller-selected root.

    Raw file hashes bind the source snapshot, not arbitrary callback behavior or
    Python bytecode attestation. Production adapters remain trusted boundaries.
    """
    directory = Path(__file__).resolve().parent
    paths = sorted(directory.glob('*.py'))
    expected = {('digitalmodel.ansys' if path.name == '__init__.py'
                 else 'digitalmodel.ansys.' + path.stem): path for path in paths}
    for name, module in list(sys.modules.items()):
        if name == 'digitalmodel.ansys' or name.startswith('digitalmodel.ansys.'):
            origin = getattr(module, '__file__', None)
            if (name not in expected or not origin
                    or Path(origin).resolve() != expected[name].resolve()):
                raise ValueError('loaded runtime dependency originates outside source inventory')
    result = []
    for path in paths:
        if path.is_symlink() or not path.is_file() or path.resolve().parent != directory:
            raise ValueError('runtime source is missing or redirected')
        result.append({'path': 'src/digitalmodel/ansys/' + path.name,
                       'sha256': digest_bytes(path.read_bytes())})
    return result


def retained_bytes(root):
    """Count retained regular files without following redirected directories."""
    total = 0
    for directory, subdirs, files in os.walk(root, followlinks=False):
        for name in subdirs + files:
            path = Path(directory) / name
            if path.is_symlink() or path.is_junction() or not path.resolve().is_relative_to(root):
                raise ValueError('capture tree contains redirected path')
        for name in files:
            path = Path(directory) / name
            if not path.is_file():
                raise ValueError('capture contains nonregular file')
            total += path.stat().st_size
    return total


def _capture_audit(root, approval, receipt):
    receipt['retained_bytes'] = retained_bytes(root)
    receipt['retained_bytes_scope'] = 'current run tree before final outcome receipt; no live quota'
    if receipt['retained_bytes'] > approval['capture_allowance_bytes']:
        raise ValueError('retained capture allowance exceeded; preserve evidence and stop')


def _file(root, name):
    if not isinstance(name, str) or '\\' in name:
        raise ValueError('invalid artifact locator')
    path = PurePosixPath(name)
    if path.is_absolute() or not path.parts or any(p in ('..', '.') for p in path.parts):
        raise ValueError('artifact locator escapes bundle')
    target = root.joinpath(*path.parts)
    if not target.resolve().is_relative_to(root.resolve()) or target.is_symlink():
        raise ValueError('artifact resolves outside bundle')
    return target


def _manifest(bundle, approval):
    raw = (bundle / 'manifest.json').read_bytes()
    if digest_bytes(raw) != approval['manifest_sha256']:
        raise ValueError('manifest differs from B2 approval')
    data = parse_json(raw)
    if data['schema'] != 'cylinder-b1-1' or data['case_order'] != list(ORDER):
        raise ValueError('unapproved case order or schema')
    if data.get('runtime_sources') != runtime_sources():
        raise ValueError('runtime source inventory differs from approved exact package')
    if [c['case_id'] for c in data['cases']] != list(ORDER):
        raise ValueError('missing, duplicate or additional case')
    seen = set()
    for artifact in data['artifacts']:
        name = artifact['path']
        if name in seen:
            raise ValueError('duplicate artifact identity')
        seen.add(name)
        if digest_bytes(_file(bundle, name).read_bytes()) != artifact['sha256']:
            raise ValueError('artifact changed after approval')
    if not {data['reference'], *[c['deck'] for c in data['cases']]}.issubset(seen):
        raise ValueError('unbound reference or deck')
    return data


def _authority(approval, verify):
    if not callable(verify) or verify(parse_json(canonical_bytes(approval))) is not True:
        raise ValueError('B2 authority not established')
    for key in ('approval_id', 'operator_id', 'checker_id', 'execution_host', 'ledger_directory'):
        if not isinstance(approval[key], str) or not approval[key].strip():
            raise ValueError('missing approved identity')
    if approval['operator_id'] == approval['checker_id']:
        raise ValueError('checker must be independent')
    if approval['profile'] != PROFILE:
        raise ValueError('unapproved solver profile')
    for key in ('manifest_sha256', 'executable_sha256'):
        if not re.fullmatch('[0-9a-f]{64}', approval[key]):
            raise ValueError('invalid approved hash')
    for key in ('capture_allowance_bytes', 'reserve_bytes'):
        if type(approval[key]) is not int or approval[key] <= 0:
            raise ValueError('storage limits require positive integers')


def _preflight(approval, adapter, remaining_capture):
    evidence = adapter(parse_json(canonical_bytes(approval)))
    for key in ('license_query', 'source_rights'):
        if not isinstance(evidence[key], str) or not evidence[key].strip():
            raise ValueError('missing live preflight evidence')
    if evidence['exclusive_seat_owner'] != approval['operator_id']:
        raise ValueError('exclusive seat ownership not established')
    if evidence['process_inventory'] != []:
        raise ValueError('existing solver processes require operator disposition')
    if evidence['executable_sha256'] != approval['executable_sha256']:
        raise ValueError('executable differs from approved profile')
    if evidence['profile'] != approval['profile']:
        raise ValueError('observed profile differs from approval')
    if evidence['execution_host'] != approval['execution_host']:
        raise ValueError('live host differs from authority-bound execution host')
    minimum = remaining_capture + approval['reserve_bytes']
    if type(evidence['free_bytes']) is not int or evidence['free_bytes'] <= minimum:
        raise ValueError('insufficient storage above allowance plus reserve')
    evidence['remaining_capture_allowance_bytes'] = remaining_capture
    return evidence


def _save(path, data):
    raw = canonical_bytes(data)
    with path.open('xb') as stream:
        stream.write(raw)
        stream.flush()
        os.fsync(stream.fileno())
    if path.read_bytes() != raw:
        raise OSError('receipt readback mismatch')


def _execution(result):
    if (result['return_code'] != 0 or result['timed_out'] is not False
            or result['owned_processes_remaining'] != 0):
        raise ValueError('execution/owned-process evidence incomplete; retain seat if uncertain')
    if result['stdout'] != b'' or result['stderr'] != b'':
        raise ValueError('process streams are nonempty or not original bytes')
    if (result.get('containment_verified') is not True
            or result.get('evidence_complete') is not True
            or result.get('settlement_required') is not False
            or result.get('streams_finalized') is not True):
        raise ValueError('containment or settlement evidence unestablished; retain seat')


def _claim(approval, root, index):
    ledger = Path(approval['ledger_directory'])
    if not ledger.is_absolute() or ledger.is_symlink():
        raise ValueError('approval ledger requires an absolute authoritative directory')
    ledger.mkdir(parents=True, exist_ok=True)
    key = digest_bytes(approval['approval_id'].encode('utf-8'))
    path = ledger / (key + '.json')
    claim = {'approval_id': approval['approval_id'], 'output': str(root),
             'approval_sha256': digest_bytes(canonical_bytes(approval))}
    if index == 1:
        _save(path, claim)
    elif path.read_bytes() != canonical_bytes(claim):
        raise ValueError('approval consumption ledger changed')


def _record_execution(directory, result):
    metadata = {key: value for key, value in result.items() if key not in ('stdout', 'stderr')}
    metadata['duration_seconds'] = str(metadata['duration_seconds'])
    errors = []
    for key in ('stdout', 'stderr'):
        raw = result[key]
        metadata[key + '_available'] = isinstance(raw, bytes)
        if not isinstance(raw, bytes):
            continue  # Retain supervisor token and failure facts even without a stream.
        metadata[key + '_sha256'] = digest_bytes(raw)
        try:
            path = directory / (key + '.bin')
            if not path.exists():
                with path.open('xb') as stream:
                    stream.write(raw)
                    stream.flush()
                    os.fsync(stream.fileno())
            retained = path.read_bytes()
            metadata[key + '_retained_sha256'] = digest_bytes(retained)
            metadata[key + '_readback_matches'] = retained == raw
            if retained != raw:
                errors.append(key + ': binary stream readback differs')
        except OSError as error:
            metadata[key + '_readback_matches'] = False
            errors.append(key + ': ' + str(error))
    metadata['stream_readback_errors'] = errors
    _save(directory / 'execution.json', metadata)
    if errors:
        raise ValueError('; '.join(errors))


def _attempt(bundle, root, case, index, launch, approval, manifest):
    directory = root / case['case_id']
    directory.mkdir(exist_ok=False)
    raw = _file(bundle, case['deck']).read_bytes()
    expected = next(a['sha256'] for a in manifest['artifacts'] if a['path'] == case['deck'])
    if digest_bytes(raw) != expected:
        raise ValueError('current deck differs from approved artifact')
    validate_deck(case['case_id'], raw)
    target = directory / Path(case['deck']).name
    with target.open('xb') as stream:
        stream.write(raw)
    if target.read_bytes() != raw:
        raise OSError('copied deck differs')
    record = {'case_id': case['case_id'], 'ordinal': index,
              'state': 'attempt_consumed', 'input_sha256': digest_bytes(raw)}
    _claim(approval, root, index)
    _save(root / f'attempt-{index}.json', record)
    return directory, launch(case, directory, 300)


def _prepare_next(bundle, root, approval, adapters, index, remaining):
    _authority(approval, adapters['verify_authority'])
    manifest = _manifest(bundle, approval)
    if adapters['verify_reference'](_file(bundle, manifest['reference'])) is not True:
        raise ValueError('independent reference agreement not established')
    remaining_capture = approval['capture_allowance_bytes'] - retained_bytes(root)
    if remaining_capture <= 0:
        raise ValueError('no remaining capture allowance before next attempt')
    preflight = _preflight(approval, adapters['preflight'], remaining_capture)
    _save(root / f'preflight-{index}.json', preflight)
    if remaining() < 300:
        raise ValueError('insufficient total wall time for full next attempt')
    _manifest(bundle, approval)  # Detect mutations during external observations.
    return manifest


def _adjudicate(receipt, approval, adapter):
    proof = adapter(parse_json(canonical_bytes(receipt)), parse_json(canonical_bytes(approval)))
    proof = parse_json(canonical_bytes(proof))
    expected = digest_bytes(canonical_bytes(receipt))
    if not proof or proof.get('checker_id') != approval['checker_id']:
        raise ValueError('independent evidence adjudication missing')
    if proof.get('receipt_sha256') != expected:
        raise ValueError('adjudication does not bind current evidence')
    receipt['adjudication'] = proof


def _run(bundle, root, approval, receipt, adapters, clock):
    start = clock()
    remaining = lambda: 1800 - (clock() - start)
    for index, case_id in enumerate(ORDER, 1):
        manifest = _prepare_next(bundle, root, approval, adapters, index, remaining)
        case = manifest['cases'][index - 1]
        try:
            directory, result = _attempt(bundle, root, case, index, adapters['launch'],
                                         approval, manifest)
        finally:
            if (root / f'attempt-{index}.json').exists():
                receipt['attempted'].append(case_id)
                receipt['unattempted'].remove(case_id)
        _record_execution(directory, result)
        _capture_audit(root, approval, receipt)
        _execution(result)
        evidence = parse_json(canonical_bytes(adapters['extract'](case, directory, result)))
        receipt['records'].append(evidence)
        _capture_audit(root, approval, receipt)
        records = parse_json(canonical_bytes({'records': receipt['records']}))['records']
        assessment = parse_json(canonical_bytes(adapters['assess'](records)))
        if assessment['status'] not in ('PASS', 'FAIL', 'INCOMPLETE'):
            raise ValueError('unknown assessment status')
        receipt['checks'] = assessment['checks']
        if remaining() < 0:
            raise ValueError('total wall-time budget exhausted')
        if assessment['status'] == 'INCOMPLETE':
            raise ValueError('evidence assessment incomplete')
        if index in (1, 4) and assessment['status'] == 'FAIL':
            receipt['status'] = 'FAIL'
            return
    receipt['status'] = 'PASS'
    _adjudicate(receipt, approval, adapters['adjudicate'])
    _capture_audit(root, approval, receipt)
    body = {key: value for key, value in receipt.items() if key != 'adjudication'}
    if receipt['adjudication']['receipt_sha256'] != digest_bytes(canonical_bytes(body)):
        raise ValueError('capture summary changed during final adjudication')
    if remaining() < 0:
        raise ValueError('total wall-time budget exhausted during adjudication')


def run_canary(bundle, output, approval, *, verify_authority, preflight, launch,
               extract, assess, adjudicate, verify_reference, clock=time.monotonic):
    """Run only through explicitly supplied approved adapters; preserve failures.

    Approval verification, live seat/rights observations and competent adjudication
    are caller-owned authority boundaries, not manufactured by this library.
    """
    bundle, output = Path(bundle).resolve(), Path(output).resolve()
    approval = parse_json(canonical_bytes(approval))
    output.mkdir(parents=True, exist_ok=False)
    receipt = {'schema': 'cylinder-canary-outcome-1', 'status': 'INCOMPLETE',
               'attempted': [], 'unattempted': list(ORDER), 'records': [], 'checks': []}
    adapters = dict(verify_authority=verify_authority, preflight=preflight, launch=launch,
                    extract=extract, assess=assess, adjudicate=adjudicate,
                    verify_reference=verify_reference)
    try:
        if any(not callable(adapter) for adapter in adapters.values()):
            raise ValueError('all B2 authority and execution adapters are required')
        _run(bundle, output, approval, receipt, adapters, clock)
    except Exception as error:
        receipt['status'] = 'INCOMPLETE'
        receipt['reason'] = str(error)
    _save(output / 'outcome.json', receipt)
    return receipt
