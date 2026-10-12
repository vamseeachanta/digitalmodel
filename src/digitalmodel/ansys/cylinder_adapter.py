"""Offline adapters for frozen canary evidence; no B2 authority is manufactured."""
import re
import hashlib
from copy import deepcopy
from pathlib import Path

from digitalmodel.ansys.cylinder_reference_provenance import verify_prepared_reference
from digitalmodel.ansys.analysis_records import parse_json
from digitalmodel.ansys.cylinder_results_validation import validate_native_evidence
from digitalmodel.ansys.cylinder_benchmark import validate_deck

from digitalmodel.ansys.cylinder_criteria import (
    CASE_IDS, EXPECTED_KEYS, STATIONS, evaluate_attempt, evaluate_canary,
)
from digitalmodel.ansys.cylinder_results import QUANTITIES, decimal_value


def reference_table(data):
    """Require both complete pressure tables with exact response units and keys."""
    if not isinstance(data, dict) or data.get('units') != {'stress':'MPa','displacement':'mm'}:
        raise ValueError('Reference units must be MPa and mm')
    if not isinstance(data.get('rows'), list) or len(data['rows']) != 18:
        raise ValueError('Reference requires exactly 18 pressure/station rows')
    result = {'0':{}, '10':{}}
    seen = set()
    for row in data['rows']:
        if not isinstance(row,dict):
            raise ValueError('Invalid reference row')
        pressure, station = row.get('pressure_mpa'), row.get('station_id')
        if pressure not in result or station not in STATIONS or (pressure,station) in seen:
            raise ValueError('Unknown or duplicate reference pressure/station')
        values = row.get('values')
        if not isinstance(values,dict) or set(values)!=set(QUANTITIES):
            raise ValueError('Reference quantities differ')
        seen.add((pressure,station))
        result[pressure].update({(station,q):decimal_value(v) for q,v in values.items()})
    if any(set(table)!=EXPECTED_KEYS for table in result.values()):
        raise ValueError('Incomplete reference response table')
    return result


def _profile_fields(profile):
    keys = ('release','build','update','platform')
    if not isinstance(profile,dict) or set(profile)!=set(keys):
        raise ValueError('All four native profile identities are required')
    for key,value in profile.items():
        if (not isinstance(value,str) or not 1<=len(value)<=64 or value!=value.strip()
                or not re.fullmatch(r'[A-Za-z0-9_. -]+',value)):
            raise ValueError('Invalid bounded native profile field')
    if not re.fullmatch(r'[0-9]{8}',profile['update']):
        raise ValueError('Native update requires eight date digits without UP prefix')
    return profile


def validate_runtime_profile(raw, profile):
    """Bind all four fields to one observed-form native startup header.

    Shape derives from retained PV native output line 106; the owning handoff records
    its source digest. Repeated summary/platform lines are not startup headers.
    This identity gate grants neither license authority nor model qualification.
    """
    profile = _profile_fields(profile)
    if not isinstance(raw,bytes):
        raise ValueError('Original native output bytes are required')
    text = raw.decode('utf-8',errors='strict')
    candidates=[line for line in text.splitlines() if re.match(r'^[ \t]*RELEASE\s*=',line)]
    if len(candidates)!=1:
        raise ValueError('Expected exactly one complete native startup profile header')
    pattern=(r'[ \t]*RELEASE=[ \t]*(?P<release>[A-Za-z0-9_. -]+?)[ \t]+'
             r'BUILD=[ \t]*(?P<build>[0-9]+(?:\.[0-9]+)*)[ \t]+'
             r'UP(?P<update>[0-9]{8})[ \t]+VERSION=[ \t]*'
             r'(?P<platform>[A-Za-z0-9_. -]+?)[ \t]*')
    match=re.fullmatch(pattern,candidates[0])
    if not match or match.groupdict()!=profile:
        raise ValueError('Native labeled profile differs from approved fields')


def _attempt_row(record):
    if not isinstance(record,dict) or not isinstance(record.get('values'),dict):
        raise ValueError('Missing keyed station values')
    values = record['values']
    if set(values)!=set(STATIONS):
        raise ValueError('Missing or additional station')
    flat = {}
    for station,quantities in values.items():
        if not isinstance(quantities,dict) or set(quantities)!=set(QUANTITIES):
            raise ValueError('Missing or additional response quantity')
        flat.update({(station,q):decimal_value(v) for q,v in quantities.items()})
    return {key:record[key] for key in ('case_id','rfy_sum','evidence_errors')} | {'values':flat}


def assessment_for_records(records, table):
    """PASS on a prefix authorizes numerical continuation only, never a solve."""
    checks = []
    try:
        if not isinstance(records,list) or not 1<=len(records)<=4:
            raise ValueError('Expected an ordered nonempty four-case prefix')
        attempts = [_attempt_row(row) for row in records]
        for index,attempt in enumerate(attempts):
            if attempt['case_id']!=CASE_IDS[index]:
                raise ValueError('Wrong, duplicate or out-of-order case')
            result = evaluate_attempt(**attempt,reference=table['10'])
            checks.extend(result['checks'])
            if result['status'] in ('FAIL','INCOMPLETE'):
                if index!=len(attempts)-1:
                    raise ValueError('Additional attempt follows a blocking result')
                return {**result,'checks':checks}
        if len(attempts)==4:
            return evaluate_canary(attempts,table['10'])
        return {'status':'PASS','checks':checks,'engineering_qualified':False,
                'scope':'operator continuation only','evidence_errors':[]}
    except (ValueError,KeyError,TypeError) as error:
        return {'status':'INCOMPLETE','checks':checks,'engineering_qualified':False,
                'evidence_errors':[str(error)]}



def _frozen_case(case_id):
    from digitalmodel.ansys.cylinder_benchmark import build_case
    return build_case(case_id)


def _read_owned(root, name, *, retain=True, nonempty=False):
    """Bind bytes/digest to one direct regular file within the attempt directory."""
    if not isinstance(name,str) or not re.fullmatch(r'[A-Za-z0-9_.-]+',name):
        raise ValueError('Artifact requires a direct basename')
    path = root/name
    if path.is_symlink() or path.resolve().parent!=root or not path.is_file():
        raise ValueError('Missing or nonlocal native artifact '+name)
    before = path.stat()
    digest, chunks = hashlib.sha256(), []
    with path.open('rb') as stream:
        while chunk := stream.read(1024*1024):
            digest.update(chunk)
            if retain:
                chunks.append(chunk)
    after = path.stat()
    fields = ('st_dev','st_ino','st_size','st_mtime_ns','st_ctime_ns')
    if any(getattr(before,k)!=getattr(after,k) for k in fields):
        raise ValueError('Artifact changed during readback')
    if nonempty and after.st_size==0:
        raise ValueError('Empty required native artifact '+name)
    return b''.join(chunks), {'path':name,'sha256':digest.hexdigest(),'bytes':after.st_size}


def _execution_bytes(execution, artifacts):
    if (type(execution.get('return_code')) is not int or execution['return_code']!=0
            or execution.get('timed_out') is not False
            or type(execution.get('owned_processes_remaining')) is not int
            or execution['owned_processes_remaining']!=0):
        raise ValueError('Execution or process settlement is incomplete')
    for key, expected in (('containment_verified',True),('evidence_complete',True),
                          ('settlement_required',False),('streams_finalized',True)):
        if execution.get(key) is not expected:
            raise ValueError('Missing execution containment/finalization evidence')
    for key in ('stdout','stderr'):
        if not isinstance(execution.get(key),bytes) or execution[key]!=artifacts[key]:
            raise ValueError('Retained stream differs from supervised execution')
        if execution[key]:
            raise ValueError('Process streams must be byte-empty')


def _capture(root, deck_name):
    names = {'native.out':Path(deck_name).stem+'.out','jobname.err':'file.err',
             'stdout':'stdout.bin','stderr':'stderr.bin'}
    for name in ('model.cdb','station_values.txt','state_values.txt',
                 'support_reactions.txt','precision_witness.txt'):
        names[name]=name
    artifacts, hashes = {}, {}
    for key,name in names.items():
        raw, metadata = _read_owned(root,name,nonempty=key not in ('jobname.err','stdout','stderr'))
        artifacts[key], hashes[name] = raw,metadata
    for name in ('file.rst','file.db','file.mntr'):
        _,hashes[name] = _read_owned(root,name,retain=False,nonempty=True)
    return artifacts,hashes


def extract_record(case, directory, execution, *, reference_path,
                   approved_reference_hash, runtime_profile):
    """Bind retained raw outputs to frozen input and reference, without launching.

    Authority, exclusive directory creation, approved executable identity, license
    ownership and independent checking remain external operator boundaries.
    Binary result presence is recorded; no binary result format is inferred.
    """
    root = Path(directory)
    if root.is_symlink() or not root.is_dir():
        raise ValueError('Existing owned attempt directory required')
    root = root.resolve()
    model = _frozen_case(case['case_id'])
    deck = Path(case.get('deck_basename',case.get('deck',''))).name
    if not re.fullmatch(r'[A-Za-z0-9_-]+\.(?:inp|ans)',deck):
        raise ValueError('Invalid native input basename')
    raw,deck_metadata = _read_owned(root,deck,nonempty=True)
    if raw!=model['deck_bytes']:
        raise ValueError('Retained deck differs from frozen case')
    reference = Path(reference_path)
    reference_raw,reference_metadata = _read_owned(reference.parent.resolve(),reference.name,nonempty=True)
    if not re.fullmatch(r'[0-9a-f]{64}',approved_reference_hash) or reference_metadata['sha256']!=approved_reference_hash:
        raise ValueError('Reference differs from approved bytes')
    artifacts,hashes = _capture(root,deck)
    hashes[deck] = deck_metadata
    _execution_bytes(execution,artifacts)
    validate_runtime_profile(artifacts['native.out'],runtime_profile)
    parsed = validate_native_evidence(case=model,artifacts=artifacts,
        approved_reference_hash=approved_reference_hash,observed_reference_hash=reference_metadata['sha256'])
    values = {}
    for (station,quantity),value in sorted(parsed['values'].items()):
        values.setdefault(station,{})[quantity]=str(value)
    errors = list(parsed['errors'])
    if parsed['status']!='COMPLETE' and not errors:
        errors.append('Native evidence is incomplete')
    return {'case_id':case['case_id'],'values':values,
            'rfy_sum':None if parsed['rfy_sum'] is None else str(parsed['rfy_sum']),
            'evidence_errors':errors,'artifacts':hashes,'reference_sha256':reference_metadata['sha256'],
            'engineering_qualified':False}



def _launch_case(case,directory,timeout,executable):
    from digitalmodel.ansys.cylinder_runner import launch_case
    return launch_case(case,directory,timeout,executable)


def _bound_reference(bundle, approval):
    raw,_ = _read_owned(bundle,'manifest.json',nonempty=True)
    if hashlib.sha256(raw).hexdigest()!=approval.get('manifest_sha256'):
        raise ValueError('Manifest differs from approval')
    locator=parse_json(raw).get('reference')
    if not isinstance(locator,str) or '\\' in locator:
        raise ValueError('Reference requires a relative bundle locator')
    relative=Path(locator)
    target=bundle/relative
    if relative.is_absolute() or '..' in relative.parts or target.is_symlink() or not target.resolve().is_relative_to(bundle):
        raise ValueError('Reference escapes bundle')
    raw,metadata=_read_owned(target.parent.resolve(),target.name,nonempty=True)
    return target,raw,metadata['sha256']


def make_execution_adapters(bundle, executable, approval):
    """Bind four callbacks only; B2 authority/preflight/adjudication stay external.

    Creation invokes no solver or checker. Reference verification uses 14 read-only
    Git subprocesses per verification; Git resolved from PATH is a trusted runtime
    dependency. Calling launch requires the operator's separate authority, seat,
    runtime and remaining-budget gates.
    """
    approval=deepcopy(approval)
    profile=approval.get('runtime_profile')
    if not isinstance(profile,dict) or set(profile)!={'release','build','update','platform'}:
        raise ValueError('Approved runtime profile is required')
    _profile_fields(profile)
    exe=Path(executable).absolute()
    approved_exe=approval.get('executable_sha256')
    if not isinstance(approved_exe,str) or not re.fullmatch(r'[0-9a-f]{64}',approved_exe):
        raise ValueError('Approved executable digest required')
    bundle=Path(bundle).resolve()
    reference,raw,reference_hash=_bound_reference(bundle,approval)
    verify_prepared_reference(reference)
    table=reference_table(parse_json(raw))
    def verify(path):
        if Path(path).resolve()!=reference.resolve():
            raise ValueError('Unexpected reference path')
        current,_,digest=_bound_reference(bundle,approval)
        if digest!=reference_hash:
            raise ValueError('Prepared reference changed')
        return verify_prepared_reference(current)
    def launch(case,directory,timeout):
        _,metadata=_read_owned(exe.parent.resolve(),exe.name,retain=False,nonempty=True)
        if metadata['sha256']!=approved_exe:
            raise ValueError('Executable changed after approval')
        verify(reference)
        root=Path(directory)
        if root.is_symlink() or not root.is_dir():
            raise ValueError('Existing owned attempt directory required')
        deck=Path(case['deck']).name
        raw,_=_read_owned(root.resolve(),deck,nonempty=True)
        validate_deck(case['case_id'],raw)
        return _launch_case({**case,'deck_basename':deck},directory,timeout,exe)
    def extract(case,directory,execution):
        verify(reference)
        return extract_record(case,directory,execution,reference_path=reference,
            approved_reference_hash=reference_hash,runtime_profile=profile)
    return {'launch':launch,'extract':extract,'assess':lambda records:assessment_for_records(records,table),
            'verify_reference':verify}
