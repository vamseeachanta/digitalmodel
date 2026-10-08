"""Read-only verification of the single committed independent reference capture.

Pinned input/capture commits identify the already-consumed checker invocation.
Replaying arithmetic does not establish competent engineering adjudication.
"""
import hashlib
import json
from decimal import Decimal
from pathlib import Path, PurePosixPath
import subprocess

INPUT_COMMIT = 'bf60335f773739ef449166da5f38286478f4d587'
CAPTURE_COMMIT = '48027b1a1c6311fc738a08bb8992f97f4936c694'
EVIDENCE = 'examples/ansys/cylinder-benchmark/'
CAPTURE_FILES = ('checker_invocation.json','checker_capture.json','checker_response.json',
                 'checker_transport.json','checker_process_stderr.txt')
FROZEN_FILES = tuple('src/digitalmodel/ansys/'+name+'.py' for name in
                    ('cylinder_reference','cylinder_expression','cylinder_agreement')) + tuple(
    'tests/ansys/test_'+name+'.py' for name in
    ('cylinder_reference','cylinder_expression','cylinder_agreement')) + tuple(
    EVIDENCE+name for name in ('checker_contract.json','checker_brief.txt','producer_derivation.json'))


def _json(raw):
    def unique(pairs):
        result = {}
        for key,value in pairs:
            if key in result:
                raise ValueError('Duplicate evidence JSON key')
            result[key]=value
        return result
    def invalid(value):
        raise ValueError('Nonfinite evidence JSON number')
    data=json.loads(raw,object_pairs_hook=unique,parse_float=Decimal,parse_constant=invalid)
    if not isinstance(data,dict):
        raise ValueError('Evidence requires a JSON object')
    return data


def _git_blob(root, commit, name):
    completed=subprocess.run(['git','-C',str(root),'show',commit+':'+name],
                             capture_output=True,check=False)
    if completed.returncode:
        raise ValueError('Pinned committed evidence is unavailable')
    return completed.stdout


def _root(path):
    for parent in (path.parent,*path.parents):
        if (parent/'.git').exists():
            return parent.resolve()
    raise ValueError('Reference requires its owning Git checkout')


def _read(root, relative):
    parts=PurePosixPath(relative)
    if parts.is_absolute() or '..' in parts.parts or '\\' in relative:
        raise ValueError('Invalid provenance locator')
    path=root.joinpath(*parts.parts)
    if path.is_symlink() or not path.is_file() or not path.resolve().is_relative_to(root):
        raise ValueError('Missing or escaping provenance artifact')
    return path.read_bytes()


def _capture(root, directory):
    raw={name:_read(root,str((directory/name).relative_to(root)).replace('\\','/'))
         for name in CAPTURE_FILES}
    for name,value in raw.items():
        if value!=_git_blob(root,CAPTURE_COMMIT,EVIDENCE+name):
            raise ValueError('Capture differs from pinned committed invocation')
    invocation, capture, transport = (_json(raw[name]) for name in
        ('checker_invocation.json','checker_capture.json','checker_transport.json'))
    if (invocation.get('input_commit')!=INPUT_COMMIT or invocation.get('invocation_ordinal')!=1
            or invocation.get('maximum_invocations')!=1 or invocation.get('state')!='invocation_consumed'):
        raise ValueError('Single frozen checker invocation not established')
    if (capture.get('exit_code')!=0 or capture.get('num_turns')!=1
            or transport.get('is_error') is not False or transport.get('num_turns')!=1
            or transport.get('subtype')!='success' or transport.get('terminal_reason')!='completed'
            or capture.get('session_id')!=transport.get('session_id')
            or not capture.get('session_id') or raw['checker_process_stderr.txt']!=b''):
        raise ValueError('Successful single checker capture not established')
    for field,name in (('transport_sha256','checker_transport.json'),
                       ('stderr_sha256','checker_process_stderr.txt'),('response_sha256','checker_response.json')):
        if capture.get(field)!=hashlib.sha256(raw[name]).hexdigest():
            raise ValueError('Capture digest mismatch')
    if transport.get('result','').encode('utf-8')!=raw['checker_response.json']:
        raise ValueError('Retained response differs from transport result')
    return invocation,raw['checker_response.json']


def _frozen(root, invocation):
    rows=invocation.get('frozen_files')
    if not isinstance(rows,list) or len(rows)!=len(FROZEN_FILES):
        raise ValueError('Frozen input inventory differs')
    if {row['path'] for row in rows}!=set(FROZEN_FILES):
        raise ValueError('Frozen input inventory differs')
    for row in rows:
        committed=_git_blob(root,INPUT_COMMIT,row['path'])
        if hashlib.sha256(committed).hexdigest()!=row['sha256'] or _read(root,row['path'])!=committed:
            raise ValueError('Frozen producer/checker inputs changed')
    if invocation.get('prompt_sha256')!=hashlib.sha256(_read(root,EVIDENCE+'checker_brief.txt')).hexdigest():
        raise ValueError('Frozen checker prompt changed')


def _rows(reference, replay):
    from digitalmodel.ansys.cylinder_adapter import reference_table
    table=reference_table(reference)
    if reference.get('schema')!='cylinder-analytical-reference-1':
        raise ValueError('Unsupported prepared reference schema')
    for row in reference['rows']:
        radial,y=row['station_id'].split('_y')
        if row.get('r_mm')!={'inner':'750','middle':'780','outer':'810'}[radial] or row.get('y_mm')!=y:
            raise ValueError('Reference coordinate/identity differs')
    for row in replay['comparisons']:
        radial={'750':'inner','780':'middle','810':'outer'}[row['r_mm']]
        key=(radial+'_y'+row['y_mm'],row['quantity'])
        if table[row['pressure_mpa']][key]!=Decimal(row['producer']):
            raise ValueError('Prepared reference differs from replayed producer value')


def verify_prepared_reference(reference_path):
    """Replay committed evidence, raising on drift; no provider/native invocation."""
    from digitalmodel.ansys.cylinder_agreement import compare_reference
    path=Path(reference_path).absolute()
    root=_root(path)
    if path.is_symlink() or not path.resolve().is_relative_to(root):
        raise ValueError('Reference is not an owned artifact')
    reference=_json(path.read_bytes())
    if reference.get('checker_capture_commit')!=CAPTURE_COMMIT:
        raise ValueError('Unapproved checker capture revision')
    invocation,response=_capture(root,path.parent)
    _frozen(root,invocation)
    comparison_raw=(path.parent/'reference_comparison.json').read_bytes()
    if hashlib.sha256(comparison_raw).hexdigest()!=reference.get('comparison_sha256'):
        raise ValueError('Reference comparison digest changed')
    comparison=_json(comparison_raw)
    replay=compare_reference(response)
    if comparison!={**replay,'checker_capture_commit':CAPTURE_COMMIT} or replay['status']!='AGREEMENT' or len(replay['comparisons'])!=126:
        raise ValueError('Independent comparison replay differs')
    _rows(reference,replay)
    return True
