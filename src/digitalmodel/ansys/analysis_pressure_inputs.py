"""Historical pressure capture evidence binding; no execution or numerical parsing."""
from pathlib import PurePosixPath
import re
import json
from digitalmodel.ansys.analysis_records import canonical_bytes, digest_bytes, parse_json, verify_reference

CASE_ID = 'ocv-t60-p10-n4'
REASON = 'PRESSURE_PARSER_NOT_VALIDATED'
ROLES = ('config','review','review_transport','review_bundle','runtime_manifest','parent_claim',
         'claim','attempt','invocation','terminal','outcome','capture','execution','prefix_replay','preflight','result_review','result_review_transport','result_review_bundle','source_provenance')
EXPECTED = ('file.db','file.rst','file.mntr','file.err','model.cdb',CASE_ID+'.inp',CASE_ID+'.out',
            'state_values.txt','station_values.txt','support_reactions.txt','precision_witness.txt','stdout.bin','stderr.bin')
UNVERIFIED = ['file.db','file.mntr','file.rst','station_values.txt','support_reactions.txt',
              'pressure_loads_and_support_conditions','stress_and_reaction_recovery']
EXECUTION_REVISION='d9ead839cad40bf92c437501edd55cbc94b91726'
EXECUTION_INVENTORY_SHA='79d0707c112d958f7a872d101f58156224e162eef80bcd772d8bce785f7c69ad'
SOURCE_PROVENANCE_SHA='ae4790b1acc1ac8f9676eb4e404ce93d97d57f14db245dcfb5d98caab3de989f'
CORE_SOURCES={'scripts/ansys/run_pressure_diagnostic.py',*[
    'src/digitalmodel/ansys/'+name+'.py' for name in ('cylinder_pressure_admission',
    'cylinder_pressure_resume','cylinder_pressure_journal','cylinder_pressure_capture','cylinder_runner',
    'cylinder_pressure_resources','cylinder_diagnostic_preflight','cylinder_operational_reservation')]}
GATES = dict(return_code=0,timed_out=False,owned_processes_remaining=0,containment_verified=True,
             evidence_complete=True,settlement_required=False,streams_finalized=True)
STREAM_LIMITATION = ('Readback equality compares reads of the same retained files and is not '
                     'independent stream-content corroboration.')


def document(role,raw):
    if role in ('review_transport','result_review_transport'):
        data=decode_document(raw)
        canonical_bytes(data.get('structured_output'))
        return data
    return parse_json(raw)


def decode_document(raw):
    """Decode transport telemetry lexically; engineering descriptors use parse_json.

    Original bytes remain digest authority. Fractional telemetry is retained as
    text and is never promoted to a result; consumed state fields remain typed.
    """
    def unique(pairs):
        result={}
        for key,value in pairs:
            if key in result:
                raise ValueError('duplicate native document key')
            result[key]=value
        return result
    def invalid_constant(value):
        raise ValueError('nonfinite native document number')
    data=json.loads(raw,object_pairs_hook=unique,parse_float=str,parse_constant=invalid_constant)
    require(isinstance(data,dict),'native document object required')
    return data


def require(condition, message):
    if not condition:
        raise ValueError(message)


def reference(ref):
    require(isinstance(ref,dict) and set(ref)=={'id','sha256'}, 'opaque reference fields differ')
    require(isinstance(ref['id'],str) and re.fullmatch(r'[A-Za-z0-9][A-Za-z0-9_-]{0,99}',ref['id']), 'opaque id invalid')
    require(isinstance(ref['sha256'],str) and re.fullmatch(r'[0-9a-f]{64}',ref['sha256']), 'digest invalid')


def equal(actual, expected, message):
    require(canonical_bytes(actual)==canonical_bytes(expected),message)


def _review_packet(d, sources):
    receipt, transport, bundle = (d[k] for k in ('review','review_transport','review_bundle'))
    decision=receipt.get('review',{})
    require(receipt.get('status')=='REVIEW_RECEIVED' and receipt.get('exit_code')==0
            and decision.get('verdict') in ('APPROVE','MINOR'), 'pressure review unavailable or blocking')
    require(receipt.get('bundle_sha256')==decision.get('bundle_sha256')==sources['review_bundle']['sha256']
            and receipt.get('stdout_sha256')==sources['review_transport']['sha256'], 'review transport digests differ')
    require(transport.get('is_error') is False and isinstance(transport.get('session_id'),str)
            and transport['session_id'] and transport['session_id']!='SOLVERS', 'review session invalid')
    equal(transport.get('structured_output'),decision,'review transport decision differs')
    rows=bundle.get('files'); require(isinstance(rows,list) and rows,'review bundle files missing')
    hashes={}
    for row in rows:
        require(isinstance(row,dict) and isinstance(row.get('path'),str)
                and isinstance(row.get('content'),str),'review file shape invalid')
        require(row['path'] not in hashes,'duplicate reviewed path')
        require(digest_bytes(row['content'].encode('utf-8'))==row.get('sha256'),'reviewed content digest differs')
        hashes[row['path']]=row['sha256']
    equal(receipt.get('files'),[{k:r[k] for k in ('path','sha256')} for r in rows], 'review inventory differs')
    return hashes


def _review(d,sources):
    validate_execution_sources(d['config'])
    _source_provenance(d,sources)
    hashes=_review_packet(d,sources)
    require(sources['config']['sha256'] in hashes.values(),'executed config absent from review')
    for source in d['config'].get('source_files',[]):
        name=source['path'].replace('\\','/')
        require(any(path.replace('\\','/').endswith('/'+name) or path.replace('\\','/')==name
                    for path,h in hashes.items() if h==source['sha256']), 'historical source absent from review')
    require(bool(d['config'].get('source_files')), 'source inventory missing')
    names=('review','review_transport','review_bundle')
    require(d['result_review_transport'].get('session_id')!=d['review_transport'].get('session_id'),
            'production and result review sessions must differ')
    result_hashes=_review_packet({k:d['result_'+k] for k in names},
                                {k:sources['result_'+k] for k in names})
    require(all(sources[k]['sha256'] in result_hashes.values() for k in ('capture','execution','outcome')),
            'native result absent from independent review')


def _authority(d, sources, old):
    config,claim,runtime=d['config'],d['claim'],d['runtime_manifest']
    equal(config.get('scope'),dict(case_ids=[CASE_ID],ordinal=2,max_attempts=1,capture_only=True,
                                  qualification='diagnostic_only'),'pressure scope differs')
    require(config.get('operator_id')=='SOLVERS' and isinstance(config.get('campaign_id'),str)
            and re.fullmatch('[0-9a-f]{40}',config.get('source_revision','')), 'pressure source/operator invalid')
    require(config['execution_binding']['manifest_sha256']==sources['runtime_manifest']['sha256'], 'runtime binding differs')
    require(CASE_ID in runtime.get('case_order',[]), 'runtime case missing')
    equal(runtime.get('runtime_lineage',{}).get('original_manifest_sha256'),
          old['input_descriptor']['benchmark_manifest_reference']['sha256'],'physical lineage differs')
    artifacts=runtime.get('artifacts',[])
    require(len({r['path'] for r in artifacts})==len(artifacts),'duplicate runtime artifact')
    require(any(r['path']=='prepared/'+CASE_ID+'.inp' and r['sha256']==old['model_revision']
                for r in artifacts),'runtime input differs')
    for key,value in dict(ordinal=2,case_id=CASE_ID,state='attempt_consumed',
        parent_sha256=sources['parent_claim']['sha256'],input_sha256=old['model_revision'],
        config_sha256=sources['config']['sha256'],review_receipt_sha256=sources['review']['sha256']).items():
        equal(claim.get(key),value,'claim '+key+' differs')
    require(config['lineage']['parent_claim']['sha256']==sources['parent_claim']['sha256'],'parent lineage differs')
    equal(claim.get('output'),config['operational']['output_directory'],'claim output differs')
    equal(d['attempt'],claim,'attempt copy differs')
    equal(d['terminal'],d['outcome'],'terminal copy differs')
    for key in ('ordinal','case_id','config_sha256','review_receipt_sha256'):
        equal(d['invocation'].get(key),claim[key],'invocation '+key+' differs')
    require(d['prefix_replay'].get('case_id')=='ocv-zero-t60-n16','prefix case differs')
    _review(d,sources)


def _capture(d):
    execution,capture,outcome=(d[k] for k in ('execution','capture','outcome'))
    for key,value in GATES.items():
        equal(execution.get(key),value,'execution '+key+' differs')
        equal(capture.get('execution_claim',{}).get(key),value,'capture execution claim differs')
    require(set(capture.get('execution_claim',{}))==set(GATES),'execution claim fields differ')
    for key,value in dict(case_id=CASE_ID,capture_status='INCOMPLETE',retention_status='COMPLETE',
        independent_check_status='COMPLETE',execution_status='COMPLETE',accepted_values={},
        numerical_assessment='NOT_EVALUATED',reason=REASON,engineering_qualified=False,
        unverified_evidence=UNVERIFIED,auxiliary_anomalies=[]).items():
        equal(capture.get(key),value,'capture '+key+' differs')
    checks=capture.get('checks')
    expected={'deck','execution','runtime_profile','case_titles','configuration','diagnostics','model','state','precision_witness'}
    require(isinstance(checks,list) and len(checks)==len(expected)
            and {r['check'] for r in checks}==expected
            and all(r.get('status')=='COMPLETE' for r in checks),'capture independent checks differ')
    for key,value in dict(case_id=CASE_ID,native_launch_count=1,launch_adapter_calls=1,consumed_count=2,
        no_owned_processes_established=True,reservation_released=True,accepted_values={},engineering_qualified=False,
        assessment_status='NOT_EVALUATED',assessment_reason=REASON,campaign_status='INCOMPLETE',
        terminal_reason='PLANNED_SCOPE_STOP').items():
        equal(outcome.get(key),value,'outcome '+key+' differs')


def _raw(receipt,d,resolver,old):
    rows=d['capture'].get('inventory'); require(isinstance(rows,list),'capture inventory missing')
    names=[row['path'] for row in rows]
    require(len(set(names))==len(names) and set(EXPECTED).issubset(names),'capture inventory membership differs')
    equal(sorted(receipt['raw']),sorted(names),'raw membership differs')
    for row in rows:
        name=row['path']; path=PurePosixPath(name)
        require(len(path.parts)==1 and name not in ('.','..') and ':' not in name and '\\' not in name,
                'unsafe capture path')
        ref=receipt['raw'][name];reference(ref);raw=verify_reference(ref,resolver)
        require(row.get('status')=='retained' and type(row.get('bytes')) is int
                and len(raw)==row['bytes'] and ref['sha256']==row.get('sha256'),'retained capture differs')
        if name==CASE_ID+'.inp':require(ref['sha256']==old['model_revision'],'retained deck differs')
        if name in ('stdout.bin','stderr.bin'):
            stream=name.split('.')[0]
            for suffix in ('_sha256','_retained_sha256'):
                require(d['execution'].get(stream+suffix)==ref['sha256'],'stream hash differs')
            require(d['execution'].get(stream+'_readback_matches') is True,'stream readback differs')


def load_inputs(old,ref,resolver):
    reference(ref)
    try:
        receipt=parse_json(verify_reference(ref,resolver))
        equal(set_to_list(receipt),sorted(('schema','case_id','case_index','previous_case_hash','sources','raw','limitations')),'intake fields differ')
        equal([receipt['schema'],receipt['case_id'],receipt['case_index']],
              ['pressure-diagnostic-intake-1',CASE_ID,9],'intake identity differs')
        equal(receipt['limitations'],[STREAM_LIMITATION],'stream limitation missing')
        equal(receipt['previous_case_hash'],old['row_hash'],'predecessor row differs')
        equal(sorted(receipt['sources']),sorted(ROLES),'source roles differ')
        refs=[ref,*receipt['sources'].values(),*receipt['raw'].values()]
        for item in refs:reference(item)
        require(len({r['id'] for r in refs})==len(refs),'duplicate logical evidence id')
        documents={k:document(k,verify_reference(receipt['sources'][k],resolver)) for k in ROLES}
        _authority(documents,receipt['sources'],old);_capture(documents)
        _raw(receipt,documents,resolver,old)
        return receipt,documents
    except (KeyError,TypeError,IndexError,AttributeError) as error:
        raise ValueError('pressure evidence shape invalid') from error


def set_to_list(value):
    return sorted(value)


def validate_execution_sources(config):
    require(config.get('source_revision')==EXECUTION_REVISION,'historical execution revision differs')
    rows=config.get('source_files')
    require(isinstance(rows,list),'historical source inventory missing')
    mapping={row['path']:row['sha256'] for row in rows}
    require(len(mapping)==len(rows) and CORE_SOURCES.issubset(mapping),'required historical core sources missing')
    require(digest_bytes(canonical_bytes(mapping))==EXECUTION_INVENTORY_SHA,'fixed historical inventory differs')
    return mapping


def _source_provenance(d,sources):
    require(sources['source_provenance']['sha256']==SOURCE_PROVENANCE_SHA,'source provenance receipt differs')
    proof=d['source_provenance'];mapping=validate_execution_sources(d['config'])
    require(proof.get('git_revision')==EXECUTION_REVISION
            and proof.get('configuration_sha256')==sources['config']['sha256'], 'source provenance identity differs')
    rows=proof.get('files');require(isinstance(rows,list) and len(rows)==len(mapping),'source proof incomplete')
    require({r['source_path'] for r in rows}==set(mapping),'source proof membership differs')
    for row in rows:
        require(row.get('git_revision')==EXECUTION_REVISION and row.get('comparison')=='MATCH', 'source proof state differs')
        require(all(row.get(k)==mapping[row['source_path']] for k in
            ('git_blob_sha256','configuration_sha256','working_sha256','copied_readback_sha256')),
            'source proof digest differs')


def invocation_start(invocation):
    """Use the retained invocation clock; it does not establish solver completion time."""
    from datetime import datetime,timezone
    value=invocation.get('start_utc_unix_ns')
    require(type(value) is int and value>0,'invocation start timestamp must be positive integer nanoseconds')
    seconds,nanos=divmod(value,1000000000)
    try:
        text=datetime.fromtimestamp(seconds,timezone.utc).strftime('%Y-%m-%dT%H:%M:%S')
    except (ValueError,OverflowError,OSError) as error:
        raise ValueError('invocation start timestamp out of range') from error
    return text+f'.{nanos:09d}Z'
