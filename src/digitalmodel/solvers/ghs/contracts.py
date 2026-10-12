"""Provisional source-neutral packet and receipt preview; no authorization issuance."""
import ntpath
from datetime import datetime

from ._canonical import bytes_hash, decimal, digest, fail, integer, sha, shape, text

UNITS_FRAME={'length':'m','mass':'tonne','volume':'m3','area':'m2','x':'forward_from_aft',
             'y':'port_from_centreplane','z':'up_from_baseline','depth':'origin_depth_m'}
TOLERANCES={'relative':'0.001','coordinate_m':'0.005','waterplane_m2':'0.01','mass_t':'0.1','volume_m3':'0.1'}
RESOURCE_LIMITS={'wall_timeout_seconds':120,'max_instances':1,
                 'max_artifact_bytes':8*1024*1024,'max_total_bytes':32*1024*1024}
RUNTIME_KEYS={'schema_version','profile_id','platform','executable','executable_sha256','version',
              'work_dir','temp_dir','command_profile','containment_profile'}
PACKET_KEYS={'schema_version','case_id','geometry_sha256','runfile_sha256','runtime_profile_id',
    'runtime_profile_sha256','argv_sha256','output_policy_sha256','units_frame','depths_m','density_kg_m3',
    'oracle_version','oracle_sha256','tolerance_profile','command_template_version','resource_limits','packet_sha256'}


def path_text(value):
    value=text(value,1024)
    drive,tail=ntpath.splitdrive(value)
    if (len(drive)!=2 or drive[1]!=':' or not drive[0].isalpha() or not ntpath.isabs(value)
            or any(c in value for c in '"<>|*?') or ':' in tail
            or any(part in {'.','..'} or part.endswith(('.', ' '))
                   for part in tail.replace('\\','/').split('/') if part)):
        fail()
    return value


def runtime_record(value):
    shape(value,RUNTIME_KEYS)
    integer(value['schema_version'],1,1)
    result={k:text(v,1024 if k in {'executable','work_dir','temp_dir'} else 128)
            for k,v in value.items() if k!='schema_version'}
    result['schema_version']=1
    for key in ('executable','work_dir','temp_dir'):
        result[key]=path_text(value[key])
    sha(result['executable_sha256'])
    if (result['platform']!='windows' or not result['executable'].lower().endswith('.exe')
            or result['command_profile']!='provisional_not_runnable'
            or result['containment_profile']!='unqualified'
            or ntpath.normcase(ntpath.normpath(result['work_dir']))
               ==ntpath.normcase(ntpath.normpath(result['temp_dir']))):
        fail()
    return result


def output_record(value):
    shape(value,{'schema_version','report_name','diagnostic_profile','max_artifact_bytes','max_total_bytes'})
    integer(value['schema_version'],1,1)
    for key in ('max_artifact_bytes','max_total_bytes'):
        integer(value[key],RESOURCE_LIMITS[key],RESOURCE_LIMITS[key])
    if value['report_name']!='canary.pf' or value['diagnostic_profile']!='unqualified':
        fail()
    return dict(value)


def build_argv(runtime_profile):
    r=runtime_record(runtime_profile)
    return [r['executable'],'/R:canary.rf','/L','/S','/G:canary.gf',
            '/D:'+r['work_dir'],'/T:'+r['temp_dir']]


def preview_runfile():
    return (b'` PROVISIONAL NOT RUNNABLE: geometry, units, density and runtime qualification absent\n'
            b'REPORT canary.pf\nHS 1\nHS 2\nHS 3\nREPORT OFF\nEND\n')


def normalized_request(request):
    shape(request,{'case_id','geometry_sha256','depths_m','density_kg_m3','units_frame',
                   'tolerance_profile','resource_limits'})
    if type(request['depths_m']) is not list or len(request['depths_m'])!=3:
        fail()
    depths=[decimal(v,positive=True) for v in request['depths_m']]
    density=decimal(request['density_kg_m3'],positive=True)
    if depths!=['1','2','3'] or density!='1000' or request['units_frame']!=UNITS_FRAME:
        fail()
    shape(request['units_frame'],UNITS_FRAME)
    shape(request['tolerance_profile'],TOLERANCES)
    tolerances={k:decimal(v,positive=True) for k,v in request['tolerance_profile'].items()}
    if tolerances!=TOLERANCES:
        fail()
    shape(request['resource_limits'],RESOURCE_LIMITS)
    for k,v in RESOURCE_LIMITS.items():integer(request['resource_limits'][k],v,v)
    return {'case_id':text(request['case_id']),'geometry_sha256':sha(request['geometry_sha256']),
        'depths_m':depths,'density_kg_m3':density,'units_frame':dict(UNITS_FRAME),
        'tolerance_profile':tolerances,'resource_limits':dict(RESOURCE_LIMITS)}


def prepare_canary(request,runtime_profile,output_policy,geometry_bytes):
    from .comparison import oracle_material
    request=normalized_request(request);runtime=runtime_record(runtime_profile);policy=output_record(output_policy)
    if bytes_hash(geometry_bytes)!=request['geometry_sha256']:
        fail()
    packet=dict(request,schema_version=1,runfile_sha256=bytes_hash(preview_runfile()),
        runtime_profile_id=runtime['profile_id'],runtime_profile_sha256=digest(runtime),
        argv_sha256=digest(build_argv(runtime)),output_policy_sha256=digest(policy),
        oracle_version=1,oracle_sha256=digest(oracle_material()),command_template_version=1)
    packet['packet_sha256']=digest(packet)
    return packet


def validate_prepared(packet):
    from .comparison import oracle_material
    shape(packet,PACKET_KEYS)
    for k in ('schema_version','oracle_version','command_template_version'):integer(packet[k],1,1)
    req={k:packet[k] for k in ('case_id','geometry_sha256','depths_m','density_kg_m3',
                             'units_frame','tolerance_profile','resource_limits')}
    normalized=normalized_request(req)
    if digest(req)!=digest(normalized):
        fail()
    text(packet['runtime_profile_id'])
    for k in ('geometry_sha256','runfile_sha256','runtime_profile_sha256','argv_sha256',
              'output_policy_sha256','oracle_sha256','packet_sha256'):sha(packet[k])
    if (packet['packet_sha256']!=digest({k:v for k,v in packet.items() if k!='packet_sha256'})
            or packet['runfile_sha256']!=bytes_hash(preview_runfile())
            or packet['oracle_sha256']!=digest(oracle_material())):
        fail()
    return packet


def validate_packet(packet,request,runtime_profile,output_policy,geometry_bytes):
    validate_prepared(packet)
    if digest(packet)!=digest(prepare_canary(request,runtime_profile,output_policy,geometry_bytes)):
        fail()
    return {'state':'prepared','packet_sha256':packet['packet_sha256'],'launch_allowed':False}


def timestamp(value):
    text(value,40)
    try:parsed=datetime.fromisoformat(value.replace('Z','+00:00'))
    except ValueError:fail()
    if parsed.tzinfo is None or parsed.utcoffset() is None:fail()
    return parsed


def preview_approval(receipt,packet,*,approved_digest,now,consumed_nonces):
    validate_prepared(packet)
    shape(receipt,{'schema_version','packet_sha256','runtime_profile_sha256','approval_reference','expires_at','nonce'})
    integer(receipt['schema_version'],1,1)
    if type(consumed_nonces) is not list or len(consumed_nonces)>10000:fail()
    used={text(v) for v in consumed_nonces};nonce=text(receipt['nonce'])
    text(receipt['approval_reference'],256)
    if (sha(receipt['packet_sha256'])!=packet['packet_sha256']
            or sha(approved_digest)!=packet['packet_sha256']
            or sha(receipt['runtime_profile_sha256'])!=packet['runtime_profile_sha256']
            or timestamp(receipt['expires_at'])<=timestamp(now) or nonce in used):
        fail()
    return {'state':'preview_validated','launch_allowed':False,'packet_sha256':packet['packet_sha256'],
            'limitations':['No receipt provenance, ACL, nonce reservation or live eligibility verified.']}
