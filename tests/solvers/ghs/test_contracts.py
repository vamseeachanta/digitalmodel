"""Neutral synthetic contracts; never launch GHS."""
import copy
import hashlib
import json

import pytest

from digitalmodel.solvers.ghs import contracts as c
from digitalmodel.solvers.ghs.runner import run_approved_capture


def inputs():
    geometry=b'SYNTHETIC PLACEHOLDER GEOMETRY; NOT GHS FORMAT\n'
    request={'case_id':'neutral-box','geometry_sha256':hashlib.sha256(geometry).hexdigest(),
        'depths_m':['1','2','3'],'density_kg_m3':'1000','units_frame':dict(c.UNITS_FRAME),
        'tolerance_profile':dict(c.TOLERANCES),'resource_limits':dict(c.RESOURCE_LIMITS)}
    runtime={'schema_version':1,'profile_id':'synthetic-runtime','platform':'windows',
        'executable':'C:/Synthetic Tools/GHS.exe','executable_sha256':'a'*64,'version':'unqualified',
        'work_dir':'C:/Synthetic Work','temp_dir':'C:/Synthetic Temp',
        'command_profile':'provisional_not_runnable','containment_profile':'unqualified'}
    policy={'schema_version':1,'report_name':'canary.pf','diagnostic_profile':'unqualified',
            'max_artifact_bytes':8*1024*1024,'max_total_bytes':32*1024*1024}
    return request,runtime,policy,geometry


def packet():
    return c.prepare_canary(*inputs())


def canonical(value):
    return json.dumps(value,sort_keys=True,separators=(',',':'),ensure_ascii=False,allow_nan=False).encode()


def receipt(p):
    return {'schema_version':1,'packet_sha256':p['packet_sha256'],
        'runtime_profile_sha256':p['runtime_profile_sha256'],'approval_reference':'synthetic-preview-only',
        'expires_at':'2030-01-01T00:00:00Z','nonce':'synthetic-once'}


def test_prepared_identity_and_fixed_argv():
    args=inputs();before=copy.deepcopy(args);p=c.prepare_canary(*args)
    assert args==before
    assert p==c.prepare_canary(*args)
    assert p['packet_sha256']==hashlib.sha256(canonical({k:v for k,v in p.items() if k!='packet_sha256'})).hexdigest()
    argv=c.build_argv(args[1])
    assert argv==['C:/Synthetic Tools/GHS.exe','/R:canary.rf','/L','/S','/G:canary.gf',
                  '/D:C:/Synthetic Work','/T:C:/Synthetic Temp']
    assert p['argv_sha256']==hashlib.sha256(canonical(argv)).hexdigest()
    assert b'REPORT OFF\nEND\n' in c.preview_runfile()
    assert b'HS 1\n' in c.preview_runfile()
    assert b'PROVISIONAL' in c.preview_runfile()


def test_decimal_trailing_zero_normalization():
    args=inputs();a=c.prepare_canary(*args)
    args[0]['depths_m']=['1.00','2.0','3.000'];args[0]['density_kg_m3']='1000.0'
    assert c.prepare_canary(*args)==a


@pytest.mark.parametrize('bad',[1.0,True,'+1','01','1e0','NaN','Infinity','-0','-0.00','0','4'])
def test_invalid_depth(bad):
    args=inputs();args[0]['depths_m'][0]=bad
    with pytest.raises(ValueError):c.prepare_canary(*args)


@pytest.mark.parametrize('part,field,value',[(0,'unknown',True),(0,'density_kg_m3','1025'),
    (1,'executable','GHS.exe'),(1,'executable','C:/Tools/run.cmd'),
    (1,'extra_args',['/X']),(1,'containment_profile','qualified'),(2,'report_name','../escape.pf')])
def test_closed_profile_and_fixed_case(part,field,value):
    args=inputs();args[part][field]=value
    with pytest.raises(ValueError):c.prepare_canary(*args)


def test_tolerance_cannot_widen():
    args=inputs();args[0]['tolerance_profile']['relative']='0.5'
    with pytest.raises(ValueError):c.prepare_canary(*args)


@pytest.mark.parametrize('component',['geometry','runtime','policy','packet'])
def test_swapped_components_rejected(component):
    args=inputs();p=c.prepare_canary(*args)
    if component=='geometry':args=(*args[:3],args[3]+b'changed')
    elif component=='runtime':args[1]['executable_sha256']='b'*64
    elif component=='policy':args[2]['diagnostic_profile']='different'
    else:p['density_kg_m3']='1000.0'
    with pytest.raises(ValueError):c.validate_packet(p,*args)


def test_receipt_preview_not_authorization():
    p=packet();r=receipt(p)
    result=c.preview_approval(r,p,approved_digest=p['packet_sha256'],
                            now='2029-01-01T00:00:00Z',consumed_nonces=[])
    assert result['launch_allowed'] is False
    assert result['state']=='preview_validated'
    with pytest.raises(RuntimeError):run_approved_capture(p,r)


@pytest.mark.parametrize('mutation',['expired','nonce','packet','runtime','boolean','extra'])
def test_receipt_boundary(mutation):
    p=packet();r=receipt(p);used=[]
    if mutation=='expired':r['expires_at']='2020-01-01T00:00:00Z'
    elif mutation=='nonce':used=[r['nonce']]
    elif mutation=='packet':r['packet_sha256']='0'*64
    elif mutation=='runtime':r['runtime_profile_sha256']='0'*64
    elif mutation=='boolean':r['approved']=True
    else:r['shell']=True
    with pytest.raises(ValueError):c.preview_approval(r,p,approved_digest=p['packet_sha256'],
                                now='2029-01-01T00:00:00Z',consumed_nonces=used)


@pytest.mark.parametrize('path',[
    'C:/Synthetic Work/.','C:/Synthetic Work.', 'C:/Synthetic Work ',
    'C:/Synthetic Work/child./other','C:/Synthetic Work/child /other',
    'C:/Synthetic Work/', 'c:\\synthetic work', 'C:/Synthetic Work//'])
def test_runtime_directory_alias_rejected(path):
    args=inputs();args[1]['temp_dir']=path
    with pytest.raises(ValueError):c.prepare_canary(*args)


def test_legitimate_internal_path_spaces_preserved():
    args=inputs();args[1]['temp_dir']='C:/Synthetic Work/Distinct Temp'
    assert c.build_argv(args[1])[-1]=='/T:C:/Synthetic Work/Distinct Temp'
