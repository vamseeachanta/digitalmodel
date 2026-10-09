"""Synthetic pressure preflight/storage tests; no live queries or native calls."""
import copy
import importlib
import os
from types import SimpleNamespace
import pytest


def module():return importlib.import_module('digitalmodel.ansys.cylinder_pressure_resources')


def evidence():
    return dict(capacity_observation={'samples':[dict(observed_at=str(1001+i),
        interval_seconds='1',logical_processors=64,cpu_percent='90',available_memory_bytes=8*1024**3)
        for i in range(5)]},process_snapshot={'observed_at':'1005'},
        license_observation={'observed_at':'1005'},classification={'status':'CLEAR'})


@pytest.mark.parametrize('maximum,now',[(20,'1025'),(30,'1035')])
def test_all_four_age_boundaries(maximum,now):
    value=evidence();before=copy.deepcopy(value)
    assert module().validate_observation_ages(value,'1005',now,maximum)['status']=='PASS'
    assert value==before


@pytest.mark.parametrize('field',['capacity','process','license','ready'])
@pytest.mark.parametrize('time',['1004','1030','NaN',None,True])
def test_missing_future_stale_or_invalid_timestamp_refuses(field,time):
    value=evidence();ready='1005'
    if field=='capacity':value['capacity_observation']['samples'][-1]['observed_at']=time
    elif field=='process':value['process_snapshot']['observed_at']=time
    elif field=='license':value['license_observation']['observed_at']=time
    else:ready=time
    with pytest.raises(ValueError):module().validate_observation_ages(value,ready,'1025',20)


def preflight():
    calls=[]
    value=SimpleNamespace(last_evidence=evidence(),_ready_at='1005')
    value._bindings=lambda:calls.append('bindings')
    value._reservation=lambda:calls.append('reservation')
    value._licence=lambda:pytest.fail('Licence queried afterclaim')
    return value,calls


def test_phase2_requires_callback_and_reuses_cached_observations(monkeypatch):
    tool=module();value,calls=preflight();before=copy.deepcopy(value.last_evidence)
    monkeypatch.setattr(tool,'_now',lambda:'1010')
    def verify(approval):calls.append('verify');return approval=={'synthetic':True}
    result=tool.production_phase2(value,verify,{'synthetic':True})
    assert result['status']=='PASS' and calls==['verify','bindings','reservation']
    assert value.last_evidence==before
    with pytest.raises(ValueError):tool.production_phase2(value,None,{})
    with pytest.raises(ValueError):tool.production_phase2(value,lambda a:False,{})


def storage(tmp_path):
    original=tmp_path/'original';original.mkdir();payload=original/'raw'
    with payload.open('wb') as stream:stream.truncate(10174192)
    output=tmp_path/'output';output.mkdir();(output/'new').write_bytes(b'123')
    claim=tmp_path/'claim';claim.write_bytes(b'1')
    ledger=tmp_path/'ordinal';ledger.write_bytes(b'12')
    reservation=tmp_path/'reservation';reservation.write_bytes(b'1234')
    return original,claim,output,[ledger],reservation


def test_storage_counts_only_declared_roots_and_exact_reserve(tmp_path):
    args=storage(tmp_path);(tmp_path/'unrelated').write_bytes(b'unrelated')
    total=10174192+10;remaining=1024**3-total
    result=module().cumulative_storage(*args,free_bytes=remaining+2*1024**3)
    assert result['status']=='PASS' and result['total_bytes']==total
    assert result['remaining_allowance_bytes']==remaining
    assert module().cumulative_storage(*args,free_bytes=remaining+2*1024**3-1)['status']=='FAIL'


@pytest.mark.parametrize('fault',['overlap','duplicate','hardlink','missing','original_changed','free_bool'])
def test_storage_unsafe_or_unbound_inputs_refuse(tmp_path,fault):
    args=list(storage(tmp_path));free=4*1024**3
    if fault=='overlap':args[2]=args[0]
    elif fault=='duplicate':args[3]*=2
    elif fault=='hardlink':
        (args[2]/'new').unlink();os.link(args[1],args[2]/'new')
    elif fault=='missing':args[3]=[tmp_path/'absent']
    elif fault=='original_changed':(args[0]/'raw').write_bytes(b'changed')
    else:free=True
    with pytest.raises(ValueError):module().cumulative_storage(*args,free_bytes=free)


def test_storage_overrun_retains_files_and_reports_counts(tmp_path):
    args=storage(tmp_path)
    with (args[2]/'large').open('wb') as stream:stream.truncate(1024**3)
    result=module().cumulative_storage(*args,free_bytes=4*1024**3)
    assert result['status']=='FAIL' and result['remaining_allowance_bytes']<0
    assert (args[2]/'large').exists()


@pytest.mark.parametrize('fault',['stale','process','capacity','callback'])
def test_phase2_refuses_without_renewing_evidence(monkeypatch,fault):
    tool=module();value,calls=preflight()
    monkeypatch.setattr(tool,'_now',lambda:'1036' if fault=='stale' else '1010')
    if fault=='process':value.last_evidence['classification']['status']='UNKNOWN'
    if fault=='capacity':
        value.last_evidence['capacity_observation']['samples'][0]['available_memory_bytes']=1
    before=copy.deepcopy(value.last_evidence)
    with pytest.raises(ValueError):
        tool.production_phase2(value,lambda approval:1 if fault=='callback' else True,{})
    assert value.last_evidence==before


@pytest.mark.parametrize('fault',['missing','empty_samples','max_bool','max_other','now_bool'])
def test_age_shape_and_limit_refusal(fault):
    value=evidence();maximum=20;now='1010'
    if fault=='missing':del value['license_observation']
    elif fault=='empty_samples':value['capacity_observation']['samples']=[]
    elif fault=='max_bool':maximum=True
    elif fault=='max_other':maximum=31
    else:now=True
    with pytest.raises(ValueError):module().validate_observation_ages(value,'1005',now,maximum)


def test_storage_symlink_refuses_without_traversal(tmp_path):
    args=list(storage(tmp_path));link=tmp_path/'redirect'
    try:link.symlink_to(args[2],target_is_directory=True)
    except OSError:pytest.skip('OS does not grant synthetic symlink creation')
    args[2]=link
    with pytest.raises(ValueError):module().cumulative_storage(*args,free_bytes=4*1024**3)
