"""Synthetic fixed-history refusal controls; no native or licensing calls."""
from copy import deepcopy
from pathlib import Path
import pytest
from . import test_cylinder_intermediate_admission as admission_fixtures
from .test_cylinder_intermediate_admission import write

coarse = admission_fixtures.coarse
from digitalmodel.ansys import cylinder_intermediate_lineage as history
from digitalmodel.ansys.cylinder_pressure_scope import pressure_step,COARSE_SCOPE
from digitalmodel.ansys.analysis_records import parse_json


@pytest.mark.parametrize('config',[{},None,{'scope':None},{'scope':dict(COARSE_SCOPE,extra=True)},
    {'scope':dict(COARSE_SCOPE,max_attempts=True)}])
def test_missing_or_changed_scope_refuses(config):
    with pytest.raises(ValueError):pressure_step(config)


def test_coarse_scope_remains_explicit():
    assert pressure_step({'scope':deepcopy(COARSE_SCOPE)})==(2,'ocv-t60-p10-n4')


def reanchor(config,monkeypatch,role,data):
    pin=config['predecessor'][role];pin.update(write(Path(pin['path']),data))
    monkeypatch.setitem(history.FIXED,role,pin['sha256'])


def test_missing_supervisor_token_is_not_established_settlement(coarse,monkeypatch):
    pin=coarse['predecessor']['execution'];d=parse_json(Path(pin['path']).read_bytes())
    del d['retained_supervisor_token'];reanchor(coarse,monkeypatch,'execution',d)
    with pytest.raises(ValueError):history.validate_coarse_predecessor(coarse)


@pytest.mark.parametrize('role,field,value',[('invocation','ordinal',True),('claim','state','reserved'),
    ('claim','case_id','ocv-t60-p10-n8'),('outcome','reservation_released',1),
    ('outcome','no_owned_processes_established',False),('outcome','native_launch_count',True),
    ('capture','engineering_qualified',0),('capture','accepted_values',{'x':'0'})])
def test_typed_settlement_and_identity_refuse(coarse,monkeypatch,role,field,value):
    pin=coarse['predecessor'][role];d=parse_json(Path(pin['path']).read_bytes())
    d[field]=value;reanchor(coarse,monkeypatch,role,d)
    with pytest.raises(ValueError):history.validate_coarse_predecessor(coarse)


def test_replay_calls_actual_frozen_wrapper_with_exact_paths(coarse,monkeypatch):
    calls=[];result={'synthetic':'replay-result'}
    monkeypatch.setattr(history.replay,'replay_n4_predecessor',lambda *a:calls.append(a) or result)
    assert history.replay_coarse_predecessor(coarse) is result
    d=coarse['predecessor']
    assert calls==[(d['dataset_root'],d['runtime_root'],d['observation']['path'])]


def test_invalid_history_prevents_replay(coarse,monkeypatch):
    Path(coarse['predecessor']['claim']['path']).write_bytes(b'{}')
    def prohibited(*a):pytest.fail('Replay called before historical validation')
    monkeypatch.setattr(history.replay,'replay_n4_predecessor',prohibited)
    with pytest.raises(ValueError):history.replay_coarse_predecessor(coarse)


def test_missing_observation_fast_pins_refuse_as_value_error(coarse):
    with pytest.raises(ValueError):history.coarse_read_pins(coarse)
