"""Raw solver evidence is required before engineering interpretation."""
import json
import pytest
from digitalmodel.asset_integrity.assessment import example_vessel_run_summary as summary


def test_native_error_is_not_an_engineering_nonpass(tmp_path):
    (tmp_path/'execution.json').write_text(json.dumps({'result':{'return_code':1}}))
    with pytest.raises(ValueError, match='Native'):
        summary.verify_run(tmp_path)


def test_changed_raw_evidence_is_rejected(tmp_path):
    execution = dict(result=dict(return_code=0,timed_out=False,owned_processes_remaining=0,
                     containment_verified=True,evidence_complete=True),reservation_released=True)
    (tmp_path/'execution.json').write_text(json.dumps(execution))
    (tmp_path/'model.json').write_text('{}')
    (tmp_path/'result-manifest.json').write_text(json.dumps({'files':{
        'model.json':{'sha256':'0'*64,'bytes':2}}}))
    with pytest.raises(ValueError, match='digest'):
        summary.verify_run(tmp_path)


def test_missing_reactions_and_equilibrium_residual_are_rejected(tmp_path):
    model = dict(nodes=[[1,0,1000,0]], reference_nodes=[[1,0]], basis=dict(pressure_mpa=1.5))
    (tmp_path/'displacements.csv').write_text('node_id,ux,uy,uz\n1,0,0.4,0\n')
    (tmp_path/'reactions.csv').write_text('node_id,fx,fy,fz\n')
    with pytest.raises(ValueError, match='Reaction node coverage'):
        summary._kinematics(tmp_path, model)
    (tmp_path/'reactions.csv').write_text('node_id,fx,fy,fz\n1,1000000,0,0\n')
    with pytest.raises(ValueError, match='equilibrium'):
        summary._kinematics(tmp_path, model)
