"""Collection requires successful pinned evidence before comparison."""
from hashlib import sha256
import json
from pathlib import Path

import pytest

from digitalmodel.workflows import installation_numerical_collect as collect


def write(path, value):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value), encoding='utf-8')
    return sha256(path.read_bytes()).hexdigest()


def fixture(tmp_path, monkeypatch):
    root=tmp_path; model=root/'prepared/model.yml'; request=model.with_name('request.yml')
    mh=write(model, {'model':1}); qh=write(request, {'request':1})
    parent=root/'parent'; candidate=root/'candidate'
    receipt={'status':'completed','model_sha256':mh,'request_sha256':qh}
    ph=write(parent/'run.json',receipt); write(candidate/'run.json',receipt)
    meta={'trace_sha256':'a'*64}; pm=write(parent/'installation_traces/metadata.json',meta)
    cm=write(candidate/'installation_traces/metadata.json',meta)
    row={'id':'child','source_case':'base','compare_to':'base','changes':{'General.TargetLogSampleInterval':.05},
         'status':'native_verified_not_run','model_path':'prepared/model.yml','model_sha256':mh,'request_sha256':qh}
    anchors={'base':{}}; plan={'source_anchors':anchors,'cases':[row]}
    plan_sha=write(root/'plan.json',plan)
    prep_sha=write(root/'preparation.json',dict(status='native_verified_not_run',plan_sha256=plan_sha,source_anchors=anchors,cases=[row]))
    config=dict(schema_version=1,plan_path='plan.json',plan_sha256=plan_sha,
        preparation_manifest='preparation.json',preparation_sha256=prep_sha,cases=[dict(case_id='child',parent='parent',
        parent_receipt_sha256=ph,parent_metadata_sha256=pm,candidate='candidate')])
    config_sha=write(root/'config.json',config)
    worker=dict(id='child',status='completed',run_dir=str(candidate),model=str(model),model_sha256=mh,
                request=str(request),request_sha256=qh,trace_metadata_sha256=cm,trace_sha256='a'*64)
    pool=dict(total_files=1,successful=1,failed=0,results=[dict(file_path='child',status='success',row=worker)])
    pool_sha=write(root/'pool.json',pool)
    calls=[]
    def compare(config):
        calls.append(config)
        return dict(status='VERIFIED_DIAGNOSTIC',kind='logging',channels={},requires_review=True,dependent_stage_release=False)
    monkeypatch.setattr(collect,'compare_planned_pair',compare)
    monkeypatch.setattr(collect,'_anchor_row',lambda name,anchor,consumed:(mh,qh))
    return dict(config_path=root/'config.json',config_sha256=config_sha,pool_result=root/'pool.json',
                pool_sha256=pool_sha,output=root/'output'), calls


def test_collects_pinned_success_and_requires_review(tmp_path,monkeypatch):
    args,calls=fixture(tmp_path,monkeypatch); result=collect.collect_stage(**args)
    assert len(calls)==1 and result['requires_review'] is True
    assert result['dependent_stage_release'] is False
    assert result['cases'][0]['comparison_sha256']==sha256((args['output']/'child.json').read_bytes()).hexdigest()
    assert calls[0]['candidate_receipt_sha256']==sha256((tmp_path/'candidate/run.json').read_bytes()).hexdigest()
    assert (args['output']/'summary.html').exists() and not (args['output']/'INCOMPLETE').exists()
    assert set(calls[0])=={'plan_path','plan_sha256','preparation_manifest','preparation_sha256','case_id',
        'parent','candidate','parent_receipt_sha256','parent_metadata_sha256','candidate_receipt_sha256','candidate_metadata_sha256'}
    html=(args['output']/'summary.html').read_text()
    assert all(result[k] in html for k in ('config_sha256','pool_sha256','generator_sha256','created_utc'))
    assert '<th>Control</th><th>Status</th>' in html and 'Case count: 1' in html


@pytest.mark.parametrize('defect',['pending','failed','row_failed','wrong_id','wrong_path','wrong_model','wrong_request','metadata','trace','duplicate','missing'])
def test_pool_defects_prevent_comparison(tmp_path,monkeypatch,defect):
    args,calls=fixture(tmp_path,monkeypatch); pool=json.loads(args['pool_result'].read_bytes()); row=pool['results'][0]['row']
    if defect in ('pending','failed'):pool['results'][0]['status']=defect
    elif defect=='row_failed':row['status']='failed'
    elif defect=='wrong_id':row['id']='other'
    elif defect=='wrong_path':row['run_dir']=str(tmp_path/'other')
    elif defect=='wrong_model':row['model_sha256']='0'*64
    elif defect=='wrong_request':row['request_sha256']='0'*64
    elif defect=='metadata':row['trace_metadata_sha256']='0'*64
    elif defect=='trace':row['trace_sha256']='0'*64
    elif defect=='duplicate':pool['results'].append(pool['results'][0])
    else:pool['results']=[]
    args['pool_sha256']=write(args['pool_result'],pool)
    with pytest.raises(ValueError):collect.collect_stage(**args)
    assert calls==[] and not args['output'].exists()


def test_blocked_logging_is_retained_without_release(tmp_path,monkeypatch):
    args,_=fixture(tmp_path,monkeypatch)
    monkeypatch.setattr(collect,'compare_planned_pair',lambda c:dict(status='BLOCKED',kind='logging',channels={}))
    result=collect.collect_stage(**args)
    assert result['status']=='BLOCKED' and result['dependent_stage_release'] is False
    assert result['requires_review'] is True and 'BLOCKED' in (args['output']/'summary.html').read_text()


def test_pin_failure_and_existing_output_fail_closed(tmp_path,monkeypatch):
    args,calls=fixture(tmp_path,monkeypatch); args['config_sha256']='0'*64
    with pytest.raises(ValueError):collect.collect_stage(**args)
    assert not calls
    args['output'].mkdir()
    with pytest.raises(FileExistsError):collect.collect_stage(**args)


def test_comparison_failure_retains_incomplete_marker(tmp_path,monkeypatch):
    args,_=fixture(tmp_path,monkeypatch)
    def fail(config):raise ValueError('trace mismatch')
    monkeypatch.setattr(collect,'compare_planned_pair',fail)
    with pytest.raises(ValueError):collect.collect_stage(**args)
    assert (args['output']/'INCOMPLETE').exists()


@pytest.mark.parametrize('key',['total_files','successful','failed'])
def test_pool_summary_counts_are_verified(tmp_path,monkeypatch,key):
    args,calls=fixture(tmp_path,monkeypatch); pool=json.loads(args['pool_result'].read_bytes())
    pool[key]+=1;args['pool_sha256']=write(args['pool_result'],pool)
    with pytest.raises(ValueError):collect.collect_stage(**args)
    assert calls==[]


def test_parent_receipt_wrong_model_rejected_before_comparison(tmp_path,monkeypatch):
    args,calls=fixture(tmp_path,monkeypatch);config=json.loads(args['config_path'].read_bytes())
    path=tmp_path/'parent/run.json';receipt=json.loads(path.read_bytes());receipt['model_sha256']='0'*64
    config['cases'][0]['parent_receipt_sha256']=write(path,receipt)
    args['config_sha256']=write(args['config_path'],config)
    with pytest.raises(ValueError):collect.collect_stage(**args)
    assert not calls and not args['output'].exists()


def test_nested_parent_candidate_roots_rejected(tmp_path,monkeypatch):
    args,calls=fixture(tmp_path,monkeypatch);config=json.loads(args['config_path'].read_bytes())
    config['cases'][0]['candidate']='parent/nested'
    args['config_sha256']=write(args['config_path'],config)
    with pytest.raises(ValueError,match='disjoint'):collect.collect_stage(**args)
    assert not calls


@pytest.mark.parametrize('key',['plan_sha256','preparation_manifest','kind'])
def test_case_cannot_shadow_stage_contract(tmp_path,monkeypatch,key):
    args,calls=fixture(tmp_path,monkeypatch);config=json.loads(args['config_path'].read_bytes())
    config['cases'][0][key]='unexpected';args['config_sha256']=write(args['config_path'],config)
    with pytest.raises(ValueError,match='case keys'):collect.collect_stage(**args)
    assert not calls and not args['output'].exists()


def test_output_inside_run_rejected_before_comparison(tmp_path,monkeypatch):
    args,calls=fixture(tmp_path,monkeypatch);args['output']=tmp_path/'candidate/report'
    with pytest.raises(ValueError,match='isolated'):collect.collect_stage(**args)
    assert not calls and not args['output'].exists()


def test_consumed_source_mutation_leaves_incomplete(tmp_path,monkeypatch):
    args,_=fixture(tmp_path,monkeypatch)
    def mutate(config):
        args['pool_result'].write_text('{}')
        return dict(status='VERIFIED_DIAGNOSTIC',kind='logging',channels={})
    monkeypatch.setattr(collect,'compare_planned_pair',mutate)
    with pytest.raises(ValueError):collect.collect_stage(**args)
    assert (args['output']/'INCOMPLETE').exists() and not (args['output']/'summary.json').exists()


def test_extra_pool_row_rejected(tmp_path,monkeypatch):
    args,calls=fixture(tmp_path,monkeypatch);pool=json.loads(args['pool_result'].read_bytes())
    extra=json.loads(json.dumps(pool['results'][0]));extra['file_path']='extra';extra['row']['id']='extra'
    pool['results'].append(extra);pool.update(total_files=2,successful=2)
    args['pool_sha256']=write(args['pool_result'],pool)
    with pytest.raises(ValueError,match='coverage'):collect.collect_stage(**args)
    assert not calls


@pytest.mark.parametrize('duplicate',[False,True])
def test_two_cases_shared_parent_and_candidate_uniqueness(tmp_path,monkeypatch,duplicate):
    args,calls=fixture(tmp_path,monkeypatch);config=json.loads(args['config_path'].read_bytes())
    second=dict(config['cases'][0],case_id='second',candidate='candidate' if duplicate else 'candidate2')
    config['cases'].append(second)
    plan=json.loads((tmp_path/'plan.json').read_bytes())
    plan['cases'].append(dict(plan['cases'][0],id='second'))
    config['plan_sha256']=write(tmp_path/'plan.json',plan)
    prep=json.loads((tmp_path/'preparation.json').read_bytes());prep['cases']=plan['cases']
    prep['plan_sha256']=config['plan_sha256']
    config['preparation_sha256']=write(tmp_path/'preparation.json',prep)
    args['config_sha256']=write(args['config_path'],config)
    pool=json.loads(args['pool_result'].read_bytes());worker=dict(pool['results'][0]['row'],id='second',run_dir=str(tmp_path/second['candidate']))
    for suffix in ('run.json','installation_traces/metadata.json'):
        write(tmp_path/second['candidate']/suffix,json.loads((tmp_path/'candidate'/suffix).read_bytes()))
    pool['results'].append(dict(file_path='second',status='success',row=worker));pool.update(total_files=2,successful=2)
    args['pool_sha256']=write(args['pool_result'],pool)
    if duplicate:
        with pytest.raises(ValueError,match='distinct'):collect.collect_stage(**args)
        assert not calls
    else:
        result=collect.collect_stage(**args)
        assert len(calls)==2 and len(result['cases'])==2
