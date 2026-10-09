"""Prepare and native-verify numerical controls without statics or dynamics."""
import argparse
import copy
from hashlib import sha256
import json
import math
import os
from pathlib import Path
import re

import yaml

from digitalmodel.solvers.orcaflex.yaml_utils import OrcaFlexLoader, orcaflex_dump
from digitalmodel.workflows.installation_added_mass_bracket import read_pinned
from digitalmodel.workflows.installation_added_mass_materialize import verify_solver
from digitalmodel.workflows.installation_seastates import _snapshot, _json, _fixed_mode

GENERAL = {'General.TargetLogSampleInterval','General.ImplicitConstantTimeStep'}


def _field(key):
    if key in GENERAL:return ('General',key.split('.')[1])
    match=re.fullmatch(r'Lines\.([^\.]+)\.TargetSegmentLength',key)
    if not match:raise ValueError('Unlisted numerical field')
    return ('Lines',match[1])


def _line(snapshot,name):
    found=[r for r in snapshot.get('Lines',[]) if r.get('Name')==name]
    if len(found)!=1:raise ValueError('One named source line required')
    line=found[0]
    tables=[key for key in line if 'TargetSegmentLength' in [s.strip() for s in key.split(',')]]
    if len(tables)!=1:raise ValueError('One explicit target-segment column required')
    key=tables[0];column=[s.strip() for s in key.split(',')].index('TargetSegmentLength')
    return line,key,column


def _value(snapshot,key,value=None,write=False):
    kind,name=_field(key)
    if kind=='General':
        old=snapshot[kind][name]
        if write:snapshot[kind][name]=value
        return old
    line,table,column=_line(snapshot,name)
    old=[r[column] for r in line[table]]
    if write:
        if len(value)!=len(old):raise ValueError('Line section count changed')
        for row,target in zip(line[table],value):row[column]=target
    return old


def verify_changes(before,after,changes):
    restored=copy.deepcopy(after)
    for key,value in changes.items():
        actual=_value(after,key)
        if actual!=value:raise ValueError('Native numerical control readback mismatch')
        _value(restored,key,_value(before,key),True)
    if restored!=before:raise ValueError('Unlisted native model property changed')


def _anchors(plan):
    result={}
    for name,entry in plan['source_anchors'].items():
        if not re.fullmatch(r'[A-Za-z0-9_-]+',name):raise ValueError('Unsafe anchor ID')
        source,raw=read_pinned(entry['model_path'],entry['model_sha256'])
        generation_path=source.parent/'generation.json'
        _,gen_raw=read_pinned(generation_path,entry['generation_sha256']);gen=json.loads(gen_raw)
        if gen.get('status')!='native_verified_not_run':raise ValueError('Verified source generation required')
        if gen.get('case_id')!=name or gen.get('model_sha256')!=entry['model_sha256']:
            raise ValueError('Anchor generation identity mismatch')
        if any(gen.get(k)!=entry[k] for k in ('factor','added_mass_t')):
            raise ValueError('Anchor added-mass identity mismatch')
        request_path=source.parent/'request.yml'
        _,request_raw=read_pinned(request_path,gen['request_sha256'])
        model=yaml.load(raw,Loader=OrcaFlexLoader);request=yaml.safe_load(request_raw)
        if request.get('solver_version')!='11.6c':raise ValueError('Pinned source solver version mismatch')
        if request.get('model_sha256')!=entry['model_sha256']:
            raise ValueError('Anchor request/model mismatch')
        if not _fixed_mode(model['General']['ImplicitUseVariableTimeStep']):
            raise ValueError('Fixed integration source required')
        _timing(model)
        if re.search(rb'(?im)^\s*(?:BaseFile|IncludeFile)\s*:',raw):
            raise ValueError('Expanded self-contained anchor required')
        result[name]=dict(path=source,raw=raw,model=model,request=request,request_path=request_path,
            request_sha256=gen['request_sha256'],generation_path=generation_path,generation_sha256=entry['generation_sha256'])
    return result


def _timing(model):
    if model['General'].get('LogPrecision')!='Single':raise ValueError('Single logging precision required')
    values=[model['General'][k] for k in ('ImplicitConstantTimeStep','TargetLogSampleInterval')]
    if any(type(v) not in (int,float) or not math.isfinite(v) or v<=0 for v in values):
        raise ValueError('Positive finite actual time/log controls required')
    if values[1]<values[0]:raise ValueError('Logging interval cannot be shorter than integration step')
    ratio=values[1]/values[0]
    if not math.isclose(ratio,round(ratio),rel_tol=0,abs_tol=1e-10):
        raise ValueError('Logging interval must be an integer multiple of integration step')


def _plan_evidence(plan_path,plan):
    if plan.get('solver_version')!='11.6c':raise ValueError('Explicit pinned solver version required')
    pins=[(plan['comparison_report_path'],plan['comparison_report_sha256']),
        (plan['runtime_source']['path'],plan['runtime_source']['sha256'])]
    for path,digest in pins:read_pinned(plan_path.parent/path,digest)
    return [{'path':path,'sha256':digest} for path,digest in pins]


def _ordered(plan,anchors):
    cases=plan.get('cases',[]);names=[c['id'] for c in cases]
    if len(cases)!=5 or len(set(names))!=5 or set(names)&set(anchors):raise ValueError('Five unique check IDs required')
    states={k:dict(source=k,model=v['model']) for k,v in anchors.items()};ordered=[]
    for case in cases:
        if not re.fullmatch(r'[A-Za-z0-9_-]+',case['id']):raise ValueError('Unsafe check ID')
        if case['source_case'] not in anchors or not case.get('changes'):raise ValueError('Missing source or controls')
        for key,value in case['changes'].items():
            kind,_=_field(key);values=value if kind=='Lines' else [value]
            if not isinstance(values,list) or not values or any(type(v) not in (float,int) or not math.isfinite(v) or v<=0 for v in values):
                raise ValueError('Positive finite numerical controls required')
    pending=list(cases)
    while pending:
        ready=[c for c in pending if c['compare_to'] in states]
        if not ready:raise ValueError('Cyclic or missing comparison parent')
        for case in ready:
            parent=states[case['compare_to']]
            if parent['source']!=case['source_case']:raise ValueError('Comparator source anchor differs')
            model=copy.deepcopy(anchors[case['source_case']]['model'])
            for key,value in case['changes'].items():_value(model,key,value,True)
            _timing(model)
            intervals=[m['General']['TargetLogSampleInterval'] for m in (model,parent['model'])]
            ratio=max(intervals)/min(intervals)
            if not math.isclose(ratio,round(ratio),rel_tol=0,abs_tol=1e-10):
                raise ValueError('Nested logging intervals required')
            keys=set(case['changes']) | set(next((c['changes'] for c in cases if c['id']==case['compare_to']),{}))
            if sum(_value(model,k)!=_value(parent['model'],k) for k in keys)!=1:
                raise ValueError('Each comparison must change exactly one numerical control')
            states[case['id']]=dict(source=case['source_case'],model=model)
            ordered.append(case);pending.remove(case)
    return ordered


def _reset(api,model):
    if model.state!=api.ModelState.Reset:raise ValueError('ModelReset state required')
    if model.threadCount!=1:raise ValueError('Actual native thread count must equal one')


def _components(model):
    fields=('WaveTrainIndex','Frequency','FrequencyLowerBound','FrequencyUpperBound','Amplitude',
        'PhaseLagWrtWaveTrainTime','PhaseLagWrtSimulationTime','WaveNumber','Direction','Period')
    rows=[]
    for component in model.waveComponents:
        row=[]
        for field in fields:
            value=getattr(component,field,None)
            if value is None or not math.isfinite(float(value)):raise ValueError('Native wave component unavailable/nonfinite')
            row.append(float(value))
        rows.append(row)
    if not rows:raise ValueError('Native wave components unavailable')
    return dict(count=len(rows),fields=list(fields),sha256=sha256(json.dumps(rows,allow_nan=False).encode()).hexdigest())


def _apply_native(model,changes):
    for key,value in changes.items():
        kind,name=_field(key)
        if kind=='General':setattr(model.general,name,value)
        else:model[name].TargetSegmentLength=value


def _counts(model,changes):
    result={}
    for key in changes:
        kind,name=_field(key)
        if kind=='Lines':
            counts=[int(v) for v in model[name].NumberOfSegments]
            if not counts or any(v<=0 for v in counts):raise ValueError('Actual native segment counts unavailable')
            result[name]=counts
    return result


def _variation(case,master,change):
    payload={'BaseFile':os.path.relpath(master,change.parent).replace('\\','/')}
    for key,value in case['changes'].items():
        kind,name=_field(key)
        if kind=='General':payload.setdefault('General',{})[name]=value
        else:payload.setdefault('Lines',{})[name]={'TargetSegmentLength':value}
    orcaflex_dump(payload,change)
    if yaml.load(change.read_bytes(),Loader=OrcaFlexLoader)!=payload:raise ValueError('Variation readback mismatch')
    return payload


def _native_case(api,case,master,master_sha,change,output):
    read_pinned(master,master_sha)
    model=api.Model(threadCount=1);model.LoadData(str(master));_reset(api,model)
    read_pinned(master,master_sha)
    before=_snapshot(api,model);waves=_components(model);counts=_counts(model,case['changes'])
    variation=api.Model(threadCount=1);variation.LoadData(str(change));_reset(api,variation)
    read_pinned(master,master_sha)
    for key,value in case['changes'].items():
        kind,name=_field(key)
        actual=getattr(variation.general,name) if kind=='General' else list(variation[name].TargetSegmentLength)
        if actual!=value:raise ValueError('Native parent/change override did not apply')
    _apply_native(model,case['changes']);_reset(api,model)
    verify_changes(before,_snapshot(api,model),case['changes'])
    if _components(model)!=waves:raise ValueError('Realised native wave components changed')
    output.mkdir();model.SaveData(str(output/'model.yml'));_reset(api,model)
    model.LoadData(str(output/'model.yml'));_reset(api,model)
    verify_changes(before,_snapshot(api,model),case['changes'])
    if _components(model)!=waves:raise ValueError('Saved wave components changed')
    after_counts=_counts(model,case['changes'])
    return dict(wave_components=waves,segment_counts={k:dict(before=v,after=after_counts[k]) for k,v in counts.items()},
        model_state=int(model.state),fixed_time_step_s=float(model.general.ImplicitConstantTimeStep),
        expected_sample_interval_s=float(model.general.TargetLogSampleInterval),log_precision=model.general.LogPrecision)


def _request(anchor,output,settings):
    request=copy.deepcopy(anchor['request']);request.update(model='model.yml',model_sha256=sha256((output/'model.yml').read_bytes()).hexdigest())
    original=copy.deepcopy(request['extraction'])
    request.setdefault('limitations',[]).append('Preparation only; refined runtime cap requires review before any launch.')
    path=output/'request.yml';path.write_text(yaml.safe_dump(request,sort_keys=False),encoding='utf-8')
    loaded=yaml.safe_load(path.read_bytes())
    if loaded!=request or loaded['extraction']!=original:raise ValueError('Extraction/request readback mismatch')
    return dict(model_sha256=request['model_sha256'],request_sha256=sha256(path.read_bytes()).hexdigest(),
        timeout_seconds=request.get('timeout_seconds'),timeout_status='retained_requires_launch_review',
        extraction_profile_status='unchanged; actual logging interval is read from native results')


def _reverify(plan_path,plan_sha,plan,anchors,expanded):
    read_pinned(plan_path,plan_sha)
    _plan_evidence(plan_path,plan)
    for name,anchor in anchors.items():
        read_pinned(anchor['path'],plan['source_anchors'][name]['model_sha256'])
        read_pinned(anchor['generation_path'],anchor['generation_sha256'])
        read_pinned(anchor['request_path'],anchor['request_sha256'])
        read_pinned(expanded/'masters'/f'{name}.yml',plan['source_anchors'][name]['model_sha256'])


def prepare_numerical_checks(api,plan_path,plan_sha256,compact_output,expanded_output,*,solver_identity):
    compact,expanded=Path(compact_output).resolve(),Path(expanded_output).resolve()
    if compact.exists() or expanded.exists():raise FileExistsError('Exclusive new preparation outputs required')
    if compact.is_relative_to(expanded) or expanded.is_relative_to(compact):raise ValueError('Output roots must be disjoint')
    plan_path,raw=read_pinned(plan_path,plan_sha256);plan=json.loads(raw)
    if plan.get('schema_version')!=1:raise ValueError('Plan schema version 1 required')
    evidence=_plan_evidence(plan_path,plan)
    anchors=_anchors(plan);ordered=_ordered(plan,anchors)
    for path in [plan_path,*[a['path'] for a in anchors.values()]]:
        if path.is_relative_to(compact) or path.is_relative_to(expanded):raise ValueError('Output overlaps pinned source')
    identity=verify_solver(api,'11.6c',solver_identity)
    compact.mkdir(parents=True);expanded.mkdir(parents=True)
    for root in (compact,expanded):(root/'INCOMPLETE').write_text('Native preparation verification pending')
    (expanded/'masters').mkdir();(compact/'changes').mkdir();cases=[]
    for name,anchor in anchors.items():(expanded/'masters'/f'{name}.yml').write_bytes(anchor['raw'])
    for case in ordered:
        master=expanded/'masters'/f"{case['source_case']}.yml";change=compact/'changes'/f"{case['id']}.yml"
        _variation(case,master,change);destination=expanded/case['id']
        native=_native_case(api,case,master,plan['source_anchors'][case['source_case']]['model_sha256'],change,destination)
        receipt=dict(case,status='native_verified_not_run',engineering_acceptance='NOT EVALUATED',**native,
            **_request(anchors[case['source_case']],destination,native),
            change_sha256=sha256(change.read_bytes()).hexdigest(),source_model_sha256=plan['source_anchors'][case['source_case']]['model_sha256'],
            model_path=os.path.relpath(destination/'model.yml',compact).replace('\\','/'))
        _json(compact/f"{case['id']}.json",receipt);cases.append(receipt)
    _reverify(plan_path,plan_sha256,plan,anchors,expanded)
    result=dict(status='native_verified_not_run',engineering_acceptance='NOT EVALUATED',plan_sha256=plan_sha256,
        solver=identity,generator_sha256=sha256(Path(__file__).read_bytes()).hexdigest(),cases=cases,evidence=evidence,
        source_anchors=plan['source_anchors'],limitations=['No statics or dynamics performed; no numerical or engineering acceptance inferred.',
        'Compact variations reference external pinned masters; regeneration requires the retained source chain.'])
    _json(compact/'manifest.json',result)
    for root in (compact,expanded):(root/'INCOMPLETE').unlink()
    return result


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--plan',required=True,type=Path);parser.add_argument('--plan-sha256',required=True)
    parser.add_argument('--output',required=True,type=Path);parser.add_argument('--expanded-output',required=True,type=Path)
    args=parser.parse_args()
    from digitalmodel.workflows.orcaflex_reproduce import _load_api
    api,identity=_load_api({'solver_version':'11.6c'})
    result=prepare_numerical_checks(api,args.plan,args.plan_sha256,args.output,args.expanded_output,solver_identity=identity)
    print(json.dumps({'status':result['status'],'cases':len(result['cases'])}))


if __name__=='__main__':main()
