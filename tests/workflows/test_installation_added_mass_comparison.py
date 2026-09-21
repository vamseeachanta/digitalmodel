import copy
from hashlib import sha256
import json

import numpy as np
import pytest
import yaml

from digitalmodel.workflows import installation_added_mass_comparison as comparison


def bracket():
    return {'cases':[dict(id=f'case_{i}_factor_{factor}'.replace('.','p'),source_case_index=i,factor=factor,
        settings={'hs_m':1.,'tp_s':8+i,'seed':123},added_mass_t=80*factor)
        for factor in (.5,1.,1.5) for i in range(5)]}


def test_composite_case_coverage_requires_all_fifteen():
    data=bracket();comparison.validate_coverage(data,[r['id'] for r in data['cases']])
    with pytest.raises(ValueError):comparison.validate_coverage(data,[r['id'] for r in data['cases']][:-1])
    with pytest.raises(ValueError):comparison.validate_coverage(data,[data['cases'][0]['id']]*15)


@pytest.mark.parametrize('defect',['factor','coordinate','duplicate_pair'])
def test_inconsistent_bracket_rejected(defect):
    data=bracket()
    if defect=='factor':data['cases'][0]['factor']=2.
    elif defect=='coordinate':data['cases'][0]['settings']['tp_s']=999
    else:data['cases'][0]['source_case_index']=1
    with pytest.raises(ValueError):comparison.validate_coverage(data,[r['id'] for r in data['cases']])


def test_zero_reference_is_not_reported_as_percent_change():
    assert comparison.absolute_delta(2.,0.)==2.
    assert comparison.absolute_delta(0.,0.)==0.
    assert comparison.absolute_delta(3.,2.)==1.


def test_semantic_identity_includes_endpoint_and_units():
    a={'object':'Sling','variable':'Effective tension','position':'End A','units':'kN'}
    assert comparison.channel_identity(a)!=comparison.channel_identity(dict(a,position='End B'))
    assert comparison.channel_identity(a)!=comparison.channel_identity(dict(a,units='N'))


def test_postexit_censoring_and_duration_rank_are_separate():
    events=[{'duration_s':.01,'retension_peak':40.,'right_censored':False,'retension_window_censored':False},
            {'duration_s':.2,'retension_peak':20.,'right_censored':False,'retension_window_censored':False},
            {'duration_s':.3,'retension_peak':30.,'right_censored':False,'retension_window_censored':True},
            {'duration_s':.4,'retension_peak':None,'right_censored':True,'retension_window_censored':True}]
    result=comparison.peak_summary(events)
    assert result['all_observed']['retension_peak']==40.
    assert result['duration_ge_0_1_s']['retension_peak']==30.
    assert result['duration_ge_0_1_s_uncensored']['retension_peak']==20.
    assert result['right_censored_count']==1 and result['window_censored_count']==2


def test_matched_wave_and_time_require_exact_equality():
    a={'time':np.array([0.,.1,.2]),'wave_elevation':np.array([1.,2.,3.])}
    comparison.verify_matched_arrays(a,a)
    for field in a:
        b=copy.deepcopy(a);b[field][1]+=1e-6
        with pytest.raises(ValueError):comparison.verify_matched_arrays(a,b)


def test_metric_recalculation_keeps_chord_separate():
    arrays={'time':np.array([0.,.1,.2,.3,.4]),'load':np.array([1.,-1.,-1.,1.,2.]),
            'chord':np.array([0.,.1,.2,.1,0.])}
    metadata={'channels':{'load':{'object':'Sling','variable':'Effective tension','position':'End B','units':'kN'},
                          'chord':{'object':'Sling','variable':'unstretched_length_minus_span_m','units':'m'}}}
    result=comparison.calculate_channels(arrays,metadata)
    assert result[0]['metrics']['maximum_tension_kN']==2.
    assert result[0]['metrics']['nonpositive_total_s']==pytest.approx(.2)
    assert result[0]['metrics']['strict_negative_event_count']==1
    assert result[1]['metrics']['maximum_chord_deficit_m']==.2
    assert 'physical_slack' not in json.dumps(result)


def test_output_refuses_existing_destination_before_reading(tmp_path):
    output=tmp_path/'existing';output.mkdir()
    with pytest.raises(FileExistsError):comparison.generate_comparison('missing','0'*64,'missing','0'*64,output)
    assert list(output.iterdir())==[]


def test_tampered_sequence_fails_before_output(tmp_path):
    sequence=tmp_path/'sequence.json';sequence.write_text('{}')
    with pytest.raises(ValueError,match='digest'):
        comparison.generate_comparison(sequence,'0'*64,sequence,sha256(sequence.read_bytes()).hexdigest(),tmp_path/'output')
    assert not (tmp_path/'output').exists()


@pytest.mark.parametrize('field',['case_id','factor','coefficient_z','added_mass_t','settings','manifest_sha256','master_sha256','change_sha256'])
def test_generation_is_bound_to_exact_bracket_case(field):
    from digitalmodel.workflows.installation_added_mass_evidence import verify_generation
    case=dict(id='case_1_factor_0',factor=.5,coefficient_z=10.,added_mass_t=8.,
              settings={'hs_m':1.,'tp_s':8.,'seed':12},change_sha256='change')
    gen=dict(status='native_verified_not_run',case_id=case['id'],
             **{k:case[k] for k in ('factor','coefficient_z','added_mass_t','settings')},
             dependencies={'manifest_sha256':'bracket','master_sha256':'master','change_sha256':'change'})
    verify_generation(gen,case,'bracket','master')
    if field in gen['dependencies']:gen['dependencies'][field]='changed'
    else:gen[field]='changed'
    with pytest.raises(ValueError):verify_generation(gen,case,'bracket','master')


def test_audit_adapter_uses_composite_identity_and_full_events(tmp_path):
    from digitalmodel.workflows.installation_added_mass_evidence import audit_row
    channel={'variable':'Effective tension','events':{'low_tension':{'events':[{'duration_s':.1}]}}}
    metadata={'channels':{'profile_000':channel},'trace_sha256':'trace'}
    row=audit_row('case_043_factor_00',tmp_path,{'simulation_sha256':'sim'},metadata,'meta')
    assert row['index']=='case_043_factor_00' and row['simulation_sha256']=='sim'
    assert row['channels']['profile_000']['events']['low_tension']['events']==[{'duration_s':.1}]


def _json(path,data):
    path.parent.mkdir(parents=True,exist_ok=True);path.write_text(json.dumps(data))
    return sha256(path.read_bytes()).hexdigest()


def _synthetic_channels(profile,t,load,wave,arrays):
    from digitalmodel.workflows.installation_response_metrics import tension_event_metrics
    channels={'wave_elevation':dict(object='Environment',variable='Elevation',units='m',minimum=float(wave.min()),maximum=float(wave.max())),
        'profile_000':dict(profile['channels'][0],minimum=float(load.min()),maximum=float(load.max()),events=tension_event_metrics(t,load,units='kN'))}
    for key in ('span_m','unstretched_length_minus_span_m','span_rate_m_per_s'):
        v=arrays['Sling_'+key];channels['Sling_'+key]=dict(object='Sling',variable=key,units='m/s' if 'rate' in key else 'm',minimum=float(v.min()),maximum=float(v.max()))
    return channels


def _synthetic_bracket(tmp_path):
    spec=bracket();spec.update(master_sha256='master',body_name='Body',limitations=['Synthetic fixture only'])
    for row in spec['cases']:
        row.update(coefficient_z=row['factor']*80,change_sha256='change')
    bp=tmp_path/'bracket.json';bsha=_json(bp,spec)
    return spec,bp,bsha


def synthetic_study(tmp_path,defect=None):
    from digitalmodel.workflows.installation_trace_extract import profile_digest
    spec,bp,bsha=_synthetic_bracket(tmp_path)
    batches=[];jobs=[]
    for batch_index,factor in enumerate((.5,1.,1.5)):
        prepared=tmp_path/f'prepared{batch_index}';root=tmp_path/f'batch{batch_index}';rows=[];pins=[];matched=[]
        for case in [r for r in spec['cases'] if r['factor']==factor]:
            name=case['id'];source=prepared/name;run=root/'runs'/name
            source.mkdir(parents=True);(run/'source').mkdir(parents=True);(run/'batch_runs/sims').mkdir(parents=True)
            model=yaml.safe_dump({'6DBuoys':[{'Name':'Body','AddedMassCoefficient':[1,2,case['coefficient_z']]}]})
            (source/'model.yml').write_text(model);model_sha=sha256((source/'model.yml').read_bytes()).hexdigest()
            (run/'source/model.yml').write_text(model)
            profile={'schema_version':1,'channels':[{'object':'Sling','variable':'Effective tension','units':'kN','position':'End B'}], 'geometry_lines':['Sling']}
            if defect=='missing_geometry':profile['geometry_lines']=[]
            request={'model':'model.yml','model_sha256':model_sha,'extraction':{'period':[0,3], 'supplemental_profile':profile}}
            for path in (source/'request.yml',run/'request.yml'):path.write_text(yaml.safe_dump(request))
            request_sha=sha256((source/'request.yml').read_bytes()).hexdigest()
            gen=dict(status='native_verified_not_run',case_id=name,**{k:case[k] for k in ('factor','added_mass_t','coefficient_z','settings')},
                dependencies=dict(manifest_sha256=bsha,master_sha256='master',change_sha256='change'),
                model_sha256=model_sha,request_sha256=request_sha,solver={'resolved_version':'test','library_sha256':'library'})
            if defect=='wrong_generation':gen['factor']=99
            _json(source/'generation.json',gen)
            for filename in ('model.yml','request.yml','generation.json'):
                pins.append(dict(path=f'{name}/{filename}',sha256=sha256((source/filename).read_bytes()).hexdigest()))
            sim=run/'batch_runs/sims/model.sim';sim.write_bytes(b'synthetic solver fixture')
            receipt=dict(status='completed',warnings=['Synthetic warning\r\nwith another line'],extraction={'status':'complete'},model_sha256=model_sha,request_sha256=request_sha,
                simulation_sha256=sha256(sim.read_bytes()).hexdigest(),simulation_stop=3.,actual_logging_interval=.1,solver_version='test')
            _json(run/'run.json',receipt)
            trace=run/'installation_traces';trace.mkdir();t=np.linspace(0,3,31);load=np.sin(t*5)*factor
            wave=np.cos(t)+(0.01 if defect=='unmatched_wave' and batch_index==2 else 0)
            arrays={'time':t,'wave_elevation':wave,'profile_000':load,'Sling_span_m':np.ones(31),
                    'Sling_unstretched_length_minus_span_m':np.ones(31)*.2,'Sling_span_rate_m_per_s':np.zeros(31)}
            np.savez(trace/'traces.npz',**arrays)
            channels=_synthetic_channels(profile,t,load,wave,arrays)
            metadata=dict(channels=channels,simulation_sha256=receipt['simulation_sha256'],request_sha256=request_sha,
                trace_sha256=sha256((trace/'traces.npz').read_bytes()).hexdigest(),supplemental_profile=profile,
                supplemental_profile_sha256=profile_digest(profile),extractor_sha256='extractor',metrics_sha256='metrics')
            meta_sha=_json(trace/'metadata.json',metadata)
            rows.append(dict(id=name,status='completed',run_dir=str(run),model=str(source/'model.yml'),request=str(source/'request.yml'),
                model_sha256=model_sha,request_sha256=request_sha,trace_metadata_sha256=meta_sha,trace_sha256=metadata['trace_sha256'],supplemental_profile_sha256=profile_digest(profile)))
            matched.append(dict(id=name,request=f'{name}/request.yml',model_sha256=model_sha))
        ap=prepared/'artifacts.json';mp=prepared/'matched.json'
        msh=_json(mp,{'cases':matched});pins.append({'path':'matched.json','sha256':msh})
        ash=_json(ap,{'files':pins})
        config=dict(output_root=str(root),artifact_manifest=str(ap),artifact_manifest_sha256=ash,manifest_path=str(mp),manifest_sha256=msh)
        campaign={'status':'completed','cases':rows};_json(root/'campaign.json',campaign)
        jobs.append({'config_args':config});batches.append({'index':batch_index,'result':campaign})
    sequence={'status':'completed','finished_utc':'2026-09-21T00:00:00Z','jobs':jobs,'batch_results':batches}
    sp=tmp_path/'sequence.json';ssha=_json(sp,sequence)
    return sp,ssha,bp,bsha


def test_synthetic_complete_chain_generates_fifteen_case_report(tmp_path):
    args=synthetic_study(tmp_path)
    result=comparison.generate_comparison(*args,tmp_path/'report')
    assert len(result['cases'])==15
    assert all(c['status']=='VERIFIED' and 'source_status' in c for c in result['cases'])
    assert result['engineering_acceptance']=='NOT EVALUATED'
    assert (tmp_path/'report/comparison.html').is_file()
    assert json.loads((tmp_path/'report/comparison.json').read_bytes())==result


@pytest.mark.parametrize('defect',['wrong_generation','missing_geometry','unmatched_wave'])
def test_synthetic_chain_rejects_mismatch(tmp_path,defect):
    args=synthetic_study(tmp_path,defect)
    with pytest.raises(ValueError):comparison.generate_comparison(*args,tmp_path/'report')
    assert not (tmp_path/'report/comparison.html').exists()


@pytest.mark.parametrize('defect',['missing_finish','live_lock'])
def test_sequence_must_be_finished_and_unlocked(tmp_path,defect):
    args=list(synthetic_study(tmp_path))
    if defect=='missing_finish':
        data=json.loads(args[0].read_bytes());data.pop('finished_utc');args[1]=_json(args[0],data)
    else:(tmp_path/'sequence.lock').write_text('active')
    with pytest.raises(ValueError):comparison.generate_comparison(*args,tmp_path/'report')


def test_exact_zero_plateau_is_distinguished_from_negative_event():
    arrays={'time':np.arange(40)*.1,'load':np.array([1.,0.,0.,1.]+[2.]*36)}
    metadata={'channels':{'load':{'object':'Wire','variable':'Effective tension','position':'End A','units':'kN'}}}
    row=comparison.calculate_channels(arrays,metadata)[0]
    assert row['metrics']['strict_negative_event_count']==0
    assert row['metrics']['event_count']==1
    peak=row['event_summary']['all_observed']
    assert peak['classification']=='exact_zero_plateau'
    assert peak['duration_resolution_limited'] is True


def test_rankings_include_most_negative_tension():
    cases=[dict(id='case',source_case_index=0,factor=1.,settings={'hs_m':1.,'tp_s':8.},
        channels=[dict(object='Wire',position='End A',channel='load',
                       metrics={'minimum_signed_tension_kN':-3.})])]
    rows=comparison._rankings(cases)
    assert any(r['metric']=='minimum_signed_tension_kN' and r['governing']['value']==-3.
               for r in rows if r['factor']==1.)


def test_missing_solver_warnings_fails_closed(tmp_path):
    args=synthetic_study(tmp_path)
    for p in tmp_path.glob('batch*/runs/*/run.json'):
        data=json.loads(p.read_bytes());data.pop('warnings',None);_json(p,data)
    with pytest.raises(ValueError,match='warning'):
        comparison.generate_comparison(*args,tmp_path/'report')


def test_report_separates_observed_and_filtered_results(tmp_path):
    args=synthetic_study(tmp_path)
    comparison.generate_comparison(*args,tmp_path/'report')
    html=(tmp_path/'report/comparison.html').read_text(encoding='utf-8')
    assert 'As-observed' in html and 'Duration-filtered' in html
    assert 'not evidence of resolved snap loading' in html
