from hashlib import sha256
import json

import pytest

from digitalmodel.workflows import installation_priority_sequence as sequence


def setup(tmp_path, monkeypatch):
    jobs=[];configs=[]
    for i in range(3):
        config=dict(manifest_path=str(tmp_path/f'input{i}/matched.json'),manifest_sha256='a'*64,
            artifact_manifest=str(tmp_path/f'input{i}/manifest.json'),artifact_manifest_sha256='b'*64,
            baseline_root=str(tmp_path/'baseline'),identities=[{'pid':1,'create_time':1.}],
            workers=3,cpus=[60,61,62],poll_seconds=15,wait_timeout_seconds=0,
            output_root=str(tmp_path/f'batch{i}'))
        path=tmp_path/f'config{i}.json';path.write_text(json.dumps(config));configs.append(config)
        jobs.append({'config':path.name,'sha256':sha256(path.read_bytes()).hexdigest()})
    spec=tmp_path/'sequence-spec.json';spec.write_text(json.dumps({'schema_version':1,'jobs':jobs}))
    monkeypatch.setattr(sequence.runner,'_inputs',lambda **kw:[{'id':str(i)} for i in range(5)])
    monkeypatch.setattr(sequence.runner,'_validate_options',lambda *a:None)
    return spec,configs


def run(spec,output):
    return sequence.run_sequence(spec,sha256(spec.read_bytes()).hexdigest(),output)


def test_batches_run_sequentially_preserving_caps(tmp_path,monkeypatch):
    spec,configs=setup(tmp_path,monkeypatch);calls=[]
    def execute(**config):
        assert (tmp_path/'sequence/sequence.lock').is_file()
        owner=json.loads((tmp_path/'sequence/sequence.lock').read_bytes())
        assert owner['pid']>0 and owner['create_time']>0
        assert owner['spec_sha256']==sha256(spec.read_bytes()).hexdigest()
        assert owner['active_batch']==len(calls)
        calls.append(config)
        return {'status':'completed','cases':[{'status':'completed'}]*5}
    monkeypatch.setattr(sequence.runner,'run_priority_requests',execute)
    result=run(spec,tmp_path/'sequence')
    assert calls==configs and result['status']=='completed'
    assert len(result['batch_results'])==3
    assert len(result['disk_checks'])==3
    assert all(r['free_bytes']>=10*1024**3 for r in result['disk_checks'])
    assert result['engineering_acceptance']=='NOT EVALUATED'
    assert result['coordinator']['pid']>0 and result['coordinator']['create_time']>0
    assert not (tmp_path/'sequence/sequence.lock').exists()


@pytest.mark.parametrize('mode',['failure','exception'])
def test_failed_batch_stops_later_dispatch(tmp_path,monkeypatch,mode):
    spec,_=setup(tmp_path,monkeypatch);calls=[]
    def execute(**config):
        calls.append(config)
        if mode=='exception':raise RuntimeError('fixture failure')
        return {'status':'failed','cases':[]}
    monkeypatch.setattr(sequence.runner,'run_priority_requests',execute)
    if mode=='exception':
        with pytest.raises(RuntimeError,match='fixture failure'):run(spec,tmp_path/'sequence')
    else:assert run(spec,tmp_path/'sequence')['status']=='failed'
    assert len(calls)==1
    record=json.loads((tmp_path/'sequence/sequence.json').read_bytes())
    assert record['status']=='failed'
    assert not (tmp_path/'sequence/sequence.lock').exists()


def test_wrong_spec_hash_stops_before_output(tmp_path,monkeypatch):
    spec,_=setup(tmp_path,monkeypatch)
    with pytest.raises(ValueError,match='digest'):
        sequence.run_sequence(spec,'0'*64,tmp_path/'sequence')
    assert not (tmp_path/'sequence').exists()


def test_existing_output_refused(tmp_path,monkeypatch):
    spec,_=setup(tmp_path,monkeypatch);out=tmp_path/'sequence';out.mkdir()
    with pytest.raises(FileExistsError):run(spec,out)
    assert list(out.iterdir())==[]


def test_duplicate_batch_outputs_refused(tmp_path,monkeypatch):
    spec,configs=setup(tmp_path,monkeypatch)
    config=tmp_path/'config1.json';configs[1]['output_root']=configs[0]['output_root']
    config.write_text(json.dumps(configs[1]));data=json.loads(spec.read_bytes())
    data['jobs'][1]['sha256']=sha256(config.read_bytes()).hexdigest();spec.write_text(json.dumps(data))
    with pytest.raises(ValueError,match='overlap|disjoint|duplicate'):run(spec,tmp_path/'sequence')
    assert not (tmp_path/'sequence').exists()


def test_revalidate_config_before_each_batch(tmp_path,monkeypatch):
    spec,_=setup(tmp_path,monkeypatch);calls=[]
    def execute(**config):
        calls.append(config);target=tmp_path/'config1.json';target.write_bytes(target.read_bytes()+b' ')
        return {'status':'completed','cases':[{'status':'completed'}]*5}
    monkeypatch.setattr(sequence.runner,'run_priority_requests',execute)
    with pytest.raises(ValueError,match='digest'):run(spec,tmp_path/'sequence')
    assert len(calls)==1


def test_all_batches_are_prevalidated_before_any_dispatch(tmp_path,monkeypatch):
    spec,_=setup(tmp_path,monkeypatch);calls=[]
    def validate(**kwargs):
        if 'input2' in kwargs['manifest_path']:raise ValueError('fixture invalid final batch')
        return [{'id':str(i)} for i in range(5)]
    monkeypatch.setattr(sequence.runner,'_inputs',validate)
    monkeypatch.setattr(sequence.runner,'run_priority_requests',lambda **kw:calls.append(kw))
    with pytest.raises(ValueError,match='invalid final'):run(spec,tmp_path/'sequence')
    assert calls==[] and not (tmp_path/'sequence').exists()


def test_completed_status_with_failed_case_stops_dispatch(tmp_path,monkeypatch):
    spec,_=setup(tmp_path,monkeypatch);calls=[]
    def execute(**kw):
        calls.append(kw)
        return {'status':'completed','cases':[{'status':'failed'}]+[{'status':'completed'}]*4}
    monkeypatch.setattr(sequence.runner,'run_priority_requests',execute)
    assert run(spec,tmp_path/'sequence')['status']=='failed'
    assert len(calls)==1


def test_partial_completed_case_list_halts(tmp_path,monkeypatch):
    spec,_=setup(tmp_path,monkeypatch);calls=[]
    def execute(**kw):
        calls.append(kw)
        return {'status':'completed','cases':[{'status':'completed'}]*4}
    monkeypatch.setattr(sequence.runner,'run_priority_requests',execute)
    assert run(spec,tmp_path/'sequence')['status']=='failed'
    assert len(calls)==1


@pytest.mark.parametrize('target',['nested_output','sequence_overlap','source_overlap'])
def test_canonical_output_containment_rejected(tmp_path,monkeypatch,target):
    spec,configs=setup(tmp_path,monkeypatch)
    paths={'nested_output':tmp_path/'batch0/child', 'sequence_overlap':tmp_path/'sequence/child',
           'source_overlap':tmp_path/'input0'}
    configs[1]['output_root']=str(paths[target]);config=tmp_path/'config1.json'
    config.write_text(json.dumps(configs[1]));data=json.loads(spec.read_bytes())
    data['jobs'][1]['sha256']=sha256(config.read_bytes()).hexdigest();spec.write_text(json.dumps(data))
    with pytest.raises(ValueError,match='overlap|disjoint'):run(spec,tmp_path/'sequence')
    assert not (tmp_path/'sequence').exists()


def test_unavailable_volume_fails_without_ancestor_loop(monkeypatch):
    class MissingRoot:
        calls = 0

        @property
        def parent(self):
            return self

        def exists(self):
            self.calls += 1
            if self.calls > 2:
                raise AssertionError('Unbounded ancestor walk')
            return False

    root = MissingRoot()
    monkeypatch.setattr(sequence, 'Path', lambda value: root)
    with pytest.raises(ValueError, match='volume'):
        sequence._disk_check({'output_root': 'missing'}, 0, {'disk_checks': []})


def test_insufficient_disk_stops_before_batch_and_records_observation(tmp_path,monkeypatch):
    spec,_=setup(tmp_path,monkeypatch);calls=[]
    monkeypatch.setattr(sequence.psutil,'disk_usage',lambda path:type('Usage',(),{'free':1024})())
    monkeypatch.setattr(sequence.runner,'run_priority_requests',lambda **kw:calls.append(kw))
    with pytest.raises(RuntimeError,match='disk'):run(spec,tmp_path/'sequence')
    record=json.loads((tmp_path/'sequence/sequence.json').read_bytes())
    assert calls==[] and record['status']=='failed'
    assert record['disk_checks'][0]['free_bytes']==1024
    assert record['disk_checks'][0]['required_free_bytes']==10*1024**3
