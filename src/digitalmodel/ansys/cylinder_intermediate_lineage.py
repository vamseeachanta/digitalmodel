"""Read-only fixed N4 settlement and replay bindings for an N8 continuation."""
from pathlib import Path
import re
from digitalmodel.ansys.analysis_records import canonical_bytes,parse_json
from digitalmodel.ansys.cylinder_pressure_scope import pressure_step
from digitalmodel.ansys import cylinder_refinement_replay as replay

CASE_ID = 'ocv-t60-p10-n4'
OBSERVATION_SHA256 = 'd38703f10f5cdd190802b57d1374ddf9f7b81d628e36ae77030a0e1418881c5f'
FIXED = dict(
    invocation='100235891544da26e381b3b0639903fd03cf9ec4444c70a2ced83347e03efd6f',
    claim='12322f9136e01af1b0cd1d2b544a344deb75f4ab9ce2431d6f933047b3559bdd',
    terminal='58d0eae8a799bca4109985df14dca2285f6b4f8bbf121aba76b3fdf759089cb5',
    outcome='58d0eae8a799bca4109985df14dca2285f6b4f8bbf121aba76b3fdf759089cb5',
    execution='6153f3b70067a834456b47b338a628bd17569e4086cd188b5e1f2d94af76069b',
    capture='ab5b6a9f198517e3798fbf3507bd054a42d515bcf639fb41b86e877d5e4e5603')


def _require(condition, message):
    if not condition:raise ValueError(message)


def _equal(actual, expected, name):
    _require(type(actual) is type(expected) and actual == expected, name+' differs')


def _pin(pin, expected=None):
    _require(isinstance(pin,dict) and set(pin)=={'path','sha256'},'Exact path/digest pin required')
    _require(isinstance(pin['path'],str),'String evidence path required')
    sha=pin['sha256']
    _require(isinstance(sha,str) and re.fullmatch(r'[0-9a-f]{64}',sha),'Invalid evidence digest')
    if expected is not None:_equal(sha,expected,'Fixed historical digest')
    return pin


def _descriptor(config):
    _require(pressure_step(config)==(3,'ocv-t60-p10-n8'),'Intermediate scope required')
    descriptor=config.get('predecessor')
    keys={'schema','original_root','dataset_root','runtime_root','observation'}|set(FIXED)
    _require(isinstance(descriptor,dict) and set(descriptor)==keys,'Exact predecessor descriptor required')
    _equal(descriptor['schema'],'cylinder-coarse-predecessor-1','Predecessor schema')
    for key in ('original_root','dataset_root','runtime_root'):
        replay._path(descriptor[key],directory=True)
    pin=_pin(descriptor['observation'],OBSERVATION_SHA256)
    path=Path(pin['path'])
    _require(path.is_absolute() and path.absolute()==path.resolve(),'Observation path must be absolute and unredirected')
    replay._path(path.parent,directory=True)
    return descriptor


def _records(config, descriptor):
    root=replay._path(descriptor['original_root'],directory=True)
    ledger=replay._path(config['ledger_directory'],directory=True)
    parent=_pin(config['lineage']['parent_claim'])
    parent_path=replay._path(parent['path'])
    _require(parent_path.parent==ledger and re.fullmatch(r'[0-9a-f]{64}\.json',parent_path.name),
        'Parent claim must use campaign ledger stem')
    replay._read(parent_path,parent['sha256'])
    stem=parent_path.stem+'.ordinal-2'
    paths=dict(claim=ledger/(stem+'.json'),invocation=ledger/(stem+'.invocation.json'),
        terminal=ledger/(stem+'.terminal.json'),outcome=root/'outcome.json',
        execution=root/CASE_ID/'execution.json',capture=root/CASE_ID/'capture.json')
    records={}
    for role in FIXED:
        pin=_pin(descriptor[role],FIXED[role])
        _require(replay._path(pin['path'])==paths[role],'Historical '+role+' path differs')
        records[role]=parse_json(replay._read(paths[role],pin['sha256']))
        _require(isinstance(records[role],dict),'Historical record must be object')
    for role in ('claim','invocation'):
        d=records[role]
        for k,v in dict(case_id=CASE_ID,ordinal=2,parent_sha256=parent['sha256']).items():
            _equal(d.get(k),v,role+' '+k)
        _require(replay._path(d.get('output'),directory=True)==root,'Historical output root differs')
    _equal(records['claim'].get('state'),'attempt_consumed','Consumed historical claim')
    return records


def _settled(records):
    execution=records['execution']
    for k,v in dict(return_code=0,timed_out=False,containment_verified=True,
        streams_finalized=True,owned_processes_remaining=0,settlement_required=False,
        evidence_complete=True,error=[],retained_supervisor_token=None).items():
        _require(k in execution,'Missing execution field '+k)
        _equal(execution[k],v,'Execution '+k)
    outcome=records['outcome']
    for k,v in dict(case_id=CASE_ID,consumed_count=2,native_launch_count=1,launch_adapter_calls=1,
        terminal_reason='PLANNED_SCOPE_STOP',reservation_released=True,
        no_owned_processes_established=True,engineering_qualified=False).items():
        _equal(outcome.get(k),v,'Outcome '+k)
    _require(canonical_bytes(records['terminal'])==canonical_bytes(outcome),'Terminal and outcome differ')
    capture=records['capture']
    for k,v in dict(case_id=CASE_ID,retention_status='COMPLETE',independent_check_status='COMPLETE',
        execution_status='COMPLETE',engineering_qualified=False,accepted_values={},capture_status='INCOMPLETE').items():
        _equal(capture.get(k),v,'Capture '+k)
    _require(canonical_bytes(outcome.get('capture'))==canonical_bytes(capture),'Embedded capture differs')


def validate_coarse_predecessor(config):
    """Check fixed original settlement; numerical observation is read by replay separately."""
    try:
        records=_records(config,_descriptor(config));_settled(records)
        return records
    except (KeyError,TypeError,AttributeError,OSError) as error:
        raise ValueError('Coarse predecessor unavailable or malformed') from error


def replay_coarse_predecessor(config):
    """Run the actual frozen numerical replay; never substitute an affirmative callback."""
    validate_coarse_predecessor(config)
    d=config['predecessor']
    return replay.replay_n4_predecessor(d['dataset_root'],d['runtime_root'],d['observation']['path'])


def _coarse_read_pins(config):
    """Bind original records and all files consumed by the historical replay for fast recheck."""
    validate_coarse_predecessor(config)
    d=config['predecessor'];observation=_pin(d['observation'],OBSERVATION_SHA256)
    report=parse_json(replay._read(observation['path'],observation['sha256']))
    pins=[dict(d[role]) for role in FIXED]+[dict(observation),dict(config['lineage']['parent_claim'])]
    root=Path(__file__).resolve().parents[3]
    replay._sources(report)
    pins.extend(dict(path=str(replay._relative(root,name)),sha256=sha)
        for name,sha in report['source_files'].items())
    runtime=replay._path(d['runtime_root'],directory=True)
    _,_,runtime_pins=replay._inputs(runtime,report)
    pins.extend(dict(path=str(replay._relative(runtime,name)),sha256=sha)
        for name,sha in runtime_pins.items())
    dataset=replay._path(d['dataset_root'],directory=True)
    replay._raw(dataset,report)
    pins.extend(dict(path=str(replay._relative(dataset,row['owner_relative_path'])),sha256=row['sha256'])
        for row in report['source_evidence'].values())
    observed={}
    for pin in pins:
        path=replay._path(pin['path']);sha=pin['sha256']
        _require(path not in observed or observed[path]==sha,'Conflicting predecessor pin')
        replay._read(path,sha);observed[path]=sha
    return [dict(path=str(path),sha256=sha) for path,sha in observed.items()]


def coarse_read_pins(config):
    """Return verified path/digest rows; malformed or missing dependencies refuse uniformly."""
    try:
        return _coarse_read_pins(config)
    except (KeyError,TypeError,AttributeError,OSError) as error:
        raise ValueError('Coarse replay pin dependencies unavailable or malformed') from error
