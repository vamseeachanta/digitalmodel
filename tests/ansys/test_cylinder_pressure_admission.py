"""Synthetic authority fixtures only: no native, licence or ledger operations."""
import copy
import json
from pathlib import Path

import pytest

from digitalmodel.ansys.analysis_records import canonical_bytes, digest_bytes
from digitalmodel.ansys import cylinder_pressure_admission as admission
from digitalmodel.ansys import cylinder_pressure_lineage as lineage
from digitalmodel.ansys.cylinder_canary import PROFILE


def write(path, value):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(canonical_bytes(value))
    return dict(path=str(path), sha256=digest_bytes(path.read_bytes()))


def refresh(f):
    f['config_pin'] = write(f['config_path'], f['config'])['sha256']
    files = []
    for path in [f['config_path'], *f['source_root'].rglob('*.py')]:
        files.append(dict(path=str(path), sha256=digest_bytes(path.read_bytes()),
                          content=path.read_text(encoding='utf-8')))
    bundle = write(f['bundle'], dict(context='Synthetic offline test', files=files))
    review = dict(verdict='MINOR', findings=f['findings'], bundle_sha256=bundle['sha256'])
    transport = write(f['stdout'], dict(is_error=False, session_id=f['session'], structured_output=review))
    receipt = dict(status='REVIEW_RECEIVED', exit_code=0, review=review,
                   stdout_sha256=transport['sha256'], bundle_sha256=bundle['sha256'],
                   files=[{k:r[k] for k in ('path','sha256')} for r in files])
    f['review_pin'] = write(f['receipt'], receipt)['sha256']


def make(f):
    return admission.make_pressure_admission(f['config_path'], f['receipt'], f['stdout'], f['bundle'],
        expected_config_sha256=f['config_pin'], expected_review_sha256=f['review_pin'],
        source_root=f['source_root'])


def history(tmp, monkeypatch):
    root = tmp/'original'; root.mkdir()
    rows = []
    for i in range(23):
        path = root/('attempt-1.json' if i == 0 else f'file-{i}.json')
        value = dict(case_id='ocv-zero-t60-n16', ordinal=1, state='attempt_consumed', input_sha256='1'*64) if i == 0 else {'synthetic':i}
        pin = write(path,value); rows.append(dict(path=path.name, sha256=pin['sha256'], bytes=path.stat().st_size))
    inv = write(tmp/'inventory.json',dict(root=str(root),files=rows))
    base = dict(schema='cylinder-b1-1',case_order=list(lineage.ORDER),
                cases=[dict(case_id=c,deck=f'prepared/{c}.inp',metadata=f'prepared/{c}.json') for c in lineage.ORDER],
                reference='reference.json',artifacts=[])
    for i in range(21):
        name=f'fixture-{i}.txt'; data=b'synthetic artifact';
        for folder in ('base','executed','successor'):
            target=tmp/folder/name;target.parent.mkdir(exist_ok=True);target.write_bytes(data)
        base['artifacts'].append(dict(path=name,sha256=digest_bytes(data),bytes=len(data)))
    pins={k:write(tmp/k/'manifest.json',base) for k in ('base','executed','successor')}
    cfg=write(tmp/'old-config.json',dict(campaign_id='parent',execution_binding=dict(manifest_sha256=pins['executed']['sha256'])))
    app=write(tmp/'approval.json',dict(approval_id='parent',config_sha256=cfg['sha256'],manifest_sha256=pins['executed']['sha256']))
    claim=write(tmp/'ledger'/(digest_bytes(b'parent')+'.json'),dict(approval_id='parent',approval_sha256=app['sha256'],output=str(root)))
    baseline=write(tmp/'r3.json',dict(dataset_id='ansys-retained-evidence',package_hash='a'*64,
        cases=[{}]*8+[dict(case_id='ocv-zero-t60-n16',row_hash='b'*64)]))
    monkeypatch.setattr(lineage,'FIXED',dict(approval=app['sha256'],base_manifest=pins['base']['sha256'],
        executed_manifest=pins['executed']['sha256'],baseline_file=baseline['sha256'],
        baseline_package='a'*64,baseline_row='b'*64,inventory=inv['sha256']))
    monkeypatch.setattr(lineage,'ORIGINAL_BYTES',sum(r['bytes'] for r in rows))
    monkeypatch.setattr(lineage,'_validate_baseline',lambda d: None)
    monkeypatch.setattr(lineage,'_required_artifacts',lambda d:set(r['path'] for r in d['artifacts']))
    return dict(original_root=str(root),inventory=inv,parent_claim=claim,
        attempt=dict(path=str(root/'attempt-1.json'),sha256=rows[0]['sha256']),approval=app,
        config=cfg,base_manifest=pins['base'],executed_manifest=pins['executed'],
        baseline=dict(path=baseline['path'],file_sha256=baseline['sha256'],package_hash='a'*64,row_hash='b'*64)),pins['successor']


@pytest.fixture
def fixture(tmp_path,monkeypatch):
    (tmp_path.parent/'_coordination').mkdir(exist_ok=True)
    hist, successor=history(tmp_path,monkeypatch)
    monkeypatch.setattr(admission,'_git_blob',lambda root,revision,name:(root/name).read_bytes(),raising=False)
    source=tmp_path/'source'; name='scripts/ansys/run_pressure_diagnostic.py'
    p=source/name;p.parent.mkdir(parents=True);p.write_bytes(b'# synthetic source\n')
    monkeypatch.setattr(admission,'required_source_inventory',lambda root:[dict(path=name,sha256=digest_bytes(p.read_bytes()))])
    runtime=[dict(path=name,sha256=digest_bytes(p.read_bytes()))]
    monkeypatch.setattr(admission.cylinder_canary,'runtime_sources',lambda:runtime)
    manifest_path=Path(successor['path']);manifest=json.loads(manifest_path.read_bytes())
    manifest.update(runtime_sources=runtime,runtime_lineage=dict(
        original_manifest_sha256=hist['base_manifest']['sha256'],
        runtime_inventory_sha256=digest_bytes(canonical_bytes(runtime))))
    successor=write(manifest_path,manifest)
    config=dict(schema='cylinder-pressure-admission-1',source_revision='e'*40,campaign_id='pressure-once',operator_id='SOLVERS',
        ledger_directory=str(tmp_path/'ledger'),lineage=hist,scope=copy.deepcopy(admission.SCOPE),
        source_files=[dict(path=name,sha256=digest_bytes(p.read_bytes()))],
        operational=dict(bundle=str(Path(successor['path']).parent),
                         output_directory=str(tmp_path/'output')),
        execution_binding=dict(manifest_sha256=successor['sha256'],executable_sha256='c'*64,
        execution_host='synthetic-host',profile=copy.deepcopy(PROFILE),
        runtime_profile=dict(release='2026 R1.01',build='26.1',update='20260202',platform='WINDOWS x64'),
        launch_environment=dict(ANSYS261_PRODUCT='ansys',ANS_CONSEC='YES'),
        capture_allowance_bytes=1073741824,reserve_bytes=2147483648))
    f=dict(config=config,source_root=source,config_path=tmp_path/'config.json',receipt=tmp_path/'review.json',
           stdout=tmp_path/'stdout.json',bundle=tmp_path/'bundle.json',findings=[],session='independent-synthetic')
    refresh(f);return f


def test_offline_authority_revalidates_without_claims(fixture):
    before=set(Path(fixture['config']['ledger_directory']).iterdir())
    result=make(fixture)
    assert result['approval']['scope']==admission.SCOPE
    assert result['verify_authority'](result['approval']) is True
    assert set(Path(fixture['config']['ledger_directory']).iterdir())==before


@pytest.mark.parametrize('finding',['F1 MAJOR: defect','[MAJOR] defect','CRITICAL: defect','F2 BLOCKER defect'])
def test_nonblocking_verdict_cannot_hide_blocking_finding(fixture,finding):
    fixture['findings']=[finding];refresh(fixture)
    with pytest.raises(ValueError,match='blocking|contradict'):make(fixture)


@pytest.mark.parametrize('key',['baseline_file','baseline_package','baseline_row'])
def test_reviewed_repin_cannot_substitute_historical_baseline(fixture,key):
    b=fixture['config']['lineage']['baseline']
    b[{'baseline_file':'file_sha256','baseline_package':'package_hash','baseline_row':'row_hash'}[key]]='d'*64
    refresh(fixture)
    with pytest.raises(ValueError,match='baseline'):make(fixture)


@pytest.mark.parametrize('mutation',['extra','missing','changed'])
def test_complete_original_inventory_required(fixture,mutation):
    root=Path(fixture['config']['lineage']['original_root'])
    if mutation=='extra':(root/'unlisted').write_bytes(b'extra')
    elif mutation=='missing':(root/'file-2.json').unlink()
    else:(root/'file-2.json').write_bytes(b'changed')
    with pytest.raises(ValueError,match='original|inventory|digest|unavailable'):make(fixture)


def test_changed_approval_is_rejected_even_with_reviewed_repin(fixture):
    pin=fixture['config']['lineage']['approval'];pin.update(write(Path(pin['path']),{'wrong':'approval'}));refresh(fixture)
    with pytest.raises(ValueError,match='approval'):make(fixture)


def test_changed_successor_artifact_is_rejected(fixture):
    (Path(fixture['config']['operational']['bundle'])/'fixture-0.txt').write_bytes(b'changed')
    with pytest.raises(ValueError,match='artifact|digest'):make(fixture)


def test_approval_cannot_be_modified(fixture):
    result=make(fixture);result['approval']['scope']['ordinal']=3
    with pytest.raises(ValueError,match='approval'):result['verify_authority'](result['approval'])


def test_source_mutation_after_factory_refuses(fixture):
    result=make(fixture);next(fixture['source_root'].rglob('*.py')).write_bytes(b'changed')
    with pytest.raises(ValueError):result['verify_authority'](result['approval'])


def test_operator_cannot_be_independent_reviewer(fixture):
    fixture['session']='SOLVERS';refresh(fixture)
    with pytest.raises(ValueError,match='independent'):make(fixture)


def test_scope_cannot_expand_even_under_new_review(fixture):
    fixture['config']['scope']['case_ids'].append('ocv-t60-p10-n8');refresh(fixture)
    with pytest.raises(ValueError,match='scope'):make(fixture)


def test_entrypoint_cannot_be_omitted(fixture):
    fixture['config']['source_files']=[];refresh(fixture)
    with pytest.raises(ValueError,match='source|inventory'):make(fixture)


def test_successor_stale_runtime_inventory_refuses(fixture):
    p=Path(fixture['config']['operational']['bundle'])/'manifest.json'
    d=json.loads(p.read_bytes());d['runtime_sources']=[]
    fixture['config']['execution_binding']['manifest_sha256']=write(p,d)['sha256'];refresh(fixture)
    with pytest.raises(ValueError,match='runtime'):make(fixture)


def test_parent_claim_relocated_refuses(fixture):
    pin=fixture['config']['lineage']['parent_claim'];p=Path(pin['path'])
    pin.update(write(p.with_name('other.json'),json.loads(p.read_bytes())));refresh(fixture)
    with pytest.raises(ValueError,match='parent claim'):make(fixture)


def test_review_transport_session_mutation_refuses(fixture):
    result=make(fixture);p=fixture['stdout'];d=json.loads(p.read_bytes());d['session_id']='replaced';write(p,d)
    with pytest.raises(ValueError,match='digest'):result['verify_authority'](result['approval'])


def test_baseline_bytes_mutated_after_factory_refuse(fixture):
    result=make(fixture)
    Path(fixture['config']['lineage']['baseline']['path']).write_bytes(b'{}')
    with pytest.raises(ValueError,match='digest'):result['verify_authority'](result['approval'])


def test_missing_original_parent_claim_refuses(fixture):
    Path(fixture['config']['lineage']['parent_claim']['path']).unlink()
    with pytest.raises(ValueError,match='unavailable'):make(fixture)


@pytest.mark.parametrize('name',['scripts/ansys/run_pressure_diagnostic.py',
    'src/digitalmodel/__init__.py','src/digitalmodel/ansys/runner.py'])
def test_every_reviewed_source_must_match_committed_blob(fixture,monkeypatch,name):
    p=fixture['source_root']/name;p.parent.mkdir(parents=True,exist_ok=True)
    if not p.exists():
        p.write_bytes(b'# reviewed working bytes\n')
        fixture['config']['source_files'].append(dict(path=name,sha256=digest_bytes(p.read_bytes())))
    refresh(fixture)
    monkeypatch.setattr(admission,'_git_blob',lambda root,rev,item:b'# different committed bytes' if item==name else (root/item).read_bytes(),raising=False)
    with pytest.raises(ValueError,match='commit'):make(fixture)


@pytest.mark.parametrize('revision',['main','e'*39,True,None])
def test_source_revision_requires_full_commit_identifier(fixture,revision):
    fixture['config']['source_revision']=revision;refresh(fixture)
    with pytest.raises(ValueError,match='revision|commit'):make(fixture)


def test_blocking_review_precedes_lineage_work(fixture,monkeypatch):
    fixture['findings']=['F1 MAJOR: unresolved'];refresh(fixture)
    def forbidden(config):
        raise AssertionError('lineage/replay preparation preceded review rejection')
    monkeypatch.setattr(admission,'validate_lineage',forbidden)
    with pytest.raises(ValueError,match='blocking'):make(fixture)


def test_required_inventory_includes_parent_compatibility_module():
    root = Path(admission.__file__).resolve().parents[3]
    names = {row['path'] for row in admission.required_source_inventory(root)}
    assert 'src/digitalmodel/_compat.py' in names


@pytest.mark.parametrize('finding',['INFO: no major concerns','INFO: not a blocker',
    'MINOR - no major concerns','MINOR: prior MAJOR finding resolved'])
def test_negated_or_historical_prose_is_not_a_blocking_label(fixture,finding):
    fixture['findings']=[finding];refresh(fixture)
    assert make(fixture)['checker_id']==fixture['session']


@pytest.mark.parametrize('finding',['BLOCKING (MAJOR) - unresolved','F1: MAJOR - unresolved',
    'Finding severity MAJOR: unresolved','F2 SEVERE - unresolved','[SEVERE] unresolved',
    'SEVERE: unresolved'])
def test_explicit_and_unknown_severity_labels_refuse(fixture,finding):
    fixture['findings']=['MINOR: first finding',finding];refresh(fixture)
    with pytest.raises(ValueError,match='blocking|severity'):make(fixture)


def test_required_inventory_uses_resolved_module_path(monkeypatch):
    path=Path(admission.__file__).resolve();root=path.parents[3]
    monkeypatch.chdir(root);monkeypatch.setattr(admission,'__file__',str(path.relative_to(root)))
    names={row['path'] for row in admission.required_source_inventory(root)}
    assert 'src/digitalmodel/_compat.py' in names
    assert admission.ENTRYPOINT in names


@pytest.mark.parametrize('finding',['severe: unresolved','[severe] unresolved'])
def test_unknown_explicit_labels_are_case_insensitive(fixture,finding):
    fixture['findings']=[finding];refresh(fixture)
    with pytest.raises(ValueError,match='blocking|severity'):make(fixture)


def test_bracketed_prose_reference_is_not_an_unknown_severity(fixture):
    fixture['findings']=['MINOR: see [API] documentation'];refresh(fixture)
    assert make(fixture)['checker_id']==fixture['session']
