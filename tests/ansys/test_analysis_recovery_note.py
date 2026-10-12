"""Synthetic migration tests; fixtures confer no native or engineering authority."""
from copy import deepcopy
import json
import pytest

from digitalmodel.ansys.analysis_evidence import publish_package, read_response_csv
from digitalmodel.ansys.analysis_records import canonical_bytes, digest_bytes
from digitalmodel.ansys.analysis_matrix_publish import publish_matrix, _relation
from tests.ansys.test_analysis_pressure_observed import pressure, build, rehash


@pytest.fixture
def note_data(pressure, monkeypatch):
    from digitalmodel.ansys import analysis_recovery_note as note
    baseline = build(pressure)
    monkeypatch.setattr(note, 'BASELINE_HASH', baseline['package_hash'])
    quantities = ('sigma_r', 'sigma_theta', 'sigma_z', 'tau_rz', 'sigma_vm', 'u_r', 'u_z')
    checks = [dict(criterion='accuracy', response=[s+'_y120', q], residual='0',
                   limit='0.010', passed=True) for s in ('inner', 'middle', 'outer') for q in quantities]
    checks[14].update(criterion='expected_zero', residual='0.01059709996930703', passed=False)
    checks.append(dict(criterion='axial_equilibrium', response='RFY', residual='0', limit='1', passed=True))
    values = [dict(station_id=s+'_y'+str(y), quantity=q,
                   value='0.01059709996930703' if s=='outer' and q=='sigma_r' else '0',
                   unit='mm' if q.startswith('u_') else 'MPa',
                   origin='derived_von_mises' if q=='sigma_vm' else 'native_export')
              for y in (60,120,180) for s in ('inner','middle','outer') for q in quantities]
    observation = dict(case_id=note.CASE_ID, run_id='synthetic-run',
        observed_utc='2026-09-16T00:00:00Z', recovery_status='COMPLETE', recovery_errors=[],
        engineering_qualified=False, native_launches=0, replay_driver_sha256='a'*64,
        owner_manifest_sha256='b'*64, reference_sha256=baseline['criteria_reference']['sha256'],
        source_evidence={'native.out':{'sha256':pressure['receipt']['raw'][note.CASE_ID+'.out']['sha256']}},
        values=values, assessment=dict(status='CONTINUE', checks=checks,
            engineering_qualified=False, independent_adjudication='required outside numerical evaluator'),
        limits=['Synthetic fixture; CONTINUE is not PASS.'])
    resolver=pressure['resolver']; pins={}
    for role, content in [('observation',canonical_bytes(observation)),('report',b'synthetic html'),('review_archive',b'synthetic archive')]:
        old=note.REFERENCES[role]; p=pressure['tmp']/('note-'+role);p.write_bytes(content)
        pins[role]=dict(id=old['id'],sha256=digest_bytes(content));resolver[old['id']]=p
    monkeypatch.setattr(note,'REFERENCES',pins)
    return baseline,resolver,observation,note


def test_note_preserves_every_case_and_null_response(note_data):
    baseline,resolver,_,note=note_data; candidate=note.build_recovery_note(baseline,resolver)
    assert canonical_bytes(candidate['cases'])==canonical_bytes(baseline['cases'])
    assert candidate['coverage']==baseline['coverage']
    assert all(r['value'] is None for r in candidate['cases'][9]['responses'])
    assert candidate['diagnostic_supplement']['assessment']['checks'][14]['passed'] is False
    assert candidate['diagnostic_supplement']['numerical_adoption']=='NOT_ADOPTED'
    _relation(candidate,baseline,resolver)


@pytest.mark.parametrize('field', ['coverage','cases','finding_ledger','code_revision','diagnostic_supplement'])
def test_rehashed_unrelated_or_promoted_mutation_refused(note_data,field):
    baseline,resolver,_,note=note_data; candidate=note.build_recovery_note(baseline,resolver)
    if field=='coverage':candidate[field]['qualified_responses']=1
    elif field=='cases':candidate[field][0]['author']='forged'
    elif field=='finding_ledger':candidate[field][0]['disposition']='resolved'
    elif field=='code_revision':candidate[field]='forged'
    else:candidate[field]['transformation_source_files']={}
    candidate=rehash(candidate,resolver)
    with pytest.raises(ValueError):_relation(candidate,baseline,resolver)


@pytest.mark.parametrize('role',['observation','report','review_archive'])
def test_corrupted_reference_refused_before_publication(note_data,tmp_path,role):
    baseline,resolver,_,note=note_data; candidate=note.build_recovery_note(baseline,resolver)
    owner=tmp_path/'owner';publish_package(baseline,owner)
    manifest=tmp_path/'matrix.json';before=canonical_bytes(baseline);manifest.write_bytes(before)
    resolver[note.REFERENCES[role]['id']].write_bytes(b'tampered')
    with pytest.raises(ValueError):publish_matrix(candidate,baseline,manifest,owner,resolver)
    assert manifest.read_bytes()==before


def test_wrong_baseline_pin_refused(note_data):
    baseline,resolver,_,note=note_data;changed=deepcopy(baseline);changed['revision']='wrong'
    changed=rehash(changed,resolver)
    with pytest.raises(ValueError,match='baseline'):note.build_recovery_note(changed,resolver)


def test_publish_resume_and_csv_revision_preserve_rows(note_data,tmp_path):
    baseline,resolver,_,note=note_data;candidate=note.build_recovery_note(baseline,resolver)
    owner=tmp_path/'owner';publish_package(baseline,owner)
    manifest=tmp_path/'matrix.json';manifest.write_bytes(canonical_bytes(baseline))
    publish_matrix(candidate,baseline,manifest,owner,resolver)
    manifest.with_name('responses.csv').write_text('interrupted CSV refresh')
    publish_matrix(candidate,baseline,manifest,owner,resolver)
    records=read_response_csv(manifest.with_name('responses.csv').read_text())
    assert len(records)==350 and {r['revision'] for r in records}=={'r6'}
    hashes={c['case_id']:c['row_hash'] for c in baseline['cases']}
    assert all(r['row_hash']==hashes[r['case_id']] for r in records)
    assert json.loads(manifest.read_bytes())==candidate


def test_unchanged_metadata_successor_after_r6_remains_publishable(note_data,tmp_path):
    baseline,resolver,_,note=note_data;r6=note.build_recovery_note(baseline,resolver)
    owner=tmp_path/'owner';publish_package(r6,owner)
    manifest=tmp_path/'matrix.json';manifest.write_bytes(canonical_bytes(r6))
    r7=deepcopy(r6);r7.update(revision='r7',previous_package_hash=r6['package_hash'])
    r7=rehash(r7,resolver)
    publish_matrix(r7,r6,manifest,owner,resolver)
    assert json.loads(manifest.read_bytes())==r7


@pytest.mark.parametrize('mutation',['qualified','native','assessment','check_count','failure','origins'])
def test_observation_scope_guards_with_synthetic_repinned_evidence(note_data,monkeypatch,mutation):
    baseline,resolver,observation,note=note_data;report=deepcopy(observation)
    if mutation=='qualified':report['engineering_qualified']=True
    elif mutation=='native':report['native_launches']=True
    elif mutation=='assessment':report['assessment']['status']='PASS'
    elif mutation=='check_count':report['assessment']['checks'].pop()
    elif mutation=='failure':report['assessment']['checks'][14]['limit']='0.02'
    else:report['values'][0]['origin']='derived_von_mises'
    raw=canonical_bytes(report);ref=note.REFERENCES['observation']
    resolver[ref['id']].write_bytes(raw)
    monkeypatch.setitem(ref,'sha256',digest_bytes(raw))
    with pytest.raises(ValueError):note.build_recovery_note(baseline,resolver)
