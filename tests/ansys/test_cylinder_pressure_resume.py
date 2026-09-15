"""Synthetic ordinal-2 orchestration contracts; no production/native adapters."""
from copy import deepcopy
import json
from pathlib import Path

import pytest

from digitalmodel.ansys import cylinder_pressure_resume as resume
from digitalmodel.ansys.cylinder_benchmark import build_case
from digitalmodel.ansys.cylinder_criteria import EXPECTED_KEYS, evaluate_attempt
from digitalmodel.ansys.analysis_replay import make_check_receipt
from digitalmodel.ansys.cylinder_canary import ORDER, runtime_sources
from digitalmodel.ansys.analysis_records import canonical_bytes, digest_bytes

CASE_ID = 'ocv-t60-p10-n4'
ZERO_ID = 'ocv-zero-t60-n16'


def bundle_fixture(root):
    """Real frozen decks; synthetic reference/authority, never numerical evidence."""
    root.mkdir();(root/'prepared').mkdir()
    manifest=dict(schema='cylinder-b1-1',case_order=list(ORDER),cases=[],artifacts=[],
                  reference='reference.json',runtime_sources=runtime_sources())
    files={'reference.json':b'{}'}
    for case_id in ORDER:
        case=build_case(case_id);deck=f'prepared/{case_id}.inp';metadata=f'prepared/{case_id}.json'
        files[deck]=case.pop('deck_bytes');files[metadata]=canonical_bytes(case)
        manifest['cases'].append(dict(case_id=case_id,deck=deck,metadata=metadata))
    for name,raw in files.items():
        (root/name).write_bytes(raw)
        manifest['artifacts'].append(dict(path=name,sha256=digest_bytes(raw),bytes=len(raw)))
    raw=canonical_bytes(manifest);(root/'manifest.json').write_bytes(raw)
    return digest_bytes(raw)



def actual_zero_receipt_shape():
    # Synthetic zero values; real criterion/receipt APIs, no native or reference derivation.
    values = {key:'0' for key in EXPECTED_KEYS}
    assessment = evaluate_attempt(ZERO_ID, values, values, '0', [])
    assert assessment['status'] == 'PASS'
    receipt = make_check_receipt('0'*64, [], assessment['checks'], dict(values, RFY='0'))
    return dict(case_id=ZERO_ID, assessment_status='complete', replay_checks=receipt)


class Clock:
    def __init__(self):
        self.elapsed = 0
        self.utc_offset = 0

    def monotonic_ns(self):
        return self.elapsed

    def time_ns(self):
        return 1800000000000000000 + self.elapsed + self.utc_offset

    def advance(self, nanoseconds):
        self.elapsed += nanoseconds


class Reservation:
    def __init__(self):
        self.releases = []

    def evidence(self):
        return {'synthetic': 'owned-reservation'}

    def release(self, *, no_owned_processes):
        self.releases.append(no_owned_processes)
        return no_owned_processes


class Harness:
    def __init__(self, tmp_path):
        self.clock, self.reservation = Clock(), Reservation()
        self.events, self.launches = [], []
        self.replay_delay = self.phase2_delay = self.launch_delay = self.capture_delay = 0
        self.failure = None
        ledger = tmp_path/'ledger'; ledger.mkdir()
        parent = ledger/('a'*64+'.json');parent.write_bytes(b'{"synthetic":"parent"}')
        output = tmp_path/'capture'
        bundle=tmp_path/'bundle';manifest_sha=bundle_fixture(bundle)
        self.config = dict(ledger_directory=str(ledger),operational=dict(output_directory=str(output),bundle=str(bundle)),
            lineage=dict(parent_claim=dict(path=str(parent),sha256=digest_bytes(parent.read_bytes()))),
            campaign_id='synthetic-ordinal2',scope=dict(case_ids=[CASE_ID],ordinal=2,max_attempts=1,
                capture_only=True,qualification='diagnostic_only'))
        self.approval = dict(self.config,manifest_sha256=manifest_sha,config_sha256='c'*64,review_receipt_sha256='d'*64)
        self.prefix = actual_zero_receipt_shape()
        self.execution = dict(return_code=0,timed_out=False,owned_processes_remaining=0,
            containment_verified=True,evidence_complete=True,settlement_required=False,
            streams_finalized=True,stdout=b'',stderr=b'',error=[],retained_supervisor_token=None,duration_seconds=0)
        self.captured = dict(retention_status='COMPLETE',independent_check_status='COMPLETE',
            capture_status='INCOMPLETE',accepted_values={},engineering_qualified=False,
            assessment_status='NOT_EVALUATED',reason='PRESSURE_PARSER_NOT_VALIDATED',
            unverified_evidence=['pressure_loads_and_support_conditions','station_values.txt'])

    def verify(self, approval):
        self.events.append('verify')
        assert approval == self.approval
        if self.failure == 'authority': raise ValueError('blocking review')
        return True

    def replay(self, approval):
        self.events.append('replay'); self.clock.advance(self.replay_delay)
        assert approval == self.approval
        return deepcopy(self.prefix)

    def __call__(self, approval):
        self.events.append('preflight')
        if self.failure == 'preflight': raise ValueError('resource refusal')
        return {'synthetic': 'fresh resource evidence'}

    def before_launch(self):
        self.events.append('before_launch')
        if self.failure == 'before_launch': raise ValueError('licence observation refused')
        now = str(self.clock.time_ns() // 1000000000)
        self._ready_at = now
        self.last_evidence = dict(capacity_observation={'samples':[{'observed_at':now}]},
            process_snapshot={'observed_at':now}, license_observation={'observed_at':now})
        return deepcopy(self.last_evidence)

    def phase2(self, *args):
        self.events.append('phase2'); self.clock.advance(self.phase2_delay)
        if self.failure == 'utc_rollback': self.clock.utc_offset = -1000000000
        if self.failure == 'phase2': raise ValueError('changed local ownership')

    def launch(self, case_id, directory, timeout):
        case_id=case_id['case_id']
        self.events.append('launch');self.launches.append((case_id,timeout))
        assert case_id == CASE_ID and timeout == 300
        self.clock.advance(self.launch_delay)
        if self.failure == 'spawn': raise OSError('synthetic spawn failure')
        return deepcopy(self.execution)

    def capture(self, case_id, directory, execution):
        self.events.append('capture');self.clock.advance(self.capture_delay)
        assert case_id['case_id'] == CASE_ID
        return deepcopy(self.captured)

    def storage(self, *args):
        self.events.append('storage')
        if self.failure == 'storage': raise ValueError('reserve insufficient')
        return {'status': 'PASS', 'synthetic': True}

    def run(self):
        admission = dict(approval=self.approval,verify_authority=self.verify,checker_id='synthetic-reviewer')
        return resume.execute_pressure_resume(self.config,admission,replay_prefix=self.replay,
            preflight=self,phase2_recheck=self.phase2,launch=self.launch,capture=self.capture,
            account_storage=self.storage,clock=self.clock,reservation=self.reservation)

    def record(self, suffix):
        parent = Path(self.config['lineage']['parent_claim']['path'])
        return parent.with_name(parent.stem+'.ordinal-2'+suffix+'.json')


@pytest.fixture
def harness(tmp_path):
    return Harness(tmp_path)


def test_one_n4_after_verified_zero_and_no_numerical_acceptance(harness):
    result = harness.run()
    assert harness.launches == [(CASE_ID,300)]
    assert harness.events.index('verify') < harness.events.index('replay')
    assert harness.events.count('verify') >= 2
    assert harness.events.index('before_launch') < harness.events.index('phase2') < harness.events.index('launch')
    assert result['terminal_reason'] == 'PLANNED_SCOPE_STOP'
    assert result['consumed_count'] == 2 and result['native_launch_count'] == 1
    assert result['accepted_values'] == {} and result['assessment_status'] == 'NOT_EVALUATED'
    assert result['campaign_status'] == 'INCOMPLETE'
    assert result['capture']['capture_status'] == 'INCOMPLETE'
    assert result['capture']['unverified_evidence'] == harness.captured['unverified_evidence']
    assert all(harness.record(s).is_file() for s in ('.invocation','','.terminal'))


@pytest.mark.parametrize('failure',['authority','preflight','before_launch','storage'])
def test_preclaim_refusal_never_launches_or_creates_successor(harness,failure):
    harness.failure=failure
    result=harness.run()
    assert not harness.launches and not harness.record('').exists()
    assert result['consumed_count']==1 and result['native_launch_count']==0
    if failure != 'authority':
        assert harness.record('.invocation').exists() and harness.record('.terminal').exists()
        with pytest.raises(ValueError):harness.run()


@pytest.mark.parametrize('mutation',['missing','short','failed','wrong-case'])
def test_prefix_requires_concrete_64_zero_checks(harness,mutation):
    if mutation=='missing':harness.prefix={'status':'PASS'}
    elif mutation=='short':harness.prefix['replay_checks']['checks'].pop()
    elif mutation=='failed':harness.prefix['replay_checks']['checks'][0]['passed']=False
    else:harness.prefix['case_id']=CASE_ID
    result=harness.run()
    assert not harness.launches and result['consumed_count']==1
    assert harness.record('.terminal').exists()


@pytest.mark.parametrize('suffix',['.invocation','','.terminal'])
@pytest.mark.parametrize('content',[b'',b'{'])
def test_existing_or_partial_record_never_overwritten(harness,suffix,content):
    path=harness.record(suffix);path.write_bytes(content)
    with pytest.raises(ValueError):harness.run()
    assert path.read_bytes()==content and not harness.launches


def test_restart_cannot_change_output_or_campaign_to_readmit(harness):
    harness.run()
    before=harness.record('.invocation').read_bytes()
    harness.config['campaign_id']='different-campaign'
    harness.config['operational']['output_directory'] += '-different'
    with pytest.raises(ValueError):harness.run()
    assert len(harness.launches)==1 and harness.record('.invocation').read_bytes()==before


@pytest.mark.parametrize('seconds,launches,consumed',[(234,1,2),(235,1,2),(236,0,1)])
def test_365_second_preclaim_boundary(harness,seconds,launches,consumed):
    harness.replay_delay=seconds*1000000000
    result=harness.run()
    assert len(harness.launches)==launches and result['consumed_count']==consumed


@pytest.mark.parametrize('nanoseconds,launches',[(4999999999,1),(5000000000,1),(5000000001,0)])
def test_360_second_launch_and_five_second_phase2_boundaries(harness,nanoseconds,launches):
    harness.replay_delay=235000000000;harness.phase2_delay=nanoseconds
    result=harness.run()
    assert len(harness.launches)==launches and result['consumed_count']==2


@pytest.mark.parametrize('failure',['phase2','utc_rollback','spawn'])
def test_postclaim_failure_consumes_without_readmission(harness,failure):
    harness.failure=failure
    result=harness.run()
    assert harness.record('').exists() and result['consumed_count']==2
    assert result['terminal_reason'] != 'PLANNED_SCOPE_STOP'
    with pytest.raises(ValueError):harness.run()


def test_retention_precedes_deadline_disposition(harness):
    harness.replay_delay=235000000000;harness.phase2_delay=5000000000
    harness.launch_delay=305000000000;harness.capture_delay=56000000000
    harness.execution.update(timed_out=True,return_code=None,settlement_required=True)
    result=harness.run()
    assert 'capture' in harness.events and harness.clock.elapsed==601000000000
    assert result['terminal_reason'] != 'PLANNED_SCOPE_STOP'
    assert result['consumed_count']==2 and harness.record('.terminal').exists()


def test_unknown_pressure_format_retention_is_not_solver_failure(harness):
    harness.captured['independent_check_status']='INCOMPLETE'
    result=harness.run()
    assert result['capture']['retention_status']=='COMPLETE'
    assert result['capture']['independent_check_status']=='INCOMPLETE'
    assert result['terminal_reason'] != 'PLANNED_SCOPE_STOP'
    assert result['accepted_values']=={} and result['native_launch_count']==1


def test_unsettled_owned_process_never_releases_reservation(harness):
    harness.execution.update(owned_processes_remaining=None,settlement_required=True,
        evidence_complete=False,retained_supervisor_token='synthetic-owned-handle')
    result=harness.run()
    assert not any(harness.reservation.releases)
    assert result['terminal_reason'] != 'PLANNED_SCOPE_STOP'


class Crash(BaseException):
    pass


@pytest.mark.parametrize('suffix',['.invocation.json','.ordinal-2.json','.terminal.json'])
def test_crash_after_each_durable_write_refuses_restart(harness,monkeypatch,suffix):
    original=resume._write_exclusive
    def interrupted(path,value):
        original(path,value)
        if str(path).endswith(suffix):raise Crash('synthetic process death after durable write')
    monkeypatch.setattr(resume,'_write_exclusive',interrupted)
    with pytest.raises(Crash):harness.run()
    monkeypatch.setattr(resume,'_write_exclusive',original)
    with pytest.raises(ValueError):harness.run()
    assert len(harness.launches)<=1


def test_storage_failure_status_is_not_accepted(harness):
    harness.storage = lambda: {'status':'FAIL'}
    result = harness.run()
    assert not harness.launches and result['consumed_count'] == 1


def test_stale_preclaim_observation_refuses(harness):
    original = harness.before_launch
    def stale():
        result = original()
        harness.last_evidence['license_observation']['observed_at'] = '0'
        return result
    harness.before_launch = stale
    result = harness.run()
    assert not harness.launches and result['consumed_count'] == 1


def test_launch_exception_does_not_invent_native_completion(harness):
    harness.failure = 'spawn'
    result = harness.run()
    assert result['launch_adapter_calls'] == 1
    assert result['native_launch_count'] is None
    assert result['no_owned_processes_established'] is False


def test_duplicate_prefix_check_is_rejected(harness):
    harness.prefix['replay_checks']['checks'][1] = harness.prefix['replay_checks']['checks'][0]
    assert harness.run()['consumed_count'] == 1
    assert not harness.launches


@pytest.mark.parametrize('stage', ['launch', 'capture'])
def test_final_storage_observed_after_partial_failure(harness, stage):
    original = getattr(harness, stage)
    def failed(*args):
        (Path(harness.config['operational']['output_directory'])/'partial.bin').write_bytes(b'partial')
        original(*args)
        raise OSError('synthetic partial evidence failure')
    setattr(harness, stage, failed)
    result = harness.run()
    assert 'final_storage_before_receipts' in result
    assert result['terminal_reason'] != 'PLANNED_SCOPE_STOP'


def test_final_storage_can_refuse_after_capture(harness):
    calls = []
    def accounting():
        calls.append(1)
        return {'status':'FAIL' if len(calls) == 3 else 'PASS'}
    harness.storage = accounting
    result = harness.run()
    assert result['final_storage_before_receipts']['status'] == 'FAIL'
    assert result['terminal_reason'] != 'PLANNED_SCOPE_STOP'


def test_real_criterion_receipt_key_shape_is_accepted(harness):
    assert isinstance(harness.prefix['replay_checks']['checks'][0]['response'], list)
    assert harness.run()['terminal_reason'] == 'PLANNED_SCOPE_STOP'


def test_foreign_prefix_response_is_rejected(harness):
    harness.prefix['replay_checks']['checks'][0]['response'] = ['foreign-station', 'sigma_r']
    assert harness.run()['consumed_count'] == 1
    assert not harness.launches


def test_execution_failure_and_deadline_remain_distinct(harness):
    harness.execution.update(return_code=1, evidence_complete=False)
    harness.capture_delay = 601000000000
    result = harness.run()
    assert result['terminal_reason'] == 'EXECUTION_INCOMPLETE'
    assert 'DEADLINE_EXCEEDED' in result['terminal_reasons']


@pytest.mark.parametrize('stage', ['preflight_write', 'case_copy', 'storage'])
def test_entire_preclaim_interval_counts_against_freshness(harness, monkeypatch, stage):
    if stage == 'preflight_write':
        original = resume._write_exclusive
        def write(path, value):
            result = original(path, value)
            if path.name == 'preflight.json':
                harness.clock.advance(21_000_000_000)
            return result
        monkeypatch.setattr(resume, '_write_exclusive', write)
    elif stage == 'case_copy':
        original = resume._prepare_case
        def prepare(*args):
            result = original(*args)
            harness.clock.advance(21_000_000_000)
            return result
        monkeypatch.setattr(resume, '_prepare_case', prepare)
    else:
        original = harness.storage
        def storage(*args):
            result = original(*args)
            harness.clock.advance(21_000_000_000)
            return result
        harness.storage = storage
    result = harness.run()
    assert not harness.launches
    assert result['consumed_count'] == 1
    assert not harness.record('').exists()
