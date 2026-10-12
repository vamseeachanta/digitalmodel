"""Synthetic intermediate-mesh orchestration; no solver execution."""
from copy import deepcopy
from pathlib import Path
import pytest
from digitalmodel.ansys import cylinder_pressure_resume as resume
from digitalmodel.ansys.analysis_records import canonical_bytes, digest_bytes, parse_json
from .test_cylinder_pressure_resume import Harness

CASE = 'ocv-t60-p10-n8'

class Intermediate(Harness):
    def __init__(self, root):
        super().__init__(root)
        self.config['scope'].update(ordinal=3, case_ids=[CASE])
        parent = self.config['lineage']['parent_claim']
        path = Path(parent['path']).with_name(Path(parent['path']).stem + '.ordinal-2.json')
        raw = canonical_bytes(dict(ordinal=2, case_id='ocv-t60-p10-n4',
            parent_sha256=parent['sha256'], state='attempt_consumed'))
        path.write_bytes(raw)
        self.config['predecessor'] = dict(claim=dict(path=str(path),sha256=digest_bytes(raw)))
        self.approval.update(deepcopy(self.config))
        self.captured.update(case_id=CASE, numerical_assessment='NOT_EVALUATED',
            reason='PRESSURE_NUMERICAL_ASSESSMENT_NOT_EVALUATED')

    def launch(self, case, directory, timeout):
        assert case['case_id'] == CASE and timeout == 300
        self.events.append('launch'); self.launches.append((CASE, timeout))
        if self.failure == 'spawn': raise OSError('synthetic spawn failure')
        return deepcopy(self.execution)

    def capture(self, case, directory, execution):
        assert case['case_id'] == CASE
        return deepcopy(self.captured)

    def record(self, suffix):
        parent = Path(self.config['lineage']['parent_claim']['path'])
        return parent.with_name(parent.stem+'.ordinal-3'+suffix+'.json')

    def preclaim(self, approval):
        assert approval == self.approval
        self.events.append('preclaim_probe')
        self.clock.advance(getattr(self, 'preclaim_delay', 0))
        if self.failure == 'preclaim':
            raise ValueError('synthetic probe refusal')
        return {'status': 'PASS', 'synthetic': True}

    def run(self):
        admission = dict(approval=self.approval, verify_authority=self.verify,
                         checker_id='synthetic-reviewer')
        return resume.execute_pressure_resume(self.config, admission,
            replay_prefix=self.replay, preflight=self, phase2_recheck=self.phase2,
            preclaim_recheck=self.preclaim, launch=self.launch, capture=self.capture,
            account_storage=self.storage, clock=self.clock, reservation=self.reservation)

@pytest.fixture
def intermediate(tmp_path, monkeypatch):
    harness = Intermediate(tmp_path)
    monkeypatch.setattr(resume, 'verify_streams', lambda config:
                        {'stdout': 'synthetic-stdout', 'stderr': 'synthetic-stderr'})
    def replay(config):
        harness.events.append('coarse_replay')
        if harness.failure == 'coarse_replay': raise ValueError('altered coarse evidence')
        return dict(schema='synthetic-coarse-replay', replay_status='COMPLETE')
    monkeypatch.setattr(resume, 'replay_coarse_predecessor', replay, raising=False)
    return harness

def test_intermediate_uses_third_claim_and_exact_case(intermediate):
    result = intermediate.run()
    assert intermediate.launches == [(CASE, 300)]
    assert result['case_id'] == CASE and result['consumed_count'] == 3
    assert result['terminal_reason'] == 'PLANNED_SCOPE_STOP'
    assert result['assessment_reason'] == 'PRESSURE_NUMERICAL_ASSESSMENT_NOT_EVALUATED'
    assert result['accepted_values'] == {} and result['engineering_qualified'] is False
    assert intermediate.events.index('coarse_replay') < intermediate.events.index('preflight')
    output = Path(intermediate.config['operational']['output_directory'])
    assert (output / 'attempt-3.json').is_file() and not (output/'attempt-2.json').exists()
    assert (output / 'coarse-prefix-replay.json').is_file()
    assert all(intermediate.record(s).is_file() for s in ('.invocation', '', '.terminal'))
    assert parse_json(intermediate.record('').read_bytes())['ordinal'] == 3

@pytest.mark.parametrize('failure', ['coarse_replay', 'preflight', 'before_launch', 'storage'])
def test_preclaim_failure_leaves_two_consumed(intermediate, failure):
    intermediate.failure = failure
    result = intermediate.run()
    assert result['case_id'] == CASE and result['consumed_count'] == 2
    assert result['native_launch_count'] == 0 and not intermediate.launches
    assert not intermediate.record('').exists()
    assert not intermediate.record('.invocation').exists()
    output = Path(intermediate.config['operational']['output_directory'])
    assert parse_json((output/'preparation-refusal.json').read_bytes()) == result

@pytest.mark.parametrize('failure', ['phase2', 'spawn'])
def test_postclaim_failure_never_reopens_attempt(intermediate, failure):
    intermediate.failure = failure
    result = intermediate.run()
    assert result['consumed_count'] == 3
    assert result['terminal_reason'] == 'POSTCLAIM_INCOMPLETE'
    with pytest.raises(ValueError): intermediate.run()

def test_changed_frozen_case_order_refuses_before_launch(intermediate):
    bundle = Path(intermediate.config['operational']['bundle'])
    manifest = parse_json((bundle/'manifest.json').read_bytes())
    manifest['cases'].reverse()
    raw=canonical_bytes(manifest); (bundle/'manifest.json').write_bytes(raw)
    intermediate.approval['manifest_sha256']=digest_bytes(raw)
    result = intermediate.run()
    assert result['case_id'] == CASE and result['terminal_reason'] == 'PRECLAIM_REFUSAL'
    assert intermediate.launches == []


@pytest.mark.parametrize('key,value', [('case_id', 'ocv-t60-p10-n4'),
    ('numerical_assessment', 'PASS'), ('reason', 'PRESSURE_PARSER_NOT_VALIDATED')])
def test_intermediate_capture_contract_mismatch_is_incomplete(intermediate, key, value):
    intermediate.captured[key] = value
    result = intermediate.run()
    assert result['terminal_reason'] == 'CAPTURE_CHECKS_INCOMPLETE'
    assert result['consumed_count'] == 3 and result['engineering_qualified'] is False


def test_changed_predecessor_after_consumption_retains_outcome(intermediate):
    original = intermediate.capture
    def capture(*args):
        result = original(*args)
        Path(intermediate.config['predecessor']['claim']['path']).write_bytes(b'changed')
        return result
    intermediate.capture = capture
    result = intermediate.run()
    assert result['consumed_count'] == 3
    assert 'TERMINAL_RETENTION_INCOMPLETE' in result['terminal_reasons']
    assert result['reservation_released'] is True
    output = Path(intermediate.config['operational']['output_directory'])
    assert parse_json((output/'outcome.json').read_bytes()) == result
    assert 'terminal_record_error' in result and not intermediate.record('.terminal').exists()


def test_outcome_write_failure_still_returns_failure_record(intermediate, monkeypatch):
    original = resume._write_exclusive
    def write(path, value):
        if path.name == 'outcome.json': raise OSError('synthetic outcome write failure')
        return original(path, value)
    monkeypatch.setattr(resume, '_write_exclusive', write)
    result = intermediate.run()
    assert result['consumed_count'] == 3 and 'outcome_record_error' in result
    assert 'OUTCOME_RETENTION_INCOMPLETE' in result['terminal_reasons']


def test_combined_capture_failures_have_unique_reasons(intermediate):
    intermediate.captured.update(case_id='wrong',retention_status='INCOMPLETE')
    result = intermediate.run()
    assert result['terminal_reasons'] == ['CAPTURE_CHECKS_INCOMPLETE']


def test_failed_phase2_retains_last_preflight_observation(intermediate):
    intermediate.failure = 'phase2'
    result = intermediate.run()
    assert result['last_preflight_evidence'] == intermediate.last_evidence
    output = Path(intermediate.config['operational']['output_directory'])
    assert parse_json((output/'outcome.json').read_bytes())['last_preflight_evidence'] == intermediate.last_evidence
