"""Synthetic four-attempt integration; no native process or engineering authority.

Real reference replay, adapters, protocol validators and numerical criteria run.
Only launch, Git-blob access for the temporary owning checkout, and external
authority/preflight/adjudication are synthetic. Native binary placeholders are
labelled synthetic and do not establish MAPDL compatibility.
"""
import json
from pathlib import Path

import pytest

from digitalmodel.ansys import cylinder_adapter as adapter
from digitalmodel.ansys import cylinder_reference_provenance as provenance
from digitalmodel.ansys.analysis_records import canonical_bytes, digest_bytes
from digitalmodel.ansys.cylinder_benchmark import build_case
from digitalmodel.ansys.cylinder_canary import ORDER, PROFILE, run_canary, runtime_sources
from tests.ansys.cylinder_synthetic_protocol import synthetic_protocol


def reference_checkout(root, monkeypatch):
    source = Path(adapter.__file__).resolve().parents[3]
    relative = Path('examples/ansys/cylinder-benchmark')
    evidence = source / relative
    invocation = json.loads((evidence / 'checker_invocation.json').read_bytes())
    names = ['reference.json', 'reference_comparison.json', *provenance.CAPTURE_FILES]
    files = {str(relative / name).replace('\\', '/'): (evidence / name).read_bytes() for name in names}
    files.update({row['path']: (source / row['path']).read_bytes() for row in invocation['frozen_files']})
    for name, raw in files.items():
        target = root / name
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_bytes(raw)
    (root / '.git').mkdir()
    def git_blob(checkout, commit, name):
        assert Path(checkout) == root
        assert commit in (provenance.INPUT_COMMIT, provenance.CAPTURE_COMMIT)
        return files[name]
    monkeypatch.setattr(provenance, '_git_blob', git_blob)
    return files, str(relative / 'reference.json').replace('\\', '/')


def workflow_setup(tmp_path, monkeypatch, fault):
    bundle = tmp_path / 'owning-checkout'
    bundle.mkdir()
    files, reference = reference_checkout(bundle, monkeypatch)
    cases = []
    for case_id in ORDER:
        name = case_id + '.inp'
        raw = build_case(case_id)['deck_bytes']
        (bundle / name).write_bytes(raw)
        files[name] = raw
        cases.append({'case_id': case_id, 'deck': name})
    manifest = {'schema': 'cylinder-b1-1', 'case_order': list(ORDER), 'cases': cases,
                'reference': reference, 'runtime_sources': runtime_sources(),
                'artifacts': [{'path': path, 'sha256': digest_bytes(raw)} for path, raw in sorted(files.items())]}
    (bundle / 'manifest.json').write_bytes(canonical_bytes(manifest))
    executable = tmp_path / 'synthetic-never-executed.exe'
    executable.write_bytes(b'SYNTHETIC TEST PLACEHOLDER; NEVER EXECUTED')
    profile = dict(release='SYNTHETIC RELEASE', build='0.0',
                   update='20000101', platform='SYNTHETIC PLATFORM')
    approval = {'approval_id': 'synthetic-workflow', 'operator_id': 'synthetic-operator',
                'checker_id': 'synthetic-checker', 'ledger_directory': str(tmp_path / 'ledger'),
                'execution_host': 'synthetic-host', 'profile': PROFILE,
                'runtime_profile': profile, 'manifest_sha256': digest_bytes(canonical_bytes(manifest)),
                'executable_sha256': digest_bytes(executable.read_bytes()),
                'capture_allowance_bytes': 100_000_000, 'reserve_bytes': 1_000_000}
    launched = []
    monkeypatch.setattr(adapter, '_launch_case', synthetic_launcher(launched, profile, fault))
    callbacks = adapter.make_execution_adapters(bundle, executable, approval)
    callbacks.update(authority_callbacks(approval))
    return bundle, tmp_path / 'run', approval, callbacks, launched


def synthetic_launcher(launched, profile, fault):
    def launch(case, directory, timeout, executable):
        assert timeout == 300 and executable.name == 'synthetic-never-executed.exe'
        launched.append(case['case_id'])
        _, artifacts = synthetic_protocol(case['case_id'])
        identity = (f" RELEASE= {profile['release']} BUILD= {profile['build']} "
                    f"UP{profile['update']} VERSION={profile['platform']}\n")
        artifacts['native.out'] = identity.encode() + artifacts['native.out']
        if fault == 'control' and len(launched) == 1:
            artifacts['state_values.txt'] = artifacts['state_values.txt'].replace(b'NSET    ', b'BADSET  ', 1)
        mapping = {'native.out': Path(case['deck']).stem + '.out',
                   'jobname.err': 'file.err', 'stdout': 'stdout.bin', 'stderr': 'stderr.bin'}
        for name, raw in artifacts.items():
            (directory / mapping.get(name, name)).write_bytes(raw)
        for name in ('file.rst', 'file.db', 'file.mntr'):
            if not (fault == 'missing' and name == 'file.rst'):
                (directory / name).write_bytes(b'SYNTHETIC NONEMPTY PLACEHOLDER; NOT NATIVE FORMAT')
        return {'return_code': 0, 'timed_out': False, 'owned_processes_remaining': 0,
                'stdout': b'', 'stderr': b'', 'duration_seconds': '1',
                'containment_verified': True, 'evidence_complete': True,
                'settlement_required': False, 'streams_finalized': True}
    return launch


def authority_callbacks(approval):
    def preflight(a):
        return {'license_query': 'SYNTHETIC external authority fixture',
                'exclusive_seat_owner': a['operator_id'], 'process_inventory': [],
                'free_bytes': 200_000_000, 'source_rights': 'SYNTHETIC generated fixture rights',
                'execution_host': a['execution_host'], 'executable_sha256': a['executable_sha256'],
                'profile': a['profile']}
    return {'verify_authority': lambda a: a == approval, 'preflight': preflight,
            'adjudicate': lambda receipt, a: {'checker_id': a['checker_id'],
                'receipt_sha256': digest_bytes(canonical_bytes(receipt)),
                'scope': 'SYNTHETIC authority fixture; no human engineering adjudication'}}


def test_real_operator_adapter_validator_and_criteria_four_case_pass(tmp_path, monkeypatch):
    bundle, root, approval, callbacks, launched = workflow_setup(tmp_path, monkeypatch, None)
    result = run_canary(bundle, root, approval, **callbacks)
    assert result['status'] == 'PASS', result.get('reason')
    assert launched == list(ORDER)
    assert len(result['records']) == 4 and result['unattempted'] == []
    assert all(row['engineering_qualified'] is False for row in result['records'])
    assert all(not row['evidence_errors'] for row in result['records'])
    assert all(len(row['values']) == 9 for row in result['records'])
    assert (root / 'outcome.json').is_file()


@pytest.mark.parametrize('fault', ['control', 'missing'])
def test_real_workflow_refuses_first_case_fault_without_second_launch(tmp_path, monkeypatch, fault):
    bundle, root, approval, callbacks, launched = workflow_setup(tmp_path, monkeypatch, fault)
    result = run_canary(bundle, root, approval, **callbacks)
    assert result['status'] == 'INCOMPLETE'
    assert launched == [ORDER[0]]
    assert result['attempted'] == [ORDER[0]] and len(result['unattempted']) == 3
    assert (root / ORDER[0] / 'execution.json').is_file()
