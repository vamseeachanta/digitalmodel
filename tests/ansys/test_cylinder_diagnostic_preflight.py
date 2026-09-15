"""Synthetic observations only; native/licence/process probes are replaced."""
import base64
import copy
from decimal import localcontext
import os
from pathlib import Path
from types import SimpleNamespace

import pytest

from digitalmodel.ansys import cylinder_diagnostic_preflight as module
from digitalmodel.ansys.analysis_records import canonical_bytes, digest_bytes


LICENSE = (b'Feature "ansys" v2026.0202, expiry: permanent(no expiration date)\r\n'
           b'Total of 2 licenses issued; Total of 0 floating non-reserved licenses in use\r\n'
           b'Total of 0 users queued; Total of 0 licenses reserved\r\n')


@pytest.fixture
def prepared(tmp_path, monkeypatch):
    config = {'license_server': '1055@synthetic', 'lock_path': str(tmp_path / 'lock')}
    for key, raw in [('executable', b'exe'), ('license_utility', b'utility'),
                     ('cfd_binding_path', b'null'), ('source_rights_path', b'{"scope":"test"}')]:
        path = tmp_path / key
        path.write_bytes(raw)
        config[key] = str(path)
        pin = key.removesuffix('_path') + '_sha256'
        config[pin] = digest_bytes(raw)
    bundle = tmp_path / 'bundle'
    bundle.mkdir()
    (bundle / 'manifest.json').write_bytes(b'{}')
    config.update(bundle=str(bundle), output_directory=str(tmp_path / 'output'))
    approval = dict(operator_id='SOLVERS', execution_host='TEST-HOST',
                    executable_sha256=config['executable_sha256'],
                    manifest_sha256=digest_bytes(b'{}'),
                    profile={'cores': 1, 'parallel': 'smp', 'timeout_seconds': 300},
                    launch_environment={'ANSYS261_PRODUCT': 'ansys', 'ANS_CONSEC': 'YES'},
                    runtime_profile={'update': '20260202'})
    lock = dict(pid=os.getpid(), path=config['lock_path'], device=1, inode=2,
                content_sha256=digest_bytes(str(os.getpid()).encode()))
    reservation = SimpleNamespace(evidence=lambda: copy.deepcopy(lock))
    for key, value in {**approval['launch_environment'],
                       'ANSYSLMD_LICENSE_FILE': config['license_server']}.items():
        monkeypatch.setenv(key, value)
    monkeypatch.setattr(module, '_now', lambda: '1006')
    monkeypatch.setattr(module, '_host', lambda: 'TEST-HOST')
    monkeypatch.setattr(module, '_free_bytes', lambda path: 10**12)
    monkeypatch.setattr(module, '_license_query', lambda config: dict(
        argv=['synthetic'], stdout=LICENSE, stderr=b'', returncode=0, observed_at='1006'))
    capacity = {'samples': [dict(observed_at=str(1001+i), interval_seconds='1',
                                cpu_percent='10', logical_processors=8,
                                available_memory_bytes=16*1024**3) for i in range(5)]}
    monkeypatch.setattr(module, '_capacity', lambda: copy.deepcopy(capacity))
    snapshot = dict(schema='process-snapshot-1', host='TEST-HOST', observed_at='1006',
                    enumeration_complete=True, rows=[], coverage=dict(
                        selector='ansys-mpi-lineage-v1', enumerated_count=8,
                        selected_count=0, excluded_count=8, selected_details_complete=True))
    monkeypatch.setattr(module, 'collect_process_snapshot', lambda binding: copy.deepcopy(snapshot))
    return config, approval, reservation, lock, snapshot


def test_preflight_retains_original_license_bytes_and_scoped_observations(prepared):
    config, approval, reservation, _, _ = prepared
    collector = module.ProductionPreflight(config, approval, reservation)
    result = collector(approval)
    assert result['process_inventory'] == []
    assert result['process_inventory_scope'] == 'blocking-projection-only'
    assert result['exclusive_seat_owner'] == 'SOLVERS'
    assert result['lock_evidence']['pid'] == os.getpid()
    assert base64.b64decode(result['license_observation']['stdout_base64']) == LICENSE
    assert result['license_observation']['stdout_sha256'] == digest_bytes(LICENSE)
    assert result['raw_inventory'] == []
    assert result['capacity']['status'] == 'PASS'
    canonical_bytes(result)
    collector.before_launch()


@pytest.mark.parametrize('change', ['host', 'environment', 'endpoint', 'approval',
                                    'rights', 'utility', 'executable', 'manifest', 'lock_pid'])
def test_preflight_refuses_binding_changes(prepared, monkeypatch, change):
    config, approval, reservation, lock, _ = prepared
    collector = module.ProductionPreflight(config, approval, reservation)
    supplied = copy.deepcopy(approval)
    if change == 'host':
        monkeypatch.setattr(module, '_host', lambda: 'OTHER')
    elif change == 'environment':
        monkeypatch.setenv('ANSYS261_PRODUCT', 'other')
    elif change == 'endpoint':
        monkeypatch.setenv('ANSYSLMD_LICENSE_FILE', 'other')
    elif change == 'approval':
        supplied['operator_id'] = 'OTHER'
    elif change == 'lock_pid':
        lock['pid'] += 1
    else:
        key = {'rights': 'source_rights_path', 'utility': 'license_utility',
               'executable': 'executable'}.get(change)
        path = Path(config[key]) if key else Path(config['bundle']) / 'manifest.json'
        path.write_bytes(b'changed')
    with pytest.raises(ValueError):
        collector(supplied)


@pytest.mark.parametrize('change', ['stale', 'environment', 'endpoint', 'lock', 'license'])
def test_before_launch_rechecks_and_never_retries(prepared, monkeypatch, change):
    config, approval, reservation, lock, _ = prepared
    collector = module.ProductionPreflight(config, approval, reservation)
    collector(approval)
    if change == 'stale':
        monkeypatch.setattr(module, '_now', lambda: '1037')
    elif change == 'environment':
        monkeypatch.setenv('ANS_CONSEC', 'NO')
    elif change == 'endpoint':
        monkeypatch.setenv('ANSYSLMD_LICENSE_FILE', 'other')
    elif change == 'lock':
        lock['inode'] += 1
    else:
        monkeypatch.setattr(module, '_license_query', lambda config: dict(
            argv=['synthetic'], stdout=b'failed', stderr=b'error', returncode=1,
            observed_at='1006'))
    with pytest.raises(ValueError):
        collector.before_launch()


def test_before_launch_requires_successful_preflight(prepared):
    config, approval, reservation, _, _ = prepared
    with pytest.raises(ValueError):
        module.ProductionPreflight(config, approval, reservation).before_launch()


def test_license_query_uses_fixed_argv_and_captures_bytes(prepared, monkeypatch):
    config, _, _, _, _ = prepared
    calls = []
    def run(argv, **kwargs):
        calls.append((argv, kwargs))
        return SimpleNamespace(stdout=LICENSE, stderr=b'raw\r\n', returncode=0)
    monkeypatch.setattr(module.subprocess, 'run', run)
    result = module._run_license_query(config)
    assert calls == [([config['license_utility'], 'lmstat', '-f', 'ansys',
                      '--no-user-info', '-c', config['license_server'], '-t', '10'],
                     dict(capture_output=True, timeout=20, check=False))]
    assert result['stdout'] == LICENSE
    assert result['stderr'] == b'raw\r\n'


def test_snapshot_selects_mpi_descendants_and_pins_not_terminal_siblings(monkeypatch):
    from digitalmodel.ansys import cylinder_diagnostic_snapshot as snapshot

    rows = [dict(pid=1, parent_pid=0, name='terminal.exe', executable_path='C:/terminal.exe'),
            dict(pid=2, parent_pid=1, name='mpiexec.exe', executable_path='C:/mpiexec.exe'),
            dict(pid=3, parent_pid=2, name='worker.exe', executable_path='C:/worker.exe'),
            dict(pid=4, parent_pid=1, name='unrelated.exe', executable_path='C:/unrelated.exe'),
            dict(pid=5, parent_pid=1, name='guard.exe', executable_path='C:/guard.exe')]
    monkeypatch.setattr(snapshot, '_enumerate', lambda: rows)
    monkeypatch.setattr(snapshot, '_details', lambda row, cache: dict(row))
    result = snapshot.collect_process_snapshot({'processes': [{'pid': 5}]})
    assert [r['pid'] for r in result['rows']] == [2, 3, 5]
    assert result['coverage']['excluded_count'] == 2
    assert result['initial_inventory'] == rows


def test_snapshot_refuses_selected_missing_details(monkeypatch):
    from digitalmodel.ansys import cylinder_diagnostic_snapshot as snapshot

    monkeypatch.setattr(snapshot, '_enumerate', lambda: [
        dict(pid=2, parent_pid=1, name='mpiexec.exe', executable_path=None)])
    def missing(*args):
        raise ValueError('selected details unavailable')
    monkeypatch.setattr(snapshot, '_details', missing)
    with pytest.raises(ValueError, match='details'):
        snapshot.collect_process_snapshot(None)


def test_timeout_keeps_original_partial_query_bytes(prepared, monkeypatch):
    config, approval, reservation, _, _ = prepared
    def timeout(argv, **kwargs):
        raise module.subprocess.TimeoutExpired(argv, 20, output=b'partial\r\n', stderr=b'raw error')
    monkeypatch.setattr(module.subprocess, 'run', timeout)
    monkeypatch.setattr(module, '_license_query', module._run_license_query)
    collector = module.ProductionPreflight(config, approval, reservation)
    with pytest.raises(ValueError, match='licence query failed'):
        collector(approval)
    retained = collector.last_evidence['license_observation']
    assert base64.b64decode(retained['stdout_base64']) == b'partial\r\n'
    assert base64.b64decode(retained['stderr_base64']) == b'raw error'
    assert retained['failure'] == 'timeout'
    assert retained['returncode'] is None


def test_long_postclaim_query_cannot_age_prior_observations_past_bound(prepared, monkeypatch):
    config, approval, reservation, _, _ = prepared
    collector = module.ProductionPreflight(config, approval, reservation)
    collector(approval)
    clock = {'time': '1025'}
    monkeypatch.setattr(module, '_now', lambda: clock['time'])
    def delayed(config):
        clock['time'] = '1040'
        return dict(argv=['synthetic'], stdout=LICENSE, stderr=b'',
                    returncode=0, observed_at='1040')
    monkeypatch.setattr(module, '_license_query', delayed)
    with pytest.raises(ValueError, match='stale'):
        collector.before_launch()


def test_unknown_process_projection_cannot_enable_before_launch(prepared):
    config, approval, reservation, _, snapshot = prepared
    snapshot['rows'] = [dict(pid=200, parent_pid=0, creation_time='1000',
        name='mpiexec.exe', executable_path='C:/synthetic/mpiexec.exe',
        executable_sha256='a'*64, argv=['C:/synthetic/mpiexec.exe'], script_sources=[])]
    snapshot['coverage'].update(selected_count=1, excluded_count=7)
    collector = module.ProductionPreflight(config, approval, reservation)
    result = collector(approval)
    assert result['classification']['status'] == 'UNKNOWN'
    assert result['process_inventory'][0]['pid'] == 200
    assert result['raw_inventory'] == snapshot['rows']
    with pytest.raises(ValueError, match='clear preflight'):
        collector.before_launch()


def test_snapshot_refuses_recycled_pid_with_matching_image(tmp_path, monkeypatch):
    from digitalmodel.ansys import cylinder_diagnostic_snapshot as snapshot

    executable = tmp_path / 'mpiexec.exe'
    executable.write_bytes(b'image')
    process = SimpleNamespace(pid=5, create_time=lambda: 2000.0,
        name=lambda: 'mpiexec.exe', ppid=lambda: 1, exe=lambda: str(executable),
        cmdline=lambda: [str(executable)], is_running=lambda: True)
    monkeypatch.setattr(snapshot.psutil, 'Process', lambda pid: process)
    initial = dict(pid=5, parent_pid=1, name='mpiexec.exe',
                   executable_path=str(executable), creation_time='1000.0')
    with pytest.raises(ValueError, match='changed'):
        snapshot._details(initial, {})


def test_freshness_refuses_small_excess_under_low_decimal_precision():
    with localcontext() as context:
        context.prec = 2
        with pytest.raises(ValueError, match='stale'):
            module._fresh('1006', '1036.0001')


@pytest.mark.parametrize('name', ['ansysli_server.exe', 'ansyslmd.exe',
                                  'ansysli_monitor.exe', 'ansyslmutil.exe'])
def test_resident_license_program_is_not_a_solver_seed(name):
    from digitalmodel.ansys import cylinder_diagnostic_snapshot as snapshot

    assert snapshot._seed(dict(name=name,
        executable_path='C:/Program Files/ANSYS Inc/Shared Files/licensing/' + name)) is False


@pytest.mark.parametrize('stderr', [b'query error\r\n', b' \r\n'])
def test_successful_license_stdout_does_not_override_stderr(prepared, monkeypatch, stderr):
    config, approval, reservation, _, _ = prepared
    monkeypatch.setattr(module, '_license_query', lambda config: dict(
        argv=['synthetic'], stdout=LICENSE, stderr=stderr, returncode=0,
        observed_at='1006'))
    collector = module.ProductionPreflight(config, approval, reservation)
    with pytest.raises(ValueError, match='stderr'):
        collector(approval)
    retained = collector.last_evidence['license_observation']
    assert base64.b64decode(retained['stderr_base64']) == stderr
    assert retained['stderr_sha256'] == digest_bytes(stderr)
    assert base64.b64decode(retained['stdout_base64']) == LICENSE


def _set_cfd_binding(config, binding):
    raw = canonical_bytes(binding)
    Path(config['cfd_binding_path']).write_bytes(raw)
    config['cfd_binding_sha256'] = digest_bytes(raw)


def test_pressure_scope_refuses_v1_in_nested_operational_config(prepared):
    from digitalmodel.ansys.cylinder_pressure_admission import SCOPE

    config, approval, reservation, _, _ = prepared
    approval['scope'] = SCOPE
    _set_cfd_binding(config, {'schema': 'cfd-process-binding-1'})
    assert 'schema' not in config
    with pytest.raises(ValueError, match='pressure.*v2'):
        module.ProductionPreflight(config, approval, reservation)._bindings()


def test_historical_scope_retains_v1_binding_route(prepared):
    config, approval, reservation, _, _ = prepared
    binding = {'schema': 'cfd-process-binding-1'}
    _set_cfd_binding(config, binding)
    assert module.ProductionPreflight(config, approval, reservation)._bindings() == binding


def test_v2_owner_resolution_repeats_and_retains_evidence(prepared, monkeypatch):
    from digitalmodel.ansys import cylinder_cfd_owner_evidence as owner
    from digitalmodel.ansys.cylinder_pressure_admission import SCOPE

    config, approval, reservation, _, _ = prepared
    approval['scope'] = SCOPE
    binding = {'schema': 'cfd-process-binding-2'}
    _set_cfd_binding(config, binding)
    calls = []
    evidence = {'scope': 'synthetic-fixed-source-relationships'}
    def resolve(operation, supplied, read_callback):
        calls.append((operation, supplied, read_callback))
        return evidence.copy()
    monkeypatch.setattr(owner, 'resolve_owner_evidence', resolve)
    collector = module.ProductionPreflight(config, approval, reservation)
    for _ in range(3):
        assert collector._bindings() == binding
    assert calls == [(config, binding, module._read_owner)] * 3
    assert collector.last_evidence['cfd_owner_evidence'] == evidence


def test_v2_missing_owner_evidence_refuses(prepared):
    config, approval, reservation, _, _ = prepared
    _set_cfd_binding(config, {'schema': 'cfd-process-binding-2'})
    with pytest.raises(ValueError):
        module.ProductionPreflight(config, approval, reservation)._bindings()


def test_owner_reader_rejects_oversize_before_open(tmp_path, monkeypatch):
    path = tmp_path / 'oversize.json'
    path.write_bytes(b'x' * (4 * 1024 * 1024 + 1))
    monkeypatch.setattr(Path, 'open', lambda *a, **k: pytest.fail('oversize file opened'))
    with pytest.raises(ValueError, match='bound'):
        module._read_owner(path, 'a' * 64)


def test_pressure_owner_recheck_runs_in_actual_phase2(prepared, monkeypatch):
    from digitalmodel.ansys import cylinder_cfd_owner_evidence as owner
    from digitalmodel.ansys import cylinder_pressure_resources as resources
    from digitalmodel.ansys.cylinder_pressure_admission import SCOPE

    config, approval, reservation, _, _ = prepared
    approval['scope'] = SCOPE
    _set_cfd_binding(config, {'schema': 'cfd-process-binding-2'})
    calls = []
    monkeypatch.setattr(owner, 'resolve_owner_evidence',
                        lambda *args: calls.append('owner') or {'synthetic': True})
    monkeypatch.setattr(resources, '_now', lambda: '1006')
    monkeypatch.setattr(module, 'classify_process_inventory',
                        lambda *a, **k: {'status': 'CLEAR', 'process_inventory': []})
    collector = module.ProductionPreflight(config, approval, reservation)
    collector(approval)
    collector.before_launch()
    assert calls == ['owner', 'owner']
    monkeypatch.setattr(module, '_license_query', lambda *a: pytest.fail('postclaim query'))
    result = resources.production_phase2(collector, lambda supplied: supplied == approval, approval)
    assert result['status'] == 'PASS'
    assert calls == ['owner', 'owner', 'owner']
