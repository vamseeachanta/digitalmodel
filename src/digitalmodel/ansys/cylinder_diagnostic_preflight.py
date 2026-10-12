"""Production observations for a reviewed diagnostic; no affirmative defaults.

Construction performs no observations. Calling the collector queries host and
licence state but never launches a solver or acquires the cooperative lock.
The root driver must retain last_evidence even when an observation refuses.
"""
import base64
from copy import deepcopy
from fractions import Fraction
import os
from pathlib import Path
import shutil
import re
import socket
import subprocess
import time

import psutil

from digitalmodel.ansys.analysis_records import canonical_bytes, digest_bytes, parse_json
from digitalmodel.ansys.cylinder_canary import PROFILE
from digitalmodel.ansys.cylinder_diagnostic_resources import (
    parse_compatible_license, validate_capacity, validate_environment,
)
from digitalmodel.ansys.cylinder_diagnostic_snapshot import collect_process_snapshot
from digitalmodel.ansys.cylinder_process_inventory import classify_process_inventory


def _now():
    return str(time.time())


def _host():
    return socket.gethostname()


def _owned_path(value):
    path = Path(value)
    if not path.is_absolute() or any(p.is_symlink() or p.is_junction()
                                     for p in (path, *path.parents)):
        raise ValueError('preflight path relative or redirected')
    return path


def _read(value, expected):
    raw = _owned_path(value).read_bytes()
    if digest_bytes(raw) != expected:
        raise ValueError('preflight file digest changed')
    return raw


def _read_owner(value, expected):
    path = _owned_path(value)
    maximum = 4 * 1024 * 1024
    if path.stat().st_size > maximum:
        raise ValueError('owner source exceeds read bound')
    with path.open('rb') as stream:
        raw = stream.read(maximum + 1)
    if len(raw) > maximum or digest_bytes(raw) != expected:
        raise ValueError('owner source bound or digest differs')
    return raw


def _free_bytes(path):
    return shutil.disk_usage(_owned_path(path).parent).free


def _decimal_nanoseconds(value):
    if type(value) is not int or value < 0:
        raise ValueError('nonnegative integer nanoseconds required')
    seconds, nanoseconds = divmod(value, 1_000_000_000)
    fraction = f'{nanoseconds:09d}'.rstrip('0')
    return str(seconds) + ('.' + fraction if fraction else '')


def _absence_snapshot():
    from .cylinder_absence_collection import collect_absence_snapshot
    return collect_absence_snapshot()


def _capacity():
    samples = []
    previous_end = None
    for _ in range(5):
        start = time.time_ns()
        if previous_end is not None and start < previous_end:
            raise ValueError('capacity wall clock moved backward between samples')
        cpu = psutil.cpu_percent(interval=1)
        end = time.time_ns()
        if end < start:
            raise ValueError('capacity wall clock moved backward during sample')
        samples.append(dict(observed_at=_decimal_nanoseconds(end),
                            interval_seconds=_decimal_nanoseconds(end-start),
                            logical_processors=psutil.cpu_count(), cpu_percent=str(cpu),
                            available_memory_bytes=psutil.virtual_memory().available))
        previous_end = end
    return {'samples': samples}


def _run_license_query(config):
    argv = [config['license_utility'], 'lmstat', '-f', 'ansys', '--no-user-info',
            '-c', config['license_server'], '-t', '10']
    try:
        result = subprocess.run(argv, capture_output=True, timeout=20, check=False)
        return dict(argv=argv, stdout=result.stdout, stderr=result.stderr,
                    returncode=result.returncode, observed_at=_now())
    except subprocess.TimeoutExpired as error:
        return dict(argv=argv, stdout=error.stdout or b'', stderr=error.stderr or b'',
                    returncode=None, observed_at=_now(), failure='timeout')
    except OSError as error:
        return dict(argv=argv, stdout=b'', stderr=b'', returncode=None,
                    observed_at=_now(), failure=type(error).__name__)


def _license_query(config):
    return _run_license_query(config)


def _encoded_query(observation):
    result = {k: v for k, v in observation.items() if k not in ('stdout', 'stderr')}
    for key in ('stdout', 'stderr'):
        raw = observation[key]
        result[key+'_base64'] = base64.b64encode(raw).decode('ascii')
        result[key+'_sha256'] = digest_bytes(raw)
    return result


def _fresh(observed, now):
    for value in (observed, now):
        if (not isinstance(value, str) or len(value) > 64
                or not re.fullmatch(r'(?:0|[1-9][0-9]*)(?:\.[0-9]+)?', value)):
            raise ValueError('preflight timestamp requires bounded decimal text')
    age = Fraction(now) - Fraction(observed)
    if not 0 <= age <= 30:
        raise ValueError('preflight observation stale or future')


class ProductionPreflight:
    """Bind actual observations to the immutable driver configuration/approval."""

    def __init__(self, config, approval, reservation):
        self.config = deepcopy(config)
        self.approval = deepcopy(approval)
        self.reservation = reservation
        self.last_evidence = {}
        self._ready_at = None
        self._lock = None
        self._absence_mode = False

    def _bindings(self):
        config, approval = self.config, self.approval
        if _host() != approval['execution_host'] or approval['profile'] != PROFILE:
            raise ValueError('actual host or fixed profile differs')
        environment = validate_environment(approval, os.environ)
        if os.environ.get('ANSYSLMD_LICENSE_FILE') != config['license_server']:
            raise ValueError('inherited licence endpoint differs')
        environment['ANSYSLMD_LICENSE_FILE'] = config['license_server']
        _read(config['executable'], approval['executable_sha256'])
        _read(config['license_utility'], config['license_utility_sha256'])
        _read(Path(config['bundle'])/'manifest.json', approval['manifest_sha256'])
        _owned_path(config['output_directory'])
        rights = _read(config['source_rights_path'], config['source_rights_sha256'])
        binding = _read_owner(config['cfd_binding_path'], config['cfd_binding_sha256'])
        self.last_evidence['environment'] = environment
        self.last_evidence['source_rights'] = config['source_rights_sha256']
        self.last_evidence['source_rights_observation_base64'] = base64.b64encode(rights).decode()
        parsed = None if binding.strip() == b'null' else parse_json(binding)
        parsed = self._pressure_binding(parsed)
        if ('cfd_wrapper_console_evidence' in config and
                (not isinstance(parsed, dict) or parsed.get('schema') != 'cfd-process-binding-2')):
            raise ValueError('Wrapper console supplement requires v2 binding')
        self.last_evidence.pop('cfd_owner_evidence', None)
        if parsed is not None:
            from digitalmodel.ansys.cylinder_cfd_owner_evidence import resolve_owner_evidence

            if not isinstance(parsed, dict):
                raise ValueError('CFD binding requires an object or null')
            if parsed.get('schema') == 'cfd-process-binding-2':
                self.last_evidence['cfd_owner_evidence'] = resolve_owner_evidence(config, parsed, _read_owner)
        self._console_agreement(parsed)
        return parsed

    def _pressure_binding(self, parsed):
        from .cylinder_pressure_scope import COARSE_SCOPE, INTERMEDIATE_SCOPE
        from .cylinder_resource_absence import validate_absence_descriptor

        self._absence_mode = False
        if self.approval.get('scope') not in (COARSE_SCOPE, INTERMEDIATE_SCOPE):
            return parsed
        if validate_absence_descriptor(parsed, _host()):
            if any(key in self.config for key in ('cfd_owner_evidence', 'cfd_wrapper_console_evidence')):
                raise ValueError('absence mode cannot request owner or wrapper exemptions')
            self._absence_mode = True
            self.last_evidence['process_absence'] = dict(
                descriptor_sha256=self.config['cfd_binding_sha256'],
                scope='ansys-mpi-lineage-v1 plus interFoam.exe names',
                meaning='point-in-time observed absence; no family exemption or ownership')
            return None
        if not isinstance(parsed, dict) or parsed.get('schema') != 'cfd-process-binding-2':
            raise ValueError('pressure admission requires v2 CFD binding or explicit absence descriptor')
        return parsed

    def _observe_processes(self, binding, stage):
        try:
            snapshot = _absence_snapshot() if self._absence_mode else collect_process_snapshot(binding)
        except Exception as error:
            self.last_evidence['process_collection_error'] = dict(stage=stage,
                error=f'{type(error).__name__}: {error}',
                evidence=deepcopy(getattr(error, 'evidence', {})))
            raise
        self.last_evidence['process_snapshot_stage'] = stage if self._absence_mode else 'initial-only'
        self.last_evidence['process_snapshot'] = snapshot
        if self._absence_mode:
            from .cylinder_resource_absence import verify_absence_snapshot
            checks = self.last_evidence.setdefault('process_absence_checks', [])
            record = dict(stage=stage, observed_at=snapshot['observed_at'],
                snapshot_sha256=digest_bytes(canonical_bytes(snapshot)),
                snapshot=deepcopy(snapshot), status='NOT_EVALUATED')
            checks.append(record)
            try:
                classification = verify_absence_snapshot(snapshot, _host(), _now())
                record['status'] = classification['status']
            except Exception as error:
                record.update(status='REFUSED', error=f'{type(error).__name__}: {error}')
                raise
        else:
            classification = classify_process_inventory(snapshot, expected_host=_host(),
                cfd_binding=binding, now=_now(), maximum_age_seconds='30')
        self.last_evidence.update(classification=classification,
            raw_inventory=snapshot['rows'], process_inventory=classification['process_inventory'],
            process_inventory_scope='blocking-projection-only')
        return classification

    def refresh_absence(self, stage):
        if self._absence_mode:
            self._observe_processes(None, stage)

    def _console_agreement(self, binding):
        from digitalmodel.ansys.cylinder_wrapper_consoles import verify_agreement

        verify_agreement(self.config, binding,
            self.last_evidence.get('cfd_owner_evidence', {}),
            self.last_evidence.get('classification'))

    def _reservation(self):
        lock = self.reservation.evidence()
        if (lock['pid'] != os.getpid() or lock['path'] != str(_owned_path(self.config['lock_path']).resolve())
                or lock['content_sha256'] != digest_bytes(str(os.getpid()).encode('ascii'))):
            raise ValueError('actual reservation owner binding differs')
        if self._lock is not None and lock != self._lock:
            raise ValueError('reservation identity changed')
        self.last_evidence['lock_evidence'] = lock
        self.last_evidence['exclusive_seat_owner'] = self.approval['operator_id']
        self.last_evidence['reservation_scope'] = 'participating-local-queue-only'
        self._lock = deepcopy(lock)

    def _licence(self):
        observation = _license_query(self.config)
        self.last_evidence['license_observation'] = _encoded_query(observation)
        if observation['returncode'] != 0:
            raise ValueError('licence query failed; raw observation retained')
        if observation['stderr'] != b'':
            raise ValueError('licence query stderr is nonempty; raw observation retained')
        _fresh(observation['observed_at'], _now())
        if self.approval['runtime_profile']['update'] != '20260202':
            raise ValueError('minimum licence version requires reviewed runtime update')
        result = parse_compatible_license(observation['stdout'], feature='ansys',
                                          minimum_version='2026.0202')
        self.last_evidence['license_availability'] = result
        self.last_evidence['license_query'] = digest_bytes(observation['stdout'])

    def __call__(self, approval):
        self._ready_at = None
        self.last_evidence = {'observed_at': _now(), 'native_launch': False}
        if canonical_bytes(approval) != canonical_bytes(self.approval):
            raise ValueError('preflight approval differs')
        binding = self._bindings()
        self._reservation()
        capacity = _capacity()
        self.last_evidence['capacity_observation'] = capacity
        self.last_evidence['capacity'] = validate_capacity(capacity, now=_now())
        classification = self._observe_processes(binding, 'initial')
        snapshot = self.last_evidence['process_snapshot']
        self._console_agreement(binding)
        self._licence()
        validate_capacity(capacity, now=_now())
        _fresh(snapshot['observed_at'], _now())
        self.last_evidence.update(execution_host=_host(), profile=deepcopy(PROFILE),
            executable_sha256=self.approval['executable_sha256'],
            free_bytes=_free_bytes(self.config['output_directory']))
        if classification['status'] == 'CLEAR':
            self._ready_at = _now()
        return deepcopy(self.last_evidence)

    def before_launch(self):
        """Recheck after durable claim; any failure consumes that claim externally."""
        if self._ready_at is None:
            raise ValueError('successful clear preflight required before launch')
        _fresh(self._ready_at, _now())
        self._bindings()
        self._reservation()
        validate_capacity(self.last_evidence['capacity_observation'], now=_now())
        _fresh(self.last_evidence['process_snapshot']['observed_at'], _now())
        self._licence()
        self.refresh_absence('before_launch')
        _fresh(self._ready_at, _now())
        self._reservation()
        validate_capacity(self.last_evidence['capacity_observation'], now=_now())
        _fresh(self.last_evidence['process_snapshot']['observed_at'], _now())
        return deepcopy(self.last_evidence)
