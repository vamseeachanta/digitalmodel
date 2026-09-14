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


def _free_bytes(path):
    return shutil.disk_usage(_owned_path(path).parent).free


def _decimal_nanoseconds(value):
    if type(value) is not int or value < 0:
        raise ValueError('nonnegative integer nanoseconds required')
    seconds, nanoseconds = divmod(value, 1_000_000_000)
    fraction = f'{nanoseconds:09d}'.rstrip('0')
    return str(seconds) + ('.' + fraction if fraction else '')


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
        binding = _read(config['cfd_binding_path'], config['cfd_binding_sha256'])
        self.last_evidence['environment'] = environment
        self.last_evidence['source_rights'] = config['source_rights_sha256']
        self.last_evidence['source_rights_observation_base64'] = base64.b64encode(rights).decode()
        return None if binding.strip() == b'null' else parse_json(binding)

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
        snapshot = collect_process_snapshot(binding)
        self.last_evidence['process_snapshot'] = snapshot
        classification = classify_process_inventory(snapshot, expected_host=_host(),
            cfd_binding=binding, now=_now(), maximum_age_seconds='30')
        self.last_evidence.update(classification=classification,
            raw_inventory=snapshot['rows'], process_inventory=classification['process_inventory'],
            process_inventory_scope='blocking-projection-only')
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
        _fresh(self._ready_at, _now())
        self._reservation()
        validate_capacity(self.last_evidence['capacity_observation'], now=_now())
        _fresh(self.last_evidence['process_snapshot']['observed_at'], _now())
        return deepcopy(self.last_evidence)
