"""Externally pinned coarse/intermediate pressure driver; no retry or numerical qualification."""
import argparse
from copy import deepcopy
import os
from pathlib import Path
import shutil
import sys
import time

from digitalmodel.ansys.analysis_records import canonical_bytes, digest_bytes, parse_json
from digitalmodel.ansys.analysis_replay import derive_replay_case
from digitalmodel.ansys.cylinder_diagnostic_admission import _path, _read, _sha
from digitalmodel.ansys.cylinder_diagnostic_resources import validate_environment
from digitalmodel.ansys.cylinder_pressure_admission import make_pressure_admission
from digitalmodel.ansys.cylinder_preparation_history import validate_preparation_history
from digitalmodel.ansys.cylinder_preparation_streams import verify_streams


def _arguments(argv):
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ('config', 'review_receipt', 'review_stdout', 'review_bundle'):
        parser.add_argument(name)
    parser.add_argument('--config-sha256', required=True)
    parser.add_argument('--review-sha256', required=True)
    return parser.parse_args(argv)


def bound_inputs(arguments, source_root):
    """External pins and actual admission gate precede replay or reservation."""
    raw = _read(arguments.config, _sha(arguments.config_sha256))
    config = parse_json(raw)
    admission = make_pressure_admission(arguments.config, arguments.review_receipt,
        arguments.review_stdout, arguments.review_bundle,
        expected_config_sha256=arguments.config_sha256,
        expected_review_sha256=arguments.review_sha256, source_root=source_root)
    validate_environment(admission['approval'], os.environ)
    return config, admission


def _resolver(config):
    record = config['replay']
    if set(record) != {'reference', 'resolver', 'review_sha256'}:
        raise ValueError('Exact replay reference, resolver pin and review pin required')
    pin = record['resolver']
    if not isinstance(pin, dict) or set(pin) != {'path', 'sha256'}:
        raise ValueError('Pinned resolver descriptor required')
    values = parse_json(_read(pin['path'], _sha(pin['sha256'])))
    if not isinstance(values, dict) or not values:
        raise ValueError('Nonempty evidence ID to path resolver required')
    resolver = {}
    for identity, path in values.items():
        if not isinstance(identity, str) or not identity.strip() or not isinstance(path, str):
            raise ValueError('Invalid resolver identity or path')
        resolver[identity] = _path(path)
    return resolver


def replay_callback(config):
    """Bind actual derivation, never a caller assertion that zero checks passed."""
    bound = deepcopy(config)
    def replay_prefix(approval):
        pin = bound['lineage']['baseline']
        baseline = parse_json(_read(pin['path'], _sha(pin['file_sha256'])))
        resolver = _resolver(bound)
        return derive_replay_case(baseline, bound['replay']['reference'], resolver,
                                  review_sha256=_sha(bound['replay']['review_sha256']))
    return replay_prefix


def fast_binding_check(pins, approval):
    """Rehash prebound bytes only; no Git subprocess or retrospective authority."""
    expected_approval = canonical_bytes(approval)
    expected = {}
    for pin in pins:
        path, sha = _path(pin['path']), _sha(pin['sha256'])
        if path in expected and expected[path] != sha:
            raise ValueError('Conflicting byte pins')
        _read(path, sha)
        expected[path] = sha
    if not expected:
        raise ValueError('Fast bindings cannot be empty')
    def verify_current(actual):
        if canonical_bytes(actual) != expected_approval:
            raise ValueError('Current approval differs')
        for path, sha in expected.items():
            _read(path, sha)
        return True
    return verify_current


def _pins(config, arguments, root):
    pins = [{'path': str(root / row['path']), 'sha256': row['sha256']}
            for row in config['source_files']]
    pins += [{'path': arguments.config, 'sha256': arguments.config_sha256},
             {'path': arguments.review_receipt, 'sha256': arguments.review_sha256},
             dict(config['replay']['resolver'])]
    # Admission authenticates review transport; retain those exact bytes for phase2.
    for path in (arguments.review_stdout, arguments.review_bundle):
        pins.append({'path': str(path), 'sha256': digest_bytes(_read(path))})
    for path in _resolver(config).values():
        pins.append({'path': str(path), 'sha256': digest_bytes(_read(path))})
    from digitalmodel.ansys.cylinder_pressure_scope import pressure_step
    if pressure_step(config)[0] == 3:
        from digitalmodel.ansys.cylinder_intermediate_lineage import coarse_read_pins
        pins.extend(coarse_read_pins(config))
    baseline = config['lineage']['baseline']
    pins.append({'path': baseline['path'], 'sha256': baseline['file_sha256']})
    return pins


def ledger_paths(parent_claim, ordinal=2):
    """Count coarse (N4) records and, for intermediate (N8), its successor records."""
    if type(ordinal) is not int or ordinal not in (2, 3):
        raise ValueError('only integer pressure ordinals 2 and 3 supported')
    parent = _path(parent_claim)
    result = []
    suffixes = [f'.ordinal-{number}{suffix}' for number in range(2, ordinal + 1)
                for suffix in ('.invocation.json', '.json', '.terminal.json')]
    for suffix in suffixes:
        path = parent.with_name(parent.stem + suffix)
        if path.is_symlink() or path.is_junction():
            raise ValueError('Redirected ordinal ledger path')
        if path.exists():
            result.append(_path(path))
    return result


def history_binding(config, check):
    """Recheck retained membership after source/config pins, including postclaim."""
    frozen = deepcopy(config)
    def verify(actual):
        if check(actual) is not True:
            raise ValueError('Current source/config binding did not pass')
        verify_streams(frozen)
        validate_preparation_history(frozen, require_unclaimed=False)
        return True
    return verify


def history_storage(config):
    """Bind prior captures without rejecting the journal's own current claim."""
    from digitalmodel.ansys.cylinder_pressure_scope import pressure_step
    if pressure_step(config)[0] != 3:
        return {}
    history = validate_preparation_history(config, require_unclaimed=False)
    return dict(prior_capture_roots=(config['predecessor']['original_root'],
                                    *history['prior_capture_roots']),
                supplemental_files=tuple(history['supplemental_files']))


def production_callbacks(config, admission, arguments, root, reservation):
    from digitalmodel.ansys.cylinder_diagnostic_preflight import ProductionPreflight
    from digitalmodel.ansys.cylinder_pressure_capture import capture_pressure
    from digitalmodel.ansys.cylinder_pressure_resources import production_phase2, cumulative_storage
    from digitalmodel.ansys.cylinder_runner import launch_case

    from digitalmodel.ansys.cylinder_pressure_scope import pressure_step
    config = deepcopy(config)
    ordinal, case_id = pressure_step(config)
    verify_streams(config)
    validate_preparation_history(config)
    operation, approval = config['operational'], admission['approval']
    preflight = ProductionPreflight(operation, approval, reservation)
    check = fast_binding_check(_pins(config, arguments, root), approval)
    if ordinal == 3:
        check = history_binding(config, check)
    def phase2(actual):
        validate_environment(actual, os.environ)
        return production_phase2(preflight, check, actual)
    def preclaim(actual):
        validate_environment(actual, os.environ)
        return production_phase2(preflight, check, actual, stage='preclaim-probe')
    def launch(case, directory, timeout):
        bound_case = dict(case, deck_basename=Path(case['deck']).name)
        return launch_case(bound_case, directory, timeout, operation['executable'])
    def capture(case, directory, execution):
        kwargs = {'capture_case_id': case['case_id']} if ordinal == 3 else {}
        return capture_pressure(case, directory, execution,
            runtime_profile=approval['runtime_profile'], **kwargs)
    def account_storage():
        verify_streams(config)
        lineage = config['lineage']
        parent = lineage['parent_claim']['path']
        free = shutil.disk_usage(operation['output_directory']).free
        kwargs = history_storage(config)
        return cumulative_storage(lineage['original_root'], parent, operation['output_directory'],
            ledger_paths(parent, ordinal=ordinal), operation['lock_path'], free, **kwargs)
    callbacks = dict(replay_prefix=replay_callback(config), preflight=preflight,
                phase2_recheck=phase2, launch=launch, capture=capture,
                clock=time, reservation=reservation, account_storage=account_storage)
    if ordinal == 3:
        callbacks['preclaim_recheck'] = preclaim
    return callbacks


def pressure_exit_code(result):
    planned = (result.get('terminal_reason') == 'PLANNED_SCOPE_STOP'
               and result.get('reservation_released') is True
               and result.get('no_owned_processes_established') is True
               and result.get('engineering_qualified') is False)
    return 0 if planned else 2


def main(argv=None):
    from digitalmodel.ansys.cylinder_operational_reservation import acquire_local_reservation
    from digitalmodel.ansys.cylinder_pressure_resume import execute_pressure_resume

    script = Path(__file__).resolve()
    if os.name != 'nt' or Path(sys.argv[0]).resolve() != script:
        raise ValueError('Reviewed Windows pressure entrypoint required')
    arguments = _arguments(argv)
    root = script.parents[2]
    config, admission = bound_inputs(arguments, root)
    verify_streams(config)
    reservation = acquire_local_reservation(config['operational']['lock_path'])
    try:
        callbacks = production_callbacks(config, admission, arguments, root, reservation)
    except BaseException:
        reservation.release(no_owned_processes=True)  # No native adapter entered.
        raise
    result = execute_pressure_resume(config, admission, **callbacks)
    verify_streams(config)
    print(canonical_bytes(result).decode('utf-8'))
    return pressure_exit_code(result)  # Never grants numerical qualification.


if __name__ == '__main__':
    raise SystemExit(main())
