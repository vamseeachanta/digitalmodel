"""Pinned single-case operator entry; no discovery, retries or qualified reuse."""
import argparse
import os
from pathlib import Path
import sys

from digitalmodel.ansys.analysis_records import canonical_bytes, digest_bytes, parse_json
from digitalmodel.ansys.cylinder_adapter import make_execution_adapters
from digitalmodel.ansys.cylinder_canary import run_canary
from digitalmodel.ansys.cylinder_diagnostic_admission import make_diagnostic_admission
from digitalmodel.ansys.cylinder_diagnostic_resources import validate_environment
from digitalmodel.ansys.cylinder_operational_reservation import acquire_local_reservation


def _settled(result):
    return (type(result.get('owned_processes_remaining')) is int
            and result['owned_processes_remaining'] == 0
            and result.get('streams_finalized') is True
            and not result.get('retained_supervisor_token'))


def execute_diagnostic(*, bundle, output, approval, admission, preflight, execution, reservation):
    """Join reviewed callbacks; late refusal consumes claim but starts no process."""
    no_owned_processes = True
    release_error = None

    def observe(bound):
        validate_environment(bound, os.environ)
        lock = reservation.evidence()
        evidence = preflight(bound)
        return {**evidence, 'operational_reservation': lock,
                'operational_owner': bound['operator_id'],
                'launch_environment': validate_environment(bound, os.environ)}

    def launch(*arguments):
        nonlocal no_owned_processes
        validate_environment(approval, os.environ)
        reservation.evidence()
        if hasattr(preflight, 'before_launch'):
            preflight.before_launch()
        no_owned_processes = False  # Exceptions inside native adapter are uncertain.
        result = execution['launch'](*arguments)
        no_owned_processes = _settled(result)
        return result

    try:
        outcome = run_canary(bundle, output, approval,
            verify_authority=admission['verify_authority'], preflight=observe,
            launch=launch, extract=execution['extract'], assess=execution['assess'],
            adjudicate=admission['adjudicate'], verify_reference=execution['verify_reference'])
    finally:
        try:
            released = reservation.release(no_owned_processes=no_owned_processes)
        except Exception as error:
            released, release_error = False, f'{type(error).__name__}: {error}'
    return {'outcome': outcome, 'reservation_released': released,
            'reservation_release_error': release_error,
            'no_owned_processes_established': no_owned_processes,
            'preflight_observation': getattr(preflight, 'last_evidence', None),
            'execution_approval_sha256': digest_bytes(canonical_bytes(approval)),
            'qualification': 'diagnostic_only'}


def _arguments(argv):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('config')
    parser.add_argument('review_receipt')
    parser.add_argument('review_stdout')
    parser.add_argument('review_bundle')
    parser.add_argument('--config-sha256', required=True)
    parser.add_argument('--review-sha256', required=True)
    return parser.parse_args(argv)


def _bound_inputs(arguments):
    script = Path(__file__).resolve()
    root = script.parents[2]
    if Path(sys.argv[0]).resolve() != script:
        raise ValueError('running entrypoint differs from reviewed source')
    if os.name != 'nt':
        raise ValueError('this native diagnostic requires the established Windows host')
    config_path = Path(arguments.config)
    raw = config_path.read_bytes()
    if digest_bytes(raw) != arguments.config_sha256:
        raise ValueError('externally pinned config differs')
    config = parse_json(raw)
    admission = make_diagnostic_admission(config_path, arguments.review_receipt,
        arguments.review_stdout, review_bundle_path=arguments.review_bundle,
        expected_config_sha256=arguments.config_sha256,
        expected_review_sha256=arguments.review_sha256, source_root=root)
    approval = dict(config['execution_binding'], approval_id=config['campaign_id'],
        operator_id=config['operator_id'], checker_id=admission['checker_id'],
        ledger_directory=config['ledger_directory'], config_sha256=arguments.config_sha256,
        review_receipt_sha256=arguments.review_sha256)
    admission['verify_authority'](approval)
    validate_environment(approval, os.environ)
    return config, approval, admission


def diagnostic_exit_code(result):
    outcome = result['outcome']
    valid_capture = (outcome.get('status') == 'INCOMPLETE'
        and outcome.get('attempted') == ['ocv-zero-t60-n16']
        and len(outcome.get('records', [])) == 1
        and outcome.get('reason') == 'diagnostic scope exhausted: durable claim consumed'
        and result.get('reservation_released') is True
        and result.get('no_owned_processes_established') is True)
    return 0 if valid_capture else 2


def main(argv=None):
    from digitalmodel.ansys.cylinder_diagnostic_preflight import ProductionPreflight

    config, approval, admission = _bound_inputs(_arguments(argv))
    operation = config['operational']
    execution = make_execution_adapters(operation['bundle'], operation['executable'], approval)
    reservation = acquire_local_reservation(operation['lock_path'])
    try:
        preflight = ProductionPreflight(operation, approval, reservation)
    except BaseException:
        reservation.release(no_owned_processes=True)  # Native adapter has not been entered.
        raise
    result = execute_diagnostic(bundle=operation['bundle'], output=operation['output_directory'],
        approval=approval, admission=admission, preflight=preflight,
        execution=execution, reservation=reservation)
    target = Path(operation['output_directory']) / 'operator-outcome.json'
    raw = canonical_bytes(result)
    with target.open('xb') as stream:
        stream.write(raw)
        stream.flush()
        os.fsync(stream.fileno())
    if target.read_bytes() != raw:
        raise OSError('operator outcome readback mismatch')
    print(canonical_bytes({'outcome_path': str(target),
        'attempted': result['outcome']['attempted'],
        'status': result['outcome']['status'],
        'reservation_released': result['reservation_released'],
        'qualified_rows_added': 0}).decode())
    return diagnostic_exit_code(result)


if __name__ == '__main__':
    raise SystemExit(main())
