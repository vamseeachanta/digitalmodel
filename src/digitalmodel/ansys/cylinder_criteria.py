"""Frozen four-attempt numerical limits; PASS never grants engineering authority."""
from decimal import Decimal, localcontext

from digitalmodel.ansys.cylinder_results import (
    EvidenceError, QUANTITIES, decimal_value, dimensional_floor, decimal_context,
    fixed_decimal_context,
)

D = Decimal
CASE_IDS = ('ocv-zero-t60-n16', 'ocv-t60-p10-n4',
            'ocv-t60-p10-n8', 'ocv-t60-p10-n16')
STATIONS = tuple(f'{r}_y{y}' for y in (60, 120, 180)
                 for r in ('inner', 'middle', 'outer'))
EXPECTED_KEYS = {(s, q) for s in STATIONS for q in QUANTITIES}
PI = D('3.1415926535897932384626433832795028841971693993751')


def _checked(values):
    if not isinstance(values, dict) or set(values) != EXPECTED_KEYS:
        raise EvidenceError('Expected exactly 63 named station quantities')
    return {key: decimal_value(value) for key, value in values.items()}


def _zero(station, quantity):
    return quantity in ('sigma_z', 'tau_rz') or (
        quantity == 'sigma_r' and station.startswith('outer_'))


def _check(checks, rule, key, residual, limit):
    response = list(key) if isinstance(key, tuple) else key
    checks.append({'criterion': rule, 'response': response, 'residual': str(abs(residual)),
                   'limit': str(limit), 'passed': abs(residual) <= limit})


def _primary(values, reference, checks):
    for key in sorted(EXPECTED_KEYS):
        station, quantity = key
        if not station.endswith('_y120'):
            continue
        if _zero(*key):
            _check(checks, 'expected_zero', key, values[key], D('.010'))
        else:
            limit = max(D('.01')*abs(reference[key]), dimensional_floor(quantity))
            _check(checks, 'accuracy', key, values[key]-reference[key], limit)


def _auxiliary(values, reference, checks):
    for key in sorted(EXPECTED_KEYS):
        station, quantity = key
        radial, y = station.split('_y')
        if y == '120':
            continue
        middle = (f'{radial}_y120', quantity)
        if not quantity.startswith('u_'):
            limit, target = D('.010'), values[middle]
        elif quantity == 'u_r':
            limit = max(D('.01')*abs(reference[middle]), D('1e-9'))
            target = values[middle]
        else:
            limit = max(D('.01')*abs(reference[key]), D('1e-9'))
            target = D(y)/120*values[middle]
        _check(checks, 'auxiliary', key, values[key]-target, limit)


def _outcome(status, checks=(), errors=()):
    return {'status': status, 'checks': list(checks), 'evidence_errors': list(errors),
            'engineering_qualified': False,
            'independent_adjudication': 'required outside numerical evaluator'}


@decimal_context
def evaluate_attempt(case_id, values, reference, rfy_sum, evidence_errors):
    """Check one attempt; coarse numerical differences do not stop execution."""
    if case_id not in CASE_IDS or not isinstance(evidence_errors, list):
        return _outcome('INCOMPLETE', errors=['Unknown case or missing evidence status'])
    if evidence_errors:
        return _outcome('INCOMPLETE', errors=evidence_errors)
    try:
        values, reference, force = _checked(values), _checked(reference), decimal_value(rfy_sum)
    except (EvidenceError, TypeError) as exc:
        return _outcome('INCOMPLETE', errors=[str(exc)])
    checks = []
    with localcontext(fixed_decimal_context()):
        if case_id == CASE_IDS[0]:
            for key, value in sorted(values.items()):
                _check(checks, 'zero_control', key, value, dimensional_floor(key[1]))
            _check(checks, 'axial_equilibrium', 'RFY', force, D('1e-6'))
        else:
            _primary(values, reference, checks)
            _check(checks, 'axial_equilibrium', 'RFY', force, D('1e-6')*10*PI*750**2)
            if case_id == CASE_IDS[-1]:
                _auxiliary(values, reference, checks)
    if case_id in CASE_IDS[1:3]:
        return _outcome('CONTINUE', checks)
    return _outcome('FAIL' if any(not c['passed'] for c in checks) else 'PASS', checks)


def _refinement(attempts, reference):
    checks = []
    with localcontext(fixed_decimal_context()):
        values = [_checked(row['values']) for row in attempts[1:]]
        for key in sorted(EXPECTED_KEYS):
            if not key[0].endswith('_y120'):
                continue
            floor = D('.010') if _zero(*key) else dimensional_floor(key[1])
            limit = max(D('.005')*abs(reference[key]), floor)
            d16 = values[2][key]-values[1][key]
            d8 = values[1][key]-values[0][key]
            _check(checks, 'refinement_magnitude', key, d16, limit)
            _check(checks, 'refinement_decreasing', key, d16, abs(d8)+floor)
    return checks


@decimal_context
def evaluate_canary(attempts, reference):
    """Evaluate an ordered prefix, reporting unattempted cases and stop violations."""
    if not isinstance(attempts, list) or len(attempts) > 4:
        return _outcome('INCOMPLETE', errors=['Invalid attempt collection or fifth attempt'])
    checks, errors = [], []
    try:
        reference = _checked(reference)
        for index, attempt in enumerate(attempts):
            if attempt['case_id'] != CASE_IDS[index]:
                raise EvidenceError('Wrong, duplicate or out-of-order attempt')
            result = evaluate_attempt(**attempt, reference=reference)
            checks.extend(result['checks'])
            if result['status'] in ('FAIL', 'INCOMPLETE'):
                if index != len(attempts)-1:
                    raise EvidenceError('Attempts continued after a blocking result')
                return dict(result, checks=checks, attempted=list(CASE_IDS[:len(attempts)]),
                            unattempted=list(CASE_IDS[len(attempts):]))
        if len(attempts) == 4:
            checks.extend(_refinement(attempts, reference))
    except (EvidenceError, KeyError, TypeError) as exc:
        errors.append(str(exc))
    status = ('INCOMPLETE' if errors or len(attempts) != 4 else
              'FAIL' if any(not c['passed'] for c in checks
                            if c['criterion'].startswith('refinement')) else 'PASS')
    return dict(_outcome(status, checks, errors), attempted=[a.get('case_id') for a in attempts],
                unattempted=list(CASE_IDS[len(attempts):]))
