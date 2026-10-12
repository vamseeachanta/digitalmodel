"""Synthetic checker expressions test disagreement; these are not independent evidence."""
import json

import pytest

from digitalmodel.ansys.cylinder_agreement import compare_reference


EXPRESSIONS = {
    'sigma_r': 'p*a**2/(b**2-a**2)*(1-b**2/r**2)',
    'sigma_theta': 'p*a**2/(b**2-a**2)*(1+b**2/r**2)',
    'sigma_z': '0', 'tau_rz': '0',
    'sigma_vm': 'p*a**2/(b**2-a**2)*sqrt(1+3*b**4/r**4)',
    'u_r': 'p*a**2/(b**2-a**2)/E*((1-nu)*r+(1+nu)*b**2/r)',
    'u_z': '-2*nu*p*a**2/(b**2-a**2)*y/E',
}


def synthetic(expressions):
    return json.dumps({'expressions': expressions,
                       'derivation': 'Synthetic test fixture only.'}).encode()


def test_equivalent_expressions_compare_all_126_values():
    record = compare_reference(synthetic(EXPRESSIONS))
    assert record['status'] == 'AGREEMENT'
    assert len(record['comparisons']) == 126
    assert record['engineering_qualification'] is False


@pytest.mark.parametrize('expression', ['1e-8', '-1e-8', '0.01*u_r', 'p*a**2/(b**2-a**2)'])
def test_disagreement_or_unsupported_symbols_cannot_establish_reference(expression):
    modified = {**EXPRESSIONS, 'sigma_z': expression}
    if 'u_r' in expression:
        with pytest.raises(ValueError):
            compare_reference(synthetic(modified))
    else:
        result = compare_reference(synthetic(modified))
        assert result['status'] == 'DISAGREEMENT'


def test_displacement_tolerance_is_not_stress_tolerance():
    changed = {**EXPRESSIONS, 'u_r': '(' + EXPRESSIONS['u_r'] + ')+2e-12'}
    assert compare_reference(synthetic(changed))['status'] == 'DISAGREEMENT'


def test_comparison_refuses_fenced_output_without_a_result_record():
    with pytest.raises(ValueError):
        compare_reference(b'```json\n' + synthetic(EXPRESSIONS) + b'\n```')


def test_process_default_decimal_mutation_does_not_change_comparison(monkeypatch):
    import decimal
    expected = compare_reference(synthetic(EXPRESSIONS))
    monkeypatch.setattr(decimal.DefaultContext, 'rounding', decimal.ROUND_DOWN)
    monkeypatch.setitem(decimal.DefaultContext.traps, decimal.Inexact, True)
    assert compare_reference(synthetic(EXPRESSIONS)) == expected
