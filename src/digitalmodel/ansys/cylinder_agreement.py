"""Compare independently supplied expressions; provenance remains a separate gate."""
from decimal import (Context, Decimal, localcontext, ROUND_HALF_EVEN,
                     InvalidOperation, DivisionByZero, Overflow)

from digitalmodel.ansys.analysis_records import decimal_text, digest_bytes
from digitalmodel.ansys.cylinder_expression import evaluate_checker
from digitalmodel.ansys.cylinder_reference import reference, symbols_for

NUMERIC_CONTEXT = Context(prec=50, rounding=ROUND_HALF_EVEN, Emin=-4096, Emax=4096,
                          capitals=1, clamp=0, flags=[],
                          traps=[InvalidOperation, DivisionByZero, Overflow])


def compare_reference(raw_checker):
    """Evaluate 126 pressure/control values with fixed dimensional tolerances.

    Call only after producer and checker derivations have both been committed.
    This arithmetic result cannot authenticate a checker session or human authority.
    """
    rows = []
    with localcontext(NUMERIC_CONTEXT):
        for pressure in ('0', '10'):
            for radius in ('750', '780', '810'):
                for axial in ('60', '120', '180'):
                    inputs = symbols_for(pressure, radius, axial)
                    observed = evaluate_checker(raw_checker, inputs)
                    expected = reference(pressure, radius, axial)
                    for name, value in expected.items():
                        tolerance = Decimal('1e-12' if name.startswith('u_') else '1e-10')
                        difference = abs(observed[name] - value)
                        rows.append({'pressure_mpa': pressure, 'r_mm': radius,
                                     'y_mm': axial, 'quantity': name,
                                     'producer': decimal_text(str(value)),
                                     'checker': decimal_text(str(observed[name])),
                                     'absolute_difference': decimal_text(str(difference)),
                                     'tolerance': decimal_text(str(tolerance)),
                                     'unit': 'mm' if name.startswith('u_') else 'MPa',
                                     'passes': difference <= tolerance})
    return {'schema': 'cylinder-reference-comparison-1',
            'checker_output_sha256': digest_bytes(raw_checker),
            'status': 'AGREEMENT' if all(r['passes'] for r in rows) else 'DISAGREEMENT',
            'comparisons': rows, 'engineering_qualification': False,
            'limitation': 'Arithmetic agreement only; session provenance, symbolic branch '
                          'review and competent independent B2 adjudication remain separate.'}
