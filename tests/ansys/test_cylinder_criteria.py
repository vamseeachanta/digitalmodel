"""Synthetic numerical responses exercise the frozen canary criteria."""
from decimal import Decimal, localcontext

import pytest

from digitalmodel.ansys.cylinder_criteria import evaluate_attempt, evaluate_canary


def reference(pressure=10):
    values = {}
    with localcontext() as ctx:
        ctx.prec = 50
        p = Decimal(pressure); a = Decimal(750); b = Decimal(810)
        A = p*a*a/(b*b-a*a); B = A*b*b
        for y in (60, 120, 180):
            for r, radius in [('inner', 750), ('middle', 780), ('outer', 810)]:
                x = Decimal(radius); sr = A-B/x**2; st = A+B/x**2
                for q, v in dict(sigma_r=Decimal(0) if r=='outer' else sr,
                    sigma_theta=st, sigma_z=Decimal(0), tau_rz=Decimal(0),
                    sigma_vm=(sr*sr+st*st-sr*st).sqrt(),
                    u_r=(Decimal('.7')*A*x+Decimal('1.3')*B/x)/200000,
                    u_z=-Decimal('.6')*A*y/200000).items(): values[f'{r}_y{y}',q]=v
    return values


def attempts():
    ids = ['ocv-zero-t60-n16','ocv-t60-p10-n4','ocv-t60-p10-n8','ocv-t60-p10-n16']
    return [dict(case_id=c, values=reference(0 if n==0 else 10),
                 rfy_sum=Decimal(0), evidence_errors=[]) for n,c in enumerate(ids)]


def test_complete_reference_case_passes_numerics_only():
    result = evaluate_canary(attempts(), reference())
    assert result['status'] == 'PASS'
    assert result['engineering_qualified'] is False


@pytest.mark.parametrize('sign', [-1, 1])
@pytest.mark.parametrize('quantity', ['sigma_r','sigma_theta','u_r','u_z'])
def test_five_percent_gain_on_nonzero_response_fails(sign, quantity):
    rows=attempts(); key=('inner_y120',quantity)
    rows[-1]['values'][key] *= 1 + sign*Decimal('.05')
    assert evaluate_canary(rows,reference())['status']=='FAIL'


@pytest.mark.parametrize('sign', [-1, 1])
def test_zero_control_and_definition_zero_fail_both_signs(sign):
    rows=attempts(); rows[0]['values']['inner_y60','sigma_z']=sign*Decimal('.1')
    assert evaluate_canary(rows[:1],reference())['status']=='FAIL'
    rows=attempts(); rows[-1]['values']['outer_y120','sigma_r']=sign*Decimal('.1')
    assert evaluate_canary(rows,reference())['status']=='FAIL'


def test_coarse_discrepancy_is_diagnostic_and_partial_never_passes():
    row=attempts()[1];row['values']['inner_y120','sigma_r']=Decimal(900)
    assert evaluate_attempt(**row, reference=reference())['status']=='CONTINUE'
    assert evaluate_canary(attempts()[:2],reference())['status']=='INCOMPLETE'


def test_invalid_evidence_precedes_numeric_fail():
    rows=attempts();rows[0]['values']['inner_y60','sigma_z']=Decimal(99)
    rows[0]['evidence_errors']=['missing native log']
    assert evaluate_canary(rows[:1],reference())['status']=='INCOMPLETE'


@pytest.mark.parametrize('fault', ['auxiliary','refinement','force','missing','order'])
def test_mandatory_collection_guards(fault):
    rows=attempts()
    if fault=='auxiliary': rows[-1]['values']['inner_y60','sigma_theta']+=Decimal('.1')
    if fault=='refinement': rows[2]['values']['inner_y120','sigma_theta']+=Decimal(10)
    if fault=='force': rows[-1]['rfy_sum']=Decimal(20)
    if fault=='missing': rows[-1]['values'].pop(('inner_y120','u_z'))
    if fault=='order': rows[1],rows[2]=rows[2],rows[1]
    assert evaluate_canary(rows,reference())['status']==('INCOMPLETE' if fault in ['missing','order'] else 'FAIL')


@pytest.mark.parametrize('sign',[-1,1])
def test_fixed_zero_limit_is_inclusive_then_refuses(sign):
    row=attempts()[-1];key=('inner_y120','sigma_z')
    row['values'][key]=sign*Decimal('.010')
    assert evaluate_attempt(**row,reference=reference())['status']=='PASS'
    row['values'][key]=sign*Decimal('.0100000000001')
    assert evaluate_attempt(**row,reference=reference())['status']=='FAIL'


@pytest.mark.parametrize('fault',['reversed_pressure','units','component_swap','float','nonfinite'])
def test_response_mutations_cannot_pass(fault):
    row=attempts()[-1];key=('inner_y120','sigma_r')
    if fault=='reversed_pressure':row['values'][key]*=-1
    if fault=='units':row['values'][key]*=Decimal(1000000)
    if fault=='component_swap':row['values'][key]=row['values']['inner_y120','sigma_theta']
    if fault=='float':row['values'][key]=-10.0
    if fault=='nonfinite':row['values'][key]=Decimal('NaN')
    assert evaluate_attempt(**row,reference=reference())['status']==('INCOMPLETE' if fault in ['float','nonfinite'] else 'FAIL')


def test_primary_check_order_is_canonical():
    row=attempts()[-1]
    checks=evaluate_attempt(**row,reference=reference())['checks']
    keys=[c['response'] for c in checks if c['criterion'] in ['accuracy','expected_zero']]
    assert keys==sorted(keys)


def test_complete_criteria_receipt_is_canonical_json():
    from digitalmodel.ansys.analysis_records import canonical_bytes, parse_json
    result=evaluate_canary(attempts(),reference())
    assert parse_json(canonical_bytes(result))==result


def test_criteria_ignore_ambient_decimal_rounding_precision_and_traps():
    from decimal import Context, Inexact, ROUND_DOWN
    rows,ref=attempts(),reference()
    expected=evaluate_canary(rows,ref)
    with localcontext(Context(prec=6,rounding=ROUND_DOWN)) as ctx:
        ctx.traps[Inexact]=True
        assert evaluate_canary(rows,ref)==expected
