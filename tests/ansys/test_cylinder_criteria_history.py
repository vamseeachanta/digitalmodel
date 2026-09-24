"""Preserve accumulated evidence when a later attempt blocks the canary."""
from decimal import Decimal

import pytest

from digitalmodel.ansys.cylinder_criteria import evaluate_attempt, evaluate_canary
from tests.ansys.test_cylinder_criteria import attempts, reference


@pytest.mark.parametrize('fault',['fine_failure','third_incomplete'])
def test_later_blocking_result_retains_all_prior_checks_in_order(fault):
    rows=attempts()
    rows[1]['values']['inner_y120','sigma_theta']=Decimal('99999')
    if fault=='fine_failure':
        rows[-1]['values']['inner_y120','sigma_theta']=Decimal('99999')
        expected_status='FAIL'
    else:
        rows=rows[:3]
        rows[-1]['evidence_errors']=['Missing native state']
        expected_status='INCOMPLETE'
    expected=[]
    for row in rows:
        expected.extend(evaluate_attempt(**row,reference=reference())['checks'])
    result=evaluate_canary(rows,reference())
    assert result['status']==expected_status
    assert result['checks']==expected
    assert any(check['criterion']=='zero_control' for check in result['checks'])
    assert any(not check['passed'] for check in result['checks'])
    assert result['attempted']==[row['case_id'] for row in rows]
    if fault=='third_incomplete':
        assert result['evidence_errors']==['Missing native state']
        assert result['unattempted']==['ocv-t60-p10-n16']
