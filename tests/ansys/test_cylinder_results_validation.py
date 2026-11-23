"""Offline evidence orchestration guards; no native completeness is fabricated."""
from digitalmodel.ansys.cylinder_results_validation import validate_native_evidence


def test_missing_native_artifacts_cannot_be_replaced_with_booleans():
    result=validate_native_evidence({}, {'verified':True}, 'a'*64, 'a'*64)
    assert result['status']=='INCOMPLETE'
    assert result['values']=={}


def test_reference_hash_drift_refuses_before_values():
    result=validate_native_evidence({}, {}, 'a'*64, 'b'*64)
    assert result['status']=='INCOMPLETE'
    assert any('reference' in error for error in result['errors'])
