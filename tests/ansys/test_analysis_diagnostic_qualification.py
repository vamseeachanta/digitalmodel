"""Diagnostic markers override otherwise complete isolated test authority."""
import copy

import pytest

from tests.ansys.test_analysis_evidence import intake
from tests.ansys.test_analysis_qualification import protocol
from digitalmodel.ansys.analysis_lookup import _qualify


@pytest.mark.parametrize("marker", [
    {"engineering_qualified": False},
    {"capture_role": "diagnostic_observation"},
    {"capture_role": "diagnostic_replay"},
    {"campaign_assessment_status": "INCOMPLETE"},
    {"campaign_assessment_status": "FAIL"},
])
def test_diagnostic_marker_refuses_permissive_test_authority(protocol, marker):
    _, resolver, package, _, authority = protocol
    case = copy.deepcopy(package["cases"][0])
    response = case["responses"][0]
    # Confirm the unmarked protocol fixture reaches the positive boundary.
    assert _qualify(package, case, response, "screening", authority, resolver)
    case.update(marker)
    with pytest.raises(ValueError, match="diagnostic or incomplete"):
        _qualify(package, case, response, "screening", authority, resolver)
