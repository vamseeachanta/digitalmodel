"""Explicitly authorized harmless Windows qualification; never a solver."""
import os
import pytest
from digitalmodel.solvers.ghs import qualification as q


@pytest.mark.skipif(os.name!='nt' or os.environ.get('GHS_SENTINEL_ACCEPTANCE')!='1',
                    reason='Explicit Windows harmless-sentinel acceptance only')
def test_eight_actual_windows_scenarios(tmp_path):
    result=q.qualify(tmp_path/'observations')
    assert result['state']=='sentinel_containment_passed'
    assert {item['name'] for item in result['scenarios']}==set(q.SCENARIOS)
    assert all(item['passed'] for item in result['scenarios'])
    assert result['ghs_launch_allowed'] is False
    assert result['licensed_execution_verified'] is False
