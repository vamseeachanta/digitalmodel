"""Native shape-summary gates; fixtures are synthetic, not result evidence."""
import pytest

from tests.ansys.padeye_pressure_quality import assess_shape_output


def summary(warnings=0, elements=1032):
    return f'''ELEMENT SHAPE CHECKING IS ON WITH DEFAULT LIMITS
SHAPE TESTING SUMMARY
FOR ALL SELECTED ELEMENTS
| Element count {elements} PLANE182 |
Test Number tested Warning count Error count Warn+Err %
Aspect Ratio {elements} {warnings} 0 0.00 %
Parallel Deviation {elements} 0 0 0.00 %
Maximum Angle {elements} 0 0 0.00 %
Jacobian Ratio {elements} 0 0 0.00 %
Any {elements} {warnings} 0 0.00 %
NUMBER OF WARNING MESSAGES ENCOUNTERED= {warnings}
NUMBER OF ERROR MESSAGES ENCOUNTERED= 0
'''


def test_complete_default_shape_summary_passes():
    result = assess_shape_output(summary(), 1032)
    assert result['shape_gate_passed'] is True
    assert result['elements_tested'] == 1032
    assert result['native_qualification_complete'] is False


@pytest.mark.parametrize('text', [
    summary(34), summary().replace('DEFAULT LIMITS', 'MODIFIED LIMITS'),
    summary().replace('Maximum Angle 1032', 'Maximum Angle 1031'),
    summary().replace('Jacobian Ratio 1032 0 0 0.00 %', ''),
    summary()+summary(), summary().replace('ENCOUNTERED= 0', 'ENCOUNTERED= 1'),
    summary().replace('SHAPE TESTING SUMMARY', 'ELEMENT SHAPE CHECKING IS ON WITH MODIFIED LIMITS\nSHAPE TESTING SUMMARY'),
    summary().replace('Any 1032', 'Any 1031'),
    summary()+'\n*** ERROR *** database export failed',
])
def test_missing_partial_duplicated_or_failed_summary_refuses(text):
    with pytest.raises(ValueError):
        assess_shape_output(text, 1032)


def test_wrong_expected_mesh_count_refuses():
    with pytest.raises(ValueError):
        assess_shape_output(summary(), 832)
