"""Illustrative example inputs exercising established integrity methods."""

import math

import pytest

from digitalmodel.asset_integrity.circumferential_defect import part5_shell_parameter
from digitalmodel.asset_integrity.common.API579_components import API579_components
from digitalmodel.structural.stress.vm_stress import (
    StressState,
    VonMisesStressCalculator,
)


def test_existing_shell_parameter_against_hand_formula_and_scale_identity():
    diameter, wall, length = 0.8, 0.02, 0.12
    expected = 1.285 * length / math.sqrt(diameter * wall)
    assert part5_shell_parameter(diameter, wall, length) == pytest.approx(expected)
    assert part5_shell_parameter(2 * diameter, 2 * wall, 2 * length) == pytest.approx(expected)
    assert part5_shell_parameter(diameter, wall, 0.0) == 0.0


@pytest.mark.parametrize('diameter,wall,length', [(0, 0.02, 0.12), (0.8, 0, 0.12), (0.8, 0.02, -0.12)])
def test_existing_shell_parameter_rejects_invalid_geometry(diameter, wall, length):
    with pytest.raises(ValueError):
        part5_shell_parameter(diameter, wall, length)


def test_existing_supplied_factor_strength_equation():
    """Illustrative factor samples are arbitrary, not a standards table."""
    calculator = API579_components({})
    config = {'API579Parameters': {'FoliasFactor': {
        'FlawParameter': [0.0, 4.0], 'Mt': {'Cylindrical': [1.0, 3.0]},
    }}}
    # Linear interpolation at the midpoint gives Mt=2. Remaining area is 1/5.
    data = {'RSFa': 0.85, 'LongitudinalFlawLengthParameter': 2.0,
            'Rt': 0.1, 'MAWP': 12.0, 'AARatio': 0.2}
    result = calculator.LMLMAWPrEvaluation(data, config, Level=2)
    expected = (1 / 5) / (1 - (1 - 1 / 5) / 2)
    assert result['Mt'] == 2.0
    assert result['RSF'] == pytest.approx(expected)
    assert result['MAWPr'] == pytest.approx(12.0 * expected / 0.85)


def test_existing_strength_gate_retains_unreduced_capacity():
    """An established gate returns unity above its thickness-ratio floor."""
    calculator = API579_components({})
    config = {'API579Parameters': {'FoliasFactor': {
        'FlawParameter': [0.0, 4.0], 'Mt': {'Cylindrical': [1.0, 3.0]},
    }}}
    data = {'RSFa': 0.85, 'LongitudinalFlawLengthParameter': 2.0,
            'Rt': 0.8, 'MAWP': 12.0, 'AARatio': 0.2}
    result = calculator.LMLMAWPrEvaluation(data, config, Level=2)
    assert result['RSF'] == 1.0
    assert result['MAWPr'] == 12.0


def test_existing_averaging_length_high_ratio_branch():
    readings = [0.016, 0.018, 0.020]
    result = API579_components({}).GMLAssessmentLengthEvaluation(
        readings, {'tmin': 0.016, 'NominalID': 0.8, 'RSFa': 0.85},
    )
    mean = sum(readings) / len(readings)
    correction = mean - 0.016
    reference = 0.016 - correction
    diameter = 0.8 + 2 * correction
    assert result['LengthParameter'] == 50.0
    assert result['AveragingLength'] == pytest.approx(50.0 * math.sqrt(diameter * reference))


@pytest.mark.parametrize('hoop,axial,shear', [(80e6, 30e6, 5e6), (0.0, 0.0, 4e6), (6e6, 6e6, 0.0)])
def test_existing_equivalent_membrane_stress(hoop, axial, shear):
    expected = math.sqrt(hoop ** 2 - hoop * axial + axial ** 2 + 3 * shear ** 2)
    state = StressState(hoop, axial, 0.0, tau_xy=shear)
    actual = VonMisesStressCalculator.calculate_from_stress_state(state)
    assert actual == pytest.approx(expected)


def test_existing_equivalent_stress_rejects_nonfinite_input():
    with pytest.raises(ValueError):
        VonMisesStressCalculator.calculate_from_stress_state(StressState(math.nan, 0.0, 0.0))
