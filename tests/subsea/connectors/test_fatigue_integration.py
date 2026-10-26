# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Synthetic signed-radius histories through the native fatigue modules."""

import math

import numpy as np
import pytest

from digitalmodel.fatigue.assessment import AssessmentInput, assess_fatigue
from digitalmodel.fatigue.sn_curves import get_sn_curve
from digitalmodel.subsea.connectors import (
    PipeSection, JumperType, get_jumper_spec, assess_jumper_fatigue,
)


def settings(**changes):
    """A synthetic one-year block and one-year criterion."""
    values = dict(block_years=1.0, design_years=1.0, youngs_modulus_pa=1e9)
    values.update(changes)
    return values


@pytest.fixture
def synthetic_power_law(monkeypatch):
    """Inject only curve lookup; radius conversion, rainflow and Miner stay real.

    Synthetic assumptions: N = C / delta_sigma**m, C = 8,000,000 cycles*MPa**3
    and m = 3, with ranges in MPa. These deliberately chosen coefficients
    have no catalogue or standards basis. Each test writes its own hand
    calculation of N and damage without calling this curve for expectations.
    """
    class SyntheticCurve:
        def cycles(self, stress_ranges):
            return 8_000_000.0 / np.asarray(stress_ranges, dtype=float)**3

    def lookup(name, environment):
        # The existing selection reaches this boundary; no library curve runs.
        assert (name, environment) == ("D", "air")
        return SyntheticCurve()

    monkeypatch.setattr("digitalmodel.fatigue.assessment.get_sn_curve", lookup)


def test_jumper_damage_independent_synthetic_curve(synthetic_power_law):
    """Hand calculation using only synthetic geometry, material and exposure.

    Assume E = 1e9 Pa, D = 0.2 m, wall = 0.02 m and signed radii
    [10, -10, 10] m. At the fixed hot spot sigma = E*D/(2*R_signed),
    so nominal stresses are [10, -10, 10] MPa (1 MPa = 1e6 Pa).
    Explicit synthetic SCF = 2 gives [20, -20, 20] MPa. Two reversals
    leave two residual half cycles, each of range 40 MPa: sum n = 1.
    The fixture's synthetic law gives N = 8,000,000/40**3 = 125 cycles.
    Block damage = sum(n/N) = 0.5/125 + 0.5/125 = 1/125.
    For an assumed one-year block, life = block_years/damage = 125 years.
    An assumed 25-year design exposure and fatigue factor 2 give utilisation
    = damage * design_years/block_years * factor = (1/125)*25*2 = 0.4.
    """
    result = assess_jumper_fatigue(
        get_jumper_spec(JumperType.FLEXIBLE), PipeSection(0.2, 0.02),
        [10.0, -10.0, 10.0], **settings(
            stress_concentration_factor=2.0, design_years=25.0,
            design_fatigue_factor=2.0,
        ),
    )
    assert result.stress_history_mpa == pytest.approx([20.0, -20.0, 20.0])
    assessment = result.assessment
    assert assessment.block_damage == pytest.approx(1.0 / 125.0)
    assert assessment.life_years == pytest.approx(125.0)
    assert assessment.utilisation == pytest.approx(0.4)
    assert assessment.passed is True
    assert assessment.governing_load_case == "jumper-bending"
    case, = assessment.cases
    assert case.block_damage == pytest.approx(1.0 / 125.0)
    assert len(case.cycles) == 2
    for cycle in case.cycles:
        assert cycle.stress_range_mpa == pytest.approx(40.0)
        assert cycle.cycles == pytest.approx(0.5)
        assert cycle.allowable_cycles == pytest.approx(125.0)
        assert cycle.damage == pytest.approx(0.5 / 125.0)


@pytest.mark.parametrize(
    "repetitions,block_years,expected_life,expected_utilisation,expected_passed",
    [
        (1, 0.25, 125.0 / 36.0, 72.0 / 25.0, False),
        (1, 1.0, 125.0 / 9.0, 18.0 / 25.0, True),
        (3, 0.75, 125.0 / 36.0, 72.0 / 25.0, False),
        (3, 0.25, 125.0 / 108.0, 216.0 / 25.0, False),
    ],
    ids=["quarter-year", "one-year", "three-blocks", "three-blocks-fixed-time"],
)
def test_jumper_signed_curvature_and_exposure_independent(
    synthetic_power_law, repetitions, block_years, expected_life,
    expected_utilisation, expected_passed,
):
    """Hand count unequal signed curvatures and repeated synthetic blocks.

    Assume E = 1e9 Pa, D = 0.2 m, wall = 0.02 m, explicit SCF = 2
    and radii [5, -10, 10, -5, 5] m. Sigma = E*D/(2*R_signed)
    gives [20, -10, 10, -20, 20] MPa before SCF (1 MPa = 1e6 Pa),
    hence [40, -20, 20, -40, 40] MPa after SCF. Four reversals contain
    one closed inner cycle of range 40 MPa and two outer half cycles
    of range 80 MPa. Thus sum n = 2 equivalent full cycles per block.
    Synthetic N = 8,000,000/delta_sigma**3 gives N40 = 125 and
    N80 = 125/8 cycles. Damage = sum(n/N) = 1/125 + 1/(125/8) = 9/125.

    Join repetitions at the common 40 MPa endpoint without duplicating it.
    Each repetition adds four reversals and one equivalent cycle per range;
    damage = repetitions*9/125. Assumed design exposure = 5 years and
    fatigue factor = 2, so life = block_years*125/(9*repetitions) and
    utilisation = (9*repetitions/125)*5/block_years*2. The parameter rows
    are these independent hand results for (repetitions, block_years):
    (1, 1/4): life = 125/36 years, utilisation = 72/25;
    (1, 1): life = 125/9 years, utilisation = 18/25;
    (3, 3/4): life = 125/36 years, utilisation = 72/25;
    (3, 1/4): life = 125/108 years, utilisation = 216/25.
    Repeating history and duration together preserves life; changing only
    duration scales life directly and utilisation inversely. Unity is the
    mathematical damage criterion, not a standards qualification claim.
    """
    radii = [5.0] + [-10.0, 10.0, -5.0, 5.0] * repetitions
    expected_stresses = [40.0] + [-20.0, 20.0, -40.0, 40.0] * repetitions
    result = assess_jumper_fatigue(
        get_jumper_spec(JumperType.FLEXIBLE), PipeSection(0.2, 0.02),
        radii, **settings(
            stress_concentration_factor=2.0, block_years=block_years,
            design_years=5.0, design_fatigue_factor=2.0,
        ),
    )
    assert result.stress_history_mpa == pytest.approx(expected_stresses)
    assessment = result.assessment
    assert assessment.block_damage == pytest.approx(repetitions * 9.0 / 125.0)
    assert assessment.life_years == pytest.approx(expected_life)
    assert assessment.utilisation == pytest.approx(expected_utilisation)
    assert assessment.passed is expected_passed
    case, = assessment.cases
    assert case.block_damage == pytest.approx(repetitions * 9.0 / 125.0)
    counts = {40.0: 0.0, 80.0: 0.0}
    for cycle in case.cycles:
        assert cycle.stress_range_mpa in counts
        counts[cycle.stress_range_mpa] += cycle.cycles
    assert counts == pytest.approx({40.0: repetitions, 80.0: repetitions})


def test_signed_radius_cycles_have_hand_checkable_damage():
    # E=1e9 Pa, D=0.2 m, R=10 m gives +/-10 MPa, range=20 MPa.
    # Two reversals give two residual half cycles: one equivalent full cycle.
    result = assess_jumper_fatigue(
        get_jumper_spec(JumperType.FLEXIBLE), PipeSection(0.2, 0.02),
        [10.0, -10.0, 10.0], **settings(),
    )
    curve = get_sn_curve("D")
    allowable = float(curve.cycles(np.array([20.0]))[0])
    assert result.assessment.block_damage == pytest.approx(1.0 / allowable)
    assert result.assessment.life_years == pytest.approx(allowable)
    assert result.stress_history_mpa == pytest.approx([10.0, -10.0, 10.0])
    assert result.assessment.governing_load_case == "jumper-bending"


def test_supplied_scf_is_applied_before_counting():
    result = assess_jumper_fatigue(
        get_jumper_spec(JumperType.FLEXIBLE), PipeSection(0.2, 0.02),
        [10.0, -10.0, 10.0], **settings(stress_concentration_factor=2.0),
    )
    expected = assess_fatigue(AssessmentInput(
        block_years=1.0, design_years=1.0,
        load_cases=[dict(name="jumper-bending", kind="history",
                         stress_history_mpa=[20.0, -20.0, 20.0])],
    ))
    assert result.assessment == expected


def test_positive_radius_does_not_invent_reversal():
    result = assess_jumper_fatigue(
        get_jumper_spec(JumperType.VERTICAL), PipeSection(0.2, 0.02),
        [10.0, 10.0, 10.0], **settings(),
    )
    assert result.assessment.block_damage == 0.0
    assert result.assessment.life_years is None


def test_static_catalogue_entry_does_not_trigger_fatigue():
    with pytest.raises(ValueError, match="dynamic"):
        assess_jumper_fatigue(get_jumper_spec(JumperType.RIGID),
                             PipeSection(0.2, 0.02), [10.0, -10.0], **settings())


@pytest.mark.parametrize("radii", [[], [10.0], [0.0, 10.0],
                                   [math.nan, 10.0], [math.inf, 10.0]])
def test_invalid_radius_history_is_rejected(radii):
    with pytest.raises(ValueError):
        assess_jumper_fatigue(get_jumper_spec(JumperType.FLEXIBLE),
                             PipeSection(0.2, 0.02), radii, **settings())


@pytest.mark.parametrize("changes", [dict(youngs_modulus_pa=0),
                                     dict(youngs_modulus_pa=math.inf),
                                     dict(stress_concentration_factor=0.5),
                                     dict(stress_concentration_factor=math.nan)])
def test_invalid_material_or_scf_is_rejected(changes):
    with pytest.raises(ValueError):
        assess_jumper_fatigue(get_jumper_spec(JumperType.FLEXIBLE),
                             PipeSection(0.2, 0.02), [10.0, -10.0],
                             **settings(**changes))
