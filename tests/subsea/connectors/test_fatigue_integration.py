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
