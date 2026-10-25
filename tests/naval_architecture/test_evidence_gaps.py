# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""No empty evidence collection may imply an evaluated safe condition."""

import numpy as np
import pytest

from digitalmodel.naval_architecture.damage_stability_screening import DamageCaseResult
from digitalmodel.naval_architecture.region_report import _notes
from digitalmodel.naval_architecture.seakeeping import significant_motion
from digitalmodel.naval_architecture.symmetry_cut import check_symmetry_cut
from digitalmodel.naval_architecture.vessel_stability_screening import LiftingResult


def test_empty_damage_criteria_do_not_pass():
    result = DamageCaseResult(
        case_name="synthetic", method="direct", displacement_t=1.0,
        gm_fluid_m=1.0, heel_deg=None, limit_angle_deg=None,
        downflooding_margin_deg=None, range_beyond_equilibrium_deg=None,
        residual_area_m_rad=None, heeling_area_m_rad=None, area_ratio=None,
        criteria=(),
    )
    assert result.passed is False


def test_empty_lifting_criteria_do_not_pass():
    result = LiftingResult(
        case_name="synthetic", heeling_moment_t_m=0.0, heeling_arm_0_m=0.0,
        equilibrium_heel_deg=None, residual_range_to_deg=None,
        residual_area_m_rad=None, heeling_area_m_rad=None,
        residual_area_ratio=None, criteria=(),
    )
    assert result.passed is False


def test_empty_spectral_evidence_is_refused():
    with pytest.raises(ValueError, match="nonempty"):
        significant_motion([], [], 1.0)


@pytest.mark.parametrize(
    "rao_values,spectrum,step,expected",
    [
        (np.ones(10), np.ones(10), 0.1, 2.0),
        (np.array([0.0]), np.array([1.0]), 0.1, 0.0),
        (np.array([2.0]), np.array([0.25]), 1.0, 2.0),
    ],
)
def test_nonempty_numpy_spectral_evidence(rao_values, spectrum, step, expected):
    # s = 2*sqrt(sum(RAO**2*S*step)): 10*1*1*0.1 = 1 gives 2;
    # 0**2*1*0.1 = 0 gives 0; 2**2*0.25*1 = 1 gives 2.
    assert significant_motion(rao_values, spectrum, step) == pytest.approx(expected)


def test_empty_numpy_spectral_evidence_is_refused():
    with pytest.raises(ValueError, match="nonempty"):
        significant_motion(np.array([]), np.array([]), 0.1)


def test_missing_cut_evidence_emits_unevaluated_note():
    notes = _notes([], {}, {"synthetic": {}}, 0)
    assert len(notes) == 1
    assert "NOT_EVALUATED" in notes[0]


def test_explicit_clean_cut_does_not_emit_warning():
    assert _notes([], {}, {"synthetic": {"cut_is_clean": True}}, 0) == []


def test_empty_symmetry_sections_do_not_pass():
    result = check_symmetry_cut([], location_in_mesh=(0.0, 0.0, 0.0))
    assert result.ok is False
    payload = result.to_dict()
    assert payload["centreplane_cut_ok"] is False
    assert any("NOT_EVALUATED" in note for note in payload["failures"])
