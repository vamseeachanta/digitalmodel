"""#1281a: operating-envelope engine (offset x current x seastate).

The load-bearing checks are the anti-degeneracy guards the T2 review demanded:
the von Mises utilisation MUST rise with current (via the beam-column moment)
and the flex-joint utilisation MUST rise with offset. An envelope whose
responses don't move with its inputs is worthless.
"""
from __future__ import annotations

from pathlib import Path

import numpy as np
import pytest

from digitalmodel.drilling_riser.envelope import (
    EnvelopeCriteria,
    EnvelopeResult,
    OperatingMode,
    RigEnvelopeLimits,
    RiserSection,
    SeaState,
    compute_operating_envelope,
    resolve_envelope_criteria,
)


def _fixture_repo_root() -> Path:
    return Path(__file__).resolve().parent.parent / "citations" / "fixtures"


_SECTION = RiserSection(outer_diameter_m=0.5334, wall_thickness_m=0.0254)
_CRITERIA = EnvelopeCriteria(
    flexjoint_angle_mean_deg=2.0, flexjoint_angle_max_deg=4.0, von_mises_design_factor=0.67
)
_BASE = dict(
    section=_SECTION,
    water_depth_m=1500.0,
    length_m=1500.0,
    tension_n=3.0e6,
    criteria=_CRITERIA,
)


def _env(offsets, currents, seastates, **kw):
    return compute_operating_envelope(
        offsets_pct=offsets, current_speeds_mps=currents, seastates=seastates, **_BASE, **kw
    )


# -- criteria resolution (fail-closed, fixture) --------------------------------


def test_resolve_criteria_from_fixture():
    crit = resolve_envelope_criteria(OperatingMode.DRILLING, repo_root=_fixture_repo_root())
    assert crit.flexjoint_angle_mean_deg == 2.0
    assert crit.flexjoint_angle_max_deg == 4.0
    assert crit.von_mises_design_factor == 0.67


# -- shape / typing ------------------------------------------------------------


def test_envelope_shape_and_type():
    r = _env([1.0, 2.0, 3.0], [0.5, 1.0], [SeaState(3.0, 10.0)])
    assert isinstance(r, EnvelopeResult)
    assert r.allowable_mask.shape == (3, 2, 1)
    assert r.per_limit_utilisation["flexjoint_angle"].shape == (3, 2, 1)
    assert 0.0 <= r.operable_fraction <= 1.0


# -- ANTI-DEGENERACY GUARDS (the core T2 fix) ----------------------------------


def test_von_mises_utilisation_rises_with_current():
    r = _env([2.0], [0.25, 0.75, 1.5], [SeaState(2.0, 9.0)])
    vm = r.per_limit_utilisation["von_mises"][0, :, 0]
    assert vm[0] < vm[1] < vm[2]  # current moves the bending -> von Mises


def test_flexjoint_utilisation_rises_with_offset():
    r = _env([0.5, 1.5, 3.0], [0.5], [SeaState(2.0, 9.0)])
    fj = r.per_limit_utilisation["flexjoint_angle"][:, 0, 0]
    assert fj[0] < fj[1] < fj[2]  # offset moves the flex-joint angle


def test_seastate_moves_flexjoint_via_rao():
    # Low static offset/current so the dynamic (max-angle) check governs — the
    # regime where sea state actually drives the flex-joint utilisation.
    r = _env([0.2], [0.1], [SeaState(1.0, 9.0), SeaState(6.0, 12.0)], rao_angle_deg_per_m=0.3)
    fj = r.per_limit_utilisation["flexjoint_angle"][0, 0, :]
    assert fj[1] > fj[0]  # bigger Hs -> bigger motion-driven max angle


# -- allowable region behaviour ------------------------------------------------


def test_benign_point_allowable_extreme_point_not():
    r = _env(
        [0.5, 8.0],            # benign vs large offset (% WD)
        [0.2, 2.5],            # mild vs strong current
        [SeaState(2.0, 9.0)],
    )
    assert bool(r.allowable_mask[0, 0, 0]) is True     # benign corner allowable
    assert bool(r.allowable_mask[1, 1, 0]) is False    # extreme corner excluded


def test_governing_limit_is_a_known_name():
    r = _env([4.0], [1.5], [SeaState(3.0, 10.0)])
    assert r.governing_limit[0, 0, 0] in {"flexjoint_angle", "von_mises", "stroke", "moonpool"}


# -- optional rig limits populate / NaN when absent ----------------------------


def test_operating_mode_selects_active_limits():
    common = dict(
        offsets_pct=[2.0], current_speeds_mps=[1.0], seastates=[SeaState(3.0, 10.0)],
        rig_limits=RigEnvelopeLimits(tj_stroke_m=18.0, moonpool_half_min_m=3.5),
    )
    drilling = compute_operating_envelope(**_BASE, mode=OperatingMode.DRILLING, **common)
    hang = compute_operating_envelope(**_BASE, mode=OperatingMode.HANG_OFF, **common)
    # drilling gates on flex-joint angle + moonpool; hang-off does not
    assert not np.isnan(drilling.per_limit_utilisation["flexjoint_angle"]).all()
    assert np.isnan(hang.per_limit_utilisation["flexjoint_angle"]).all()
    assert np.isnan(hang.per_limit_utilisation["moonpool"]).all()
    assert not np.isnan(hang.per_limit_utilisation["von_mises"]).all()


def test_stroke_and_moonpool_nan_without_rig_data():
    r = _env([2.0], [0.5], [SeaState(2.0, 9.0)])
    assert np.isnan(r.per_limit_utilisation["stroke"]).all()
    assert np.isnan(r.per_limit_utilisation["moonpool"]).all()


def test_stroke_and_moonpool_populate_with_rig_data():
    r = _env(
        [1.0, 5.0],
        [0.5],
        [SeaState(2.0, 9.0)],
        rig_limits=RigEnvelopeLimits(tj_stroke_m=18.0, moonpool_half_min_m=3.5),
        rao_heave_m_per_m=0.6,
    )
    assert not np.isnan(r.per_limit_utilisation["stroke"]).any()
    assert not np.isnan(r.per_limit_utilisation["moonpool"]).any()
    # moonpool excursion grows with offset
    mp = r.per_limit_utilisation["moonpool"][:, 0, 0]
    assert mp[1] > mp[0]
