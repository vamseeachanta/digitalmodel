"""#1281a: operating-envelope engine (offset x current x seastate).

The load-bearing checks are the anti-degeneracy guards the T2 review demanded:
the von Mises utilisation MUST rise with current (via the beam-column moment)
and the flex-joint utilisation MUST rise with offset. An envelope whose
responses don't move with its inputs is worthless.
"""

from __future__ import annotations

import os
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.drilling_riser.envelope import (
    ConductorInput,
    EnvelopeCriteria,
    EnvelopeResult,
    OperatingMode,
    RigEnvelopeLimits,
    RiserSection,
    SeaState,
    compute_operating_envelope,
    resolve_envelope_criteria,
)

_CONDUCTOR = ConductorInput(
    outer_diameter_m=0.762,
    wall_thickness_m=0.0254,
    soil_modulus_n_per_m2=5.0e6,
    stand_off_m=18.0,
)


def _fixture_repo_root() -> Path:
    return Path(__file__).resolve().parent.parent / "citations" / "fixtures"


def _amjig_repo_root() -> Path:
    env = os.environ.get("LLM_WIKI_PATH")
    if not env:
        pytest.skip("LLM_WIKI_PATH required for AMJIG private-value criteria")
    root = Path(env)
    page = (
        root
        / "wikis"
        / "engineering-standards"
        / "wiki"
        / "standards"
        / "amjig-2000.md"
    )
    if not page.is_file():
        pytest.skip("amjig-2000 wiki criteria page not available")
    return root


_SECTION = RiserSection(outer_diameter_m=0.5334, wall_thickness_m=0.0254)
_CRITERIA = EnvelopeCriteria(
    flexjoint_angle_mean_deg=2.0,
    flexjoint_angle_max_deg=4.0,
    von_mises_design_factor=0.67,
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
        offsets_pct=offsets,
        current_speeds_mps=currents,
        seastates=seastates,
        **_BASE,
        **kw,
    )


# -- criteria resolution (fail-closed, fixture) --------------------------------


def test_resolve_criteria_from_fixture():
    crit = resolve_envelope_criteria(
        OperatingMode.DRILLING, repo_root=_fixture_repo_root()
    )
    assert crit.flexjoint_angle_mean_deg == 2.0
    assert crit.flexjoint_angle_max_deg == 4.0
    assert crit.von_mises_design_factor == 0.67
    assert crit.criteria_set == "16q"


def test_resolve_amjig_criteria_from_private_wiki():
    plain = resolve_envelope_criteria(
        OperatingMode.CONNECTED, criteria_set="16q", repo_root=_fixture_repo_root()
    )
    amjig = resolve_envelope_criteria(
        OperatingMode.CONNECTED,
        criteria_set="16q-amjig",
        repo_root=_amjig_repo_root(),
    )
    assert amjig.criteria_set == "16q-amjig"
    assert amjig.category
    assert (
        amjig.von_mises_design_factor != plain.von_mises_design_factor
        or amjig.flexjoint_angle_max_deg != plain.flexjoint_angle_max_deg
    )


# -- shape / typing ------------------------------------------------------------


def test_envelope_shape_and_type():
    r = _env([1.0, 2.0, 3.0], [0.5, 1.0], [SeaState(3.0, 10.0)])
    assert isinstance(r, EnvelopeResult)
    assert r.allowable_mask.shape == (3, 2, 1)
    assert r.per_limit_utilisation["flexjoint_angle"].shape == (3, 2, 1)
    assert 0.0 <= r.operable_fraction <= 1.0
    assert r.criteria_set == "16q"


def test_criteria_set_tag_propagates_to_result():
    criteria = EnvelopeCriteria(
        flexjoint_angle_mean_deg=2.0,
        flexjoint_angle_max_deg=4.0,
        von_mises_design_factor=0.67,
        criteria_set="16q-amjig",
        category="extreme",
    )
    result = compute_operating_envelope(
        **{**_BASE, "criteria": criteria},
        offsets_pct=[1.0],
        current_speeds_mps=[0.5],
        seastates=[SeaState(2.0, 9.0)],
    )
    assert result.criteria_set == "16q-amjig"
    assert result.criteria_category == "extreme"


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
    r = _env(
        [0.2], [0.1], [SeaState(1.0, 9.0), SeaState(6.0, 12.0)], rao_angle_deg_per_m=0.3
    )
    fj = r.per_limit_utilisation["flexjoint_angle"][0, 0, :]
    assert fj[1] > fj[0]  # bigger Hs -> bigger motion-driven max angle


# -- allowable region behaviour ------------------------------------------------


def test_benign_point_allowable_extreme_point_not():
    r = _env(
        [0.5, 8.0],  # benign vs large offset (% WD)
        [0.2, 2.5],  # mild vs strong current
        [SeaState(2.0, 9.0)],
    )
    assert bool(r.allowable_mask[0, 0, 0]) is True  # benign corner allowable
    assert bool(r.allowable_mask[1, 1, 0]) is False  # extreme corner excluded


def test_16q_and_amjig_can_produce_different_allowable_masks():
    common = dict(
        section=_SECTION,
        water_depth_m=1500.0,
        length_m=1500.0,
        tension_n=3.0e6,
        offsets_pct=np.linspace(0.5, 8.0, 8),
        current_speeds_mps=np.linspace(0.2, 2.5, 8),
        seastates=[SeaState(2.0, 9.0), SeaState(6.0, 12.0)],
        mode=OperatingMode.CONNECTED,
    )
    plain = compute_operating_envelope(
        criteria=resolve_envelope_criteria(
            OperatingMode.CONNECTED, criteria_set="16q", repo_root=_fixture_repo_root()
        ),
        **common,
    )
    amjig = compute_operating_envelope(
        criteria=resolve_envelope_criteria(
            OperatingMode.CONNECTED,
            criteria_set="16q-amjig",
            repo_root=_amjig_repo_root(),
        ),
        **common,
    )
    assert plain.criteria_set == "16q"
    assert amjig.criteria_set == "16q-amjig"
    assert not np.array_equal(plain.allowable_mask, amjig.allowable_mask)


def test_governing_limit_is_a_known_name():
    r = _env([4.0], [1.5], [SeaState(3.0, 10.0)])
    assert r.governing_limit[0, 0, 0] in {
        "flexjoint_angle",
        "von_mises",
        "stroke",
        "moonpool",
    }


# -- optional rig limits populate / NaN when absent ----------------------------


def test_operating_mode_selects_active_limits():
    common = dict(
        offsets_pct=[2.0],
        current_speeds_mps=[1.0],
        seastates=[SeaState(3.0, 10.0)],
        rig_limits=RigEnvelopeLimits(tj_stroke_m=18.0, moonpool_half_min_m=3.5),
    )
    drilling = compute_operating_envelope(
        **_BASE, mode=OperatingMode.DRILLING, **common
    )
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


# -- #1346 solver-tier dynamic path (stub library) -----------------------------


def test_dynamic_amplifies_von_mises_and_carries_stub_provenance(monkeypatch):
    from digitalmodel.parametric.atlas import Atlas
    from digitalmodel.parametric.query import DEFAULT_ATLAS_ROOT

    atlas = Atlas.load(DEFAULT_ATLAS_ROOT, "drilling_riser_envelope")
    # Synthetic measured-zero metadata exercises the validated caller path.
    atlas.validation = {"max_rel_error": 0.0}
    monkeypatch.setattr(Atlas, "load", lambda *args, **kwargs: atlas)
    common = dict(
        offsets_pct=[2.0], current_speeds_mps=[1.0], seastates=[SeaState(3.0, 12.0)]
    )
    static = compute_operating_envelope(**_BASE, **common)
    dyn = compute_operating_envelope(**_BASE, dynamic=True, **common)
    vs = static.per_limit_utilisation["von_mises"][0, 0, 0]
    vd = dyn.per_limit_utilisation["von_mises"][0, 0, 0]
    assert vd > vs  # DAF >= 1 amplifies the static von Mises
    assert vd / vs == pytest.approx(1.20, rel=1e-3)  # the stub DAF at this point
    # governance: a dynamically-amplified verdict never leaves without its marker
    assert dyn.dynamic_provenance["solver_licensed"] is False
    assert dyn.dynamic_provenance["solver_version"] == "STUB"
    assert dyn.dynamic_provenance["applies_to"] == "von_mises"
    assert "disclaimer" in dyn.dynamic_provenance


def test_dynamic_without_validation_escalates_each_covered_point():
    from digitalmodel.parametric.atlas import Atlas
    from digitalmodel.parametric.query import DEFAULT_ATLAS_ROOT

    atlas = Atlas.load(DEFAULT_ATLAS_ROOT, "drilling_riser_envelope")
    assert not atlas.validation_available
    assert atlas.predict({"mode": "drilling", "offset_pct": 2.0,
                          "current_speed_mps": 1.0, "hs_m": 3.0,
                          "tp_s": 12.0}).in_range
    dyn = compute_operating_envelope(
        **_BASE, dynamic=True, offsets_pct=[2.0, 4.0],
        current_speeds_mps=[0.5, 1.0], seastates=[SeaState(3.0, 12.0)],
    )
    assert np.isnan(dyn.per_limit_utilisation["von_mises"]).all()
    assert dyn.dynamic_provenance["escalated_points"] == 4
    assert not dyn.allowable_mask.any()


@pytest.mark.parametrize("tension_n", [3.0e6, 30.0e6])
def test_unavailable_dynamic_check_cannot_make_hang_off_allowable(monkeypatch, tension_n):
    from digitalmodel.parametric.atlas import Atlas
    from digitalmodel.parametric.query import DEFAULT_ATLAS_ROOT

    atlas = Atlas.load(DEFAULT_ATLAS_ROOT, "drilling_riser_envelope")
    monkeypatch.setattr(Atlas, "load", lambda *args, **kwargs: atlas)
    common = dict(
        **{**_BASE, "tension_n": tension_n}, dynamic=True,
        offsets_pct=[2.0], current_speeds_mps=[1.0],
        seastates=[SeaState(3.0, 12.0)], mode=OperatingMode.HANG_OFF,
        rig_limits=RigEnvelopeLimits(tj_stroke_m=18.0),
    )
    atlas.validation = {"max_rel_error": 0.0}
    measured = compute_operating_envelope(**common)
    measured_vm = measured.per_limit_utilisation["von_mises"][0, 0, 0]
    assert (measured_vm > 1.0) == (tension_n == 30.0e6)
    assert measured.allowable_mask.item() == (measured_vm <= 1.0)
    assert measured.dynamic_provenance["escalated_points"] == 0

    atlas.validation = {}
    unavailable = compute_operating_envelope(**common)
    assert unavailable.per_limit_utilisation["stroke"].item() < 1.0
    assert np.isnan(unavailable.per_limit_utilisation["von_mises"]).all()
    assert unavailable.dynamic_provenance["escalated_points"] == 1
    assert not unavailable.allowable_mask.any()
    assert unavailable.operable_fraction == 0.0


def test_dynamic_coverage_escalation_rejects_only_affected_points(monkeypatch):
    from digitalmodel.parametric.atlas import Atlas
    from digitalmodel.parametric.query import DEFAULT_ATLAS_ROOT

    atlas = Atlas.load(DEFAULT_ATLAS_ROOT, "drilling_riser_envelope")
    atlas.validation = {"max_rel_error": 0.0}
    monkeypatch.setattr(Atlas, "load", lambda *args, **kwargs: atlas)
    result = compute_operating_envelope(
        **_BASE, dynamic=True, offsets_pct=[1.0, 2.0],
        current_speeds_mps=[1.0],
        seastates=[SeaState(3.0, 12.0), SeaState(3.0, 99.0)],
        mode=OperatingMode.HANG_OFF, rig_limits=RigEnvelopeLimits(tj_stroke_m=18.0),
    )
    assert (result.per_limit_utilisation["stroke"] < 1.0).all()
    assert result.allowable_mask[:, :, 0].all()
    assert not result.allowable_mask[:, :, 1].any()
    assert result.dynamic_provenance["escalated_points"] == 2
    assert result.operable_fraction == 0.5


def test_unavailable_dynamic_check_does_not_gate_an_inactive_limit(monkeypatch):
    from digitalmodel.drilling_riser import envelope

    monkeypatch.setattr(envelope, "_mode_active_limits", lambda: {"hang_off": {"stroke"}})
    result = compute_operating_envelope(
        **_BASE, dynamic=True, offsets_pct=[2.0], current_speeds_mps=[1.0],
        seastates=[SeaState(3.0, 12.0)], mode=OperatingMode.HANG_OFF,
        rig_limits=RigEnvelopeLimits(tj_stroke_m=18.0),
    )
    assert result.dynamic_provenance["escalated_points"] == 1
    assert result.allowable_mask.all()


def test_dynamic_false_is_a_noop():
    common = dict(
        offsets_pct=[1.0, 3.0],
        current_speeds_mps=[0.5, 1.5],
        seastates=[SeaState(2.0, 10.0)],
    )
    a = compute_operating_envelope(**_BASE, **common)
    b = compute_operating_envelope(**_BASE, dynamic=False, **common)
    assert b.dynamic_provenance is None
    assert np.array_equal(
        a.per_limit_utilisation["von_mises"], b.per_limit_utilisation["von_mises"]
    )


def test_dynamic_out_of_coverage_escalates_von_mises(monkeypatch):
    from digitalmodel.parametric.atlas import Atlas
    from digitalmodel.parametric.query import DEFAULT_ATLAS_ROOT

    atlas = Atlas.load(DEFAULT_ATLAS_ROOT, "drilling_riser_envelope")
    atlas.validation = {"max_rel_error": 0.0}
    monkeypatch.setattr(Atlas, "load", lambda *args, **kwargs: atlas)
    # offset 99% WD is outside the atlas grid -> von Mises escalates (NaN), counted
    dyn = compute_operating_envelope(
        **_BASE,
        dynamic=True,
        offsets_pct=[99.0],
        current_speeds_mps=[1.0],
        seastates=[SeaState(3.0, 12.0)],
    )
    assert np.isnan(dyn.per_limit_utilisation["von_mises"][0, 0, 0])
    assert dyn.dynamic_provenance["escalated_points"] == 1
    assert not dyn.allowable_mask.any()


# -- #1345 wellhead/conductor moment + flex-joint hardware rating ---------------


def test_wh_moment_rises_with_offset_and_current():
    r = compute_operating_envelope(
        **_BASE,
        offsets_pct=[1.0, 4.0],
        current_speeds_mps=[0.5, 1.5],
        seastates=[SeaState(2.0, 9.0)],
        conductor=_CONDUCTOR,
        rig_limits=RigEnvelopeLimits(conductor_moment_capacity_kn_m=8000.0),
    )
    whm = r.per_limit_utilisation["wh_moment"]
    assert not np.isnan(whm).any()
    assert whm[1, 0, 0] > whm[0, 0, 0]  # rises with offset (via lower-FJ shear)
    assert whm[0, 1, 0] > whm[0, 0, 0]  # rises with current


def test_wh_moment_nan_without_conductor_or_capacity():
    # no conductor input -> NaN
    a = compute_operating_envelope(
        **_BASE,
        offsets_pct=[2.0],
        current_speeds_mps=[1.0],
        seastates=[SeaState(2.0, 9.0)],
        rig_limits=RigEnvelopeLimits(conductor_moment_capacity_kn_m=8000.0),
    )
    assert np.isnan(a.per_limit_utilisation["wh_moment"]).all()
    # conductor but no rated capacity (columns ship empty) -> NaN
    b = compute_operating_envelope(
        **_BASE,
        offsets_pct=[2.0],
        current_speeds_mps=[1.0],
        seastates=[SeaState(2.0, 9.0)],
        conductor=_CONDUCTOR,
    )
    assert np.isnan(b.per_limit_utilisation["wh_moment"]).all()


def test_flexjoint_governing_min_uses_hardware_rating():
    common = dict(
        offsets_pct=[3.0],
        current_speeds_mps=[1.0],
        seastates=[SeaState(5.0, 11.0)],
        rao_angle_deg_per_m=0.2,
    )
    no_rating = compute_operating_envelope(**_BASE, **common)
    # a hardware rating tighter than the 4.0 deg operating max RAISES the
    # utilisation (governing min picks the smaller limit -> larger util)
    with_rating = compute_operating_envelope(
        **_BASE,
        **common,
        rig_limits=RigEnvelopeLimits(flexjoint_angle_rating_deg=1.0),
    )
    assert (
        with_rating.per_limit_utilisation["flexjoint_angle"][0, 0, 0]
        > no_rating.per_limit_utilisation["flexjoint_angle"][0, 0, 0]
    )
