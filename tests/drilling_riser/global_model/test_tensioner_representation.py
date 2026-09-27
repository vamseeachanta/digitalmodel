"""Tensioner representation: inclined constant-tension lines (lateral tie of the ring to the vessel) or the
equivalent string (``vertical_force``): the tensioner force applied vertically at the string top, which is
pinned laterally to the vessel and free along z, with the telescopic joint locked and no lateral tie at the
ring. The two bound the split of rotation between the upper and lower flex joints."""

from __future__ import annotations

import math
from pathlib import Path

import pytest

from digitalmodel.drilling_riser.global_model.build import (
    TOP,
    build_generic_spec,
    equivalent_string_top_tension_n,
)
from digitalmodel.drilling_riser.global_model.spec import RiserGlobalModelSpec, Tensioners
from digitalmodel.solvers.orcaflex import orcaflex_api

from .conftest import synthetic_spec


def _vertical(**extra) -> RiserGlobalModelSpec:
    d = synthetic_spec().model_dump()
    d["tensioners"]["representation"] = "vertical_force"
    d.update(extra)
    return RiserGlobalModelSpec.model_validate(d)


def _line(gen, name):
    return next(ln for ln in gen["lines"] if ln["name"] == name)["properties"]


def test_default_representation_is_lines():
    spec = synthetic_spec()
    assert spec.tensioners.representation == "lines"
    gen = build_generic_spec(spec)["generic"]
    assert [w["name"] for w in gen["winches"]] == [f"Tensioner{i}" for i in range(1, 7)]
    assert [c["name"] for c in gen["constraints"]] == ["SlipJoint"]
    assert _line(gen, "InnerBarrel")["Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionAzimuth, "
                                     "ConnectionDeclination, ConnectionGamma, ConnectionReleaseStage, "
                                     "ConnectionzRelativeTo"][0][0] == "Vessel"
    assert _line(gen, "Riser")["IncludeTorsion"] is False


def test_equivalent_string_topology():
    spec = _vertical()
    gen = build_generic_spec(spec)["generic"]
    ends = _line(gen, "InnerBarrel")["Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionAzimuth, "
                                      "ConnectionDeclination, ConnectionGamma, ConnectionReleaseStage, ConnectionzRelativeTo"]
    assert ends[0][0] == TOP
    cons = {c["name"]: c for c in gen["constraints"]}
    # the telescopic joint is locked: the string is continuous
    assert cons["SlipJoint"]["properties"]["DOFFree, DOFInitialValue"][2] == [False]
    # the string top is on the vessel, free only along z, at the UFJ pivot
    top = cons[TOP]
    assert top["in_frame_connection"] == "Vessel"
    assert top["properties"]["InFrameInitialPosition"] == [0, 0, spec.upper_flex_joint.pivot_z_m]
    assert top["properties"]["DOFFree, DOFInitialValue"] == [[False], [False], [True, 0.0], [False], [False], [False]]
    # one vertical winch in the vessel frame applies the top tension; no ring tensioners
    (w,) = gen["winches"]
    conn = w["properties"]["Connection, ConnectionX, ConnectionY, ConnectionZ"]
    assert conn[0][:3] == ["Vessel", 0, 0] and conn[0][3] > spec.upper_flex_joint.pivot_z_m + 100
    assert conn[1] == [TOP, 0, 0, 0]
    top_kn = w["properties"]["StageMode, StageValue"][0][1]
    assert top_kn * 1000 == pytest.approx(equivalent_string_top_tension_n(spec))


def test_top_tension_adds_the_inner_barrel_weight_carried_by_the_vessel_in_the_lines_model():
    spec = _vertical()
    w = sum((s.mass_per_m_kg + spec.contents.density_kg_m3 * math.pi / 4 * s.bore_id_m ** 2) * s.length_m
            for s in spec.inner_barrel) * 9.80665
    assert equivalent_string_top_tension_n(spec) == pytest.approx(3.0e6 + w)


def test_equivalent_string_restrains_ring_yaw_through_riser_torsion():
    gen = build_generic_spec(_vertical())["generic"]
    riser = _line(gen, "Riser")
    assert riser["IncludeTorsion"] is True
    key = "ConnectionxBendingStiffness, ConnectionyBendingStiffness, ConnectionTwistingStiffness"
    assert riser[key][0] == ["Infinity", None, "Infinity"]
    assert riser[key][1][2] > 0 and riser[key][1][2] != "Infinity"
    assert _line(gen, "InnerBarrel")["IncludeTorsion"] is False


def test_g3_hand_reference_rejects_the_equivalent_string():
    """The tensioned-beam reference models the lines representation (inner barrel under self-weight only, a
    tensioner spring at the ring); it must not silently return periods for the equivalent string."""
    from digitalmodel.drilling_riser.global_model.hand_checks import reference_periods

    assert len(reference_periods(synthetic_spec(), n_modes=3)) == 3
    with pytest.raises(ValueError, match="lines"):
        reference_periods(_vertical(), n_modes=3)


def test_unknown_representation_is_rejected():
    t = synthetic_spec().tensioners.model_dump()
    t["representation"] = "cylinders"
    with pytest.raises(ValueError):
        Tensioners.model_validate(t)


def test_vertical_force_rating_check_uses_the_vertical_share_per_tensioner():
    d = synthetic_spec().model_dump()
    d["tensioners"]["representation"] = "vertical_force"
    d["tensioners"]["rated_tension_each_n"] = 3.0e6 / 6 * 0.99  # just below T / n
    with pytest.raises(ValueError, match="exceeds the rated"):
        RiserGlobalModelSpec.model_validate(d)
    d["tensioners"]["rated_tension_each_n"] = 3.0e6 / 6 * 1.01
    RiserGlobalModelSpec.model_validate(d)


@pytest.mark.solver
@pytest.mark.skipif(not orcaflex_api.available(), reason="OrcFxAPI not available")
def test_tensioner_vertical_sum_is_invariant_to_a_vessel_offset(tmp_path: Path):
    """Sheave (vessel frame) and ring attachment (ring frame) positions are taken in global coordinates:
    offsetting the vessel does not change the vertical sum (a local/global mix-up gave -18 % at 10 m)."""
    from digitalmodel.drilling_riser.global_model.build import write_model
    from digitalmodel.drilling_riser.global_model.orcaflex_run import load_and_solve_statics, tensioner_vertical_sum_n

    for off in (0.0, 10.0):
        d = synthetic_spec().model_dump()
        d["vessel_offset_m"] = (off, 0.0)
        s = RiserGlobalModelSpec.model_validate(d)
        m = load_and_solve_statics(write_model(s, tmp_path / f"o{off:g}") / "master.yml")
        assert tensioner_vertical_sum_n(m) == pytest.approx(3.0e6, rel=5e-3), off


@pytest.mark.solver
@pytest.mark.skipif(not orcaflex_api.available(), reason="OrcFxAPI not available")
def test_equivalent_string_matches_the_lines_tension_and_removes_the_lateral_tie(tmp_path: Path):
    from digitalmodel.drilling_riser.global_model.build import write_model
    from digitalmodel.drilling_riser.global_model.orcaflex_run import load_and_solve_statics, static_responses

    lines0, vert0 = synthetic_spec(), _vertical()
    r_l0 = static_responses(load_and_solve_statics(write_model(lines0, tmp_path / "l0") / "master.yml"), lines0)
    r_v0 = static_responses(load_and_solve_statics(write_model(vert0, tmp_path / "v0") / "master.yml"), vert0)
    # same tension below the ring at zero offset
    assert r_v0["te_top_n"] == pytest.approx(r_l0["te_top_n"], rel=2e-3)
    assert r_v0["te_bottom_n"] == pytest.approx(r_l0["te_bottom_n"], rel=2e-3)
    d = synthetic_spec().model_dump()
    d["vessel_offset_m"] = (3.0, 0.0)
    lines = RiserGlobalModelSpec.model_validate(d)
    vert = _vertical(vessel_offset_m=(3.0, 0.0))
    m_l = load_and_solve_statics(write_model(lines, tmp_path / "l") / "master.yml")
    m_v = load_and_solve_statics(write_model(vert, tmp_path / "v") / "master.yml")
    r_l, r_v = static_responses(m_l, lines), static_responses(m_v, vert)
    assert r_v["ufj_angle_deg"] > 0 and r_v["lfj_angle_deg"] > 0
    # the string top follows the vessel laterally
    ofx = orcaflex_api.api()
    assert m_v["InnerBarrel"].StaticResult("X", ofx.oeEndA) == pytest.approx(3.0, abs=1e-6)
    # the locked telescopic joint puts the top tension through the inner barrel (the string's own tie to the
    # vessel), where the lines model leaves the inner barrel nearly unloaded
    top = equivalent_string_top_tension_n(vert)
    assert m_v["InnerBarrel"].StaticResult("Wall tension", ofx.oeEndA) * 1000 == pytest.approx(top, rel=0.01)
    assert abs(m_l["InnerBarrel"].StaticResult("Effective tension", ofx.oeEndB)) * 1000 < 0.01 * top
    assert r_l["ufj_angle_deg"] > 0
