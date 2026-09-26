"""Spec validation and OrcaFlex text-model generation (no OrcFxAPI needed)."""

from __future__ import annotations

import math
from pathlib import Path

import pytest
import yaml

from digitalmodel.drilling_riser.global_model.build import (
    build_generic_spec,
    hydrodynamic_od_m,
    winch_tension_n,
    write_model,
)
from digitalmodel.drilling_riser.global_model.spec import RiserGlobalModelSpec, tube_section

from .conftest import synthetic_spec


def test_tube_section_properties():
    p = tube_section(od_m=0.5334, wall_m=0.0254, youngs_modulus_pa=207e9)
    a = math.pi / 4 * (0.5334**2 - 0.4826**2)
    i = math.pi / 64 * (0.5334**4 - 0.4826**4)
    assert p["id_m"] == pytest.approx(0.4826)
    assert p["ea_n"] == pytest.approx(207e9 * a)
    assert p["ei_nm2"] == pytest.approx(207e9 * i)


def test_spec_rejects_length_mismatch_at_lower_flex_joint(spec):
    data = spec.model_dump()
    data["lower_flex_joint"]["pivot_z_m"] += 0.5
    with pytest.raises(ValueError, match="lower flex-joint pivot"):
        RiserGlobalModelSpec.model_validate(data)


def test_spec_rejects_stack_not_reaching_datum(spec):
    data = spec.model_dump()
    data["wellhead_datum_z_m"] -= 1.0
    with pytest.raises(ValueError, match="wellhead datum"):
        RiserGlobalModelSpec.model_validate(data)


def test_spec_rejects_bore_larger_than_buoyancy_diameter(spec):
    data = spec.model_dump()
    data["riser"][1]["displaced_volume_per_m_m3"] = 0.05
    with pytest.raises(ValueError, match="bore"):
        RiserGlobalModelSpec.model_validate(data)


def test_hydrodynamic_od_reproduces_displaced_volume():
    assert math.pi / 4 * hydrodynamic_od_m(1.26) ** 2 == pytest.approx(1.26)


def test_winch_tensions_sum_to_the_vertical_target(spec):
    t = winch_tension_n(spec)
    dx = spec.tensioners.sheave_radius_m - spec.tensioners.ring_attach_radius_m
    dz = spec.tensioners.sheave_z_m - spec.tension_ring.z_static_m
    cos_phi = dz / math.hypot(dx, dz)
    assert spec.tensioners.count * t * cos_phi == pytest.approx(spec.tensioners.total_vertical_tension_n)


def _objects(gen, section):
    return {o["name"]: o for o in gen["generic"][section]}


def test_line_types_carry_units_in_te_and_kn(spec):
    gen = build_generic_spec(spec)
    lts = _objects(gen, "line_types")
    s = spec.riser[1]
    lt = lts[s.name]
    assert lt["outer_diameter"] == pytest.approx(hydrodynamic_od_m(s.displaced_volume_per_m_m3))
    assert lt["inner_diameter"] == pytest.approx(s.bore_id_m)
    assert lt["mass_per_length"] == pytest.approx(s.mass_per_m_kg / 1000.0)
    assert lt["axial_stiffness"] == pytest.approx(s.ea_n / 1000.0)
    assert lt["bending_stiffness"][0] == pytest.approx(s.ei_nm2 / 1000.0)
    assert lt["properties"]["NormalDragLiftDiameter"] == pytest.approx(s.drag_diameter_m)
    assert lt["properties"]["Cd"][0] == pytest.approx(s.cd_normal)
    assert lt["properties"]["Ca"][0] == pytest.approx(s.ca_normal)


def test_topology_ring_riser_stack(spec):
    gen = build_generic_spec(spec)
    lines = _objects(gen, "lines")
    assert set(lines) == {"InnerBarrel", "Riser", "Stack"}
    conn = "Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionAzimuth, ConnectionDeclination, ConnectionGamma, ConnectionReleaseStage, ConnectionzRelativeTo"
    riser_ends = lines["Riser"]["properties"][conn]
    assert riser_ends[0][0] == "TensionRing"
    assert riser_ends[1][0] == "Stack" and riser_ends[1][-1] == "End B"
    stack_ends = lines["Stack"]["properties"][conn]
    assert stack_ends[0][0] == "Fixed"
    assert stack_ends[0][3] == pytest.approx(spec.wellhead_datum_z_m)
    ib_ends = lines["InnerBarrel"]["properties"][conn]
    assert ib_ends[0][0] == "Vessel" and ib_ends[0][3] == pytest.approx(spec.upper_flex_joint.pivot_z_m)
    assert ib_ends[1][0] == "SlipJoint"


def test_slip_joint_is_a_constraint_free_only_along_z(spec):
    # The telescopic joint passes shear and moment into the tension ring but no axial load.
    gen = build_generic_spec(spec)
    (c,) = gen["generic"]["constraints"]
    assert c["name"] == "SlipJoint" and c["in_frame_connection"] == "TensionRing"
    assert c["constraint_type"] == "Calculated DOFs"
    rows = c["properties"]["DOFFree, DOFInitialValue"]
    assert [r[0] for r in rows] == [False, False, True, False, False, False]
    assert rows[2][1] == pytest.approx(-(spec.tension_ring.z_static_m - spec.ring_z_geometric_m))
    # a small axial spring keeps statics on the physical branch; it carries ~k x stretch (kN/m in the model)
    assert c["properties"]["TranslationalStiffness"] == pytest.approx(spec.slip_joint_axial_stiffness_n_per_m / 1000.0)


def test_flex_joint_stiffness_in_kn_m_per_degree(spec):
    gen = build_generic_spec(spec)
    lines = _objects(gen, "lines")
    key = "ConnectionxBendingStiffness, ConnectionyBendingStiffness"
    k_lfj = lines["Riser"]["properties"][key][1][0]
    k_ufj = lines["InnerBarrel"]["properties"][key][0][0]
    per_deg = math.pi / 180.0 / 1000.0
    assert k_lfj == pytest.approx(spec.lower_flex_joint.rotational_stiffness_nm_per_rad * per_deg)
    assert k_ufj == pytest.approx(spec.upper_flex_joint.rotational_stiffness_nm_per_rad * per_deg)


def test_winches_are_specified_tension_from_vessel_to_ring(spec):
    gen = build_generic_spec(spec)
    winches = gen["generic"]["winches"]
    assert len(winches) == spec.tensioners.count
    for w in winches:
        conn = w["properties"]["Connection, ConnectionX, ConnectionY, ConnectionZ"]
        assert conn[0][0] == "Vessel" and conn[1][0] == "TensionRing"
        rows = w["properties"]["StageMode, StageValue"]
        assert rows[0][0] == "Specified tension"
        assert rows[0][1] == pytest.approx(winch_tension_n(spec) / 1000.0)


def test_riser_contents_only_on_riser_and_inner_barrel(spec):
    gen = build_generic_spec(spec)
    lines = _objects(gen, "lines")
    assert lines["Riser"]["properties"]["ContentsDensity"] == pytest.approx(1.5)
    assert lines["Stack"]["properties"]["ContentsDensity"] == 0


def test_write_model_is_text_yaml_without_user_or_machine(tmp_path: Path, spec):
    out = write_model(spec, tmp_path)
    master = out / "master.yml"
    assert master.exists()
    text = "\n".join(p.read_text(encoding="utf-8") for p in out.rglob("*.yml"))
    assert "User:" not in text and "Machine:" not in text
    docs = [yaml.safe_load(p.read_text(encoding="utf-8")) for p in (out / "includes").glob("*.yml")]
    assert any("Lines" in (d or {}) for d in docs)


def test_build_is_deterministic(tmp_path: Path):
    a = write_model(synthetic_spec(), tmp_path / "a")
    b = write_model(synthetic_spec(), tmp_path / "b")
    for p in sorted(a.rglob("*.yml")):
        q = b / p.relative_to(a)
        assert p.read_bytes() == q.read_bytes(), p.name


def test_stack_line_runs_up_from_the_datum(spec):
    gen = build_generic_spec(spec)
    stack = _objects(gen, "lines")["Stack"]["properties"]["LineType, Length, TargetSegmentLength"]
    names = [row[0] for row in stack]
    assert [n for i, n in enumerate(names) if i == 0 or n != names[i - 1]] == [s.name for s in reversed(spec.stack)]


def test_stack_has_no_seabed_contact(spec):
    # The stack stands on the wellhead datum at the mudline; seabed contact on its nodes is
    # not physical and would carry part of its weight into the seabed.
    gen = build_generic_spec(spec)
    lts = _objects(gen, "line_types")
    for s in spec.stack:
        assert lts[s.name]["properties"]["OuterContactDiameter"] == pytest.approx(1.0e-3)
    for s in spec.riser:
        assert lts[s.name]["properties"].get("OuterContactDiameter") is None

def test_initial_state_is_the_expected_tensioned_state(spec):
    # Statics starts from the ring at its expected tensioned elevation and the inner-barrel end
    # at its unstretched position (constraint DOF = -ring rise), so no line starts overstretched.
    gen = build_generic_spec(spec)
    (ring,) = gen["generic"]["buoys_6d"]
    assert ring["initial_position"][2] == pytest.approx(spec.tension_ring.z_static_m)
