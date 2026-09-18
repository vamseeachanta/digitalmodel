"""Original shell model definition tests; not normative acceptance tests."""
import math
import pytest

from digitalmodel.asset_integrity.assessment import example_vessel_fea as fe


def test_closed_end_load_and_minimal_restraints():
    model = fe.build_model("C", 1.5, local_pitch=50, far_pitch=150)
    for end in ("left", "right"):
        resultant = sum(force for _, force in model["end_forces"][end])
        sign = -1 if end == "left" else 1
        assert resultant == pytest.approx(sign * 1.5 * math.pi * 1000**2)
    assert len(model["reference_nodes"]) == 3
    lookup = {n[0]:n[1:] for n in model["nodes"]}
    for node, angle in model["reference_nodes"]:
        _,y,z = lookup[node]
        assert -math.sin(angle)*y + math.cos(angle)*z == pytest.approx(0,abs=1e-9)
    deck = fe.render_deck(model)
    assert deck.count("CE,NEXT") == 2
    assert "D,1,UZ,0" in deck
    assert deck.count(",UX,0") == 3
    assert "SECOFFSET,BOT" in deck
    assert "SFE,ALL,1,PRES,,1.5" in deck
    assert "KEYOPT,1,10,1" in deck
    assert "NROPT,UNSYM" in deck
    assert model["basis"]["active_constitutive_model"] == "linear elastic; no plasticity activated"
    assert "SHELL,MID" in deck and "SHELL,TOP" in deck and "SHELL,BOT" in deck


def test_connectivity_normals_and_assessed_geometry():
    model = fe.build_model("D", 0.8, local_pitch=50, far_pitch=150)
    nodes = {n[0]: n[1:] for n in model["nodes"]}
    for item in model["elements"]:
        p, q, _, r = [nodes[n] for n in item["nodes"]]
        a, b = [q[i]-p[i] for i in range(3)], [r[i]-p[i] for i in range(3)]
        cross = [a[1]*b[2]-a[2]*b[1], a[2]*b[0]-a[0]*b[2], a[0]*b[1]-a[1]*b[0]]
        assert cross[1]*p[1] + cross[2]*p[2] > 0
    assert min(e["thickness_mm"] for e in model["elements"]) == pytest.approx(4.3)
    assert max(e["thickness_mm"] for e in model["elements"]) == pytest.approx(15.3)
    repaired = fe.build_model("repair", 1.5, local_pitch=50, far_pitch=150)
    assert all(e["thickness_mm"] == pytest.approx(15.3) for e in repaired["elements"])
    assert repaired["basis"]["physical_repair_qualification"] == "NOT EVALUATED"


def test_end_thrust_acts_at_annular_traction_centroid():
    model = fe.build_model("intact",1.5,local_pitch=50,far_pitch=150)
    nodes = {n[0]:n[1:] for n in model["nodes"]}
    inner,outer = 1000,1015.3
    offset = 2/3*(outer**3-inner**3)/(outer**2-inner**2)-inner
    for end in ("left","right"):
        forces = dict(model["end_forces"][end])
        for node,my,mz in model["end_moments"][end]:
            _,y,z = nodes[node]
            assert my == pytest.approx(forces[node]*offset*z/inner,abs=1e-7)
            assert mz == pytest.approx(-forces[node]*offset*y/inner,abs=1e-7)
    assert "traction-centroid" in model["basis"]["end_thrust_basis"]


@pytest.mark.parametrize("case,pressure,pitch", [("bogus",1.5,25),("C",0,25),("D",1.5,0),
                                                ("D",float("nan"),25)])
def test_invalid_inputs(case, pressure, pitch):
    with pytest.raises(ValueError):
        fe.build_model(case, pressure, local_pitch=pitch)


def test_axis_resource_bound_precedes_coordinate_allocation(monkeypatch):
    original = fe.math.ceil
    monkeypatch.setattr(fe.math,"ceil",lambda _: 200001)
    with pytest.raises(ValueError,match="axis"):
        fe._axis(6000,None,0,25,100)
    monkeypatch.setattr(fe.math,"ceil",original)


def test_export_stress_components_and_provenance(tmp_path):
    target = fe.write_case(tmp_path, "c-test", "C", 1.5, local_pitch=50, far_pitch=150)
    deck = (target / "vessel.inp").read_text()
    assert "RSYS,SOLU" in deck
    assert all(f"ETABLE,V{i},S,{name}" in deck for i,name in enumerate(("X","Y","Z","XY","YZ","XZ"),1))
    assert "*CFOPEN,stress_mid,csv" in deck
    assert "/OUTPUT,stress_mid_nodes,txt\nPRESOL,S,COMP\n/OUTPUT" in deck
    assert deck.count("PRESOL,S,COMP") == 1
    assert "*CFOPEN,stress_mid,csv\n*VLEN,1\n*VWRITE" in deck
    assert f"*VLEN,{len(fe.build_model('C',1.5,local_pitch=50,far_pitch=150)['nodes'])}\n*DIM,NIDS" in deck
    assert "VSL_DONE" in deck
    assert "*VWRITE,'element_id,sx,sy,sz,sxy,syz,sxz'\n%C" in deck
    with pytest.raises(FileExistsError):
        fe.write_case(tmp_path, "c-test", "C", 1.5)
    with pytest.raises(ValueError):
        fe.write_case(tmp_path, "../escape", "C", 1.5)
