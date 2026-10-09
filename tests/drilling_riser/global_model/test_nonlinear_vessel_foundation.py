"""Nonlinear flex joints, tensioner rating, vessel RAOs, regular waves and a p-y foundation
(spec validation and text-model generation; no OrcFxAPI needed)."""

from __future__ import annotations

import math

import pytest

from digitalmodel.drilling_riser.global_model.build import build_generic_spec, winch_tension_n
from digitalmodel.drilling_riser.global_model.foundation import (
    PYCurve,
    interpolate_py,
    spring_stations,
)
from digitalmodel.drilling_riser.global_model.raos import parse_direction_grouped_rao_text
from digitalmodel.drilling_riser.global_model.spec import (
    DisplacementRAOs,
    Dynamics,
    FlexJoint,
    Foundation,
    RegularWave,
    RiserGlobalModelSpec,
    VesselMotion,
    flex_joint_from_secants,
)

from .conftest import _tube, synthetic_spec

KEY_STIFF = "ConnectionxBendingStiffness, ConnectionyBendingStiffness"


def _objects(gen, section):
    return {o["name"]: o for o in gen["generic"][section]}


# ------------------------------------------------------------------ flex joints
def test_flex_joint_from_secants_builds_moment_rotation_curve():
    fj = flex_joint_from_secants(pivot_z_m=10.0, secants_nm_per_deg=[(1.0, 1000.0), (15.0, 500.0)])
    assert fj.moment_rotation_deg_nm == [(0.0, 0.0), (1.0, 1000.0), (15.0, 7500.0)]
    # the small-rotation stiffness is the first-segment slope
    assert fj.rotational_stiffness_nm_per_rad == pytest.approx(1000.0 * 180.0 / math.pi)


def test_flex_joint_curve_must_be_monotonic():
    with pytest.raises(ValueError, match="increasing"):
        FlexJoint(pivot_z_m=0.0, rotational_stiffness_nm_per_rad=1000.0 * 180 / math.pi,
                  moment_rotation_deg_nm=[(0.0, 0.0), (1.0, 1000.0), (2.0, 900.0)])


def test_flex_joint_small_rotation_stiffness_must_match_the_curve():
    with pytest.raises(ValueError, match="first segment"):
        FlexJoint(pivot_z_m=0.0, rotational_stiffness_nm_per_rad=1.0e6,
                  moment_rotation_deg_nm=[(0.0, 0.0), (1.0, 1000.0), (10.0, 2000.0)])


def test_nonlinear_flex_joints_emit_bending_connection_stiffness_variable_data():
    d = synthetic_spec().model_dump()
    d["upper_flex_joint"] = flex_joint_from_secants(
        pivot_z_m=d["upper_flex_joint"]["pivot_z_m"], secants_nm_per_deg=[(1.0, 38e3), (15.0, 19e3)]).model_dump()
    spec = RiserGlobalModelSpec.model_validate(d)
    gen = build_generic_spec(spec)
    vd = {v["name"]: v for v in gen["generic"]["variable_data_sources"]}
    assert vd["UFJ stiffness"]["data_type"] == "Bendingconnectionstiffness"
    # OrcaFlex: independent = rotation (deg), dependent = moment (kN.m)
    assert vd["UFJ stiffness"]["entries"] == [[0.0, 0.0], [1.0, 38.0], [15.0, 285.0]]
    ib = _objects(gen, "lines")["InnerBarrel"]["properties"][KEY_STIFF]
    assert ib[0][0] == "UFJ stiffness"
    # the linear lower flex joint keeps its numeric kN.m/deg value
    riser = _objects(gen, "lines")["Riser"]["properties"][KEY_STIFF]
    assert isinstance(riser[1][0], float)


# ------------------------------------------------------------------ tensioners
def test_tensioner_rating_is_recorded_and_checked():
    d = synthetic_spec().model_dump()
    d["tensioners"]["rated_tension_each_n"] = 2.5e6
    spec = RiserGlobalModelSpec.model_validate(d)
    assert winch_tension_n(spec) < spec.tensioners.rated_tension_each_n
    d["tensioners"]["rated_tension_each_n"] = 0.4e6
    with pytest.raises(ValueError, match="rated"):
        RiserGlobalModelSpec.model_validate(d)


# ------------------------------------------------------------------ RAOs
RAO_TEXT = """\tSome export header
\t RAO for direction 0.00 deg
\t var\t\tSurge [m]\t\tSway [m] \t\tHeave [m]\t\tRoll [deg]\t\tPitch [deg]\t\tYaw [deg]\t
Heading\t Freq.\t\tAmpl\tPhase\tAmpl\tPhase\tAmpl\tPhase\tAmpl\tPhase\tAmpl\tPhase\tAmpl\tPhase
\t [rad/s]\t\t[-]\t[deg]\t[-]\t[deg]\t[-]\t[deg]\t[-]\t[deg]\t[-]\t[deg]\t[-]\t[deg]
\t---------
0.0\t0.1\t\t1.00E+00\t2.70E+02\t0.0E+00\t0.00E+00\t1.00E+00\t0.00E+00\t0.0E+00\t0.00E+00\t5.80E-02\t9.00E+01\t0.0E+00\t0.00E+00
0.0\t0.5\t\t4.00E-01\t2.80E+02\t0.0E+00\t0.00E+00\t3.00E-01\t1.00E+01\t0.0E+00\t0.00E+00\t8.00E-01\t1.00E+02\t0.0E+00\t0.00E+00
\t Some export header
\t RAO for direction 90.00 deg
Heading\t Freq.\t\tAmpl\tPhase\tAmpl\tPhase\tAmpl\tPhase\tAmpl\tPhase\tAmpl\tPhase\tAmpl\tPhase
90.0\t0.1\t\t0.0E+00\t0.00E+00\t1.00E+00\t2.70E+02\t1.00E+00\t0.00E+00\t5.80E-02\t2.70E+02\t0.0E+00\t0.00E+00\t0.0E+00\t0.00E+00
90.0\t0.5\t\t0.0E+00\t0.00E+00\t9.00E-01\t2.70E+02\t9.50E-01\t5.00E+00\t4.00E-01\t2.60E+02\t0.0E+00\t0.00E+00\t0.0E+00\t0.00E+00
"""


def test_parse_direction_grouped_rao_text():
    raos = parse_direction_grouped_rao_text(RAO_TEXT)
    assert raos["directions_deg"] == [0.0, 90.0]
    assert raos["frequencies_rad_s"] == [0.1, 0.5]
    assert raos["amplitude"][0][0] == pytest.approx([1.0, 0.0, 1.0, 0.0, 0.058, 0.0])
    assert raos["phase_deg"][1][1][1] == pytest.approx(270.0)


def test_parse_rejects_inconsistent_frequency_grids():
    bad = RAO_TEXT.replace("90.0\t0.5", "90.0\t0.6")
    with pytest.raises(ValueError, match="frequency"):
        parse_direction_grouped_rao_text(bad)


def _vessel_motion():
    raos = parse_direction_grouped_rao_text(RAO_TEXT)
    return VesselMotion(class_id="drillship-class-x", rao_origin_m=(0.0, 0.0, 2.9),
                        raos=DisplacementRAOs(phase_convention="leads", **raos))


def test_vessel_type_carries_displacement_raos_with_conventions():
    d = synthetic_spec().model_dump()
    d["vessel_motion"] = _vessel_motion().model_dump()
    spec = RiserGlobalModelSpec.model_validate(d)
    gen = build_generic_spec(spec)
    (vt,) = gen["generic"]["vessel_types"]
    assert vt["name"] == "drillship-class-x"
    p = vt["properties"]
    assert p["WavesReferredToBy"] == "frequency (rad/s)"
    assert p["RAOPhaseConvention"] == "leads" and p["RAOPhaseRelativeToConvention"] == "crest"
    assert p["RAOWaveUnit"] == "amplitude" and p["RAOResponseUnits"] == "degrees"
    assert p["PitchPositiveBow"] == "down" and p["RollPositiveStarboard"] == "down"
    assert p["Symmetry"] == "xz plane"  # directions 0-180 only
    disp = p["Draughts"][0]["DisplacementRAOs"]
    assert disp["RAOOrigin"] == [0.0, 0.0, 2.9]
    assert [r["RAODirection"] for r in disp["RAOs"]] == [0.0, 90.0]
    key = next(k for k in disp["RAOs"][0] if k.startswith("RAOPeriodOrFrequency"))
    assert disp["RAOs"][0][key][0] == pytest.approx([0.1, 1.0, 270.0, 0.0, 0.0, 1.0, 0.0,
                                                     0.0, 0.0, 0.058, 90.0, 0.0, 0.0])
    (v,) = gen["generic"]["vessels"]
    assert v["vessel_type"] == "drillship-class-x"
    assert v["properties"]["SuperimposedMotion"] == "RAOs + harmonics"


def test_no_vessel_motion_keeps_a_fixed_vessel():
    gen = build_generic_spec(synthetic_spec())
    (v,) = gen["generic"]["vessels"]
    assert v["properties"]["SuperimposedMotion"] == "None"


def test_regular_wave_and_dynamics_settings():
    d = synthetic_spec().model_dump()
    d["regular_wave"] = RegularWave(height_m=4.2, period_s=9.0, direction_deg=180.0).model_dump()
    d["dynamics"] = Dynamics(time_step_s=0.05, build_up_s=18.0, duration_s=90.0).model_dump()
    spec = RiserGlobalModelSpec.model_validate(d)
    gen = build_generic_spec(spec)
    w = gen["environment"]["waves"]
    assert (w["type"], w["height"], w["period"], w["direction"]) == ("airy", 4.2, 9.0, 180.0)
    assert gen["simulation"] == {"time_step": 0.05, "stages": [18.0, 90.0]}


# ------------------------------------------------------------------ foundation
def _curves():
    return [PYCurve(depth_m=0.0, y_m=[0.0, 0.01, 0.1], p_n_per_m=[0.0, 1000.0, 2000.0]),
            PYCurve(depth_m=10.0, y_m=[0.0, 0.01, 0.1], p_n_per_m=[0.0, 3000.0, 6000.0])]


def test_py_interpolates_linearly_in_depth():
    c = interpolate_py(_curves(), 5.0)
    assert c.y_m == [0.0, 0.01, 0.1]
    assert c.p_n_per_m == pytest.approx([0.0, 2000.0, 4000.0])
    # below the last curve the last curve holds
    assert interpolate_py(_curves(), 20.0).p_n_per_m == pytest.approx([0.0, 3000.0, 6000.0])


def test_py_interpolation_on_the_union_of_y_points():
    curves = [PYCurve(depth_m=0.0, y_m=[0.0, 0.02], p_n_per_m=[0.0, 100.0]),
              PYCurve(depth_m=2.0, y_m=[0.0, 0.01, 0.02], p_n_per_m=[0.0, 300.0, 300.0])]
    c = interpolate_py(curves, 1.0)
    assert c.y_m == [0.0, 0.01, 0.02]
    assert c.p_n_per_m == pytest.approx([0.0, 0.5 * (50.0 + 300.0), 0.5 * (100.0 + 300.0)])


def _foundation(seg=2.0):
    s1 = _tube("Conductor A", 10.0, 38.0, 1.5, mass=900.0, vol=0.73, drag_in=38.0, seg=seg, cd=0.0, ca=0.0)
    s2 = _tube("Conductor B", 10.0, 38.0, 1.0, mass=600.0, vol=0.73, drag_in=38.0, seg=seg, cd=0.0, ca=0.0)
    return Foundation(sections=[s1, s2], py_curves=_curves())


def test_spring_stations_are_the_nodes_with_tributary_lengths():
    st = spring_stations(_foundation())
    assert [s["depth_m"] for s in st] == pytest.approx([0, 2, 4, 6, 8, 10, 12, 14, 16, 18])
    assert sum(s["tributary_m"] for s in st) == pytest.approx(19.0)  # base node (fixed) carries no spring
    assert st[0]["tributary_m"] == pytest.approx(1.0) and st[1]["tributary_m"] == pytest.approx(2.0)


def test_foundation_sections_must_mesh_on_whole_segments():
    with pytest.raises(ValueError, match="whole number"):
        _foundation(seg=3.0)


def test_foundation_emits_conductor_line_links_and_no_seabed_stiffness():
    d = synthetic_spec().model_dump()
    d["foundation"] = _foundation().model_dump()
    spec = RiserGlobalModelSpec.model_validate(d)
    gen = build_generic_spec(spec)
    lines = _objects(gen, "lines")
    conn = "Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionAzimuth, ConnectionDeclination, ConnectionGamma, ConnectionReleaseStage, ConnectionzRelativeTo"
    c_ends = lines["Conductor"]["properties"][conn]
    assert c_ends[0][0] == "Fixed" and c_ends[0][3] == pytest.approx(spec.wellhead_datum_z_m - 20.0)
    s_ends = lines["Stack"]["properties"][conn]
    assert s_ends[0][0] == "Conductor" and s_ends[0][-1] == "End B"
    links = gen["generic"]["links"]
    assert len(links) == 2 * 10  # x and y spring per station
    first = links[0]["properties"]
    rows = first["SpringLength, SpringTension"]
    lengths = [r[0] for r in rows]
    assert lengths == sorted(lengths)
    mid = len(rows) // 2
    l0 = rows[mid][0]
    assert rows[mid][1] == 0.0
    # tension (kN) when the node moves away from the anchor = p x tributary length
    st = spring_stations(spec.foundation)[0]
    c = interpolate_py(spec.foundation.py_curves, st["depth_m"])
    j = next(i for i, r in enumerate(rows) if r[0] == pytest.approx(l0 + c.y_m[1]))
    assert rows[j][1] == pytest.approx(c.p_n_per_m[1] * st["tributary_m"] / 1000.0)
    assert gen["environment"]["seabed"]["stiffness"] == {"normal": 0.0, "shear": 0.0}


def test_fixed_base_model_is_unchanged_without_foundation():
    gen = build_generic_spec(synthetic_spec())
    assert "Conductor" not in _objects(gen, "lines")
    assert not gen["generic"].get("links")
