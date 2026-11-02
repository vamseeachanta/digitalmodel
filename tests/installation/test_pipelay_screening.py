"""Synthetic installation cases; no field data or native solver oracle."""

import math
from dataclasses import asdict

import pytest

from digitalmodel.installation.pipelay.screening import (
    ar_cable_tension,
    displacement_controlled_check,
    lay_geometry,
    pressure_screen,
)


def pressure_inputs():
    return dict(
        outer_diameter_m=0.5, wall_thickness_m=0.02,
        yield_strength_pa=400e6, youngs_modulus_pa=200e9,
        poisson_ratio=0.3, fabrication_factor=1.0, ovality=0.005,
        external_pressure_pa=2e6, minimum_internal_pressure_pa=0.0,
        gamma_m=1.15, gamma_sc=1.1,
    )


def test_worked_s_lay_example():
    # Independent hand check: w=1000 N/m, h=100 m, H=200000 N.
    # a=H/w=200 m; T=H+wh=300000 N; V=sqrt(T^2-H^2)
    # =223606.797750 N; L=V/w=223.606797750 m.
    # x=a*acosh(1+h/a)=200*acosh(1.5)=192.484730024 m.
    # eps_sag=D/(2a)=0.5/400=0.00125; eps_over=0.5/500=0.001.
    lay = lay_geometry(
        method="S-Lay", outer_diameter_m=0.5, suspended_height_m=100.0,
        submerged_weight_n_per_m=1000.0, horizontal_tension_n=200000.0,
        stinger_radius_m=250.0,
    )
    assert lay.top_tension_n == pytest.approx(300000)
    assert lay.top_vertical_force_n == pytest.approx(223606.797750)
    assert lay.suspended_length_m == pytest.approx(223.606797750)
    assert lay.touchdown_offset_m == pytest.approx(192.484730024)
    assert lay.sagbend_radius_m == pytest.approx(200)
    assert lay.sagbend_strain == pytest.approx(0.00125)
    assert lay.overbend_strain == pytest.approx(0.001)
    assert lay.departure_angle_from_horizontal_deg == pytest.approx(48.189685104)
    # Pel=2*200e9*(.02/.5)^3/(1-.3^2)=28.131868132 MPa.
    # Pp=2*400e6*.02/.5=32 MPa. Dividing the collapse cubic by
    # Pel*Pp^2 gives (1-p/Pel)*(1-(p/Pp)^2)-p/Pp*.005*25=0.
    # At 22.943424742 MPa this is -8.4e-12 (dimensionless).
    # Ppr=35*400e6*.04^2.5=4.48 MPa; g=1.15*1.1=1.265.
    pressure = pressure_screen(**pressure_inputs())
    assert pressure.elastic_collapse_pa == pytest.approx(28.131868132e6)
    assert pressure.plastic_collapse_pa == pytest.approx(32e6)
    assert pressure.collapse_pa == pytest.approx(22.943424742e6)
    assert pressure.propagation_pa == pytest.approx(4.48e6)
    assert pressure.collapse_utilisation == pytest.approx(0.11027124453)
    assert pressure.propagation_utilisation == pytest.approx(0.56473214286)
    assert pressure.collapse_passes and pressure.propagation_passes
    # Illustrative externally supplied characteristic strain .01 and exponent .8.
    # No direct axial strain relief: design compression=1.2*eps_sag=.0015.
    # U=(.0015*2/.01)^.8 + 2e6*1.265/22.943424742e6
    #  = .38167789096 + .11027124453 = .49194913549.
    check = displacement_controlled_check(
        design_compressive_strain=1.2 * lay.sagbend_strain,
        characteristic_strain_capacity=0.01, gamma_e=2.0,
        net_external_pressure_pa=pressure.net_external_pressure_pa,
        collapse_pressure_pa=pressure.collapse_pa,
        gamma_m=1.15, gamma_sc=1.1, interaction_exponent=0.8,
        displacement_controlled=True,
    )
    assert check.utilisation == pytest.approx(0.49194913549)
    assert check.passes
    # Separate A&R configuration: H_end=30 kN, V_end=40 kN,
    # cable wet weight 100 N/m * 100 m =10 kN, head=0 N.
    # T_static=sqrt(30^2+50^2)=58.309518948 kN; DAF=1.2 ->69.971422738.
    ar = ar_cable_tension(
        horizontal_end_force_n=30000, downward_end_force_n=40000,
        suspended_cable_length_m=100, cable_submerged_weight_n_per_m=100,
        submerged_head_weight_n=0, dynamic_factor=1.2,
    )
    assert ar.static_top_tension_n == pytest.approx(58309.518948)
    assert ar.design_top_tension_n == pytest.approx(69971.422738)
    assert ar.sources and pressure.sources and check.sources and lay.sources


def test_j_lay_uses_same_free_span_without_stinger():
    lay = lay_geometry(
        method="J-Lay", outer_diameter_m=0.5, suspended_height_m=1000,
        submerged_weight_n_per_m=1000, horizontal_tension_n=50000,
    )
    assert lay.top_tension_n == pytest.approx(1050000)
    assert lay.overbend_strain is None  # Not evaluated, not a true zero.
    assert lay.sagbend_strain == pytest.approx(0.005)
    # a*ln((a+h)/a + sqrt(((a+h)/a)^2-1))=50*ln(21+sqrt(440)).
    assert lay.touchdown_offset_m == pytest.approx(186.855112110)
    # Force closure independently of the geometry formula.
    assert math.hypot(50000, 1000 * lay.suspended_length_m) == pytest.approx(
        lay.top_tension_n
    )


@pytest.mark.parametrize("method,radius", [("S-Lay", None), ("J-Lay", 250), ("Reel-Lay", None)])
def test_method_and_stinger_contract(method, radius):
    with pytest.raises(ValueError):
        lay_geometry(method=method, outer_diameter_m=0.5, suspended_height_m=100,
                     submerged_weight_n_per_m=1000, horizontal_tension_n=200000,
                     stinger_radius_m=radius)


@pytest.mark.parametrize("key", ["outer_diameter_m", "suspended_height_m", "submerged_weight_n_per_m", "horizontal_tension_n", "stinger_radius_m"])
@pytest.mark.parametrize("bad", [0, -1, math.nan, math.inf, -math.inf])
def test_geometry_rejects_invalid_input(key, bad):
    args = dict(method="S-Lay", outer_diameter_m=0.5, suspended_height_m=100,
                submerged_weight_n_per_m=1000, horizontal_tension_n=200000,
                stinger_radius_m=250)
    args[key] = bad
    with pytest.raises(ValueError, match=key):
        lay_geometry(**args)


@pytest.mark.parametrize("key", list(pressure_inputs()))
@pytest.mark.parametrize("bad", [math.nan, math.inf, -math.inf])
def test_pressure_rejects_nonfinite(key, bad):
    args = pressure_inputs()
    args[key] = bad
    with pytest.raises(ValueError, match=key):
        pressure_screen(**args)


@pytest.mark.parametrize("diameter_to_wall", [15, 45, 10, 50])
def test_propagation_applicability_is_enforced(diameter_to_wall):
    args = pressure_inputs()
    args["wall_thickness_m"] = args["outer_diameter_m"] / diameter_to_wall
    with pytest.raises(ValueError, match="15 < D/t < 45"):
        pressure_screen(**args)


@pytest.mark.parametrize("key,bad", [("wall_thickness_m", 0), ("wall_thickness_m", 0.3),
    ("yield_strength_pa", 0), ("youngs_modulus_pa", -1), ("poisson_ratio", 0.5),
    ("poisson_ratio", -1), ("fabrication_factor", 0), ("fabrication_factor", 1.1),
    ("ovality", -0.1), ("ovality", 1), ("external_pressure_pa", -1),
    ("minimum_internal_pressure_pa", -1), ("gamma_m", 0.9), ("gamma_sc", 0.9)])
def test_pressure_rejects_invalid_domain(key, bad):
    args = pressure_inputs()
    args[key] = bad
    with pytest.raises(ValueError):
        pressure_screen(**args)


def test_ovality_floor_and_increase_are_reported():
    args = pressure_inputs()
    args["ovality"] = 0.0
    floor = pressure_screen(**args)
    assert floor.applied_ovality == 0.005
    args["ovality"] = 0.02
    worse = pressure_screen(**args)
    assert worse.collapse_pa < floor.collapse_pa
    assert worse.collapse_utilisation > floor.collapse_utilisation


def test_pressure_failure_and_equal_pressure():
    args = pressure_inputs()
    args["external_pressure_pa"] = 25e6
    failure = pressure_screen(**args)
    assert not failure.collapse_passes and not failure.propagation_passes
    args["minimum_internal_pressure_pa"] = 25e6
    equal = pressure_screen(**args)
    assert equal.collapse_utilisation == equal.propagation_utilisation == 0
    args["minimum_internal_pressure_pa"] = 26e6
    with pytest.raises(ValueError, match="internal overpressure"):
        pressure_screen(**args)


def dcc_inputs():
    return dict(design_compressive_strain=0.005,
                characteristic_strain_capacity=0.01, gamma_e=2,
                net_external_pressure_pa=0, collapse_pressure_pa=20e6,
                gamma_m=1, gamma_sc=1, interaction_exponent=0.8,
                displacement_controlled=True)


def test_dcc_boundary_and_pressure_interaction():
    args = dcc_inputs()
    assert displacement_controlled_check(**args).utilisation == 1
    assert displacement_controlled_check(**args).passes
    args["net_external_pressure_pa"] = 1e6
    assert not displacement_controlled_check(**args).passes
    args["design_compressive_strain"] = 0
    args["net_external_pressure_pa"] = 20e6
    assert displacement_controlled_check(**args).utilisation == 1


@pytest.mark.parametrize("key", [k for k in dcc_inputs() if k != "displacement_controlled"])
@pytest.mark.parametrize("bad", [math.nan, math.inf, -math.inf, -1])
def test_dcc_invalid_values(key, bad):
    args = dcc_inputs()
    args[key] = bad
    with pytest.raises(ValueError, match=key):
        displacement_controlled_check(**args)


@pytest.mark.parametrize("flag", [False, None, "yes", 1])
def test_dcc_refuses_unconfirmed_control(flag):
    args = dcc_inputs()
    args["displacement_controlled"] = flag
    with pytest.raises(ValueError, match="displacement_controlled"):
        displacement_controlled_check(**args)


def test_ar_weightless_cable_and_negative_values():
    args = dict(horizontal_end_force_n=30000, downward_end_force_n=40000,
                suspended_cable_length_m=100, cable_submerged_weight_n_per_m=0,
                submerged_head_weight_n=0, dynamic_factor=1)
    assert ar_cable_tension(**args).static_top_tension_n == 50000
    for key in args:
        for bad in [-1, math.nan, math.inf]:
            invalid = {**args, key: bad}
            with pytest.raises(ValueError, match=key):
                ar_cable_tension(**invalid)
    with pytest.raises(ValueError, match="dynamic_factor"):
        ar_cable_tension(**{**args, "dynamic_factor": 0.9})


def test_pressure_units_scale_and_length_units_cancel():
    # Pa -> MPa in the EXISTING consistent-unit helpers: check adapter uses
    # Pa and m throughout, without hidden kN or mm factors.
    from digitalmodel.code_checks.dnv_os_f101 import collapse_pressure, propagation_pressure
    si = pressure_screen(**pressure_inputs())
    assert si.collapse_pa / 1e6 == pytest.approx(collapse_pressure(
        500, 20, 400, 200000, 0.005, 0.3, 1))
    assert si.propagation_pa / 1e6 == pytest.approx(propagation_pressure(500, 20, 400, 1))


def test_result_is_strict_json_serialisable():
    import json
    json.dumps(asdict(pressure_screen(**pressure_inputs())), allow_nan=False)


def test_catenary_length_scaling_and_tension_trend():
    args = dict(method="S-Lay", outer_diameter_m=0.5, suspended_height_m=100,
                submerged_weight_n_per_m=1000, horizontal_tension_n=200000,
                stinger_radius_m=250)
    base = lay_geometry(**args)
    more_tension = lay_geometry(**{**args, "horizontal_tension_n": 400000})
    assert more_tension.sagbend_strain == pytest.approx(base.sagbend_strain / 2)
    assert more_tension.touchdown_offset_m > base.touchdown_offset_m
    # Geometric similitude: double all lengths, double H at fixed w.
    scaled = lay_geometry(**{**args, "outer_diameter_m": 1, "suspended_height_m": 200,
                             "horizontal_tension_n": 400000, "stinger_radius_m": 500})
    assert scaled.sagbend_strain == base.sagbend_strain
    assert scaled.overbend_strain == base.overbend_strain
    assert scaled.touchdown_offset_m == pytest.approx(2 * base.touchdown_offset_m)


@pytest.mark.parametrize("key,bad", [("characteristic_strain_capacity", 0),
    ("characteristic_strain_capacity", 1), ("design_compressive_strain", 1),
    ("interaction_exponent", 0), ("gamma_e", 0.9), ("gamma_m", 0.9),
    ("gamma_sc", 0.9), ("collapse_pressure_pa", 0)])
def test_dcc_zero_and_fraction_contract(key, bad):
    with pytest.raises(ValueError):
        displacement_controlled_check(**{**dcc_inputs(), key: bad})


def test_extreme_finite_values_fail_before_nonfinite_output():
    with pytest.raises(ValueError):
        lay_geometry(method="J-Lay", outer_diameter_m=0.5,
                     suspended_height_m=1e308, submerged_weight_n_per_m=1e308,
                     horizontal_tension_n=1e308)
    with pytest.raises(ValueError):
        pressure_screen(**{**pressure_inputs(), "youngs_modulus_pa": 1e308})
    with pytest.raises(ValueError):
        displacement_controlled_check(**{**dcc_inputs(),
            "characteristic_strain_capacity": 1e-300, "interaction_exponent": 2})
    with pytest.raises(ValueError):
        ar_cable_tension(horizontal_end_force_n=1e308, downward_end_force_n=1e308,
                         suspended_cable_length_m=100, cable_submerged_weight_n_per_m=0,
                         submerged_head_weight_n=0, dynamic_factor=2)
