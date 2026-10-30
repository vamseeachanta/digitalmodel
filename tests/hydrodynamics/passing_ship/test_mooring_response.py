# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Independent spring-equilibrium checks, using synthetic inputs.

OpenStax University Physics Volume 1 (2016), sections 12.1 and 15.1:
force/moment balance and F=-kx. All numerical geometry, stiffnesses and
loads below are selected test inputs, not recommended engineering values.
The explicit absolute tolerances only bound floating-point roundoff.
"""

from itertools import permutations

import numpy as np
import pytest

from digitalmodel.hydrodynamics.passing_ship.force_time_history import ForceTimeHistory
from digitalmodel.hydrodynamics.passing_ship.input_schemas import (
    FenderSpec,
    HullSpec,
    MooredVesselSpec,
    MooringLineSpec,
    ResponseSettings,
)
from digitalmodel.hydrodynamics.passing_ship import mooring_response as response_module
from digitalmodel.hydrodynamics.passing_ship.mooring_response import solve_response


@pytest.fixture
def vessel():
    """Six opposed springs: K=diag(2000,4000,16000) in SI units.

    Surge pair at the origin; sway pairs at x=+2 and -2 m.
    Each spring is 1000 N/m, pretension 10 kN; opposing pretensions cancel.
    Kyy=4*1000; Krr=4*1000*2**2; Kxy=Kyr=0.
    """
    lines = []
    for label, x, y, nx, ny in [
        ("x+", 0, 0, 1, 0),
        ("x-", 0, 0, -1, 0),
        ("bow+", 2, 0, 0, 1),
        ("bow-", 2, 0, 0, -1),
        ("aft+", -2, 0, 0, 1),
        ("aft-", -2, 0, 0, -1),
    ]:
        lines.append(
            MooringLineSpec(
                line_id=label,
                line_type="synthetic",
                length=20,
                pretension=10,
                fairlead_x=x,
                fairlead_y=y,
                anchor_x=x - 20 * nx,
                anchor_y=y - 20 * ny,
                horizontal_stiffness_n_m=1000,
            )
        )
    return MooredVesselSpec(hull=HullSpec(loa=10, beam=2, draft=1), mooring_lines=lines)


@pytest.fixture
def settings():
    """User-selected validity bounds and numerical residual tolerances."""
    return ResponseSettings(
        max_translation_m=5,
        max_yaw_rad=0.2,
        force_tolerance_n=1e-7,
        moment_tolerance_nm=1e-7,
    )


def history(loads):
    loads = np.asarray(loads, dtype=float)
    return ForceTimeHistory(
        time=np.arange(len(loads), dtype=float),
        surge=loads[:, 0],
        sway=loads[:, 1],
        yaw=loads[:, 2],
        stagger=np.arange(len(loads), dtype=float),
        separation=100,
        speed=1,
    )


def test_hand_equilibrium_and_tension_units(vessel, settings):
    """q=(1000/2000,800/4000,160/16000)=(0.5,0.2,0.01).

    Bow+ extends 0.2+2*0.01=0.22 m, hence T=10000+220=10220 N.
    Bow- contracts equally. Initial T is converted from kN exactly once.
    """
    result = solve_response(vessel, history([[0, 0, 0], [1000, 800, 160]]), settings)
    np.testing.assert_allclose(
        result.motions, [[0, 0, 0], [0.5, 0.2, 0.01]], atol=1e-12
    )
    assert result.line_tensions_n["bow+"][1] == pytest.approx(10220)
    assert result.line_tensions_n["bow-"][1] == pytest.approx(9780)
    assert result.line_tensions_n["x+"][1] == pytest.approx(10500)
    assert np.max(np.abs(result.residuals)) < settings.force_tolerance_n


def test_motions_scale_with_force(vessel, settings):
    """A fixed K gives q(2F)=2q(F) by linear algebra."""
    result = solve_response(vessel, history([[100, 200, 30], [200, 400, 60]]), settings)
    np.testing.assert_allclose(result.motions[1], 2 * result.motions[0], atol=1e-12)


def test_fender_contact_and_gap(vessel, settings):
    """At Fy=800 N: before contact y=800/4000=0.2 m.

    Gap=0.1 m, kf=4000 N/m: 4000*y+4000*(y-0.1)=800,
    giving y=0.15 m, fender load=200 N. Negative sway opens contact.
    """
    vessel.fenders = [
        FenderSpec(
            fender_id="contact",
            x=0,
            y=0,
            normal_x=0,
            normal_y=1,
            gap_m=0.1,
            stiffness_n_m=4000,
        )
    ]
    result = solve_response(vessel, history([[0, 800, 0], [0, -800, 0]]), settings)
    np.testing.assert_allclose(result.motions[:, 1], [0.15, -0.2], atol=1e-12)
    np.testing.assert_allclose(result.fender_loads_n["contact"], [200, 0], atol=1e-9)


@pytest.mark.parametrize(
    "defect,match",
    [
        ("stiffness", "stiffness"),
        ("geometry", "reach"),
        ("singular", "restrain"),
        ("slack", "slack"),
        ("bounds", "translation"),
        ("nonfinite", "finite"),
        ("time", "time"),
        ("duplicate", "unique"),
    ],
)
def test_refuses_invalid_or_unqualified_response(vessel, settings, defect, match):
    loads = history([[0, 0, 0], [1000, 0, 0]])
    if defect == "stiffness":
        vessel.mooring_lines[0].horizontal_stiffness_n_m = None
    elif defect == "geometry":
        vessel.mooring_lines[0].anchor_x = 0
    elif defect == "singular":
        vessel.mooring_lines = vessel.mooring_lines[:2]
    elif defect == "slack":
        loads.surge[1] = 30000
        settings.max_translation_m = 50
    elif defect == "bounds":
        settings.max_translation_m = 0.1
    elif defect == "nonfinite":
        loads.surge[1] = np.nan
    elif defect == "time":
        loads.time[1] = loads.time[0]
    elif defect == "duplicate":
        vessel.mooring_lines[0].line_id = vessel.mooring_lines[1].line_id
    with pytest.raises(ValueError, match=match):
        solve_response(vessel, loads, settings)


@pytest.mark.parametrize("tolerance", [1e-7, 60.0])
def test_pretension_resultant_is_always_balanced(vessel, settings, tolerance):
    """B.T*T0=(50,0,0) N; qx=(0-50)/2000=-0.025 m.

    T(x+)=10050-25=10025 N; T(x-)=10000+25=10025 N.
    The residual tolerance does not remove a physical load from equilibrium.
    """
    vessel.mooring_lines[0].pretension = 10.05
    settings.force_tolerance_n = tolerance
    result = solve_response(vessel, history([[0, 0, 0]]), settings)
    np.testing.assert_allclose(result.motions, [[-0.025, 0, 0]], atol=1e-12)
    assert result.line_tensions_n["x+"][0] == pytest.approx(10025)
    assert result.line_tensions_n["x-"][0] == pytest.approx(10025)
    np.testing.assert_allclose(result.residuals, 0, atol=1e-9)


def test_pretension_moment_with_fender_is_balanced(vessel, settings):
    """Bow+ bias 100 N gives initial Fy=100 N, Mz=200 Nm.

    At q=(0,0.1,0.025), R=200 N: total Fy=100+400+200=700 N;
    total Mz=200+400+400=1000 Nm, T(bow+)=10100+150=10250 N.
    """
    vessel.mooring_lines[2].pretension = 10.1
    vessel.fenders = [FenderSpec(
        fender_id="contact", x=2, y=0, normal_x=0, normal_y=1,
        gap_m=0.1, stiffness_n_m=4000,
    )]
    result = solve_response(vessel, history([[0, 700, 1000]]), settings)
    np.testing.assert_allclose(result.motions, [[0, 0.1, 0.025]], atol=1e-12)
    assert result.fender_loads_n["contact"][0] == pytest.approx(200)
    assert result.line_tensions_n["bow+"][0] == pytest.approx(10250)
    np.testing.assert_allclose(result.residuals, 0, atol=1e-9)


def test_pretensioned_span_can_exceed_unstretched_length(vessel, settings):
    """The effective tangent input does not reconstruct stretched length."""
    vessel.mooring_lines[0].length = 19
    result = solve_response(vessel, history([[1000, 0, 0]]), settings)
    assert result.motions[0, 0] == pytest.approx(0.5)


def test_uncoupled_surge_period_uses_mass_and_stiffness(vessel, settings):
    """OpenStax (2016), 15.1: m=2 t=2000 kg, Kxx=2000 N/m.

    T=2*pi*sqrt(2000/2000)=6.283185307179586 s, excluding added mass.
    """
    vessel.hull.displacement_tonnes = 2
    result = solve_response(vessel, history([[0, 0, 0]]), settings)
    assert result.surge_natural_period_s == pytest.approx(6.283185307179586)


@pytest.mark.parametrize("mass", [float("nan"), float("inf")])
def test_period_estimate_refuses_nonfinite_mass(vessel, settings, mass):
    vessel.hull.displacement_tonnes = mass
    with pytest.raises(ValueError, match="finite"):
        solve_response(vessel, history([[0, 0, 0]]), settings)


def test_off_centre_fender_moment_and_normal(vessel, settings):
    """q=(0,0.1,0.025); contact at x=2 closes 0.1+2*0.025=0.15 m.

    Gap=0.1, kf=4000 gives R=200 N. Fy=4000*0.1+200=600 N;
    Mz=16000*0.025+2*200=800 Nm. A nonunit normal is normalized.
    """
    vessel.fenders = [
        FenderSpec(
            fender_id="bow",
            x=2,
            y=0,
            normal_x=0,
            normal_y=3,
            gap_m=0.1,
            stiffness_n_m=4000,
        )
    ]
    result = solve_response(vessel, history([[0, 600, 800]]), settings)
    np.testing.assert_allclose(result.motions, [[0, 0.1, 0.025]], atol=1e-12)
    assert result.fender_loads_n["bow"][0] == pytest.approx(200)


@pytest.mark.parametrize(
    "defect,match",
    [
        ("normal", "normal"),
        ("fender_id", "unique"),
        ("yaw", "yaw"),
        ("empty", "nonempty"),
        ("infinite_bound", "finite"),
    ],
)
def test_additional_response_limits(vessel, settings, defect, match):
    loads = history([[0, 0, 160]])
    if defect in ("normal", "fender_id"):
        fender = FenderSpec(
            fender_id="one",
            x=0,
            y=0,
            normal_x=0,
            normal_y=1,
            gap_m=0,
            stiffness_n_m=1000,
        )
        vessel.fenders = [fender]
        if defect == "normal":
            fender.normal_y = 0
        else:
            vessel.fenders.append(fender.model_copy())
    elif defect == "yaw":
        settings.max_yaw_rad = 0.001
    elif defect == "empty":
        loads.time = np.array([])
        loads.surge = loads.sway = loads.yaw = np.array([])
    elif defect == "infinite_bound":
        settings.max_translation_m = float("inf")
    with pytest.raises(ValueError, match=match):
        solve_response(vessel, loads, settings)


@pytest.fixture
def coupled_contact_case(vessel):
    """Synthetic geometry with a hand-worked third-contact-only equilibrium."""
    contacts = [
        FenderSpec(fender_id="contact-1", x=-3, y=9, normal_x=1, normal_y=17,
                   gap_m=0, stiffness_n_m=40000),
        FenderSpec(fender_id="contact-2", x=4, y=-6, normal_x=3, normal_y=8,
                   gap_m=0.2, stiffness_n_m=400000),
        FenderSpec(fender_id="contact-3", x=-10, y=-4, normal_x=1, normal_y=-26,
                   gap_m=0, stiffness_n_m=40000),
    ]
    vessel.fenders = contacts
    load = [1600 + 6560 / 677, -1080 - 170560 / 677, -464 + 1731840 / 677]
    return vessel, load


@pytest.mark.parametrize("order", list(permutations(range(3))))
def test_coupled_fenders_do_not_cycle(coupled_contact_case, settings, order):
    """OpenStax (2016), sections 12.1 and 15.1: sum forces and moments.

    At q=(0.8,-0.27,-0.029), only contact 3 closes:
    b3=(1,-26,264)/sqrt(677), b3.q=0.164/sqrt(677) m.
    R3=40000*0.164/sqrt(677)=6560/sqrt(677) N. Thus the load is
    (1600,-1080,-464)+(6560/677)*(1,-26,264) in N and N m.
    Bow+ tension=10000+1000*(-0.27+2*(-0.029))=9672 N;
    surge tensions=10000+/-1000*0.8=10800 and 9200 N.
    """
    vessel, load = coupled_contact_case
    vessel.fenders = [vessel.fenders[i] for i in order]
    result = solve_response(vessel, history([load, [0, 0, 0], load]), settings)
    np.testing.assert_allclose(result.motions, [[0.8, -0.27, -0.029], [0, 0, 0],
                                               [0.8, -0.27, -0.029]], atol=1e-10)
    np.testing.assert_allclose(result.fender_loads_n["contact-1"], 0, atol=1e-7)
    np.testing.assert_allclose(result.fender_loads_n["contact-2"], 0, atol=1e-7)
    np.testing.assert_allclose(result.fender_loads_n["contact-3"],
                               [6560 / np.sqrt(677), 0, 6560 / np.sqrt(677)], atol=1e-7)
    assert result.line_tensions_n["bow+"][0] == pytest.approx(9672)
    assert result.line_tensions_n["x+"][0] == pytest.approx(10800)
    assert result.line_tensions_n["x-"][0] == pytest.approx(9200)
    np.testing.assert_allclose(result.residuals, 0, atol=1e-7)


@pytest.mark.parametrize("gap", [0.1, 0.125])
def test_contact_balance_with_loose_residual_tolerance(vessel, settings, gap):
    """Fy=500 N gives y=0.125 m; the 0.125 m gap has zero reaction.

    For gap=0.1 m, 4000*y+4000*(y-0.1)=500 gives y=0.1125 m,
    R=50 N. A loose supplied tolerance must not omit this contact load.
    """
    settings.force_tolerance_n = 1000
    vessel.fenders = [FenderSpec(
        fender_id="contact", x=0, y=0, normal_x=0, normal_y=1,
        gap_m=gap, stiffness_n_m=4000,
    )]
    result = solve_response(vessel, history([[0, 500, 0]]), settings)
    expected_y, expected_reaction = (0.1125, 50) if gap == 0.1 else (0.125, 0)
    assert result.motions[0, 1] == pytest.approx(expected_y, abs=1e-12)
    assert result.fender_loads_n["contact"][0] == pytest.approx(expected_reaction)
    np.testing.assert_allclose(result.residuals, 0, atol=1e-9)


@pytest.mark.parametrize(
    "budget,message", [("_CONTACT_ITERATIONS", "iteration limit"),
                       ("_CONTACT_BACKTRACKS", "line search")],
)
def test_contact_budget_exhaustion_refuses_partial_result(
    coupled_contact_case, settings, monkeypatch, budget, message,
):
    vessel, load = coupled_contact_case
    monkeypatch.setattr(response_module, budget, 0)
    with pytest.raises(ValueError, match=message):
        solve_response(vessel, history([load]), settings)


@pytest.mark.parametrize("order", list(permutations(range(3))))
def test_decimal_grazing_contact_is_balanced(vessel, settings, order):
    """Fy=1120 N and Mz=-1440 N m give y=0.28 m and yaw=-0.09 rad.

    The off-centre contact closure is 0.28+2*(-0.09)-0.1=0 m by hand.
    The decimal gap can nevertheless change sign at floating-point roundoff.
    """
    contacts = [
        FenderSpec(fender_id="touching", x=2, y=0, normal_x=0, normal_y=1,
                   gap_m=0.1, stiffness_n_m=40000),
        FenderSpec(fender_id="open-x", x=0, y=0, normal_x=1, normal_y=0,
                   gap_m=2, stiffness_n_m=4000),
        FenderSpec(fender_id="open-y", x=0, y=0, normal_x=0, normal_y=-1,
                   gap_m=2, stiffness_n_m=4000),
    ]
    vessel.fenders = [contacts[i] for i in order]
    result = solve_response(vessel, history([[0, 1120, -1440]]), settings)
    np.testing.assert_allclose(result.motions, [[0, 0.28, -0.09]], atol=1e-12)
    for reaction in result.fender_loads_n.values():
        np.testing.assert_allclose(reaction, 0, atol=1e-9)
    np.testing.assert_allclose(result.residuals, 0, atol=1e-9)
