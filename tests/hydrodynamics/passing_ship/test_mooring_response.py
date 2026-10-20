# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Independent spring-equilibrium checks, using synthetic inputs.

OpenStax University Physics Volume 1 (2016), sections 12.1 and 15.1:
force/moment balance and F=-kx. All numerical geometry, stiffnesses and
loads below are selected test inputs, not recommended engineering values.
The explicit absolute tolerances only bound floating-point roundoff.
"""

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
        ("pretension", "reference"),
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
    elif defect == "pretension":
        vessel.mooring_lines[0].pretension = 11
    elif defect == "nonfinite":
        loads.surge[1] = np.nan
    elif defect == "time":
        loads.time[1] = loads.time[0]
    elif defect == "duplicate":
        vessel.mooring_lines[0].line_id = vessel.mooring_lines[1].line_id
    with pytest.raises(ValueError, match=match):
        solve_response(vessel, loads, settings)


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
