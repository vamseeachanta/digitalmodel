# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Hand calculations for #2748 gaps; inputs are illustrative, not design data.

Sources are equations in docs/engineering-citations.md (R1/R2, C2, D1).
All physical inputs below are explicitly assumed worked-example inputs from
#2748, sections 14, 15 and 19, not values inferred from function defaults.
Assertion tolerances come from those acceptance statements, not a standard.
"""

import numpy as np
import pytest

from digitalmodel.orcawave.drift_forces import compute_wind_current_drift
from digitalmodel.orcawave.hydro_coefficients import create_hydrostatic_restoring
from digitalmodel.orcawave.motion_statistics import (
    compute_response_spectrum,
    short_term_statistics,
)
from digitalmodel.orcawave.vessel_database import (
    get_representative_raos,
    get_vessel,
    list_vessels,
)
from digitalmodel.orcawave.wave_spectrum import (
    heuristic_two_peak,
    issc_spectrum,
    torsethaugen,
)


def test_explicit_beam_drag_and_assumed_yaw_arm():
    """NASA, Drag Equation (2025): F=Cd*rho*V^2*A/2.

    Assumed wind inputs give 1.225*20^2*.8*3000/2/1000=588 kN.
    Assumed current inputs give 1025*1^2*1.2*1500/2/1000=922.5 kN.
    Moment algebra Mz=rx*Fy: assumed rx=.1*250=25 m gives 14700
    and 23062.5 kN m. The .1*L empirical arm is NOT_EVALUATED against
    a published yaw model; these are arithmetic checks of that assumption.
    Inputs, .1 arm, and rel=1e-9: #2748 section 14 and sidecar D1.
    """
    result = compute_wind_current_drift(
        wind_speed_m_s=20,
        current_speed_m_s=1,
        heading_deg=90,
        projected_wind_area_m2=3000,
        projected_current_area_m2=1500,
        vessel_length_m=250,
        cx_wind=1,
        cy_wind=0.8,
        cx_current=0.08,
        cy_current=1.2,
        rho_air=1.225,
        rho_water=1025,
    )
    assert result.wind_force_sway_kn == pytest.approx(588, rel=1e-9)
    assert result.current_force_sway_kn == pytest.approx(922.5, rel=1e-9)
    assert result.wind_moment_yaw_knm == pytest.approx(14700, rel=1e-9)
    assert result.current_moment_yaw_knm == pytest.approx(23062.5, rel=1e-9)


def test_explicit_head_wind_body_frame():
    """#2748 section 19; D1 frame, NASA drag equation (2025).

    FROM the bow pushes along -x: -1.225*20^2*1*3000/2/1000
    = -735 kN. The explicit current speed is zero as an isolated control.
    All coefficients and physical inputs are assumed test inputs; rel=1e-9
    is the issue acceptance. This does not qualify imported solver frames.
    """
    result = compute_wind_current_drift(
        wind_speed_m_s=20,
        current_speed_m_s=0,
        heading_deg=0,
        projected_wind_area_m2=3000,
        projected_current_area_m2=1500,
        vessel_length_m=250,
        cx_wind=1,
        cy_wind=0.8,
        cx_current=0.08,
        cy_current=1.2,
        rho_air=1.225,
        rho_water=1025,
    )
    assert result.wind_force_surge_kn == pytest.approx(-735, rel=1e-9)
    assert result.current_force_surge_kn == 0
    assert result.frame == "x_forward_y_port_z_up"
    assert result.heading_convention == "from_clockwise_bow"


def test_nondefault_drag_inputs_are_used():
    """D1/NASA drag law (2025); independently assumed sensitivity inputs.

    Wind: .5*1*10^2*.5*2000/1000=50 kN; current:
    .5*1000*2^2*1*1000/1000=2000 kN. With assumed L=100 m,
    retained arm=.1*100=10 m gives 500 and 20000 kN m.
    These explicit coefficients are arithmetic inputs, not empirical advice.
    rel=1e-9 is #2748 section 14 numerical acceptance. This case detects
    hardcoded defaults that could pass the illustrative default-valued case.
    """
    result = compute_wind_current_drift(
        wind_speed_m_s=10,
        current_speed_m_s=2,
        heading_deg=90,
        projected_wind_area_m2=2000,
        projected_current_area_m2=1000,
        vessel_length_m=100,
        cx_wind=1,
        cy_wind=0.5,
        cx_current=0.1,
        cy_current=1,
        rho_air=1,
        rho_water=1000,
    )
    assert result.wind_force_sway_kn == pytest.approx(50, rel=1e-9)
    assert result.current_force_sway_kn == pytest.approx(2000, rel=1e-9)
    assert result.wind_moment_yaw_knm == pytest.approx(500, rel=1e-9)
    assert result.current_moment_yaw_knm == pytest.approx(20000, rel=1e-9)


@pytest.mark.parametrize(
    "rho,g,expected",
    [
        (1025, 9.81, (30165750, 201105000, 10055250000)),
        (1000, 10, (30000000, 200000000, 10000000000)),
    ],
)
def test_explicit_hydrostatic_inputs(rho, g, expected):
    """C2: WAMIT 7.5 manual 3.1, reduced about principal flotation axes.

    Assumed V=10000 m3, Awp=3000 m2, GMt=2 m, GMl=100 m.
    C33=1025*9.81*3000=30165750 N/m;
    C44=1025*9.81*10000*2=201105000 N m/rad;
    C55=1025*9.81*10000*100=10055250000 N m/rad.
    A second assumed rho=1000, g=10 case gives 30e6, 200e6, 10e9:
    it detects implementations that ignore supplied density or gravity.
    rel=1e-12 is #2748 section 15 acceptance, not a design tolerance.
    """
    matrix = np.asarray(
        create_hydrostatic_restoring(
            displaced_volume_m3=10000,
            waterplane_area_m2=3000,
            gm_t=2,
            gm_l=100,
            rho=rho,
            g=g,
        )
    )
    np.testing.assert_allclose(np.diag(matrix)[2:5], expected, rtol=1e-12, atol=0)
    assert matrix[2, 4] == matrix[4, 2] == 0  # C2 documents omitted coupling.


def test_significant_amplitudes_from_hand_integrated_spectrum():
    """R1, Rayleigh peak law derivation; #2748 sections 5 and 13.

    Assumed RAO=2, wave density=1/4 at omega=1,2 rad/s.
    Response density is 4*(1/4)=1 at both bins; trapezoidal m0
    =(2-1)*(1+1)/2=1, independently of the returned response moments.
    Hence RMS=1, significant single=2 and double=4. rel=1e-9 is
    section 5 acceptance; no spectrum normalization is involved.
    """
    response = compute_response_spectrum(
        np.array([1.0, 2.0]), np.array([2.0, 2.0]), np.array([0.25, 0.25])
    )
    assert response.m0 == pytest.approx(1, rel=1e-9)
    result = short_term_statistics(response)
    assert result.rms == pytest.approx(1, rel=1e-9)
    assert result.significant_single_amplitude == pytest.approx(2, rel=1e-9)
    assert result.significant_double_amplitude == pytest.approx(4, rel=1e-9)


def test_issc_absolute_ordinates_without_grid_rescaling():
    """W3 closed form; #2748 section 12 also needs absolute shape checks.

    For assumed Hs=4, Tz=10, wz=2*pi/10, evaluate the defining
    S=C*w^-5*exp(-B/w^4), C=Hs^2*wz^4/(4*pi), B=wz^4/pi.
    S(wz)=16*10/(8*pi^2)*exp(-1/pi);
    S(2*wz)=16*10/(8*pi^2*32)*exp(-1/(16*pi)).
    A period ratio cancels overall scale, so cannot catch rescaling alone.
    These absolute ordinates do. rel=1e-12 compares scalar arithmetic,
    not quadrature accuracy or a standard acceptance criterion.
    """
    wz = 2 * np.pi / 10
    omega = np.unique(np.append(np.geomspace(0.01, 20, 2000), [wz, 2 * wz]))
    actual = issc_spectrum(omega, hs=4, tz=10)
    expected = [
        16 * 10 / (8 * np.pi**2) * np.exp(-1 / np.pi),
        16 * 10 / (8 * np.pi**2 * 32) * np.exp(-1 / (16 * np.pi)),
    ]
    np.testing.assert_allclose(
        actual[np.searchsorted(omega, [wz, 2 * wz])], expected, rtol=1e-12, atol=0
    )


def test_deprecated_two_peak_callable_identifies_heuristic():
    """#2748 section 18 compatibility; this checks dispatch, not physics.

    The source paper was unavailable. No published-model values are claimed;
    explicit heuristic ordinates remain independently checked elsewhere.
    """
    omega = np.linspace(0.01, 20, 2000)
    with pytest.warns(DeprecationWarning, match="heuristic_two_peak.*unqualified"):
        actual = torsethaugen(omega, 4, 10)
    np.testing.assert_array_equal(actual, heuristic_two_peak(omega, 4, 10))


@pytest.mark.parametrize(
    "key", list_vessels(), ids=lambda key: f"entry{list_vessels().index(key) + 1}"
)
def test_each_database_entry_is_synthetic_and_mass_consistent(key):
    """R4: Cb=V/(LBT), mass=rho*V; #2748 section 17 input band=1%.

    rho=1.025 t/m3 is the explicitly assumed density in the preliminary
    database, not a universal constant. Each entry must expose its synthetic
    provenance as well as its displacement consistency.
    """
    entry = get_vessel(key)
    assert entry.synthetic
    expected = (
        1.025 * entry.length_m * entry.beam_m * entry.draft_m * entry.block_coefficient
    )
    assert entry.displacement_te == pytest.approx(expected, rel=0.01)


@pytest.mark.parametrize(
    "index",
    [
        pytest.param(
            i,
            marks=pytest.mark.xfail(
                strict=True,
                raises=AssertionError,
                reason="#2748 section 17: unresolved finite-period slope criterion",
            ),
        )
        if i != 3
        else i
        for i in range(5)
    ],
    ids=lambda i: f"entry{i + 1}",
)
def test_requested_pitch_slope_band_at_25_seconds(index):
    """R4 dispersion derivation: k=omega^2/g=(2*pi/25)^2/9.81.

    For unit wave amplitude, slope=k rad/m; multiply by 180/pi for
    degrees/m, giving about 0.369. Inputs and 10% band are #2748
    section 17 acceptance. A finite-period resonance can violate this
    proposed band: retain dynamics and mark the unresolved cases strictly.
    """
    entry = get_vessel(list_vessels()[index])
    response = get_representative_raos(entry.vessel_type)
    amplitude = response.pitch_rao[0][response.periods.index(25)]
    slope = (2 * np.pi / 25) ** 2 / 9.81 * 180 / np.pi
    assert amplitude == pytest.approx(slope, rel=0.1)
