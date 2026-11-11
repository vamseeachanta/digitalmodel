"""Tests for the component (Ikeda-type) viscous roll damping estimate.

The method is the one collected by Chakrabarti (2001), "Empirical calculation
of roll damping for ships and barges", Ocean Engineering 28, 915-932, which
restates Ikeda, Himeno and Tanaka (1978) and Himeno (1981), and adds the
sharp-cornered barge eddy formula of Ikeda et al. (1993). Equation numbers in
the test names refer to that paper unless another source is named.

Three kinds of check are made:

* identities that hold exactly whatever the inputs (the laminar friction
  formula equals the leading term of the turbulent one; the Lewis mapping
  returns the beam, draught and area it was built from; the lift formula
  equals the lever-arm form in ITTC 7.5-02-07-04.5);
* reproduction of the few numbers the paper prints for its derrick barge,
  under input assumptions stated next to each test because the paper does not
  print every input;
* behaviour (zero-speed limits, monotonic trends, input validation).
"""

import math

import pytest

from digitalmodel.hydrodynamics.diffraction.roll_damping import critical_damping
from digitalmodel.hydrodynamics.roll_damping_components import (
    HullSection,
    RollDampingComponentError,
    RollDampingComponents,
    barge_eddy_damping,
    barge_roll_damping,
    bilge_keel_damping_section,
    bilge_keel_lever,
    bilge_keel_normal_section,
    bilge_radius_from_area_coefficient,
    eddy_damping_section,
    eddy_damping_sections,
    eddy_forward_speed_factor,
    eddy_pressure_ratio,
    equivalent_linear_damping,
    friction_damping_laminar,
    friction_damping_turbulent,
    friction_forward_speed_factor,
    friction_radius,
    lewis_coefficients,
    lift_damping,
    ship_wetted_area,
    wave_damping_forward_speed,
)

RHO_SW = 1025.0  # kg/m3
NU_SW = 1.19e-6  # m2/s

# --- Derrick barge of Chakrabarti (2001), section 7, in ft-slug-s units ------
# The formulas are dimensionally homogeneous, so the paper's own units are used.
BARGE_L = 400.0  # ft
BARGE_B = 120.0  # ft
BARGE_D = 19.0  # ft, draught for Table 1
BARGE_CB = 0.923
BARGE_RB = 1.97  # ft, bilge radius
BARGE_SIGMA = 0.999  # midship area coefficient
RHO_SLUG = 1.99  # slug/ft3
AMPLITUDE_10DEG = math.radians(10.0)


def _box_inertia_paper_basis(draft: float) -> float:
    """Roll inertia on the basis the paper describes but does not print.

    p. 928: "The moment of inertia is computed in the program using the simple
    formula of a rectangular box" with "roll mass coefficient ... 2.0". Read
    here as twice the box displacement times (B**2 + D**2)/12. This is an
    assumption; it is stated in the evaluation note.
    """
    mass = RHO_SLUG * BARGE_L * BARGE_B * draft * BARGE_CB
    return 2.0 * mass * (BARGE_B**2 + draft**2) / 12.0


# --- eq. (4): equivalent linearisation ---------------------------------------


def test_eq4_equivalent_linearisation_matches_energy_balance():
    """Energy dissipated per cycle by B1 + B2|v|v + B3 v**3 equals that of Beq."""
    b1, b2, b3 = 2.0e5, 3.0e7, 4.0e8
    omega, amplitude = 0.6, math.radians(8.0)
    n = 20000
    dissipated = 0.0
    for k in range(n):
        t = (k + 0.5) * (2.0 * math.pi / omega) / n
        v = -amplitude * omega * math.sin(omega * t)
        dissipated += (b1 * v + b2 * abs(v) * v + b3 * v**3) * v
    dissipated *= (2.0 * math.pi / omega) / n
    linear_per_unit = math.pi * omega * amplitude**2  # energy for B = 1
    beq = equivalent_linear_damping(b1, b2, b3, omega, amplitude)
    assert beq == pytest.approx(dissipated / linear_per_unit, rel=1e-6)


# --- eqs. (6), (7), (10): skin friction --------------------------------------


def test_eq6_eq7_laminar_equals_leading_term_of_eq10():
    """p. 920 states the first term of eq. 10 equals eq. 6 with eq. 7 inserted.

    4/(3 pi) * 1.328 * sqrt(2 pi / 3.22) = 0.7873; eq. 10 prints 0.787.
    """
    area, radius, omega, amplitude = 5000.0, 9.0, 0.5, math.radians(5.0)
    laminar = friction_damping_laminar(RHO_SW, NU_SW, area, radius, amplitude, omega)
    leading = 0.787 * RHO_SW * area * radius**2 * math.sqrt(omega * NU_SW)
    assert laminar == pytest.approx(leading, rel=1e-3)


def test_eq10_turbulent_tends_to_laminar_as_amplitude_vanishes():
    area, radius, omega = 5000.0, 9.0, 0.5
    tiny = 1e-9
    turbulent = friction_damping_turbulent(RHO_SW, NU_SW, area, radius, tiny, omega)
    laminar = friction_damping_laminar(RHO_SW, NU_SW, area, radius, tiny, omega)
    assert turbulent == pytest.approx(laminar, rel=1e-3)
    big = friction_damping_turbulent(
        RHO_SW, NU_SW, area, radius, math.radians(20.0), omega
    )
    assert big > turbulent


def test_eq8_eq9_wetted_area_and_friction_radius_hand_values():
    area = ship_wetted_area(length=100.0, draft=5.0, beam=20.0, block_coefficient=0.6)
    assert area == pytest.approx(100.0 * (1.7 * 5.0 + 0.6 * 20.0))
    radius = friction_radius(
        block_coefficient=0.6, wetted_area=area, length=100.0, og=1.0
    )
    expected = ((0.887 + 0.145 * 0.6) * (1.7 * 5.0 + 0.6 * 20.0) - 2.0) / math.pi
    assert radius == pytest.approx(expected)


def test_eq12_friction_forward_speed_factor():
    assert friction_forward_speed_factor(0.0, 0.5, 100.0) == 1.0
    assert friction_forward_speed_factor(5.0, 0.5, 100.0) == pytest.approx(1.41)


# --- Lewis mapping used by eqs. (19)-(34) -------------------------------------


def test_lewis_coefficients_of_a_circle_are_zero():
    a1, a3 = lewis_coefficients(1.0, math.pi / 4.0)
    assert a1 == pytest.approx(0.0, abs=1e-12)
    assert a3 == pytest.approx(0.0, abs=1e-12)


@pytest.mark.parametrize(
    "h0, sigma", [(1.4, 0.95), (2.0, 0.9), (3.158, 0.8), (0.8, 0.7)]
)
def test_lewis_coefficients_reproduce_beam_draught_and_area(h0, sigma):
    a1, a3 = lewis_coefficients(h0, sigma)
    assert (1.0 + a1 + a3) / (1.0 - a1 + a3) == pytest.approx(h0, rel=1e-10)
    area_coefficient = (
        (math.pi / 4.0) * (1.0 - a1**2 - 3.0 * a3**2) / ((1.0 + a3) ** 2 - a1**2)
    )
    assert area_coefficient == pytest.approx(sigma, rel=1e-10)


def test_lewis_coefficients_reject_area_outside_the_lewis_family():
    with pytest.raises(RollDampingComponentError):
        lewis_coefficients(1.0, 0.2)


# --- eqs. (29)/(59): bilge radius ---------------------------------------------


def test_bilge_radius_reproduces_the_section_area():
    beam, draft, sigma = 30.0, 10.0, 0.98
    radius = bilge_radius_from_area_coefficient(beam, draft, sigma)
    area = beam * draft - 2.0 * (1.0 - math.pi / 4.0) * radius**2
    assert area / (beam * draft) == pytest.approx(sigma, rel=1e-12)
    assert bilge_radius_from_area_coefficient(beam, draft, 1.0) == 0.0


# --- eqs. (18)-(34): sectional eddy making ------------------------------------


def test_eddy_section_reproduces_fig5_midbody_level():
    """Fig. 5 (p. 930) plots C_R along the derrick barge; the midbody reads ~120.

    Inputs from p. 929: B = 120 ft, D = 19 ft, bilge radius 1.97 ft,
    sigma = 0.999. The roll axis is taken at the waterline (OG = 0); C_R moves
    by under 10 % for OG between -20 ft and +5 ft, so the check does not hinge
    on that assumption. Tolerance 10 % covers reading the figure.
    """
    cr = eddy_pressure_ratio(
        BARGE_B, BARGE_D, BARGE_SIGMA, og=0.0, bilge_radius=BARGE_RB
    )
    assert cr == pytest.approx(120.0, rel=0.10)


def test_eq25_as_printed_does_not_reproduce_fig5():
    """Eq. 25 as printed applies (r_max/D)**2 to the first product only.

    Himeno (1981) and ITTC 7.5-02-07-04.5 apply it to the whole bracket. The
    printed bracketing gives a C_R near 19 for the barge, far from Fig. 5,
    which shows that the print is a typesetting error and not the method used.
    """
    printed = eddy_pressure_ratio(
        BARGE_B,
        BARGE_D,
        BARGE_SIGMA,
        og=0.0,
        bilge_radius=BARGE_RB,
        variant="as_printed",
    )
    assert printed < 30.0


def test_eddy_section_is_linear_in_amplitude_and_frequency():
    args = dict(rho=RHO_SW, beam=20.0, draft=8.0, sigma=0.95, og=0.0, bilge_radius=None)
    base = eddy_damping_section(amplitude=0.1, omega=0.5, **args)
    assert eddy_damping_section(amplitude=0.2, omega=0.5, **args) == pytest.approx(
        2 * base
    )
    assert eddy_damping_section(amplitude=0.1, omega=1.0, **args) == pytest.approx(
        2 * base
    )


def test_eddy_sections_integrate_prismatic_hull_and_apply_end_factor():
    stations = [
        HullSection(x=float(x), beam=20.0, draft=8.0, sigma=0.95)
        for x in range(0, 101, 10)
    ]
    kwargs = dict(rho=RHO_SW, og=0.0, amplitude=0.1, omega=0.5)
    per_metre = eddy_damping_section(
        beam=20.0, draft=8.0, sigma=0.95, bilge_radius=None, **kwargs
    )
    total = eddy_damping_sections(stations, **kwargs)
    assert total == pytest.approx(100.0 * per_metre, rel=1e-12)
    reduced = eddy_damping_sections(stations, end_station_factor=0.7, **kwargs)
    # Trapezoid: each end station carries half a 10 m interval of the 100 m.
    assert reduced == pytest.approx(total * (1.0 - 2 * 0.3 * 5.0 / 100.0), rel=1e-12)


# --- eq. (35): sharp-cornered barge -------------------------------------------


def test_eq35_barge_eddy_reproduces_table1_new_formula():
    """Table 1 (p. 929), 'new formula & end factor', reduced rakes: 0.16.

    Assumptions, as the paper prints neither: prismatic hull (no rake, no end
    factor), KG equal to the draught (stated for the ship example, p. 928) and
    roll inertia of twice a box (see _box_inertia_paper_basis). The damping
    factor is B/(2 omega_n I) evaluated at omega = omega_n, so the frequency
    cancels. Tolerance 10 %: the table carries two significant figures and the
    end factor, which the prismatic model omits, lowers the value slightly.
    """
    omega = 0.5  # rad/s; cancels in the ratio
    b_e = barge_eddy_damping(
        RHO_SLUG,
        BARGE_L,
        BARGE_B,
        BARGE_D,
        og=0.0,
        amplitude=AMPLITUDE_10DEG,
        omega=omega,
    )
    factor = b_e / (2.0 * omega * _box_inertia_paper_basis(BARGE_D))
    assert factor == pytest.approx(0.16, rel=0.10)


def test_eq35_og_factor_variant_matches_at_waterline_roll_axis():
    common = dict(
        rho=RHO_SW, length=100.0, beam=30.0, draft=5.0, amplitude=0.1, omega=0.5
    )
    assert barge_eddy_damping(og=0.0, **common) == pytest.approx(
        barge_eddy_damping(og=0.0, og_factor=1.5, **common)
    )
    # Roll axis above the waterline (OG < 0) raises the damping in both forms.
    assert barge_eddy_damping(og=-3.0, **common) > barge_eddy_damping(og=0.0, **common)


def test_eq36_eddy_forward_speed_factor():
    assert eddy_forward_speed_factor(0.5, 100.0, 0.0) == 1.0
    k = 0.04 * 0.5 * 100.0 / 5.0
    assert eddy_forward_speed_factor(0.5, 100.0, 5.0) == pytest.approx(
        k**2 / (1 + k**2)
    )


# --- eqs. (37)-(38): lift -----------------------------------------------------


def test_eq37_lift_equals_ittc_lever_form():
    """ITTC 7.5-02-07-04.5 (2024) eq. 33 with l0 = 0.3 d and lR = 0.5 d."""
    rho, speed, length, draft, beam, og = RHO_SW, 7.0, 150.0, 8.0, 25.0, 1.5
    kn = 2.0 * math.pi * draft / length + 0.1 * (4.1 * beam / length - 0.045)
    l0, lr = 0.3 * draft, 0.5 * draft
    ittc = (
        0.5
        * rho
        * speed
        * length
        * draft
        * kn
        * l0
        * lr
        * (1.0 - 1.4 * og / lr + 0.7 * og**2 / (l0 * lr))
    )
    ours = lift_damping(rho, speed, length, draft, beam, og, midship_coefficient=0.95)
    assert ours == pytest.approx(ittc, rel=1e-3)


def test_eq37_lift_is_zero_at_zero_speed():
    assert lift_damping(RHO_SW, 0.0, 150.0, 8.0, 25.0, 0.0, 0.98) == 0.0


# --- eqs. (39)-(43): wave damping at speed ------------------------------------


def test_eq39_wave_factor_is_unity_at_zero_speed():
    """The fitted factor tends to 1 + O(1e-4) as U -> 0; U = 0 returns B_w0."""
    b_w0 = 1.0e6
    assert wave_damping_forward_speed(b_w0, omega=0.6, draft=8.0, speed=0.0) == b_w0
    creeping = wave_damping_forward_speed(b_w0, omega=0.6, draft=8.0, speed=1e-6)
    assert creeping == pytest.approx(b_w0, rel=1e-3)


def test_eq39_wave_factor_peaks_near_tau_one_quarter():
    omega, draft, g = 0.6, 8.0, 9.80665
    speeds = [tau * g / omega for tau in (0.1, 0.25, 0.6)]
    values = [wave_damping_forward_speed(1.0, omega, draft, u) for u in speeds]
    assert values[1] > values[0] and values[1] > values[2]


# --- eqs. (46)-(60): bilge keel -----------------------------------------------


def test_eq47_eq48_normal_force_hand_value():
    rho, beam, draft, sigma, og = RHO_SW, 20.0, 8.0, 0.98, 0.0
    radius = bilge_radius_from_area_coefficient(beam, draft, sigma)
    lever = bilge_keel_lever(beam, draft, og, radius)
    b_bk, amplitude, omega = 0.4, math.radians(10.0), 0.6
    f = 1.0 + 0.3 * math.exp(-160.0 * (1.0 - sigma))
    cd = 22.5 * b_bk / (math.pi * lever * amplitude * f) + 2.4
    expected = (
        8.0 / (3.0 * math.pi) * rho * lever**3 * b_bk * omega * amplitude * f**2 * cd
    )
    got = bilge_keel_normal_section(rho, beam, draft, sigma, og, b_bk, amplitude, omega)
    assert got == pytest.approx(expected, rel=1e-12)


def test_eq60_lever_is_distance_to_bilge_circle_midpoint():
    beam, draft, radius = 20.0, 8.0, 2.0
    lever = bilge_keel_lever(beam, draft, 0.0, radius)
    cx = beam / 2 - radius * (1 - math.cos(math.pi / 4))
    cy = draft - radius * (1 - math.sin(math.pi / 4))
    assert lever == pytest.approx(math.hypot(cx, cy), rel=1e-3)


def test_bilge_keel_total_is_positive_and_grows_with_keel_breadth():
    args = dict(
        rho=RHO_SW, beam=20.0, draft=8.0, sigma=0.98, og=0.0, amplitude=0.17, omega=0.6
    )
    small = bilge_keel_damping_section(bilge_keel_breadth=0.2, **args)
    large = bilge_keel_damping_section(bilge_keel_breadth=0.6, **args)
    assert 0.0 < small < large


# --- totals and percent critical ---------------------------------------------


def test_components_total_and_percent_critical_use_existing_basis():
    parts = RollDampingComponents(
        friction=1.0, eddy=2.0, lift=0.5, wave=3.0, bilge_keel=4.0
    )
    assert parts.total == pytest.approx(10.5)
    stiffness, inertia, added = 4.0e8, 6.0e9, 1.5e9
    expected = 100.0 * parts.total / critical_damping(stiffness, inertia, added)
    assert parts.percent_critical(stiffness, inertia, added) == pytest.approx(expected)


def test_barge_roll_damping_breakdown_sums_and_has_no_lift_at_rest():
    parts = barge_roll_damping(
        rho=RHO_SW,
        nu=NU_SW,
        length=120.0,
        beam=36.0,
        draft=5.5,
        kg=8.0,
        amplitude=math.radians(10.0),
        omega=0.6,
        wave_damping_zero_speed=2.0e6,
        bilge_keel_breadth=0.5,
        bilge_keel_length=60.0,
    )
    assert parts.lift == 0.0
    assert parts.wave == pytest.approx(2.0e6, rel=1e-4)
    assert parts.eddy > parts.friction > 0.0
    assert parts.bilge_keel > 0.0
    assert parts.total == pytest.approx(
        parts.friction + parts.eddy + parts.lift + parts.wave + parts.bilge_keel
    )


# --- validation ----------------------------------------------------------------


@pytest.mark.parametrize(
    "call",
    [
        lambda: barge_eddy_damping(RHO_SW, -1.0, 30.0, 5.0, 0.0, 0.1, 0.5),
        lambda: friction_damping_laminar(RHO_SW, NU_SW, 100.0, 1.0, 0.1, 0.0),
        lambda: eddy_damping_section(RHO_SW, 20.0, 8.0, 1.2, 0.0, None, 0.1, 0.5),
        lambda: eddy_damping_sections([], RHO_SW, 0.0, 0.1, 0.5),
        lambda: equivalent_linear_damping(1.0, 1.0, 1.0, float("nan"), 0.1),
    ],
)
def test_invalid_inputs_raise(call):
    with pytest.raises(RollDampingComponentError):
        call()
