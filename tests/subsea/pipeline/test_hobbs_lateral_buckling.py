"""
Tests for digitalmodel.subsea.pipeline.global_buckling (Hobbs 1984).

The checks are of three kinds:

1. **Closed-form identities** -- the published equations evaluated directly and
   compared with the module, so a typo in a coefficient or an exponent fails.
2. **Structural properties of the solution** -- the P0(L) turning point really
   is a minimum, the two roots either side of it really reproduce the target
   force, the periodic mode's analytic minimum agrees with a numeric scan.
3. **Guard rails** -- invalid geometry and non-physical inputs raise.

There is no benchmark against published worked examples here: Hobbs' own
tabulated cases are not redistributable, and a synthetic number asserted
against itself would prove nothing.
"""
import math

import pytest

from digitalmodel.subsea.pipeline.global_buckling import (
    MODE_CONSTANTS,
    HobbsMode,
    PipeSection,
    SoilResistance,
    critical_state,
    effective_driving_force,
    equilibria_at_temperature,
    governing_mode,
    lateral_equilibrium,
    screen_modes,
)

# A representative 12.75 in x 15.9 mm X65 export line.
OD_M = 0.3239
WT_M = 0.0159
E_PA = 207e9
WEIGHT_N_M = 900.0


@pytest.fixture
def pipe() -> PipeSection:
    return PipeSection.from_dimensions(
        e_modulus_pa=E_PA,
        od_m=OD_M,
        wt_m=WT_M,
        submerged_weight_N_m=WEIGHT_N_M,
    )


@pytest.fixture
def soil() -> SoilResistance:
    return SoilResistance(axial_friction=0.5, lateral_friction=0.7)


# ----------------------------------------------------------------------
# Section properties
# ----------------------------------------------------------------------


def test_from_dimensions_matches_annulus_formulae(pipe):
    id_m = OD_M - 2.0 * WT_M
    assert pipe.area_m2 == pytest.approx(math.pi / 4.0 * (OD_M**2 - id_m**2))
    assert pipe.inertia_m4 == pytest.approx(math.pi / 64.0 * (OD_M**4 - id_m**4))
    assert pipe.outer_radius_m == pytest.approx(OD_M / 2.0)
    assert pipe.EA == pytest.approx(E_PA * pipe.area_m2)
    assert pipe.EI == pytest.approx(E_PA * pipe.inertia_m4)


def test_wall_thicker_than_radius_rejected():
    with pytest.raises(ValueError, match="less than half"):
        PipeSection.from_dimensions(
            e_modulus_pa=E_PA, od_m=0.3, wt_m=0.16, submerged_weight_N_m=900.0
        )


@pytest.mark.parametrize("bad", [0.0, -1.0, float("nan"), float("inf")])
def test_non_physical_weight_rejected(bad):
    with pytest.raises(ValueError):
        PipeSection.from_dimensions(
            e_modulus_pa=E_PA, od_m=OD_M, wt_m=WT_M, submerged_weight_N_m=bad
        )


@pytest.mark.parametrize("bad", [0.0, -0.3, float("nan")])
def test_non_physical_friction_rejected(bad):
    with pytest.raises(ValueError):
        SoilResistance(axial_friction=bad, lateral_friction=0.7)
    with pytest.raises(ValueError):
        SoilResistance(axial_friction=0.5, lateral_friction=bad)


# ----------------------------------------------------------------------
# Hobbs Table 1 constants
# ----------------------------------------------------------------------


def test_mode_2_and_periodic_buckle_coefficient_is_four_pi_squared():
    # Modes 2 and infinity share the Euler-like coefficient 4*pi^2.
    assert MODE_CONSTANTS[HobbsMode.MODE_2].k1 == pytest.approx(39.4784, abs=1e-4)
    assert MODE_CONSTANTS[HobbsMode.INFINITE].k1 == pytest.approx(39.4784, abs=1e-4)


def test_periodic_mode_has_no_axial_slip_term():
    # k3 = 0: the periodic solution has no feed-in from outside the buckle.
    assert MODE_CONSTANTS[HobbsMode.INFINITE].k3 == 0.0
    assert MODE_CONSTANTS[HobbsMode.INFINITE].k6 is not None
    for mode in (HobbsMode.MODE_1, HobbsMode.MODE_2, HobbsMode.MODE_3, HobbsMode.MODE_4):
        assert MODE_CONSTANTS[mode].k3 > 0.0
        assert MODE_CONSTANTS[mode].k6 is None


# ----------------------------------------------------------------------
# Equilibrium equations, evaluated independently
# ----------------------------------------------------------------------


@pytest.mark.parametrize(
    "mode", [HobbsMode.MODE_1, HobbsMode.MODE_2, HobbsMode.MODE_3, HobbsMode.MODE_4]
)
def test_finite_mode_equations(pipe, soil, mode):
    length = 60.0
    k = MODE_CONSTANTS[mode]
    q_a = soil.axial_friction * WEIGHT_N_M
    q_l = soil.lateral_friction * WEIGHT_N_M

    expected_p = k.k1 * pipe.EI / length**2
    z = k.k2 * pipe.EA * q_l**2 * length**5 / (q_a * pipe.EI**2)
    expected_p0 = expected_p + k.k3 * q_a * length * (math.sqrt(1.0 + z) - 1.0)
    expected_amplitude = k.k4 * q_l * length**4 / pipe.EI
    expected_moment = k.k5 * q_l * length**2

    state = lateral_equilibrium(pipe, soil, length, mode)
    assert state.buckle_force_N == pytest.approx(expected_p, rel=1e-12)
    assert state.far_field_force_N == pytest.approx(expected_p0, rel=1e-9)
    assert state.amplitude_m == pytest.approx(expected_amplitude, rel=1e-12)
    assert state.max_moment_Nm == pytest.approx(expected_moment, rel=1e-12)
    assert state.max_slope is None


def test_periodic_mode_equations(pipe, soil):
    length = 60.0
    k = MODE_CONSTANTS[HobbsMode.INFINITE]
    q_l = soil.lateral_friction * WEIGHT_N_M

    expected_p = k.k1 * pipe.EI / length**2
    expected_p0 = expected_p + k.k2 * pipe.EA * q_l**2 * length**6 / pipe.EI**2

    state = lateral_equilibrium(pipe, soil, length, HobbsMode.INFINITE)
    assert state.buckle_force_N == pytest.approx(expected_p, rel=1e-12)
    assert state.far_field_force_N == pytest.approx(expected_p0, rel=1e-12)
    assert state.max_slope == pytest.approx(k.k6 * q_l * length**3 / pipe.EI, rel=1e-12)


def test_equal_friction_collapses_to_single_coefficient_form(pipe):
    """With phi_A = phi_L the radical reduces to the textbook k2 EA q L^5 / EI^2."""
    phi = 0.6
    soil = SoilResistance(axial_friction=phi, lateral_friction=phi)
    q = phi * WEIGHT_N_M
    length = 55.0
    k = MODE_CONSTANTS[HobbsMode.MODE_3]

    z = k.k2 * pipe.EA * q * length**5 / pipe.EI**2
    expected = k.k1 * pipe.EI / length**2 + k.k3 * q * length * (math.sqrt(1.0 + z) - 1.0)

    state = lateral_equilibrium(pipe, soil, length, HobbsMode.MODE_3)
    assert state.far_field_force_N == pytest.approx(expected, rel=1e-9)


def test_amplitude_and_moment_scale_with_length_powers(pipe, soil):
    short = lateral_equilibrium(pipe, soil, 40.0, HobbsMode.MODE_3)
    long = lateral_equilibrium(pipe, soil, 80.0, HobbsMode.MODE_3)
    assert long.amplitude_m / short.amplitude_m == pytest.approx(2.0**4, rel=1e-12)
    assert long.max_moment_Nm / short.max_moment_Nm == pytest.approx(2.0**2, rel=1e-12)
    assert long.buckle_force_N / short.buckle_force_N == pytest.approx(2.0**-2, rel=1e-12)


def test_stresses_follow_from_force_and_moment(pipe, soil):
    state = lateral_equilibrium(pipe, soil, 70.0, HobbsMode.MODE_3)
    assert state.axial_stress_pa == pytest.approx(state.buckle_force_N / pipe.area_m2)
    assert state.bending_stress_pa == pytest.approx(
        state.max_moment_Nm * pipe.outer_radius_m / pipe.inertia_m4
    )
    assert state.combined_stress_pa == pytest.approx(
        state.axial_stress_pa + state.bending_stress_pa
    )


def test_temperature_is_the_thermal_equivalent_of_the_far_field_force(pipe, soil):
    state = lateral_equilibrium(pipe, soil, 70.0, HobbsMode.MODE_2)
    recovered = pipe.fully_restrained_thermal_force(state.temperature_rise_K)
    assert recovered == pytest.approx(state.far_field_force_N, rel=1e-12)


@pytest.mark.parametrize("bad", [0.0, -10.0, float("nan")])
def test_non_physical_buckle_length_rejected(pipe, soil, bad):
    with pytest.raises(ValueError):
        lateral_equilibrium(pipe, soil, bad, HobbsMode.MODE_3)


def test_unknown_mode_rejected(pipe, soil):
    with pytest.raises(ValueError, match="mode must be"):
        lateral_equilibrium(pipe, soil, 60.0, "mode-5")


def test_mode_accepts_int_and_string_aliases(pipe, soil):
    by_enum = lateral_equilibrium(pipe, soil, 60.0, HobbsMode.MODE_3)
    by_int = lateral_equilibrium(pipe, soil, 60.0, 3)
    by_str = lateral_equilibrium(pipe, soil, 60.0, "3")
    assert by_int.far_field_force_N == by_enum.far_field_force_N
    assert by_str.far_field_force_N == by_enum.far_field_force_N


# ----------------------------------------------------------------------
# The turning point of P0(L)
# ----------------------------------------------------------------------


@pytest.mark.parametrize("mode", list(HobbsMode))
def test_critical_state_is_a_local_minimum(pipe, soil, mode):
    state = critical_state(pipe, soil, mode)
    for factor in (0.9, 0.99, 1.01, 1.1):
        neighbour = lateral_equilibrium(
            pipe, soil, state.buckle_length_m * factor, mode
        )
        assert neighbour.far_field_force_N > state.far_field_force_N


def test_periodic_analytic_minimum_agrees_with_a_numeric_scan(pipe, soil):
    state = critical_state(pipe, soil, HobbsMode.INFINITE)
    lengths = [state.buckle_length_m * (0.5 + i / 100.0) for i in range(101)]
    scanned = min(
        lengths,
        key=lambda length: lateral_equilibrium(
            pipe, soil, length, HobbsMode.INFINITE
        ).far_field_force_N,
    )
    assert scanned == pytest.approx(state.buckle_length_m, rel=2e-2)


@pytest.mark.parametrize("mode", list(HobbsMode))
def test_critical_state_gives_a_physically_plausible_buckle(pipe, soil, mode):
    state = critical_state(pipe, soil, mode)
    # A 12.75 in line on friction of this order buckles over tens of metres at
    # a temperature rise of order tens of degrees, with amplitude under a
    # metre; anything outside these bands means a coefficient or exponent slip.
    assert 5.0 < state.buckle_length_m < 500.0
    assert 1.0 < state.temperature_rise_K < 300.0
    assert 0.0 < state.amplitude_m < 5.0


def test_higher_lateral_friction_raises_the_critical_force(pipe):
    low = critical_state(
        pipe, SoilResistance(axial_friction=0.5, lateral_friction=0.4), HobbsMode.MODE_3
    )
    high = critical_state(
        pipe, SoilResistance(axial_friction=0.5, lateral_friction=1.0), HobbsMode.MODE_3
    )
    assert high.far_field_force_N > low.far_field_force_N
    # Stiffer lateral restraint also shortens the buckle.
    assert high.buckle_length_m < low.buckle_length_m


# ----------------------------------------------------------------------
# Roots at a given temperature
# ----------------------------------------------------------------------


def test_below_the_critical_temperature_there_is_no_equilibrium(pipe, soil):
    critical = critical_state(pipe, soil, HobbsMode.MODE_3)
    assert equilibria_at_temperature(
        pipe, soil, critical.temperature_rise_K * 0.95, HobbsMode.MODE_3
    ) == ()


def test_at_the_critical_temperature_the_two_roots_merge(pipe, soil):
    critical = critical_state(pipe, soil, HobbsMode.MODE_3)
    states = equilibria_at_temperature(
        pipe, soil, critical.temperature_rise_K, HobbsMode.MODE_3
    )
    assert len(states) == 1
    assert states[0].buckle_length_m == pytest.approx(critical.buckle_length_m)


def test_above_the_critical_temperature_there_are_two_branches(pipe, soil):
    critical = critical_state(pipe, soil, HobbsMode.MODE_3)
    target = critical.temperature_rise_K * 1.5
    states = equilibria_at_temperature(pipe, soil, target, HobbsMode.MODE_3)

    assert len(states) == 2
    short, long = states
    assert short.buckle_length_m < critical.buckle_length_m < long.buckle_length_m
    for state in states:
        assert state.temperature_rise_K == pytest.approx(target, rel=1e-9)
    # The post-snap branch is the damaging one: longer, larger, more curved.
    assert long.amplitude_m > short.amplitude_m
    assert long.max_moment_Nm > short.max_moment_Nm


# ----------------------------------------------------------------------
# Mode screening
# ----------------------------------------------------------------------


def test_screen_modes_is_ordered_and_flags_the_governing_mode(pipe, soil):
    governing = governing_mode(pipe, soil)
    results = screen_modes(pipe, soil, driving_force_N=governing.far_field_force_N * 1.2)

    forces = [r.critical_state.far_field_force_N for r in results]
    assert forces == sorted(forces)
    assert results[0].mode is governing.mode
    assert results[0].susceptible is True
    assert results[0].utilisation == pytest.approx(1.2, rel=1e-9)


def test_a_line_below_every_critical_force_is_not_susceptible(pipe, soil):
    governing = governing_mode(pipe, soil)
    results = screen_modes(pipe, soil, driving_force_N=governing.far_field_force_N * 0.5)
    assert all(not r.susceptible for r in results)


def test_screen_modes_rejects_a_non_positive_driving_force(pipe, soil):
    with pytest.raises(ValueError):
        screen_modes(pipe, soil, driving_force_N=0.0)


# ----------------------------------------------------------------------
# Effective driving force
# ----------------------------------------------------------------------


def test_effective_driving_force_sums_thermal_and_pressure_terms(pipe):
    dt = 60.0
    pressure = 15e6
    internal_area = math.pi / 4.0 * (OD_M - 2.0 * WT_M) ** 2

    force = effective_driving_force(
        pipe,
        temperature_rise_K=dt,
        internal_pressure_pa=pressure,
        internal_area_m2=internal_area,
        poisson_ratio=0.3,
    )
    expected = pipe.fully_restrained_thermal_force(dt) + pressure * internal_area * 0.4
    assert force == pytest.approx(expected, rel=1e-12)


def test_residual_lay_tension_relieves_compression(pipe):
    with_tension = effective_driving_force(
        pipe, temperature_rise_K=40.0, residual_lay_tension_N=50e3
    )
    without = effective_driving_force(pipe, temperature_rise_K=40.0)
    assert with_tension == pytest.approx(without - 50e3)


def test_a_line_in_net_tension_has_no_driving_force(pipe):
    assert (
        effective_driving_force(
            pipe, temperature_rise_K=1.0, residual_lay_tension_N=1e9
        )
        == 0.0
    )
