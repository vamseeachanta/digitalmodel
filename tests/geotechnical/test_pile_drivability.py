# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent checks for axial wave propagation and a single driving blow."""

import math

import pytest

from digitalmodel.geotechnical.pile_drivability import (
    Hammer,
    LumpedPile,
    SoilResistance,
    simulate,
    uniform_pile,
)


def rod(segments=10):
    return uniform_pile(
        length_m=10.0,
        area_m2=0.01,
        youngs_modulus_pa=200e9,
        density_kg_m3=7850.0,
        segments=segments,
    )


def test_uniform_discretisation_preserves_mass_and_axial_stiffness():
    pile = rod()
    assert sum(pile.masses_kg) == pytest.approx(785.0)
    assert pile.masses_kg[0] == pile.masses_kg[-1] == 39.25
    assert sum(1 / k for k in pile.stiffnesses_n_m) == pytest.approx(10 / (200e9 * 0.01))
    assert pile.node_depths_m[-1] == 10.0


def test_free_rod_pulse_speed_and_travel_time_converge():
    length, c = 10.0, math.sqrt(200e9 / 7850)
    errors = []
    for count in (80, 160):
        pile = rod(count)
        width, centre = 0.04 * length, 0.2 * length
        u = [1e-5 * math.exp(-0.5 * ((x - centre) / width) ** 2)
             for x in pile.node_depths_m]
        v = [c * (x - centre) / width**2 * displacement
             for x, displacement in zip(pile.node_depths_m, u)]
        result = simulate(
            pile, time_step_s=pile.recommended_time_step_s,
            duration_s=0.7 * length / c,
            initial_displacement_m=u, initial_velocity_m_s=v,
        )
        arrivals = []
        for index in (count * 3 // 10, count * 7 // 10):
            peak = max(range(len(result.time_s)), key=lambda j: result.displacement_m[j][index])
            arrivals.append(result.time_s[peak])
        travel = arrivals[1] - arrivals[0]
        expected = 0.4 * length / c
        errors.append(abs(travel / expected - 1))
        assert 0.4 * length / travel == pytest.approx(c, rel=0.02)
    assert errors[1] < errors[0]
    assert errors[1] < 0.01


def test_free_rod_first_mode_matches_closed_form():
    pile = rod(30)
    c = math.sqrt(200e9 / 7850)
    period = 20.0 / c
    initial = [1e-4 * math.cos(math.pi * x / 10.0) for x in pile.node_depths_m]
    result = simulate(
        pile, time_step_s=pile.recommended_time_step_s,
        duration_s=period, initial_displacement_m=initial,
    )
    assert result.displacement_m[-1] == pytest.approx(initial, abs=2e-8)
    assert max(abs(e.residual_j) for e in result.energy) < result.initial_energy_j * 2e-12


@pytest.mark.parametrize("damping", [0.0, 0.3])
def test_blow_energy_closes_with_yield_rebound_and_contact(damping):
    pile = rod()
    shaft = [SoilResistance(10_000.0, 0.001, damping)] * 10
    result = simulate(
        pile, hammer=Hammer(1000.0, 5000.0, 2e8), shaft=shaft,
        toe=SoilResistance(100_000.0, 0.002, damping),
        time_step_s=pile.recommended_time_step_s, duration_s=0.04,
    )
    # The ledger is evaluated independently from motion and plastic state.
    for energy in result.energy:
        total = math.fsum((energy.kinetic_j, energy.pile_elastic_j, energy.cushion_elastic_j,
                           energy.soil_elastic_j, energy.soil_plastic_work_j, energy.damping_work_j))
        assert total == pytest.approx(5000.0, rel=2e-12)
        assert energy.residual_j == result.initial_energy_j - total
    assert result.energy[-1].soil_plastic_work_j > 0
    assert result.energy[-1].damping_work_j >= 0
    assert (result.energy[-1].damping_work_j > 0) == (damping > 0)
    assert min(result.cushion_force_n) >= 0
    assert result.cushion_force_n[-1] == 0
    assert min(row[-1] for row in result.velocity_m_s) < 0
    assert all(b.soil_plastic_work_j >= a.soil_plastic_work_j
               for a, b in zip(result.energy, result.energy[1:]))
    assert all(b.damping_work_j >= a.damping_work_j
               for a, b in zip(result.energy, result.energy[1:]))


def test_zero_soil_preserves_momentum_and_does_not_report_a_permanent_set():
    pile, hammer = rod(), Hammer(1000.0, 5000.0, 2e8)
    result = simulate(
        pile, hammer=hammer,
        time_step_s=pile.recommended_time_step_s, duration_s=0.02,
    )
    expected = math.sqrt(2 * hammer.mass_kg * hammer.energy_j)
    for velocities, ram_velocity in zip(result.velocity_m_s, result.hammer_velocity_m_s):
        momentum = sum(m * v for m, v in zip(pile.masses_kg, velocities))
        assert momentum + hammer.mass_kg * ram_velocity == pytest.approx(expected, rel=2e-12)
    assert result.permanent_set_m is None
    assert result.blows_per_m is None
    assert result.smith_set_estimate_m is None
    assert not result.settled
    assert result.energy[-1].kinetic_j > 0
    assert result.energy[-1].soil_plastic_work_j == result.energy[-1].damping_work_j == 0
    assert max(result.max_compressive_stress_pa) > 0
    assert max(result.max_tensile_stress_pa) > 0


def test_published_smith_initial_ram_motion():
    # Smith, Pile-Driving Impact, Figures 4 and 6, printed pp. 47–48.
    # Only the incident phase is comparable: the published toe is rigid before
    # force-controlled slip and uses direction-switched friction afterwards.
    lbf, inch, foot = 4.4482216152605, 0.0254, 0.3048
    gravity = 32.17 * foot  # Use the source's gravity, not a modern conversion.
    weights = [1468.0] + [695.0] * 8 + [1160.0, 2940.0, 3116.0, 1558.0]
    stiffness = [7_206_000.0] * 9 + [14_900_000.0, 19_600_000.0, 19_600_000.0]
    pile = LumpedPile(
        masses_kg=tuple(w * lbf / gravity for w in weights),
        stiffnesses_n_m=tuple(k * lbf / inch for k in stiffness),
        areas_m2=(0.01,) * 12, lengths_m=(8.4 * foot,) * 12,
    )
    mass, speed = 7500.0 * lbf / gravity, 14.25 * foot
    result = simulate(
        pile, hammer=Hammer(mass, 0.5 * mass * speed**2, 6_400_000 * lbf / inch),
        time_step_s=5e-6, duration_s=0.005,
    )
    # Ram displacement at n=5 and n=20 includes that interval's update.
    # Smith states about 5% accuracy for the historical time discretisation.
    # The scanned table reads 0.54314; automated text extraction misreads it.
    for time, displacement in ((0.00125, 0.19828), (0.005, 0.54314)):
        index = min(range(len(result.time_s)), key=lambda j: abs(result.time_s[j] - time))
        assert result.hammer_displacement_m[index] / inch == pytest.approx(displacement, rel=0.05)


def test_history_envelopes_and_final_time_are_consistent():
    pile = rod(4)
    result = simulate(
        pile, hammer=Hammer(100.0, 1000.0, 1e8),
        time_step_s=pile.recommended_time_step_s, duration_s=0.005001,
    )
    assert result.time_s[0] == 0
    assert result.time_s[-1] == 0.005001
    assert len(result.time_s) == len(result.stress_pa) == len(result.energy)
    for segment in range(4):
        history = [row[segment] for row in result.stress_pa]
        assert result.max_compressive_stress_pa[segment] == max(0, max(history))
        assert result.max_tensile_stress_pa[segment] == max(0, -min(history))
    assert result.permanent_set_m is None  # A short record cannot claim settlement.


def test_soil_spring_yield_gap_and_unloading_energy():
    # A one-element very stiff pile approximates a rigid body. Validate the
    # internal path integration directly at the constitutive discontinuities.
    from digitalmodel.geotechnical.pile_drivability import _soil_step

    soil = SoilResistance(1000.0, 0.01, 0.2)
    force, _, damping, _, plastic, elastic, work = _soil_step(soil, 0, 0.02, 0, 0.1, False)
    assert force == pytest.approx(750.0)
    assert plastic == pytest.approx(0.01)
    assert elastic == pytest.approx(5.0)
    assert work == pytest.approx(10.0)
    assert force * 0.02 == pytest.approx(elastic + work)
    assert damping == pytest.approx(0.2 * 750.0 * 0.2)
    # Unloading through a toe gap recovers only its elastic energy.
    force, _, _, _, plastic, elastic, work = _soil_step(soil, 0.02, -0.03, 0.01, 0.1, True)
    assert force * -0.03 == pytest.approx(-5.0)
    assert plastic == 0.01
    assert elastic == work == 0


@pytest.mark.parametrize("toe,start,increment,plastic", [
    (False, 0.005, 0.02, 0.0),
    (False, 0.02, -0.035, 0.01),
    (False, -0.02, 0.035, -0.01),
    (True, -0.005, 0.05, 0.01),
    (True, 0.02, -0.03, 0.01),
    (True, -0.02, -0.01, 0.01),
])
def test_soil_path_work_and_force_tangents(toe, start, increment, plastic):
    from digitalmodel.geotechnical.pile_drivability import _soil_step

    soil = SoilResistance(1000.0, 0.01, 0.2)
    response = _soil_step(soil, start, increment, plastic, 0.1, toe)
    force, tangent, damping, damping_tangent, _, elastic, work = response
    initial_extension = max(0, start - plastic) if toe else start - plastic
    initial_energy = 0.5 * 100_000 * initial_extension**2
    assert force * increment == pytest.approx(elastic - initial_energy + work, abs=1e-12)
    assert work >= 0
    assert damping * increment >= 0
    if toe:
        assert force >= 0
    epsilon = 1e-7
    plus = _soil_step(soil, start, increment + epsilon, plastic, 0.1, toe)
    minus = _soil_step(soil, start, increment - epsilon, plastic, 0.1, toe)
    assert tangent == pytest.approx((plus[0] - minus[0]) / (2 * epsilon), rel=1e-7, abs=1e-7)
    assert damping_tangent == pytest.approx((plus[2] - minus[2]) / (2 * epsilon), rel=1e-7, abs=1e-7)


def test_settlement_gates_permanent_set_and_reciprocal_blow_count():
    pile = rod(6)
    inputs = dict(
        hammer=Hammer(1000, 5000, 2e8),
        shaft=[SoilResistance(10000, 0.001, 1)] * 6,
        toe=SoilResistance(100000, 0.002, 1),
        time_step_s=pile.recommended_time_step_s / 2,
    )
    short = simulate(pile, duration_s=0.02, **inputs)
    long = simulate(pile, duration_s=0.6, **inputs)
    assert not short.settled
    assert short.permanent_set_m is short.blows_per_m is None
    assert short.smith_set_estimate_m > 0
    assert long.settled
    assert long.permanent_set_m > 0
    assert long.blows_per_m == 1 / long.permanent_set_m
    assert long.smith_blows_per_m_estimate == 1 / long.smith_set_estimate_m
    window = [row for time, row in zip(long.time_s, long.velocity_m_s)
              if time >= long.time_s[-1] - pile.settling_window_s]
    assert max(abs(v) for row in window for v in row) < 1e-3
    assert max(abs(e.residual_j) for e in long.energy) < long.initial_energy_j * 1e-11


def test_no_forward_set_is_distinct_from_an_unfinished_blow():
    pile = rod(3)
    result = simulate(
        pile, hammer=Hammer(100, 0, 1e8),
        shaft=[SoilResistance(1000, 0.001, 0)] * 3,
        time_step_s=pile.recommended_time_step_s / 2, duration_s=0.02,
    )
    assert result.settled
    assert result.permanent_set_m == 0
    assert result.blows_per_m == math.inf
    assert result.initial_energy_j == 0
    assert all(energy.residual_j == 0 for energy in result.energy)


def test_toe_damping_cannot_create_a_tensile_contact_force():
    from digitalmodel.geotechnical.pile_drivability import _soil_step

    soil = SoilResistance(1000, 0.01, 0.2)
    static, tangent, dynamic, dynamic_tangent, _, _, _ = _soil_step(
        soil, 0.02, -0.03, 0.01, 0.001, True,
    )
    assert static + dynamic == 0
    assert tangent + dynamic_tangent == 0
    assert dynamic * -0.03 >= 0


def test_releasing_toe_contact_preserves_blow_energy_without_suction():
    pile = rod(3)
    result = simulate(
        pile, toe=SoilResistance(1000, 0.01, 1),
        initial_displacement_m=[0.005] * 4,
        initial_velocity_m_s=[-10.0] * 4,
        time_step_s=pile.recommended_time_step_s, duration_s=0.001,
    )
    # The net toe force is clipped at zero throughout fast rebound: stored
    # soil energy becomes dissipation, rather than a tensile reaction.
    assert result.velocity_m_s[-1] == pytest.approx([-10] * 4, abs=1e-12)
    assert result.energy[-1].soil_elastic_j == 0
    assert result.energy[-1].damping_work_j == pytest.approx(1.25, abs=1e-12)
    assert max(abs(e.residual_j) for e in result.energy) < 1e-9


def test_blow_stress_and_set_converge_under_time_step_refinement():
    pile = rod(6)
    results = [simulate(
        pile, hammer=Hammer(1000, 5000, 2e8),
        shaft=[SoilResistance(10000, 0.001, 0.3)] * 6,
        toe=SoilResistance(100000, 0.002, 0.3),
        time_step_s=pile.recommended_time_step_s / divisor, duration_s=0.03,
    ) for divisor in (1, 2, 4)]
    for extract in (lambda r: max(r.max_compressive_stress_pa),
                    lambda r: max(r.max_tensile_stress_pa),
                    lambda r: r.smith_set_estimate_m):
        coarse, medium, fine = [extract(result) for result in results]
        assert abs(medium - fine) < abs(coarse - fine)
        assert medium == pytest.approx(fine, rel=0.01)


@pytest.mark.parametrize("bad", [0.0, -1.0, math.nan, math.inf])
def test_positive_pile_inputs_are_validated(bad):
    with pytest.raises(ValueError):
        uniform_pile(bad, 0.01, 200e9, 7850, 10)


@pytest.mark.parametrize("count", [0, -1, 1.5, True])
def test_segment_count_is_a_positive_integer(count):
    with pytest.raises(ValueError):
        uniform_pile(10, 0.01, 200e9, 7850, count)


def test_invalid_models_and_integration_inputs_are_rejected():
    for args in ((-1, 0.01, 0), (1, 0, 0), (1, 0.01, -1), (math.nan, 0.01, 0)):
        with pytest.raises(ValueError):
            SoilResistance(*args)
    with pytest.raises(ValueError):
        Hammer(1, -1, 1)
    with pytest.raises(ValueError):
        LumpedPile((1, 1), (1, 1), (1,), (1,))
    for kwargs in (
        {"time_step_s": 0}, {"duration_s": -1},
        {"time_step_s": 1}, {"shaft": [SoilResistance(1, 0.01, 0)]},
        {"initial_velocity_m_s": [math.nan] * 11},
        {"initial_displacement_m": [0]}, {"settlement_velocity_m_s": 0},
    ):
        inputs = {"time_step_s": rod().recommended_time_step_s, "duration_s": 0.001}
        inputs.update(kwargs)
        with pytest.raises(ValueError):
            simulate(rod(), **inputs)
