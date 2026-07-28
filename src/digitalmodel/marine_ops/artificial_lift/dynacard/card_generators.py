# ABOUTME: Synthetic dynacard generators for all 18 pump failure modes.
# ABOUTME: Every generator returns a DOWNHOLE (pump) card; see the module note.
"""Synthetic pump cards, and the forward model that lifts them to the surface.

Every ``generate_*`` function here returns a **downhole** card. They are named
for pump conditions -- fluid pound, gas lock, a traveling-valve leak -- and the
shapes they draw are the shapes those conditions make *at the pump*. They exist
to train the diagnostic classifier, which classifies downhole cards.

That makes them the wrong thing to hand a surface-to-downhole solver. A card
generated here has already had the rod string taken out of it; asking a solver
to take the rod string out again is a category error, and it was only ever
invisible because the solver in use did not actually transform the card.

:func:`surface_card_from_pump_card` closes that gap. It runs the rod string in
the other direction -- pump upward to the polished rod -- so a generated pump
card can be turned into the surface card that would have produced it. Feed
*that* to the workflow and the surface-to-downhole solver has real work to do
and returns the pump card it started from.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Callable, Optional

import numpy as np

from .models import CardData

if TYPE_CHECKING:  # pragma: no cover - typing only
    from .models import DynacardAnalysisContext

_NUM_POINTS = 100


def _base_card(
    rng: np.random.Generator,
    stroke: float = 100.0,
    high_load: float = 15000.0,
    low_load: float = 5000.0,
    noise_pct: float = 0.02,
) -> tuple[np.ndarray, np.ndarray]:
    """Generate base rectangular pump card arrays with noise."""
    n = _NUM_POINTS
    half = n // 2
    t_up = np.linspace(0, np.pi, half)
    t_down = np.linspace(np.pi, 2 * np.pi, half)

    # Smooth sinusoidal position
    pos_up = stroke / 2 * (1 - np.cos(t_up))
    pos_down = stroke / 2 * (1 - np.cos(t_down))

    # Smooth load transitions
    transition = 5
    load_up = np.full(half, high_load, dtype=np.float64)
    load_up[:transition] = np.linspace(low_load, high_load, transition)
    load_down = np.full(half, low_load, dtype=np.float64)
    load_down[:transition] = np.linspace(high_load, low_load, transition)

    pos = np.concatenate([pos_up, pos_down])
    load = np.concatenate([load_up, load_down])

    # Add noise
    load_range = high_load - low_load
    load += rng.normal(0, noise_pct * load_range, n)

    return pos, load


def generate_normal_card(seed: int = 0, noise_pct: float = 0.03) -> CardData:
    """Normal operation: clean rectangular card, moderate loads."""
    rng = np.random.default_rng(seed)
    high = rng.uniform(12000, 18000)
    low = rng.uniform(4000, 7000)
    stroke = rng.uniform(80, 120)
    pos, load = _base_card(rng, stroke=stroke, high_load=high, low_load=low, noise_pct=noise_pct)
    return CardData(position=pos.tolist(), load=load.tolist())


def generate_gas_interference_card(seed: int = 0, severity: float = 0.5) -> CardData:
    """Gas interference: low min load, concave downstroke, compressed lower-right."""
    rng = np.random.default_rng(seed)
    high = rng.uniform(10000, 16000)
    low = rng.uniform(500, 2500)  # Very low min load
    stroke = rng.uniform(80, 120)
    pos, load = _base_card(rng, stroke=stroke, high_load=high, low_load=low, noise_pct=0.02)

    # Concave depression on downstroke (second half)
    n = len(pos)
    half = n // 2
    # Create concave shape in lower-right quadrant
    t = np.linspace(0, np.pi, half)
    depression = severity * (high - low) * 0.3 * np.sin(t)
    load[half:] -= depression

    return CardData(position=pos.tolist(), load=load.tolist())


def generate_fluid_pound_card(seed: int = 0, drop_position: float = 0.6) -> CardData:
    """Fluid pound: sharp downstroke load drop indicating incomplete fillage."""
    rng = np.random.default_rng(seed)
    high = rng.uniform(12000, 18000)
    low = rng.uniform(4000, 7000)
    stroke = rng.uniform(80, 120)
    pos, load = _base_card(rng, stroke=stroke, high_load=high, low_load=low, noise_pct=0.02)

    n = len(pos)
    half = n // 2
    drop_idx = half + int((n - half) * drop_position * 0.5)
    drop_idx = min(drop_idx, n - 2)
    # Sharp drop
    drop_magnitude = rng.uniform(4000, 8000)
    load[drop_idx] = load[drop_idx - 1] - drop_magnitude
    # Gradual recovery
    remaining = n - drop_idx - 1
    if remaining > 0:
        recovery = np.linspace(load[drop_idx], low + rng.uniform(500, 1500), remaining)
        load[drop_idx + 1:] = recovery

    return CardData(position=pos.tolist(), load=load.tolist())


def generate_pump_tagging_card(seed: int = 0) -> CardData:
    """Pump tagging: extreme peak load spikes at stroke ends."""
    rng = np.random.default_rng(seed)
    base_high = rng.uniform(15000, 22000)
    low = rng.uniform(4000, 7000)
    stroke = rng.uniform(80, 120)
    pos, load = _base_card(rng, stroke=stroke, high_load=base_high, low_load=low, noise_pct=0.02)

    # Add spike at top of stroke (near max position)
    n = len(pos)
    half = n // 2
    spike_magnitude = rng.uniform(15000, 25000)
    spike_width = rng.integers(2, 6)
    spike_center = half - 1
    for i in range(max(0, spike_center - spike_width), min(n, spike_center + spike_width)):
        dist = abs(i - spike_center)
        load[i] += spike_magnitude * max(0, 1 - dist / spike_width)

    return CardData(position=pos.tolist(), load=load.tolist())


def generate_tubing_movement_card(seed: int = 0) -> CardData:
    """Tubing movement: elongated card, excessive position range vs ideal."""
    rng = np.random.default_rng(seed)
    high = rng.uniform(12000, 18000)
    low = rng.uniform(4000, 7000)
    stroke = rng.uniform(130, 180)  # Elongated stroke
    pos, load = _base_card(rng, stroke=stroke, high_load=high, low_load=low, noise_pct=0.03)

    # Add slope to load (tubing stretching effect)
    n = len(pos)
    slope = rng.uniform(10, 40)
    load += slope * np.linspace(-1, 1, n)

    return CardData(position=pos.tolist(), load=load.tolist())


def generate_valve_leak_tv_card(seed: int = 0) -> CardData:
    """Traveling valve leak: sloping/rounded top, load loss on upstroke."""
    rng = np.random.default_rng(seed)
    high = rng.uniform(12000, 18000)
    low = rng.uniform(4000, 7000)
    stroke = rng.uniform(80, 120)
    pos, load = _base_card(rng, stroke=stroke, high_load=high, low_load=low, noise_pct=0.02)

    # Upstroke load decay (traveling valve leaking)
    n = len(pos)
    half = n // 2
    leak_rate = rng.uniform(0.2, 0.5)
    decay = np.linspace(0, leak_rate * (high - low), half)
    load[:half] -= decay

    return CardData(position=pos.tolist(), load=load.tolist())


def generate_valve_leak_sv_card(seed: int = 0) -> CardData:
    """Standing valve leak: sloping/rounded bottom, load gain on downstroke."""
    rng = np.random.default_rng(seed)
    high = rng.uniform(12000, 18000)
    low = rng.uniform(4000, 7000)
    stroke = rng.uniform(80, 120)
    pos, load = _base_card(rng, stroke=stroke, high_load=high, low_load=low, noise_pct=0.02)

    # Downstroke load increase (standing valve leaking)
    n = len(pos)
    half = n // 2
    leak_rate = rng.uniform(0.2, 0.5)
    rise = np.linspace(0, leak_rate * (high - low), half)
    load[half:] += rise

    return CardData(position=pos.tolist(), load=load.tolist())


def generate_rod_parting_card(seed: int = 0) -> CardData:
    """Rod parting: near-zero area, very thin wavy card, very low loads."""
    rng = np.random.default_rng(seed)
    n = _NUM_POINTS
    t = np.linspace(0, 2 * np.pi, n)
    stroke = rng.uniform(80, 120)
    pos = stroke / 2 * (1 - np.cos(t))

    # Very small load range (thin card), wavy pattern
    mean_load = rng.uniform(1000, 3000)
    amplitude = rng.uniform(200, 800)
    load = mean_load + amplitude * np.sin(2 * t) + rng.normal(0, 100, n)

    return CardData(position=pos.tolist(), load=load.tolist())


def generate_stuck_pump_card(seed: int = 0) -> CardData:
    """Stuck pump: near-zero position range, tiny collapsed card."""
    rng = np.random.default_rng(seed)
    n = _NUM_POINTS
    t = np.linspace(0, 2 * np.pi, n)

    # Very small position range
    stroke = rng.uniform(2, 10)
    pos = stroke / 2 * (1 - np.cos(t))

    # Small load range
    mean_load = rng.uniform(8000, 15000)
    amplitude = rng.uniform(500, 2000)
    load = mean_load + amplitude * np.sin(t) + rng.normal(0, 200, n)

    return CardData(position=pos.tolist(), load=load.tolist())


def generate_worn_barrel_card(seed: int = 0) -> CardData:
    """Worn barrel: gradual area loss, upstroke load decay."""
    rng = np.random.default_rng(seed)
    high = rng.uniform(12000, 18000)
    low = rng.uniform(4000, 7000)
    stroke = rng.uniform(80, 120)
    pos, load = _base_card(rng, stroke=stroke, high_load=high, low_load=low, noise_pct=0.02)

    # Progressive upstroke load decay
    n = len(pos)
    half = n // 2
    decay_rate = rng.uniform(0.15, 0.35)
    t_decay = np.linspace(0, 1, half)
    load[:half] -= decay_rate * (high - low) * t_decay ** 2

    return CardData(position=pos.tolist(), load=load.tolist())


def generate_gas_lock_card(seed: int = 0) -> CardData:
    """Gas lock: extreme gas interference, near-zero card area."""
    rng = np.random.default_rng(seed)
    n = _NUM_POINTS
    t = np.linspace(0, 2 * np.pi, n)
    stroke = rng.uniform(80, 120)
    pos = stroke / 2 * (1 - np.cos(t))

    # Near-zero load range -- pump trapped by gas
    mean_load = rng.uniform(1000, 3000)
    amplitude = rng.uniform(300, 1000)
    load = mean_load + amplitude * np.sin(t) + rng.normal(0, 50, n)

    return CardData(position=pos.tolist(), load=load.tolist())


def generate_delayed_tv_closure_card(seed: int = 0) -> CardData:
    """Delayed traveling valve closure: exponential curve on top-left of card."""
    rng = np.random.default_rng(seed)
    high = rng.uniform(12000, 18000)
    low = rng.uniform(4000, 7000)
    stroke = rng.uniform(80, 120)
    pos, load = _base_card(rng, stroke=stroke, high_load=high, low_load=low, noise_pct=0.02)

    # Delayed load pickup at start of upstroke
    n = len(pos)
    delay_points = rng.integers(10, 25)
    t_delay = np.linspace(0, 1, delay_points)
    # Exponential transition instead of linear
    exp_curve = 1 - np.exp(-3 * t_delay)
    load[:delay_points] = low + (high - low) * exp_curve * 0.5

    return CardData(position=pos.tolist(), load=load.tolist())


def generate_excessive_friction_card(seed: int = 0) -> CardData:
    """Excessive friction: large hysteresis, thick card loop."""
    rng = np.random.default_rng(seed)
    high = rng.uniform(12000, 18000)
    low = rng.uniform(4000, 7000)
    stroke = rng.uniform(80, 120)
    pos, load = _base_card(rng, stroke=stroke, high_load=high, low_load=low, noise_pct=0.02)

    # Add friction offset: upstroke loads higher, downstroke loads lower
    n = len(pos)
    half = n // 2
    friction = rng.uniform(1500, 4000)
    load[:half] += friction
    load[half:] -= friction

    return CardData(position=pos.tolist(), load=load.tolist())


def generate_plunger_undertravel_card(seed: int = 0) -> CardData:
    """Plunger undertravel: short net stroke, truncated position range."""
    rng = np.random.default_rng(seed)
    high = rng.uniform(12000, 18000)
    low = rng.uniform(4000, 7000)
    stroke = rng.uniform(30, 55)  # Short stroke
    pos, load = _base_card(rng, stroke=stroke, high_load=high, low_load=low, noise_pct=0.02)

    return CardData(position=pos.tolist(), load=load.tolist())


def generate_paraffin_restriction_card(seed: int = 0) -> CardData:
    """Paraffin restriction: thin card with concave dents, increased friction."""
    rng = np.random.default_rng(seed)
    high = rng.uniform(11000, 15000)
    low = rng.uniform(5000, 8000)
    stroke = rng.uniform(80, 120)
    pos, load = _base_card(rng, stroke=stroke, high_load=high, low_load=low, noise_pct=0.02)

    # Add concave dents
    n = len(pos)
    num_dents = rng.integers(2, 5)
    for _ in range(num_dents):
        dent_center = rng.integers(10, n - 10)
        dent_width = rng.integers(3, 8)
        dent_depth = rng.uniform(1000, 3000)
        for j in range(max(0, dent_center - dent_width), min(n, dent_center + dent_width)):
            dist = abs(j - dent_center)
            load[j] -= dent_depth * max(0, 1 - dist / dent_width)

    # Add friction component
    half = n // 2
    friction = rng.uniform(800, 2000)
    load[:half] += friction * 0.5
    load[half:] -= friction * 0.5

    return CardData(position=pos.tolist(), load=load.tolist())


def generate_bent_barrel_card(seed: int = 0) -> CardData:
    """Bent barrel: asymmetric quadrant distribution, shifted centroid."""
    rng = np.random.default_rng(seed)
    high = rng.uniform(12000, 18000)
    low = rng.uniform(4000, 7000)
    stroke = rng.uniform(80, 120)
    pos, load = _base_card(rng, stroke=stroke, high_load=high, low_load=low, noise_pct=0.02)

    # Shift centroid: add position-dependent load bias
    n = len(pos)
    pos_norm = (pos - np.min(pos)) / (np.max(pos) - np.min(pos) + 1e-10)
    bias_direction = rng.choice([-1, 1])
    bias_magnitude = rng.uniform(1500, 4000)
    load += bias_direction * bias_magnitude * (pos_norm - 0.5)

    return CardData(position=pos.tolist(), load=load.tolist())


def generate_sand_abrasion_card(seed: int = 0) -> CardData:
    """Sand abrasion: jagged features, noise-like load oscillation."""
    rng = np.random.default_rng(seed)
    high = rng.uniform(12000, 18000)
    low = rng.uniform(4000, 7000)
    stroke = rng.uniform(80, 120)
    pos, load = _base_card(rng, stroke=stroke, high_load=high, low_load=low, noise_pct=0.01)

    # Add high-frequency jagged noise
    n = len(pos)
    jaggedness = rng.uniform(1500, 4000)
    load += rng.uniform(-jaggedness, jaggedness, n)

    return CardData(position=pos.tolist(), load=load.tolist())


def generate_excessive_vibration_card(seed: int = 0) -> CardData:
    """Excessive vibration: sloping wave pattern, high-freq load oscillation."""
    rng = np.random.default_rng(seed)
    high = rng.uniform(12000, 18000)
    low = rng.uniform(4000, 7000)
    stroke = rng.uniform(80, 120)
    pos, load = _base_card(rng, stroke=stroke, high_load=high, low_load=low, noise_pct=0.01)

    # Add high-frequency sinusoidal oscillation
    n = len(pos)
    freq = rng.uniform(6, 12)
    amplitude = rng.uniform(1000, 3000)
    t = np.linspace(0, 2 * np.pi * freq, n)
    load += amplitude * np.sin(t)

    return CardData(position=pos.tolist(), load=load.tolist())


# Registry of all generators keyed by failure mode name
ALL_GENERATORS: dict[str, Callable] = {
    "NORMAL": generate_normal_card,
    "GAS_INTERFERENCE": generate_gas_interference_card,
    "FLUID_POUND": generate_fluid_pound_card,
    "PUMP_TAGGING": generate_pump_tagging_card,
    "TUBING_MOVEMENT": generate_tubing_movement_card,
    "VALVE_LEAK_TV": generate_valve_leak_tv_card,
    "VALVE_LEAK_SV": generate_valve_leak_sv_card,
    "ROD_PARTING": generate_rod_parting_card,
    "STUCK_PUMP": generate_stuck_pump_card,
    "WORN_BARREL": generate_worn_barrel_card,
    "GAS_LOCK": generate_gas_lock_card,
    "DELAYED_TV_CLOSURE": generate_delayed_tv_closure_card,
    "EXCESSIVE_FRICTION": generate_excessive_friction_card,
    "PLUNGER_UNDERTRAVEL": generate_plunger_undertravel_card,
    "PARAFFIN_RESTRICTION": generate_paraffin_restriction_card,
    "BENT_BARREL": generate_bent_barrel_card,
    "SAND_ABRASION": generate_sand_abrasion_card,
    "EXCESSIVE_VIBRATION": generate_excessive_vibration_card,
}


# ---------------------------------------------------------------------------
# Forward model: pump card -> surface card
# ---------------------------------------------------------------------------
#
# The Everitt-Jennings solver marches the damped wave equation DOWN the rod
# string: rows i=0 and i=1 are seeded from the measured surface card and each
# further row is built from the two above it. For a uniform section the
# recurrence is
#
#     u[i+1] = (1 + r) u[i] - r u[i-1] + c1 D2t(u[i]) + c2 D1t(u[i])
#
# with r = EA_minus / EA_plus, and D1t / D2t the first and second differences
# in time. Every term on the right except u[i-1] belongs to row i, so the same
# relation solves for u[i-1] just as explicitly:
#
#     u[i-1] = [ (1 + r) u[i] - u[i+1] + c1 D2t(u[i]) + c2 D1t(u[i]) ] / r
#
# That is an upward march, and it is the exact algebraic inverse of the
# downward one on the same grid -- not an approximation of it. Seeded at the
# pump with the generated card, it produces the surface card that the solver
# will map back onto that same pump card.
#
# The coefficient fields come from the solver's own ``build_simulation`` /
# ``build_coefficients``, never from a re-derivation, so the two directions
# cannot drift apart.

# Only the stroke-direction switch in the damping profile depends on the
# surface card, so the march is iterated to a fixed point on it. It settles in
# two or three passes; the cap only bounds a card that oscillates between two
# neighbouring turnaround samples.
_MAX_TURNAROUND_ITERATIONS = 8


def _march_up(
    u_pump: np.ndarray,
    u_pump_above: np.ndarray,
    sim,
    coeff,
) -> np.ndarray:
    """March displacement from the pump up to the polished rod.

    Args:
        u_pump: Displacement at the pump node, all time steps (solver sign
            convention: positive downward, i.e. the negated card position).
        u_pump_above: Displacement one node above the pump.
        sim: The solver's :class:`Simulation` grid.
        coeff: The solver's space-time :class:`Coefficients`.

    Returns:
        The full ``(n_x, n_t)`` displacement field, row 0 being the surface.
    """
    n_x, n_t = sim.n_x, sim.n_t
    # The last time sample duplicates the first: a stroke closes on itself.
    n_unique = n_t - 1

    u = np.empty((n_x, n_t), dtype=np.float64)
    u[n_x - 1] = u_pump
    u[n_x - 2] = u_pump_above

    e_mod, area, rho_a = coeff.moduli, coeff.areas, coeff.rho_a
    damping = coeff.damping
    dx, dt = sim.dx, sim.dt

    for i in range(n_x - 2, 0, -1):
        ea_plus = (e_mod[i + 1] * area[i + 1] + e_mod[i] * area[i]) / 2.0
        ea_minus = (e_mod[i - 1] * area[i - 1] + e_mod[i] * area[i]) / 2.0
        ratio = ea_minus / ea_plus
        c1 = (rho_a[i] / ea_plus) * (dx / dt) ** 2
        c2 = (rho_a[i] * damping[i] / ea_plus) * dx ** 2 / dt

        row = u[i][:n_unique]
        forward = np.roll(row, -1)
        backward = np.roll(row, 1)
        d2 = np.empty(n_t, dtype=np.float64)
        d1 = np.empty(n_t, dtype=np.float64)
        d2[:n_unique] = forward - 2.0 * row + backward
        d1[:n_unique] = (forward - backward) / 2.0
        d2[n_t - 1] = d2[0]
        d1[n_t - 1] = d1[0]

        u[i - 1] = (
            (1.0 + ratio) * u[i] - u[i + 1] + c1 * d2 + c2 * d1
        ) / ratio
        u[i - 1][n_t - 1] = u[i - 1][0]

    return u


def surface_card_from_pump_card(
    pump_card: CardData,
    ctx: "DynacardAnalysisContext",
    tubing_id_in: Optional[float] = None,
    viscosity_cp: float = 10.0,
    n_nodes: int = 200,
    zero_position_datum: bool = True,
) -> CardData:
    """Synthesise the surface card that a given pump card would produce.

    This is the inverse of the Everitt-Jennings surface-to-downhole solve, run
    on the identical grid and coefficient fields, so
    ``solve_downhole_card(ctx_with_this_surface_card)`` returns ``pump_card``
    again (up to the solver's output smoothing and the position datum).

    Args:
        pump_card: Downhole card, position in inches and load in pounds --
            typically straight from one of the generators above.
        ctx: Analysis context supplying the rod taper, pump bore, speed,
            fluid density and survey. Its ``surface_card`` is ignored; that is
            the field being computed.
        tubing_id_in: Tubing inner diameter, inches. Defaults to the solver
            adapter's value. Affects the damping estimate only, but it must
            match what the downward solve will use.
        viscosity_cp: Produced-fluid viscosity, centipoise. Same requirement.
        n_nodes: Spatial nodes down the rod string. Same requirement.
        zero_position_datum: Shift the surface card so its lowest point is
            zero, the convention a surface instrument records in. The shift is
            a rigid translation of the displacement field, so it moves the
            recovered pump card by the same constant -- the static rod stretch
            -- and changes nothing else about either card.

    Returns:
        The surface card in oilfield units (inches, pounds).

    Raises:
        ValueError: If the context has no rod sections, or the card is too
            short to march.
    """
    # Imported lazily: this pulls in scipy, and most callers of this module
    # only ever want the generators.
    from .everitt_jennings import units as U
    from .everitt_jennings.adapter import (
        DEFAULT_TUBING_ID_IN,
        rod_string_from_context,
        survey_from_context,
    )
    from .everitt_jennings.damping import estimate_damping_profile
    from .everitt_jennings.solver import build_coefficients, build_simulation

    if tubing_id_in is None:
        tubing_id_in = DEFAULT_TUBING_ID_IN

    rods = rod_string_from_context(ctx)
    survey = survey_from_context(ctx, rods)
    damping = estimate_damping_profile(
        rod_diameters=rods.diameters,
        coupling_diameters=rods.coupling_diameters,
        taper_lengths=rods.lengths,
        rod_densities=rods.densities,
        pump_diameter=ctx.pump.diameter * U.IN2M,
        tubing_id=tubing_id_in * U.IN2M,
        viscosity=viscosity_cp * 1.0e-3,
    )

    pump_position_m = np.asarray(pump_card.position, dtype=np.float64) * U.IN2M
    pump_load_n = np.asarray(pump_card.load, dtype=np.float64) * U.LB2N

    # The grid depends on the surface card only through the index at which the
    # stroke turns around, which selects upstroke from downstroke damping. Seed
    # that with the pump card and iterate to a fixed point: the card is
    # self-consistent once the turnaround read off the card we produced is the
    # one that was used to produce it.
    guess_position, guess_load = pump_position_m, pump_load_n
    turnaround_used = None
    surface_position = surface_load = None

    for _ in range(_MAX_TURNAROUND_ITERATIONS):
        sim = build_simulation(
            position=guess_position,
            load=guess_load,
            rods=rods,
            strokes_per_minute=ctx.spm,
            fluid_density=ctx.fluid_density * U.LBPFT32KGPM3,
            damping=damping,
            friction_coefficient=ctx.calc_params.coulomb_friction_coefficient,
            n_nodes=n_nodes,
            include_gravity=False,
        )
        # ``sim.up_dn`` is the turnaround of ``guess_position``, so on every
        # pass but the first this is the turnaround of the card the previous
        # pass produced.
        if sim.up_dn == turnaround_used:
            break
        turnaround_used = sim.up_dn

        coeff = build_coefficients(sim, survey)
        last = sim.n_x - 1

        # Solver sign convention is positive-down, and the downward march reads
        # the pump load off the strain between the last two nodes.
        u_pump = -pump_position_m
        u_pump_above = u_pump - pump_load_n * sim.dx / (
            coeff.moduli[last] * coeff.areas[last]
        )
        u = _march_up(u_pump, u_pump_above, sim, coeff)

        surface_position = -u[0]
        # Mirror of the downward solve's boundary row: with gravity muted in
        # the kernel, the buoyant string weight lives at the surface boundary
        # and nowhere else, so it goes back on here.
        surface_load = (coeff.moduli[0] * coeff.areas[0] / sim.dx) * (
            u[1] - u[0]
        ) + sim.buoyant_weight

        guess_position, guess_load = surface_position, surface_load

    position_in = surface_position * U.M2IN
    if zero_position_datum:
        position_in = position_in - np.min(position_in)

    return CardData(
        position=position_in.tolist(),
        load=(surface_load * U.N2LB).tolist(),
    )


def generate_training_dataset(
    samples_per_mode: int = 200,
) -> tuple[list[CardData], list[str]]:
    """Generate a full training dataset with all 18 failure modes.

    Args:
        samples_per_mode: Number of synthetic cards per failure mode.

    Returns:
        Tuple of (cards, labels).
    """
    cards: List[CardData] = []
    labels: List[str] = []

    for mode_name, gen_func in ALL_GENERATORS.items():
        for i in range(samples_per_mode):
            card = gen_func(seed=i)
            cards.append(card)
            labels.append(mode_name)

    return cards, labels
