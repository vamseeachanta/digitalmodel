# ABOUTME: Synthetic dynacard generators for all 20 pump failure modes.
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

# Half-width, in crank radians, of the window over which fluid load transfers
# between the standing and travelling valves. A real transfer is not
# instantaneous and it straddles the stroke turnaround: rod stretch has to be
# taken up while the polished rod is still decelerating. That is why a
# reference card's corners are visibly cut away rather than square.
#
# Calibrated against the CC0 diagnosis plate (see the regression test fixture
# under tests/.../dynacard/testdata/): the plate's normal card clears its
# bounding-box corners by 0.12-0.15 of the normalised diagonal. A 5-sample
# linear ramp -- the value this replaced -- clears them by 0.03-0.05, which is
# a squarer card than any pump makes, and Bezerra vertical-projection features
# read exactly that geometry.
_TRANSITION_PHASE = 0.85


def _smoothstep(u: np.ndarray | float) -> np.ndarray:
    """Hermite 0->1 ramp with zero slope at both ends, clamped outside [0, 1]."""
    u = np.clip(u, 0.0, 1.0)
    return u * u * (3.0 - 2.0 * u)


def _crank_phase(n: int = _NUM_POINTS) -> np.ndarray:
    """Crank angle for one stroke: 0 -> pi upstroke, pi -> 2pi downstroke.

    The first half of the returned array is the upstroke and the second half
    the downstroke, which is the indexing contract every generator here relies
    on when it reaches for ``load[:half]`` or ``load[half:]``.
    """
    half = n // 2
    return np.concatenate(
        [np.linspace(0, np.pi, half), np.linspace(np.pi, 2 * np.pi, half)]
    )


def _position(theta: np.ndarray, stroke: float) -> np.ndarray:
    """Simple-harmonic plunger position for a crank angle array."""
    return stroke / 2.0 * (1.0 - np.cos(theta))


def _position_fraction(theta: np.ndarray) -> np.ndarray:
    """Plunger position as a 0-1 fraction of stroke."""
    return (1.0 - np.cos(theta)) / 2.0


def _blend_branches(
    theta: np.ndarray,
    up_fraction: np.ndarray | float,
    down_fraction: np.ndarray | float,
    width: float = _TRANSITION_PHASE,
) -> np.ndarray:
    """Blend an upstroke and a downstroke load profile across the turnarounds.

    Args:
        theta: Crank angle, as returned by :func:`_crank_phase`.
        up_fraction: Load carried on the upstroke, as a fraction of the card's
            load range. Scalar or per-sample.
        down_fraction: Same, for the downstroke.
        width: Half-width of the transfer window in crank radians.

    Returns:
        The blended load fraction, one value per sample.

    The blend weight is a smoothstep centred *on* each turnaround rather than
    starting at it, so the load is already part-way transferred when the
    plunger reverses. That is what rounds the card's corners.
    """
    wrapped = np.mod(theta, 2 * np.pi)
    to_bottom = np.where(wrapped > np.pi, wrapped - 2 * np.pi, wrapped)
    to_top = wrapped - np.pi

    rising = _smoothstep((to_bottom + width) / (2 * width))
    falling = _smoothstep((to_top + width) / (2 * width))
    # Near the bottom turnaround the rising edge governs; near the top, the
    # falling one. Away from both, each saturates and the choice is moot.
    weight_up = np.where(np.abs(to_bottom) <= np.abs(to_top), rising, 1.0 - falling)

    return weight_up * up_fraction + (1.0 - weight_up) * down_fraction


def _base_card(
    rng: np.random.Generator,
    stroke: float = 100.0,
    high_load: float = 15000.0,
    low_load: float = 5000.0,
    noise_pct: float = 0.02,
    up_fraction: np.ndarray | float = 1.0,
    down_fraction: np.ndarray | float = 0.0,
    transition_phase: float = _TRANSITION_PHASE,
) -> tuple[np.ndarray, np.ndarray]:
    """Generate base pump card arrays with rounded corners and noise.

    Args:
        rng: Random generator, for the load noise.
        stroke: Plunger stroke, inches.
        high_load: Load with the full fluid column on the plunger, pounds.
        low_load: Load with the fluid column on the standing valve, pounds.
        noise_pct: Load noise as a fraction of ``high_load - low_load``.
        up_fraction: Fraction of the load range carried on the upstroke.
            Pass an array to shape the upstroke branch (see the
            plunger-out-of-barrel generator).
        down_fraction: Same, for the downstroke. Gas interference and fluid
            pound both drive their signature through this.
        transition_phase: Half-width of the valve-transfer window, radians.

    Returns:
        ``(position, load)`` in inches and pounds.
    """
    theta = _crank_phase()
    pos = _position(theta, stroke)

    load_range = high_load - low_load
    fraction = _blend_branches(theta, up_fraction, down_fraction, transition_phase)
    load = low_load + load_range * fraction

    load += rng.normal(0, noise_pct * load_range, len(theta))

    return pos, load


def generate_normal_card(seed: int = 0, noise_pct: float = 0.03) -> CardData:
    """Normal operation: clean rectangular card, moderate loads."""
    rng = np.random.default_rng(seed)
    high = rng.uniform(12000, 18000)
    low = rng.uniform(4000, 7000)
    stroke = rng.uniform(80, 120)
    pos, load = _base_card(rng, stroke=stroke, high_load=high, low_load=low, noise_pct=noise_pct)
    return CardData(position=pos.tolist(), load=load.tolist())


def _late_transfer_downstroke(
    theta: np.ndarray,
    shelf: float,
    hold_to: float,
    release_at: float,
) -> np.ndarray:
    """Downstroke load fraction for a pump that sheds fluid load late.

    Both gas interference and fluid pound leave the travelling valve shut for
    part of the downstroke, so the rods keep carrying fluid load well past the
    top of the stroke and only shed it lower down. That gouges the card's
    lower-right corner -- the single feature that separates either of them from
    a normal card.

    Args:
        theta: Crank angle array.
        shelf: Load fraction still carried at the top of the downstroke.
        hold_to: Position fraction down to which ``shelf`` is held flat.
        release_at: Position fraction at which the load has fully transferred.

    Returns:
        Per-sample downstroke load fraction.
    """
    p = _position_fraction(theta)
    return shelf * _smoothstep((p - release_at) / (hold_to - release_at))


def generate_gas_interference_card(seed: int = 0, severity: float = 0.5) -> CardData:
    """Gas interference: fluid load shed gradually down the downstroke.

    Free gas in the barrel has to be compressed back up to discharge pressure
    before the travelling valve can open, and compression is gradual, so the
    load bleeds off over a long slanted branch instead of dropping at the top
    of the stroke. The card's lower-right corner is rounded away and the lower
    branch slopes upward with position -- the reference plate measures corner
    clearance 0.65 and lower-branch slope +0.35 there.

    Args:
        seed: Random seed.
        severity: 0-1. Scales how much load survives into the downstroke and
            how far down the stroke the travelling valve finally opens.
    """
    rng = np.random.default_rng(seed)
    high = rng.uniform(10000, 16000)
    low = rng.uniform(500, 2500)  # Very low min load
    stroke = rng.uniform(80, 120)
    theta = _crank_phase()

    severity = float(np.clip(severity, 0.0, 1.0))
    shelf = (0.52 + 0.28 * severity) * rng.uniform(0.94, 1.06)
    hold_to = rng.uniform(0.64, 0.76)
    release_at = (0.34 - 0.12 * severity) * rng.uniform(0.88, 1.12)

    down = _late_transfer_downstroke(theta, shelf, hold_to, release_at)
    pos, load = _base_card(
        rng, stroke=stroke, high_load=high, low_load=low, noise_pct=0.015,
        down_fraction=down,
    )

    return CardData(position=pos.tolist(), load=load.tolist())


def generate_fluid_pound_card(seed: int = 0, drop_position: float = 0.35) -> CardData:
    """Fluid pound: full fluid load held into the downstroke, then collapsed.

    With the barrel only part filled, the plunger falls through empty space
    carrying the full fluid load until it slaps the fluid surface; the load
    then collapses almost vertically. The signature is the same gutted
    lower-right corner as gas interference, but the collapse is abrupt rather
    than gradual -- that width is the whole difference between the two classes.

    Args:
        seed: Random seed.
        drop_position: Position fraction, 0-1, at which the plunger meets
            fluid. Lower means poorer fillage.
    """

    rng = np.random.default_rng(seed)
    high = rng.uniform(12000, 18000)
    low = rng.uniform(4000, 7000)
    stroke = rng.uniform(80, 120)
    theta = _crank_phase()

    drop_position = float(np.clip(drop_position, 0.15, 0.85))
    shelf = rng.uniform(0.74, 0.82)
    # The collapse is abrupt -- under a fifth of the stroke, against better
    # than 0.4 of it for gas interference. That width is the whole difference
    # between the two classes and the reason they are not aliases.
    width = rng.uniform(0.15, 0.21)
    hold_to = drop_position + width / 2
    release_at = drop_position - width / 2

    # A starved barrel lets fluid slip past the plunger for the whole
    # upstroke, so the top of the card droops slightly as the stroke proceeds
    # (plate upper-branch slope -0.12, against +0.01 for a normal card).
    droop = rng.uniform(0.10, 0.18)
    p = _position_fraction(theta)
    up = 1.0 - droop * _smoothstep((p - 0.15) / 0.6)

    down = _late_transfer_downstroke(theta, shelf, hold_to, release_at)
    pos, load = _base_card(
        rng, stroke=stroke, high_load=high, low_load=low, noise_pct=0.015,
        up_fraction=up, down_fraction=down,
    )

    return CardData(position=pos.tolist(), load=load.tolist())


def _tagging_spike(theta: np.ndarray, at_top: bool, width_phase: float) -> np.ndarray:
    """Unit-height impact spike at one end of the stroke."""
    centre = np.pi if at_top else 0.0
    d = np.abs(np.mod(theta - centre + np.pi, 2 * np.pi) - np.pi)
    return np.clip(1.0 - d / width_phase, 0.0, 1.0) ** 2


def generate_pump_tagging_up_card(seed: int = 0) -> CardData:
    """Pump tagging up: impact spike at the top of the stroke.

    The plunger strikes the top of the barrel or the pull tube, so the load
    spikes at maximum position. On the reference plate the spike stands about
    a tenth of the card's load range above the upstroke plateau -- it is a
    distinct peak, not a doubling of the card, and the card body underneath it
    stays a recognisable card (plate fill ratio 0.72).
    """
    rng = np.random.default_rng(seed)
    high = rng.uniform(14000, 19000)
    low = rng.uniform(4000, 7000)
    stroke = rng.uniform(80, 120)
    theta = _crank_phase()

    # While the plunger is jammed against the barrel top the fluid load has
    # nowhere to go, so it does not transfer at the turnaround at all; it comes
    # off as the plunger backs away, over the top quarter of the stroke. That
    # is what opens the card's lower-right corner -- plate clearance 0.25,
    # against 0.15 for a normal card.
    down = _late_transfer_downstroke(
        theta,
        shelf=rng.uniform(0.94, 1.00),
        hold_to=1.0,
        release_at=rng.uniform(0.70, 0.78),
    )
    pos, load = _base_card(
        rng, stroke=stroke, high_load=high, low_load=low, noise_pct=0.015,
        down_fraction=down,
    )
    # A tenth of the card's load range above the plateau, per the plate.
    spike = rng.uniform(0.16, 0.24) * (high - low)
    load += spike * _tagging_spike(theta, at_top=True, width_phase=0.30)

    return CardData(position=pos.tolist(), load=load.tolist())


def generate_pump_tagging_down_card(seed: int = 0) -> CardData:
    """Pump tagging down: compression spike at the bottom of the stroke.

    The plunger strikes the standing valve or the bottom of the barrel, so the
    rods go into compression at minimum position and the load dips sharply
    *below* the downstroke load line. It is the mirror of tagging up and a
    different repair -- re-space up rather than down -- which is why the two
    are separate classes here.
    """
    rng = np.random.default_rng(seed)
    high = rng.uniform(14000, 19000)
    low = rng.uniform(4000, 7000)
    stroke = rng.uniform(80, 120)
    theta = _crank_phase()

    down = _late_transfer_downstroke(
        theta,
        shelf=rng.uniform(0.24, 0.34),
        hold_to=1.0,
        release_at=rng.uniform(0.70, 0.80),
    )
    pos, load = _base_card(
        rng, stroke=stroke, high_load=high, low_load=low, noise_pct=0.015,
        down_fraction=down,
    )
    # Deep enough to take the load below the downstroke line: the impact puts
    # the rods into compression, so minimum load coincides with minimum
    # position. The plate's bottom-left corner clearance is 0.01 -- the card
    # reaches its own corner, which a normal card never does.
    dip = rng.uniform(0.62, 0.78) * (high - low)
    load -= dip * _tagging_spike(theta, at_top=False, width_phase=0.60)

    return CardData(position=pos.tolist(), load=load.tolist())


def generate_pump_tagging_card(seed: int = 0) -> CardData:
    """Deprecated alias for :func:`generate_pump_tagging_up_card`.

    ``PUMP_TAGGING`` was one class covering two opposite mechanisms; it is now
    ``PUMP_TAGGING_UP`` and ``PUMP_TAGGING_DOWN``. This alias resolves to the
    up case, which is the shape the old generator drew.
    """
    return generate_pump_tagging_up_card(seed=seed)


def generate_plunger_out_of_barrel_card(seed: int = 0) -> CardData:
    """Plunger out of barrel: fluid load dumped part-way up the upstroke.

    The pump is spaced so long that the plunger leaves the top of the barrel
    before the upstroke ends. Fluid load is lost the moment it clears, so the
    upstroke falls off a cliff mid-stroke and runs flat and low to the top --
    the top-right corner of the card is missing entirely (plate corner
    clearance 0.46 there, against 0.07 for a normal card).
    """
    rng = np.random.default_rng(seed)
    high = rng.uniform(12000, 18000)
    low = rng.uniform(4000, 7000)
    stroke = rng.uniform(80, 120)
    theta = _crank_phase()
    p = _position_fraction(theta)

    exit_at = rng.uniform(0.46, 0.58)      # where the plunger clears the barrel
    exit_width = rng.uniform(0.18, 0.26)   # how fast the fluid load dumps
    residual = rng.uniform(0.16, 0.26)     # load left once the fluid is gone

    up = 1.0 - (1.0 - residual) * _smoothstep((p - exit_at) / exit_width)
    pos, load = _base_card(
        rng, stroke=stroke, high_load=high, low_load=low, noise_pct=0.015,
        up_fraction=up,
    )

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
    """Traveling valve leak: load builds slowly up the upstroke.

    Fluid bypasses the travelling valve at a rate set by the pressure across
    it, while the rate the plunger *displaces* fluid is set by its velocity --
    which is zero at the turnaround. So near the bottom of the upstroke the
    leak wins outright and no load develops; the load only builds as the
    plunger speeds up. The card's top-left corner is rounded away while its
    lower-right stays square (plate clearances 0.20 and 0.03).
    """
    rng = np.random.default_rng(seed)
    high = rng.uniform(12000, 18000)
    low = rng.uniform(4000, 7000)
    stroke = rng.uniform(80, 120)
    theta = _crank_phase()
    p = _position_fraction(theta)

    leak_rate = rng.uniform(0.30, 0.55)     # peak load deficit, fraction of range
    recovered_by = rng.uniform(0.28, 0.45)  # position at which the load is full
    up = 1.0 - leak_rate * (1.0 - _smoothstep(p / recovered_by))

    pos, load = _base_card(
        rng, stroke=stroke, high_load=high, low_load=low, noise_pct=0.015,
        up_fraction=up,
    )

    return CardData(position=pos.tolist(), load=load.tolist())


def generate_valve_leak_sv_card(seed: int = 0) -> CardData:
    """Standing valve leak: load sheds slowly down the downstroke.

    The mirror of a travelling-valve leak. Fluid bypasses the standing valve
    instead, so the standing valve cannot take the fluid column back off the
    rods at the top of the stroke; the rods keep carrying part of it until the
    plunger is moving down fast enough to out-run the leak. The lower-right
    corner opens up (plate clearance 0.23) while the top stays square.
    """
    rng = np.random.default_rng(seed)
    high = rng.uniform(12000, 18000)
    low = rng.uniform(4000, 7000)
    stroke = rng.uniform(80, 120)
    theta = _crank_phase()

    down = _late_transfer_downstroke(
        theta,
        shelf=rng.uniform(0.22, 0.38),
        hold_to=rng.uniform(0.92, 1.00),
        release_at=rng.uniform(0.55, 0.72),
    )
    pos, load = _base_card(
        rng, stroke=stroke, high_load=high, low_load=low, noise_pct=0.015,
        down_fraction=down,
    )

    return CardData(position=pos.tolist(), load=load.tolist())


def generate_rod_parting_card(seed: int = 0) -> CardData:
    """Rod parting: a thin but proper card loop at very low load.

    Below the break nothing is lifted, so the surviving string carries only its
    own buoyant weight plus friction and inertia. That still traces a closed
    loop -- friction opens it, and rod inertia (which for simple harmonic
    motion is proportional to displacement from mid-stroke) tilts both branches
    upward with position. What makes it diagnosable is the *scale*: the plate
    exemplar's load range is 0.24 of its mean load, against 1.9 for a normal
    card. Absolute load, not shape, is the tell.

    The previous version drew ``load = mean + A*sin(2t)`` against
    ``pos = (1-cos t)/2``, a 1:2 Lissajous figure-eight enclosing essentially
    no area (fill ratio 0.01 against the plate's 0.73). It was not a card.
    """
    rng = np.random.default_rng(seed)
    stroke = rng.uniform(80, 120)
    theta = _crank_phase()

    mean_load = rng.uniform(1000, 3000)
    # Total load range, as a fraction of mean load, matching the plate.
    total_range = rng.uniform(0.18, 0.30) * mean_load
    # Split between the friction loop (its height) and the inertial tilt.
    tilt_share = rng.uniform(0.30, 0.42)
    hysteresis = total_range * (1.0 - tilt_share)
    tilt = total_range * tilt_share

    pos, load = _base_card(
        rng,
        stroke=stroke,
        high_load=mean_load + hysteresis / 2,
        low_load=mean_load - hysteresis / 2,
        noise_pct=0.02,
    )
    load += tilt * (_position_fraction(theta) - 0.5)

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
    """Sand abrasion: jagged load trace over a slightly slanted card.

    Sand grinds the plunger-barrel clearance open, so the card carries two
    marks: a jagged load trace from grains passing through the fit, and a
    slant, because a worn fit slips at a rate that follows the differential
    across the plunger. The jaggedness is the diagnostic feature and it is
    stochastic -- see the module's regression test for why that means this
    class is checked on its texture statistic rather than on loop shape.
    """
    rng = np.random.default_rng(seed)
    high = rng.uniform(12000, 18000)
    low = rng.uniform(4000, 7000)
    stroke = rng.uniform(80, 120)
    theta = _crank_phase()
    pos, load = _base_card(
        rng, stroke=stroke, high_load=high, low_load=low, noise_pct=0.01,
    )

    # Worn-fit slip: load tracks position on both branches (plate slopes
    # +0.05 upstroke, +0.10 downstroke).
    slant = rng.uniform(0.13, 0.19) * (high - low)
    load += slant * (_position_fraction(theta) - 0.5)

    # Grain-by-grain jaggedness.
    jaggedness = rng.uniform(0.09, 0.13) * (high - low)
    load += rng.uniform(-jaggedness, jaggedness, len(load))

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


# Registry of all generators keyed by failure mode name.
#
# 20 modes. ``PUMP_TAGGING`` split into ``PUMP_TAGGING_UP`` and
# ``PUMP_TAGGING_DOWN`` -- opposite mechanisms with opposite repairs that were
# sharing one label and one generator -- and ``PLUNGER_OUT_OF_BARREL`` was
# added, having previously had a name in the literature but no generator.
ALL_GENERATORS: dict[str, Callable] = {
    "NORMAL": generate_normal_card,
    "GAS_INTERFERENCE": generate_gas_interference_card,
    "FLUID_POUND": generate_fluid_pound_card,
    "PUMP_TAGGING_UP": generate_pump_tagging_up_card,
    "PUMP_TAGGING_DOWN": generate_pump_tagging_down_card,
    "PLUNGER_OUT_OF_BARREL": generate_plunger_out_of_barrel_card,
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


# Retired mode names, and what they now resolve to. The shipped classifier
# model was trained before the split and still emits ``PUMP_TAGGING``, and
# stored workflow configs still request it by that name.
MODE_ALIASES: dict[str, str] = {
    "PUMP_TAGGING": "PUMP_TAGGING_UP",
}


def resolve_mode(mode: str) -> str:
    """Map a possibly-retired failure-mode name onto a current one."""
    return MODE_ALIASES.get(mode, mode)


def get_generator(mode: str) -> Callable:
    """Look up a generator by failure-mode name, honouring retired names.

    Args:
        mode: Failure mode name, current or retired.

    Returns:
        The generator callable.

    Raises:
        KeyError: If the name is neither a current mode nor a known alias.
    """
    return ALL_GENERATORS[resolve_mode(mode)]


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
    """Generate a full training dataset with all 20 failure modes.

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
