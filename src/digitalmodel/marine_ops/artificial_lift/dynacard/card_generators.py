# ABOUTME: Synthetic dynacard generators for all 18 pump failure modes.
# ABOUTME: Used to train the ML-based diagnostic classifier.

from __future__ import annotations

from typing import Callable

import numpy as np

from .models import CardData

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
