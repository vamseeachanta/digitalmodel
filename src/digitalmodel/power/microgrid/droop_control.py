"""Droop control simulator — governor/AVR droop, parallel load sharing.

Models frequency-power and voltage-reactive-power droop characteristics
for synchronous generators operating in parallel on an islanded microgrid.

References
----------
IEEE 1547.4-2011 §5 — Frequency and voltage regulation for island systems.
"""

from __future__ import annotations

from dataclasses import dataclass


@dataclass
class GovernorDroop:
    """Governor droop characteristic for active power control.

    Parameters
    ----------
    rated_power_kw : float
        Generator rated active power [kW].
    droop_percent : float
        Speed droop setting [%]. Must be in [0, 10]. Typical: 4-5%.
    nominal_freq_hz : float
        Nominal system frequency [Hz]. Default 60.0.
    """

    rated_power_kw: float
    droop_percent: float
    nominal_freq_hz: float = 60.0

    def __post_init__(self) -> None:
        if self.droop_percent < 0 or self.droop_percent > 10:
            raise ValueError(
                f"droop_percent must be in [0, 10], got {self.droop_percent}"
            )


@dataclass
class AVRDroop:
    """AVR droop characteristic for reactive power control.

    Parameters
    ----------
    rated_reactive_kvar : float
        Generator rated reactive power [kVAR].
    droop_percent : float
        Voltage droop setting [%]. Must be in [0, 10].
    nominal_voltage_v : float
        Nominal system voltage [V]. Default 480.0.
    """

    rated_reactive_kvar: float
    droop_percent: float
    nominal_voltage_v: float = 480.0

    def __post_init__(self) -> None:
        if self.droop_percent < 0 or self.droop_percent > 10:
            raise ValueError(
                f"droop_percent must be in [0, 10], got {self.droop_percent}"
            )


@dataclass
class Generator:
    """Generator with governor and AVR droop settings.

    Parameters
    ----------
    gen_id : str
        Unique identifier.
    governor : GovernorDroop
        Governor droop characteristic.
    avr : AVRDroop
        AVR droop characteristic.
    is_isochronous : bool
        If True, generator holds nominal frequency. Default False.
    """

    gen_id: str
    governor: GovernorDroop
    avr: AVRDroop
    is_isochronous: bool = False


def power_at_frequency(governor: GovernorDroop, frequency_hz: float) -> float:
    """Compute active power output at a given frequency.

    Uses the linear droop equation:
        P = rated_power * (f_nom - f) / (f_nom * droop/100)

    Clamped to [0, rated_power].

    Parameters
    ----------
    governor : GovernorDroop
        Governor droop settings.
    frequency_hz : float
        System frequency [Hz].

    Returns
    -------
    float
        Active power output [kW].
    """
    if governor.droop_percent == 0:
        return 0.0
    f_nom = governor.nominal_freq_hz
    droop_frac = governor.droop_percent / 100.0
    delta_f = f_nom - frequency_hz
    p = governor.rated_power_kw * delta_f / (f_nom * droop_frac)
    return max(0.0, min(governor.rated_power_kw, p))


def reactive_at_voltage(avr: AVRDroop, voltage_v: float) -> float:
    """Compute reactive power output at a given voltage.

    Uses the linear droop equation:
        Q = rated_reactive * (v_nom - v) / (v_nom * droop/100)

    Clamped to [0, rated_reactive].

    Parameters
    ----------
    avr : AVRDroop
        AVR droop settings.
    voltage_v : float
        System voltage [V].

    Returns
    -------
    float
        Reactive power output [kVAR].
    """
    if avr.droop_percent == 0:
        return 0.0
    v_nom = avr.nominal_voltage_v
    droop_frac = avr.droop_percent / 100.0
    delta_v = v_nom - voltage_v
    q = avr.rated_reactive_kvar * delta_v / (v_nom * droop_frac)
    return max(0.0, min(avr.rated_reactive_kvar, q))


def _total_power_at_freq(
    generators: list[Generator],
    frequency_hz: float,
    iso_load_kw: float,
) -> float:
    """Sum of all generator outputs at a given frequency.

    Isochronous generators contribute ``iso_load_kw`` (the remainder).
    Droop generators contribute per their droop curve.
    """
    total = 0.0
    for gen in generators:
        if gen.is_isochronous:
            continue
        total += power_at_frequency(gen.governor, frequency_hz)
    return total


def parallel_load_sharing(
    generators: list[Generator],
    total_load_kw: float,
    nominal_freq_hz: float = 60.0,
) -> dict:
    """Find equilibrium frequency and generator power outputs.

    For droop-only systems, finds the frequency where sum of generator
    outputs equals total load via bisection. For mixed isochronous/droop
    systems, isochronous generators hold nominal frequency and absorb
    the remaining load after droop generators contribute.

    Parameters
    ----------
    generators : list[Generator]
        List of generators.
    total_load_kw : float
        Total active load to serve [kW].
    nominal_freq_hz : float
        Nominal system frequency [Hz]. Default 60.0.

    Returns
    -------
    dict
        frequency_hz : float
        gen_outputs : list[dict] with gen_id, power_kw, pct_rated
        unserved_kw : float
    """
    if total_load_kw <= 0.0:
        return {
            "frequency_hz": nominal_freq_hz,
            "gen_outputs": [
                {"gen_id": g.gen_id, "power_kw": 0.0, "pct_rated": 0.0}
                for g in generators
            ],
            "unserved_kw": 0.0,
        }

    has_iso = any(g.is_isochronous for g in generators)

    if has_iso:
        return _solve_with_isochronous(generators, total_load_kw, nominal_freq_hz)
    return _solve_droop_only(generators, total_load_kw, nominal_freq_hz)


def _solve_with_isochronous(
    generators: list[Generator],
    total_load_kw: float,
    nominal_freq_hz: float,
) -> dict:
    """Solve load sharing with isochronous generator(s).

    Isochronous generators hold nominal frequency. Droop generators
    output zero at nominal frequency. Isochronous generators absorb
    all remaining load.
    """
    freq = nominal_freq_hz
    droop_total = 0.0
    for gen in generators:
        if not gen.is_isochronous:
            droop_total += power_at_frequency(gen.governor, freq)

    iso_gens = [g for g in generators if g.is_isochronous]
    iso_capacity = sum(g.governor.rated_power_kw for g in iso_gens)

    remaining = total_load_kw - droop_total
    unserved = 0.0
    if remaining > iso_capacity:
        unserved = remaining - iso_capacity
        remaining = iso_capacity

    remaining = max(0.0, remaining)

    outputs = []
    for gen in generators:
        if gen.is_isochronous:
            share = (
                remaining * gen.governor.rated_power_kw / iso_capacity
                if iso_capacity > 0
                else 0.0
            )
            pct = share / gen.governor.rated_power_kw * 100.0
            outputs.append(
                {"gen_id": gen.gen_id, "power_kw": share, "pct_rated": pct}
            )
        else:
            p = power_at_frequency(gen.governor, freq)
            pct = p / gen.governor.rated_power_kw * 100.0
            outputs.append(
                {"gen_id": gen.gen_id, "power_kw": p, "pct_rated": pct}
            )

    return {
        "frequency_hz": freq,
        "gen_outputs": outputs,
        "unserved_kw": unserved,
    }


def _solve_droop_only(
    generators: list[Generator],
    total_load_kw: float,
    nominal_freq_hz: float,
) -> dict:
    """Solve load sharing for droop-only generators via bisection."""
    total_capacity = sum(g.governor.rated_power_kw for g in generators)
    unserved = 0.0
    effective_load = total_load_kw

    if total_load_kw > total_capacity:
        unserved = total_load_kw - total_capacity
        effective_load = total_capacity

    max_droop_pct = max(g.governor.droop_percent for g in generators)
    f_low = nominal_freq_hz * (1.0 - max_droop_pct / 100.0) - 0.1
    f_high = nominal_freq_hz

    for _ in range(200):
        f_mid = (f_low + f_high) / 2.0
        total_gen = sum(
            power_at_frequency(g.governor, f_mid) for g in generators
        )
        if total_gen < effective_load:
            f_high = f_mid
        else:
            f_low = f_mid
        if abs(total_gen - effective_load) < 0.01:
            break

    freq = (f_low + f_high) / 2.0
    outputs = []
    for gen in generators:
        p = power_at_frequency(gen.governor, freq)
        pct = p / gen.governor.rated_power_kw * 100.0
        outputs.append(
            {"gen_id": gen.gen_id, "power_kw": p, "pct_rated": pct}
        )

    return {
        "frequency_hz": freq,
        "gen_outputs": outputs,
        "unserved_kw": unserved,
    }


def recommend_droop_settings(
    generators: list[Generator],
    target_splits: list[float],
) -> list[float]:
    """Recommend droop percentages for target load split ratios.

    For generators with equal rated power, droop is inversely
    proportional to the desired load share. The function normalizes
    so that the average droop equals 5% (typical default).

    Parameters
    ----------
    generators : list[Generator]
        List of generators.
    target_splits : list[float]
        Target load fraction per generator (must sum to 1.0).

    Returns
    -------
    list[float]
        Recommended droop_percent for each generator.
    """
    n = len(generators)
    if n != len(target_splits):
        raise ValueError("generators and target_splits must be same length")

    # For each gen: P_i = rated_i * delta_f / (f_nom * droop_i/100)
    # At equilibrium all share same delta_f, so:
    # P_i / P_j = (rated_i / droop_i) / (rated_j / droop_j)
    # Target: P_i / P_total = split_i
    # So: rated_i / droop_i = k * split_i  =>  droop_i = rated_i / (k * split_i)
    # We choose k so average droop = 5%.

    raw = []
    for gen, split in zip(generators, target_splits):
        if split <= 0:
            raise ValueError("target_splits must be positive")
        raw.append(gen.governor.rated_power_kw / split)

    # Normalize: set average droop to 5%
    target_avg = 5.0
    k = sum(raw) / (n * target_avg)
    droops = [r / k for r in raw]

    # Clamp to valid range
    return [max(0.01, min(10.0, d)) for d in droops]
