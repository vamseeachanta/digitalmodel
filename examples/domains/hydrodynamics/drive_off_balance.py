# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Run an offline force balance with cited textbook inputs in SI units.

Basis: OpenStax, University Physics Volume 1, first edition (2016), section 6.1,
Example 6.6; kinematics: section 3.4, equations 3.12 and 3.13, including 1/2.
Source links and the uniform-acceleration idealisation: drive_off_balance.md.
This integrates one translation from rest; it does not predict physical drag.
"""

from collections.abc import Sequence
from dataclasses import dataclass
from math import isfinite


@dataclass(frozen=True)
class State:
    """Time [s], displacement [m] and velocity [m/s] in the positive x direction."""

    time_s: float
    position_m: float
    velocity_m_s: float


def simulate(
    mass_kg: float,
    times_s: Sequence[float],
    thrust_n: Sequence[float],
    drag_n: Sequence[float],
) -> list[State]:
    """Integrate m*dv/dt = thrust - drag from x=v=0 at t=0.

    Args:
        mass_kg: Positive, constant mass [kg].
        times_s: Strictly increasing finite interval boundaries [s], starting
            at zero, with at least one interval.
        thrust_n: Finite prescribed force components along +x [N], one per
            interval, held constant until its right boundary.
        drag_n: Finite prescribed force components along -x [N], one per
            interval. These are inputs, not a velocity-dependent drag law.

    Returns:
        States at every boundary, including the initial state. Each interval
        uses the exact constant-acceleration kinematics derived in the note.

    Raises:
        ValueError: Mass, time boundaries or interval forces are invalid.
    """
    if not isfinite(mass_kg) or mass_kg <= 0:
        raise ValueError("mass_kg must be finite and positive")
    if len(times_s) < 2 or any(not isfinite(time) for time in times_s):
        raise ValueError("times_s must contain finite interval boundaries")
    if times_s[0] != 0 or any(
        end <= start for start, end in zip(times_s, times_s[1:])
    ):
        raise ValueError("times_s must start at zero and increase strictly")
    intervals = len(times_s) - 1
    if len(thrust_n) != intervals or len(drag_n) != intervals:
        raise ValueError("provide one thrust and drag value per interval")
    if any(not isfinite(force) for force in (*thrust_n, *drag_n)):
        raise ValueError("prescribed forces must be finite")

    states = [State(time_s=0.0, position_m=0.0, velocity_m_s=0.0)]
    for start, end, thrust, drag in zip(
        times_s, times_s[1:], thrust_n, drag_n
    ):
        previous = states[-1]
        dt = end - start
        acceleration = (thrust - drag) / mass_kg
        states.append(
            State(
                time_s=end,
                position_m=(
                    previous.position_m
                    + previous.velocity_m_s * dt
                    + acceleration * dt**2 / 2
                ),
                velocity_m_s=previous.velocity_m_s + acceleration * dt,
            )
        )
    return states


def main() -> None:
    """Print the textbook uniform-acceleration case and algebraic cancellation."""
    # OpenStax (2016), section 6.1, Example 6.6: 70.0 kg, rest to 8.00 m/s
    # in 2.50 s, negligible air resistance; mean force = 70*(8/2.5) = 224 N.
    mass_kg = 70.0
    times_s = [0.0, 0.25, 1.0, 2.5]  # Numerical partition of the cited duration.
    thrust_n = 224.0
    drag_n = 0.0
    intervals = len(times_s) - 1
    states = simulate(
        mass_kg, times_s, [thrust_n] * intervals, [drag_n] * intervals
    )
    print("OpenStax (2016), section 6.1, Example 6.6; uniform-acceleration idealisation.")
    print(f"Mass: {mass_kg:.1f} kg; thrust: {thrust_n:.1f} N; drag: {drag_n:.1f} N")
    print("Rest initial state: t=0.0 s, x=0.0 m, v=0.0 m/s.")
    print("Analytical limit: a=(224.0-0.0)/70.0=3.20 m/s^2; v=a*t; x=a*t^2/2.")
    print("time [s]  velocity [m/s]  displacement [m]")
    for state in states:
        print(f"{state.time_s:8.3f} {state.velocity_m_s:15.3f} {state.position_m:17.3f}")
    # Algebraic cancellation using the cited force; not a published motion case.
    balanced = simulate(
        mass_kg, times_s, [thrust_n] * intervals, [thrust_n] * intervals
    )
    print(
        f"Algebraic cancellation ({thrust_n:.1f} N each): "
        f"final v={balanced[-1].velocity_m_s:.3f} m/s, "
        f"x={balanced[-1].position_m:.3f} m."
    )
    print("Omitted: other motions, added mass, wave/wind/current loads and restraints.")
    print("Ramp-up, drag and shutdown histories require evidence; see the method note.")


if __name__ == "__main__":
    main()
