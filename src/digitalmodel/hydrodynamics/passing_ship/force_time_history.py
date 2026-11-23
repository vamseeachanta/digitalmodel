"""ForceTimeHistory: time series of passing ship forces (WRK-131 Phase 2).

As a ship passes, it moves along the track.  At each time step the relative
longitudinal position (stagger) between the ships changes as::

    stagger(t) = x_start + U * t

where ``U`` is the passing ship speed and ``x_start`` is the initial position.
Forces are computed at each stagger value using PassingShipCalculator.

Typical usage::

    from digitalmodel.hydrodynamics.passing_ship.calculator import (
        PassingShipCalculator,
    )
    from digitalmodel.hydrodynamics.passing_ship.force_time_history import (
        ForceTimeHistory,
        generate_time_history,
    )

    calc = PassingShipCalculator(moored_vessel, passing_vessel)
    fth = generate_time_history(calc, speed_ms=2.0, separation_m=100.0)

    print(fth.peak_sway)
    df = fth.to_dataframe()
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Union

import numpy as np


@dataclass
class ForceTimeHistory:
    """Time series of surge/sway/yaw forces from a single passing ship event.

    Attributes:
        time:       Time array (s), shape ``(N,)``. Starts at 0.
        surge:      Surge force array (N), shape ``(N,)``.
        sway:       Sway force array (N), shape ``(N,)``.
        yaw:        Yaw moment array (N·m), shape ``(N,)``.
        stagger:    Longitudinal stagger array (m), shape ``(N,)``.
        separation: Lateral separation used (m).
        speed:      Passing ship speed used (m/s).
    """

    time: np.ndarray
    surge: np.ndarray
    sway: np.ndarray
    yaw: np.ndarray
    stagger: np.ndarray
    separation: float
    speed: float

    # ------------------------------------------------------------------
    # Scalar peak properties
    # ------------------------------------------------------------------

    @property
    def peak_surge(self) -> float:
        """Absolute peak surge force (N)."""
        return float(np.max(np.abs(self.surge)))

    @property
    def peak_sway(self) -> float:
        """Absolute peak sway force (N)."""
        return float(np.max(np.abs(self.sway)))

    @property
    def peak_yaw(self) -> float:
        """Absolute peak yaw moment (N·m)."""
        return float(np.max(np.abs(self.yaw)))

    # ------------------------------------------------------------------
    # Export helpers
    # ------------------------------------------------------------------

    def to_dataframe(self):
        """Return forces as a pandas DataFrame.

        Columns: ``time``, ``surge``, ``sway``, ``yaw``, ``stagger``.

        Returns:
            pandas.DataFrame with one row per time step.
        """
        import pandas as pd

        return pd.DataFrame(
            {
                "time": self.time,
                "surge": self.surge,
                "sway": self.sway,
                "yaw": self.yaw,
                "stagger": self.stagger,
            }
        )

    def to_csv(self, path: Union[str, Path]) -> Path:
        """Write the time history to a CSV file.

        Args:
            path: Destination path for the CSV file.

        Returns:
            Path object pointing to the written file.
        """
        path = Path(path)
        df = self.to_dataframe()
        df.to_csv(path, index=False)
        return path


# ---------------------------------------------------------------------------
# Generator function
# ---------------------------------------------------------------------------


def generate_time_history(
    calculator,
    speed_ms: float,
    separation_m: float,
    dt: float = 1.0,
    x_start_factor: float = -2.0,
    x_end_factor: float = 2.0,
) -> ForceTimeHistory:
    """Generate a force time history for a single passing ship event.

    The passing ship starts at ``x_start = x_start_factor * LOA_moored``
    forward of the moored vessel and traverses to
    ``x_end = x_end_factor * LOA_moored``, advancing by
    ``speed_ms * dt`` metres per step.

    Args:
        calculator:     Configured PassingShipCalculator instance.
        speed_ms:       Passing ship speed (m/s); must be positive.
        separation_m:   Lateral centreline-to-centreline separation (m);
                        must be positive.
        dt:             Time step (s); default 1.0 s.
        x_start_factor: Multiple of moored LOA for the initial stagger;
                        typically -2.0 (ship starts 2 LOA astern).
        x_end_factor:   Multiple of moored LOA for the final stagger;
                        typically +2.0 (ship ends 2 LOA ahead).

    Returns:
        ForceTimeHistory containing the complete time series.

    Raises:
        ValueError: If ``speed_ms`` or ``separation_m`` are not positive,
                    or if ``dt`` is not positive.
    """
    if speed_ms <= 0:
        raise ValueError(f"speed_ms must be positive, got {speed_ms}")
    if separation_m <= 0:
        raise ValueError(f"separation_m must be positive, got {separation_m}")
    if dt <= 0:
        raise ValueError(f"dt must be positive, got {dt}")

    loa = calculator.moored_vessel.length
    x_start = x_start_factor * loa
    x_end = x_end_factor * loa

    step = speed_ms * dt
    stagger_values = np.arange(x_start, x_end, step)

    if len(stagger_values) == 0:
        # Degenerate case: ensure at least one point at x_start
        stagger_values = np.array([x_start])

    times = (stagger_values - x_start) / speed_ms

    surges: list[float] = []
    sways: list[float] = []
    yaws: list[float] = []

    for stagger in stagger_values:
        forces = calculator.calculate_forces(
            stagger=float(stagger),
            separation=separation_m,
            velocity=speed_ms,
        )
        surges.append(forces["surge"])
        sways.append(forces["sway"])
        yaws.append(forces["yaw"])

    return ForceTimeHistory(
        time=times,
        surge=np.array(surges),
        sway=np.array(sways),
        yaw=np.array(yaws),
        stagger=stagger_values,
        separation=separation_m,
        speed=speed_ms,
    )


__all__ = [
    "ForceTimeHistory",
    "generate_time_history",
]
