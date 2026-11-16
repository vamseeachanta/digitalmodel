# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Sum independent scalar well profiles over a finite field-time horizon.

Time and start days are days, rates are bbl/day, volumes are bbl and nominal
exponential decline is in 1/day. Elapsed well time is field time minus start
day. No facility cap, interference, reserve constraint or economic cutoff is
imposed; cumulative production is not an EUR or qualified reserves estimate.
Schedules are consumed once and validated in full, including inactive wells.
Finite negative field times and empty schedules return zero after validation.
"""

import math
from collections.abc import Callable, Iterable
from dataclasses import dataclass
from numbers import Real

from .production_profile import cumulative_production, rate_at_time


@dataclass(frozen=True)
class WellSchedule:
    """Immutable start day and caller-supplied scalar profile parameters.

    Durations/start are days, plateau rate is bbl/day, decline is 1/day.
    All values must be finite nonnegative real numbers, excluding booleans.
    Validation occurs when a field total is requested, including future wells.
    """

    start_day: float
    ramp_duration_days: float
    plateau_duration_days: float
    plateau_rate_bbl_per_day: float
    decline_rate_per_day: float


def _parameters(well: WellSchedule) -> dict[str, float]:
    return dict(
        ramp_duration_days=well.ramp_duration_days,
        plateau_duration_days=well.plateau_duration_days,
        plateau_rate_bbl_per_day=well.plateau_rate_bbl_per_day,
        decline_rate_per_day=well.decline_rate_per_day,
    )


def _validate_coordinate(name: str, value: float) -> None:
    try:
        valid = (
            isinstance(value, Real)
            and not isinstance(value, bool)
            and math.isfinite(value)
            and (name == "time_days" or value >= 0.0)
        )
    except OverflowError:
        valid = False
    if not valid:
        raise ValueError(
            f"{name} must be a finite real number; start_day must be nonnegative"
        )


def _field_total(
    time_days: float,
    wells: Iterable[WellSchedule],
    scalar: Callable[..., float],
) -> float:
    _validate_coordinate("time_days", time_days)
    scheduled = tuple((well.start_day, _parameters(well)) for well in wells)
    for start_day, parameters in scheduled:
        _validate_coordinate("start_day", start_day)
        # The scalar API validates every parameter before its negative-time zero.
        rate_at_time(-1.0, **parameters)
    if time_days < 0.0:
        return 0.0
    return math.fsum(
        scalar(time_days - start_day, **parameters)
        for start_day, parameters in scheduled
    )


def field_rate_at_time(time_days: float, wells: Iterable[WellSchedule]) -> float:
    """Return summed instantaneous rates in bbl/day at finite field time.

    Wells contribute zero before startup. At startup, a zero ramp jumps to
    peak; otherwise rate is zero. Ramp and plateau endpoints retain the peak.
    Zero plateau starts decline at ramp end; zero decline holds the peak.

    Raises:
        ValueError: Time is nonfinite, non-real or boolean, or any well has an
            invalid start day or scalar profile, even when it is inactive.
    """
    return _field_total(time_days, wells, rate_at_time)


def field_cumulative_production(
    time_days: float, wells: Iterable[WellSchedule]
) -> float:
    """Return summed finite-horizon production through field time, in bbl.

    Each well integrates from its own startup; its volume at startup is zero.
    Partial ramps, plateaus and exponential tails use the analytic scalar API.
    This volume has no reserves qualification or infinite-tail EUR meaning.

    Raises:
        ValueError: Time is nonfinite, non-real or boolean, or any well has an
            invalid start day or scalar profile, even when it is inactive.
    """
    return _field_total(time_days, wells, cumulative_production)
