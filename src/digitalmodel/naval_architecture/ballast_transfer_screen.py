# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Screen two-tank capacity for a small, constant-MCT even-keel correction.

Positions are metres, positive forward; trim is forward minus aft draft in
metres; masses and capacities are tonnes; supplied MCT is tonne-metres/cm.
Only internal transfer is considered, at unchanged displacement. The existing
midship-flotation approximation is retained. Capacity feasibility establishes
neither stability, navigability nor operational approval. Free surface,
strength, pumping, addition/discharge and updated hydrostatics are excluded.
"""

from __future__ import annotations

from dataclasses import dataclass
import math
from numbers import Real
from pathlib import Path

from digitalmodel.naval_architecture.trim_ballast import (
    even_keel_transfer_mass,
    even_keel_transfer_mass_cited,
)


@dataclass(frozen=True)
class TankState:
    """Explicit tank position, present mass and maximum mass; no defaults."""

    identifier: str
    position_m: float
    mass_t: float
    capacity_t: float

    def __post_init__(self) -> None:
        """Reject invalid identifiers, nonfinite quantities and mass bounds."""
        if not isinstance(self.identifier, str) or not self.identifier.strip():
            raise ValueError("identifier must be a nonempty string")
        for name in ("position_m", "mass_t", "capacity_t"):
            _finite_number(name, getattr(self, name))
        if self.capacity_t < 0.0:
            raise ValueError("capacity_t must be nonnegative")
        if not 0.0 <= self.mass_t <= self.capacity_t:
            raise ValueError("mass_t must be between zero and capacity_t")


def _finite_number(name: str, value: float) -> None:
    if isinstance(value, bool) or not isinstance(value, Real) or not math.isfinite(value):
        raise ValueError(f"{name} must be a finite real number, excluding booleans")


def _inputs(a: TankState, b: TankState, trim: float, mct: float) -> float:
    if a.identifier == b.identifier:
        raise ValueError("tank identifiers must be distinct")
    if a.position_m == b.position_m:
        raise ValueError("tank position_m values must be distinct")
    _finite_number("initial_trim_m", trim)
    _finite_number("mct_t_m_per_cm", mct)
    return b.position_m - a.position_m


@dataclass(frozen=True)
class TransferScreen:
    """Signed tonnes relative to A to B, actual direction and capacity only.

    Zero required transfer has no source or destination and zero available
    transfer tonnes. No tank state is changed, including infeasible cases.
    """

    required_signed_t: float
    source_id: str | None
    destination_id: str | None
    available_transfer_t: float
    capacity_feasible: bool


def _capacity_result(a: TankState, b: TankState, required: float) -> TransferScreen:
    if required == 0.0:
        return TransferScreen(required, None, None, 0.0, True)
    source, destination = (a, b) if required > 0.0 else (b, a)
    available = min(source.mass_t, destination.capacity_t - destination.mass_t)
    return TransferScreen(
        required, source.identifier, destination.identifier,
        available, abs(required) <= available,
    )


def screen_even_keel_transfer(
    *, tank_a: TankState, tank_b: TankState,
    initial_trim_m: float, mct_t_m_per_cm: float,
) -> TransferScreen:
    """Return the required signed mass and a two-tank capacity comparison.

    Positive required tonnes mean A to B; negative tonnes mean B to A.
    Availability is the lesser of actual source mass and destination spare
    capacity. Equality is feasible. All inputs are required, including MCT;
    zero trim still requires valid tanks, distinct positions and positive MCT.
    """
    lever = _inputs(tank_a, tank_b, initial_trim_m, mct_t_m_per_cm)
    required = even_keel_transfer_mass(
        initial_trim_m=initial_trim_m,
        lever_arm_m=lever,
        mct_t_m_per_cm=mct_t_m_per_cm,
    )
    return _capacity_result(tank_a, tank_b, required)


def screen_even_keel_transfer_cited(
    *, tank_a: TankState, tank_b: TankState,
    initial_trim_m: float, mct_t_m_per_cm: float,
    repo_root: Path | None = None,
) -> dict:
    """Return the screen as ``value``, tonne units and required sidecars.

    Delegate to the existing cited moment-balance helper. Missing or
    mismatched EN400 or NIST references propagate CitationResolutionError,
    including for zero trim. Synthetic metadata does not qualify live data.
    """
    lever = _inputs(tank_a, tank_b, initial_trim_m, mct_t_m_per_cm)
    cited = even_keel_transfer_mass_cited(
        initial_trim_m=initial_trim_m,
        lever_arm_m=lever,
        mct_t_m_per_cm=mct_t_m_per_cm, repo_root=repo_root,
    )
    return {**cited, "value": _capacity_result(tank_a, tank_b, cited["value"])}
