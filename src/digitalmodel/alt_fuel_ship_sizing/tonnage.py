# ABOUTME: Concept-stage gross/net tonnage per the International Convention on
# ABOUTME: Tonnage Measurement of Ships, 1969 (ITC 69), Annex I, Regs 3 and 4.
"""ITC 1969 gross and net tonnage (concept-design estimate).

International Convention on Tonnage Measurement of Ships, 1969 (ITC 69),
Annex I:

- **Regulation 3 (gross tonnage)**::

      GT = K1 * V,          K1 = 0.2 + 0.02 * log10(V)

  where ``V`` is the total volume of all enclosed spaces of the ship in
  cubic metres.

- **Regulation 4 (net tonnage)**::

      NT = K2 * Vc * (4d / 3D)^2  +  K3 * (N1 + N2 / 10)

  where ``Vc`` is the total volume of cargo spaces [m3],
  ``K2 = 0.2 + 0.02 * log10(Vc)``,
  ``K3 = 1.25 * (GT + 10000) / 10000``, ``d`` is the moulded draught
  amidships [m], ``D`` is the moulded depth amidships [m], ``N1`` is the
  number of passengers in cabins with not more than 8 berths and ``N2``
  the number of other passengers, subject to the Regulation 4 provisos:

  - the factor ``(4d/3D)^2`` shall not be taken as greater than unity;
  - the term ``K2 * Vc * (4d/3D)^2`` shall not be taken as less than
    ``0.25 GT``;
  - when ``N1 + N2 < 13``, ``N1`` and ``N2`` shall be taken as zero;
  - ``NT`` shall not be taken as less than ``0.30 GT``.

At concept stage ``V`` and ``Vc`` are estimates from the arrangement; this
is a screening estimate, not an admeasurement.
"""

from __future__ import annotations

import math
from dataclasses import dataclass


@dataclass(frozen=True)
class TonnageResult:
    gross_tonnage: float
    net_tonnage: float
    k1: float
    k2: float
    k3: float
    draught_depth_factor: float
    cargo_term: float
    passenger_term: float
    nt_floor_applied: bool
    cargo_term_floor_applied: bool


def gross_tonnage(total_enclosed_volume_m3: float) -> float:
    """ITC 69 Annex I Reg 3: ``GT = (0.2 + 0.02 log10 V) * V``."""
    if total_enclosed_volume_m3 <= 0.0:
        raise ValueError("total_enclosed_volume_m3 must be positive")
    k1 = 0.2 + 0.02 * math.log10(total_enclosed_volume_m3)
    return k1 * total_enclosed_volume_m3


def tonnage(
    total_enclosed_volume_m3: float,
    cargo_volume_m3: float,
    moulded_draught_m: float,
    moulded_depth_m: float,
    passengers_in_cabins_up_to_8_berths: int = 0,
    passengers_other: int = 0,
) -> TonnageResult:
    """ITC 69 Annex I Regs 3 and 4 gross/net tonnage with all provisos."""
    if cargo_volume_m3 <= 0.0:
        raise ValueError("cargo_volume_m3 must be positive")
    if cargo_volume_m3 > total_enclosed_volume_m3:
        raise ValueError("cargo_volume_m3 cannot exceed total_enclosed_volume_m3")
    if moulded_draught_m <= 0.0 or moulded_depth_m <= 0.0:
        raise ValueError("moulded draught and depth must be positive")
    if passengers_in_cabins_up_to_8_berths < 0 or passengers_other < 0:
        raise ValueError("passenger counts must be non-negative")

    v = total_enclosed_volume_m3
    k1 = 0.2 + 0.02 * math.log10(v)
    gt = k1 * v

    k2 = 0.2 + 0.02 * math.log10(cargo_volume_m3)
    k3 = 1.25 * (gt + 10000.0) / 10000.0

    factor = (4.0 * moulded_draught_m / (3.0 * moulded_depth_m)) ** 2
    factor = min(factor, 1.0)  # Reg 4: shall not be taken as greater than unity

    cargo_term = k2 * cargo_volume_m3 * factor
    cargo_floor = 0.25 * gt
    cargo_term_floor_applied = cargo_term < cargo_floor
    cargo_term = max(cargo_term, cargo_floor)

    n1 = passengers_in_cabins_up_to_8_berths
    n2 = passengers_other
    if n1 + n2 < 13:  # Reg 4: fewer than 13 passengers -> both taken as zero
        n1 = n2 = 0
    passenger_term = k3 * (n1 + n2 / 10.0)

    nt = cargo_term + passenger_term
    nt_floor = 0.30 * gt
    nt_floor_applied = nt < nt_floor
    nt = max(nt, nt_floor)

    return TonnageResult(
        gross_tonnage=gt,
        net_tonnage=nt,
        k1=k1,
        k2=k2,
        k3=k3,
        draught_depth_factor=factor,
        cargo_term=cargo_term,
        passenger_term=passenger_term,
        nt_floor_applied=nt_floor_applied,
        cargo_term_floor_applied=cargo_term_floor_applied,
    )
