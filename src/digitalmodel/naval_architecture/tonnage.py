# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Validated concept-screening facade for ITC 1969 gross and net tonnage.

Source: International Convention on Tonnage Measurement of Ships, 1969,
Annex I, Regulations 3 and 4, UN Treaty Series volume 1291, pages 15-16:
https://treaties.un.org/doc/Publication/UNTS/Volume%201291/volume-1291-I-21264-English.pdf

Volumes are in cubic metres; moulded draught and depth amidships are in
metres. GT and NT are dimensionless indices, not masses or displacements.
The existing alternative-fuel sizing implementation owns all equations,
coefficients and provisos; its immutable result type is reused unchanged.

This facade accepts positive enclosed and cargo volumes, cargo volume no
larger than enclosed volume, and positive draught no greater than depth.
Passenger counts must be nonnegative integers; booleans are rejected for
every input. Zero cargo volume is unsupported by the logarithmic equation.
Numeric extremes that cannot produce finite positive coefficients and
tonnages are rejected rather than returned as screening results.
Overflowing draught/depth intermediates are rejected before delegation.

The caller supplies estimated volumes, the applicable Regulation 4(2)
draught and permitted passenger counts. No enclosed-space geometry,
exclusions, load-line selection, regulatory applicability or certified
admeasurement is established here. Draught above moulded depth is outside
this concept-screening interface. Freeboard tables and MARPOL are excluded.
"""

from __future__ import annotations

import math
from importlib import import_module
from numbers import Integral, Real

from digitalmodel.alt_fuel_ship_sizing.tonnage import TonnageResult

_equations = import_module("digitalmodel.alt_fuel_ship_sizing.tonnage")

__all__ = ["TonnageResult", "gross_tonnage", "tonnage"]


def _positive_finite(name: str, value: float) -> None:
    try:
        valid = (
            isinstance(value, Real)
            and not isinstance(value, bool)
            and math.isfinite(value)
            and value > 0
        )
    except OverflowError:
        valid = False
    if not valid:
        raise ValueError(f"{name} must be a finite positive real number")


def gross_tonnage(total_enclosed_volume_m3: float) -> float:
    """Return dimensionless GT from enclosed volume in cubic metres (Reg 3)."""
    _positive_finite("total_enclosed_volume_m3", total_enclosed_volume_m3)
    result: float = _equations.gross_tonnage(total_enclosed_volume_m3)
    _positive_finite("gross_tonnage result", result)
    return result


def tonnage(
    total_enclosed_volume_m3: float,
    cargo_volume_m3: float,
    moulded_draught_m: float,
    moulded_depth_m: float,
    passengers_in_cabins_up_to_8_berths: int = 0,
    passengers_other: int = 0,
) -> TonnageResult:
    """Return dimensionless GT/NT and diagnostics using Regs 3 and 4.

    Volumes use cubic metres and dimensions use metres. Passenger arguments
    count permitted passengers in cabins with at most eight berths and all
    other permitted passengers, respectively. Invalid or unsupported inputs
    raise ValueError. All calculations delegate to the shared equations.
    """
    for name, value in (
        ("total_enclosed_volume_m3", total_enclosed_volume_m3),
        ("cargo_volume_m3", cargo_volume_m3),
        ("moulded_draught_m", moulded_draught_m),
        ("moulded_depth_m", moulded_depth_m),
    ):
        _positive_finite(name, value)
    if cargo_volume_m3 > total_enclosed_volume_m3:
        raise ValueError("cargo_volume_m3 cannot exceed total_enclosed_volume_m3")
    if moulded_draught_m > moulded_depth_m:
        raise ValueError("moulded_draught_m cannot exceed moulded_depth_m")
    for name, count in (
        ("passengers_in_cabins_up_to_8_berths", passengers_in_cabins_up_to_8_berths),
        ("passengers_other", passengers_other),
    ):
        if isinstance(count, bool) or not isinstance(count, Integral) or count < 0:
            raise ValueError(f"{name} must be a nonnegative integer")
    try:
        # Reg 4: both products in 4d/(3D) must remain representable.
        if not (
            math.isfinite(4.0 * moulded_draught_m)
            and math.isfinite(3.0 * moulded_depth_m)
        ):
            raise ValueError(
                "draught/depth intermediates exceed the supported numeric range"
            )
        result: TonnageResult = _equations.tonnage(
            total_enclosed_volume_m3,
            cargo_volume_m3,
            moulded_draught_m,
            moulded_depth_m,
            passengers_in_cabins_up_to_8_berths,
            passengers_other,
        )
    except (OverflowError, ZeroDivisionError) as error:
        raise ValueError("tonnage inputs exceed the supported numeric range") from error
    for name in ("gross_tonnage", "net_tonnage", "k1", "k2", "k3"):
        _positive_finite(name + " result", getattr(result, name))
    if not math.isfinite(result.draught_depth_factor):
        raise ValueError("draught/depth factor exceeds the supported numeric range")
    return result
