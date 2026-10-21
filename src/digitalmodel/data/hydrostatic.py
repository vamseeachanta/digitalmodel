# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Persist synthetic constant-density gauge-pressure profiles in SI units.

Basis: OpenStax, University Physics Volume 1 (2016), section 14.1,
equation 14.9: p - p_surface = rho*g*h. No fluid properties are assumed.
"""

import csv
import math
from pathlib import Path
from typing import Iterable


def _nonnegative(value: float, name: str) -> float:
    """Require finite nonnegative values for the gauge-pressure domain."""
    try:
        number = float(value)
    except (TypeError, ValueError) as exc:
        raise ValueError(f"{name} must be finite and nonnegative") from exc
    if not math.isfinite(number) or number < 0:
        raise ValueError(f"{name} must be finite and nonnegative")
    return number


def write_hydrostatic_csv(
    path: Path,
    depths_m: Iterable[float],
    *,
    density_kg_m3: float,
    gravity_m_s2: float,
) -> None:
    """Write depth and gauge pressure; validate all inputs before opening a file.

    Depth increases downward from a zero-gauge-pressure surface. Density and
    gravitational acceleration are caller-supplied positive finite inputs.
    This model excludes compressibility and variation of density or gravity.
    """
    density = _nonnegative(density_kg_m3, "density_kg_m3")
    gravity = _nonnegative(gravity_m_s2, "gravity_m_s2")
    if density == 0 or gravity == 0:
        raise ValueError("density_kg_m3 and gravity_m_s2 must be positive")
    rows = []
    for value in depths_m:
        depth = _nonnegative(value, "depth_m")
        # OpenStax University Physics Vol. 1 (2016), §14.1, Eq. 14.9.
        pressure = _nonnegative(density * gravity * depth, "pressure_pa")
        rows.append({"depth_m": depth, "pressure_pa": pressure})
    with Path(path).open("w", newline="", encoding="utf-8") as stream:
        writer = csv.DictWriter(stream, fieldnames=["depth_m", "pressure_pa"])
        writer.writeheader()
        writer.writerows(rows)


def read_pressure_csv(path: Path) -> list[dict[str, float]]:
    """Retrieve stored values with explicit SI columns, without recomputation.

    This checks representation and the nonnegative gauge-pressure domain;
    it cannot establish a fluid's density, gravity or physical provenance.
    """
    with Path(path).open(newline="", encoding="utf-8") as stream:
        reader = csv.DictReader(stream)
        if reader.fieldnames != ["depth_m", "pressure_pa"]:
            raise ValueError("expected CSV columns depth_m,pressure_pa in SI units")
        rows = []
        for row in reader:
            if set(row) != {"depth_m", "pressure_pa"}:
                raise ValueError("pressure CSV row has extra columns")
            rows.append(
                {name: _nonnegative(value, name) for name, value in row.items()}
            )
        return rows
