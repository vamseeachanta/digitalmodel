# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Reduce a single benchmark run without executing OpenFOAM.

Timing/window policy: scripts/cfd/bench_run.sh, benchmark metrics section,
repository revision preceding this helper (2026-10-19). The window length and
tail fraction are explicit inputs; neither is an engineering acceptance limit.
"""

from __future__ import annotations

import argparse
import math
import re
import sys
from pathlib import Path


# NIST, SI prefixes, micro = 10^-6 (accessed 2026-10-19):
# https://physics.nist.gov/cuu/Units/prefixes.html
MICROSECONDS_PER_SECOND = 1_000_000


def _finite(text: str) -> float:
    value = float(text)
    if not math.isfinite(value):
        raise ValueError("non-finite measurement")
    return value


def timings(log: Path) -> tuple[list[float], float]:
    """Return complete cumulative CPU measurements and the final residual.

    Each Time record must have exactly one ExecutionTime record. This prevents
    a partial last step, or concatenated/restarted output, from looking complete.
    CPU time includes initialization before the first step, as in bench_run.sh.
    """
    elapsed = []
    previous_time = -math.inf
    pending = False
    residual = math.nan
    with log.open() as stream:
        for line in stream:
            if line.startswith("Time = "):
                current = _finite(line.split("=", 1)[1])
                if pending or current <= previous_time:
                    raise ValueError("incomplete or non-increasing time records")
                previous_time = current
                pending = True
            elif line.startswith("ExecutionTime = "):
                cpu = _finite(line.split()[2])
                if not pending or cpu < (elapsed[-1] if elapsed else 0):
                    raise ValueError("invalid cumulative CPU records")
                elapsed.append(cpu)
                pending = False
            match = re.search(r"Solving for p_rgh, Initial residual = ([^,]+)", line)
            if match:
                try:
                    residual = _finite(match.group(1))
                except ValueError:
                    residual = math.nan
    if pending or not elapsed:
        raise ValueError("no complete timing series")
    return elapsed, residual


def _owner_cells(owner: Path) -> int | None:
    try:
        # Source: mesh_store.py::mesh_cells, repository revision 6476218e.
        with owner.open("rb") as stream:
            match = re.search(rb"nCells:(\d+)", stream.read(4096))
    except OSError:
        return None
    if match and (count := int(match.group(1))) > 0:
        return count
    return None


def cell_count(case: Path, ranks: int) -> int | None:
    """Prefer the serial mesh; otherwise require every requested rank."""
    serial = case / "constant/polyMesh/owner"
    if serial.exists():
        return _owner_cells(serial)
    counts = [
        _owner_cells(case / f"processor{rank}/constant/polyMesh/owner")
        for rank in range(ranks)
    ]
    return sum(counts) if counts and all(c is not None for c in counts) else None


def _tail_columns(
    case: Path, filename: str, columns: tuple[str, ...], fraction: float
) -> list[float] | None:
    """Read a unique output with named columns; refuse ambiguous formats.

    Source: OpenFOAM v2312 (2023), forces.C::writeIntegratedDataFileHeader and
    writeIntegratedDataFile (total, pressure, viscous, optional porous):
    https://api.openfoam.com/2312/forces_8C_source.html
    forceCoeffs.C writes only active coefficients in sorted order:
    https://api.openfoam.com/2312/forceCoeffs_8C_source.html
    """
    paths = list((case / "postProcessing").rglob(filename))
    if len(paths) != 1:
        return None
    header = None
    rows = []
    previous_time = -math.inf
    try:
        with paths[0].open() as stream:
            for line in stream:
                if not line.strip():
                    continue
                if line.lstrip().startswith("#"):
                    tokens = line.lstrip()[1:].split()
                    if tokens and tokens[0] == "Time":
                        if rows or len(set(tokens)) != len(tokens):
                            return None
                        header = tokens
                    continue
                if header is None or any(c not in header for c in columns):
                    return None
                tokens = line.translate(str.maketrans({"(": " ", ")": " "})).split()
                if len(tokens) != len(header):
                    return None
                values = [_finite(token) for token in tokens]
                if values[0] <= previous_time:
                    return None
                previous_time = values[0]
                rows.append([values[header.index(c)] for c in columns])
    except (OSError, ValueError):
        return None
    if not rows:
        return None
    # Preserve bench_run.sh's sample-count tail rule: floor(n*(1-fraction)).
    tail = rows[math.floor(len(rows) * (1 - fraction)) :]
    return [math.fsum(row[i] for row in tail) / len(tail) for i in range(len(columns))]


def reduce_metrics(
    case: Path, log: Path, ranks: int, window: int, tail_fraction: float
) -> tuple[list[float], list[str]]:
    """Return the existing nine metric columns plus unavailable-data reasons."""
    if ranks <= 0 or window <= 0 or not 0 < tail_fraction <= 1:
        raise ValueError(
            "positive ranks/window and a tail fraction in (0, 1] are required"
        )
    elapsed, residual = timings(log)
    count = len(elapsed)
    per_step = elapsed[-1] / count
    size = min(window, count)
    # Independent arithmetic: a cumulative difference divided by its step count.
    first = elapsed[size - 1] / size
    last = (elapsed[-1] - (elapsed[-size - 1] if size < count else 0)) / size
    cells = cell_count(case, ranks)
    per_cell = per_step * MICROSECONDS_PER_SECOND / cells if cells else math.nan
    force = _tail_columns(
        case, "force.dat", ("total_x", "pressure_x", "viscous_x"), tail_fraction
    )
    coeff = _tail_columns(case, "coefficient.dat", ("Cd",), tail_fraction)
    notes = []
    pressure = viscous = math.nan
    if force and force[0] != 0:
        # Percent is the dimensionless component/total ratio multiplied by 100.
        pressure, viscous = (100 * component / force[0] for component in force[1:])
    else:
        notes.append("force shares: missing, ambiguous, invalid, or zero-total data")
    if coeff is None:
        notes.append("drag coefficient: missing, ambiguous, or invalid data")
    if cells is None:
        notes.append("cell count: no complete mesh header representation")
    if math.isnan(residual):
        notes.append("pressure residual: absent or invalid in log")
    return [
        count,
        per_step,
        per_cell,
        first,
        last,
        coeff[0] if coeff else math.nan,
        pressure,
        viscous,
        residual,
    ], notes


def main() -> int:
    """Print TSV metrics; keep diagnostics separate from machine-readable data."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--case", type=Path, required=True)
    parser.add_argument("--log", type=Path, required=True)
    parser.add_argument("--ranks", type=int, required=True)
    parser.add_argument("--window", type=int, required=True)
    parser.add_argument("--tail-fraction", type=float, required=True)
    args = parser.parse_args()
    try:
        values, notes = reduce_metrics(
            args.case, args.log, args.ranks, args.window, args.tail_fraction
        )
    except (OSError, ValueError, IndexError):
        print(
            "bench_metrics: invalid or incomplete benchmark timing data",
            file=sys.stderr,
        )
        return 2
    for note in notes:
        print(f"bench_metrics: NOT_EVALUATED: {note}", file=sys.stderr)
    print(
        str(values[0]) + "\t" + "\t".join(format(value, ".6g") for value in values[1:])
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
