"""Deck 6 records: AQWA Reference Manual 2025 R1, sections 10.3 and 10.4."""
import math
import re
from typing import Iterable

import numpy as np


def parse_frequency_records(
    lines: Iterable[str], structure: int = 1,
) -> tuple[list[float], list[float]]:
    """Select one FDR grid; FILE/CSTR/CPDB reference external database data."""
    grids = parse_frequency_grids(lines)
    if structure not in grids:
        raise ValueError("Requested deck-6 structure is absent")
    return grids[structure]


def parse_frequency_grids(lines: Iterable[str]) -> dict[int, tuple[list[float], list[float]]]:
    """Read independent frequency/direction indices for each FDRn block."""
    grids: dict[int, tuple[list[float], list[float]]] = {}
    structure = 1
    frequencies: list[float] = []
    directions: list[float] = []
    for line in lines:
        stripped = line.strip()
        if not stripped or stripped.startswith("*") or stripped == "END":
            continue
        marker = re.fullmatch(r"FDR(\d+)", stripped, re.I)
        if marker:
            structure = int(marker[1])
            if structure in grids:
                raise ValueError("Duplicate deck-6 structure")
            frequencies, directions = [], []
            grids[structure] = (frequencies, directions)
            continue
        grids.setdefault(structure, (frequencies, directions))
        card = line[6:10].strip().upper()
        if card in {"FILE", "CSTR", "CPDB"}:
            continue
        if card not in {"HRTZ", "FREQ", "PERD", "DIRN"}:
            raise ValueError(f"Unknown deck-6 card: {stripped}")
        target = directions if card == "DIRN" else frequencies
        values = record_values(line, len(target))
        if card == "DIRN":
            if any(abs(v) > 180 for v in values):
                raise ValueError("DIRN directions must be in [-180, 180] degrees")
            directions.extend(values)
            continue
        for value in values:
            if value <= 0:
                raise ValueError(f"{card} value must be positive")
            omega = value * 2*math.pi if card == "HRTZ" else value
            if card == "PERD":
                omega = 2*math.pi / value
            if frequencies and omega <= frequencies[-1]:
                raise ValueError("Frequency cards must be strictly increasing in rad/s")
            frequencies.append(omega)
    return grids or {1: ([], [])}


def record_values(line, previous_count):
    """Read I5/I5 indices and up to six F10 values; support generated ranges."""
    try:
        first, last = int(line[10:15].strip() or "0"), int(line[15:20])
        values = [float(line[i:i+10].replace("D", "E"))
                  for i in range(20, len(line), 10) if line[i:i+10].strip()]
    except ValueError as exc:
        raise ValueError(f"Malformed deck-6 card: {line.strip()}") from exc
    if not values or not all(math.isfinite(v) for v in values):
        raise ValueError("Deck-6 card requires finite values")
    if first == 0:
        count = last - previous_count
        if count <= 0 or len(values) != 2:
            raise ValueError("Generated deck-6 card requires two endpoints and increasing index")
        return np.linspace(values[0], values[1], count).tolist()
    if first != previous_count + 1 or last < first or len(values) != last-first+1:
        raise ValueError("Deck-6 card indices must be contiguous and match value count")
    return values
