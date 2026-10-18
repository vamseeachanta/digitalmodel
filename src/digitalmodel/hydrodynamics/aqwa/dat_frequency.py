"""Deck 6 records: AQWA Reference Manual 2025 R1, sections 10.3 and 10.4."""
import math
import re

import numpy as np


def parse_frequency_records(lines):
    frequencies, directions = [], []
    for line in lines:
        stripped = line.strip()
        if not stripped or stripped.startswith("*") or stripped == "END":
            continue
        if re.fullmatch(r"FDR\d+", stripped):
            continue
        card = line[6:10].strip().upper()
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
    return frequencies, directions


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
