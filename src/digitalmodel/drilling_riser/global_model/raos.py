"""Reader for direction-grouped vessel RAO text tables (amplitude and phase components).

The layout is a plain-text export with one block per wave direction::

    RAO for direction <d> deg
    <header rows>
    <direction> <frequency> <surge amp> <surge phase> <sway amp> ... <yaw phase>

Rows are whitespace separated; header and separator rows are skipped. Only the numbers are
read here: units and sign/phase/direction conventions are not stated reliably in such exports
and must be established and recorded by the caller (see ``spec.DisplacementRAOs``).
"""

from __future__ import annotations

import re
from typing import Any

_DIR = re.compile(r"RAO for direction\s+([-+\d.eE]+)\s*deg", re.I)


def _floats(tokens: list[str]) -> list[float] | None:
    try:
        return [float(t) for t in tokens]
    except ValueError:
        return None


def parse_direction_grouped_rao_text(text: str) -> dict[str, Any]:
    """Directions, frequencies and [direction][frequency][6] amplitude and phase arrays."""
    blocks: dict[float, list[list[float]]] = {}
    current: float | None = None
    for line in text.splitlines():
        m = _DIR.search(line)
        if m:
            current = float(m.group(1))
            if current in blocks:
                raise ValueError(f"direction {current} deg appears twice")
            blocks[current] = []
            continue
        vals = _floats(line.split())
        if current is None or vals is None or len(vals) != 14:
            continue
        if abs(vals[0] - current) > 1e-9:
            raise ValueError(f"row heading {vals[0]} inside the block for direction {current}")
        blocks[current].append(vals[1:])
    if not blocks:
        raise ValueError("no RAO direction blocks found")
    dirs = sorted(blocks)
    freqs = [r[0] for r in blocks[dirs[0]]]
    for d in dirs:
        if [r[0] for r in blocks[d]] != freqs:
            raise ValueError(f"direction {d} deg: frequency grid differs from direction {dirs[0]} deg")
    amp = [[[r[1 + 2 * k] for k in range(6)] for r in blocks[d]] for d in dirs]
    ph = [[[r[2 + 2 * k] for k in range(6)] for r in blocks[d]] for d in dirs]
    return {"directions_deg": dirs, "frequencies_rad_s": freqs, "amplitude": amp, "phase_deg": ph}
