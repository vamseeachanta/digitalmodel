# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Generate synthetic listing/deck pairs; no solver output is used.

Synthetic input definition: n positive frequencies omega_i=i/10 rad/s,
i=1..n; coefficient j at row i is i*j (added mass) or -i*j (damping),
j=1..12. These arbitrary test values are not engineering coefficients.
T=2*pi/omega: OpenStax, University Physics Volume 1 (2016), section 15.1.
HRTZ card layout: aqwa_backend.py, build_deck6,
explicit indexed cards in columns 7-10, 11-15, 16-20 and 21-30.
"""

import argparse
import math
from pathlib import Path

ADDED = "ADDED MASS-VARIATION WITH WAVE PERIOD/FREQUENCY"
DAMPING = "DAMPING-VARIATION WITH WAVE PERIOD/FREQUENCY"
RAO = "R.A.O.S-VARIATION WITH WAVE PERIOD/FREQUENCY"
END = "S T A B I L I T Y"


def listing(count=33, *, added_rows=None, damping_rows=None, gap=0,
            row_padding=0, terminate=True):
    """Return listing text and its independently declared explicit HRTZ deck.

    Count 33 reproduces issue #2107. Gap and padding are caller-controlled
    layout stress inputs, not bounds on any physical quantity.
    """
    deck = ["          FDR1"]
    for i in range(1, count + 1):
        deck.append(f"     1HRTZ{i:5d}{i:5d}{i / 10 / (2 * math.pi):10.7f}")
    deck.append("          END")
    out = []
    for marker, rows, sign in ((ADDED, added_rows, 1),
                                (DAMPING, damping_rows, -1)):
        out.extend([marker, " PERIOD FREQUENCY " + " VALUE" * 12])
        for i in range(1, (count if rows is None else rows) + 1):
            omega = i / 10
            values = " ".join(f"{sign * i * j:24.8E}" for j in range(1, 13))
            out.append(f" {2 * math.pi / omega:.8f} {omega:.3f} {values}")
            if i == 10:
                out.extend([" PAGE HEADER"] * gap)
    out.extend([RAO, " PERIOD FREQUENCY DIRECTION"])
    # Two arbitrary headings and twelve amplitude/phase fields exercise the
    # established six-DOF row grammar, with no response-accuracy claim.
    for heading in (0, 90):
        for i in range(1, count + 1):
            omega = i / 10
            out.append(f" {2 * math.pi / omega:.8f} {omega:.3f} {heading:.1f} "
                       + " ".join(["1.0", "0.0"] * 6) + " " * row_padding)
    if terminate:
        out.append(END)
    return "\n".join(out) + "\n", "\n".join(deck) + "\n"


def write_pair(directory, count=33, **kwargs):
    """Write generated text at a caller-supplied destination."""
    directory = Path(directory)
    directory.mkdir(parents=True, exist_ok=True)
    text, deck = listing(count, **kwargs)
    path = directory / "synthetic.LIS"
    path.write_text(text, encoding="ascii")
    path.with_suffix(".dat").write_text(deck, encoding="ascii")
    return path


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", type=Path)
    parser.add_argument("--count", type=int, default=33)
    args = parser.parse_args()
    write_pair(args.output, args.count)
