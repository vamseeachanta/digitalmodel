# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Recover global mid-surface centroid stresses from planar MYSTRAN QUAD4 F06.

Only the supplied basic-coordinate, positive-normal planar decks are supported.
MYSTRAN's default QUADAXIS=SPLITD uses the bisector of the two diagonals.
See Source/EMG/EMG1/ELMGM2.f90, lines 270-299, revision
3a5913a904ee1adbda9fbcaf9c0a6af8e10f0f9f. Native ERR records confirm SPLITD.
No deck or comparison criterion is modified by this post-processing.
"""

import argparse
import csv
import math
import re
from pathlib import Path


def mid_surface_stresses(text: str) -> dict[int, tuple[float, ...]]:
    """Parse exactly one pair of opposite fibre stresses per centroid."""
    lines = text.splitlines()
    stresses = {}
    for index, line in enumerate(lines):
        match = re.match(r"\s*(\d+)\s+CENTER\s+(.+)$", line)
        if not match:
            continue
        eid = int(match[1])
        if eid in stresses:
            raise ValueError(f"duplicate centroid {eid}")
        first = match[2].split()
        following = next((item.split() for item in lines[index + 1:] if item.strip()), [])
        try:
            a = [float(value.replace("D", "E")) for value in first[:4]]
            b = [float(value.replace("D", "E")) for value in following[:4]]
        except ValueError as error:
            raise ValueError(f"invalid fibre pair {eid}") from error
        if (len(a) != 4 or len(b) != 4 or not all(map(math.isfinite, a + b))
                or not a[0] < 0 < b[0] or not math.isclose(a[0], -b[0])):
            raise ValueError(f"incomplete or invalid fibre pair {eid}")
        stresses[eid] = tuple((a[i] + b[i]) / 2 for i in range(1, 4))
    if not stresses:
        raise ValueError("no centroid stress pairs")
    return stresses


def global_stress(stress: tuple[float, ...], points: list[tuple[float, ...]]) -> tuple[float, float, float]:
    """Rotate a planar symmetric tensor from SPLITD to global XY axes."""
    if any(point[2] != 0 for point in points):
        raise ValueError("only XY-plane elements are supported")
    d13 = [points[2][i] - points[0][i] for i in range(2)]
    d24 = [points[3][i] - points[1][i] for i in range(2)]
    n13, n24 = math.hypot(*d13), math.hypot(*d24)
    if n13 == 0 or n24 == 0 or d13[0] * d24[1] - d13[1] * d24[0] <= 0:
        raise ValueError("degenerate or reversed element")
    direction = [d13[i] / n13 - d24[i] / n24 for i in range(2)]
    length = math.hypot(*direction)
    c, s = (value / length for value in direction)
    xx, yy, xy = stress
    return (c * c * xx + s * s * yy - 2 * c * s * xy,
            s * s * xx + c * c * yy + 2 * c * s * xy,
            c * s * (xx - yy) + (c * c - s * s) * xy)


def geometry(deck: Path) -> dict[int, list[tuple[float, ...]]]:
    """Read the supplied free-field basic-coordinate GRID and CQUAD4 cards."""
    grids, elements = {}, {}
    for line in deck.read_text().splitlines():
        fields = line.split(",")
        if fields[0] == "GRID":
            if fields[2] not in {"", "0"}:
                raise ValueError("only basic grid coordinates are supported")
            grids[int(fields[1])] = tuple(map(float, fields[3:6]))
        elif fields[0] == "CQUAD4":
            elements[int(fields[1])] = list(map(int, fields[3:7]))
    return {eid: [grids[node] for node in nodes] for eid, nodes in elements.items()}


def extract(deck: Path, native: Path, output: Path) -> None:
    """Validate the native frame and write one global centroid row per element."""
    text = native.read_text()
    if "S T R E S S E S   I N   L O C A L" not in text:
        raise ValueError("local element stress header is required")
    diagnostics = native.with_suffix(".ERR").read_text()
    if "PARAM QUADAXIS = SPLITD" not in diagnostics:
        raise ValueError("native SPLITD coordinate-basis evidence is required")
    banner = re.search(r"MYSTRAN Version\s+([\d.]+)", text)
    if banner is None:
        raise ValueError("native program version is required")
    version = banner[1]
    local, elements = mid_surface_stresses(text), geometry(deck)
    if set(local) != set(elements):
        raise ValueError("centroid element IDs differ from the deck")
    patch = deck.stem == "patch"
    with output.open("w", newline="") as stream:
        stream.write(f"# program: MYSTRAN {version}\n# mesh: {deck.stem}\n")
        stream.write("# recovery: centroid; arithmetic mean of two fibre stresses; global axes\n")
        writer = csv.writer(stream, lineterminator="\n")
        writer.writerow(["eid", "sigma_xx_MPa", "sigma_yy_MPa", "tau_xy_MPa"]
                        if patch else ["eid", "sigma_yy_MPa"])
        for eid, points in sorted(elements.items()):
            global_values = global_stress(local[eid], points)
            values = global_values if patch else [global_values[1]]
            writer.writerow([eid] + [format(value, ".9g") for value in values])
    print(f"{output.name}: {len(elements)} global mid-surface centroid rows")


def main() -> None:
    """Extract the supplied deck's native F06 result."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--deck", type=Path, required=True)
    parser.add_argument("--f06", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    extract(args.deck, args.f06, args.output)


if __name__ == "__main__":
    main()
