"""Offline low-order GDF checks; no solver import or licence is required.

Hydrostatic references: NREL/TP-5000-60601 (2014), Table 4-6, p.25.
Platform CM: Table 3-3, p.9. https://www.nlr.gov/docs/fy14osti/60601.pdf
Waterplane ~380 m2 and numerical acceptance limits: maintainer mesh criteria,
documented in README.md. These criteria are not publication-derived constants.
"""

import argparse
from collections import Counter
from itertools import combinations
from pathlib import Path

import numpy as np


def read_gdf(path):
    """Read full-body, metre-based low-order GDF; reject partial/invalid files."""
    lines = Path(path).read_text(encoding="ascii").splitlines()
    try:
        length, gravity = map(float, lines[1].split())
        symmetry = tuple(map(int, lines[2].split()))
        count = int(lines[3])
        points = np.array([float(x) for line in lines[4:] for x in line.split()])
        if (
            length != 1
            or not np.isfinite(gravity)
            or gravity <= 0
            or symmetry != (0, 0)
            or count <= 0
            or points.size != count * 12
            or not np.all(np.isfinite(points))
        ):
            raise ValueError("Invalid full-body metre-based GDF")
        return points.reshape(count, 4, 3)
    except (IndexError, ValueError) as error:
        raise ValueError("Invalid full-body metre-based GDF") from error


def measure(panels):
    """Integrate signed tetrahedra; the implicit SWL lid has zero contribution."""
    directed = Counter()
    triangles = []
    max_aspect = max_span = 0.0
    crossings = degenerate = 0
    for panel in panels:
        vertices = [tuple(p) for p in np.round(panel, 8)]
        # Low-order GDF triangles repeat their final vertex.
        if vertices[2] == vertices[3]:
            vertices.pop()
        unique = len(set(vertices)) == len(vertices)
        lengths = [
            np.linalg.norm(np.subtract(b, a))
            for a, b in zip(vertices, vertices[1:] + vertices[:1])
        ]
        span = max(
            np.linalg.norm(np.subtract(a, b)) for a, b in combinations(vertices, 2)
        )
        max_span = max(max_span, span)
        max_aspect = max(
            max_aspect, max(lengths) / min(lengths) if min(lengths) else np.inf
        )
        crossings += int(min(panel[:, 2]) < -1e-8 and max(panel[:, 2]) > 1e-8)
        for a, b in zip(vertices, vertices[1:] + vertices[:1]):
            directed[(a, b)] += 1
        a, b, c = panel[:3]
        area = np.linalg.norm(np.cross(b - a, c - a)) / 2
        triangles.append([a, b, c])
        if len(vertices) == 4:
            a, b, c = panel[[0, 2, 3]]
            area += np.linalg.norm(np.cross(b - a, c - a)) / 2
            triangles.append([a, b, c])
        degenerate += int(not unique or area <= 1e-12)
    bad_edges = 0
    waterplane = 0.0
    for a, b in {tuple(sorted(edge)) for edge in directed}:
        forward, backward = directed[(a, b)], directed[(b, a)]
        if forward + backward == 1 and abs(a[2]) < 1e-8 and abs(b[2]) < 1e-8:
            # Reverse the wetted boundary to obtain the upward SWL cap.
            sign = 1 if backward else -1
            waterplane += sign * (a[0] * b[1] - a[1] * b[0]) / 2
        elif forward != 1 or backward != 1:
            bad_edges += 1
    tri = np.array(triangles)
    volumes = np.einsum("ij,ij->i", tri[:, 0], np.cross(tri[:, 1], tri[:, 2])) / 6
    volume = volumes.sum()
    moment = (volumes[:, None] * tri.sum(axis=1) / 4).sum(axis=0)
    cob = moment / volume if abs(volume) > 1e-12 else np.full(3, np.nan)
    return dict(
        volume=volume,
        waterplane=waterplane,
        cob_x=cob[0],
        cob_y=cob[1],
        cob_z=cob[2],
        max_span=max_span,
        max_aspect=max_aspect,
        crossings=crossings,
        bad_edges=bad_edges,
        degenerate=degenerate,
        above_water=int(np.any(panels[:, :, 2] > 1e-8)),
    )


def checks(values):
    """Return label, criterion, value and verdict; tolerances are task criteria."""
    criteria = (
        ("Waterline crossings", "= 0", "crossings", 0, 0),
        ("Above-water vertices", "= 0", "above_water", 0, 0),
        ("Degenerate panels", "= 0", "degenerate", 0, 0),
        ("Bad non-SWL edges", "= 0", "bad_edges", 0, 0),
        ("Maximum aspect", "<= 3.0", "max_aspect", 0, 3),
        ("Maximum span (m)", "<= 3.66", "max_span", 0, 3.66),
        (
            "Displacement (m3) [T4-6]",
            "13917 +/- 1%",
            "volume",
            13917 * 0.99,
            13917 * 1.01,
        ),
        (
            "Waterplane (m2) [criteria]",
            "380 +/- 1%",
            "waterplane",
            380 * 0.99,
            380 * 1.01,
        ),
        ("COB z (m) [T4-6]", "-13.15 +/- 1%", "cob_z", -13.15 * 1.01, -13.15 * 0.99),
    )
    return [
        (label, limit, values[key], "PASS" if low <= values[key] <= high else "FAIL")
        for label, limit, key, low, high in criteria
    ]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("mesh", type=Path)
    args = parser.parse_args()
    try:
        values = measure(read_gdf(args.mesh))
    except ValueError as error:
        print(f"GDF structure | valid full-body GDF | {error} | FAIL")
        return 1
    rows = checks(values)
    print("Check | Limit | Value | Result")
    for label, limit, value, verdict in rows:
        print(f"{label} | {limit} | {value:.1f} | {verdict}")
    print(
        f"COB x,y (m) [computed] | reported | {values['cob_x']:.1f}, {values['cob_y']:.1f} | INFO"
    )
    print("Published platform CM z (m) [T3-3] | metadata | -13.46 | INFO")
    print(
        "Requested system CM z | required public source | NOT_EVALUATED | NOT_EVALUATED"
    )
    return int(any(row[3] == "FAIL" for row in rows))


if __name__ == "__main__":
    raise SystemExit(main())
