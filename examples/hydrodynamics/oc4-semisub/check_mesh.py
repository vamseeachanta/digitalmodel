# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Offline low-order GDF checks; no solver import or licence is required.

Hydrostatic references: NREL/TP-5000-60601 (2014), Table 4-6, p.25.
Platform CM: Table 3-3, p.9. https://www.nlr.gov/docs/fy14osti/60601.pdf
Waterplane ~380 m2 and numerical acceptance limits: maintainer mesh criteria,
documented in README.md. These criteria are not publication-derived constants.
"""

import argparse
import math
from collections import Counter
from itertools import combinations
from pathlib import Path

import numpy as np


def panel_span_limit(omega, depth, gravity=9.80665, panels_per_wave=6):
    """Solve omega²=g k tanh(k h); lambda/6 is an ASSUMED study rule."""
    if any(not math.isfinite(x) or x <= 0 for x in
           (omega, depth, gravity, panels_per_wave)):
        raise ValueError("Dispersion inputs must be finite and positive")
    low, high = 0.0, max(omega**2 / gravity, 1 / depth)
    while gravity * high * math.tanh(high * depth) < omega**2:
        high *= 2
    for _ in range(100):
        mid = (low + high) / 2
        if gravity * mid * math.tanh(mid * depth) < omega**2:
            low = mid
        else:
            high = mid
    return 2 * math.pi / high / panels_per_wave


def df3(panels, frequencies, *, depth, gravity=9.80665):
    """Compare every maximum pairwise vertex span with the shortest wave."""
    frequencies = np.asarray(frequencies, dtype=float)
    panels = np.asarray(panels, dtype=float)
    if (frequencies.ndim != 1 or not frequencies.size
            or not np.all(np.isfinite(frequencies)) or np.any(frequencies <= 0)
            or panels.ndim != 3 or panels.shape[1:] != (4, 3)
            or not len(panels) or not np.all(np.isfinite(panels))):
        raise ValueError("Invalid DF3 mesh or frequency inputs")
    limit = panel_span_limit(float(max(frequencies)), depth, gravity)
    spans = np.max(np.stack([np.linalg.norm(panels[:, a] - panels[:, b], axis=1)
                            for a, b in combinations(range(4), 2)]), axis=0)
    oversized = int(np.count_nonzero(spans > limit))
    return {"pass": oversized == 0, "panels": len(panels),
            "max_span_m": float(max(spans)), "limit_m": limit,
            "oversized_panels": oversized}


def validate_lid(hull, lid):
    """Check planar interior coverage; shared hull boundary edges are permitted."""
    hull, lid = np.asarray(hull), np.asarray(lid)
    if (hull.ndim != 3 or hull.shape[1:] != (4, 3) or not len(hull)
            or lid.ndim != 3 or lid.shape[1:] != (4, 3) or not len(lid)
            or not np.isfinite(hull).all() or not np.isfinite(lid).all()
            or np.any(hull[:, :, 2] > 1e-8) or np.any(abs(lid[:, :, 2]) > 1e-8)):
        raise ValueError("Lid must be at SWL and hull below SWL")
    at_swl = abs(hull[:, :, 2]) < 1e-8
    if np.any(np.all(at_swl, axis=1)):
        raise ValueError("Hull overlaps the planar lid")
    boundary = Counter()
    for panel in np.round(hull[np.sum(at_swl, axis=1) >= 2], 8):
        for a, b in zip(panel, np.roll(panel, -1, axis=0)):
            if abs(a[2]) < 1e-8 and abs(b[2]) < 1e-8 and not np.array_equal(a, b):
                boundary[(tuple(b), tuple(a))] += 1
    directed = Counter()
    centroids = []
    area = 0.0
    for panel in np.round(lid, 8):
        vertices = [tuple(p) for p in panel]
        for i in range(4):
            if vertices[i] == vertices[(i + 1) % 4]:
                vertices.pop((i + 1) % 4)
                break
        if len(set(vertices)) != len(vertices):
            raise ValueError("Degenerate lid")
        for a, b in zip(vertices, vertices[1:] + vertices[:1]):
            directed[(a, b)] += 1
        for indices in ((0, 1, 2),) if len(vertices) == 3 else ((0, 1, 2), (0, 2, 3)):
            a, b, c = np.asarray(vertices)[list(indices)]
            signed = np.cross(b - a, c - a)[2] / 2
            if signed <= 1e-12:
                raise ValueError("Lid must have positive upward area")
            area += signed
            centroids.append((a + b + c) / 3)
    actual = Counter()
    for edge, count in directed.items():
        reverse = directed[(edge[1], edge[0])]
        if count != 1 or reverse > 1:
            raise ValueError("Lid duplicate or overlapping edges")
        if reverse == 0:
            actual[edge] = count
    if not boundary or actual != boundary:
        raise ValueError("Lid boundary must match the hull waterline exactly")
    edges = np.array(list(boundary))
    a, b = edges[:, 0], edges[:, 1]
    expected_area = np.sum(a[:, 0] * b[:, 1] - a[:, 1] * b[:, 0]) / 2
    if not math.isclose(area, expected_area, rel_tol=1e-7, abs_tol=1e-7):
        raise ValueError("Lid area must equal the hull waterplane")
    for chunk in np.array_split(np.asarray(centroids), max(1, len(centroids) // 256)):
        x, y = chunk[:, 0, None], chunk[:, 1, None]
        dy = b[:, 1] - a[:, 1]
        intersections = a[:, 0] + (y - a[:, 1]) * (b[:, 0] - a[:, 0]) / np.where(dy, dy, 1)
        crosses = ((a[:, 1] > y) != (b[:, 1] > y)) & (x < intersections)
        if np.any(np.sum(crosses, axis=1) % 2 != 1):
            raise ValueError("Lid panel outside the hull waterline")
    return {"pass": True, "panels": len(lid), "area_m2": float(area)}


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
        # Low-order GDF permits any adjacent pair to coincide, including 4/1.
        for index in range(4):
            repeated = (index + 1) % 4
            if vertices[index] == vertices[repeated]:
                vertices.pop(repeated)
                panel = np.delete(panel, repeated, axis=0)
                break
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
    )
    return [
        (label, limit, values[key], "PASS" if low <= values[key] <= high else "FAIL")
        for label, limit, key, low, high in criteria
    ] + [("COB z (m) [T4-6]", "reference -13.15", values["cob_z"], "INFO")]


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
