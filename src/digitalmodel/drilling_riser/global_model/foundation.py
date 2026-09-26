"""Conductor p-y springs: depth interpolation of p-y curves and the spring stations of a
foundation (nodes of the conductor line below the wellhead datum)."""

from __future__ import annotations

from typing import Any

import numpy as np

from .spec import Foundation, PYCurve

__all__ = ["PYCurve", "interpolate_py", "spring_stations", "spring_table_kn"]


def _on_grid(c: PYCurve, grid: list[float]) -> list[float]:
    return [float(v) for v in np.interp(grid, c.y_m, c.p_n_per_m)]


def interpolate_py(curves: list[PYCurve], depth_m: float) -> PYCurve:
    """p-y curve at a depth: linear in depth between the bracketing curves, on the union of their
    y points; the first / last curve holds above / below the tabulated range."""
    curves = sorted(curves, key=lambda c: c.depth_m)
    if depth_m <= curves[0].depth_m:
        return PYCurve(depth_m=depth_m, y_m=list(curves[0].y_m), p_n_per_m=list(curves[0].p_n_per_m))
    if depth_m >= curves[-1].depth_m:
        return PYCurve(depth_m=depth_m, y_m=list(curves[-1].y_m), p_n_per_m=list(curves[-1].p_n_per_m))
    for a, b in zip(curves, curves[1:]):
        if a.depth_m <= depth_m <= b.depth_m:
            if b.depth_m == a.depth_m:
                return b.model_copy(update={"depth_m": depth_m})
            grid = sorted(set(a.y_m) | set(b.y_m))
            w = (depth_m - a.depth_m) / (b.depth_m - a.depth_m)
            pa, pb = _on_grid(a, grid), _on_grid(b, grid)
            return PYCurve(depth_m=depth_m, y_m=grid, p_n_per_m=[(1 - w) * x + w * y for x, y in zip(pa, pb)])
    raise AssertionError("unreachable")


def spring_stations(f: Foundation) -> list[dict[str, Any]]:
    """Nodes from the datum (depth 0) down to, but excluding, the fixed base; each with its depth,
    arc length from the conductor base (End A) and tributary length."""
    nodes = [0.0]
    for s in f.sections:
        n = int(round(s.length_m / s.segment_length_m))
        top = nodes[-1]
        nodes += [top + s.length_m * (k + 1) / n for k in range(n)]
    total = nodes[-1]
    out = []
    for i, d in enumerate(nodes[:-1]):
        above = (d - nodes[i - 1]) / 2 if i > 0 else 0.0
        below = (nodes[i + 1] - d) / 2
        out.append({"depth_m": d, "arc_from_base_m": total - d, "tributary_m": above + below})
    return out


def spring_table_kn(curve: PYCurve, tributary_m: float, length0_m: float, far_extension_m: float) -> list[list[float]]:
    """Symmetric link spring table (length m, tension kN) about the unstretched length: tension is
    +p x tributary when the node moves away from the anchor (the link lengthens), and the reverse
    when it moves towards it; p is held flat beyond the last y point."""
    ys = list(curve.y_m) + [curve.y_m[-1] + far_extension_m]
    ps = list(curve.p_n_per_m) + [curve.p_n_per_m[-1]]
    rows = [[length0_m - y, -p * tributary_m / 1000.0] for y, p in zip(reversed(ys[1:]), reversed(ps[1:]))]
    rows.append([length0_m, 0.0])
    rows += [[length0_m + y, p * tributary_m / 1000.0] for y, p in zip(ys[1:], ps[1:])]
    return rows
