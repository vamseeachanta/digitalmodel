"""Orientation and principal-axes helpers of the fowt router.

OrcaFlex vessel orientation and 6D buoy attitude conventions, and the
principal axes of the RNA inertia tensor with the precision at which the
RNA principal-axes child buoy is written. Split from ``fowt_router``, which
re-exports every name.
"""

from __future__ import annotations

import math


def vessel_rotation(heel: float, trim: float, heading: float) -> list[list[float]]:
    """Vessel-to-global rotation for an OrcaFlex vessel orientation (degrees).

    OrcaFlex applies heading about Z, then trim about the new y, then heel
    about the new x (Orcina, OrcaFlex help, Vessel data), so R = Rz Ry Rx.
    Positive angles are right-handed about each axis.
    """
    cx, sx = math.cos(math.radians(heel)), math.sin(math.radians(heel))
    cy, sy = math.cos(math.radians(trim)), math.sin(math.radians(trim))
    cz, sz = math.cos(math.radians(heading)), math.sin(math.radians(heading))
    return [[cz * cy, cz * sy * sx - sz * cx, cz * sy * cx + sz * sx],
            [sz * cy, sz * sy * sx + cz * cx, sz * sy * cx - cz * sx],
            [-sy, cy * sx, cy * cx]]


def buoy_attitude(r: list[list[float]]) -> list[float]:
    """6D buoy rotations 1, 2, 3 (degrees) that reproduce the rotation ``r``.

    A 6D buoy applies rotation 1 about x, then rotation 2 about the new y,
    then rotation 3 about the new z (Orcina, OrcaFlex help, 6D buoys: common
    data), so R = Rx Ry Rz; rotation 2 lies in [-90, 90] degrees.
    """
    r2 = math.asin(max(-1.0, min(1.0, r[0][2])))
    r1 = math.atan2(-r[1][2], r[2][2])
    r3 = math.atan2(-r[0][1], r[0][0])
    return [math.degrees(r1), math.degrees(r2), math.degrees(r3)]


# Written precision of the RNA principal-axes child buoy. np.linalg.eigh
# results differ between numpy builds at about 1e-14 relative, which changed
# the generated text in the 14th significant figure. Moments are written to
# 11 significant figures (rounding step at most 5e-11 of each moment, 20 times
# inside the 1e-9 tensor-reconstruction tolerance) and the attitude to 1e-9
# degree (at most 9e-12 rad). Both steps are three or more orders of
# magnitude above the numpy differences, so builds agree unless a value lies
# within about 1e-14 of a rounding boundary.
RNA_MOMENT_SIGNIFICANT_FIGURES = 11
RNA_ATTITUDE_DECIMAL_PLACES = 9


def principal_axes(inertia: list[list[float]]) -> tuple[list[float], list[list[float]]]:
    """Principal moments and the rotation whose columns are the principal axes.

    Eigenvectors are assigned to the host axes one-to-one, choosing the
    permutation with the largest summed alignment |cos| (the first such
    permutation when axes tie, e.g. two principal axes at 45 deg to two host
    axes). Each column is signed so that its diagonal entry is non-negative;
    if that leaves a reflection, the column with the smallest diagonal entry
    is reversed, so the rotation is proper (det +1) and close to the
    identity. The moments are returned in that column order.
    """
    import itertools

    import numpy as np

    values, vectors = np.linalg.eigh(np.array(inertia, dtype=float))
    columns = list(max(
        itertools.permutations(range(3)),
        key=lambda perm: sum(abs(vectors[axis, perm[axis]]) for axis in range(3)),
    ))
    p = vectors[:, columns]
    moments = values[columns]
    for k in range(3):
        if p[k, k] < 0:
            p[:, k] = -p[:, k]
    if np.linalg.det(p) < 0:
        k = int(np.argmin(np.abs(np.diag(p))))
        p[:, k] = -p[:, k]
    return [float(x) for x in moments], p.tolist()
