# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Planar quasi-static response to sampled forces, without a licensed solver.

At every sample, solve equilibrium about an initially balanced reference.
For q=[surge m, sway m, yaw rad] and an anchor-to-fairlead unit vector n,
b=[nx, ny, x*ny-y*nx]; extension=b.q and T=T0+k*b.q. Thus K=sum(k*b*b.T).
These are Hooke's law and force/moment balance (OpenStax, University Physics
Volume 1, 2016, sections 15.1 and 12.1). Fenders use compression-only linear
springs, R=k*max(b.q-gap,0), with user-supplied stiffness and gap.

Only taut lines, fixed horizontal directions and small motions are supported.
Stiffness is an effective horizontal tangent input, not inferred from line type,
length or pretension. No vertical/catenary reconstruction, geometric stiffness,
inertia, damping, waves or dynamic amplification is included. Each timestamp
is a static equilibrium, not a time integration. Slack lines, unsupported
restraint, unbalanced reference loads, failed balance and user validity-bound
violations raise ValueError; no partial result is returned. No strength or
operability acceptance criteria are evaluated.
"""

from __future__ import annotations

from dataclasses import dataclass

import numpy as np

from .force_time_history import ForceTimeHistory
from .input_schemas import MooredVesselSpec, ResponseSettings

# docs/UNITS.md, "The rule", defines the exact legacy kN-to-N boundary.
_KN_TO_N = 1000.0


@dataclass
class MooringResponse:
    """SI histories; motions columns are surge, sway, yaw (radians)."""

    time: np.ndarray
    motions: np.ndarray
    line_tensions_n: dict[str, np.ndarray]
    fender_loads_n: dict[str, np.ndarray]
    residuals: np.ndarray  # surge N, sway N, yaw N m


def _arm(nx: float, ny: float, x: float, y: float) -> list[float]:
    """Rigid-body linear displacement and moment arm (OpenStax 2016, 12.1)."""
    return [nx, ny, x * ny - y * nx]


def solve_response(
    vessel: MooredVesselSpec,
    forces: ForceTimeHistory,
    settings: ResponseSettings,
) -> MooringResponse:
    """Return sampled equilibrium, tensions and unilateral fender reactions."""
    # Revalidation also covers models mutated after construction.
    vessel = MooredVesselSpec.model_validate(vessel.model_dump())
    settings = ResponseSettings.model_validate(settings.model_dump())
    time = np.asarray(forces.time, dtype=float)
    loads = np.column_stack([forces.surge, forces.sway, forces.yaw])
    if time.ndim != 1 or len(time) == 0 or loads.shape != (len(time), 3):
        raise ValueError("force and time arrays must be nonempty and aligned")
    if not np.isfinite(time).all() or not np.isfinite(loads).all():
        raise ValueError("force and time samples must be finite")
    if np.any(np.diff(time) <= 0):
        raise ValueError("time samples must strictly increase")
    lines = vessel.mooring_lines
    if not lines or len({line.line_id for line in lines}) != len(lines):
        raise ValueError("mooring lines must have unique populated identifiers")
    if len({f.fender_id for f in vessel.fenders}) != len(vessel.fenders):
        raise ValueError("fenders must have unique identifiers")
    rows, stiffness, pretension = [], [], []
    for line in lines:
        if line.horizontal_stiffness_n_m is None:
            raise ValueError("every line requires horizontal_stiffness_n_m stiffness")
        delta = np.array(
            [line.fairlead_x - line.anchor_x, line.fairlead_y - line.anchor_y]
        )
        reach = np.linalg.norm(delta)
        if not np.isfinite(reach) or reach <= 0 or reach > line.length:
            raise ValueError("line length must span a positive horizontal reach")
        rows.append(_arm(*(delta / reach), line.fairlead_x, line.fairlead_y))
        stiffness.append(line.horizontal_stiffness_n_m)
        pretension.append(line.pretension * _KN_TO_N)
    b = np.asarray(rows)
    k = np.asarray(stiffness)
    t0 = np.asarray(pretension)
    fb, fk, gaps = [], [], []
    for fender in vessel.fenders:
        normal = np.array([fender.normal_x, fender.normal_y])
        norm = np.linalg.norm(normal)
        if not np.isfinite(norm) or norm <= 0:
            raise ValueError("fender normal must be finite and nonzero")
        fb.append(_arm(*(normal / norm), fender.x, fender.y))
        fk.append(fender.stiffness_n_m)
        gaps.append(fender.gap_m)
    fb = np.asarray(fb).reshape((-1, 3))
    fk, gaps = np.asarray(fk), np.asarray(gaps)
    tol = np.array([settings.force_tolerance_n] * 2 + [settings.moment_tolerance_nm])
    if not all(np.isfinite(a).all() for a in (b, k, t0, fb, fk, gaps, tol)):
        raise ValueError("response inputs must be finite")
    if not np.isfinite([settings.max_translation_m, settings.max_yaw_rad]).all():
        raise ValueError("response bounds must be finite")
    if np.any(np.abs(b.T @ t0) > tol):
        raise ValueError("pretensions must balance at the reference position")
    matrix = b.T @ (k[:, None] * b)
    if np.linalg.matrix_rank(b) < 3:
        raise ValueError("mooring lines must restrain surge, sway and yaw")
    motions, tensions, reactions, residuals = [], [], [], []
    for load in loads:
        active = np.zeros(len(fk), dtype=bool)
        visited = set()
        while True:
            key = tuple(active)
            if key in visited:
                raise ValueError("fender contact iteration did not converge")
            visited.add(key)
            contact = fb[active]
            tangent = matrix + contact.T @ (fk[active, None] * contact)
            rhs = load + contact.T @ (fk[active] * gaps[active])
            try:
                q = np.linalg.solve(tangent, rhs)
            except np.linalg.LinAlgError as exc:
                raise ValueError("mooring lines must restrain all motions") from exc
            updated = fb @ q > gaps
            if np.array_equal(active, updated):
                break
            active = updated
        tension = t0 + k * (b @ q)
        reaction = fk * np.maximum(fb @ q - gaps, 0)
        if np.any(tension < 0):
            raise ValueError("slack line: taut tangent response is not applicable")
        if np.linalg.norm(q[:2]) > settings.max_translation_m:
            raise ValueError("translation exceeds supplied model validity bound")
        if abs(q[2]) > settings.max_yaw_rad:
            raise ValueError("yaw exceeds supplied model validity bound")
        residual = b.T @ (tension - t0) + fb.T @ reaction - load
        if not np.isfinite(q).all() or np.any(np.abs(residual) > tol):
            raise ValueError("equilibrium residual exceeds supplied tolerance")
        motions.append(q)
        tensions.append(tension)
        reactions.append(reaction)
        residuals.append(residual)
    return MooringResponse(
        time=time.copy(),
        motions=np.asarray(motions),
        line_tensions_n={
            line.line_id: np.asarray(tensions)[:, i] for i, line in enumerate(lines)
        },
        fender_loads_n={
            f.fender_id: np.asarray(reactions)[:, i]
            for i, f in enumerate(vessel.fenders)
        },
        residuals=np.asarray(residuals),
    )
