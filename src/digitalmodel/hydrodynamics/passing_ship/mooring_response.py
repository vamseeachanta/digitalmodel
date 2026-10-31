# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Planar quasi-static response to sampled forces, without a licensed solver.

At every sample, solve total equilibrium including the reference pretensions.
For q=[surge m, sway m, yaw rad] and an anchor-to-fairlead unit vector n,
b=[nx, ny, x*ny-y*nx]; extension=b.q and T=T0+k*b.q. Thus K=sum(k*b*b.T).
These are Hooke's law and force/moment balance (OpenStax, University Physics
Volume 1, 2016, sections 15.1 and 12.1). Fenders use compression-only linear
springs, R=k*max(b.q-gap,0), with user-supplied stiffness and gap.
Contact equilibrium is safeguarded by an Armijo line search (Nocedal and
Wright, Numerical Optimization, 2nd edition, 2006, section 3.1).

Only taut lines, fixed horizontal directions and small motions are supported.
Stiffness is an effective horizontal tangent input, not inferred from line type,
length or pretension. No vertical/catenary reconstruction, geometric stiffness,
inertia, damping, waves or dynamic amplification is included. Each timestamp
is a static equilibrium, not a time integration. Slack lines, unsupported
restraint, failed balance and user validity-bound
violations raise ValueError; no partial result is returned. No strength or
operability acceptance criteria are evaluated.
"""

from __future__ import annotations

from dataclasses import dataclass

import numpy as np

from .force_time_history import ForceTimeHistory
from .input_schemas import MooredVesselSpec, ResponseSettings

# docs/UNITS.md, "Rule 1", defines the exact legacy kN-to-N boundary.
_KN_TO_N = 1000.0
_CONTACT_ITERATIONS = 100
_CONTACT_BACKTRACKS = 60


@dataclass
class MooringResponse:
    """SI histories; motions columns are surge, sway, yaw (radians)."""

    time: np.ndarray
    motions: np.ndarray
    line_tensions_n: dict[str, np.ndarray]
    fender_loads_n: dict[str, np.ndarray]
    residuals: np.ndarray  # surge N, sway N, yaw N m
    surge_natural_period_s: float  # uncoupled dry-mass estimate, no added mass
    surge_stiffness_n_m: float


def _arm(nx: float, ny: float, x: float, y: float) -> list[float]:
    """Rigid-body linear displacement and moment arm (OpenStax 2016, 12.1)."""
    return [nx, ny, x * ny - y * nx]


def _contact_at_roundoff(matrix, rhs, fb, fk, gaps, q, closure, mismatch):
    """Allow only geometric and equilibrium errors at machine roundoff."""
    eps = np.finfo(float).eps
    gap_scale = np.abs(fb) @ np.abs(q) + gaps
    if np.any(np.abs(closure[mismatch]) > 8 * eps * gap_scale[mismatch]):
        return False
    reaction = fk * np.maximum(closure, 0)
    residual = matrix @ q + fb.T @ reaction - rhs
    scale = np.abs(matrix) @ np.abs(q) + np.abs(fb).T @ reaction + np.abs(rhs)
    return (np.isfinite(residual).all() and np.isfinite(scale).all()
            and np.all(np.abs(residual) <= 32 * eps * scale))


def _solve_contact(matrix, rhs, fb, fk, gaps):
    """Solve the strictly convex spring potential, accepting consistent contact.

    Nocedal and Wright (2006), 2nd edition, section 3.1: Armijo decrease
    safeguards Newton steps. The numerical choices c1=0.0001 and halving
    are algorithm controls, not physical criteria. User residual tolerances
    do not truncate the equilibrium solve.
    """
    q = np.linalg.solve(matrix, rhs)
    for _ in range(_CONTACT_ITERATIONS):
        closure = fb @ q - gaps
        compression = np.maximum(closure, 0)
        active = closure > 0
        contact = fb[active]
        tangent = matrix + contact.T @ (fk[active, None] * contact)
        candidate = np.linalg.solve(tangent, rhs + contact.T @ (fk[active] * gaps[active]))
        candidate_closure = fb @ candidate - gaps
        if not np.isfinite(candidate).all() or not np.isfinite(candidate_closure).all():
            raise ValueError("fender contact solution must be finite")
        mismatch = (candidate_closure > 0) != active
        if not np.any(mismatch) or _contact_at_roundoff(
            matrix, rhs, fb, fk, gaps, candidate, candidate_closure, mismatch,
        ):
            return candidate
        gradient = matrix @ q - rhs + fb.T @ (fk * compression)
        direction = candidate - q
        slope = gradient @ direction
        if not np.isfinite(slope) or slope >= 0:
            raise ValueError("fender contact direction is not descending")
        step = 1.0
        for _ in range(_CONTACT_BACKTRACKS):
            delta = step * direction
            trial_compression = np.maximum(closure + fb @ delta, 0)
            # Difference of potentials avoids subtracting large total energies.
            change = ((matrix @ q - rhs) @ delta + 0.5 * delta @ matrix @ delta
                      + 0.5 * (fk * (trial_compression - compression))
                      @ (trial_compression + compression))
            if np.isfinite(change) and change <= 0.0001 * step * slope:
                q = q + delta
                break
            step *= 0.5
        else:
            raise ValueError("fender contact line search did not converge")
    raise ValueError("fender contact iteration limit reached")


def solve_response(
    vessel: MooredVesselSpec,
    forces: ForceTimeHistory,
    settings: ResponseSettings,
) -> MooringResponse:
    """Return sampled equilibrium, tensions and unilateral fender reactions."""
    mass_tonnes = vessel.hull.displacement_tonnes
    if mass_tonnes is None or not np.isfinite(mass_tonnes) or mass_tonnes <= 0:
        raise ValueError("surge period requires finite positive displacement")
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
        if not np.isfinite(reach) or reach <= 0:
            raise ValueError("line must have a positive finite horizontal reach")
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
    matrix = b.T @ (k[:, None] * b)
    if np.linalg.matrix_rank(b) < 3:
        raise ValueError("mooring lines must restrain surge, sway and yaw")
    # OpenStax, University Physics Volume 1 (2016), 15.1: T=2*pi*sqrt(m/k).
    # Bare-line Kxx and displacement mass omit coupling, added mass and contact.
    # docs/UNITS.md, Rule 1: tonnes to kg is an exact factor of 1000.
    surge_period = 2 * np.pi * np.sqrt(mass_tonnes * 1000 / matrix[0, 0])
    if not np.isfinite(surge_period) or surge_period <= 0:
        raise ValueError("surge period estimate must be finite and positive")
    reference_load = b.T @ t0
    motions, tensions, reactions, residuals = [], [], [], []
    for load in loads:
        try:
            q = _solve_contact(matrix, load - reference_load, fb, fk, gaps)
        except np.linalg.LinAlgError as exc:
            raise ValueError("mooring lines must restrain all motions") from exc
        tension = t0 + k * (b @ q)
        reaction = fk * np.maximum(fb @ q - gaps, 0)
        if np.any(tension < 0):
            raise ValueError("slack line: taut tangent response is not applicable")
        if np.linalg.norm(q[:2]) > settings.max_translation_m:
            raise ValueError("translation exceeds supplied model validity bound")
        if abs(q[2]) > settings.max_yaw_rad:
            raise ValueError("yaw exceeds supplied model validity bound")
        residual = b.T @ tension + fb.T @ reaction - load
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
        surge_natural_period_s=float(surge_period),
        surge_stiffness_n_m=float(matrix[0, 0]),
    )
