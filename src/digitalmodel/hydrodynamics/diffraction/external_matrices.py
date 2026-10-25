# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Work-preserving external matrix transfer and explicit native conversion.

Sources (public documentation accessed 2026-10-22):
* Handbook of Marine Craft Hydrodynamics and Motion Control, 2nd ed. (2021),
  rigid-body kinematics; public model gives the cross-product matrix S:
  https://www.fossen.biz/html/marineCraftModel.html
* OrcaWave help, Constraints, external matrix block equations; Model, Units:
  https://www.orcina.com/webhelp/OrcaWave/Content/html/Data,Constraints.htm
  https://www.orcina.com/webhelp/OrcaWave/Content/html/Data,Model.htm#Units
* BIPM SI prefixes table (accessed 2026-10-22): kilo is exactly 10**3.
  https://www.bipm.org/en/measurement-units/si-prefixes
* Aqwa Reference Manual, Release 2024 R2, sections 1.2.1, 1.4, 11.6.1,
  11.7.1: local axes at CoG, SI units, FIDP, and global-frame SSTF.
  https://ansyshelp.ansys.com/public/Views/Secured/corp/v242/en/aqwa_ref/aqwa_ref.html

The canonical model supports reciprocal linear matrices only. This module
does not establish passivity, restoring composition or solver validity.
"""

from __future__ import annotations

import json
from pathlib import Path
from typing import TYPE_CHECKING, Literal

import numpy as np

if TYPE_CHECKING:
    from digitalmodel.hydrodynamics.diffraction.input_schemas import VesselSpec


def transfer_matrix(matrix, target_to_source_offset) -> np.ndarray:
    """Return the matrix at target for parallel axes and small rotations.

    r is the target-to-source offset in metres. Rigid-body kinematics give
    u_source = u_target + theta cross r. With S(r)a = r cross a,
    T = [[I, -S(r)], [0, I]], q_source = T q_target and K_target = T.T K_source T.
    This follows by substituting q_source in virtual work or quadratic energy;
    the same congruence transfers linear damping using velocities and power.

    Hand example: K_source=diag(2,3,5,7,11,13), r=(0,0,2).
    Twice energy is 2(x+2*pitch)^2 + 3(y-2*roll)^2 + 5*z^2
    + 7*roll^2 + 11*pitch^2 + 13*yaw^2. The answer has diagonal
    (2,3,5,19,19,13), K15=K51=4, K24=K42=-6, all other couplings zero.
    Coefficients are synthetic, independently chosen test inputs.
    """
    values = np.asarray(matrix, dtype=float)
    offset = np.asarray(target_to_source_offset, dtype=float)
    if values.shape != (6, 6) or not np.all(np.isfinite(values)):
        raise ValueError("matrix must be finite and 6x6")
    if not np.array_equal(values, values.T):
        raise ValueError("matrix must be exactly symmetric")
    if offset.shape != (3,) or not np.all(np.isfinite(offset)):
        raise ValueError("offset must have three finite coordinates")
    x, y, z = offset
    skew = np.array([[0., -z, y], [z, 0., -x], [-y, x, 0.]])
    transform = np.eye(6)
    transform[:3, 3:] = -skew
    with np.errstate(over="ignore", invalid="ignore"):
        result = transform.T @ values @ transform
    if not np.all(np.isfinite(result)):
        raise ValueError("matrix transfer overflow")
    # Compute one triangle and reflect it: congruence is analytically symmetric.
    return np.triu(result) + np.triu(result, 1).T


def prepare_matrix(
    vessel: VesselSpec,
    kind: Literal["stiffness", "damping"],
    backend: Literal["orcawave", "aqwa"],
) -> dict | None:
    """Validate at emission and return native values with conversion evidence.

    CoG coordinates are the offset from the body origin in parallel body axes.
    AQWA damping is transferred to CoG (local axes); OrcaWave retains the
    supplied point. AQWA structural stiffness is refused: the old FISK card
    is not documented, while SSTF's global-frame semantics need qualification.
    """
    if kind not in ("stiffness", "damping") or backend not in ("orcawave", "aqwa"):
        raise ValueError("unsupported matrix kind or backend")
    # Models are mutable; construction-time validation alone is insufficient.
    vessel = type(vessel).model_validate(vessel.model_dump())
    field = f"external_{kind}"
    matrix = getattr(vessel, field)
    if matrix is None:
        return None
    point = getattr(vessel, f"{field}_reference_point")
    canonical = np.asarray(matrix, dtype=float)
    cog = np.asarray(vessel.inertia.centre_of_gravity, dtype=float)
    if vessel.inertia.mode == "free_floating" and (
        point == "centre_of_gravity" or backend == "aqwa"
    ):
        raise ValueError("external matrix reference is unsupported in free_floating mode")
    if backend == "aqwa" and kind == "stiffness" and np.any(canonical):
        raise ValueError(
            "AQWA external stiffness is NOT_EVALUATED: FISK is undocumented; "
            "SSTF global-frame composition requires qualification"
        )
    if backend == "orcawave":
        # OrcaWave Model / Units: SI force is kN, with m, s, rad unchanged.
        # BIPM SI prefixes table: kilo = 10**3, exactly 1000 N/kN in every block.
        scale = 1.0 / 1000.0
        normalized = canonical.copy()
        native_point = point
    else:
        # Aqwa Reference Manual 2024 R2, 1.4: N, m, kg, s, radians.
        scale = 1.0
        normalized = (transfer_matrix(canonical, -cog)
                      if point == "body_origin" else canonical.copy())
        native_point = "centre_of_gravity"
    native = normalized / (1.0 / scale)
    if not np.all(np.isfinite(native)) or np.any((normalized != 0) & (native == 0)):
        raise ValueError("native matrix conversion overflow or underflow")
    force = "kN" if backend == "orcawave" else "N"
    time = ".s" if kind == "damping" else ""
    return {
        "conversion_revision": "external-matrix-v1",
        "backend": backend,
        "kind": kind,
        "dof_order": ["surge", "sway", "heave", "roll", "pitch", "yaw"],
        "canonical_values": canonical.tolist(),
        "canonical_units": "SI: N, m, s, rad",
        "canonical_reference_point": point,
        "cog_offset_m": cog.tolist(),
        "normalized_values": normalized.tolist(),
        "normalized_units": "SI: N, m, s, rad",
        "normalized_reference_point": native_point,
        "native_values": native.tolist(),
        "native_reference_point": native_point,
        "native_units_by_block": [f"{force}{time}/m", f"{force}{time}/rad",
                                  f"{force}.m{time}/m", f"{force}.m{time}/rad"],
        "force_scale": scale,
    }


def write_matrix_provenance(spec, backend, output_dir) -> None:
    """Refresh conversion evidence, including an empty list after removal."""
    records = []
    for index, body in enumerate(spec.get_bodies()):
        for kind in ("stiffness", "damping"):
            record = prepare_matrix(body.vessel, kind, backend)
            if record is not None:
                records.append({"body_index": index, **record})
    path = Path(output_dir) / "external-matrix-provenance.json"
    path.write_text(json.dumps(records, indent=2, allow_nan=False) + "\n",
                    encoding="utf-8", newline="\n")
