# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Input-side external matrix units, origins and generated-deck provenance.

Source: OrcaWave WebHelp (2026), Data: Constraints, external stiffness/damping
block units; Data: Model, Units (m, tonne, kN, s). AQWA Reference Manual
2025 R1 sections 1.2.1 (axes), 11.6.1 (FIDP), 11.7.1 (SSTF).
This contract does not define hydrostatic restoring composition or targets.
"""

from __future__ import annotations

import json
from pathlib import Path
from typing import TYPE_CHECKING, Any, Literal

if TYPE_CHECKING:
    from .input_schemas import BodySpec, DiffractionSpec, VesselSpec

MatrixKind = Literal["stiffness", "damping"]
Backend = Literal["aqwa", "orcawave"]


ORCAWAVE_ORIGINS = {
    "body_origin": "Body origin",
    "centre_of_mass": "Centre of mass",
}
DOF_ORDER = ["surge", "sway", "heave", "roll", "pitch", "yaw"]

# OrcaWave Data: Model / Units uses kN; Data: Constraints keeps rad for
# angular columns. SI kilo prefix: BIPM SI Brochure, 9th ed. (2019), Table 7.
# Only force changes: all four matrix blocks scale identically, including
# linear/angular coupling. There is no degree-to-radian conversion.
ORCAWAVE_PER_CANONICAL = 1.0 / 1000.0


def native_matrix(
    vessel: VesselSpec, kind: MatrixKind, backend: Backend,
) -> list[list[float]] | None:
    """Convert a validated canonical matrix; reject unsupported AQWA origins."""
    if kind not in ("stiffness", "damping") or backend not in ("aqwa", "orcawave"):
        raise ValueError("Unknown external matrix kind or backend")
    # Specs may have been mutated after construction; validate current state.
    vessel = type(vessel).model_validate(vessel.model_dump())
    values = getattr(vessel, f"external_{kind}")
    if values is None:
        return None
    point = getattr(vessel, f"external_{kind}_reference_point")
    if backend == "aqwa":
        if vessel.inertia.mode != "explicit":
            raise ValueError("AQWA external matrix requires an explicit mass reference point")
        if point == "body_origin" and any(vessel.inertia.centre_of_gravity):
            raise ValueError("AQWA external matrix reference point must coincide with centre_of_mass")
        # Fixed-column cards retain four significant figures. Record and use
        # the actual emitted values rather than claiming unrounded provenance.
        if any(len(f"{x:.3e}") > 10 for row in values for x in row):
            raise ValueError("AQWA external matrix exceeds E10 field width")
        return [[float(f"{x:.3e}") for x in row] for row in values]
    return [[x * ORCAWAVE_PER_CANONICAL for x in row] for row in values]


def validate_aqwa_matrix_axes(body: BodySpec) -> None:
    """Require aligned axes until a full matrix frame transform is supported."""
    if any(getattr(body.vessel, f"external_{kind}") is not None
           for kind in ("stiffness", "damping")) and any(body.attitude):
        raise ValueError("AQWA external matrix requires zero body attitude; frame transformation unsupported")


def matrix_units(kind: MatrixKind, force: str = "N") -> list[list[str]]:
    """Give a unit for every entry in surge, sway, heave, roll, pitch, yaw order."""
    suffix = "" if kind == "stiffness" else ".s"
    return [[
        f"{force}{'.m' if i >= 3 else ''}{suffix}/{'m' if j < 3 else 'rad'}"
        for j in range(6)
    ] for i in range(6)]


def write_matrix_provenance(
    spec: DiffractionSpec, output_dir: Path, backend: Backend,
) -> None:
    """Retain canonical input and applied native/normalized matrices beside decks."""
    records: list[dict[str, Any]] = []
    for index, body in enumerate(spec.get_bodies(), start=1):
        if backend == "aqwa":
            validate_aqwa_matrix_axes(body)
        for kind in ("stiffness", "damping"):
            native = native_matrix(body.vessel, kind, backend)
            if native is None:
                continue
            point = getattr(body.vessel, f"external_{kind}_reference_point")
            factor = ORCAWAVE_PER_CANONICAL if backend == "orcawave" else 1.0
            coordinates: list[float] | None = [0.0, 0.0, 0.0]
            if point == "centre_of_mass":
                # Free-floating COM coordinates are solver-derived; the
                # explicit-mode compatibility field is ignored by the deck.
                inertia = body.vessel.inertia
                coordinates = (list(inertia.centre_of_gravity)
                               if inertia.mode == "explicit" else None)
            records.append({
                "body_index": index,
                "kind": kind,
                "canonical_input_values": getattr(body.vessel, f"external_{kind}"),
                "native_values": native,
                "normalized_values": [[x / factor for x in row] for row in native],
                "native_units": matrix_units(kind, "kN" if backend == "orcawave" else "N"),
                "normalized_units": matrix_units(kind),
                "reference_point": point,
                "reference_point_body_coordinates_m": coordinates,
                "reference_point_body_coordinates_status": (
                    "unknown_solver_derived" if coordinates is None else "known"
                ),
                "axes": "body" if backend == "orcawave" else "global_parallel",
            })
    payload = {
        "conversion_revision": "external-matrices-v1",
        "backend": backend,
        "dof_order": DOF_ORDER,
        "sign_convention": "opposes displacement and velocity",
        "matrices": records,
    }
    path = Path(output_dir) / f"external_matrices.{backend}.json"
    path.write_text(json.dumps(payload, indent=2, allow_nan=False) + "\n",
                    encoding="utf-8", newline="\n")
