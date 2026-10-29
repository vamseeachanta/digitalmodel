# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Mesh-derived calm-water friction records, with missing components explicit.

Sources: ITTC 7.5-03-02-04 (2024, revision 02), section 2.1 for Re/Fn
and the unnumbered tutorial, printed page 8, for the friction correlation;
ITTC 7.5-02-03-01.4 (2021, revision 05), section 2.4.1 for resistance
coefficients and the form-factor decomposition. Neither source makes a
friction-only estimate a total hull-resistance prediction.
"""

from __future__ import annotations

from dataclasses import asdict, dataclass
import hashlib
import json
import math
from pathlib import Path

from digitalmodel import __version__
from digitalmodel.naval_architecture.friction_scaling import (
    Fluid,
    froude_number,
    ittc57_cf,
    ittc57_cf_cited,
    reynolds_number_si,
)
from digitalmodel.naval_architecture.mesh_hydrostatics import (
    TriMesh,
    compute_hydrostatics,
)


def _finite(name: str, value: float, *, positive: bool = False) -> float:
    value = float(value)
    if not math.isfinite(value) or (positive and value <= 0):
        raise ValueError(
            f"{name} must be finite" + (" and positive" if positive else "")
        )
    return value


@dataclass(frozen=True)
class FlowConditions:
    """Explicit SI inputs. None depth declares unbounded water, not unknown depth.

    Trim is forward minus aft draft over the mesh length. The kernel accepts
    smooth, bare geometry in calm water only. Density, viscosity and gravity
    have no hidden defaults. These conditions do not establish applicability
    of the turbulent friction correlation to a particular physical hull.
    """

    speed_m_s: float
    density_kg_m3: float
    viscosity_m2_s: float
    gravity_m_s2: float
    draft_m: float
    trim_m: float
    water_depth_m: float | None
    free_surface: str
    roughness: str
    appendages: str

    def __post_init__(self) -> None:
        for name in (
            "speed_m_s",
            "density_kg_m3",
            "viscosity_m2_s",
            "gravity_m_s2",
            "draft_m",
        ):
            object.__setattr__(
                self, name, _finite(name, getattr(self, name), positive=True)
            )
        object.__setattr__(self, "trim_m", _finite("trim_m", self.trim_m))
        if self.water_depth_m is not None:
            depth = _finite("water_depth_m", self.water_depth_m, positive=True)
            # Definition of mean draft and signed trim; half the draft difference.
            if depth <= self.draft_m + abs(self.trim_m) / 2:
                raise ValueError("water depth must exceed both end drafts")
            object.__setattr__(self, "water_depth_m", depth)
        for name, expected in (
            ("free_surface", "calm"),
            ("roughness", "smooth"),
            ("appendages", "absent"),
        ):
            if getattr(self, name) != expected:
                raise ValueError(
                    f"{name} must be declared {expected!r} for this method"
                )


def mesh_friction(
    mesh: TriMesh,
    conditions: FlowConditions,
    *,
    form_factor_k: float | None,
    implementation_revision: str,
    cite: bool = True,
    repo_root: Path | None = None,
) -> dict:
    """Return a JSON-ready record; no missing contribution is replaced by zero.

    Citation-enabled calls preserve the existing fail-closed Citation sidecar
    contract. Explicit ``cite=False`` leaves a visible unresolved reference.
    ``implementation_revision`` pins the caller's source revision independently
    of package version. A declared k supplies only the k*RF component.
    """
    if not implementation_revision.strip():
        raise ValueError("implementation revision must be declared")
    if form_factor_k is not None:
        form_factor_k = _finite("form_factor_k", form_factor_k)
        if form_factor_k < 0:
            raise ValueError("form_factor_k must be non-negative")
    hydro = compute_hydrostatics(mesh, conditions.draft_m, conditions.trim_m)
    area, length = hydro["S"].value, hydro["L_wl"].value
    fluid = Fluid("declared fluid", conditions.density_kg_m3, conditions.viscosity_m2_s)
    re = reynolds_number_si(conditions.speed_m_s, length, fluid)
    fn = froude_number(conditions.speed_m_s, length, g=conditions.gravity_m_s2)
    cf_record = (
        ittc57_cf_cited(re, repo_root=repo_root)
        if cite
        else {
            "value": ittc57_cf(re),
            "citations": [],
        }
    )
    cf = cf_record["value"]
    # ITTC 7.5-02-03-01.4 (2021 rev 05), sec 2.4.1: C = R/(rho*S*U^2/2).
    force = (
        conditions.density_kg_m3
        * conditions.speed_m_s
        * conditions.speed_m_s
        * area
        * cf
        / 2
    )
    if not all(math.isfinite(v) for v in (re, fn, force, force * conditions.speed_m_s)):
        raise ValueError("derived resistance quantities must be finite")
    if fn <= 0 or force <= 0:
        raise ValueError(
            "derived Froude number and friction force must remain positive"
        )
    if form_factor_k is not None and not all(
        math.isfinite(v) for v in (form_factor_k * force, form_factor_k * cf)
    ):
        raise ValueError("derived form resistance must be finite")

    def computed(value: float, coefficient: float) -> dict:
        return {"status": "COMPUTED", "force_N": value, "coefficient": coefficient}

    def missing(reason: str) -> dict:
        return {
            "status": "NOT_EVALUATED",
            "force_N": None,
            "coefficient": None,
            "reason": reason,
        }

    identity = {
        "hydrostatics_hash": hydro.input_hash,
        "conditions": asdict(conditions),
        "form_factor_k": form_factor_k,
        "software_version": __version__,
        "implementation_revision": implementation_revision,
    }
    digest = hashlib.sha256(
        json.dumps(identity, sort_keys=True, allow_nan=False).encode()
    ).hexdigest()
    return {
        "schema": "mesh-friction-v1",
        "input_hash": digest,
        **identity,
        "geometry_digest": mesh.source_digest,
        "geometry": {key: q.to_dict() for key, q in hydro.items()},
        "conventions": hydro.conventions,
        "method_class": "empirical_correlation",
        "method": "ITTC-1957 friction with optional declared form factor",
        "equation_sources": [
            "ITTC 7.5-03-02-04 (2024 rev 02), section 2.1: Reynolds and Froude numbers",
            "ITTC 7.5-03-02-04 (2024 rev 02), unnumbered tutorial, printed page 8: ITTC-1957 correlation line",
            "ITTC 7.5-02-03-01.4 (2021 rev 05), section 2.4.1: resistance coefficients and form-factor decomposition",
        ],
        "citations": [asdict(c) for c in cf_record["citations"]],
        "citation_status": "RESOLVED" if cf_record["citations"] else "UNRESOLVED",
        "unresolved_citations": []
        if cf_record["citations"]
        else ["ITTC-1957 correlation sidecar unavailable or explicitly omitted"],
        "reynolds": re,
        "froude": fn,
        "form_factor_provenance": "declared"
        if form_factor_k is not None
        else "missing",
        "components": {
            "friction": computed(force, cf),
            "viscous_form": computed(form_factor_k * force, form_factor_k * cf)
            if form_factor_k is not None
            else missing("form factor k was not supplied"),
            "wave": missing(
                "no independently benchmarked wave-resistance method selected"
            ),
            "total": missing(
                "friction and declared form factor do not close the force budget"
            ),
        },
        # Mechanical work definition P=R*U; ITTC 2024 section 1 distinguishes effective power.
        "friction_power_W": force * conditions.speed_m_s,
        "effective_power_W": None,
        "shaft_power_W": None,
        "uncertainty": {
            "status": "NOT_EVALUATED",
            "reason": "no physical uncertainty budget supplied",
        },
        "validation_status": "UNVALIDATED",
        "warnings": [
            "Mesh topology checks do not establish triangle self-intersection freedom or refinement convergence",
            "Turbulent friction correlation; transition and hull-specific applicability are not qualified",
            "Declared form factor is not inferred or validated from geometry",
            "Wave, separation, roughness, appendage, wind and design-margin contributions are not evaluated",
            "Finite-depth effects are not evaluated even when water depth is declared",
        ],
    }
