# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Reusable evidence gate for analytical candidates; no model calls or proof claims.

The required checks follow the analytical-discovery contract in issue #2239
and its 2026-09-27 plan, W5. A symbolic fit stays a surrogate. Passing finite
residual checks does not prove a PDE identity or arbitrary-hull applicability.
"""

from __future__ import annotations

from collections.abc import Mapping
from dataclasses import asdict, dataclass

from digitalmodel.naval_architecture.mesh_resistance import _finite


REQUIRED_CHECKS = (
    "dimensional_consistency",
    "governing_equations",
    "continuity",
    "boundary_conditions",
    "initial_conditions",
    "force_balance",
    "limiting_cases",
)
METHOD_CLASSES = frozenset(
    {
        "exact_under_assumptions",
        "asymptotic_approximation",
        "empirical_correlation",
        "numerical_pde_or_integral",
        "learned_surrogate",
    }
)


@dataclass(frozen=True)
class Candidate:
    expression: str
    method_class: str
    assumptions: tuple[str, ...]
    provenance: str

    def __post_init__(self) -> None:
        if self.method_class not in METHOD_CLASSES:
            raise ValueError(
                "method must have a recognized mathematical classification"
            )
        if not self.expression.strip() or not self.provenance.strip():
            raise ValueError("candidate expression and provenance must be declared")
        if not self.assumptions or any(not a.strip() for a in self.assumptions):
            raise ValueError("candidate assumptions must be declared")
        object.__setattr__(self, "assumptions", tuple(self.assumptions))


@dataclass(frozen=True)
class EvidenceCheck:
    """Nonnegative residual norm and predeclared same-unit limit, with sources.

    The caller supplies an independent calculation or reference, not a number
    recomputed by the candidate itself. Text records attestations; this gate
    cannot authenticate independence or evaluate symbolic expressions.
    """

    magnitude: float
    limit: float
    units: str
    criterion_source: str
    independent_reference: str

    def __post_init__(self) -> None:
        for name in ("magnitude", "limit"):
            value = _finite(name, getattr(self, name))
            if value < 0:
                raise ValueError("residual magnitude and limit must be non-negative")
            object.__setattr__(self, name, value)
        for name in ("units", "criterion_source", "independent_reference"):
            if not getattr(self, name).strip():
                raise ValueError(f"{name} must be declared")


def assess_candidate(
    candidate: Candidate,
    checks: Mapping[str, EvidenceCheck],
    domain_bounds: Mapping[str, tuple[float, float]],
    input_values: Mapping[str, float],
) -> dict:
    """Retain positive, missing and negative evidence and reject extrapolation.

    Bounds describe the caller's declared dimensioned or nondimensional inputs.
    No numerical tolerance or applicable range is invented by this gate.
    """
    if not domain_bounds or set(domain_bounds) != set(input_values):
        raise ValueError(
            "a complete declared domain and matching input point are required"
        )
    outside = []
    bounds, point = {}, {}
    for name, (lo, hi) in domain_bounds.items():
        lo, hi = _finite("lower bound", lo), _finite("upper bound", hi)
        value = _finite("domain input", input_values[name])
        if lo > hi:
            raise ValueError("domain bounds are reversed")
        bounds[name], point[name] = (lo, hi), value
        if not lo <= value <= hi:
            outside.append(name)
    unknown = set(checks) - set(REQUIRED_CHECKS)
    if unknown:
        raise ValueError("unrecognized analytical check name")
    missing = [name for name in REQUIRED_CHECKS if name not in checks]
    failed = [name for name, check in checks.items() if check.magnitude > check.limit]
    status = (
        "REJECTED"
        if failed or outside
        else "NOT_EVALUATED"
        if missing
        else "CHECKS_PASSED"
    )
    return {
        **asdict(candidate),
        "status": status,
        "formal_proof": False,
        "domain_bounds": bounds,
        "input_values": point,
        "outside_domain": outside,
        "missing_checks": missing,
        "failed_checks": failed,
        "checks": {name: asdict(check) for name, check in checks.items()},
        "limitation": "Recorded evidence is caller supplied; finite checks are not a formal proof",
    }
