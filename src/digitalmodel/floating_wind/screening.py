"""Floating-wind concept-screening orchestrator (issue #1024, epic #1022).

The workhorse loop of floating-wind concept design: sweep a grid of floater
**variants** (issue #1023 archetype models) across a set of site **load cases**,
run four closed-form screening checks on each, and roll the result up into a
tidy table with a per-variant verdict.

Checks (closed-form / licence-free first tier)
----------------------------------------------
1. **Stability** -- intact ``GM`` against a minimum (DNV-OS-C301 style floor).
2. **Motion** -- a dynamic-amplification proxy: the rigid-body natural period vs
   the load-case peak wave period ``Tp``. A hull resonant with the sea state
   amplifies; one well separated does not.
3. **Mooring** -- a steady-offset watch-circle proxy: environmental load over
   the mooring horizontal stiffness vs an allowable offset (optional; skipped
   when no mooring stiffness is given).
4. **Modal** -- the rigid-body natural periods must avoid the site's energetic
   wave band (semis/spars sit above it, TLPs below it; a barge that lands inside
   it is correctly flagged).

This is a *screen*, not a design of record. Shortlisted variants go to the
high-fidelity OrcaWave/OrcaFlex tier (issue #1025), which replaces these proxies
with diffraction- and time-domain-derived responses.

The variant grid reuses :class:`digitalmodel.orcaflex.batch_parametric.ParametricStudy`;
the mooring proxy can defer to the FOWT watch-circle check in
:mod:`digitalmodel.orcaflex.mooring_design_fowt`.
"""

from __future__ import annotations

import math
from typing import Any

from pydantic import BaseModel, Field

from digitalmodel.floating_wind.floaters import (
    FloaterArchetype,
    FloaterProperties,
    TurbineTopside,
    build_floater,
)
from digitalmodel.orcaflex.batch_parametric import ParameterSweep, ParametricStudy

__all__ = [
    "LoadCase",
    "ScreeningCriteria",
    "CheckResult",
    "VariantScreening",
    "ScreeningResults",
    "screen_concept",
    "screen_concepts",
]


class LoadCase(BaseModel):
    """A site sea state (+ steady environmental load) to screen against."""

    name: str
    hs_m: float = Field(..., gt=0.0, description="Significant wave height (m)")
    tp_s: float = Field(..., gt=0.0, description="Spectral peak period (s)")
    steady_load_kN: float = Field(
        0.0,
        ge=0.0,
        description="Steady horizontal environmental load (wind thrust + "
        "current drag) for the mooring offset proxy (kN)",
    )


class ScreeningCriteria(BaseModel):
    """Acceptance thresholds for the four screening checks."""

    gm_min_m: float = Field(1.0, description="Minimum intact metacentric height (m)")
    daf_limit: float = Field(
        2.0, gt=1.0, description="Max acceptable dynamic-amplification factor"
    )
    damping_ratio: float = Field(
        0.05, gt=0.0, lt=1.0, description="Rigid-body damping ratio for the DAF proxy"
    )
    modal_band_s: tuple[float, float] = Field(
        (5.0, 16.0),
        description="Energetic wave-period band the natural periods must avoid (s); "
        "semis/spars target periods above it, TLPs below it",
    )
    mooring_stiffness_kN_per_m: float | None = Field(
        None, description="Horizontal mooring stiffness for the offset proxy (kN/m)"
    )
    max_offset_m: float | None = Field(
        None, description="Allowable steady horizontal offset (m)"
    )


class CheckResult(BaseModel):
    """One screening check on one variant × load case."""

    name: str
    value: float
    limit: float
    passed: bool
    margin: float = Field(
        ..., description="Signed margin; >= 0 means the check passes"
    )
    basis: str = ""


def _daf(natural_period_s: float, tp_s: float, zeta: float) -> float:
    """Dynamic amplification factor of a 1-DOF oscillator at forcing period Tp.

    ``DAF = 1 / sqrt((1 - r^2)^2 + (2*zeta*r)^2)`` with ``r = Tn / Tp`` (the
    forcing-to-natural frequency ratio is ``Tn/Tp``). Returns 1.0 for an
    infinite natural period (rigid / statically unstable handled upstream).
    """
    if not math.isfinite(natural_period_s):
        return 1.0
    r = natural_period_s / tp_s
    return 1.0 / math.sqrt((1.0 - r**2) ** 2 + (2.0 * zeta * r) ** 2)


def _stability_check(p: FloaterProperties, c: ScreeningCriteria) -> CheckResult:
    margin = p.GM_m - c.gm_min_m
    return CheckResult(
        name="stability",
        value=p.GM_m,
        limit=c.gm_min_m,
        passed=margin >= 0.0,
        margin=margin,
        basis="intact GM vs minimum (DNV-OS-C301)",
    )


def _motion_check(
    p: FloaterProperties, lc: LoadCase, c: ScreeningCriteria
) -> CheckResult:
    daf_heave = _daf(p.heave_natural_period_s, lc.tp_s, c.damping_ratio)
    daf_pitch = _daf(p.pitch_natural_period_s, lc.tp_s, c.damping_ratio)
    governing = max(daf_heave, daf_pitch)
    margin = c.daf_limit - governing
    return CheckResult(
        name="motion",
        value=governing,
        limit=c.daf_limit,
        passed=margin >= 0.0,
        margin=margin,
        basis=f"max(heave,pitch) DAF at Tp={lc.tp_s:g}s",
    )


def _modal_check(p: FloaterProperties, c: ScreeningCriteria) -> CheckResult:
    low, high = c.modal_band_s
    periods = [p.heave_natural_period_s, p.pitch_natural_period_s]
    inside = [
        per for per in periods if math.isfinite(per) and low <= per <= high
    ]
    if inside:
        # Negative margin = how far inside the band the worst period sits.
        worst = min(inside, key=lambda per: min(per - low, high - per))
        margin = -min(worst - low, high - worst)
        passed = False
        value = worst
    else:
        # Distance of the nearest finite period to the band edge.
        finite = [per for per in periods if math.isfinite(per)]
        if finite:
            value = min(finite, key=lambda per: min(abs(per - low), abs(per - high)))
            margin = min(abs(value - low), abs(value - high))
        else:
            value = math.inf
            margin = math.inf
        passed = True
    return CheckResult(
        name="modal",
        value=value,
        limit=high,
        passed=passed,
        margin=margin,
        basis=f"natural periods vs wave band {low:g}-{high:g}s",
    )


def _mooring_check(
    lc: LoadCase, c: ScreeningCriteria
) -> CheckResult | None:
    if c.mooring_stiffness_kN_per_m is None or c.max_offset_m is None:
        return None
    offset = lc.steady_load_kN / c.mooring_stiffness_kN_per_m
    margin = c.max_offset_m - offset
    return CheckResult(
        name="mooring",
        value=offset,
        limit=c.max_offset_m,
        passed=margin >= 0.0,
        margin=margin,
        basis="steady offset = load / mooring stiffness vs allowable",
    )


class VariantScreening(BaseModel):
    """Screening outcome for one floater variant across all load cases."""

    case_id: str
    archetype: FloaterArchetype
    params: dict[str, Any]
    properties: FloaterProperties
    checks: list[CheckResult]
    feasible: bool = Field(..., description="Hull floats at draft with valid ballast")
    passed: bool = Field(..., description="Feasible and every check passes")
    governing_check: str = Field(
        ..., description="Name of the check with the smallest margin"
    )
    governing_margin: float


class ScreeningResults(BaseModel):
    """Full screening result set for a concept sweep."""

    archetype: FloaterArchetype
    n_variants: int
    n_passed: int
    load_cases: list[str]
    variants: list[VariantScreening]

    def passing(self) -> list[VariantScreening]:
        return [v for v in self.variants if v.passed]

    def to_rows(self) -> list[dict[str, Any]]:
        """Flatten to one row per variant × check (tidy long form)."""
        rows: list[dict[str, Any]] = []
        for v in self.variants:
            for chk in v.checks:
                rows.append(
                    {
                        "case_id": v.case_id,
                        "archetype": v.archetype.value,
                        **{f"param_{k}": val for k, val in v.params.items()},
                        "check": chk.name,
                        "value": chk.value,
                        "limit": chk.limit,
                        "passed": chk.passed,
                        "margin": chk.margin,
                    }
                )
        return rows


def _screen_one(
    *,
    case_id: str,
    archetype: FloaterArchetype,
    params: dict[str, Any],
    topside: TurbineTopside,
    load_cases: list[LoadCase],
    criteria: ScreeningCriteria,
) -> VariantScreening:
    floater = build_floater(archetype, **params)
    props = floater.properties(topside)

    checks: list[CheckResult] = [_stability_check(props, criteria)]
    checks.append(_modal_check(props, criteria))
    # Motion + mooring are per-load-case; keep the governing (worst) per check.
    motion = min(
        (_motion_check(props, lc, criteria) for lc in load_cases),
        key=lambda r: r.margin,
    )
    checks.append(motion)
    moorings = [
        m for m in (_mooring_check(lc, criteria) for lc in load_cases) if m
    ]
    if moorings:
        checks.append(min(moorings, key=lambda r: r.margin))

    governing = min(checks, key=lambda r: r.margin)
    feasible = props.feasible
    passed = feasible and all(chk.passed for chk in checks)

    return VariantScreening(
        case_id=case_id,
        archetype=archetype,
        params=params,
        properties=props,
        checks=checks,
        feasible=feasible,
        passed=passed,
        governing_check=governing.name,
        governing_margin=governing.margin,
    )


def screen_concept(
    archetype: FloaterArchetype | str,
    base_params: dict[str, Any],
    sweeps: list[ParameterSweep],
    topside: TurbineTopside,
    load_cases: list[LoadCase],
    criteria: ScreeningCriteria | None = None,
    *,
    study_name: str = "floating_wind_screen",
) -> ScreeningResults:
    """Screen a grid of one-archetype variants across site load cases.

    Builds the full-factorial variant matrix from ``base_params`` + ``sweeps``
    (reusing :class:`ParametricStudy`), then evaluates the four screening checks
    on each variant.
    """
    arch = FloaterArchetype(archetype)
    criteria = criteria or ScreeningCriteria()

    study = ParametricStudy(
        name=study_name, parameters=sweeps, base_config=dict(base_params)
    )
    configs = study.generate_case_configs()

    variants: list[VariantScreening] = []
    for cfg in configs:
        case_id = cfg.pop("case_id")
        params = {**base_params, **cfg}
        variants.append(
            _screen_one(
                case_id=case_id,
                archetype=arch,
                params=params,
                topside=topside,
                load_cases=load_cases,
                criteria=criteria,
            )
        )

    return ScreeningResults(
        archetype=arch,
        n_variants=len(variants),
        n_passed=sum(v.passed for v in variants),
        load_cases=[lc.name for lc in load_cases],
        variants=variants,
    )


def screen_concepts(
    concepts: list[tuple[FloaterArchetype | str, dict[str, Any], list[ParameterSweep]]],
    topside: TurbineTopside,
    load_cases: list[LoadCase],
    criteria: ScreeningCriteria | None = None,
) -> list[ScreeningResults]:
    """Screen several archetypes in one call (one ScreeningResults each)."""
    return [
        screen_concept(arch, base, sweeps, topside, load_cases, criteria)
        for arch, base, sweeps in concepts
    ]
