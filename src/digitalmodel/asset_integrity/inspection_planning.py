# ABOUTME: Condition-based inspection-interval planning (API 510/570/653).
# ABOUTME: Remaining life from corrosion rate + half-life inspection interval.
"""Inspection planning — remaining life and next-inspection interval.

Condition-based (thickness/corrosion) inspection planning per the API
510 (pressure vessels) / 570 (piping) / 653 (tanks) approach:

    corrosion_rate = (t_previous - t_current) / years_between      [mm/yr]
    remaining_life = (t_current - t_required) / corrosion_rate     [yr]
    next_interval  = min(remaining_life / 2, code_max_interval)    [yr]

The "half-life rule" sets the next inspection at no more than half the
remaining life, capped by the code maximum interval. The component is flagged
for repair/retirement when the current thickness is already at or below the
minimum required thickness.

References: API 510 (pressure vessel inspection), API 570 (piping inspection),
API 653 (tank inspection) — remaining-life and inspection-interval rules.
"""

from __future__ import annotations

import json
import math
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import TYPE_CHECKING, Any
from collections.abc import Mapping

if TYPE_CHECKING:
    from digitalmodel.fatigue.threshold_growth import GrowthResult


@dataclass
class InspectionPlan:
    standard: str
    corrosion_rate_mm_yr: float
    corrosion_allowance_remaining_mm: float
    remaining_life_years: float | None  # None => not corroding (rate <= 0)
    half_life_interval_years: float | None
    code_max_interval_years: float
    next_inspection_interval_years: float
    screening_status: str
    notes: list[str]


@dataclass
class CrackInspectionPlan:
    """Same year/half-life/cap semantics as InspectionPlan, without corrosion."""

    remaining_life_cycles: float | None
    remaining_life_years: float | None
    half_life_interval_years: float | None
    code_max_interval_years: float
    next_inspection_interval_years: float
    screening_status: str
    growth_status: str
    policy_basis: str
    cycles_per_year: float
    life_divisor: float
    notes: list[str]


def validate_crack_policy_inputs(
    *,
    cycles_per_year: float,
    code_max_interval_years: float,
    life_divisor: float,
    policy_basis: str,
) -> tuple[float, float, float]:
    """Validate inspection policy before any potentially expensive growth work."""
    from digitalmodel.fatigue.threshold_growth import validate_scalar

    cycles_per_year = validate_scalar(cycles_per_year, "cycles_per_year")
    code_max_interval_years = validate_scalar(
        code_max_interval_years, "code_max_interval_years"
    )
    life_divisor = validate_scalar(life_divisor, "life_divisor")
    if min(cycles_per_year, code_max_interval_years, life_divisor) <= 0:
        raise ValueError("cycle rate, interval cap and divisor must be finite positive")
    # Repository policy: plan_inspection uses remaining_life / 2, above.
    if life_divisor < 2 or not isinstance(policy_basis, str) or not policy_basis.strip():
        raise ValueError("life_divisor must be >= 2 and policy_basis is required")
    return cycles_per_year, code_max_interval_years, life_divisor


def plan_crack_inspection(
    growth: GrowthResult,
    *,
    cycles_per_year: float,
    code_max_interval_years: float,
    life_divisor: float,
    policy_basis: str,
) -> CrackInspectionPlan:
    """Convert cycles to years and cap the interval using the supplied policy.

    Half-life semantics follow ``plan_inspection``. The policy divisor is at
    least 2; the cap and basis are required, rather than asserted to be API
    579 requirements. Arrest is unbounded only for the supplied constant-
    amplitude model and threshold; loading/environment changes require reassessment.
    """
    from digitalmodel.fatigue.threshold_growth import GrowthResult, validate_scalar

    if not isinstance(growth, GrowthResult):
        raise ValueError("growth must be a GrowthResult")
    cycles_per_year, code_max_interval_years, life_divisor = validate_crack_policy_inputs(
        cycles_per_year=cycles_per_year, code_max_interval_years=code_max_interval_years,
        life_divisor=life_divisor, policy_basis=policy_basis,
    )
    life = growth.life_cycles
    if growth.status not in {"ARRESTED", "LIMIT_REACHED"}:
        raise ValueError("unsupported growth status")
    if (growth.status == "ARRESTED") != (life is None):
        raise ValueError("growth status and remaining life disagree")
    if life is not None:
        life = validate_scalar(life, "remaining cycles")
        if life < 0:
            raise ValueError("remaining cycles must be finite non-negative")
    years = None if life is None else life / cycles_per_year
    if years is not None and not math.isfinite(years):
        raise ValueError("remaining years exceed finite range")
    half = None if years is None else years / 2
    interval = (code_max_interval_years if years is None else
                min(years / life_divisor, code_max_interval_years))
    return CrackInspectionPlan(
        life, years, half, code_max_interval_years, interval,
        "fail" if life == 0 else "pass", growth.status, policy_basis,
        cycles_per_year, life_divisor,
        ["Model-based screening; material calibration and inspection policy require qualification.",
         "Arrest applies only to the supplied loading and service threshold." if life is None
         else f"Interval {interval:.3g} yr = min(life {years:.3g} yr / "
              f"divisor {life_divisor:.3g}, policy cap {code_max_interval_years:.3g} yr)."],
    )


def validate_crack_plan(plan: CrackInspectionPlan) -> None:
    """Reject inconsistent mutable plan fields before an RBI recommendation."""
    from digitalmodel.fatigue.threshold_growth import validate_scalar

    if not isinstance(plan, CrackInspectionPlan):
        raise ValueError("plan must be a CrackInspectionPlan")
    rate, cap, divisor = validate_crack_policy_inputs(
        cycles_per_year=plan.cycles_per_year, code_max_interval_years=plan.code_max_interval_years,
        life_divisor=plan.life_divisor, policy_basis=plan.policy_basis,
    )
    if plan.growth_status == "ARRESTED":
        if plan.remaining_life_cycles is not None or plan.remaining_life_years is not None:
            raise ValueError("arrested growth requires null remaining life")
        years = half = None
        status, interval = "pass", cap
    elif plan.growth_status == "LIMIT_REACHED":
        life = validate_scalar(plan.remaining_life_cycles, "remaining cycles")
        years = validate_scalar(plan.remaining_life_years, "remaining years")
        if life < 0 or years != life / rate:
            raise ValueError("remaining cycles and years disagree")
        half = years / 2
        status = "fail" if life == 0 else "pass"
        interval = min(years / divisor, cap)
    else:
        raise ValueError("unsupported crack growth status")
    actual = validate_scalar(plan.next_inspection_interval_years, "plan interval")
    if actual != interval or plan.screening_status != status:
        raise ValueError("crack status or interval disagrees with life and policy")
    actual_half = (None if plan.half_life_interval_years is None else
                   validate_scalar(plan.half_life_interval_years, "half-life interval"))
    if actual_half != half:
        raise ValueError("half-life interval disagrees with remaining years")


def corrosion_rate_mm_yr(
    previous_mm: float, current_mm: float, years_between: float
) -> float:
    """Long-term corrosion rate from two thickness readings."""
    if years_between <= 0.0:
        raise ValueError("years_between must be positive")
    return (previous_mm - current_mm) / years_between


def plan_inspection(
    current_mm: float,
    required_mm: float,
    code_max_interval_years: float,
    corrosion_rate: float | None = None,
    previous_mm: float | None = None,
    years_between: float | None = None,
    required_remaining_life_years: float | None = None,
) -> InspectionPlan:
    """Remaining life and next-inspection interval (API 510/570/653)."""
    if current_mm <= 0.0 or required_mm <= 0.0:
        raise ValueError("current_mm and required_mm must be positive")
    if code_max_interval_years <= 0.0:
        raise ValueError("code_max_interval_years must be positive")

    if corrosion_rate is None:
        if previous_mm is None or years_between is None:
            raise ValueError(
                "supply corrosion_rate, or previous_mm and years_between to derive it"
            )
        corrosion_rate = corrosion_rate_mm_yr(previous_mm, current_mm, years_between)
    if corrosion_rate < 0.0:
        raise ValueError("corrosion_rate must be non-negative (metal loss)")

    notes: list[str] = []
    allowance_remaining = current_mm - required_mm

    if allowance_remaining <= 0.0:
        # Already at or below the minimum required thickness.
        return InspectionPlan(
            standard="API 510 / 570 / 653",
            corrosion_rate_mm_yr=corrosion_rate,
            corrosion_allowance_remaining_mm=allowance_remaining,
            remaining_life_years=0.0,
            half_life_interval_years=0.0,
            code_max_interval_years=code_max_interval_years,
            next_inspection_interval_years=0.0,
            screening_status="fail",
            notes=["current thickness at or below t_min — repair/retire/re-rate now"],
        )

    if corrosion_rate == 0.0:
        # Not corroding: remaining life is unbounded; interval = code maximum.
        notes.append("corrosion rate is zero — interval set by the code maximum")
        return InspectionPlan(
            standard="API 510 / 570 / 653",
            corrosion_rate_mm_yr=0.0,
            corrosion_allowance_remaining_mm=allowance_remaining,
            remaining_life_years=None,
            half_life_interval_years=None,
            code_max_interval_years=code_max_interval_years,
            next_inspection_interval_years=code_max_interval_years,
            screening_status="pass",
            notes=notes,
        )

    remaining_life = allowance_remaining / corrosion_rate
    half_life = remaining_life / 2.0
    next_interval = min(half_life, code_max_interval_years)
    if half_life < code_max_interval_years:
        notes.append("half-life rule governs (below the code maximum interval)")
    else:
        notes.append("code maximum interval governs")

    status = "pass"
    if (
        required_remaining_life_years is not None
        and remaining_life < required_remaining_life_years
    ):
        status = "fail"
        notes.append(
            f"remaining life {remaining_life:.1f} yr < "
            f"required {required_remaining_life_years:.1f} yr"
        )

    return InspectionPlan(
        standard="API 510 / 570 / 653",
        corrosion_rate_mm_yr=corrosion_rate,
        corrosion_allowance_remaining_mm=allowance_remaining,
        remaining_life_years=remaining_life,
        half_life_interval_years=half_life,
        code_max_interval_years=code_max_interval_years,
        next_inspection_interval_years=next_interval,
        screening_status=status,
        notes=notes,
    )


def router(cfg: dict) -> dict:
    settings = cfg.get("inspection_planning") or {}
    if "crack_growth" in settings:
        if "thickness" in settings:
            raise ValueError("supply crack_growth or thickness, not both")
        return _route_crack_growth(cfg, settings)
    thickness = settings["thickness"]
    corrosion = settings.get("corrosion", {})
    design = settings.get("design", {})

    plan = plan_inspection(
        current_mm=float(thickness["current_mm"]),
        required_mm=float(thickness["required_mm"]),
        code_max_interval_years=float(design.get("code_max_interval_years", 10.0)),
        corrosion_rate=(
            None
            if corrosion.get("rate_mm_per_year") is None
            else float(corrosion["rate_mm_per_year"])
        ),
        previous_mm=(
            None
            if corrosion.get("previous_mm") is None
            else float(corrosion["previous_mm"])
        ),
        years_between=(
            None
            if corrosion.get("years_between") is None
            else float(corrosion["years_between"])
        ),
        required_remaining_life_years=(
            None
            if design.get("required_remaining_life_years") is None
            else float(design["required_remaining_life_years"])
        ),
    )

    payload = {**settings, "result": asdict(plan)}
    payload["screening_status"] = plan.screening_status
    payload["summary_json"] = _write_summary(
        cfg, settings.get("outputs", {}), payload
    ).as_posix()
    cfg["inspection_planning"] = payload
    cfg["screening_status"] = plan.screening_status
    return cfg


def _route_crack_growth(cfg: dict, settings: dict) -> dict:
    """Route explicit crack inputs while preserving the thickness workflow."""
    from digitalmodel.fatigue.crack_growth import stress_intensity_factor
    from digitalmodel.fatigue.threshold_growth import GrowthLaw, validate_scalar

    from .crack_life import assess_crack_life, plan_crack_from_fad

    supplied = settings["crack_growth"]
    common = {"law", "stress_range_mpa", "geometry_factor", "initial_depth_mm", "steps",
              "cycles_per_year", "code_max_interval_years", "life_divisor", "policy_basis"}
    endpoint = {"critical_depth_mm", "wall_thickness_mm", "critical_depth_basis"}
    if not isinstance(supplied, Mapping) or common - supplied.keys():
        raise ValueError("crack_growth must supply a mapping with all required growth and policy inputs")
    if "fad_inputs" in supplied:
        if endpoint & supplied.keys():
            raise ValueError("FAD inputs and explicit depth limits are mutually exclusive")
    elif endpoint - supplied.keys():
        raise ValueError("supply FAD inputs or all explicit depth-limit inputs")
    if supplied.keys() - (common | endpoint | {"fad_inputs"}):
        raise ValueError("unknown crack_growth inputs")
    inputs = dict(supplied)
    material = inputs.pop("law")
    if not isinstance(material, Mapping) or set(material) != set(GrowthLaw.__dataclass_fields__):
        raise ValueError("law must supply every GrowthLaw parameter and no unknown keys")
    law = GrowthLaw(**material)
    stress = validate_scalar(inputs.pop("stress_range_mpa"), "stress_range_mpa")
    factor = validate_scalar(inputs.pop("geometry_factor"), "geometry_factor")
    if stress < 0:
        raise ValueError("stress_range_mpa must be finite non-negative")
    if factor <= 0:
        raise ValueError("geometry_factor must be finite positive")

    def delta_k(a):
        return stress_intensity_factor(stress, a, Y=factor)

    assess = plan_crack_from_fad if "fad_inputs" in inputs else assess_crack_life
    assessment = assess(law=law, delta_k=delta_k, **inputs)
    plan = assessment.inspection
    payload = {**settings, "result": asdict(plan),
               "crack_assessment": asdict(assessment),
               "screening_status": plan.screening_status}
    payload["summary_json"] = _write_summary(
        cfg, settings.get("outputs", {}), payload
    ).as_posix()
    cfg["inspection_planning"] = payload
    cfg["screening_status"] = plan.screening_status
    return cfg


def _write_summary(cfg: dict, output_cfg: dict, payload: dict[str, Any]) -> Path:
    directory = Path(output_cfg.get("directory", "results/inspection_planning"))
    if not directory.is_absolute():
        directory = Path(cfg.get("_config_dir_path", Path.cwd())) / directory
    directory.mkdir(parents=True, exist_ok=True)
    summary_path = directory / output_cfg.get(
        "summary_json", "inspection_planning_summary.json"
    )
    summary_path.write_text(
        json.dumps(payload, indent=2, allow_nan=False), encoding="utf-8", newline="\n"
    )
    return summary_path
