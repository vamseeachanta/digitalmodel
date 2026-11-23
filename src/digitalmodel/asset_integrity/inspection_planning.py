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
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Any


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
    payload["summary_json"] = str(
        _write_summary(cfg, settings.get("outputs", {}), payload)
    )
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
    summary_path.write_text(json.dumps(payload, indent=2), encoding="utf-8")
    return summary_path
