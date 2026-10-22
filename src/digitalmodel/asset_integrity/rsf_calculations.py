"""Fitness-for-Service (FFS) standalone calculation helpers.

Implements API 579-1 / ASME FFS-1 core formulas:
  - Remaining Strength Factor (RSF) for general metal loss
  - MAWP re-rating from measured thickness
  - Remaining life projection (linear and power-law models)
  - UT grid ingestion and Critical Thickness Profile (CTP) summary
"""
from __future__ import annotations

import csv
import math
from datetime import date, timedelta
from pathlib import Path
from typing import Literal

from .input_validation import finite_number

# API 579-1 Table 4-4: default RSF_allowable for Level 1/2
RSF_ALLOWABLE = 0.90


def calculate_rsf(
    t_measured: float,
    t_required: float,
    t_nominal: float,
) -> dict:
    """Remaining Strength Factor per API 579-1 Part 4 (General Metal Loss).

    RSF = (t_measured - t_future_corrosion_allowance) / t_required
    Simplified Level 1: RSF = t_measured / t_nominal (conservative)
    More refined: RSF = actual_t / required_t_for_full_pressure

    Args:
        t_measured: Current measured wall thickness (same units as others).
        t_required: Minimum required wall thickness from design code.
        t_nominal: Original nominal wall thickness.

    Returns:
        dict with rsf, ffs_acceptable, mawp_ratio, metal_loss_fraction.
    """
    finite_number("t_nominal", t_nominal, positive=True)
    finite_number("t_required", t_required, positive=True)
    finite_number("t_measured", t_measured, minimum=0)
    if t_nominal <= 0:
        raise ValueError("t_nominal must be positive")
    if t_required <= 0:
        raise ValueError("t_required must be positive")
    if t_measured < 0:
        raise ValueError("t_measured must be non-negative")

    # RSF: ratio of actual to nominal thickness (Level 1 conservative)
    rsf = t_measured / t_nominal
    # Refined RSF for MAWP comparison
    rsf_refined = t_measured / t_required if t_required > 0 else 0.0
    finite_number("derived rsf", rsf, minimum=0)
    finite_number("derived refined rsf", rsf_refined, minimum=0)
    # Acceptance needs both nominal-wall RSF and measured >= required wall.
    ffs_acceptable = rsf >= RSF_ALLOWABLE and t_measured >= t_required
    # MAWP scales linearly with remaining thickness
    mawp_ratio = t_measured / t_required if t_required > 0 else 0.0
    metal_loss_fraction = (t_nominal - t_measured) / t_nominal

    return {
        "rsf": rsf,
        "rsf_refined": rsf_refined,
        "ffs_acceptable": ffs_acceptable,
        "mawp_ratio": mawp_ratio,
        "metal_loss_fraction": metal_loss_fraction,
        "rsf_allowable": RSF_ALLOWABLE,
    }


def check_ffs_level1(
    t_measured: float,
    t_required: float,
    t_nominal: float,
    design_pressure_psi: float,
    smys_psi: float,
    outer_diameter_in: float,
    weld_efficiency: float = 1.0,
) -> dict:
    """Level 1 FFS acceptability check per API 579-1 Part 4.

    Combines RSF check with MAWP re-rating for pipe/vessel under internal pressure.
    MAWP = (2 * SMYS * E * t_measured) / OD  — simplified Barlow formula.

    Args:
        t_measured: Measured remaining wall thickness (inches).
        t_required: Code-minimum required wall thickness (inches).
        t_nominal: Original design wall thickness (inches).
        design_pressure_psi: Positive, finite design/operating pressure (psi).
        smys_psi: Specified minimum yield strength (psi).
        outer_diameter_in: Pipe outer diameter (inches).
        weld_efficiency: Weld joint efficiency (1.0 for seamless).

    Returns:
        dict with acceptable, rsf, mawp_psi, mawp_reduced, remaining_pressure_fraction.

    Raises:
        ValueError: Design pressure is non-positive or non-finite.
    """
    # API domain requirement, not a standard acceptance limit: issue 2598.
    # https://github.com/vamseeachanta/digitalmodel/issues/2598
    try:
        finite_number("design_pressure_psi", design_pressure_psi, positive=True)
    except ValueError:
        raise ValueError("design_pressure_psi must be positive and finite")

    finite_number("smys_psi", smys_psi, positive=True)
    finite_number("weld_efficiency", weld_efficiency, positive=True, maximum=1)
    finite_number("outer_diameter_in", outer_diameter_in, positive=True)
    rsf_result = calculate_rsf(t_measured, t_required, t_nominal)

    if outer_diameter_in <= 0:
        raise ValueError("outer_diameter_in must be positive")

    # Barlow MAWP with measured thickness
    mawp_psi = (2.0 * smys_psi * weld_efficiency * t_measured) / outer_diameter_in
    finite_number("derived mawp_psi", mawp_psi, minimum=0)

    # All three criteria must pass: RSF, required wall and pressure capacity.
    acceptable = (rsf_result["ffs_acceptable"] and mawp_psi >= design_pressure_psi)
    remaining_pressure_fraction = mawp_psi / design_pressure_psi

    return {
        "acceptable": acceptable,
        "rsf": rsf_result["rsf"],
        "mawp_psi": mawp_psi,
        "mawp_reduced": mawp_psi < design_pressure_psi,
        "remaining_pressure_fraction": remaining_pressure_fraction,
        "metal_loss_fraction": rsf_result["metal_loss_fraction"],
    }


def remaining_life(
    t_current: float,
    t_minimum: float,
    corrosion_rate_in_per_yr: float,
    model: Literal["linear", "power_law"] = "linear",
    power_law_exponent: float = 1.0,
    reference_date: date | None = None,
) -> dict:
    """Remaining life projection to minimum allowable thickness.

    Args:
        t_current: Current measured wall thickness.
        t_minimum: Minimum allowable wall thickness (code limit).
        corrosion_rate_in_per_yr: Annual metal loss rate (positive = thinning).
        model: "linear" (constant rate) or "power_law" (accelerating).
        power_law_exponent: n in rate(t) = rate_0 * t^n; 1.0 = linear.
        reference_date: Inspection date; defaults to today.

    Returns:
        dict with remaining_years, inspection_date (ISO str), half_life_date.
    """
    if corrosion_rate_in_per_yr <= 0:
        raise ValueError("corrosion_rate_in_per_yr must be positive")
    if t_current < t_minimum:
        return {
            "remaining_years": 0.0,
            "inspection_date": None,
            "half_life_date": None,
            "already_below_minimum": True,
        }

    thickness_margin = t_current - t_minimum

    if model == "linear":
        remaining_years = thickness_margin / corrosion_rate_in_per_yr
    elif model == "power_law":
        # Integrate: dt/dτ = -rate_0 * τ^n → solve for τ when margin consumed
        n = power_law_exponent
        if abs(n - 1.0) < 1e-6:
            remaining_years = thickness_margin / corrosion_rate_in_per_yr
        else:
            # Numerical estimate: iterative accumulation
            yr = 0.0
            step = 0.1
            accumulated = 0.0
            while accumulated < thickness_margin and yr < 1000:
                rate = corrosion_rate_in_per_yr * (yr + step) ** (n - 1)
                accumulated += rate * step
                yr += step
            remaining_years = yr
    else:
        raise ValueError(f"Unknown model: {model!r}. Use 'linear' or 'power_law'.")

    ref = reference_date or date.today()
    inspection_date = ref + timedelta(days=int(remaining_years * 365))
    half_life_date = ref + timedelta(days=int(remaining_years * 365 / 2))

    return {
        "remaining_years": remaining_years,
        "inspection_date": inspection_date.isoformat(),
        "half_life_date": half_life_date.isoformat(),
        "already_below_minimum": False,
    }


def process_ut_grid(
    csv_path: str | Path,
    t_nominal: float,
    t_required: float,
) -> dict:
    """Ingest UT thickness measurement grid CSV and compute CTP summary.

    CSV format: rows = axial stations, columns = circumferential stations.
    All values are wall thickness measurements in the same units as t_nominal.

    Args:
        csv_path: Path to CSV file with thickness measurements.
        t_nominal: Original nominal wall thickness.
        t_required: Code-minimum required wall thickness.

    Returns:
        dict with t_min, t_avg, t_max, metal_loss_pct, critical_points,
        grid_shape, below_required.
    """
    finite_number("t_nominal", t_nominal, positive=True)
    finite_number("t_required", t_required, positive=True)
    path = Path(csv_path)
    if not path.exists():
        raise FileNotFoundError(f"UT grid CSV not found: {path}")

    measurements: list[list[float]] = []
    missing_readings = invalid_readings = 0
    row_widths = []
    with path.open(encoding="utf-8", newline="") as f:
        reader = csv.reader(f)
        for row in reader:
            row_widths.append(len(row))
            parsed = []
            for cell in row:
                cell = cell.strip()
                if not cell:
                    missing_readings += 1
                    continue
                try:
                    value = float(cell)
                except ValueError:
                    invalid_readings += 1
                    continue
                if not math.isfinite(value) or value < 0:
                    invalid_readings += 1
                else:
                    parsed.append(value)
            measurements.append(parsed)

    if not measurements:
        raise ValueError("UT grid CSV contains no valid data")

    rectangular = len(set(row_widths)) == 1 and row_widths[0] > 0
    missing_readings += sum(max(row_widths) - width for width in row_widths)
    if missing_readings or invalid_readings or not rectangular:
        # A complete rectangular measurement grid is required for any clearance.
        return dict(evaluation_status="NOT_EVALUATED",
                    reason="UT grid requires complete finite nonnegative readings in every cell",
                    missing_readings=missing_readings, invalid_readings=invalid_readings,
                    grid_shape=(len(row_widths), max(row_widths)),
                    t_min=None, t_avg=None, t_max=None, metal_loss_pct=None,
                    critical_points=None, total_points=sum(row_widths), below_required=None)
    flat = [v for row in measurements for v in row]
    t_min = min(flat)
    t_max = max(flat)
    t_avg = sum(flat) / len(flat)
    critical_points = sum(1 for v in flat if v < t_required)
    metal_loss_pct = (t_nominal - t_avg) / t_nominal * 100 if t_nominal > 0 else 0.0

    return {
        "t_min": t_min,
        "t_avg": t_avg,
        "t_max": t_max,
        "metal_loss_pct": metal_loss_pct,
        "critical_points": critical_points,
        "total_points": len(flat),
        "below_required": critical_points > 0,
        "grid_shape": (len(measurements), len(measurements[0]) if measurements else 0),
    }
