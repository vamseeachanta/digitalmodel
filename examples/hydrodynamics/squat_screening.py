# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic dredged-channel squat and UKC example; no solver or file writes.

Run with the source package on PYTHONPATH. Every number below is an explicit
synthetic input. In particular, critical speed and Huuska width factor are
illustrative required inputs, not qualified values for an actual channel.
"""

from dataclasses import replace
import math

from digitalmodel.hydrodynamics.parametric_hull_analysis.squat_screening import (
    ChannelKind,
    Formula,
    SquatInputs,
    SuppliedCorrections,
    UKCBudget,
    compare_formulas,
    density_draught_correction,
    maximum_speed,
    ukc_table,
)


def worked_example() -> dict:
    """Return all inputs and results for a synthetic bulk-carrier hull."""
    item = SquatInputs(
        length_bp_m=240,
        beam_m=30,
        draught_m=10,
        block_coefficient=0.8,
        water_depth_m=14.4,
        channel_width_m=120,
        speed_m_s=0,
        gravity_m_s2=9.81,
        channel_kind=ChannelKind.DREDGED,
    )
    # [D] section 8(1) interpolation, using synthetic FWA=.4 m and rho=1012.5.
    budget = UKCBudget(
        wave_allowance_m=0.4,
        tide_m=0.6,
        density_correction_m=density_draught_correction(1012.5, 0.4),
        heel_allowance_m=0.1,
        minimum_ukc_m=3,
    )
    corrections = SuppliedCorrections(
        critical_speed_m_s=8,
        huuska_width_factor=1.2,
        source="Synthetic required inputs; independent qualification required",
    )
    speeds = (2, 4, 6)
    effective = replace(
        item,
        water_depth_m=item.water_depth_m + budget.tide_m,
        draught_m=item.draught_m + budget.density_correction_m,
    )
    return {
        "inputs": item,
        "budget": budget,
        "corrections": corrections,
        "rows": {
            method: ukc_table(item, speeds, method, budget, corrections)
            for method in Formula
        },
        "comparisons": tuple(
            compare_formulas(replace(effective, speed_m_s=v), corrections)
            for v in speeds
        ),
        # Numerical tolerance and search cap are explicitly supplied inputs.
        "limits": {
            method: maximum_speed(item, method, budget, 6, 1e-6, corrections)
            for method in Formula
        },
    }


def render_example(data: dict) -> str:
    """Render metre results to one decimal; verdicts use unrounded results."""
    lines = [
        "Synthetic squat and under-keel clearance screening",
        "Navigation NOT_EVALUATED; supplied corrections require qualification.",
        "All verdicts apply only to the selected model and supplied allowances.",
        "Method | speed (m/s) | squat (m) | net UKC (m) | criterion",
    ]
    for method, rows in data["rows"].items():
        for row in rows:
            if row.meets_criterion is None:
                lines.append(
                    f"{method.value} | {row.speed_m_s:.1f} | NOT_EVALUATED: {row.squat.reason}"
                )
            else:
                verdict = "PASS" if row.meets_criterion else "FAIL"
                lines.append(
                    f"{method.value} | {row.speed_m_s:.1f} | "
                    f"{row.squat.maximum_m:.1f} | {row.net_ukc_m:.1f} | {verdict}"
                )
    lines.append(
        "Formula spread (m), by increasing speed: "
        + ", ".join(
            f"{c.spread_m:.1f}" if c.spread_m is not None else "NOT_EVALUATED"
            for c in data["comparisons"]
        )
    )
    for method, result in data["limits"].items():
        # Round a passing speed downward for display, so the label cannot
        # exceed the computed passing bracket. This is a display convention.
        speed = (
            "none"
            if result.speed_m_s is None
            else f"{math.floor(result.speed_m_s * 10) / 10:.1f}"
        )
        lines.append(f"{method.value}: screened speed {speed} m/s; {result.limited_by}")
    return "\n".join(lines)


if __name__ == "__main__":
    print(render_example(worked_example()))
