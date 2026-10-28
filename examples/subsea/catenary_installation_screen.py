# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Synthetic 1000 m static comparison; no design acceptance is implied.

Inputs are synthetic, not material presets or installation limits. Scale the
hand case in test_installation_planning.py: H=3, lower=4, buoy=6, upper=6,
w=1, net uplift=1 gives depth 12-2sqrt(13). Geometry scales by lambda and
forces by weight*lambda. Basis: Catenary Curve (2019), equations (1)-(14).
"""

import math

from digitalmodel.subsea.catenary_riser.planning import sweep_lazy_wave, sweep_simple


def worked_comparison():
    """Return unrounded calculations for independent checking or later limits."""
    depth, weight = 1000.0, 100.0  # Synthetic depth (m) and weight (N/m).
    scale = depth / (12 - 2 * math.sqrt(13))
    lazy = sweep_lazy_wave(
        weight=weight, buoyancy_start=4 * scale, buoyancy_length=6 * scale,
        net_buoyancy=weight, suspended_lengths=[16 * scale],
        top_tensions=[5 * weight * scale], hang_off_angles=[],
    )
    simple = sweep_simple(
        water_depth=depth, weight=weight,
        top_tensions=[3 * weight * scale + weight * depth],
        hang_off_angles=[], suspended_lengths=[],
    )
    if lazy.rejected or simple.rejected:
        raise ValueError("synthetic comparison is infeasible")
    return {"simple": simple.cases[0], "lazy_wave": lazy.cases[0]}


def render_worked_example() -> str:
    """Render one-decimal quantities; curvature is shown per kilometre."""
    lines = [
        "DRAFT — synthetic 1000 m static comparison; not certified.",
        "| Route | Depth m | Span m | Length m | Top kN | Bottom kN | Angle deg | Curvature 1/km |",
        "| --- | --- | --- | --- | --- | --- | --- | --- |",
    ]
    for name, state in worked_comparison().items():
        lines.append(
            f"| {name} | {state.water_depth:.1f} | {state.horizontal_span:.1f} | "
            f"{state.suspended_length:.1f} | {state.top_tension / 1000:.1f} | "
            f"{state.bottom_tension / 1000:.1f} | {state.hang_off_angle:.1f} | "
            f"{state.max_curvature * 1000:.1f} |"
        )
    return "\n".join(lines)


if __name__ == "__main__":
    print(render_worked_example())
