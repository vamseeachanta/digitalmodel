# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Print an illustrative 24 m soft-clay pipe-pile analysis as Markdown.

Run with PYTHONPATH=src python examples/geotechnical/soft_clay_pile.py.
The script writes only to stdout. Redirect it to an external results folder.
Equations and sources: docs/domains/geotechnical/soft-clay-pile-analysis.md.
Every engineering input below is assumed, based on previous experience;
the example does not represent site data or a calibrated material.
"""

import argparse
from datetime import date
import math

from digitalmodel.geotechnical.py_curves import (
    ClayLayer,
    CyclicDegradation,
    SoftClay,
    assemble_py_family,
    soft_clay_py,
    solve_lateral_pile,
)


def run_example(*, intervals: int) -> dict:
    """Compute two cycle counts using the same load, geometry and numerical controls."""
    if isinstance(intervals, bool) or not isinstance(intervals, int) or intervals < 4:
        raise ValueError("intervals must be an integer of at least four")
    # Assumed engineering inputs; no standard, site or material-table values.
    length, diameter, wall, modulus = 24.0, 1.0, 0.025, 210e6
    head_shear = 100.0
    strength_exponent, stiffness_exponent = 0.10, 0.15
    soil = SoftClay(20.0, 6.0, 0.02, 0.5)
    # Exact annulus integration: integral(y² dA)=pi*(Do⁴-Di⁴)/64.
    inertia = math.pi * (diameter**4 - (diameter - 2 * wall)**4) / 64
    ei = modulus * inertia
    depths = tuple(length * i / intervals for i in range(intervals + 1))
    layers = (ClayLayer(0, length, soil),)
    # Numerical choices, not empirical coefficients: logarithmic sampling
    # regularizes the singular initial analytical tangent by interpolation.
    grid = (0.0,) + tuple(0.00001 * 1.1**i for i in range(145))
    curve_depths, curve_displacements = (2.0, 10.0), (0.0, 0.005, 0.015, 0.05, 0.15, 0.3, 0.75)
    cases = []
    for cycles in (1, 100):
        degradation = CyclicDegradation(cycles, strength_exponent, stiffness_exponent)
        family = assemble_py_family(layers=layers, depths_m=depths, diameter_m=diameter,
                                    displacements_m=grid, cyclic=True,
                                    degradation=degradation)
        response = solve_lateral_pile(
            depths_m=depths, ei_kn_m2=ei, curves=family.curves,
            head_shear_kn=head_shear, head_moment_kn_m=0,
            displacement_tolerance_m=1e-8, force_tolerance_kn=1e-5,
            max_iterations=1000, relaxation=0.5,
        )
        curves = []
        for depth in curve_depths:
            curve = soft_clay_py(soil, depth_m=depth, diameter_m=diameter,
                                 displacements_m=curve_displacements,
                                 cyclic=True, degradation=degradation)
            curves.append(dict(depth_m=depth, p_kn_m=curve.curve.resistances))
        cases.append(dict(cycles=cycles, strength_factor=degradation.strength_factor,
                          stiffness_factor=degradation.stiffness_factor,
                          head_deflection_m=response.displacements_m[0],
                          force_balance_kn=response.force_balance_kn,
                          moment_balance_kn_m=response.moment_balance_kn_m,
                          residual_kn=response.residual_kn,
                          iterations=response.iterations, curves=curves))
    return dict(
        length_m=length, diameter_m=diameter, wall_m=wall,
        modulus_kn_m2=modulus, ei_kn_m2=ei, head_shear_kn=head_shear,
        soil=soil, strength_exponent=strength_exponent,
        stiffness_exponent=stiffness_exponent, intervals=intervals,
        curve_displacements_m=curve_displacements, cases=cases,
        status=dict(api_table_conformance="NOT_EVALUATED",
                    cyclic_material_calibration="NOT_EVALUATED",
                    nonlinear_published_case="NOT_EVALUATED",
                    foundation_design_acceptance="NOT_EVALUATED"),
    )


def render_report(result: dict) -> str:
    """Render engineering inputs and answers with one-decimal comparison results."""
    first, last = result["cases"]
    increase = (last["head_deflection_m"] / first["head_deflection_m"] - 1) * 100
    lines = [
        "# Soft-clay pile lateral analysis", "",
        "## Summary", "",
        f"A {result['length_m']:.1f} m pipe pile under {result['head_shear_kn']:.1f} kN "
        f"head shear has computed head deflections of {first['head_deflection_m'] * 1000:.1f} "
        f"and {last['head_deflection_m'] * 1000:.1f} mm at 1 and 100 cycles. "
        f"The increase is {increase:.1f} %. These are illustrative backbone solutions.", "",
        "## Revision history", "",
        "| Revision | Date | Change and reason |", "| --- | --- | --- |",
        f"| 0 | {date.today().isoformat()} | Initial illustrative analysis of explicit cycle softening. |", "",
        "## Inputs", "",
        "All engineering inputs are assumed, based on previous experience; "
        "no site or laboratory calibration is claimed.", "",
        "| Input | Value |", "| --- | --- |",
        f"| Embedded length / diameter | {result['length_m']:.1f} / {result['diameter_m']:.1f} m |",
        f"| Pipe wall / elastic modulus | {result['wall_m'] * 1000:.1f} mm / "
        f"{result['modulus_kn_m2'] / 1e6:.1f} GPa |",
        f"| EI | {result['ei_kn_m2']:.1f} kN m² |",
        "| su / effective unit weight | 20.0 kPa / 6.0 kN/m³ |",
        "| eps50 / J | 2.0 % / 0.5 |",
        "| Strength / stiffness exponents | 0.10 / 0.15 |",
        "| Head moment / toe boundary | 0.0 kN m / free |", "",
        "## Method", "",
        "OTC 1204 (1970) soft-clay cyclic backbones, as publicly reproduced by "
        "OpenPile, are sampled and used as symmetric lateral springs. Independent "
        "power-law factors reduce strength and secant stiffness. The finite-difference "
        "Euler beam uses trapezoidal spring widths and relaxed secant iteration.", "",
        f"The uniform mesh has {result['intervals']} intervals. Numerical controls: "
        "displacement increment <= 1e-8 m, nodal residual and total force error "
        "<= 1e-5 kN, total moment error <= 2.4e-4 kN m, relaxation 0.5 "
        "and at most 1000 iterations. The logarithmic curve grid begins at 1e-5 m "
        "and grows by a factor of 1.1 for 145 positive points.", "",
        "## Results", "",
        "| Cycles | Strength factor | Stiffness factor | Head deflection (mm) | Iterations |",
        "| --- | --- | --- | --- | --- |",
    ]
    for case in result["cases"]:
        lines.append(f"| {case['cycles']} | {case['strength_factor']:.1f} | "
                     f"{case['stiffness_factor']:.1f} | {case['head_deflection_m'] * 1000:.1f} | "
                     f"{case['iterations']} |")
    lines += ["", "Equilibrium checks use unrounded solutions:", "",
              "| Cycles | Max nodal residual (kN) | Force error (kN) | Moment error (kN m) |",
              "| --- | --- | --- | --- |"]
    for case in result["cases"]:
        lines.append(f"| {case['cycles']} | {case['residual_kn']:.1e} | "
                     f"{abs(case['force_balance_kn']):.1e} | "
                     f"{abs(case['moment_balance_kn_m']):.1e} |")
    lines += ["", "## Curve samples", "",
              "| Cycles | Depth (m) | y (mm) | p (kN/m) |", "| --- | --- | --- | --- |"]
    for case in result["cases"]:
        for curve in case["curves"]:
            for displacement, resistance in zip(result["curve_displacements_m"], curve["p_kn_m"]):
                lines.append(f"| {case['cycles']} | {curve['depth_m']:.1f} | "
                             f"{displacement * 1000:.1f} | {resistance:.1f} |")
    lines += ["", "## Limits and way forward", "",
              "The linear solver is compared independently with Beams on Elastic "
              "Foundation (1946), equation 19a. Absolute nonlinear pile deflection "
              "has no published comparison case. Obtain cyclic laboratory strength "
              "and stiffness data, qualify the selected exponents and compare against "
              "a suitable pile load test before design use.", ""]
    lines += [f"- {name}: {status}" for name, status in result["status"].items()]
    lines += ["", "Sources and equations: "
              "`docs/domains/geotechnical/soft-clay-pile-analysis.md`.", ""]
    return "\n".join(lines)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--intervals", type=int, default=240)
    arguments = parser.parse_args()
    print(render_report(run_example(intervals=arguments.intervals)), end="")
