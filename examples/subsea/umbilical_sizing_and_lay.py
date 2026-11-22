# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Run the synthetic worksheet in docs/domains/umbilical/sizing-and-lay-analysis.md.

All properties and limits are illustrative inputs, assumed, based on previous
experience; none are prescribed by API 17E. Equations: OpenStax, University
Physics Volume 1 (2016), §§14.1, 14.4; Catenary Curve (2019), equations
(3), (4), (9), (10), (32), (34). Links and hand arithmetic are in the worksheet.
"""

from digitalmodel.subsea.umbilicals import (
    FunctionalElement,
    LayCase,
    QualifiedLimit,
    RadialBuildLayer,
    UmbilicalSection,
    analyze_lay,
    check_pressure,
    size_umbilical,
)


def main() -> None:
    """Print sizing, operating-pressure and static touchdown screening results."""
    basis = "Synthetic worksheet input; illustrative, requires qualification"
    element = FunctionalElement(
        name="chemical",
        kind="steel_tube",
        count=2,
        outer_diameter=0.02,
        inner_diameter=0.01,
        wall_density=8000.0,
        contents_density=1000.0,
        burst_limit=QualifiedLimit(2e6, basis),
        collapse_limit=QualifiedLimit(1e6, basis),
    )
    section = UmbilicalSection(
        elements=(element,),
        core_diameter=0.04,
        filler_density=1000.0,
        armour=(RadialBuildLayer(0.005, 8000.0, 0.5),),
        sheath=RadialBuildLayer(0.005, 1000.0, 1.0),
    )
    # Supplied fluid and gravity inputs, chosen for hand arithmetic, not defaults.
    sizing = size_umbilical(section, water_density=1000.0, gravity=10.0)
    pressure = check_pressure(
        section.elements,
        {"chemical": 3e6},
        water_depth=100,
        water_density=1000,
        gravity=10,
    )
    # H/w=100 m is a selected static geometry, not a prescribed tension.
    lay = analyze_lay(
        LayCase(
            configuration="parallel",
            submerged_weight=sizing.weight_in_water,
            horizontal_tension=100 * sizing.weight_in_water,
            vertical_drop=100,
            available_length=200,
            minimum_bend_radius=QualifiedLimit(50, basis),
            minimum_residual_tension=QualifiedLimit(5000, basis),
        )
    )
    print(f"Outer diameter: {sizing.outer_diameter * 1000:.1f} mm")
    print(f"Filled mass: {sizing.mass_per_length:.1f} kg/m")
    print(f"Air weight: {sizing.weight_in_air:.1f} N/m")
    print(f"Water weight: {sizing.weight_in_water:.1f} N/m")
    print(f"External pressure: {pressure.external_pressure / 1e6:.1f} MPa")
    print(
        f"Burst / collapse: {pressure.elements[0].burst.status} / "
        f"{pressure.elements[0].collapse.status}"
    )
    print(f"Suspended length: {lay.suspended_length:.1f} m")
    print(f"Touchdown offset: {lay.horizontal_span:.1f} m")
    print(f"Minimum radius: {lay.minimum_radius:.1f} m")
    print(f"Top effective tension: {lay.top_tension:.1f} N")
    print(f"Touchdown effective tension: {lay.touchdown_tension:.1f} N")
    print(f"Bend / residual: {lay.bend_check.status} / {lay.residual_check.status}")
    print(f"Standard qualification: {lay.standard_qualification.status}")


if __name__ == "__main__":
    main()
