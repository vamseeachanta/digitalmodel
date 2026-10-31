# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Compare the analytical plate mean with the package Blasius helper.

Basis: https://su2code.github.io/tutorials/Laminar_Flat_Plate/
The cited local law is Cf(x) = 0.664 / sqrt(Re_x). The plate mean below
follows by integrating x**(-1/2), without sampling the singular endpoint.
This is a formula check, not CFD validation; no solver is invoked.
"""

import math

from digitalmodel.solvers.openfoam.validation.flat_plate import blasius_plate_cd


def compare_coefficients(
    velocity: float, length: float, kinematic_viscosity: float
) -> dict[str, float]:
    """Return local and plate-mean coefficients for positive finite inputs.

    Use consistent length and time units: SI inputs are velocity in m/s,
    plate length in m and kinematic viscosity in m^2/s. The Reynolds number
    and both coefficients are dimensionless. The flow is assumed steady,
    incompressible, laminar and at zero pressure gradient on one plate face.

    Args:
        velocity: Uniform free-stream speed in length units per time unit.
        length: Plate length in the same length units.
        kinematic_viscosity: Viscosity in squared length units per time unit.

    Returns:
        Reynolds number, trailing-edge coefficient, analytical plate mean
        and the package plate coefficient, all dimensionless.
    """
    re_l = velocity * length / kinematic_viscosity
    cf_at_l = 0.664 / math.sqrt(re_l)  # Local law from the cited SU2 tutorial.
    # Cf(x) = Cf(L)*sqrt(L/x). Its improper integral divided by L is
    # [2*Cf(L)*sqrt(L*x)] from x=0 to L, divided by L = 2*Cf(L).
    # The factor 2 is from the power-rule antiderivative, not a fitted value.
    plate_cd_integral = 2.0 * cf_at_l
    return {
        "re_l": re_l,
        "cf_at_l": cf_at_l,
        "plate_cd_integral": plate_cd_integral,
        "plate_cd_helper": blasius_plate_cd(re_l),
    }


def main() -> None:
    """Run a synthetic SI example and check agreement with the helper."""
    # Illustrative inputs, not fluid-property data: U=1 m/s, L=0.1 m,
    # nu=0.00001 m^2/s. Hand calculation gives Re_L=10000.
    result = compare_coefficients(1.0, 0.1, 1.0e-5)
    if not math.isclose(result["plate_cd_integral"], result["plate_cd_helper"]):
        raise RuntimeError("Analytical plate mean differs from the package helper")
    print("Formula comparison only; no CFD validation.")
    print(f"Re_L = {result['re_l']:.0f}")
    print(f"Cf(L) = {result['cf_at_l']:.3e}")
    print(f"Plate mean from integral = {result['plate_cd_integral']:.3e}")
    print(f"Plate coefficient from helper = {result['plate_cd_helper']:.3e}")
    print(f"integral / Cf(L) = {result['plate_cd_integral'] / result['cf_at_l']:.1f}")


if __name__ == "__main__":
    main()
