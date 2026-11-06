# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Classical elastic square-plate buckling, independent of legacy scenarios.

Sources: MIT OpenCourseWare, 2.080J Structural Mechanics, Fall 2013:
- Lecture 11, Section 11.2, Eqs. 11.15-11.16 (buckling load and coefficient):
  https://ocw.mit.edu/courses/2-080j-structural-mechanics-fall-2013/resources/mit2_080jf13_lecture11/
- Lecture 4, Eq. 4.60 (plate rigidity); Eqs. 4.14 and 4.16 (elastic moduli):
  https://ocw.mit.edu/courses/2-080j-structural-mechanics-fall-2013/resources/mit2_080jf13_lecture4/
"""

from math import isfinite, pi


def square_plate_elastic_buckling_stress(
    *, youngs_modulus: float, poisson_ratio: float, thickness: float, side_length: float
) -> float:
    """Return positive critical compressive stress in the units of Young's modulus.

    Thickness and side length shall use the same length unit. The model is a
    geometrically perfect, thin, homogeneous isotropic elastic square plate,
    simply supported on all four edges, under uniform uniaxial compression.
    Transverse in-plane expansion is free (Lecture 11, Section 11.1).

    All inputs are required. Thin-plate applicability and elastic behavior shall
    be established separately; no thickness cutoff, yield check, safety factor,
    imperfection allowance or design acceptance is supplied by this function.
    Invalid physical inputs or an unrepresentable result raise ValueError.
    """
    for name, value in (
        ("youngs_modulus", youngs_modulus),
        ("thickness", thickness),
        ("side_length", side_length),
    ):
        if not isfinite(value) or value <= 0:
            raise ValueError(f"{name} must be finite and positive")

    # Positive bulk and shear moduli: MIT 2.080J (2013), Lecture 4,
    # Eqs. 4.14 and 4.16: K=E/[3(1-2*nu)], G=E/[2(1+nu)].
    if not isfinite(poisson_ratio) or not -1 < poisson_ratio < 0.5:
        raise ValueError("poisson_ratio must be finite and between -1 and 0.5")

    # MIT 2.080J (2013), Lecture 11, Eqs. 11.15-11.16: square a=b,
    # m=n=1 gives k=(1+1)^2=4. Lecture 4, Eq. 4.60 gives the factor 12
    # and (1-nu^2) in D=E*t^3/[12(1-nu^2)]. Divide Ncr by thickness.
    try:
        stress = (
            4
            * pi**2
            / (12 * (1 - poisson_ratio**2))
            * youngs_modulus
            * (thickness / side_length) ** 2
        )
    except OverflowError as exc:
        raise ValueError("critical stress is outside the floating-point range") from exc
    if not isfinite(stress) or stress <= 0:
        raise ValueError("critical stress is outside the floating-point range")
    return stress
