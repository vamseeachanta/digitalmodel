# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Classical elastic plate buckling; consistent length and stress units.

Sources: MIT OpenCourseWare, 2.080J Structural Mechanics, Fall 2013:
- Lecture 11, Section 11.2, Eqs. 11.15-11.16 (buckling load and coefficient):
  https://ocw.mit.edu/courses/2-080j-structural-mechanics-fall-2013/resources/mit2_080jf13_lecture11/
- Lecture 4, Eq. 4.60 (plate rigidity); Eqs. 4.14 and 4.16 (elastic moduli):
  https://ocw.mit.edu/courses/2-080j-structural-mechanics-fall-2013/resources/mit2_080jf13_lecture4/
"""

from math import floor, isfinite, pi


def plate_buckling_coefficient(*, length: float, width: float) -> float:
    """Return dimensionless k for a simply supported rectangular plate.

    Length a is parallel to uniform uniaxial compression; width b is transverse.
    Both lengths use the same unit, e.g. mm. All four edges are simply
    supported and transverse in-plane expansion is free. MIT 2.080J (2013),
    Lecture 11, Eqs. 11.15-17: k=min_m(m*b/a+a/(m*b))**2, m a positive integer.
    The continuous minimum is at m=a/b. The function decreases before that
    point and increases afterwards, so only its adjacent integers are needed.
    For a=200 mm, b=100 mm: m=2 gives k=(1+1)**2=4 by hand.
    """
    for name, value in (("length", length), ("width", width)):
        if not isfinite(value) or value <= 0:
            raise ValueError(f"{name} must be finite and positive")
    ratio = length / width
    if not isfinite(ratio) or ratio <= 0:
        raise ValueError("aspect ratio is outside the floating-point range")
    lower = max(1, floor(ratio))
    try:
        coefficient = min((m / ratio + ratio / m) ** 2 for m in (lower, lower + 1))
    except OverflowError as exc:
        raise ValueError("coefficient is outside the floating-point range") from exc
    if not isfinite(coefficient):
        raise ValueError("coefficient is outside the floating-point range")
    return coefficient


def plate_elastic_buckling_stress(
    *,
    youngs_modulus: float,
    poisson_ratio: float,
    thickness: float,
    width: float,
    length: float | None = None,
) -> float:
    """Return critical compressive stress in the units of E (e.g. MPa).

    Thickness t, transverse width b and loaded length a use one length unit
    (e.g. mm). Perfect homogeneous isotropic thin plate, simply supported on
    all four edges, under uniform uniaxial compression with free transverse
    in-plane expansion. A missing length selects the long-plate limit k=4;
    a supplied length selects the minimum over integer longitudinal modes.
    MIT 2.080J (2013), Lecture 11, Eqs. 11.15-17 and Lecture 4, Eq. 4.60:
    sigma_cr=k*pi**2*E/[12*(1-nu**2)]*(t/b)**2.

    Hand calculation with synthetic inputs: E=120000 MPa, nu=0, t=1 mm,
    b=100 mm, a=200 mm -> m=2, k=4; sigma_cr=4*pi**2*120000/12/10000
    =39.47841760435743 MPa. This is an elastic bifurcation stress, with no
    yield limit, imperfection reduction, safety factor or design acceptance.
    Invalid physical inputs and unrepresentable results raise ValueError.
    """
    if not isfinite(width) or width <= 0:
        raise ValueError("width must be finite and positive")
    coefficient = (
        4.0
        if length is None
        else plate_buckling_coefficient(length=length, width=width)
    )
    # Reuse the validated k=4 expression, not a legacy scenario calculation.
    stress = square_plate_elastic_buckling_stress(
        youngs_modulus=youngs_modulus,
        poisson_ratio=poisson_ratio,
        thickness=thickness,
        side_length=width,
    ) * (coefficient / 4.0)
    if not isfinite(stress) or stress <= 0:
        raise ValueError("critical stress is outside the floating-point range")
    return stress


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

    Hand calculation, synthetic E=120000 MPa, nu=0, t=1 mm, b=100 mm:
    k=4 and sigma_cr=4*pi**2*120000/(12*10000)=39.47841760435743 MPa,
    using the MIT (2013) equations cited above.
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
