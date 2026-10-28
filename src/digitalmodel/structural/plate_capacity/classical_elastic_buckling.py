# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Classical elastic rectangular-plate buckling under uniaxial compression.

Primary basis, derived here: for a perfect isotropic thin plate with bending
rigidity D=E*t^3/[12*(1-nu^2)], the linearized equilibrium equation is
D*(w_xxxx+2*w_xxyy+w_yyyy)+Nx*w_xx=0. Compression Nx is positive.
The simply supported mode w=W*sin(m*pi*x/a)*sin(n*pi*y/b) satisfies both
zero deflection and zero normal bending moment at all four edges.
Substitution gives Nx=D*pi^2*[(m/a)^2+(n/b)^2]^2/(m/a)^2.
For any fixed m, the smallest positive transverse integer is n=1. Hence
r=a/b, k(r,m)=(m/r+r/m)^2, and sigma_cr=k*pi^2*E*(t/b)^2/[12*(1-nu^2)].
The continuous minimum is at m=r; monotonicity on either side means only
floor(r) and ceil(r), restricted to positive integers, need comparison.
Adjacent modes exchange at r^2=m*(m+1), obtained by equating their k values.
The identity k=4+(m/r-r/m)^2 gives the lower bound 4, reached at integer r;
for noninteger r the minimum approaches 4 as r tends to infinity.

Accessible corroboration: MIT OpenCourseWare, 2.080J Structural Mechanics,
Fall 2013, Lecture 11, Section 11.2, pp. 11-3--11-4, Eqs. 11.11--11.17:
https://ocw.mit.edu/courses/2-080j-structural-mechanics-fall-2013/resources/mit2_080jf13_lecture11/
Historical references: On the Stability of a Plane Plate under Thrusts
in its own Plane, with Applications to the "Buckling" of the Sides of a Ship
(1891 attribution; DOI metadata dates publication to 1890),
https://doi.org/10.1112/plms/s1-22.1.54;
Theory of Elastic Stability, 2nd ed. (1961), plate chapter.
Historical source pages: page to be confirmed by the maintainer. These
references are not asserted to have been inspected; see the companion note.

The length a is parallel to compression; b is the transverse width. All four
edges are simply supported out of plane; transverse in-plane expansion is
free. No yielding, imperfections, residual stress, postbuckling capacity,
shear, biaxial load or design acceptance factor is represented.
"""

from math import floor, isfinite

from digitalmodel.structural.plate_capacity.elastic_buckling import (
    square_plate_elastic_buckling_stress,
)


def _positive_finite(name: str, value: float) -> None:
    """Reject nonphysical scalar dimensions and aspect ratios."""
    if not isfinite(value) or value <= 0:
        raise ValueError(f"{name} must be finite and positive")


def governing_half_waves(aspect_ratio: float) -> int:
    """Return the integer longitudinal half-wave count with the smallest k.

    Only the two integers bracketing a/b are candidates; no mode cutoff or
    long-plate approximation is imposed. Equal floating-point coefficients
    select the lower integer. Near a transition either tied mode is physical.
    """
    _positive_finite("aspect_ratio", aspect_ratio)
    if aspect_ratio <= 1:
        return 1
    lower = floor(aspect_ratio)
    return min(
        (lower, lower + 1),
        key=lambda mode: plate_buckling_coefficient(aspect_ratio, half_waves=mode),
    )


def plate_buckling_coefficient(
    aspect_ratio: float, *, half_waves: int | None = None
) -> float:
    """Return dimensionless k for a specified mode, or the minimum over modes.

    The modal relation and derivation appear in the module docstring. A mode
    is a positive integer along the load direction; the transverse mode is 1.
    Nonphysical inputs and unrepresentable coefficients raise ValueError.
    """
    _positive_finite("aspect_ratio", aspect_ratio)
    if half_waves is None:
        half_waves = governing_half_waves(aspect_ratio)
    if (
        isinstance(half_waves, bool)
        or not isinstance(half_waves, int)
        or half_waves < 1
    ):
        raise ValueError("half_waves must be a positive integer")
    try:
        coefficient = (half_waves / aspect_ratio + aspect_ratio / half_waves) ** 2
    except OverflowError as exc:
        raise ValueError("coefficient is outside the floating-point range") from exc
    if not isfinite(coefficient):
        raise ValueError("coefficient is outside the floating-point range")
    return coefficient


def rectangular_plate_elastic_buckling_stress(
    *,
    youngs_modulus: float,
    poisson_ratio: float,
    thickness: float,
    length: float,
    width: float,
) -> float:
    """Return the minimum critical compressive stress in modulus units.

    All lengths use one common unit. Length is parallel to the uniform load.
    The existing square-plate solution supplies the k=4 rigidity normalization;
    multiplying by k/4 gives the derived rectangular solution. Its physical
    input validation is retained, including -1 < nu < 0.5. Thin-plate and
    elastic applicability must be established separately.
    """
    _positive_finite("length", length)
    _positive_finite("width", width)
    coefficient = plate_buckling_coefficient(length / width)
    square_stress = square_plate_elastic_buckling_stress(
        youngs_modulus=youngs_modulus,
        poisson_ratio=poisson_ratio,
        thickness=thickness,
        side_length=width,
    )
    stress = square_stress * (coefficient / 4)
    if not isfinite(stress) or stress <= 0:
        raise ValueError("critical stress is outside the floating-point range")
    return stress
