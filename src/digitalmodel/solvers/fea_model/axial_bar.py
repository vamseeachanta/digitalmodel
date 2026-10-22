# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Solver-free axial chain assembly; use consistent force and length units.

Source: MIT 2.080J Structural Mechanics (Fall 2013), Lecture 4, Eq. 4.25
(N=EA*epsilon), and Lecture 2, Eq. 2.5 (epsilon=du/dx):
https://ocw.mit.edu/courses/2-080j-structural-mechanics-fall-2013/resources/mit2_080jf13_lecture4/
https://ocw.mit.edu/courses/2-080j-structural-mechanics-fall-2013/resources/mit2_080jf13_lecture2/
"""

from math import fsum, isfinite


def build_axial_bar_model(*, elements: list[dict[str, float]], tip_load: float) -> dict:
    """Assemble a fixed-left axial chain with a point force at its right end.

    Each element requires positive finite youngs_modulus (force/length²), area
    (length²) and length. tip_load is a finite signed force. For N and mm, E is
    MPa, stiffness is N/mm, displacement is mm and reaction is N. Small strain,
    prismatic homogeneous elements and a common axial force are assumed; no
    bending, geometric instability, yielding or distributed load is evaluated.

    Derivation from the MIT (2013) equations above: epsilon=(u2-u1)/L,
    N=EA*(u2-u1)/L. Nodal equilibrium gives K_e=(EA/L)*[[1,-1],[-1,1]].
    End displacement is P*sum(L/(EA)), with u0=0. Thus two elements have
    k_eq=1/(1/k1+1/k2)=k1*k2/(k1+k2), derived by adding their extensions.
    Hand calculation: E=200000 MPa, A=100 mm², L=1000 mm -> k=20000 N/mm;
    P=10000 N -> delta=0.5 mm. No external solver is imported or started.
    """
    if not elements:
        raise ValueError("elements must contain at least one bar")
    if not isfinite(tip_load):
        raise ValueError("tip_load must be finite")
    stiffnesses = []
    for element in elements:
        for name in ("youngs_modulus", "area", "length"):
            value = element[name]
            if not isfinite(value) or value <= 0:
                raise ValueError(f"{name} must be finite and positive")
        stiffness = element["youngs_modulus"] * element["area"] / element["length"]
        if not isfinite(stiffness) or stiffness <= 0:
            raise ValueError("stiffness is outside the floating-point range")
        stiffnesses.append(stiffness)

    count = len(elements) + 1
    matrix = [[0.0] * count for _ in range(count)]
    extensions = []
    displacements = [0.0]
    try:
        equivalent = 1.0 / fsum(1.0 / k for k in stiffnesses)
        for i, stiffness in enumerate(stiffnesses):
            # Element equilibrium derived above; shared-node terms add.
            matrix[i][i] += stiffness
            matrix[i + 1][i + 1] += stiffness
            matrix[i][i + 1] -= stiffness
            matrix[i + 1][i] -= stiffness
            extensions.append(tip_load / stiffness)
            displacements.append(fsum(extensions))
    except (OverflowError, ZeroDivisionError) as exc:
        raise ValueError("bar result is outside the floating-point range") from exc
    if equivalent <= 0 or not all(
        isfinite(value)
        for value in [equivalent, *displacements, *(v for row in matrix for v in row)]
    ):
        raise ValueError("bar result is outside the floating-point range")
    return {
        "element_stiffnesses": stiffnesses,
        "stiffness_matrix": matrix,
        "equivalent_stiffness": equivalent,
        "displacements": displacements,
        "reaction": -tip_load,
    }
