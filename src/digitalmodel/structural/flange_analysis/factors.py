"""Closed-form factors: ASME VIII-1 (2019), Fig 2-7.1 and Table 2-7.1.

The three-by-three Cramer solves below are algebraically the expanded
cofactor expressions in Table 2-7.1 equations (18)-(27).
"""

from math import log10


def flange_factors(K):
    """Return the diameter-ratio factors T, U, Y and Z."""
    k2 = K * K
    numerator = k2 * (1 + 8.55246 * log10(K)) - 1
    return {
        "K": K,
        "T": numerator / ((1.04720 + 1.9448 * k2) * (K - 1)),
        "U": numerator / (1.36136 * (k2 - 1) * (K - 1)),
        "Y": (0.66845 + 5.71690 * k2 * log10(K) / (k2 - 1)) / (K - 1),
        "Z": (k2 + 1) / (k2 - 1),
    }


def _coefficients(a, c):
    return [
        0,
        1/3 + a/12,
        5/42 + 17*a/336,
        1/210 + a/360,
        11/360 + 59*a/5040 + (1 + 3*a)/c,
        1/90 + 5*a/1008 - (1 + a)**3/c,
        1/120 + 17*a/5040 + 1/c,
        215/2772 + 51*a/1232 + (60/7 + 225*a/14 + 75*a*a/7 + 5*a**3/2)/c,
        31/6930 + 128*a/45045 + (6/7 + 15*a/7 + 12*a*a/7 + 5*a**3/11)/c,
        533/30240 + 653*a/73920 + (1/2 + 33*a/14 + 39*a*a/28 + 25*a**3/84)/c,
        29/3780 + 3*a/704 - (1/2 + 33*a/14 + 81*a*a/28 + 13*a**3/12)/c,
        31/6048 + 1763*a/665280 + (1/2 + 6*a/7 + 15*a*a/28 + 5*a**3/42)/c,
        1/2925 + 71*a/300300 + (8/35 + 18*a/35 + 156*a*a/385 + 6*a**3/55)/c,
        761/831600 + 937*a/1663200 + (1/35 + 6*a/35 + 11*a*a/70 + 3*a**3/70)/c,
        197/415800 + 103*a/332640 - (1/35 + 6*a/35 + 17*a*a/70 + a**3/10)/c,
        233/831600 + 97*a/554400 + (1/35 + 3*a/35 + a*a/14 + 2*a**3/105)/c,
    ]


def _det3(matrix):
    a, b, c = matrix
    return (a[0]*(b[1]*c[2] - b[2]*c[1])
            - a[1]*(b[0]*c[2] - b[2]*c[0])
            + a[2]*(b[0]*c[1] - b[1]*c[0]))


def _solve3(matrix, rhs):
    determinant = _det3(matrix)
    return [
        _det3([[rhs[i] if j == column else row[j] for j in range(3)]
               for i, row in enumerate(matrix)]) / determinant
        for column in range(3)
    ]


def hub_factors(*, g0, g1, h, h0):
    """Return integral hub F, V, f; apply the uniform-hub special case."""
    if g1 == g0:
        return {"F": 0.908920, "V": 0.550103, "f": 1.0}
    a = g1 / g0 - 1
    c = 43.68 * (h / h0)**4
    q = _coefficients(a, c)
    matrix = [[q[1], q[2], q[3]], [q[2], q[7], q[8]], [q[3], q[8], q[12]]]
    columns = [_solve3(matrix, [q[i], q[i+5], q[i+9]]) for i in (4, 5, 6)]
    c17, c20, c23 = columns[0]
    c18, c21, c24 = columns[1]
    c19, c22, c25 = columns[2]
    c26 = -(c / 4)**0.25
    c27 = c20 - c17 - 5/12 + c17*c26
    c28 = c22 - c19 - 1/12 + c19*c26
    c29 = -(c / 4)**0.5
    c30 = -(c / 4)**0.75
    c31 = 3*a/2 - c17*c30
    c32 = 1/2 - c19*c30
    c33 = 0.5*c26*c32 + c28*c31*c29 - 0.5*c30*c28 - c32*c27*c29
    c34 = 1/12 + c18 - c21 - c18*c26
    c35 = -c18*(c / 4)**0.75
    c36 = (c28*c35*c29 - c32*c34*c29)/c33
    c37 = (0.5*c26*c35 + c34*c31*c29 - 0.5*c30*c34 - c35*c27*c29)/c33
    e1 = c17*c36 + c18 + c19*c37
    e2 = c20*c36 + c21 + c22*c37
    e3 = c23*c36 + c24 + c25*c37
    e4 = 1/4 + c37/12 + c36/4 - e3/5 - 3*e2/2 - e1
    e5 = e1*(1/2 + a/6) + e2*(1/4 + 11*a/84) + e3*(1/70 + a/105)
    e6 = e5 - c36*(7/120 + a/36 + 3*a/c) - 1/40 - a/72 - c37*(1/60 + a/120 + 1/c)
    return {"F": -e6 / ((c / 2.73)**0.25 * (1+a)**3 / c),
            "V": e4 / ((2.73 / c)**0.25 * (1+a)**3), "f": max(1.0, c36/(1+a))}
