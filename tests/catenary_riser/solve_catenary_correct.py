"""Solve general catenary BVP - clean version without unicode."""

import numpy as np
from scipy.optimize import fsolve

X = 800  # Horizontal span [m]
Y = 100  # Vertical span [m]
L = 1000  # Length [m]
w = 1962  # Weight per length [N/m]
EA = 64e9  # Stiffness [N]

def catenary_system(vars):
    """System of equations for general catenary BVP."""
    H, x1, x2 = vars

    if H <= 0 or x1 < 0 or x2 < 0:
        return [1e10, 1e10, 1e10]

    a = H / w
    h1 = a * np.cosh(x1 / a)
    h2 = a * np.cosh(x2 / a)
    s1 = a * np.sinh(x1 / a)
    s2 = a * np.sinh(x2 / a)
    elong = H * L / EA

    eq1 = (x2 - x1) - X
    eq2 = (h2 - h1) - Y
    eq3 = (s2 - s1 + elong) - L

    return [eq1, eq2, eq3]

# Initial guess
x1_guess = 400
x2_guess = x1_guess + X
H_guess = w * 400

print("Solving general catenary BVP...")
print(f"Initial guess: H={H_guess:.0f} N, x1={x1_guess:.0f} m, x2={x2_guess:.0f} m\n")

solution = fsolve(catenary_system, [H_guess, x1_guess, x2_guess], full_output=True)
vars_sol, info, ier, msg = solution

if ier == 1:
    H_sol, x1_sol, x2_sol = vars_sol

    print("SOLUTION FOUND:")
    print(f"  H = {H_sol:,.0f} N")
    print(f"  x1 = {x1_sol:.2f} m")
    print(f"  x2 = {x2_sol:.2f} m")

    a_sol = H_sol / w
    h1 = a_sol * np.cosh(x1_sol / a_sol)
    h2 = a_sol * np.cosh(x2_sol / a_sol)
    s1 = a_sol * np.sinh(x1_sol / a_sol)
    s2 = a_sol * np.sinh(x2_sol / a_sol)
    elong = H_sol * L / EA

    print(f"\nVerification:")
    print(f"  Horizontal span: {x2_sol - x1_sol:.2f} m (target: {X} m)")
    print(f"  Vertical span: {h2 - h1:.2f} m (target: {Y} m)")
    print(f"  Total length: {s2 - s1 + elong:.2f} m (target: {L} m)")

    V_fairlead = H_sol * np.sinh(x2_sol / a_sol)
    T_fairlead = np.sqrt(H_sol**2 + V_fairlead**2)

    print(f"\nTensions:")
    print(f"  H: {H_sol:,.0f} N")
    print(f"  V: {V_fairlead:,.0f} N")
    print(f"  T: {T_fairlead:,.0f} N")

    print(f"\nComparison to Excel (785000, 196200, 809000):")
    print(f"  H error: {abs(H_sol-785000)/785000*100:.1f}%")
    print(f"  V error: {abs(V_fairlead-196200)/196200*100:.1f}%")
    print(f"  T error: {abs(T_fairlead-809000)/809000*100:.1f}%")
else:
    print(f"FAILED: {msg}")
    print(f"Residuals: {info['fvec']}")
