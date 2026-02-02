"""
Solve general catenary BVP where anchor and fairlead are both above the low point.

For a catenary with low point somewhere below the anchor:
  Anchor at (0, h1) above low point
  Fairlead at (X, h2) above low point
  Where h2 - h1 = Y (vertical span, positive = fairlead higher)

Catenary equations from low point:
  y = a*cosh(x/a)
  s = a*sinh(x/a)

At anchor (x=x1, y=h1):
  h1 = a*cosh(x1/a)
  s1 = a*sinh(x1/a)

At fairlead (x=x2, y=h2):
  h2 = a*cosh(x2/a)
  s2 = a*sinh(x2/a)

Constraints:
  x2 - x1 = X (horizontal span)
  h2 - h1 = Y (vertical span)
  s2 - s1 + elongation = L (total length)

This gives 3 equations in 3 unknowns (a or H, x1, x2).
"""

import numpy as np
from scipy.optimize import fsolve

# Inputs
X = 800  # Horizontal span [m]
Y = 100  # Vertical span [m] (positive = fairlead higher than anchor)
L = 1000  # Length [m]
w = 1962  # Weight per length [N/m]
EA = 64e9  # Stiffness [N]

def catenary_system(vars):
    """
    System of equations for general catenary BVP.

    vars = [H, x1, x2]
    where:
      H = horizontal tension
      x1 = horizontal distance from low point to anchor
      x2 = horizontal distance from low point to fairlead
    """
    H, x1, x2 = vars

    if H <= 0 or x1 < 0 or x2 < 0:
        return [1e10, 1e10, 1e10]

    a = H / w

    # Heights above low point
    h1 = a * np.cosh(x1 / a)
    h2 = a * np.cosh(x2 / a)

    # Arc lengths from low point
    s1 = a * np.sinh(x1 / a)
    s2 = a * np.sinh(x2 / a)

    # Elongation
    elong = H * L / EA

    # Equations:
    # 1. Horizontal span constraint
    eq1 = (x2 - x1) - X

    # 2. Vertical span constraint
    eq2 = (h2 - h1) - Y

    # 3. Length constraint
    eq3 = (s2 - s1 + elong) - L

    return [eq1, eq2, eq3]

# Initial guess
# Assume low point is below anchor, roughly midway
x1_guess = 400  # m from low point to anchor
x2_guess = x1_guess + X  # m from low point to fairlead
H_guess = w * 400  # Initial H

initial_guess = [H_guess, x1_guess, x2_guess]

print("Solving general catenary BVP...")
print(f"Initial guess: H={H_guess:.0f} N, x1={x1_guess:.0f} m, x2={x2_guess:.0f} m")

try:
    solution = fsolve(catenary_system, initial_guess, full_output=True)
    vars_sol, info, ier, msg = solution

    if ier == 1:
        H_sol, x1_sol, x2_sol = vars_sol

        print(f"\n✓ Solution found:")
        print(f"  H = {H_sol:,.0f} N")
        print(f"  x1 = {x1_sol:.2f} m (anchor to low point)")
        print(f"  x2 = {x2_sol:.2f} m (fairlead to low point)")

        a_sol = H_sol / w
        print(f"  a = {a_sol:.2f} m")

        # Verify solution
        h1 = a_sol * np.cosh(x1_sol / a_sol)
        h2 = a_sol * np.cosh(x2_sol / a_sol)
        s1 = a_sol * np.sinh(x1_sol / a_sol)
        s2 = a_sol * np.sinh(x2_sol / a_sol)
        elong = H_sol * L / EA

        print(f"\nVerification:")
        print(f"  Horizontal span: {x2_sol - x1_sol:.2f} m (target: {X} m)")
        print(f"  Vertical span: {h2 - h1:.2f} m (target: {Y} m)")
        print(f"  Arc length: {s2 - s1:.2f} m")
        print(f"  Elongation: {elong:.4f} m")
        print(f"  Total length: {s2 - s1 + elong:.2f} m (target: {L} m)")

        # Tensions
        V_fairlead = H_sol * np.sinh(x2_sol / a_sol)
        T_fairlead = np.sqrt(H_sol**2 + V_fairlead**2)

        print(f"\nTensions:")
        print(f"  H (horizontal): {H_sol:,.0f} N")
        print(f"  V (fairlead): {V_fairlead:,.0f} N")
        print(f"  T (fairlead total): {T_fairlead:,.0f} N")

        print(f"\nComparison to Excel:")
        print(f"  H: {H_sol:,.0f} N vs 785,000 N ({abs(H_sol-785000)/785000*100:.1f}% error)")
        print(f"  V: {V_fairlead:,.0f} N vs 196,200 N ({abs(V_fairlead-196200)/196200*100:.1f}% error)")
        print(f"  T: {T_fairlead:,.0f} N vs 809,000 N ({abs(T_fairlead-809000)/809000*100:.1f}% error)")

    else:
        print(f"\n✗ Solution failed: {msg}")
        print(f"Final residuals: {info['fvec']}")

except Exception as e:
    print(f"\n✗ Error: {e}")
