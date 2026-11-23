"""Check catenary geometry for Excel reference case."""

import numpy as np

# Inputs
X = 800  # Horizontal span [m]
Y = 100  # Vertical span [m]
L = 1000  # Length [m]
w = 1962  # Weight per length [N/m]
EA = 64e9  # Stiffness [N]

# Expected from Excel
H_excel = 785_000  # N

# Compute catenary parameter
a = H_excel / w
print(f"Catenary parameter a = H/w = {a:.2f} m")

# Check vertical span
y_calc = a * (np.cosh(X/a) - 1)
print(f"\nVertical span check:")
print(f"  Given Y:      {Y} m")
print(f"  Calculated y: {y_calc:.2f} m")
print(f"  Match:        {abs(y_calc - Y) < 1.0}")

# Check arc length
s_calc = a * np.sinh(X/a)
print(f"\nArc length check:")
print(f"  Calculated s: {s_calc:.2f} m")

# Check elongation
elong = H_excel * L / EA
print(f"  Elongation:   {elong:.2f} m")

# Check total length
total = s_calc + elong
print(f"  Total (s+e):  {total:.2f} m")
print(f"  Target L:     {L} m")
print(f"  Error:        {total - L:.2f} m")
print(f"  Match:        {abs(total - L) < 1.0}")

# Vertical tension
V = H_excel * np.sinh(X/a)
print(f"\nVertical tension:")
print(f"  V = H*sinh(X/a) = {V:,.0f} N")
print(f"  Excel V:        196,200 N")

# Total tension
T = np.sqrt(H_excel**2 + V**2)
print(f"\nTotal tension:")
print(f"  T = sqrt(H² + V²) = {T:,.0f} N")
print(f"  Excel T:            809,000 N")
