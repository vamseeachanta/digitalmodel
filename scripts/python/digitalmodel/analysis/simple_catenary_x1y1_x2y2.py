import os
from pathlib import Path
import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import fsolve

def catenary(x, a, x1, y1):
    """
    Calculate the catenary curve y = a * cosh((x - x1) / a).

    Parameters:
        x (array-like): Horizontal positions.
        a (float): Shape constant.
        x1 (float): Horizontal position of the first endpoint.
        y1 (float): Vertical position of the first endpoint.

    Returns:
        array-like: Vertical displacements.
    """
    return a * np.cosh((x - x1) / a) + (y1 - a)

def solve_a(a, x1, y1, x2, y2):
    """
    Solve for the parameter 'a' given the endpoints of the catenary.

    Parameters:
        a (float): Shape constant to solve for.
        x1, x2 (float): Horizontal positions of the endpoints.
        y1, y2 (float): Vertical positions of the endpoints.

    Returns:
        float: Residual value for optimization.
    """
    return a * np.cosh((x2 - x1) / a) + (y1 - a) - y2

# Inputs
x1, y1 = -5, 10  # First endpoint
x2, y2 = 5, 10   # Second endpoint

# Initial guess for 'a'
a_guess = 10

# Solve for 'a' using fsolve
a_solution = fsolve(solve_a, a_guess, args=(x1, y1, x2, y2))[0]

# Generate x values and compute y values
x = np.linspace(x1, x2, 500)
y = catenary(x, a_solution, x1, y1)

# Plot the catenary curve
plt.figure(figsize=(8, 6))
plt.plot(x, y, label=f'Catenary (a={a_solution:.2f})', color='blue')
plt.scatter([x1, x2], [y1, y2], color='red', label='Endpoints (x1, y1) and (x2, y2)')
plt.title('Catenary Curve with Specified Endpoints')
plt.xlabel('x')
plt.ylabel('y')
plt.axhline(0, color='black', linewidth=0.8, linestyle='--')
plt.axvline(0, color='black', linewidth=0.8, linestyle='--')
plt.grid(color='gray', linestyle='--', linewidth=0.5)
plt.legend()
file_path = os.path.dirname(os.path.realpath(__file__))
filename = Path(__file__).stem + '.png'
filename_with_path = os.path.join(file_path, filename)
plt.savefig(filename_with_path)
