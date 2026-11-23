import os
from pathlib import Path
import numpy as np
import matplotlib.pyplot as plt

def catenary(x, a):
    """
    Calculate the catenary curve y = a * cosh(x / a).

    Parameters:
        x (array-like): Horizontal positions.
        a (float): Shape constant.

    Returns:
        array-like: Vertical displacements.
    """
    return a * np.cosh(x / a)

# Parameters
a = 5  # Shape constant
x_min, x_max = -10, 10  # Range for x
num_points = 500  # Number of points for smooth curve

# Generate x values and compute y values
x = np.linspace(x_min, x_max, num_points)
y = catenary(x, a)

# Plot the catenary curve
plt.figure(figsize=(8, 6))
plt.plot(x, y, label=f'Catenary (a={a})', color='blue')
plt.title('Catenary Curve')
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
