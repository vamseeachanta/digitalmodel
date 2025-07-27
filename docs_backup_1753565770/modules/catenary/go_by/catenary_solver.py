## #TODO Resolve length. Cannot understand function to resolve 2025-05-03

import os
import numpy as np
from scipy.optimize import minimize
from scipy.integrate import quad

def catenary_curve(x, a, x0, y0):
    """Catenary curve equation: y = a * cosh((x - x0)/a) + y0"""
    return a * np.cosh((x - x0) / a) + y0

def catenary_derivative(x, a, x0, y0):
    """Derivative of catenary curve (slope at point x)"""
    return np.sinh((x - x0) / a)

def arc_length(x1, x2, a, x0, y0):
    """Calculate the arc length of catenary between x1 and x2"""
    def integrand(x):
        return np.sqrt(1 + np.sinh((x - x0) / a)**2)
    
    return a * quad(integrand, x1, x2)[0]

def objective_function(params, x1, y1, x2, y2, slope1, slope2, target_length):
    """
    Objective function to minimize.
    params = [a, x0, y0]
    """
    a, x0, y0 = params
    
    # Check if parameters are in valid range
    if a <= 0:
        return 1e10  # Large penalty for invalid a
    
    # Calculate predicted y values and slopes at endpoints
    y1_pred = catenary_curve(x1, a, x0, y0)
    y2_pred = catenary_curve(x2, a, x0, y0)
    slope1_pred = catenary_derivative(x1, a, x0, y0)
    slope2_pred = catenary_derivative(x2, a, x0, y0)
    
    # Calculate length
    length_pred = arc_length(x1, x2, a, x0, y0)
    
    # Penalties for deviations from constraints
    y1_penalty = (y1_pred - y1)**2
    y2_penalty = (y2_pred - y2)**2
    slope1_penalty = (slope1_pred - slope1)**2
    slope2_penalty = (slope2_pred - slope2)**2
    length_penalty = (length_pred - target_length)**2
    
    return y1_penalty + y2_penalty + slope1_penalty + slope2_penalty + length_penalty

def solve_catenary(x1, y1, x2, y2, slope1, slope2, target_length, initial_guess=None):
    """
    Solve for catenary parameters given endpoints, slopes, and target length
    
    Parameters:
    - x1, y1: coordinates of first endpoint
    - x2, y2: coordinates of second endpoint
    - slope1: slope at first endpoint
    - slope2: slope at second endpoint
    - target_length: desired arc length
    - initial_guess: initial values for [a, x0, y0]
    
    Returns:
    - a, x0, y0: parameters of the catenary
    """
    if initial_guess is None:
        # Estimate initial parameter values
        dx = x2 - x1
        dy = y2 - y1
        direct_distance = np.sqrt(dx**2 + dy**2)
        
        # Initial guess for parameter a (based on sag estimate)
        a_guess = max(1.0, direct_distance / 5)
        
        # Midpoint as initial guess for x0
        x0_guess = (x1 + x2) / 2
        
        # Estimate y0 based on midpoint
        y0_guess = min(y1, y2) - a_guess
        
        initial_guess = [a_guess, x0_guess, y0_guess]
    
    # Perform optimization to find parameters
    result = minimize(
        objective_function,
        initial_guess,
        args=(x1, y1, x2, y2, slope1, slope2, target_length),
        method='Nelder-Mead',
        options={'maxiter': 10000}
    )
    
    return result.x

def plot_catenary(a, x0, y0, x1, x2, num_points=100):
    """Generate points along the catenary for plotting"""
    x = np.linspace(x1, x2, num_points)
    y = catenary_curve(x, a, x0, y0)
    return x, y

# Example usage:
if __name__ == "__main__":
    # Define constraints
    x1, y1 = 0, 10       # First endpoint
    x2, y2 = 30, 10      # Second endpoint
    slope1 = -7        # Slope at first endpoint (negative means sloping down to the right)
    slope2 = 7         # Slope at second endpoint (positive means sloping up to the right)
    target_length = 35   # Desired arc length
    
    # Solve for catenary parameters
    a, x0, y0 = solve_catenary(x1, y1, x2, y2, slope1, slope2, target_length)
    
    print(f"Catenary parameters: a={a:.4f}, x0={x0:.4f}, y0={y0:.4f}")
    
    # Generate points for plotting
    x, y = plot_catenary(a, x0, y0, x1, x2)
    
    # Verify the solution
    computed_length = arc_length(x1, x2, a, x0, y0)
    print(f"Target length: {target_length}")
    print(f"Computed length: {computed_length:.4f}")
    
    # To plot (requires matplotlib)
    import matplotlib.pyplot as plt
    plt.figure(figsize=(10, 6))
    plt.plot(x, y, 'b-', linewidth=2)
    plt.plot([x1, x2], [y1, y2], 'ro', markersize=8)
    
    # Plot tangent lines at endpoints to visualize slopes
    def plot_tangent(x, y, slope, length=5):
        plt.plot([x-length, x+length], [y-slope*length, y+slope*length], 'g--')
    
    plot_tangent(x1, y1, slope1)
    plot_tangent(x2, y2, slope2)
    
    plt.grid(True)
    plt.axis('equal')
    plt.title('Catenary Curve Solution')
    plt.xlabel('x')
    plt.ylabel('y')
    filename = os.path.basename(__file__) + '.png'
    plt.savefig(filename)
