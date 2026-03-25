"""
Mathematical formulations for passing ship force calculations.

Implements Wang's methodology for calculating hydrodynamic interaction forces
between passing vessels, including:
- Sectional area functions (S1, S2)
- Kernel functions (F, G)
- Force calculations (surge, sway, yaw)
- Finite depth corrections
"""

import numpy as np
from scipy import integrate
from typing import Tuple, Callable, Optional


# Sectional Area Functions
def s1_function(x: float, L: float) -> float:
    """
    Calculate S1 sectional area function.
    
    S1(x) represents the longitudinal distribution of sectional area
    along the vessel length, normalized to unity at midship.
    
    Args:
        x: Longitudinal position from midship (-L/2 to L/2) [m]
        L: Vessel length [m]
    
    Returns:
        S1 value (0 to 1)
    """
    if abs(x) > L/2:
        return 0.0
    
    # Parabolic distribution: S1 = 1 - (2x/L)^2
    return 1.0 - (2.0 * x / L) ** 2


def s2_function(x: float, L: float) -> float:
    """
    Calculate S2 sectional area function.
    
    S2(x) represents a modified sectional area distribution
    for yaw moment calculations.
    
    Args:
        x: Longitudinal position from midship (-L/2 to L/2) [m]
        L: Vessel length [m]
    
    Returns:
        S2 value
    """
    if abs(x) > L/2:
        return 0.0
    
    # Modified parabolic distribution
    s1 = s1_function(x, L)
    return s1 * (2.0 * x / L)


def ds1_dx(x: float, L: float) -> float:
    """
    Calculate derivative of S1 with respect to x.
    
    Args:
        x: Longitudinal position from midship [m]
        L: Vessel length [m]
    
    Returns:
        dS1/dx value
    """
    if abs(x) > L/2:
        return 0.0
    
    # d/dx[1 - (2x/L)^2] = -8x/L^2
    return -8.0 * x / (L ** 2)


def ds2_dx(x: float, L: float) -> float:
    """
    Calculate derivative of S2 with respect to x.
    
    Args:
        x: Longitudinal position from midship [m]
        L: Vessel length [m]
    
    Returns:
        dS2/dx value
    """
    if abs(x) > L/2:
        return 0.0
    
    # Product rule: d/dx[S1 * 2x/L]
    s1 = s1_function(x, L)
    ds1 = ds1_dx(x, L)
    
    return ds1 * (2.0 * x / L) + s1 * (2.0 / L)


# Kernel Functions for Force Calculations
def f_kernel(xi: float, eta: float, x: float, y: float, L1: float, L2: float = None) -> float:
    """
    Calculate F kernel function for sway/yaw force integration.

    Uses sectional area derivatives dS1/dx for both vessels with 3D
    slender-body Green function (y/r^3).  Wang (1975) formulation.

    Args:
        xi: Position on passing vessel (vessel 2) [m]
        eta: Position on moored vessel (vessel 1) [m]
        x: Stagger distance between vessels [m]
        y: Lateral separation between vessels [m]
        L1: Moored vessel length [m]
        L2: Passing vessel length [m] (defaults to L1 if not given)

    Returns:
        F kernel value
    """
    if L2 is None:
        L2 = L1

    dx = (x + xi) - eta
    r_sq = dx**2 + y**2

    if r_sq < 1e-12:
        return 0.0

    r = np.sqrt(r_sq)
    ds1_xi = ds1_dx(xi, L2)   # passing vessel derivative
    ds1_eta = ds1_dx(eta, L1)  # moored vessel derivative

    return ds1_xi * ds1_eta * y / (r**3)


def g_kernel(xi: float, eta: float, x: float, y: float, L1: float, L2: float = None) -> float:
    """
    Calculate G kernel function for surge force integration.

    Uses derivatives dS1/dx for BOTH vessels, which ensures the surge
    integral is antisymmetric in stagger (zero at abeam, sign-reversal).
    Wang (1975) formulation with 3D slender-body Green function (dx/r^3).

    Args:
        xi: Position on passing vessel (vessel 2) [m]
        eta: Position on moored vessel (vessel 1) [m]
        x: Stagger distance between vessels [m]
        y: Lateral separation between vessels [m]
        L1: Moored vessel length [m]
        L2: Passing vessel length [m] (defaults to L1 if not given)

    Returns:
        G kernel value
    """
    if L2 is None:
        L2 = L1

    dx = (x + xi) - eta
    r_sq = dx**2 + y**2

    if r_sq < 1e-12:
        return 0.0

    r = np.sqrt(r_sq)
    ds1_xi = ds1_dx(xi, L2)   # passing vessel derivative
    ds1_eta = ds1_dx(eta, L1)  # moored vessel derivative

    return ds1_xi * ds1_eta * dx / (r**3)


# Force Calculations - Infinite Depth
def calculate_surge_force_infinite(
    L: float = None, B: float = None, T: float = None, Cb: float = None,
    U: float = 0.0, y: float = 1.0, x: float = 0.0, rho: float = 1025.0,
    *,
    L1: float = None, A1: float = None,
    L2: float = None, A2: float = None,
) -> float:
    """
    Calculate surge force on moored vessel (infinite depth).

    Accepts either legacy single-vessel params (L, B, T, Cb) for backward
    compatibility, or separate two-vessel params (L1, A1, L2, A2) for the
    correct Wang formulation.

    Args:
        L, B, T, Cb: Legacy single-vessel parameters (used when L1/L2 not given)
        U: Passing vessel velocity [m/s]
        y: Lateral separation [m]
        x: Stagger distance [m]
        rho: Water density [kg/m³]
        L1: Moored vessel length [m]
        A1: Moored vessel midship area [m²]
        L2: Passing vessel length [m]
        A2: Passing vessel midship area [m²]

    Returns:
        Surge force [N]
    """
    if L1 is None:
        L1 = L
        L2 = L if L2 is None else L2
        A1 = B * T * Cb if A1 is None else A1
        A2 = A1 if A2 is None else A2
    else:
        if L2 is None:
            L2 = L1
        if A2 is None:
            A2 = A1

    def integrand(eta, xi):
        return g_kernel(xi, eta, x, y, L1, L2)

    try:
        result, error = integrate.dblquad(
            integrand,
            -L2 / 2, L2 / 2,   # outer: xi over passing vessel
            -L1 / 2, L1 / 2,   # inner: eta over moored vessel
            epsabs=1e-4,
            epsrel=1e-4,
        )
    except Exception:
        return 0.0

    force = (2.0 / 3.0) * rho * U**2 * A1 * A2 * result
    return force


def calculate_sway_force_infinite(
    L: float = None, B: float = None, T: float = None, Cb: float = None,
    U: float = 0.0, y: float = 1.0, x: float = 0.0, rho: float = 1025.0,
    *,
    L1: float = None, A1: float = None,
    L2: float = None, A2: float = None,
) -> float:
    """
    Calculate sway force on moored vessel (infinite depth).

    Args:
        L, B, T, Cb: Legacy single-vessel parameters
        U: Passing vessel velocity [m/s]
        y: Lateral separation [m]
        x: Stagger distance [m]
        rho: Water density [kg/m³]
        L1: Moored vessel length [m]
        A1: Moored vessel midship area [m²]
        L2: Passing vessel length [m]
        A2: Passing vessel midship area [m²]

    Returns:
        Sway force [N]
    """
    if L1 is None:
        L1 = L
        L2 = L if L2 is None else L2
        A1 = B * T * Cb if A1 is None else A1
        A2 = A1 if A2 is None else A2
    else:
        if L2 is None:
            L2 = L1
        if A2 is None:
            A2 = A1

    def integrand(eta, xi):
        return f_kernel(xi, eta, x, y, L1, L2)

    try:
        result, error = integrate.dblquad(
            integrand,
            -L2 / 2, L2 / 2,
            -L1 / 2, L1 / 2,
            epsabs=1e-4,
            epsrel=1e-4,
        )
    except Exception:
        return 0.0

    force = (2.0 / 3.0) * rho * U**2 * A1 * A2 * result
    return force


def calculate_yaw_moment_infinite(
    L: float = None, B: float = None, T: float = None, Cb: float = None,
    U: float = 0.0, y: float = 1.0, x: float = 0.0, rho: float = 1025.0,
    *,
    L1: float = None, A1: float = None,
    L2: float = None, A2: float = None,
) -> float:
    """
    Calculate yaw moment on moored vessel (infinite depth).

    Args:
        L, B, T, Cb: Legacy single-vessel parameters
        U: Passing vessel velocity [m/s]
        y: Lateral separation [m]
        x: Stagger distance [m]
        rho: Water density [kg/m³]
        L1: Moored vessel length [m]
        A1: Moored vessel midship area [m²]
        L2: Passing vessel length [m]
        A2: Passing vessel midship area [m²]

    Returns:
        Yaw moment [N·m]
    """
    if L1 is None:
        L1 = L
        L2 = L if L2 is None else L2
        A1 = B * T * Cb if A1 is None else A1
        A2 = A1 if A2 is None else A2
    else:
        if L2 is None:
            L2 = L1
        if A2 is None:
            A2 = A1

    def integrand(eta, xi):
        return eta * f_kernel(xi, eta, x, y, L1, L2)

    try:
        result, error = integrate.dblquad(
            integrand,
            -L2 / 2, L2 / 2,
            -L1 / 2, L1 / 2,
            epsabs=1e-4,
            epsrel=1e-4,
        )
    except Exception:
        return 0.0

    moment = (2.0 / 3.0) * rho * U**2 * A1 * A2 * result
    return moment


# Finite Depth Corrections
def finite_depth_correction(
    h: float, T: float, L: float, y: float, mode: str = 'surge'
) -> float:
    """
    Calculate finite water depth correction factor.
    
    Args:
        h: Water depth [m]
        T: Vessel draft [m]
        L: Vessel length [m]
        y: Lateral separation [m]
        mode: Force mode ('surge', 'sway', or 'yaw')
    
    Returns:
        Correction factor (multiply infinite depth force by this)
    """
    # Depth ratio
    h_T = h / T
    
    # Deep water limit (no correction needed)
    if h_T > 5.0:
        return 1.0
    
    # Froude number based on depth
    Fr_h = 1.0 / np.sqrt(h / L)
    
    # Harmonic summation for finite depth
    # Simplified implementation - actual formulation from Wang's paper needed
    n_terms = 10  # Number of harmonic terms
    correction = 1.0
    
    for n in range(1, n_terms + 1):
        k_n = n * np.pi / h  # Wave number
        
        # Depth function
        depth_func = np.cosh(k_n * (h - T)) / np.cosh(k_n * h)
        
        # Lateral decay
        lateral_decay = np.exp(-k_n * y)
        
        # Mode-specific coefficient
        if mode == 'surge':
            mode_coeff = 1.0 / n
        elif mode == 'sway':
            mode_coeff = 1.0 / n**2
        else:  # yaw
            mode_coeff = 1.0 / n**3
        
        # Add harmonic contribution
        correction += mode_coeff * depth_func * lateral_decay
    
    # Shallow water amplification
    shallow_factor = 1.0 + 0.5 * (1.0 - h_T / 1.5) if h_T < 1.5 else 1.0
    correction *= shallow_factor
    
    return correction


def calculate_forces_with_depth(
    vessel_params: dict,
    passing_params: dict,
    water_depth: Optional[float] = None,
    rho: float = 1025.0
) -> Tuple[float, float, float]:
    """
    Calculate all forces with optional finite depth correction.
    
    Args:
        vessel_params: Dictionary with L, B, T, Cb
        passing_params: Dictionary with U, y, x
        water_depth: Water depth [m], None for infinite depth
        rho: Water density [kg/m³]
    
    Returns:
        Tuple of (surge_force, sway_force, yaw_moment)
    """
    # Calculate infinite depth forces
    surge = calculate_surge_force_infinite(**vessel_params, **passing_params, rho=rho)
    sway = calculate_sway_force_infinite(**vessel_params, **passing_params, rho=rho)
    yaw = calculate_yaw_moment_infinite(**vessel_params, **passing_params, rho=rho)
    
    # Apply finite depth corrections if needed
    if water_depth is not None:
        surge_corr = finite_depth_correction(
            h=water_depth,
            T=vessel_params['T'],
            L=vessel_params['L'],
            y=passing_params['y'],
            mode='surge'
        )
        sway_corr = finite_depth_correction(
            h=water_depth,
            T=vessel_params['T'],
            L=vessel_params['L'],
            y=passing_params['y'],
            mode='sway'
        )
        yaw_corr = finite_depth_correction(
            h=water_depth,
            T=vessel_params['T'],
            L=vessel_params['L'],
            y=passing_params['y'],
            mode='yaw'
        )
        
        surge *= surge_corr
        sway *= sway_corr
        yaw *= yaw_corr
    
    return surge, sway, yaw