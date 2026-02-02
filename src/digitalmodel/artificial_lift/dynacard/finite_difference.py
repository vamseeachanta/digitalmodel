# ABOUTME: Time-domain finite difference solver for sucker rod wave equation.
# ABOUTME: Provides comparison against Gibbs analytical solver for validation.

import numpy as np
from scipy.signal import savgol_filter
from typing import Optional, Tuple
from .models import (
    DynacardAnalysisContext, CardData, AnalysisResults,
    RodSection
)


class FiniteDifferenceSolver:
    """
    Time-domain finite difference solver for the sucker rod wave equation.
    Ported from legacy Oxy.Cipher.DynaCard implementation.

    Solves: rho * A * u_tt = E * A * u_xx - c * u_t + F_gravity + F_friction

    Uses explicit finite difference scheme marching from surface (x=0)
    to pump (x=L) at each time step.
    """

    def __init__(self, context: DynacardAnalysisContext):
        self.ctx = context
        self.results = AnalysisResults()

        # Solver parameters
        self.num_nodes: int = context.calc_params.num_finite_difference_nodes
        self.fluid_damping: Optional[Tuple[float, float]] = None  # (upstroke, downstroke)
        self.remove_jumps: bool = True
        self.include_gravity: bool = True
        self.savgol_window: int = 21
        self.savgol_polyorder: int = 5

    def solve(self) -> AnalysisResults:
        """
        Execute the finite difference solution.
        Returns downhole card position and load.
        """
        # Extract surface card data
        u_surf = np.array(self.ctx.surface_card.position)
        f_surf = np.array(self.ctx.surface_card.load)
        Nt = len(u_surf)

        # Rod string properties
        L_rod = self.ctx.rod_length * 12.0  # Convert feet to inches
        Nx = self.num_nodes

        # Build rod property arrays at each spatial node
        x = np.linspace(0, L_rod, Nx)
        dx = x[1] - x[0]

        # Time discretization
        T = 60.0 / self.ctx.spm  # Period in seconds
        t = np.linspace(0, T, Nt)
        dt = t[1] - t[0]

        # Build material property matrices (Nx,)
        E, A, rho, c = self._build_rod_properties(x)

        # Wave speed at each node
        a = np.sqrt(E / rho)

        # Buoyant weight calculation
        rho_fluid = self.ctx.fluid_density  # lbs/ft^3
        rho_steel = 490.0  # lbs/ft^3
        buoyancy_factor = 1.0 - rho_fluid / rho_steel

        # Calculate buoyant weight distribution
        dV = dx * A  # Volume per segment (in^3)
        dW = rho * dV / (12.0 ** 3)  # Weight per segment (lbs)
        buoyant_weight_total = np.sum(dW) * buoyancy_factor * 32.174  # in lbf

        # Initialize solution matrix u(x, t)
        u = np.zeros((Nx, Nt))

        # Boundary condition at x=0 (surface): u(0,t) = surface card position
        u[0, :] = -u_surf  # Negative because downward is positive

        # Initial condition at x=1: from load equilibrium
        # F = E*A/dx * (u[1] - u[0])
        # u[1] = u[0] + F*dx/(E*A)
        D = f_surf - buoyant_weight_total if self.include_gravity else f_surf
        u[1, :] = u[0, :] + D * dx / (E[0] * A[0])

        # Damping coefficients
        if self.fluid_damping is not None:
            c_up, c_down = self.fluid_damping
        else:
            c_up = c_down = 0.0

        # Find upstroke/downstroke transition
        up_dn_idx = int(np.mean(np.where(u_surf == np.min(u_surf))[0]))

        # March through space from x=1 to x=Nx-1
        g = 32.174 * 12.0  # gravity in in/s^2

        for i in range(1, Nx - 1):
            for j in range(1, Nt - 1):
                # Material properties at this node
                E_plus = (E[i + 1] * A[i + 1] + E[i] * A[i]) / 2.0
                E_mins = (E[i - 1] * A[i - 1] + E[i] * A[i]) / 2.0
                rhoA = rho[i] * A[i]

                # Damping (direction-dependent)
                c_val = c_up if j < up_dn_idx else c_down
                rhoAc = rhoA * c_val

                # Velocity sign for friction
                if (u[i, j + 1] - u[i, j]) > 0:
                    sign = 1
                elif (u[i, j + 1] - u[i, j]) < 0:
                    sign = -1
                else:
                    sign = 0

                # Finite difference coefficients
                C1 = (rhoA / E_plus) * (dx / dt) ** 2
                C2 = (rhoAc / E_plus) * (dx ** 2) / dt

                # Explicit update formula
                u[i + 1, j] = (
                    u[i, j]
                    + (E_mins / E_plus) * u[i, j]
                    - (E_mins / E_plus) * u[i - 1, j]
                    + C1 * (u[i, j + 1] - 2 * u[i, j] + u[i, j - 1])
                    + C2 * (u[i, j + 1] - u[i, j - 1]) / 2.0
                )

            # Handle ghost points (periodic boundary in time)
            u[i + 1, 0] = u[i + 1, 1]
            u[i + 1, Nt - 1] = u[i + 1, 0]

        # Extract downhole position (at x=L)
        U_dh = -u[Nx - 1, :]

        # Calculate downhole load using stress-strain relation
        # F = E*A/dx * (u[Nx-1] - u[Nx-2]) + buoyant weight
        F_dh = np.zeros(Nt)
        for j in range(Nt):
            F_dh[j] = (E[Nx - 1] * A[Nx - 1] / dx) * (u[Nx - 1, j] - u[Nx - 2, j])

        # Add buoyant weight distribution
        buoyant_at_pump = buoyant_weight_total
        F_dh = F_dh + buoyant_at_pump

        # Apply smoothing filter
        if self.savgol_window > 0 and Nt > self.savgol_window:
            F_dh = savgol_filter(F_dh, self.savgol_window, self.savgol_polyorder)

        # Ensure continuity (periodic)
        F_dh[Nt - 1] = F_dh[0]
        U_dh[Nt - 1] = U_dh[0]

        # Normalize position to start at 0
        U_dh = U_dh - np.min(U_dh)

        # Store results
        self.results.downhole_card = CardData(
            position=U_dh.tolist(),
            load=F_dh.tolist()
        )
        self.results.peak_polished_rod_load = float(np.max(f_surf))
        self.results.minimum_polished_rod_load = float(np.min(f_surf))
        self.results.ctx = self.ctx
        self.results.solver_method = "finite_difference"

        return self.results

    def _build_rod_properties(self, x: np.ndarray) -> Tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
        """
        Build material property arrays at each spatial node.
        Returns (E, A, rho, c) arrays of shape (Nx,).
        """
        Nx = len(x)
        E = np.zeros(Nx)
        A = np.zeros(Nx)
        rho = np.zeros(Nx)
        c = np.zeros(Nx)

        # Calculate taper boundaries
        taper_lengths = [s.length * 12.0 for s in self.ctx.rod_string]  # Convert to inches
        taper_cumsum = np.cumsum(taper_lengths)
        cuts = np.zeros(len(taper_cumsum) + 1)
        cuts[1:] = taper_cumsum

        # Assign properties by taper section
        for i, section in enumerate(self.ctx.rod_string):
            mask = (x >= cuts[i]) & (x <= cuts[i + 1])

            # Calculate area in sq inches
            r = section.diameter / 2.0
            area = np.pi * r ** 2

            E[mask] = section.modulus_of_elasticity
            A[mask] = area
            rho[mask] = section.density / (12.0 ** 3)  # Convert lbs/ft^3 to lbs/in^3
            c[mask] = section.damping_factor

        return E, A, rho, c

    def detect_buckling(self) -> bool:
        """Detect rod buckling from downhole card."""
        if self.results.downhole_card is None:
            return False
        loads = np.array(self.results.downhole_card.load)
        if np.min(loads) < -500:
            self.results.buckling_detected = True
        return self.results.buckling_detected


def solve_wave_equation_fd(context: DynacardAnalysisContext) -> AnalysisResults:
    """
    Utility function to run finite difference solver.
    """
    solver = FiniteDifferenceSolver(context)
    return solver.solve()
