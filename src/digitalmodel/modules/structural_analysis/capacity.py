"""
Capacity Verification

Member capacity checks for combined loading including:
- Tension capacity
- Combined axial and bending
- Interaction formulas per Eurocode 3
"""

from typing import Dict
from .models import MaterialProperties, CapacityResult
from .stress_calculator import StressCalculator
from .buckling import PlateBucklingAnalyzer, ColumnBucklingAnalyzer


class MemberCapacityChecker:
    """
    Check member capacity for combined loading.
    """

    def __init__(self, material: MaterialProperties):
        self.material = material
        self.stress_calc = StressCalculator(material)
        self.plate_buckling = PlateBucklingAnalyzer(material)
        self.column_buckling = ColumnBucklingAnalyzer(material)

    def check_tension_member(
        self,
        axial_force: float,
        area_gross: float,
        area_net: float,
        gamma_m0: float = 1.0,
        gamma_m2: float = 1.25
    ) -> CapacityResult:
        """
        Check tension capacity per EC3.

        Args:
            axial_force: Applied tension (N)
            area_gross: Gross area (mm²)
            area_net: Net area at connections (mm²)
            gamma_m0: Material factor (yield)
            gamma_m2: Material factor (ultimate)

        Returns:
            CapacityResult
        """
        # Plastic capacity
        N_pl = area_gross * self.material.yield_strength / gamma_m0

        # Ultimate capacity at net section
        N_u = 0.9 * area_net * self.material.ultimate_strength / gamma_m2

        # Governing capacity
        N_Rd = min(N_pl, N_u)
        governing = "plastic" if N_pl <= N_u else "net_section"

        util = axial_force / N_Rd if N_Rd > 0 else float('inf')

        return CapacityResult(
            capacity=N_Rd,
            demand=axial_force,
            utilization=util,
            governing_mode=governing,
            passes=util <= 1.0,
            details={'N_pl': N_pl, 'N_u': N_u}
        )

    def check_combined_loading(
        self,
        N: float,
        M_y: float,
        M_z: float,
        area: float,
        W_pl_y: float,
        W_pl_z: float,
        N_cr_y: float,
        N_cr_z: float,
        gamma_m1: float = 1.0
    ) -> CapacityResult:
        """
        Check member under combined axial and bending.

        Args:
            N: Axial force (N, positive = compression)
            M_y: Moment about y-axis (N·mm)
            M_z: Moment about z-axis (N·mm)
            area: Cross-sectional area (mm²)
            W_pl_y: Plastic section modulus y (mm³)
            W_pl_z: Plastic section modulus z (mm³)
            N_cr_y: Critical buckling load y (N)
            N_cr_z: Critical buckling load z (N)
            gamma_m1: Material factor

        Returns:
            CapacityResult
        """
        fy = self.material.yield_strength

        # Capacities
        N_Rk = area * fy
        M_y_Rk = W_pl_y * fy
        M_z_Rk = W_pl_z * fy

        # Reduction factors
        chi_y = min(N_cr_y / N_Rk, 1.0) if N_Rk > 0 else 1.0
        chi_z = min(N_cr_z / N_Rk, 1.0) if N_Rk > 0 else 1.0
        chi = min(chi_y, chi_z, 1.0)

        # Interaction check (simplified)
        util_N = N / (chi * N_Rk / gamma_m1) if N > 0 else 0
        util_My = M_y / (M_y_Rk / gamma_m1) if M_y_Rk > 0 else 0
        util_Mz = M_z / (M_z_Rk / gamma_m1) if M_z_Rk > 0 else 0

        # Combined utilization (simplified linear)
        util_combined = util_N + util_My + util_Mz

        return CapacityResult(
            capacity=chi * N_Rk / gamma_m1,
            demand=N,
            utilization=util_combined,
            governing_mode="combined",
            passes=util_combined <= 1.0,
            details={
                'util_N': util_N,
                'util_My': util_My,
                'util_Mz': util_Mz,
                'chi': chi
            }
        )
