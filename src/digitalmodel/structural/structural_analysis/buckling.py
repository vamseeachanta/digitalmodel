"""
Buckling Analysis

Implements:
- Plate buckling per DNV-RP-C201
- Column buckling per Eurocode 3
- Johnson-Ostenfeld inelastic correction
"""

import numpy as np
from .models import MaterialProperties, PlateGeometry, BucklingResult


class PlateBucklingAnalyzer:
    """
    Plate buckling analysis per DNV-RP-C201.
    """

    def __init__(self, material: MaterialProperties):
        self.material = material
        self.E = material.youngs_modulus
        self.nu = material.poissons_ratio
        self.fy = material.yield_strength

    def elastic_buckling_stress(
        self,
        plate: PlateGeometry,
        boundary_conditions: str = "simply_supported"
    ) -> float:
        """
        Calculate elastic buckling stress.

        Args:
            plate: Plate geometry
            boundary_conditions: Boundary condition type

        Returns:
            Elastic buckling stress (MPa)
        """
        a = plate.length
        b = plate.width
        t = plate.thickness

        # Aspect ratio
        alpha = a / b

        # Buckling coefficient (simply supported, uniform compression)
        if alpha < 1:
            k = (alpha + 1/alpha)**2
        else:
            k = 4.0

        # Elastic buckling stress
        sigma_e = k * np.pi**2 * self.E / (12 * (1 - self.nu**2)) * (t / b)**2

        return sigma_e

    def reduced_slenderness(
        self,
        plate: PlateGeometry
    ) -> float:
        """
        Calculate reduced slenderness parameter.

        Args:
            plate: Plate geometry

        Returns:
            Reduced slenderness (lambda_p)
        """
        sigma_e = self.elastic_buckling_stress(plate)
        return np.sqrt(self.fy / sigma_e)

    def johnson_ostenfeld(
        self,
        sigma_e: float
    ) -> float:
        """
        Apply Johnson-Ostenfeld correction for inelastic buckling.

        Args:
            sigma_e: Elastic buckling stress

        Returns:
            Critical buckling stress
        """
        if sigma_e <= 0.5 * self.fy:
            return sigma_e
        else:
            return self.fy * (1 - self.fy / (4 * sigma_e))

    def check_plate_buckling(
        self,
        plate: PlateGeometry,
        sigma_x: float,
        sigma_y: float = 0.0,
        tau: float = 0.0,
        gamma_m: float = 1.15
    ) -> BucklingResult:
        """
        Check plate buckling under combined loading.

        Args:
            plate: Plate geometry
            sigma_x: Compressive stress in x (MPa, positive = compression)
            sigma_y: Compressive stress in y (MPa)
            tau: Shear stress (MPa)
            gamma_m: Material factor

        Returns:
            BucklingResult with utilization
        """
        # Calculate individual buckling stresses
        b = plate.width
        t = plate.thickness

        # Compressive buckling
        sigma_e_x = self.elastic_buckling_stress(plate)
        sigma_cr_x = self.johnson_ostenfeld(sigma_e_x)

        # Shear buckling
        k_tau = 5.34 + 4 * (b / plate.length)**2
        tau_e = k_tau * np.pi**2 * self.E / (12 * (1 - self.nu**2)) * (t / b)**2
        tau_cr = self.johnson_ostenfeld(tau_e)

        # Combined check (interaction formula)
        util_x = sigma_x / (sigma_cr_x / gamma_m) if sigma_cr_x > 0 else 0
        util_tau = (tau / (tau_cr / gamma_m))**2 if tau_cr > 0 else 0

        total_util = util_x + util_tau

        return BucklingResult(
            critical_stress=sigma_cr_x,
            applied_stress=sigma_x,
            utilization=total_util,
            safety_factor=1 / total_util if total_util > 0 else float('inf'),
            mode="plate_buckling",
            passes=total_util <= 1.0
        )


class ColumnBucklingAnalyzer:
    """
    Column buckling analysis per Eurocode 3.
    """

    def __init__(self, material: MaterialProperties):
        self.material = material
        self.E = material.youngs_modulus
        self.fy = material.yield_strength

    def euler_buckling_load(
        self,
        I: float,
        L_eff: float
    ) -> float:
        """
        Calculate Euler critical buckling load.

        Args:
            I: Moment of inertia (mm⁴)
            L_eff: Effective length (mm)

        Returns:
            Critical load (N)
        """
        return np.pi**2 * self.E * I / L_eff**2

    def slenderness_ratio(
        self,
        L_eff: float,
        r: float
    ) -> float:
        """
        Calculate slenderness ratio.

        Args:
            L_eff: Effective length (mm)
            r: Radius of gyration (mm)

        Returns:
            Slenderness ratio
        """
        return L_eff / r

    def reduction_factor(
        self,
        lambda_bar: float,
        buckling_curve: str = "b"
    ) -> float:
        """
        Calculate buckling reduction factor per EC3.

        Args:
            lambda_bar: Non-dimensional slenderness
            buckling_curve: EC3 buckling curve (a0, a, b, c, d)

        Returns:
            Reduction factor chi
        """
        # Imperfection factors
        alpha_dict = {
            "a0": 0.13,
            "a": 0.21,
            "b": 0.34,
            "c": 0.49,
            "d": 0.76
        }
        alpha = alpha_dict.get(buckling_curve, 0.34)

        # Calculate reduction factor
        phi = 0.5 * (1 + alpha * (lambda_bar - 0.2) + lambda_bar**2)

        # Avoid taking sqrt of negative number
        if phi**2 < lambda_bar**2:
            return 0.0

        chi = 1 / (phi + np.sqrt(phi**2 - lambda_bar**2))

        return min(chi, 1.0)

    def check_column_buckling(
        self,
        axial_force: float,
        area: float,
        I_min: float,
        L_eff: float,
        buckling_curve: str = "b",
        gamma_m: float = 1.0
    ) -> BucklingResult:
        """
        Check column buckling capacity.

        Args:
            axial_force: Applied axial force (N)
            area: Cross-sectional area (mm²)
            I_min: Minimum moment of inertia (mm⁴)
            L_eff: Effective length (mm)
            buckling_curve: EC3 curve
            gamma_m: Material factor

        Returns:
            BucklingResult
        """
        # Calculate slenderness
        r = np.sqrt(I_min / area)
        lambda_1 = np.pi * np.sqrt(self.E / self.fy)
        lambda_bar = (L_eff / r) / lambda_1

        # Get reduction factor
        chi = self.reduction_factor(lambda_bar, buckling_curve)

        # Design capacity
        N_cr = chi * area * self.fy / gamma_m

        # Utilization
        util = axial_force / N_cr if N_cr > 0 else float('inf')

        return BucklingResult(
            critical_stress=chi * self.fy / gamma_m,
            applied_stress=axial_force / area,
            utilization=util,
            safety_factor=1 / util if util > 0 else float('inf'),
            mode="column_buckling",
            passes=util <= 1.0
        )
