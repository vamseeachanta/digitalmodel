"""
Plate Buckling Calculations Module

This module provides detailed plate buckling calculations based on DNV standards.
It consolidates and modernizes the calculations from the original plateBucklingCal_*.py files
with improved error handling, type safety, and documentation.

Author: Migrated and modernized from original work by Sai Venkatesh
Date: 2025-01-15
Standards: DNV-RP-C201, API standards
"""

from typing import Dict, List, Optional, Tuple, Union, NamedTuple
from dataclasses import dataclass
import math
import logging
from enum import Enum

logger = logging.getLogger(__name__)


class PlateEdgeCondition(Enum):
    """Enumeration for plate edge boundary conditions."""
    SIMPLY_SUPPORTED = "simply_supported"
    CLAMPED = "clamped"
    FREE = "free"
    ELASTIC_RESTRAINT = "elastic_restraint"


class StressDirection(Enum):
    """Enumeration for stress directions."""
    LONGITUDINAL = "longitudinal"  # x-direction
    TRANSVERSE = "transverse"      # y-direction
    SHEAR = "shear"               # xy-direction


@dataclass
class BucklingCoefficients:
    """Data class to store buckling coefficients for different boundary conditions."""

    # Simply supported edge coefficients
    k_xx_simple: float = 4.0     # Longitudinal compression
    k_yy_simple: float = 1.0     # Transverse compression
    k_shear_simple: float = 5.34 # Shear

    # Clamped edge coefficients
    k_xx_clamped: float = 7.0    # Longitudinal compression
    k_yy_clamped: float = 1.0    # Transverse compression
    k_shear_clamped: float = 9.0 # Shear

    # Aspect ratio dependent coefficients
    def get_transverse_coefficient(self, aspect_ratio: float, edge_condition: PlateEdgeCondition) -> float:
        """
        Calculate transverse buckling coefficient based on aspect ratio.

        Args:
            aspect_ratio: Ratio of plate length to breadth (l/b)
            edge_condition: Edge boundary condition

        Returns:
            Transverse buckling coefficient
        """
        if edge_condition == PlateEdgeCondition.SIMPLY_SUPPORTED:
            return (1 + aspect_ratio**2)**2
        elif edge_condition == PlateEdgeCondition.CLAMPED:
            return 1 + 2.5 * aspect_ratio**2 + 5 * aspect_ratio**4
        else:
            return self.k_yy_simple

    def get_shear_coefficient(self, aspect_ratio: float, edge_condition: PlateEdgeCondition) -> float:
        """
        Calculate shear buckling coefficient based on aspect ratio.

        Args:
            aspect_ratio: Ratio of plate breadth to length (b/l)
            edge_condition: Edge boundary condition

        Returns:
            Shear buckling coefficient
        """
        if edge_condition == PlateEdgeCondition.SIMPLY_SUPPORTED:
            return 5.34 + 4 * aspect_ratio**2
        elif edge_condition == PlateEdgeCondition.CLAMPED:
            return 9 + 5.6 * aspect_ratio**2
        else:
            return self.k_shear_simple


class ElasticBucklingCalculator:
    """
    Calculator for elastic buckling stresses of plates under various loading conditions.

    This class implements elastic buckling calculations based on classical plate theory
    and DNV standards for different boundary conditions and loading types.
    """

    def __init__(self):
        """Initialize the elastic buckling calculator."""
        self.coefficients = BucklingCoefficients()

    def calculate_base_factor(self, youngs_modulus: float, poisson_ratio: float,
                            thickness: float, breadth: float) -> float:
        """
        Calculate the base factor for elastic buckling calculations.

        Args:
            youngs_modulus: Young's modulus of elasticity
            poisson_ratio: Poisson's ratio
            thickness: Plate thickness
            breadth: Plate breadth

        Returns:
            Base factor for buckling calculations
        """
        if youngs_modulus <= 0:
            raise ValueError("Young's modulus must be positive")
        if not 0 < poisson_ratio < 0.5:
            raise ValueError("Poisson's ratio must be between 0 and 0.5")
        if thickness <= 0 or breadth <= 0:
            raise ValueError("Thickness and breadth must be positive")

        base_factor = (math.pi**2 * youngs_modulus) / (12 * (1 - poisson_ratio**2))
        thickness_ratio_squared = (thickness / breadth)**2

        return base_factor * thickness_ratio_squared

    def calculate_longitudinal_buckling_stress(self,
                                             youngs_modulus: float,
                                             poisson_ratio: float,
                                             thickness: float,
                                             breadth: float,
                                             length: float,
                                             edge_condition: PlateEdgeCondition = PlateEdgeCondition.SIMPLY_SUPPORTED) -> float:
        """
        Calculate elastic buckling stress for longitudinal compression.

        Args:
            youngs_modulus: Young's modulus
            poisson_ratio: Poisson's ratio
            thickness: Plate thickness
            breadth: Plate breadth
            length: Plate length
            edge_condition: Edge boundary condition

        Returns:
            Longitudinal elastic buckling stress
        """
        base_factor = self.calculate_base_factor(youngs_modulus, poisson_ratio, thickness, breadth)

        if edge_condition == PlateEdgeCondition.SIMPLY_SUPPORTED:
            k_xx = self.coefficients.k_xx_simple
        elif edge_condition == PlateEdgeCondition.CLAMPED:
            k_xx = self.coefficients.k_xx_clamped
        else:
            k_xx = self.coefficients.k_xx_simple  # Default

        sigma_cr_xx = base_factor * k_xx

        logger.debug(f"Longitudinal buckling stress: {sigma_cr_xx:.2e}")
        return sigma_cr_xx

    def calculate_transverse_buckling_stress(self,
                                           youngs_modulus: float,
                                           poisson_ratio: float,
                                           thickness: float,
                                           breadth: float,
                                           length: float,
                                           edge_condition: PlateEdgeCondition = PlateEdgeCondition.SIMPLY_SUPPORTED) -> float:
        """
        Calculate elastic buckling stress for transverse compression.

        Args:
            youngs_modulus: Young's modulus
            poisson_ratio: Poisson's ratio
            thickness: Plate thickness
            breadth: Plate breadth
            length: Plate length
            edge_condition: Edge boundary condition

        Returns:
            Transverse elastic buckling stress
        """
        base_factor = self.calculate_base_factor(youngs_modulus, poisson_ratio, thickness, breadth)
        aspect_ratio = breadth / length  # b/l for transverse loading

        k_yy = self.coefficients.get_transverse_coefficient(aspect_ratio, edge_condition)
        sigma_cr_yy = base_factor * k_yy

        logger.debug(f"Transverse buckling stress: {sigma_cr_yy:.2e}")
        return sigma_cr_yy

    def calculate_shear_buckling_stress(self,
                                      youngs_modulus: float,
                                      poisson_ratio: float,
                                      thickness: float,
                                      breadth: float,
                                      length: float,
                                      edge_condition: PlateEdgeCondition = PlateEdgeCondition.SIMPLY_SUPPORTED) -> float:
        """
        Calculate elastic buckling stress for shear loading.

        Args:
            youngs_modulus: Young's modulus
            poisson_ratio: Poisson's ratio
            thickness: Plate thickness
            breadth: Plate breadth
            length: Plate length
            edge_condition: Edge boundary condition

        Returns:
            Shear elastic buckling stress
        """
        base_factor = self.calculate_base_factor(youngs_modulus, poisson_ratio, thickness, breadth)
        aspect_ratio = breadth / length  # b/l for shear

        k_shear = self.coefficients.get_shear_coefficient(aspect_ratio, edge_condition)
        tau_cr = base_factor * k_shear

        logger.debug(f"Shear buckling stress: {tau_cr:.2e}")
        return tau_cr

    def calculate_all_elastic_buckling_stresses(self,
                                              youngs_modulus: float,
                                              poisson_ratio: float,
                                              thickness: float,
                                              breadth: float,
                                              length: float,
                                              edge_condition: PlateEdgeCondition = PlateEdgeCondition.SIMPLY_SUPPORTED) -> Dict[str, float]:
        """
        Calculate all elastic buckling stresses for a plate.

        Args:
            youngs_modulus: Young's modulus
            poisson_ratio: Poisson's ratio
            thickness: Plate thickness
            breadth: Plate breadth
            length: Plate length
            edge_condition: Edge boundary condition

        Returns:
            Dictionary of elastic buckling stresses
        """
        return {
            'longitudinal': self.calculate_longitudinal_buckling_stress(
                youngs_modulus, poisson_ratio, thickness, breadth, length, edge_condition),
            'transverse': self.calculate_transverse_buckling_stress(
                youngs_modulus, poisson_ratio, thickness, breadth, length, edge_condition),
            'shear': self.calculate_shear_buckling_stress(
                youngs_modulus, poisson_ratio, thickness, breadth, length, edge_condition)
        }


class SlendernessCalculator:
    """
    Calculator for reduced slenderness ratios used in plate buckling analysis.

    Slenderness ratios are key parameters in determining the buckling behavior
    and ultimate strength of plates under various loading conditions.
    """

    @staticmethod
    def calculate_slenderness_ratio(characteristic_resistance: float,
                                  elastic_buckling_stress: float) -> float:
        """
        Calculate reduced slenderness ratio.

        Args:
            characteristic_resistance: Material characteristic resistance
            elastic_buckling_stress: Elastic buckling stress

        Returns:
            Reduced slenderness ratio
        """
        if elastic_buckling_stress <= 0:
            logger.warning("Elastic buckling stress is zero or negative")
            return 0.0

        slenderness = math.sqrt(characteristic_resistance / elastic_buckling_stress)
        return slenderness

    @staticmethod
    def calculate_equivalent_slenderness(applied_stresses: Dict[str, float],
                                       elastic_buckling_stresses: Dict[str, float],
                                       yield_strength: float,
                                       von_mises_stress: float,
                                       interaction_exponent: float = 2.0) -> float:
        """
        Calculate equivalent slenderness ratio for combined loading.

        Args:
            applied_stresses: Dictionary of applied stresses
            elastic_buckling_stresses: Dictionary of elastic buckling stresses
            yield_strength: Material yield strength
            von_mises_stress: Von Mises equivalent stress
            interaction_exponent: Interaction exponent (typically 2.0)

        Returns:
            Equivalent slenderness ratio
        """
        if von_mises_stress <= 0 or yield_strength <= 0:
            return 0.0

        # Calculate interaction term
        sigma_x = abs(applied_stresses.get('longitudinal', 0))
        sigma_y = abs(applied_stresses.get('transverse', 0))
        tau_xy = abs(applied_stresses.get('shear', 0))

        sigma_cr_x = elastic_buckling_stresses.get('longitudinal', 1)
        sigma_cr_y = elastic_buckling_stresses.get('transverse', 1)
        tau_cr = elastic_buckling_stresses.get('shear', 1)

        c = interaction_exponent

        interaction_term = 0.0
        if sigma_cr_x > 0:
            interaction_term += (sigma_x / sigma_cr_x)**c
        if sigma_cr_y > 0:
            interaction_term += (sigma_y / sigma_cr_y)**c
        if tau_cr > 0:
            interaction_term += (tau_xy / tau_cr)**c

        if interaction_term > 0:
            interaction_term = interaction_term**(1/c)
            lambda_eq = math.sqrt(yield_strength / von_mises_stress * interaction_term)
        else:
            lambda_eq = 0.0

        return lambda_eq


class UltimateStrengthCalculator:
    """
    Calculator for ultimate strength and characteristic resistance of plates.

    This class implements calculations for characteristic buckling resistance
    according to DNV standards, considering both serviceability and ultimate limit states.
    """

    @staticmethod
    def calculate_characteristic_resistance_serviceability(yield_strength: float,
                                                         slenderness_ratio: float) -> float:
        """
        Calculate characteristic buckling resistance for serviceability limit state.

        Args:
            yield_strength: Material yield strength
            slenderness_ratio: Reduced slenderness ratio

        Returns:
            Characteristic resistance for serviceability
        """
        if yield_strength <= 0:
            raise ValueError("Yield strength must be positive")

        if slenderness_ratio <= 0:
            return yield_strength

        resistance = yield_strength / math.sqrt(1 + slenderness_ratio**4)
        return resistance

    @staticmethod
    def calculate_characteristic_resistance_ultimate(yield_strength: float,
                                                   slenderness_ratio: float) -> float:
        """
        Calculate characteristic buckling resistance for ultimate limit state.

        Args:
            yield_strength: Material yield strength
            slenderness_ratio: Reduced slenderness ratio

        Returns:
            Characteristic resistance for ultimate limit state
        """
        if yield_strength <= 0:
            raise ValueError("Yield strength must be positive")

        if slenderness_ratio <= 0:
            return yield_strength

        if slenderness_ratio < 1.0:
            # For low slenderness (yielding governs)
            resistance = yield_strength / math.sqrt(1 + slenderness_ratio**4)
        else:
            # For high slenderness (buckling governs)
            resistance = yield_strength / (math.sqrt(2) * slenderness_ratio)

        return resistance

    @staticmethod
    def calculate_dnv_longitudinal_resistance(yield_strength: float,
                                            youngs_modulus: float,
                                            thickness: float,
                                            breadth: float,
                                            material_factor: float = 1.15) -> float:
        """
        Calculate longitudinal buckling resistance per DNV-RP-C201.

        Args:
            yield_strength: Material yield strength
            youngs_modulus: Young's modulus
            thickness: Plate thickness
            breadth: Plate breadth
            material_factor: Material safety factor

        Returns:
            Design longitudinal buckling resistance
        """
        # Calculate plate slenderness parameter
        lambda_p = 0.525 * (breadth / thickness) * math.sqrt(yield_strength / youngs_modulus)

        if lambda_p <= 0.673:
            # Yielding governs
            reduction_factor = 1.0
        else:
            # Buckling governs
            reduction_factor = (lambda_p - 0.22) / lambda_p**2

        sigma_xrd = reduction_factor * yield_strength / material_factor
        return sigma_xrd

    @staticmethod
    def calculate_dnv_transverse_resistance(yield_strength: float,
                                          youngs_modulus: float,
                                          thickness: float,
                                          breadth: float,
                                          length: float,
                                          k4_factor: float = 1.0,
                                          pressure: float = 0.0,
                                          material_factor: float = 1.15) -> float:
        """
        Calculate transverse buckling resistance per DNV-RP-C201.

        Args:
            yield_strength: Material yield strength
            youngs_modulus: Young's modulus
            thickness: Plate thickness
            breadth: Plate breadth
            length: Plate length
            k4_factor: Buckling factor from standards
            pressure: Applied pressure
            material_factor: Material safety factor

        Returns:
            Design transverse buckling resistance
        """
        # Calculate column-like slenderness
        lambda_c = 1.1 * (breadth / thickness) * math.sqrt(yield_strength / youngs_modulus)

        if lambda_c <= 0.2:
            reduction_factor = 1.0
        else:
            mu = 0.21 * (lambda_c - 0.2)
            term1 = 1 + mu + lambda_c**2
            term2 = math.sqrt(term1**2 - 4*lambda_c**2)
            reduction_factor = (term1 - term2) / (2 * lambda_c**2)

        # Pressure effect (simplified)
        pressure_factor = 1.0
        if pressure > 0:
            h_alpha = max(0, 0.05 * (breadth/thickness) - 0.75)
            p_limit = 2 * (thickness/breadth)**2 * yield_strength
            if pressure > p_limit:
                pressure_factor = 1 - h_alpha * (pressure/yield_strength - 2*(thickness/breadth)**2)
                pressure_factor = max(0, pressure_factor)

        # Base resistance
        sigma_y_base = (1.3 * thickness/length * math.sqrt(youngs_modulus/yield_strength) +
                       k4_factor * (1 - 1.3 * thickness/length * math.sqrt(youngs_modulus/yield_strength)))

        sigma_yrd = reduction_factor * sigma_y_base * yield_strength * pressure_factor / material_factor
        return sigma_yrd

    @staticmethod
    def calculate_dnv_shear_resistance(yield_strength: float,
                                     youngs_modulus: float,
                                     thickness: float,
                                     breadth: float,
                                     length: float,
                                     material_factor: float = 1.15) -> float:
        """
        Calculate shear buckling resistance per DNV-RP-C201.

        Args:
            yield_strength: Material yield strength
            youngs_modulus: Young's modulus
            thickness: Plate thickness
            breadth: Plate breadth
            length: Plate length
            material_factor: Material safety factor

        Returns:
            Design shear buckling resistance
        """
        # Aspect ratio
        aspect_ratio = breadth / length

        # Shear buckling coefficient
        if aspect_ratio < 1:
            k_s = 5.34 + 4 * aspect_ratio**2
        else:
            k_s = 5.34 * aspect_ratio**2 + 4

        # Web slenderness
        lambda_w = 0.795 * (breadth / thickness) * math.sqrt(yield_strength / (youngs_modulus * k_s))

        # Shear reduction factor
        if lambda_w <= 0.8:
            reduction_factor = 1.0
        elif lambda_w <= 1.2:
            reduction_factor = 1 - 0.625 * (lambda_w - 0.8)
        else:
            reduction_factor = 0.9 / lambda_w

        tau_rd = reduction_factor * yield_strength / (math.sqrt(3) * material_factor)
        return tau_rd


class UsageFactorCalculator:
    """
    Calculator for usage factors according to DNV standards.

    Usage factors compare applied stresses to allowable stresses and
    indicate the safety margin in the design.
    """

    @staticmethod
    def calculate_usage_factor(applied_stress: float, allowable_stress: float) -> float:
        """
        Calculate basic usage factor.

        Args:
            applied_stress: Applied stress value
            allowable_stress: Allowable stress value

        Returns:
            Usage factor (applied/allowable)
        """
        if allowable_stress <= 0:
            logger.warning("Allowable stress is zero or negative")
            return float('inf') if applied_stress > 0 else 0.0

        return abs(applied_stress) / allowable_stress

    @staticmethod
    def calculate_biaxial_usage_factor(sigma_x: float, sigma_y: float, tau_xy: float,
                                     sigma_xrd: float, sigma_yrd: float, tau_rd: float,
                                     interaction_factor: float = 1.0) -> float:
        """
        Calculate biaxial interaction usage factor.

        Args:
            sigma_x: Applied longitudinal stress
            sigma_y: Applied transverse stress
            tau_xy: Applied shear stress
            sigma_xrd: Design longitudinal resistance
            sigma_yrd: Design transverse resistance
            tau_rd: Design shear resistance
            interaction_factor: Interaction factor (ci)

        Returns:
            Biaxial usage factor
        """
        if sigma_xrd <= 0 or sigma_yrd <= 0 or tau_rd <= 0:
            logger.warning("One or more design resistances are zero or negative")
            return float('inf')

        term1 = (sigma_x / sigma_xrd)**2
        term2 = (sigma_y / sigma_yrd)**2
        term3 = -interaction_factor * (sigma_x / sigma_xrd) * (sigma_y / sigma_yrd)
        term4 = (tau_xy / tau_rd)**2

        biaxial_factor = math.sqrt(max(0, term1 + term2 + term3 + term4))
        return biaxial_factor

    @staticmethod
    def calculate_all_usage_factors(applied_stresses: Dict[str, float],
                                  design_resistances: Dict[str, float],
                                  interaction_factor: float = 1.0) -> Dict[str, float]:
        """
        Calculate all usage factors for a plate.

        Args:
            applied_stresses: Dictionary of applied stresses
            design_resistances: Dictionary of design resistances
            interaction_factor: Interaction factor for biaxial calculation

        Returns:
            Dictionary of usage factors
        """
        sigma_x = applied_stresses.get('longitudinal', 0)
        sigma_y = applied_stresses.get('transverse', 0)
        tau_xy = applied_stresses.get('shear', 0)

        sigma_xrd = design_resistances.get('longitudinal', 1)
        sigma_yrd = design_resistances.get('transverse', 1)
        tau_rd = design_resistances.get('shear', 1)

        usage_factors = {
            'longitudinal': UsageFactorCalculator.calculate_usage_factor(sigma_x, sigma_xrd),
            'transverse': UsageFactorCalculator.calculate_usage_factor(sigma_y, sigma_yrd),
            'shear': UsageFactorCalculator.calculate_usage_factor(tau_xy, tau_rd),
            'biaxial': UsageFactorCalculator.calculate_biaxial_usage_factor(
                sigma_x, sigma_y, tau_xy, sigma_xrd, sigma_yrd, tau_rd, interaction_factor)
        }

        return usage_factors


# Convenience functions for legacy compatibility
def calculate_plate_buckling_212(plate_data: Dict[str, float]) -> Dict[str, float]:
    """
    Legacy compatibility function for PlateBuckling_212.py calculations.

    Args:
        plate_data: Dictionary containing plate parameters

    Returns:
        Dictionary of buckling calculation results
    """
    # Extract parameters with defaults
    E = plate_data.get('YoungsModulus', 30000000)  # psi
    v = plate_data.get('PoissionsRatio', 0.3)
    k_1 = plate_data.get('constantvalueTable1', 0.425)
    t_p = plate_data.get('PlateThickness', 0.552)  # in
    b_p = plate_data.get('PlateBreadth', 27.6)     # in
    l1_p = plate_data.get('PlateLength', 104.04)   # in
    E_t = plate_data.get('TangentModulus', 25000000)  # psi
    sig_y = plate_data.get('YieldPoint', 33000)    # psi

    # Basic buckling calculations (edge compression)
    sig_cr1 = (k_1 * math.pi**2 * E / (12 * (1 - v**2))) * (t_p / b_p)**2

    # Tangent modulus effects
    lam_1 = E_t / E
    sig_cr2 = (math.pi**2 * E * lam_1 / (12 * (1 - v**2))) * (t_p / b_p)**2 * k_1
    sig_cr3 = (math.pi**2 * E * math.sqrt(lam_1) / (12 * (1 - v**2))) * (t_p / b_p)**2 * k_1

    # Shear buckling
    k_2 = 5.34 + (4 / math.sqrt(l1_p))
    sig_cr7 = (math.sqrt(3) * k_2 * math.pi**2 * E / (12 * (1 - v**2))) * (t_p / b_p)**2

    results = {
        'critical_stress_edge_compression': sig_cr1,
        'critical_stress_tangent_modulus': sig_cr2,
        'critical_stress_improved': sig_cr3,
        'critical_stress_shear': sig_cr7,
        'buckling_coefficient_shear': k_2,
        'tangent_modulus_ratio': lam_1
    }

    return results


# Example usage
if __name__ == "__main__":
    # Set up logging
    logging.basicConfig(level=logging.INFO)

    # Example calculation
    calc = ElasticBucklingCalculator()

    # Steel plate properties
    E = 210e9     # Pa
    v = 0.3
    t = 0.014     # m
    b = 0.70      # m
    l = 2.69      # m

    # Calculate elastic buckling stresses
    elastic_stresses = calc.calculate_all_elastic_buckling_stresses(
        E, v, t, b, l, PlateEdgeCondition.SIMPLY_SUPPORTED)

    print("Elastic Buckling Stresses:")
    for direction, stress in elastic_stresses.items():
        print(f"  {direction}: {stress/1e6:.2f} MPa")

    # Calculate slenderness ratios
    fy = 235e6  # Pa
    slenderness_calc = SlendernessCalculator()

    slenderness_ratios = {}
    for direction, stress in elastic_stresses.items():
        if direction != 'shear':
            char_resistance = fy
        else:
            char_resistance = fy / math.sqrt(3)

        slenderness_ratios[direction] = slenderness_calc.calculate_slenderness_ratio(
            char_resistance, stress)

    print("\nSlenderness Ratios:")
    for direction, ratio in slenderness_ratios.items():
        print(f"  {direction}: {ratio:.3f}")