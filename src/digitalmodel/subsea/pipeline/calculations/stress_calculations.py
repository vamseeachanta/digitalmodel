"""
Stress Calculations Module for API STD 2RD Analysis

This module provides functions for calculating burst pressure, collapse pressure,
and stress analysis according to API STD 2RD methods.

Author: Digitally Modernized from Legacy Code
Date: 2025-01-15
"""

import math
from typing import Dict, Any, Union, Optional
import logging
import numpy as np
import pandas as pd

logger = logging.getLogger(__name__)


class APISTD2RDCalculations:
    """
    Class for API STD 2RD stress and pressure calculations.
    """

    @staticmethod
    def calculate_burst_pressure(
        outer_diameter: float,
        inner_diameter: float,
        yield_strength: float,
        ultimate_strength: float,
        corrosion_allowance: float = 0.0,
        include_corrosion: bool = True
    ) -> float:
        """
        Calculate burst pressure according to API STD 2RD.

        Args:
            outer_diameter: Outer diameter of pipe
            inner_diameter: Inner diameter of pipe
            yield_strength: Minimum yield strength (SMYS)
            ultimate_strength: Minimum ultimate strength (SMUS)
            corrosion_allowance: Corrosion allowance
            include_corrosion: Whether to include corrosion allowance in calculation

        Returns:
            Burst pressure

        Raises:
            ValueError: If input parameters are invalid
        """
        if outer_diameter <= 0 or inner_diameter <= 0:
            raise ValueError("Diameters must be positive")

        if inner_diameter >= outer_diameter:
            raise ValueError("Inner diameter must be less than outer diameter")

        if yield_strength <= 0 or ultimate_strength <= 0:
            raise ValueError("Strength values must be positive")

        # Adjust inner diameter for corrosion allowance if required
        id_effective = inner_diameter + (2 * corrosion_allowance if include_corrosion else 0)

        if id_effective >= outer_diameter:
            raise ValueError("Corrosion allowance too large")

        # API STD 2RD burst pressure formula
        burst_pressure = 0.45 * (yield_strength + ultimate_strength) * math.log(outer_diameter / id_effective)

        logger.debug(f"Calculated burst pressure: {burst_pressure} for OD={outer_diameter}, "
                    f"ID_eff={id_effective}, SMYS={yield_strength}, SMUS={ultimate_strength}")

        return burst_pressure

    @staticmethod
    def calculate_design_pressures(
        burst_pressure: float,
        design_factors: Dict[str, Dict[str, float]]
    ) -> Dict[str, float]:
        """
        Calculate design pressures based on burst pressure and design factors.

        Args:
            burst_pressure: Calculated burst pressure
            design_factors: Dictionary containing design factors for different conditions

        Returns:
            Dictionary containing calculated design pressures
        """
        pressures = {}

        # Design pressure
        if 'internalPressure' in design_factors and 'design' in design_factors['internalPressure']:
            pressures['design'] = design_factors['internalPressure']['design'] * burst_pressure

        # Accidental pressure
        if 'internalPressure' in design_factors and 'incidentalPressure' in design_factors['internalPressure']:
            pressures['accidental'] = design_factors['internalPressure']['incidentalPressure'] * burst_pressure

        # Test pressure (no corrosion allowance)
        if 'internalPressure' in design_factors and 'hydroStaticTest' in design_factors['internalPressure']:
            pressures['test'] = design_factors['internalPressure']['hydroStaticTest'] * burst_pressure

        return pressures

    @staticmethod
    def calculate_collapse_pressure(
        outer_diameter: float,
        inner_diameter: float,
        wall_thickness: float,
        yield_strength: float,
        elastic_modulus: float,
        poisson_ratio: float,
        fabrication_factor: float = 1.0
    ) -> Dict[str, float]:
        """
        Calculate collapse pressure components according to API STD 2RD.

        Args:
            outer_diameter: Outer diameter
            inner_diameter: Inner diameter
            wall_thickness: Wall thickness
            yield_strength: Minimum yield strength
            elastic_modulus: Elastic modulus
            poisson_ratio: Poisson's ratio
            fabrication_factor: Fabrication factor (alphafab)

        Returns:
            Dictionary containing collapse pressure components
        """
        if any(val <= 0 for val in [outer_diameter, inner_diameter, wall_thickness,
                                   yield_strength, elastic_modulus]):
            raise ValueError("All input parameters must be positive")

        if not 0 < poisson_ratio < 0.5:
            raise ValueError("Poisson ratio must be between 0 and 0.5")

        # Yield collapse pressure
        py = 2 * yield_strength * (wall_thickness / inner_diameter)

        # Elastic collapse pressure
        pel = (2 * elastic_modulus * (wall_thickness / inner_diameter)**3) / (1 - poisson_ratio**2)

        # Plastic collapse pressure
        pp = 2 * (wall_thickness / outer_diameter) * yield_strength * fabrication_factor

        # Combined collapse pressure
        pc = (py * pel) / math.sqrt(py**2 + pel**2)

        collapse_data = {
            "yield_collapse": py,
            "elastic_collapse": pel,
            "plastic_collapse": pp,
            "combined_collapse": pc
        }

        logger.debug(f"Calculated collapse pressures: {collapse_data}")

        return collapse_data

    @staticmethod
    def calculate_method1_limits(
        outer_diameter: float,
        inner_diameter: float,
        wall_thickness: float,
        yield_strength: float,
        burst_pressure: float,
        design_factors: Dict[str, Dict[str, float]],
        limit_state: str,
        internal_pressure: float = 0.0,
        external_pressure: float = 0.0
    ) -> Dict[str, Any]:
        """
        Calculate Method 1 limits for API STD 2RD analysis.

        Args:
            outer_diameter: Outer diameter
            inner_diameter: Inner diameter
            wall_thickness: Wall thickness
            yield_strength: Minimum yield strength
            burst_pressure: Burst pressure
            design_factors: Design factors dictionary
            limit_state: Limit state for design factor selection
            internal_pressure: Internal pressure
            external_pressure: External pressure

        Returns:
            Dictionary containing Method 1 calculation results
        """
        # Cross-sectional area
        area = (math.pi / 4) * (outer_diameter**2 - inner_diameter**2)

        # Yield tension
        ty_kn = (yield_strength * area) / 1000 * 4.448222  # Convert to kN

        # Yield moment (converted to kN.m)
        my_knm = (math.pi / 4 * (yield_strength * (outer_diameter - wall_thickness)**2 * wall_thickness)
                 / 1000 / 12 * 1.355818)

        # Plastic moment
        mp_knm = (4 / math.pi) * my_knm

        # Pressure corrected design factor
        pressure_diff = internal_pressure - external_pressure
        pressure_ratio = pressure_diff / burst_pressure

        design_factor = design_factors['internalPressure'][limit_state]
        fd_pressure_corrected = math.sqrt(design_factor**2 - pressure_ratio**2)

        # Limiting values
        t_limiting = fd_pressure_corrected * ty_kn
        m_limiting_method1 = fd_pressure_corrected * my_knm
        m_limiting_method2 = fd_pressure_corrected * mp_knm

        # Tension limit check
        tension_check = (math.sqrt(pressure_ratio**2 + (t_limiting / ty_kn)**2) / design_factor)
        tension_check_pass = tension_check <= 1.0

        results = {
            "yield_tension": ty_kn,
            "yield_moment": my_knm,
            "plastic_moment": mp_knm,
            "pressure_corrected_factor": fd_pressure_corrected,
            "limiting_tension": t_limiting,
            "limiting_moment_method1": m_limiting_method1,
            "limiting_moment_method2": m_limiting_method2,
            "tension_check": tension_check,
            "tension_check_pass": tension_check_pass
        }

        return results

    @staticmethod
    def generate_interaction_curves(
        yield_tension: float,
        yield_moment: float,
        plastic_moment: float,
        pressure_corrected_factor: float,
        tension_steps: int = 100
    ) -> pd.DataFrame:
        """
        Generate interaction curves for Method 1 and Method 2.

        Args:
            yield_tension: Yield tension capacity
            yield_moment: Yield moment capacity
            plastic_moment: Plastic moment capacity
            pressure_corrected_factor: Pressure corrected design factor
            tension_steps: Number of steps for tension array

        Returns:
            DataFrame containing interaction curve data
        """
        # Create tension array
        t_array = np.linspace(-pressure_corrected_factor * yield_tension,
                             pressure_corrected_factor * yield_tension,
                             tension_steps)

        # Method 1 curves (linear)
        m_positive_method1 = []
        m_negative_method1 = []

        # Method 2 curves (circular)
        m_positive_method2 = []
        m_negative_method2 = []

        for tension in t_array:
            # Method 1 - Linear interaction
            tension_ratio = abs(tension / yield_tension)
            moment_capacity_1 = yield_moment * (pressure_corrected_factor - tension_ratio)

            m_positive_method1.append(moment_capacity_1)
            m_negative_method1.append(-moment_capacity_1)

            # Method 2 - Circular interaction
            normalized_tension = (tension / yield_tension) / pressure_corrected_factor
            cos_term = math.cos((math.pi / 2) * normalized_tension)
            moment_capacity_2 = plastic_moment * pressure_corrected_factor * cos_term

            m_positive_method2.append(moment_capacity_2)
            m_negative_method2.append(-moment_capacity_2)

        # Create DataFrame
        result_df = pd.DataFrame({
            'tension': t_array,
            'moment_positive_method1': m_positive_method1,
            'moment_negative_method1': m_negative_method1,
            'moment_positive_method2': m_positive_method2,
            'moment_negative_method2': m_negative_method2
        })

        return result_df


def calculate_utilization(
    applied_tension: float,
    applied_moment: float,
    yield_tension: float,
    yield_moment: float,
    pressure_corrected_factor: float,
    method: str = "method1"
) -> float:
    """
    Calculate utilization ratio for applied loads.

    Args:
        applied_tension: Applied tension force
        applied_moment: Applied bending moment
        yield_tension: Yield tension capacity
        yield_moment: Yield moment capacity
        pressure_corrected_factor: Pressure corrected design factor
        method: Analysis method ("method1" or "method2")

    Returns:
        Utilization ratio

    Raises:
        ValueError: If method is not recognized
    """
    if method.lower() == "method1":
        # Linear interaction (Method 1)
        tension_term = abs(applied_tension) / yield_tension
        moment_term = abs(applied_moment) / yield_moment
        lhs = tension_term + moment_term
        utilization = lhs / pressure_corrected_factor
    elif method.lower() == "method2":
        # Circular interaction (Method 2)
        tension_ratio = applied_tension / yield_tension
        moment_ratio = applied_moment / yield_moment
        lhs = math.sqrt(tension_ratio**2 + moment_ratio**2)
        utilization = lhs / pressure_corrected_factor
    else:
        raise ValueError("Method must be 'method1' or 'method2'")

    return utilization


def apply_temperature_derating(
    yield_strength: float,
    ultimate_strength: float,
    temperature_celsius: float
) -> Dict[str, float]:
    """
    Apply temperature derating to material properties.

    Args:
        yield_strength: Original yield strength
        ultimate_strength: Original ultimate strength
        temperature_celsius: Operating temperature in Celsius

    Returns:
        Dictionary with derated strength values

    Note:
        This is a placeholder implementation. Actual derating curves
        should be based on specific material standards.
    """
    # Placeholder implementation - actual derating would depend on material type and standard
    if temperature_celsius <= 50:
        # No derating below 50°C
        derating_factor = 1.0
    elif temperature_celsius <= 100:
        # Linear derating between 50-100°C
        derating_factor = 1.0 - 0.1 * (temperature_celsius - 50) / 50
    else:
        # Further derating above 100°C
        derating_factor = 0.9 - 0.2 * (temperature_celsius - 100) / 100
        derating_factor = max(derating_factor, 0.5)  # Minimum 50% of room temperature strength

    derated_yield = yield_strength * derating_factor
    derated_ultimate = ultimate_strength * derating_factor

    logger.info(f"Applied temperature derating at {temperature_celsius}°C: "
               f"factor={derating_factor:.3f}")

    return {
        "yield_strength": derated_yield,
        "ultimate_strength": derated_ultimate,
        "derating_factor": derating_factor,
        "temperature": temperature_celsius
    }