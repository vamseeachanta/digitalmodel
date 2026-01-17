# ABOUTME: Rod buckling analysis for sucker rod pump diagnostics.
# ABOUTME: Detects sinusoidal and helical buckling based on axial load distribution.

from typing import Optional, List, Tuple
import numpy as np

from .models import (
    DynacardAnalysisContext,
    CardData,
    RodBucklingAnalysis,
)
from .base import BaseCalculator
from .constants import STEEL_DENSITY_LB_PER_FT3
from .exceptions import DynacardException, ValidationError


class RodBucklingCalculator(BaseCalculator[RodBucklingAnalysis]):
    """
    Calculates rod buckling conditions for sucker rod pump systems.

    Rod buckling occurs when compressive loads exceed critical thresholds.
    Two buckling modes are considered:
    - Sinusoidal buckling: Initial buckling mode with wave-like deformation
    - Helical buckling: More severe mode with corkscrew deformation

    The analysis calculates:
    - Neutral point: Where rod transitions from tension to compression
    - Critical buckling loads based on rod geometry and material
    - Buckling tendency considering buoyancy effects
    """

    # Physical constants
    POISSON_RATIO = 0.30  # Steel Poisson's ratio
    STEEL_DENSITY = STEEL_DENSITY_LB_PER_FT3

    def _create_result(self) -> RodBucklingAnalysis:
        return RodBucklingAnalysis()

    def calculate(
        self,
        downhole_card: Optional[CardData] = None,
    ) -> RodBucklingAnalysis:
        """
        Perform rod buckling analysis.

        Args:
            downhole_card: Optional downhole card data. If not provided,
                          uses a simplified analysis based on surface card.

        Returns:
            RodBucklingAnalysis with buckling detection results.

        Raises:
            ValidationError: If load data or rod string data is missing/invalid.
        """
        # Use provided card or estimate from surface card
        if downhole_card is not None:
            loads = np.array(downhole_card.load)
        else:
            # Estimate downhole loads from surface card
            loads = self._estimate_downhole_loads()

        if len(loads) == 0:
            raise ValidationError(
                "No load data available for buckling analysis",
                field="load_data",
                details={"error_type": "empty_data"}
            )

        # Calculate rod string properties
        rod_length = self.ctx.rod_length  # feet
        if rod_length <= 0:
            raise ValidationError(
                "Rod string length must be positive",
                field="rod_length",
                details={"value": rod_length}
            )

        # Calculate critical buckling loads
        self._calculate_critical_loads()

        # Calculate buckling tendency along the rod
        buckling_tendency = self._calculate_buckling_tendency(loads)

        # Find neutral point
        self._find_neutral_point(buckling_tendency, rod_length)

        # Detect buckling conditions
        self._detect_buckling(loads, buckling_tendency)

        return self.result

    def _estimate_downhole_loads(self) -> np.ndarray:
        """
        Estimate downhole loads from surface card data.

        Uses the simple approximation:
        F_downhole ≈ F_surface - W_rod (on upstroke)
        F_downhole ≈ F_surface (on downstroke, after fluid transfer)
        """
        surface_loads = np.array(self.ctx.surface_card.load)
        if len(surface_loads) == 0:
            return np.array([])

        # Calculate buoyant rod weight
        rod_weight = self.ctx.rod_weight
        fluid_density = self.ctx.fluid_density
        buoyancy_factor = 1.0 - fluid_density / self.STEEL_DENSITY
        buoyant_weight = rod_weight * buoyancy_factor

        # Simple approximation: downhole load is surface load minus buoyant weight
        # This is a rough estimate - full simulation is needed for accuracy
        downhole_loads = surface_loads - buoyant_weight

        return downhole_loads

    def _calculate_critical_loads(self) -> None:
        """
        Calculate critical buckling loads for the rod string.

        Uses simplified Euler buckling formula adapted for sucker rods.
        For deviated wells, the actual critical load is lower.
        """
        if len(self.ctx.rod_string) == 0:
            return

        # Use the smallest (weakest) rod section for critical load calculation
        min_diameter = min(s.diameter for s in self.ctx.rod_string)
        section = next(s for s in self.ctx.rod_string if s.diameter == min_diameter)

        # Rod geometry
        d = min_diameter  # inches
        r = d / 2.0
        A = np.pi * r ** 2  # in^2
        I = np.pi * d ** 4 / 64.0  # in^4 (moment of inertia)

        # Material properties
        E = section.modulus_of_elasticity  # psi

        # Effective length for buckling (use rod length)
        L = self.ctx.rod_length * 12.0  # inches

        # Buoyant weight per unit length
        buoyancy_factor = 1.0 - self.ctx.fluid_density / self.STEEL_DENSITY
        W_b = section.weight_per_foot * buoyancy_factor / 12.0  # lbs/in

        # Critical load calculations
        # Sinusoidal buckling (Paslay-Dawson formula for deviated wells)
        # F_cr = 1.94 * sqrt(E * I * W_b)
        # For vertical wells, we use a modified Euler formula
        if W_b > 0:
            F_cr_sinusoidal = 1.94 * np.sqrt(E * I * W_b)
        else:
            # Fallback to Euler buckling
            F_cr_sinusoidal = np.pi ** 2 * E * I / (L ** 2) if L > 0 else 0.0

        # Helical buckling (approximately 2.83 times sinusoidal)
        F_cr_helical = 2.83 * F_cr_sinusoidal

        self.result.sinusoidal_critical_load = float(F_cr_sinusoidal)
        self.result.helical_critical_load = float(F_cr_helical)

    def _calculate_buckling_tendency(self, loads: np.ndarray) -> np.ndarray:
        """
        Calculate buckling tendency (effective axial load).

        Buckling tendency includes the effect of internal pressure
        and Poisson's ratio on the effective compressive load.

        For a submerged rod:
        F_eff = F_axial + 2 * ν * P * A

        where:
        - F_axial = actual axial load
        - ν = Poisson's ratio
        - P = hydrostatic pressure
        - A = cross-sectional area
        """
        if len(loads) == 0:
            return np.array([])

        # Calculate average rod area
        total_area_length = sum(
            np.pi * (s.diameter / 2) ** 2 * s.length
            for s in self.ctx.rod_string
        )
        rod_length = self.ctx.rod_length
        avg_area = total_area_length / rod_length if rod_length > 0 else 0.0  # in^2

        # Estimate hydrostatic pressure at pump depth
        pump_depth = self.ctx.pump.depth  # feet
        fluid_gradient = self.ctx.fluid_density / 144.0  # psi/ft
        pressure_at_pump = fluid_gradient * pump_depth  # psi

        # Buckling tendency correction
        # This is an approximation for average conditions
        pressure_correction = 2.0 * self.POISSON_RATIO * pressure_at_pump * avg_area

        # Calculate buckling tendency
        buckling_tendency = loads + pressure_correction

        self.result.min_buckling_tendency = float(np.min(buckling_tendency))
        self.result.max_buckling_tendency = float(np.max(buckling_tendency))

        return buckling_tendency

    def _find_neutral_point(
        self,
        buckling_tendency: np.ndarray,
        rod_length: float,
    ) -> None:
        """
        Find the neutral point where rod transitions from tension to compression.

        The neutral point is critical because:
        - Above it: Rod is in tension (no buckling risk)
        - Below it: Rod may be in compression (buckling risk)
        """
        if len(buckling_tendency) == 0:
            return

        min_tendency = np.min(buckling_tendency)
        max_tendency = np.max(buckling_tendency)

        # If all loads are tensile, no neutral point in the rod
        if min_tendency >= 0:
            self.result.neutral_point_depth = rod_length
            self.result.neutral_point_fraction = 1.0
            return

        # If all loads are compressive, neutral point is at surface
        if max_tendency <= 0:
            self.result.neutral_point_depth = 0.0
            self.result.neutral_point_fraction = 0.0
            self.result.compression_length = rod_length
            self.result.max_compressive_load = float(abs(min_tendency))
            return

        # Find approximate neutral point position
        # Assume linear load distribution for estimation
        total_range = max_tendency - min_tendency
        tension_fraction = max_tendency / total_range if total_range > 0 else 0.5

        neutral_depth = rod_length * tension_fraction
        compression_length = rod_length - neutral_depth

        self.result.neutral_point_depth = float(neutral_depth)
        self.result.neutral_point_fraction = float(tension_fraction)
        self.result.compression_depth_start = float(neutral_depth)
        self.result.compression_length = float(compression_length)
        self.result.max_compressive_load = float(abs(min_tendency))

    def _detect_buckling(
        self,
        loads: np.ndarray,
        buckling_tendency: np.ndarray,
    ) -> None:
        """
        Detect sinusoidal and helical buckling conditions.

        Buckling occurs when compressive load exceeds critical threshold.
        """
        if len(buckling_tendency) == 0:
            return

        # Maximum compressive tendency (most negative value)
        min_tendency = np.min(buckling_tendency)

        # Compare against critical loads
        # Buckling occurs when compressive load exceeds threshold
        if min_tendency < 0:
            compressive_load = abs(min_tendency)

            if compressive_load > self.result.sinusoidal_critical_load:
                self.result.sinusoidal_buckling_detected = True

            if compressive_load > self.result.helical_critical_load:
                self.result.helical_buckling_detected = True

        # Also check raw loads for severe compression
        min_load = np.min(loads)
        if min_load < -500:  # Significant compression
            if not self.result.sinusoidal_buckling_detected:
                self.result.sinusoidal_buckling_detected = True
                self.result.warning_message = (
                    "Significant compressive loads detected in rod string"
                )


def calculate_rod_buckling(
    context: DynacardAnalysisContext,
    downhole_card: Optional[CardData] = None,
    raise_on_error: bool = False,
) -> RodBucklingAnalysis:
    """
    Convenience function to calculate rod buckling analysis.

    Args:
        context: Complete analysis context with rod string and pump data.
        downhole_card: Optional downhole card data for more accurate analysis.
        raise_on_error: If True, raises exceptions on validation errors.
                       If False, returns result with warning message set.

    Returns:
        RodBucklingAnalysis with buckling detection results.

    Raises:
        ValidationError: If raise_on_error=True and validation fails.
    """
    calculator = RodBucklingCalculator(context)
    if raise_on_error:
        return calculator.calculate(downhole_card)

    try:
        return calculator.calculate(downhole_card)
    except DynacardException as e:
        calculator.result.warning_message = e.message
        return calculator.result


def estimate_neutral_point(
    surface_load_max: float,
    surface_load_min: float,
    rod_weight: float,
    fluid_density: float = 62.4,
    rod_length: float = 5000.0,
) -> float:
    """
    Estimate neutral point depth from surface card loads.

    Simplified estimation for quick analysis.

    Args:
        surface_load_max: Peak polished rod load (lbs)
        surface_load_min: Minimum polished rod load (lbs)
        rod_weight: Total rod string weight (lbs)
        fluid_density: Fluid density (lbs/ft^3)
        rod_length: Total rod string length (feet)

    Returns:
        Estimated neutral point depth in feet from surface.
    """
    # Buoyancy factor
    steel_density = 490.0
    buoyancy_factor = 1.0 - fluid_density / steel_density

    # Buoyant rod weight
    buoyant_weight = rod_weight * buoyancy_factor

    # Fluid load estimate (difference between upstroke and downstroke)
    fluid_load = surface_load_max - surface_load_min - buoyant_weight

    # Neutral point estimation
    # At neutral point: weight above = fluid load below
    if buoyant_weight > 0:
        neutral_fraction = fluid_load / buoyant_weight
        neutral_depth = rod_length * max(0.0, min(1.0, neutral_fraction))
    else:
        neutral_depth = rod_length

    return neutral_depth


def calculate_critical_buckling_load(
    rod_diameter: float,
    modulus: float = 30500000.0,
    weight_per_foot: float = 0.0,
    fluid_density: float = 62.4,
) -> Tuple[float, float]:
    """
    Calculate critical buckling loads for a rod section.

    Args:
        rod_diameter: Rod diameter in inches.
        modulus: Modulus of elasticity in psi.
        weight_per_foot: Rod weight per foot (calculated if 0).
        fluid_density: Fluid density in lbs/ft^3.

    Returns:
        Tuple of (sinusoidal_critical_load, helical_critical_load) in lbs.
    """
    # Calculate rod properties
    r = rod_diameter / 2.0
    A = np.pi * r ** 2  # in^2
    I = np.pi * rod_diameter ** 4 / 64.0  # in^4

    # Calculate weight if not provided
    if weight_per_foot == 0.0:
        steel_density = 490.0  # lbs/ft^3
        weight_per_foot = np.pi * r ** 2 * steel_density / 144.0

    # Buoyant weight per inch
    buoyancy_factor = 1.0 - fluid_density / 490.0
    W_b = weight_per_foot * buoyancy_factor / 12.0  # lbs/in

    # Critical loads using Paslay-Dawson formula
    if W_b > 0:
        F_cr_sinusoidal = 1.94 * np.sqrt(modulus * I * W_b)
    else:
        F_cr_sinusoidal = 0.0

    F_cr_helical = 2.83 * F_cr_sinusoidal

    return (float(F_cr_sinusoidal), float(F_cr_helical))
