"""
Von Mises Stress Analysis Module

This module provides comprehensive Von Mises stress calculations for various
loading conditions including pressure vessels, pipes, and structural elements.
Migrated and modernized from legacy VMStressCalculations modules.
"""

import math
import numpy as np
from typing import Dict, List, Optional, Tuple, Union, NamedTuple
from dataclasses import dataclass
from enum import Enum
import logging

logger = logging.getLogger(__name__)


class LoadingType(Enum):
    """Types of loading conditions"""
    PRESSURE = "pressure"
    TENSION = "tension"
    BENDING = "bending"
    COMBINED = "combined"


@dataclass
class StressState:
    """Represents a 3D stress state with principal stress components"""
    sigma_x: float  # Normal stress in x-direction (Pa)
    sigma_y: float  # Normal stress in y-direction (Pa)
    sigma_z: float  # Normal stress in z-direction (Pa)
    tau_xy: float = 0.0  # Shear stress in xy-plane (Pa)
    tau_yz: float = 0.0  # Shear stress in yz-plane (Pa)
    tau_zx: float = 0.0  # Shear stress in zx-plane (Pa)

    def validate(self) -> None:
        """Validate stress state values"""
        stress_values = [self.sigma_x, self.sigma_y, self.sigma_z,
                        self.tau_xy, self.tau_yz, self.tau_zx]

        if any(not isinstance(val, (int, float)) for val in stress_values):
            raise ValueError("All stress values must be numeric")

        if any(math.isnan(val) or math.isinf(val) for val in stress_values):
            raise ValueError("Stress values cannot be NaN or infinite")


@dataclass
class LoadingCondition:
    """Represents loading conditions for stress analysis"""
    internal_pressure: float = 0.0  # Pa
    external_pressure: float = 0.0  # Pa
    axial_force: float = 0.0  # N
    bending_moment: float = 0.0  # N⋅m
    torque: float = 0.0  # N⋅m
    temperature_change: float = 0.0  # K

    def validate(self) -> None:
        """Validate loading condition values"""
        loading_values = [self.internal_pressure, self.external_pressure,
                         self.axial_force, self.bending_moment,
                         self.torque, self.temperature_change]

        if any(not isinstance(val, (int, float)) for val in loading_values):
            raise ValueError("All loading values must be numeric")

        if any(math.isnan(val) or math.isinf(val) for val in loading_values):
            raise ValueError("Loading values cannot be NaN or infinite")


@dataclass
class PipeGeometry:
    """Pipe geometric properties"""
    outer_diameter: float  # m
    wall_thickness: float  # m
    length: Optional[float] = None  # m

    def __post_init__(self):
        self.validate()

    def validate(self) -> None:
        """Validate pipe geometry"""
        if self.outer_diameter <= 0:
            raise ValueError("Outer diameter must be positive")
        if self.wall_thickness <= 0:
            raise ValueError("Wall thickness must be positive")
        if self.wall_thickness >= self.outer_diameter / 2:
            raise ValueError("Wall thickness must be less than radius")
        if self.length is not None and self.length <= 0:
            raise ValueError("Length must be positive if specified")

    @property
    def inner_diameter(self) -> float:
        """Calculate inner diameter"""
        return self.outer_diameter - 2 * self.wall_thickness

    @property
    def mean_diameter(self) -> float:
        """Calculate mean diameter"""
        return self.outer_diameter - self.wall_thickness

    @property
    def cross_sectional_area(self) -> float:
        """Calculate cross-sectional area of pipe wall"""
        outer_area = math.pi * (self.outer_diameter / 2) ** 2
        inner_area = math.pi * (self.inner_diameter / 2) ** 2
        return outer_area - inner_area

    @property
    def inner_area(self) -> float:
        """Calculate inner cross-sectional area"""
        return math.pi * (self.inner_diameter / 2) ** 2

    @property
    def moment_of_inertia(self) -> float:
        """Calculate second moment of area"""
        return (math.pi / 64) * (self.outer_diameter**4 - self.inner_diameter**4)

    @property
    def section_modulus(self) -> float:
        """Calculate section modulus"""
        return self.moment_of_inertia / (self.outer_diameter / 2)


@dataclass
class MaterialProperties:
    """Material properties for stress analysis"""
    yield_strength: float  # Pa
    ultimate_strength: float  # Pa
    elastic_modulus: float  # Pa
    poisson_ratio: float
    density: Optional[float] = None  # kg/m³
    thermal_expansion: Optional[float] = None  # 1/K

    def __post_init__(self):
        self.validate()

    def validate(self) -> None:
        """Validate material properties"""
        if self.yield_strength <= 0:
            raise ValueError("Yield strength must be positive")
        if self.ultimate_strength <= 0:
            raise ValueError("Ultimate strength must be positive")
        if self.ultimate_strength < self.yield_strength:
            raise ValueError("Ultimate strength must be >= yield strength")
        if self.elastic_modulus <= 0:
            raise ValueError("Elastic modulus must be positive")
        if not 0 <= self.poisson_ratio <= 0.5:
            raise ValueError("Poisson ratio must be between 0 and 0.5")
        if self.density is not None and self.density <= 0:
            raise ValueError("Density must be positive if specified")


class VonMisesStressCalculator:
    """
    Calculator for Von Mises stress analysis

    Provides methods for calculating Von Mises stress from various stress states
    and loading conditions.
    """

    @staticmethod
    def calculate_from_stress_state(stress_state: StressState) -> float:
        """
        Calculate Von Mises stress from stress state

        Args:
            stress_state: 3D stress state

        Returns:
            Von Mises stress (Pa)
        """
        stress_state.validate()

        s1, s2, s3 = stress_state.sigma_x, stress_state.sigma_y, stress_state.sigma_z

        # Von Mises stress formula
        vm_stress = math.sqrt(0.5 * ((s1 - s2)**2 + (s2 - s3)**2 + (s3 - s1)**2 +
                                   6 * (stress_state.tau_xy**2 +
                                       stress_state.tau_yz**2 +
                                       stress_state.tau_zx**2)))

        return vm_stress

    @staticmethod
    def calculate_from_principal_stresses(sigma1: float, sigma2: float, sigma3: float) -> float:
        """
        Calculate Von Mises stress from principal stresses

        Args:
            sigma1, sigma2, sigma3: Principal stresses (Pa)

        Returns:
            Von Mises stress (Pa)
        """
        vm_stress = math.sqrt(0.5 * ((sigma1 - sigma2)**2 +
                                   (sigma2 - sigma3)**2 +
                                   (sigma3 - sigma1)**2))
        return vm_stress


class PipeStressAnalyzer:
    """
    Comprehensive pipe stress analyzer based on legacy VMStressCalculations

    Provides methods for analyzing stress in pressurized pipes under
    various loading conditions including pressure, tension, and bending.
    """

    def __init__(self, geometry: PipeGeometry, material: MaterialProperties):
        """
        Initialize pipe stress analyzer

        Args:
            geometry: Pipe geometric properties
            material: Material properties
        """
        self.geometry = geometry
        self.material = material
        self.allowable_stress_factor = 0.666  # ASME B31 factor
        self.design_case_factor = 1.0

    def set_design_factors(self, allowable_stress_factor: float = 0.666,
                          design_case_factor: float = 1.0) -> None:
        """Set design factors for stress analysis"""
        if not 0 < allowable_stress_factor <= 1:
            raise ValueError("Allowable stress factor must be between 0 and 1")
        if design_case_factor <= 0:
            raise ValueError("Design case factor must be positive")

        self.allowable_stress_factor = allowable_stress_factor
        self.design_case_factor = design_case_factor

    def calculate_allowable_stress(self) -> float:
        """Calculate allowable stress"""
        return (self.allowable_stress_factor *
                self.design_case_factor *
                self.material.yield_strength)

    def calculate_pressure_stresses(self, internal_pressure: float,
                                  external_pressure: float = 0.0) -> Dict[str, float]:
        """
        Calculate pressure-induced stresses in pipe

        Args:
            internal_pressure: Internal pressure (Pa)
            external_pressure: External pressure (Pa)

        Returns:
            Dictionary containing radial, circumferential, and axial stresses
        """
        if internal_pressure < 0 or external_pressure < 0:
            raise ValueError("Pressures must be non-negative")

        # Geometric properties
        ro = self.geometry.outer_diameter / 2
        ri = self.geometry.inner_diameter / 2
        t = self.geometry.wall_thickness

        # Pressure difference
        pressure_diff = internal_pressure - external_pressure

        # Circumferential stress (hoop stress)
        sigma_circumferential = (pressure_diff * ri) / t

        # Radial stress (at inner surface)
        sigma_radial = -internal_pressure

        # Axial stress due to pressure (closed end condition)
        sigma_axial_pressure = (pressure_diff * ri**2) / (ro**2 - ri**2)

        return {
            'radial': sigma_radial,
            'circumferential': sigma_circumferential,
            'axial_pressure': sigma_axial_pressure
        }

    def calculate_axial_stress(self, axial_force: float) -> float:
        """
        Calculate axial stress due to applied force

        Args:
            axial_force: Applied axial force (N)

        Returns:
            Axial stress (Pa)
        """
        return axial_force / self.geometry.cross_sectional_area

    def calculate_bending_stress(self, bending_moment: float,
                               location: str = "outer") -> float:
        """
        Calculate bending stress

        Args:
            bending_moment: Applied bending moment (N⋅m)
            location: Location for stress calculation ("outer" or "inner")

        Returns:
            Bending stress (Pa)
        """
        if location == "outer":
            radius = self.geometry.outer_diameter / 2
        elif location == "inner":
            radius = self.geometry.inner_diameter / 2
        else:
            raise ValueError("Location must be 'outer' or 'inner'")

        return (bending_moment * radius) / self.geometry.moment_of_inertia

    def calculate_combined_stress(self, loading: LoadingCondition) -> Dict[str, float]:
        """
        Calculate combined stress state from multiple loading conditions

        Args:
            loading: Loading conditions

        Returns:
            Dictionary containing stress components and Von Mises stress
        """
        loading.validate()

        # Pressure stresses
        pressure_stresses = self.calculate_pressure_stresses(
            loading.internal_pressure, loading.external_pressure
        )

        # Axial stress from force
        axial_stress_force = self.calculate_axial_stress(loading.axial_force)

        # Bending stress (at outer fiber)
        bending_stress = self.calculate_bending_stress(loading.bending_moment, "outer")

        # Total stresses
        sigma_radial = pressure_stresses['radial']
        sigma_circumferential = pressure_stresses['circumferential']
        sigma_axial = (pressure_stresses['axial_pressure'] +
                      axial_stress_force + bending_stress)

        # Create stress state
        stress_state = StressState(
            sigma_x=sigma_axial,
            sigma_y=sigma_circumferential,
            sigma_z=sigma_radial
        )

        # Von Mises stress
        vm_stress = VonMisesStressCalculator.calculate_from_stress_state(stress_state)

        return {
            'radial': sigma_radial,
            'circumferential': sigma_circumferential,
            'axial': sigma_axial,
            'von_mises': vm_stress,
            'allowable': self.calculate_allowable_stress(),
            'safety_factor': self.calculate_allowable_stress() / vm_stress if vm_stress > 0 else float('inf')
        }

    def calculate_maximum_tension(self, bending_moment: float) -> Tuple[float, float]:
        """
        Calculate maximum allowable tension for given bending moment
        Based on legacy VMStress calculations

        Args:
            bending_moment: Applied bending moment (N⋅m)

        Returns:
            Tuple of (tension_positive, tension_negative) in N
        """
        # Geometric properties
        pipe_area = self.geometry.cross_sectional_area
        pipe_I = self.geometry.moment_of_inertia
        ro = self.geometry.outer_diameter / 2
        t = self.geometry.wall_thickness

        # Allowable stress
        sigma_allowable = self.calculate_allowable_stress()

        # Pressure stresses (assuming no pressure for tension calculation)
        sigma_radial = 0.0
        sigma_circumferential = 0.0

        # Bending stress component
        bending_stress_term = (bending_moment / (2 * pipe_I)) * (ro - t)
        tension_lhs = 2 * (bending_stress_term ** 2)

        # Right-hand side of equation
        tension_rhs = (2 * sigma_allowable**2 -
                      (sigma_radial - sigma_circumferential)**2 -
                      tension_lhs)

        # Quadratic equation coefficients for tension
        a = 2 * ((1 / pipe_area) ** 2)
        b = (4 * bending_stress_term) * (1 / pipe_area)
        c = -tension_rhs

        # Solve quadratic equation
        discriminant = b**2 - 4*a*c

        if discriminant < 0:
            logger.warning("No real solution for tension calculation")
            return 0.0, 0.0

        sqrt_discriminant = math.sqrt(discriminant)
        tension_positive = (-b + sqrt_discriminant) / (2 * a)
        tension_negative = (-b - sqrt_discriminant) / (2 * a)

        return tension_positive, tension_negative

    def generate_interaction_envelope(self, moment_range: List[float]) -> Dict[str, List[float]]:
        """
        Generate tension-moment interaction envelope
        Based on legacy VMStress plotting functionality

        Args:
            moment_range: Range of bending moments to analyze

        Returns:
            Dictionary with moment and tension arrays for plotting
        """
        tension_positive = []
        tension_negative = []

        for moment in moment_range:
            t_pos, t_neg = self.calculate_maximum_tension(moment)
            tension_positive.append(t_pos / 1000)  # Convert to kN
            tension_negative.append(t_neg / 1000)  # Convert to kN

        # Create symmetric envelope (positive and negative moments)
        moments_kNm = [m / 1000 for m in moment_range]  # Convert to kN⋅m

        # Symmetric data for full envelope
        x_axis = (moments_kNm +
                 moments_kNm[::-1] +
                 [-m for m in moments_kNm] +
                 [-m for m in moments_kNm[::-1]])

        y_axis = (tension_positive +
                 tension_negative[::-1] +
                 [-t for t in tension_positive] +
                 [-t for t in tension_negative[::-1]])

        return {
            'moments': x_axis,
            'tensions': y_axis,
            'positive_envelope': {'moments': moments_kNm, 'tensions': tension_positive},
            'negative_envelope': {'moments': moments_kNm, 'tensions': tension_negative}
        }


def calculate_vm_stress(sigma_x: float, sigma_y: float, sigma_z: float,
                       tau_xy: float = 0.0, tau_yz: float = 0.0,
                       tau_zx: float = 0.0) -> float:
    """
    Convenience function to calculate Von Mises stress

    Args:
        sigma_x, sigma_y, sigma_z: Normal stresses (Pa)
        tau_xy, tau_yz, tau_zx: Shear stresses (Pa)

    Returns:
        Von Mises stress (Pa)
    """
    stress_state = StressState(sigma_x, sigma_y, sigma_z, tau_xy, tau_yz, tau_zx)
    return VonMisesStressCalculator.calculate_from_stress_state(stress_state)


def calculate_principal_stresses(stress_state: StressState) -> Tuple[float, float, float]:
    """
    Calculate principal stresses from 3D stress state

    Args:
        stress_state: 3D stress state

    Returns:
        Tuple of principal stresses (sigma1, sigma2, sigma3) in descending order
    """
    stress_state.validate()

    # Stress tensor components
    sigma_matrix = np.array([
        [stress_state.sigma_x, stress_state.tau_xy, stress_state.tau_zx],
        [stress_state.tau_xy, stress_state.sigma_y, stress_state.tau_yz],
        [stress_state.tau_zx, stress_state.tau_yz, stress_state.sigma_z]
    ])

    # Calculate eigenvalues (principal stresses)
    eigenvalues = np.linalg.eigvals(sigma_matrix)
    principal_stresses = sorted(eigenvalues, reverse=True)

    return tuple(principal_stresses)


# Example usage and testing
if __name__ == "__main__":
    # Example pipe analysis based on legacy code values
    pipe_geometry = PipeGeometry(
        outer_diameter=0.24765,  # m
        wall_thickness=0.034925  # m
    )

    material = MaterialProperties(
        yield_strength=5.52e8,  # Pa (552 MPa)
        ultimate_strength=6.20e8,  # Pa
        elastic_modulus=2.1e11,  # Pa
        poisson_ratio=0.3
    )

    analyzer = PipeStressAnalyzer(pipe_geometry, material)

    # Example loading condition
    loading = LoadingCondition(
        internal_pressure=0,
        external_pressure=0,
        axial_force=0,
        bending_moment=46e4  # N⋅m
    )

    results = analyzer.calculate_combined_stress(loading)
    print("Stress Analysis Results:")
    for key, value in results.items():
        print(f"{key}: {value:.2e} Pa")