"""
S-N Curve Models for Fatigue Analysis
====================================

This module provides comprehensive S-N curve implementations for structural
fatigue analysis, supporting various international standards including DNV,
API, BS, AWS, and IIW.

Key Features:
- Multi-slope S-N curves with fatigue limits
- Standard curve libraries (DNV-RP-C203, API RP 2A, BS 7608, etc.)
- Mean stress corrections (Goodman, Gerber, Soderberg)
- Thickness effects and weld classifications
- Probability-based curves with safety factors
- Curve fitting from experimental data
- Interactive plotting and visualization

Author: Digital Model Team
Version: 2.0.0
"""

import numpy as np
import pandas as pd
from typing import Dict, Optional, Union, Tuple, List, Literal
from dataclasses import dataclass, field
from abc import ABC, abstractmethod
import logging
from pathlib import Path
import json

logger = logging.getLogger(__name__)

# Type aliases for clarity
StressCycles = Union[float, np.ndarray]
CurveParameters = Dict[str, Union[float, int, str]]


@dataclass
class MaterialProperties:
    """Material properties for fatigue analysis"""
    ultimate_strength: float  # MPa
    yield_strength: float  # MPa
    elastic_modulus: float = 200000.0  # MPa
    reference_thickness: float = 25.0  # mm
    thickness_exponent: float = 0.25
    name: str = "Generic Steel"


@dataclass
class SNPoint:
    """Single point on S-N curve"""
    stress_range: float  # MPa
    cycles: float
    probability: float = 0.5  # Mean curve (50% probability)


class SNCurveBase(ABC):
    """Base class for all S-N curve implementations"""

    def __init__(self, name: str, material: Optional[MaterialProperties] = None):
        self.name = name
        self.material = material or MaterialProperties(
            ultimate_strength=400.0,
            yield_strength=250.0,
            name="Default Steel"
        )

    @abstractmethod
    def get_allowable_cycles(self, stress_range: StressCycles) -> StressCycles:
        """Calculate allowable cycles for given stress range"""
        pass

    @abstractmethod
    def get_stress_range(self, cycles: StressCycles) -> StressCycles:
        """Calculate stress range for given cycles (inverse function)"""
        pass

    def __repr__(self) -> str:
        return f"{self.__class__.__name__}(name='{self.name}')"


class PowerLawSNCurve(SNCurveBase):
    """
    Single-slope power law S-N curve: N = A * S^(-m)

    This is the most common form for high-cycle fatigue analysis.
    """

    def __init__(self,
                 name: str,
                 A: float,
                 m: float,
                 fatigue_limit: float = 0.0,
                 cutoff_cycles: float = 1e7,
                 material: Optional[MaterialProperties] = None):
        """
        Initialize power law S-N curve

        Parameters
        ----------
        name : str
            Curve identifier
        A : float
            Fatigue strength coefficient
        m : float
            Fatigue strength exponent (slope)
        fatigue_limit : float, default=0.0
            Constant amplitude fatigue limit (CAFL) in MPa
        cutoff_cycles : float, default=1e7
            Cycle cutoff for fatigue limit application
        material : MaterialProperties, optional
            Material properties
        """
        super().__init__(name, material)
        self.A = A
        self.m = m
        self.fatigue_limit = fatigue_limit
        self.cutoff_cycles = cutoff_cycles

    def get_allowable_cycles(self, stress_range: StressCycles) -> StressCycles:
        """Calculate allowable cycles using power law"""
        S = np.asarray(stress_range, dtype=float)

        # Handle scalar input
        if S.ndim == 0:
            S = np.array([S])
            scalar_input = True
        else:
            scalar_input = False

        N = np.zeros_like(S)

        # Above fatigue limit
        above_limit = S > self.fatigue_limit
        N[above_limit] = self.A * (S[above_limit] ** (-self.m))

        # Apply cutoff
        N = np.minimum(N, self.cutoff_cycles)

        # Below fatigue limit - infinite life
        N[S <= self.fatigue_limit] = np.inf

        return float(N[0]) if scalar_input else N

    def get_stress_range(self, cycles: StressCycles) -> StressCycles:
        """Calculate stress range for given cycles"""
        N = np.asarray(cycles, dtype=float)

        # Handle scalar input
        if N.ndim == 0:
            N = np.array([N])
            scalar_input = True
        else:
            scalar_input = False

        # Check for infinite cycles
        finite_mask = np.isfinite(N)

        S = np.full_like(N, self.fatigue_limit, dtype=float)
        S[finite_mask] = np.maximum(
            (self.A / N[finite_mask]) ** (1/self.m),
            self.fatigue_limit
        )

        return float(S[0]) if scalar_input else S


class BilinearSNCurve(SNCurveBase):
    """
    Two-slope S-N curve for low and high cycle fatigue

    Common in structural steel applications where crack initiation
    and crack propagation have different behaviors.
    """

    def __init__(self,
                 name: str,
                 A1: float,
                 m1: float,
                 A2: float,
                 m2: float,
                 transition_cycles: float,
                 fatigue_limit: float = 0.0,
                 material: Optional[MaterialProperties] = None):
        """
        Initialize bilinear S-N curve

        Parameters
        ----------
        name : str
            Curve identifier
        A1 : float
            First slope coefficient (low cycle)
        m1 : float
            First slope exponent
        A2 : float
            Second slope coefficient (high cycle)
        m2 : float
            Second slope exponent
        transition_cycles : float
            Cycle count at slope transition
        fatigue_limit : float, default=0.0
            Fatigue limit in MPa
        material : MaterialProperties, optional
            Material properties
        """
        super().__init__(name, material)
        self.A1 = A1
        self.m1 = m1
        self.A2 = A2
        self.m2 = m2
        self.transition_cycles = transition_cycles
        self.fatigue_limit = fatigue_limit

        # Calculate transition stress for continuity
        self.transition_stress = (A1 / transition_cycles) ** (1/m1)

        # Verify continuity
        stress_check = (A2 / transition_cycles) ** (1/m2)
        if abs(self.transition_stress - stress_check) > 0.01 * self.transition_stress:
            logger.warning(f"Bilinear curve {name} may not be continuous at transition")

    def get_allowable_cycles(self, stress_range: StressCycles) -> StressCycles:
        """Calculate allowable cycles using bilinear curve"""
        S = np.asarray(stress_range, dtype=float)
        N = np.zeros_like(S)

        # High stress regime (low cycle)
        high_stress = S > self.transition_stress
        N[high_stress] = self.A1 * (S[high_stress] ** (-self.m1))

        # Low stress regime (high cycle)
        low_stress = (S <= self.transition_stress) & (S > self.fatigue_limit)
        N[low_stress] = self.A2 * (S[low_stress] ** (-self.m2))

        # Below fatigue limit
        N[S <= self.fatigue_limit] = np.inf

        return N if isinstance(stress_range, np.ndarray) else float(N)

    def get_stress_range(self, cycles: StressCycles) -> StressCycles:
        """Calculate stress range for given cycles"""
        N = np.asarray(cycles, dtype=float)
        finite_mask = np.isfinite(N)

        S = np.full_like(N, self.fatigue_limit, dtype=float)

        if np.any(finite_mask):
            # Low cycle regime
            low_cycle = finite_mask & (N <= self.transition_cycles)
            S[low_cycle] = (self.A1 / N[low_cycle]) ** (1/self.m1)

            # High cycle regime
            high_cycle = finite_mask & (N > self.transition_cycles)
            S[high_cycle] = (self.A2 / N[high_cycle]) ** (1/self.m2)

            # Ensure minimum stress is fatigue limit
            S = np.maximum(S, self.fatigue_limit)

        return S if isinstance(cycles, np.ndarray) else float(S)


class StandardSNCurves:
    """
    Library of standard S-N curves from various codes and standards

    Includes curves from:
    - DNV-RP-C203 (Offshore structures)
    - API RP 2A (Fixed platforms)
    - BS 7608 (Steel structures)
    - AWS D1.1 (Structural welding)
    - IIW (International Institute of Welding)

    Enhanced with multi-slope capabilities and legacy curve data integration.
    """

    # DNV-RP-C203 Curves (in air, T=16-25mm)
    DNV_CURVES = {
        'B1': {'A': 4.22e15, 'm': 4.0, 'fatigue_limit': 106.97},
        'B2': {'A': 1.01e15, 'm': 3.5, 'fatigue_limit': 93.59},
        'C': {'A': 1.08e12, 'm': 3.0, 'fatigue_limit': 73.1},
        'C1': {'A': 4.23e11, 'm': 3.0, 'fatigue_limit': 65.5},
        'C2': {'A': 1.08e11, 'm': 3.0, 'fatigue_limit': 46.78},
        'D': {'A': 5.73e11, 'm': 3.0, 'fatigue_limit': 52.63},
        'E': {'A': 3.29e11, 'm': 3.0, 'fatigue_limit': 45.54},
        'F': {'A': 1.73e11, 'm': 3.0, 'fatigue_limit': 36.84},
        'F1': {'A': 1.08e11, 'm': 3.0, 'fatigue_limit': 36.58},
        'F3': {'A': 5.73e10, 'm': 3.0, 'fatigue_limit': 29.64},
        'G': {'A': 2.82e10, 'm': 3.0, 'fatigue_limit': 23.44},
        'W1': {'A': 2.15e10, 'm': 3.0, 'fatigue_limit': 21.34},
        'W2': {'A': 1.08e10, 'm': 3.0, 'fatigue_limit': 16.92},
        'W3': {'A': 5.3e9, 'm': 3.0, 'fatigue_limit': 13.35},
    }

    # Multi-slope DNV curves (based on legacy data structure)
    DNV_MULTISLOPE_CURVES = {
        'D_MULTISLOPE': {
            'slopes': [3.0, 3.5, 5.0],
            'constants': [5.73e11, 1.08e11, 2.5e10],
            'transition_cycles': [2e6, 1e7],
            'fatigue_limit': 52.63
        },
        'F_MULTISLOPE': {
            'slopes': [3.0, 4.0, 5.0],
            'constants': [1.73e11, 8.6e10, 2.15e10],
            'transition_cycles': [2e6, 1e7],
            'fatigue_limit': 36.84
        }
    }

    # API RP 2A-WSD Curves
    API_CURVES = {
        'X': {'A': 1.01e12, 'm': 3.0, 'fatigue_limit': 48.0},
        'X_prime': {'A': 1.52e11, 'm': 3.0, 'fatigue_limit': 32.7},
        'Y': {'A': 3.56e11, 'm': 3.0, 'fatigue_limit': 40.6},
        'S-N1': {'A': 6.21e11, 'm': 3.0, 'fatigue_limit': 0.0},  # Tubular joints
        'S-N2': {'A': 1.24e11, 'm': 3.0, 'fatigue_limit': 0.0},  # Tubular joints
    }

    # BS 7608 Curves
    BS_CURVES = {
        'B': {'A': 3.9e12, 'm': 3.0, 'fatigue_limit': 100.0},
        'C': {'A': 1.0e12, 'm': 3.0, 'fatigue_limit': 63.0},
        'D': {'A': 3.9e11, 'm': 3.0, 'fatigue_limit': 50.0},
        'E': {'A': 1.6e11, 'm': 3.0, 'fatigue_limit': 40.0},
        'F': {'A': 6.3e10, 'm': 3.0, 'fatigue_limit': 32.0},
        'F2': {'A': 2.5e10, 'm': 3.0, 'fatigue_limit': 25.0},
        'G': {'A': 1.0e10, 'm': 3.0, 'fatigue_limit': 20.0},
        'W': {'A': 3.9e9, 'm': 3.0, 'fatigue_limit': 16.0},
    }

    # AWS D1.1 Curves (simplified)
    AWS_CURVES = {
        'A': {'A': 8.11e11, 'm': 3.0, 'fatigue_limit': 165.0},
        'B': {'A': 3.91e11, 'm': 3.0, 'fatigue_limit': 110.0},
        'B1': {'A': 2.63e11, 'm': 3.0, 'fatigue_limit': 83.0},
        'C': {'A': 1.37e11, 'm': 3.0, 'fatigue_limit': 69.0},
        'C1': {'A': 1.01e11, 'm': 3.0, 'fatigue_limit': 55.0},
        'D': {'A': 7.21e10, 'm': 3.0, 'fatigue_limit': 48.0},
        'E': {'A': 3.61e10, 'm': 3.0, 'fatigue_limit': 31.0},
        'E1': {'A': 1.81e10, 'm': 3.0, 'fatigue_limit': 18.0},
    }

    @classmethod
    def get_curve(cls,
                  standard: Literal['DNV', 'API', 'BS', 'AWS'],
                  curve_class: str,
                  material: Optional[MaterialProperties] = None) -> PowerLawSNCurve:
        """
        Get standard S-N curve

        Parameters
        ----------
        standard : {'DNV', 'API', 'BS', 'AWS'}
            Standard/code name
        curve_class : str
            Curve classification (e.g., 'D', 'C', 'X')
        material : MaterialProperties, optional
            Material properties

        Returns
        -------
        PowerLawSNCurve
            Standard S-N curve
        """
        curves_dict = {
            'DNV': cls.DNV_CURVES,
            'API': cls.API_CURVES,
            'BS': cls.BS_CURVES,
            'AWS': cls.AWS_CURVES
        }

        if standard not in curves_dict:
            raise ValueError(f"Unknown standard: {standard}")

        if curve_class not in curves_dict[standard]:
            available = list(curves_dict[standard].keys())
            raise ValueError(f"Unknown {standard} curve class: {curve_class}. Available: {available}")

        params = curves_dict[standard][curve_class]
        name = f"{standard}-{curve_class}"

        return PowerLawSNCurve(
            name=name,
            A=params['A'],
            m=params['m'],
            fatigue_limit=params['fatigue_limit'],
            material=material
        )

    @classmethod
    def get_multislope_curve(cls,
                            standard: Literal['DNV'],
                            curve_class: str,
                            material: Optional[MaterialProperties] = None) -> 'MultislopeSNCurve':
        """
        Get multi-slope S-N curve

        Parameters
        ----------
        standard : {'DNV'}
            Standard/code name (currently only DNV supported)
        curve_class : str
            Multi-slope curve classification
        material : MaterialProperties, optional
            Material properties

        Returns
        -------
        MultislopeSNCurve
            Multi-slope S-N curve
        """
        # Import here to avoid circular imports
        from .analysis import MultislopeSNCurve

        if standard.upper() != 'DNV':
            raise ValueError("Multi-slope curves currently only available for DNV standard")

        if curve_class not in cls.DNV_MULTISLOPE_CURVES:
            available = list(cls.DNV_MULTISLOPE_CURVES.keys())
            raise ValueError(f"Unknown DNV multi-slope curve: {curve_class}. Available: {available}")

        params = cls.DNV_MULTISLOPE_CURVES[curve_class]
        name = f"DNV-{curve_class}"

        return MultislopeSNCurve(
            name=name,
            slopes=params['slopes'],
            constants=params['constants'],
            transition_cycles=params['transition_cycles'],
            fatigue_limit=params['fatigue_limit'],
            material=material
        )

    @classmethod
    def list_curves(cls, standard: Optional[str] = None) -> Dict[str, List[str]]:
        """List available curves by standard"""
        all_curves = {
            'DNV': list(cls.DNV_CURVES.keys()),
            'API': list(cls.API_CURVES.keys()),
            'BS': list(cls.BS_CURVES.keys()),
            'AWS': list(cls.AWS_CURVES.keys()),
            'DNV_MULTISLOPE': list(cls.DNV_MULTISLOPE_CURVES.keys())
        }

        if standard:
            if standard.upper() in all_curves:
                return {standard.upper(): all_curves[standard.upper()]}
            else:
                raise ValueError(f"Unknown standard: {standard}")

        return all_curves


class MeanStressCorrection:
    """
    Mean stress correction methods for fatigue analysis

    Accounts for the effect of mean stress on fatigue life.
    """

    @staticmethod
    def goodman(stress_amplitude: float,
                mean_stress: float,
                ultimate_strength: float) -> float:
        """
        Modified Goodman correction

        Parameters
        ----------
        stress_amplitude : float
            Alternating stress amplitude
        mean_stress : float
            Mean stress
        ultimate_strength : float
            Ultimate tensile strength

        Returns
        -------
        float
            Equivalent fully reversed stress amplitude
        """
        if mean_stress >= ultimate_strength:
            return float('inf')

        return stress_amplitude / (1 - mean_stress / ultimate_strength)

    @staticmethod
    def gerber(stress_amplitude: float,
               mean_stress: float,
               ultimate_strength: float) -> float:
        """
        Gerber correction (parabolic)

        Parameters
        ----------
        stress_amplitude : float
            Alternating stress amplitude
        mean_stress : float
            Mean stress
        ultimate_strength : float
            Ultimate tensile strength

        Returns
        -------
        float
            Equivalent fully reversed stress amplitude
        """
        if mean_stress >= ultimate_strength:
            return float('inf')

        return stress_amplitude / (1 - (mean_stress / ultimate_strength) ** 2)

    @staticmethod
    def soderberg(stress_amplitude: float,
                  mean_stress: float,
                  yield_strength: float) -> float:
        """
        Soderberg correction (conservative)

        Parameters
        ----------
        stress_amplitude : float
            Alternating stress amplitude
        mean_stress : float
            Mean stress
        yield_strength : float
            Yield strength

        Returns
        -------
        float
            Equivalent fully reversed stress amplitude
        """
        if mean_stress >= yield_strength:
            return float('inf')

        return stress_amplitude / (1 - mean_stress / yield_strength)

    @staticmethod
    def walker(stress_amplitude: float,
               mean_stress: float,
               ultimate_strength: float,
               gamma: float = 0.5) -> float:
        """
        Walker correction

        Parameters
        ----------
        stress_amplitude : float
            Alternating stress amplitude
        mean_stress : float
            Mean stress
        ultimate_strength : float
            Ultimate tensile strength
        gamma : float, default=0.5
            Walker exponent

        Returns
        -------
        float
            Equivalent fully reversed stress amplitude
        """
        stress_max = stress_amplitude + mean_stress
        stress_ratio = mean_stress / stress_max if stress_max > 0 else 0

        return stress_amplitude * ((1 - stress_ratio) ** (gamma - 1))


class ThicknessCorrection:
    """
    Thickness effect corrections for welded joints

    Based on various standards including DNV and IIW recommendations.
    """

    @staticmethod
    def apply_thickness_effect(base_curve: PowerLawSNCurve,
                              actual_thickness: float,
                              reference_thickness: float = 25.0,
                              thickness_exponent: float = 0.25) -> PowerLawSNCurve:
        """
        Apply thickness correction to S-N curve

        Parameters
        ----------
        base_curve : PowerLawSNCurve
            Base S-N curve at reference thickness
        actual_thickness : float
            Actual thickness in mm
        reference_thickness : float, default=25.0
            Reference thickness in mm
        thickness_exponent : float, default=0.25
            Thickness correction exponent

        Returns
        -------
        PowerLawSNCurve
            Thickness-corrected S-N curve
        """
        if actual_thickness <= 0:
            raise ValueError("Thickness must be positive")

        # Thickness correction factor
        thickness_factor = (actual_thickness / reference_thickness) ** thickness_exponent

        # Apply to fatigue strength coefficient
        corrected_A = base_curve.A / (thickness_factor ** base_curve.m)

        # Create corrected curve
        corrected_name = f"{base_curve.name}_t{actual_thickness}mm"

        return PowerLawSNCurve(
            name=corrected_name,
            A=corrected_A,
            m=base_curve.m,
            fatigue_limit=base_curve.fatigue_limit / thickness_factor,
            cutoff_cycles=base_curve.cutoff_cycles,
            material=base_curve.material
        )


class SNDataFitting:
    """
    S-N curve fitting from experimental data

    Supports various fitting methods and statistical analysis.
    """

    @staticmethod
    def fit_power_law(test_data: pd.DataFrame,
                     stress_col: str = 'stress_range',
                     cycles_col: str = 'cycles_to_failure') -> Tuple[PowerLawSNCurve, Dict]:
        """
        Fit power law S-N curve to test data

        Parameters
        ----------
        test_data : pd.DataFrame
            Test data with stress ranges and cycles to failure
        stress_col : str, default='stress_range'
            Column name for stress ranges
        cycles_col : str, default='cycles_to_failure'
            Column name for cycles to failure

        Returns
        -------
        curve : PowerLawSNCurve
            Fitted S-N curve
        statistics : dict
            Fitting statistics
        """
        if len(test_data) < 3:
            raise ValueError("Need at least 3 data points for curve fitting")

        # Extract data
        S = test_data[stress_col].values
        N = test_data[cycles_col].values

        # Remove invalid data
        valid_mask = (S > 0) & (N > 0) & np.isfinite(S) & np.isfinite(N)
        S = S[valid_mask]
        N = N[valid_mask]

        if len(S) < 3:
            raise ValueError("Insufficient valid data points for fitting")

        # Log-log linear regression: log(N) = log(A) - m * log(S)
        log_S = np.log10(S)
        log_N = np.log10(N)

        # Fit line
        coeffs = np.polyfit(log_S, log_N, 1)
        m = -coeffs[0]  # Slope (negative because N decreases with S)
        log_A = coeffs[1]
        A = 10 ** log_A

        # Calculate statistics
        log_N_predicted = log_A - m * log_S
        residuals = log_N - log_N_predicted

        r_squared = 1 - np.sum(residuals**2) / np.sum((log_N - np.mean(log_N))**2)
        rmse = np.sqrt(np.mean(residuals**2))

        # Estimate fatigue limit (if data spans wide enough range)
        if np.max(N) / np.min(N) > 100:
            # Use 10% quantile stress as rough fatigue limit estimate
            fatigue_limit = np.percentile(S, 10)
        else:
            fatigue_limit = 0.0

        # Create fitted curve
        curve = PowerLawSNCurve(
            name="Fitted_Curve",
            A=A,
            m=m,
            fatigue_limit=fatigue_limit
        )

        statistics = {
            'r_squared': r_squared,
            'rmse_log_cycles': rmse,
            'n_points': len(S),
            'stress_range': (np.min(S), np.max(S)),
            'cycle_range': (np.min(N), np.max(N)),
            'fitted_parameters': {'A': A, 'm': m}
        }

        return curve, statistics


def plot_sn_curve(curve: SNCurveBase,
                  cycles_range: Tuple[float, float] = (1e3, 1e8),
                  n_points: int = 100,
                  log_scale: bool = True) -> Tuple[np.ndarray, np.ndarray]:
    """
    Generate data for plotting S-N curve

    Parameters
    ----------
    curve : SNCurveBase
        S-N curve to plot
    cycles_range : tuple, default=(1e3, 1e8)
        Range of cycles for plotting
    n_points : int, default=100
        Number of points to generate
    log_scale : bool, default=True
        Use logarithmic spacing

    Returns
    -------
    cycles : np.ndarray
        Cycle values
    stress : np.ndarray
        Corresponding stress range values
    """
    if log_scale:
        cycles = np.logspace(np.log10(cycles_range[0]), np.log10(cycles_range[1]), n_points)
    else:
        cycles = np.linspace(cycles_range[0], cycles_range[1], n_points)

    stress = curve.get_stress_range(cycles)

    # Remove infinite values for plotting
    finite_mask = np.isfinite(stress)

    return cycles[finite_mask], stress[finite_mask]


# Convenience functions for common use cases
def get_dnv_curve(curve_class: str,
                  thickness: Optional[float] = None) -> PowerLawSNCurve:
    """Get DNV curve with optional thickness correction"""
    curve = StandardSNCurves.get_curve('DNV', curve_class)

    if thickness and thickness != 25.0:
        curve = ThicknessCorrection.apply_thickness_effect(curve, thickness)

    return curve


def get_api_curve(curve_class: str) -> PowerLawSNCurve:
    """Get API RP 2A curve"""
    return StandardSNCurves.get_curve('API', curve_class)


def get_bs_curve(curve_class: str) -> PowerLawSNCurve:
    """Get BS 7608 curve"""
    return StandardSNCurves.get_curve('BS', curve_class)


if __name__ == "__main__":
    # Example usage and testing
    import matplotlib.pyplot as plt

    # Create some standard curves
    dnv_d = get_dnv_curve('D')
    api_x = get_api_curve('X')
    bs_d = get_bs_curve('D')

    # Generate plot data
    cycles, stress_dnv = plot_sn_curve(dnv_d)
    _, stress_api = plot_sn_curve(api_x)
    _, stress_bs = plot_sn_curve(bs_d)

    # Plot comparison
    plt.figure(figsize=(10, 6))
    plt.loglog(cycles, stress_dnv, 'b-', label='DNV-D')
    plt.loglog(cycles, stress_api, 'r--', label='API-X')
    plt.loglog(cycles, stress_bs, 'g:', label='BS-D')

    plt.xlabel('Cycles to Failure')
    plt.ylabel('Stress Range (MPa)')
    plt.title('Comparison of Standard S-N Curves')
    plt.legend()
    plt.grid(True, alpha=0.3)
    plt.show()

    print("S-N Curves module test completed successfully!")