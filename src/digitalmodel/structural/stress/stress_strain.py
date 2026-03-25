"""
Stress-Strain Analysis Module

This module provides comprehensive stress-strain curve analysis including
linear elastic and nonlinear models. Migrated and modernized from legacy
stress-strain calculation code.
"""

import math
import numpy as np
from typing import Dict, List, Optional, Tuple, Union, Callable, Protocol
from dataclasses import dataclass, field
from abc import ABC, abstractmethod
from enum import Enum
import logging

logger = logging.getLogger(__name__)

# Optional scipy dependencies
try:
    from scipy.optimize import curve_fit, minimize_scalar, minimize
    from scipy.interpolate import interp1d
    HAS_SCIPY = True
except ImportError:
    HAS_SCIPY = False
    logger.warning("SciPy not available. Some advanced fitting features will be disabled.")


class MaterialModel(Enum):
    """Types of material models"""
    LINEAR_ELASTIC = "linear_elastic"
    RAMBERG_OSGOOD = "ramberg_osgood"
    BILINEAR = "bilinear"
    MULTILINEAR = "multilinear"
    POWER_LAW = "power_law"


@dataclass
class MaterialProperties:
    """Enhanced material properties for stress-strain analysis"""
    elastic_modulus: float  # Pa
    yield_strength: float  # Pa
    ultimate_strength: Optional[float] = None  # Pa
    poisson_ratio: float = 0.3
    density: Optional[float] = None  # kg/m³

    # Ramberg-Osgood parameters
    ramberg_osgood_k: Optional[float] = None
    ramberg_osgood_n: Optional[float] = None

    # Additional properties
    proportional_limit: Optional[float] = None  # Pa
    fracture_strain: Optional[float] = None
    strain_hardening_exponent: Optional[float] = None

    def __post_init__(self):
        self.validate()

    def validate(self) -> None:
        """Validate material properties"""
        if self.elastic_modulus <= 0:
            raise ValueError("Elastic modulus must be positive")
        if self.yield_strength <= 0:
            raise ValueError("Yield strength must be positive")
        if self.ultimate_strength is not None and self.ultimate_strength < self.yield_strength:
            raise ValueError("Ultimate strength must be >= yield strength")
        if not 0 <= self.poisson_ratio <= 0.5:
            raise ValueError("Poisson ratio must be between 0 and 0.5")


@dataclass
class StressStrainPoint:
    """Single point on stress-strain curve"""
    strain: float
    stress: float

    def validate(self) -> None:
        """Validate stress-strain point"""
        if math.isnan(self.strain) or math.isnan(self.stress):
            raise ValueError("Strain and stress must be valid numbers")
        if self.strain < 0:
            raise ValueError("Strain must be non-negative")


@dataclass
class StressStrainCurve:
    """Complete stress-strain curve data"""
    strains: List[float] = field(default_factory=list)
    stresses: List[float] = field(default_factory=list)
    material_model: Optional[MaterialModel] = None
    fitted_parameters: Optional[Dict[str, float]] = None

    def __post_init__(self):
        self.validate()

    def validate(self) -> None:
        """Validate stress-strain curve data"""
        if len(self.strains) != len(self.stresses):
            raise ValueError("Strain and stress arrays must have same length")
        if len(self.strains) < 2:
            raise ValueError("Curve must have at least 2 points")

        # Check for monotonic strain increase
        for i in range(1, len(self.strains)):
            if self.strains[i] <= self.strains[i-1]:
                raise ValueError("Strains must be monotonically increasing")

    def add_point(self, strain: float, stress: float) -> None:
        """Add a point to the curve"""
        point = StressStrainPoint(strain, stress)
        point.validate()

        # Insert in order
        for i, existing_strain in enumerate(self.strains):
            if strain < existing_strain:
                self.strains.insert(i, strain)
                self.stresses.insert(i, stress)
                return

        # Add at end
        self.strains.append(strain)
        self.stresses.append(stress)

    def get_stress_at_strain(self, strain: float) -> float:
        """Interpolate stress at given strain"""
        if strain < min(self.strains) or strain > max(self.strains):
            raise ValueError("Strain outside curve range")

        if HAS_SCIPY:
            interp_func = interp1d(self.strains, self.stresses, kind='linear')
            return float(interp_func(strain))
        else:
            # Simple linear interpolation without scipy
            return float(np.interp(strain, self.strains, self.stresses))

    def get_elastic_modulus(self) -> float:
        """Calculate elastic modulus from initial slope"""
        if len(self.strains) < 2:
            raise ValueError("Need at least 2 points to calculate modulus")

        # Use first few points for linear region
        n_points = min(5, len(self.strains))
        strains_subset = self.strains[:n_points]
        stresses_subset = self.stresses[:n_points]

        # Linear fit
        coeffs = np.polyfit(strains_subset, stresses_subset, 1)
        return coeffs[0]  # Slope


class StressStrainModel(ABC):
    """Abstract base class for stress-strain models"""

    @abstractmethod
    def calculate_stress(self, strain: float, **parameters) -> float:
        """Calculate stress for given strain"""
        pass

    @abstractmethod
    def calculate_strain(self, stress: float, **parameters) -> float:
        """Calculate strain for given stress"""
        pass

    @abstractmethod
    def get_parameter_names(self) -> List[str]:
        """Get list of model parameter names"""
        pass


class LinearElasticModel(StressStrainModel):
    """Linear elastic stress-strain model"""

    def calculate_stress(self, strain: float, elastic_modulus: float = None, **kwargs) -> float:
        """Calculate stress using Hooke's law"""
        if elastic_modulus is None:
            raise ValueError("Elastic modulus required")
        return elastic_modulus * strain

    def calculate_strain(self, stress: float, elastic_modulus: float = None, **kwargs) -> float:
        """Calculate strain using Hooke's law"""
        if elastic_modulus is None:
            raise ValueError("Elastic modulus required")
        return stress / elastic_modulus

    def get_parameter_names(self) -> List[str]:
        return ["elastic_modulus"]


class RambergOsgoodModel(StressStrainModel):
    """
    Ramberg-Osgood stress-strain model

    Based on legacy code: e = (sigma/E) + K*(sigma/sigma_ys)^n
    """

    def calculate_strain(self, stress: float, elastic_modulus: float = None,
                        yield_strength: float = None, k: float = None,
                        n: float = None, **kwargs) -> float:
        """
        Calculate strain using Ramberg-Osgood equation

        Args:
            stress: Applied stress (Pa)
            elastic_modulus: Elastic modulus (Pa)
            yield_strength: Yield strength (Pa)
            k: Ramberg-Osgood parameter K
            n: Ramberg-Osgood parameter n

        Returns:
            Total strain
        """
        required_params = [elastic_modulus, yield_strength, k, n]
        if any(param is None for param in required_params):
            raise ValueError("All Ramberg-Osgood parameters required")

        # Elastic strain
        elastic_strain = stress / elastic_modulus

        # Plastic strain
        plastic_strain = k * (stress / yield_strength) ** n

        return elastic_strain + plastic_strain

    def calculate_stress(self, strain: float, elastic_modulus: float = None,
                        yield_strength: float = None, k: float = None,
                        n: float = None, **kwargs) -> float:
        """
        Calculate stress for given strain (requires iterative solution)
        """
        required_params = [elastic_modulus, yield_strength, k, n]
        if any(param is None for param in required_params):
            raise ValueError("All Ramberg-Osgood parameters required")

        def strain_error(stress):
            calculated_strain = self.calculate_strain(
                stress, elastic_modulus, yield_strength, k, n
            )
            return abs(calculated_strain - strain)

        # Initial guess - elastic solution
        initial_stress = elastic_modulus * strain

        if HAS_SCIPY:
            # Bound the search
            max_stress = max(yield_strength * 2, initial_stress * 2)

            result = minimize_scalar(strain_error, bounds=(0, max_stress), method='bounded')

            if not result.success:
                logger.warning("Ramberg-Osgood stress calculation did not converge")

            return result.x
        else:
            # Simple iterative solution without scipy
            stress_guess = initial_stress
            for _ in range(50):  # Maximum iterations
                calculated_strain = self.calculate_strain(
                    stress_guess, elastic_modulus, yield_strength, k, n
                )
                error = abs(calculated_strain - strain)
                if error < 1e-8:
                    break

                # Simple Newton-like update
                if calculated_strain > strain:
                    stress_guess *= 0.9
                else:
                    stress_guess *= 1.1

                # Prevent negative stress
                stress_guess = max(0, stress_guess)

            return stress_guess

    def get_parameter_names(self) -> List[str]:
        return ["elastic_modulus", "yield_strength", "k", "n"]


class BilinearModel(StressStrainModel):
    """Bilinear elastic-plastic stress-strain model"""

    def calculate_stress(self, strain: float, elastic_modulus: float = None,
                        yield_strength: float = None, hardening_modulus: float = None,
                        **kwargs) -> float:
        """Calculate stress using bilinear model"""
        required_params = [elastic_modulus, yield_strength]
        if any(param is None for param in required_params):
            raise ValueError("Elastic modulus and yield strength required")

        yield_strain = yield_strength / elastic_modulus

        if strain <= yield_strain:
            # Elastic region
            return elastic_modulus * strain
        else:
            # Plastic region
            if hardening_modulus is None:
                return yield_strength  # Perfect plasticity
            else:
                plastic_strain = strain - yield_strain
                return yield_strength + hardening_modulus * plastic_strain

    def calculate_strain(self, stress: float, elastic_modulus: float = None,
                        yield_strength: float = None, hardening_modulus: float = None,
                        **kwargs) -> float:
        """Calculate strain using bilinear model"""
        required_params = [elastic_modulus, yield_strength]
        if any(param is None for param in required_params):
            raise ValueError("Elastic modulus and yield strength required")

        if stress <= yield_strength:
            # Elastic region
            return stress / elastic_modulus
        else:
            # Plastic region
            yield_strain = yield_strength / elastic_modulus
            if hardening_modulus is None or hardening_modulus == 0:
                raise ValueError("Cannot determine strain in perfect plastic region")
            else:
                plastic_stress = stress - yield_strength
                plastic_strain = plastic_stress / hardening_modulus
                return yield_strain + plastic_strain

    def get_parameter_names(self) -> List[str]:
        return ["elastic_modulus", "yield_strength", "hardening_modulus"]


class StressStrainAnalyzer:
    """
    Comprehensive stress-strain analyzer

    Provides methods for analyzing stress-strain behavior using various models
    and fitting experimental data.
    """

    def __init__(self, material: Optional[MaterialProperties] = None):
        """Initialize analyzer with optional material properties"""
        self.material = material
        self.models = {
            MaterialModel.LINEAR_ELASTIC: LinearElasticModel(),
            MaterialModel.RAMBERG_OSGOOD: RambergOsgoodModel(),
            MaterialModel.BILINEAR: BilinearModel()
        }

    def generate_curve(self, model: MaterialModel, strain_range: np.ndarray,
                      **parameters) -> StressStrainCurve:
        """
        Generate stress-strain curve using specified model

        Args:
            model: Material model to use
            strain_range: Array of strain values
            **parameters: Model parameters

        Returns:
            StressStrainCurve object
        """
        if model not in self.models:
            raise ValueError(f"Unsupported model: {model}")

        stress_model = self.models[model]
        stresses = []

        for strain in strain_range:
            try:
                stress = stress_model.calculate_stress(strain, **parameters)
                stresses.append(stress)
            except Exception as e:
                logger.warning(f"Error calculating stress at strain {strain}: {e}")
                stresses.append(np.nan)

        # Filter out NaN values
        valid_indices = ~np.isnan(stresses)
        valid_strains = strain_range[valid_indices].tolist()
        valid_stresses = np.array(stresses)[valid_indices].tolist()

        curve = StressStrainCurve(
            strains=valid_strains,
            stresses=valid_stresses,
            material_model=model,
            fitted_parameters=parameters
        )

        return curve

    def fit_ramberg_osgood(self, experimental_data: StressStrainCurve) -> Dict[str, float]:
        """
        Fit Ramberg-Osgood parameters to experimental data

        Based on legacy code implementation

        Args:
            experimental_data: Experimental stress-strain data

        Returns:
            Dictionary of fitted parameters
        """
        strains = np.array(experimental_data.strains)
        stresses = np.array(experimental_data.stresses)

        # Estimate initial parameters
        elastic_modulus = experimental_data.get_elastic_modulus()

        # Estimate yield strength as stress at 0.2% offset
        offset_strain = 0.002
        offset_line_stresses = elastic_modulus * (strains + offset_strain)

        # Find intersection (approximate yield strength)
        yield_idx = np.argmin(np.abs(stresses - offset_line_stresses))
        yield_strength = stresses[yield_idx]

        def ramberg_osgood_residual(params):
            """Residual function for curve fitting"""
            k, n = params

            predicted_strains = []
            ro_model = RambergOsgoodModel()

            for stress in stresses:
                try:
                    predicted_strain = ro_model.calculate_strain(
                        stress, elastic_modulus, yield_strength, k, n
                    )
                    predicted_strains.append(predicted_strain)
                except:
                    predicted_strains.append(np.inf)

            predicted_strains = np.array(predicted_strains)
            residual = np.sum((strains - predicted_strains) ** 2)

            return residual

        # Initial guess for k and n
        initial_guess = [0.002, 18.85]  # From legacy code

        if HAS_SCIPY:
            try:
                result = minimize(ramberg_osgood_residual, initial_guess,
                                bounds=[(1e-6, 1), (1, 50)], method='L-BFGS-B')

                if result.success:
                    k_fitted, n_fitted = result.x
                else:
                    logger.warning("Ramberg-Osgood fitting did not converge, using defaults")
                    k_fitted, n_fitted = initial_guess
            except Exception as e:
                logger.warning(f"Fitting failed: {e}, using default parameters")
                k_fitted, n_fitted = initial_guess
        else:
            logger.warning("SciPy not available, using default parameters")
            k_fitted, n_fitted = initial_guess

        return {
            'elastic_modulus': elastic_modulus,
            'yield_strength': yield_strength,
            'k': k_fitted,
            'n': n_fitted
        }

    def calculate_engineering_properties(self, curve: StressStrainCurve) -> Dict[str, float]:
        """
        Calculate engineering properties from stress-strain curve

        Args:
            curve: Stress-strain curve data

        Returns:
            Dictionary of engineering properties
        """
        strains = np.array(curve.strains)
        stresses = np.array(curve.stresses)

        properties = {}

        # Elastic modulus (initial slope)
        properties['elastic_modulus'] = curve.get_elastic_modulus()

        # Yield strength (0.2% offset method)
        if max(strains) >= 0.002:
            try:
                offset_strain = 0.002
                offset_stresses = properties['elastic_modulus'] * (strains + offset_strain)

                # Find intersection
                differences = np.abs(stresses - offset_stresses)
                yield_idx = np.argmin(differences)
                properties['yield_strength_02'] = stresses[yield_idx]
            except:
                properties['yield_strength_02'] = None

        # Ultimate tensile strength
        properties['ultimate_strength'] = max(stresses)
        properties['fracture_strain'] = strains[-1]

        # Proportional limit (deviation from linearity)
        try:
            linear_stresses = properties['elastic_modulus'] * strains
            deviations = np.abs(stresses - linear_stresses) / stresses
            prop_limit_idx = np.where(deviations > 0.01)[0]  # 1% deviation

            if len(prop_limit_idx) > 0:
                properties['proportional_limit'] = stresses[prop_limit_idx[0]]
            else:
                properties['proportional_limit'] = None
        except:
            properties['proportional_limit'] = None

        return properties

    def compare_models(self, experimental_data: StressStrainCurve,
                      models_to_compare: List[MaterialModel] = None) -> Dict[str, Dict]:
        """
        Compare different material models against experimental data

        Args:
            experimental_data: Experimental stress-strain data
            models_to_compare: List of models to compare

        Returns:
            Dictionary with model comparison results
        """
        if models_to_compare is None:
            models_to_compare = [MaterialModel.LINEAR_ELASTIC,
                               MaterialModel.RAMBERG_OSGOOD,
                               MaterialModel.BILINEAR]

        results = {}

        for model in models_to_compare:
            try:
                if model == MaterialModel.RAMBERG_OSGOOD:
                    params = self.fit_ramberg_osgood(experimental_data)
                elif model == MaterialModel.LINEAR_ELASTIC:
                    params = {'elastic_modulus': experimental_data.get_elastic_modulus()}
                elif model == MaterialModel.BILINEAR:
                    props = self.calculate_engineering_properties(experimental_data)
                    params = {
                        'elastic_modulus': props['elastic_modulus'],
                        'yield_strength': props.get('yield_strength_02', props['ultimate_strength'] * 0.8),
                        'hardening_modulus': props['elastic_modulus'] * 0.01  # Assume 1% of E
                    }
                else:
                    continue

                # Generate predicted curve
                strain_range = np.array(experimental_data.strains)
                predicted_curve = self.generate_curve(model, strain_range, **params)

                # Calculate fit quality (R-squared)
                exp_stresses = np.array(experimental_data.stresses)
                pred_stresses = np.array(predicted_curve.stresses)

                ss_res = np.sum((exp_stresses - pred_stresses) ** 2)
                ss_tot = np.sum((exp_stresses - np.mean(exp_stresses)) ** 2)
                r_squared = 1 - (ss_res / ss_tot) if ss_tot > 0 else 0

                results[model.value] = {
                    'parameters': params,
                    'r_squared': r_squared,
                    'predicted_curve': predicted_curve,
                    'rmse': np.sqrt(np.mean((exp_stresses - pred_stresses) ** 2))
                }

            except Exception as e:
                logger.warning(f"Error fitting {model.value}: {e}")
                results[model.value] = {'error': str(e)}

        return results


def calculate_stress_strain_curve(model: MaterialModel, strain_range: Union[List[float], np.ndarray],
                                **parameters) -> StressStrainCurve:
    """
    Convenience function to calculate stress-strain curve

    Args:
        model: Material model to use
        strain_range: Range of strain values
        **parameters: Model parameters

    Returns:
        StressStrainCurve object
    """
    analyzer = StressStrainAnalyzer()
    return analyzer.generate_curve(model, np.array(strain_range), **parameters)


def fit_stress_strain_data(strains: List[float], stresses: List[float],
                          model: MaterialModel = MaterialModel.RAMBERG_OSGOOD) -> Dict[str, float]:
    """
    Convenience function to fit stress-strain data

    Args:
        strains: Experimental strain data
        stresses: Experimental stress data
        model: Material model to fit

    Returns:
        Dictionary of fitted parameters
    """
    curve = StressStrainCurve(strains=strains, stresses=stresses)
    analyzer = StressStrainAnalyzer()

    if model == MaterialModel.RAMBERG_OSGOOD:
        return analyzer.fit_ramberg_osgood(curve)
    else:
        raise NotImplementedError(f"Fitting not implemented for {model}")


# Example usage based on legacy code
if __name__ == "__main__":
    # Legacy parameters from the original code
    elastic_modulus = 2.12e8  # Pa
    yield_strength = 8.27e5  # Pa
    k = 0.002
    n = 18.85

    # Create analyzer
    analyzer = StressStrainAnalyzer()

    # Generate strain range
    strain_range = np.linspace(0, 0.1, 100)

    # Generate Ramberg-Osgood curve
    curve = analyzer.generate_curve(
        MaterialModel.RAMBERG_OSGOOD,
        strain_range,
        elastic_modulus=elastic_modulus,
        yield_strength=yield_strength,
        k=k,
        n=n
    )

    print("Generated Ramberg-Osgood stress-strain curve")
    print(f"Number of points: {len(curve.strains)}")
    print(f"Max stress: {max(curve.stresses):.2e} Pa")
    print(f"Max strain: {max(curve.strains):.4f}")

    # Calculate engineering properties
    properties = analyzer.calculate_engineering_properties(curve)
    print("\nEngineering Properties:")
    for prop, value in properties.items():
        if value is not None:
            print(f"{prop}: {value:.2e}")
        else:
            print(f"{prop}: Not available")