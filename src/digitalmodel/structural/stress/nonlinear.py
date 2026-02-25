"""
Nonlinear Stress Analysis Module

This module provides comprehensive nonlinear stress analysis capabilities
including plasticity models, hardening behavior, and iterative solution methods.
"""

import math
import numpy as np
from typing import Dict, List, Optional, Tuple, Union, Callable, Protocol
from dataclasses import dataclass, field
from abc import ABC, abstractmethod
from enum import Enum
import logging

# Optional scipy dependencies
try:
    from scipy.optimize import fsolve, newton, minimize_scalar
    HAS_SCIPY = True
except ImportError:
    HAS_SCIPY = False
    logger = logging.getLogger(__name__)
    logger.warning("SciPy not available. Some advanced optimization features will be disabled.")

logger = logging.getLogger(__name__)


class PlasticityModel(Enum):
    """Types of plasticity models"""
    VON_MISES = "von_mises"
    TRESCA = "tresca"
    MOHR_COULOMB = "mohr_coulomb"
    DRUCKER_PRAGER = "drucker_prager"


class HardeningType(Enum):
    """Types of hardening behavior"""
    ISOTROPIC = "isotropic"
    KINEMATIC = "kinematic"
    COMBINED = "combined"
    PERFECT_PLASTIC = "perfect_plastic"


@dataclass
class PlasticState:
    """Represents plastic state variables"""
    equivalent_plastic_strain: float = 0.0
    plastic_strain_tensor: Optional[np.ndarray] = None
    hardening_parameter: float = 0.0
    yield_stress: float = 0.0

    def __post_init__(self):
        if self.plastic_strain_tensor is None:
            self.plastic_strain_tensor = np.zeros(6)  # Voigt notation


@dataclass
class LoadStep:
    """Single load step for incremental analysis"""
    step_number: int
    load_factor: float
    converged: bool = False
    iterations: int = 0
    residual: float = float('inf')


@dataclass
class NonlinearSolution:
    """Results from nonlinear analysis"""
    load_steps: List[LoadStep] = field(default_factory=list)
    stress_history: List[np.ndarray] = field(default_factory=list)
    strain_history: List[np.ndarray] = field(default_factory=list)
    plastic_strain_history: List[np.ndarray] = field(default_factory=list)
    convergence_history: List[float] = field(default_factory=list)
    final_stress: Optional[np.ndarray] = None
    final_strain: Optional[np.ndarray] = None
    max_iterations_reached: bool = False


class YieldCriterion(ABC):
    """Abstract base class for yield criteria"""

    @abstractmethod
    def evaluate(self, stress: np.ndarray, hardening_stress: float = 0.0) -> float:
        """Evaluate yield function"""
        pass

    @abstractmethod
    def derivative(self, stress: np.ndarray) -> np.ndarray:
        """Calculate derivative of yield function with respect to stress"""
        pass


class VonMisesYield(YieldCriterion):
    """Von Mises yield criterion"""

    def __init__(self, yield_stress: float):
        """
        Initialize Von Mises yield criterion

        Args:
            yield_stress: Initial yield stress (Pa)
        """
        self.yield_stress = yield_stress

    def evaluate(self, stress: np.ndarray, hardening_stress: float = 0.0) -> float:
        """
        Evaluate Von Mises yield function

        Args:
            stress: Stress tensor in Voigt notation [σxx, σyy, σzz, τxy, τyz, τzx]
            hardening_stress: Additional stress due to hardening

        Returns:
            Yield function value
        """
        if len(stress) != 6:
            raise ValueError("Stress must be 6-component vector in Voigt notation")

        # Von Mises stress calculation
        s11, s22, s33, s12, s23, s13 = stress

        von_mises = math.sqrt(0.5 * ((s11 - s22)**2 + (s22 - s33)**2 + (s33 - s11)**2 +
                                   6 * (s12**2 + s23**2 + s13**2)))

        current_yield_stress = self.yield_stress + hardening_stress

        return von_mises - current_yield_stress

    def derivative(self, stress: np.ndarray) -> np.ndarray:
        """
        Calculate derivative of Von Mises yield function

        Args:
            stress: Stress tensor in Voigt notation

        Returns:
            Derivative vector
        """
        if len(stress) != 6:
            raise ValueError("Stress must be 6-component vector in Voigt notation")

        s11, s22, s33, s12, s23, s13 = stress

        # Calculate Von Mises stress
        von_mises = math.sqrt(0.5 * ((s11 - s22)**2 + (s22 - s33)**2 + (s33 - s11)**2 +
                                   6 * (s12**2 + s23**2 + s13**2)))

        if von_mises < 1e-12:  # Avoid division by zero
            return np.zeros(6)

        # Derivative components
        df_ds = np.zeros(6)

        df_ds[0] = (2*s11 - s22 - s33) / (2 * von_mises)  # ∂f/∂σ11
        df_ds[1] = (2*s22 - s11 - s33) / (2 * von_mises)  # ∂f/∂σ22
        df_ds[2] = (2*s33 - s11 - s22) / (2 * von_mises)  # ∂f/∂σ33
        df_ds[3] = 3 * s12 / von_mises                      # ∂f/∂τ12
        df_ds[4] = 3 * s23 / von_mises                      # ∂f/∂τ23
        df_ds[5] = 3 * s13 / von_mises                      # ∂f/∂τ13

        return df_ds


class TrescaYield(YieldCriterion):
    """Tresca yield criterion (maximum shear stress)"""

    def __init__(self, yield_stress: float):
        """Initialize Tresca yield criterion"""
        self.yield_stress = yield_stress

    def evaluate(self, stress: np.ndarray, hardening_stress: float = 0.0) -> float:
        """Evaluate Tresca yield function"""
        if len(stress) != 6:
            raise ValueError("Stress must be 6-component vector in Voigt notation")

        # Convert to principal stresses
        principal_stresses = self._calculate_principal_stresses(stress)
        sigma1, sigma2, sigma3 = sorted(principal_stresses, reverse=True)

        # Maximum shear stress
        max_shear = (sigma1 - sigma3) / 2

        current_yield_stress = self.yield_stress + hardening_stress

        return max_shear - current_yield_stress / 2

    def derivative(self, stress: np.ndarray) -> np.ndarray:
        """Calculate derivative of Tresca yield function"""
        # Simplified implementation - in practice, this requires careful handling
        # of the non-smooth nature of the Tresca criterion
        logger.warning("Tresca derivative calculation is simplified")
        return np.ones(6) / 6  # Simplified approximation

    def _calculate_principal_stresses(self, stress: np.ndarray) -> np.ndarray:
        """Calculate principal stresses from Voigt notation"""
        # Construct stress tensor
        s11, s22, s33, s12, s23, s13 = stress
        stress_tensor = np.array([
            [s11, s12, s13],
            [s12, s22, s23],
            [s13, s23, s33]
        ])

        # Calculate eigenvalues (principal stresses)
        eigenvalues = np.linalg.eigvals(stress_tensor)
        return eigenvalues


class HardeningModel(ABC):
    """Abstract base class for hardening models"""

    @abstractmethod
    def calculate_hardening_stress(self, equivalent_plastic_strain: float) -> float:
        """Calculate hardening stress"""
        pass

    @abstractmethod
    def calculate_hardening_modulus(self, equivalent_plastic_strain: float) -> float:
        """Calculate hardening modulus"""
        pass


class IsotropicHardening(HardeningModel):
    """Isotropic hardening model"""

    def __init__(self, hardening_modulus: float, hardening_exponent: float = 1.0):
        """
        Initialize isotropic hardening

        Args:
            hardening_modulus: Hardening modulus (Pa)
            hardening_exponent: Hardening exponent for power law
        """
        self.hardening_modulus = hardening_modulus
        self.hardening_exponent = hardening_exponent

    def calculate_hardening_stress(self, equivalent_plastic_strain: float) -> float:
        """Calculate hardening stress using power law"""
        if equivalent_plastic_strain <= 0:
            return 0.0

        return self.hardening_modulus * (equivalent_plastic_strain ** self.hardening_exponent)

    def calculate_hardening_modulus(self, equivalent_plastic_strain: float) -> float:
        """Calculate instantaneous hardening modulus"""
        if equivalent_plastic_strain <= 0:
            return self.hardening_modulus

        return (self.hardening_modulus * self.hardening_exponent *
                (equivalent_plastic_strain ** (self.hardening_exponent - 1)))


class LinearHardening(HardeningModel):
    """Linear hardening model"""

    def __init__(self, hardening_modulus: float):
        """Initialize linear hardening"""
        self.hardening_modulus = hardening_modulus

    def calculate_hardening_stress(self, equivalent_plastic_strain: float) -> float:
        """Calculate hardening stress"""
        return self.hardening_modulus * equivalent_plastic_strain

    def calculate_hardening_modulus(self, equivalent_plastic_strain: float) -> float:
        """Calculate hardening modulus (constant for linear hardening)"""
        return self.hardening_modulus


class NonlinearStressAnalyzer:
    """
    Comprehensive nonlinear stress analyzer

    Provides methods for solving nonlinear stress problems using plasticity theory
    and iterative solution techniques.
    """

    def __init__(self, elastic_modulus: float, poisson_ratio: float,
                 yield_criterion: YieldCriterion,
                 hardening_model: Optional[HardeningModel] = None):
        """
        Initialize nonlinear stress analyzer

        Args:
            elastic_modulus: Elastic modulus (Pa)
            poisson_ratio: Poisson's ratio
            yield_criterion: Yield criterion to use
            hardening_model: Hardening model (optional)
        """
        self.elastic_modulus = elastic_modulus
        self.poisson_ratio = poisson_ratio
        self.yield_criterion = yield_criterion
        self.hardening_model = hardening_model

        # Calculate elastic constants
        self._calculate_elastic_matrix()

        # Solution parameters
        self.max_iterations = 100
        self.tolerance = 1e-6
        self.load_increment_size = 0.1

    def _calculate_elastic_matrix(self) -> None:
        """Calculate elastic constitutive matrix"""
        E = self.elastic_modulus
        nu = self.poisson_ratio

        # Elastic constants
        lambda_lame = (E * nu) / ((1 + nu) * (1 - 2*nu))
        mu = E / (2 * (1 + nu))

        # Elastic constitutive matrix (6x6 for Voigt notation)
        self.elastic_matrix = np.zeros((6, 6))

        # Diagonal terms
        self.elastic_matrix[0, 0] = lambda_lame + 2*mu  # C11
        self.elastic_matrix[1, 1] = lambda_lame + 2*mu  # C22
        self.elastic_matrix[2, 2] = lambda_lame + 2*mu  # C33
        self.elastic_matrix[3, 3] = mu                   # C44
        self.elastic_matrix[4, 4] = mu                   # C55
        self.elastic_matrix[5, 5] = mu                   # C66

        # Off-diagonal terms
        self.elastic_matrix[0, 1] = lambda_lame  # C12
        self.elastic_matrix[0, 2] = lambda_lame  # C13
        self.elastic_matrix[1, 0] = lambda_lame  # C21
        self.elastic_matrix[1, 2] = lambda_lame  # C23
        self.elastic_matrix[2, 0] = lambda_lame  # C31
        self.elastic_matrix[2, 1] = lambda_lame  # C32

    def solve_incremental_plasticity(self, total_strain: np.ndarray,
                                   num_increments: int = 10) -> NonlinearSolution:
        """
        Solve nonlinear problem using incremental plasticity

        Args:
            total_strain: Total strain to apply
            num_increments: Number of load increments

        Returns:
            NonlinearSolution object with results
        """
        solution = NonlinearSolution()

        # Initialize state variables
        current_stress = np.zeros(6)
        current_plastic_strain = np.zeros(6)
        equivalent_plastic_strain = 0.0

        # Strain increment
        strain_increment = total_strain / num_increments

        for step in range(num_increments):
            load_step = LoadStep(step_number=step, load_factor=(step + 1) / num_increments)

            # Trial stress (elastic prediction)
            trial_stress = current_stress + np.dot(self.elastic_matrix, strain_increment)

            # Check yield condition
            hardening_stress = 0.0
            if self.hardening_model:
                hardening_stress = self.hardening_model.calculate_hardening_stress(
                    equivalent_plastic_strain
                )

            yield_value = self.yield_criterion.evaluate(trial_stress, hardening_stress)

            if yield_value <= self.tolerance:
                # Elastic step
                current_stress = trial_stress
                load_step.converged = True
                load_step.iterations = 1
                load_step.residual = 0.0

            else:
                # Plastic step - need return mapping
                result = self._return_mapping(trial_stress, current_plastic_strain,
                                            equivalent_plastic_strain, strain_increment)

                current_stress = result['stress']
                current_plastic_strain = result['plastic_strain']
                equivalent_plastic_strain = result['equivalent_plastic_strain']
                load_step.converged = result['converged']
                load_step.iterations = result['iterations']
                load_step.residual = result['residual']

            # Store results
            solution.load_steps.append(load_step)
            solution.stress_history.append(current_stress.copy())
            solution.strain_history.append(
                strain_increment * (step + 1) + current_plastic_strain
            )
            solution.plastic_strain_history.append(current_plastic_strain.copy())
            solution.convergence_history.append(load_step.residual)

            if not load_step.converged:
                logger.warning(f"Step {step} did not converge")

        # Set final results
        solution.final_stress = current_stress
        solution.final_strain = total_strain + current_plastic_strain

        return solution

    def _return_mapping(self, trial_stress: np.ndarray, plastic_strain: np.ndarray,
                       equivalent_plastic_strain: float, strain_increment: np.ndarray) -> Dict:
        """
        Perform return mapping algorithm for plastic correction

        Args:
            trial_stress: Trial stress state
            plastic_strain: Current plastic strain
            equivalent_plastic_strain: Current equivalent plastic strain
            strain_increment: Applied strain increment

        Returns:
            Dictionary with corrected state variables
        """
        # Initial guess for plastic multiplier
        delta_lambda = 0.0

        for iteration in range(self.max_iterations):
            # Current hardening stress
            hardening_stress = 0.0
            hardening_modulus = 0.0
            if self.hardening_model:
                hardening_stress = self.hardening_model.calculate_hardening_stress(
                    equivalent_plastic_strain
                )
                hardening_modulus = self.hardening_model.calculate_hardening_modulus(
                    equivalent_plastic_strain
                )

            # Flow direction (derivative of yield function)
            flow_direction = self.yield_criterion.derivative(trial_stress)

            # Updated stress
            stress_correction = delta_lambda * np.dot(self.elastic_matrix, flow_direction)
            current_stress = trial_stress - stress_correction

            # Yield function residual
            yield_residual = self.yield_criterion.evaluate(current_stress, hardening_stress)

            if abs(yield_residual) < self.tolerance:
                # Converged
                new_plastic_strain = plastic_strain + delta_lambda * flow_direction
                new_equivalent_plastic_strain = equivalent_plastic_strain + delta_lambda

                return {
                    'stress': current_stress,
                    'plastic_strain': new_plastic_strain,
                    'equivalent_plastic_strain': new_equivalent_plastic_strain,
                    'converged': True,
                    'iterations': iteration + 1,
                    'residual': abs(yield_residual)
                }

            # Newton-Raphson update
            # Calculate derivative of residual with respect to delta_lambda
            elastic_flow = np.dot(self.elastic_matrix, flow_direction)
            denominator = np.dot(flow_direction, elastic_flow) + hardening_modulus

            if abs(denominator) < 1e-12:
                logger.warning("Near-singular matrix in return mapping")
                break

            delta_lambda_correction = yield_residual / denominator
            delta_lambda += delta_lambda_correction

            if delta_lambda < 0:
                delta_lambda = 0.0

        # Failed to converge
        logger.warning("Return mapping failed to converge")
        return {
            'stress': trial_stress,
            'plastic_strain': plastic_strain,
            'equivalent_plastic_strain': equivalent_plastic_strain,
            'converged': False,
            'iterations': self.max_iterations,
            'residual': abs(yield_residual)
        }

    def calculate_plastic_work(self, solution: NonlinearSolution) -> float:
        """
        Calculate total plastic work from solution

        Args:
            solution: Nonlinear solution object

        Returns:
            Total plastic work (J/m³)
        """
        plastic_work = 0.0

        for i in range(1, len(solution.stress_history)):
            # Average stress in increment
            stress_avg = (solution.stress_history[i] + solution.stress_history[i-1]) / 2

            # Plastic strain increment
            plastic_strain_increment = (solution.plastic_strain_history[i] -
                                      solution.plastic_strain_history[i-1])

            # Plastic work increment
            plastic_work += np.dot(stress_avg, plastic_strain_increment)

        return plastic_work

    def predict_failure(self, solution: NonlinearSolution,
                       failure_strain: float) -> Optional[int]:
        """
        Predict failure based on maximum strain criterion

        Args:
            solution: Nonlinear solution object
            failure_strain: Failure strain threshold

        Returns:
            Load step at which failure occurs (or None)
        """
        for i, strain in enumerate(solution.strain_history):
            equivalent_strain = np.sqrt(np.dot(strain, strain))
            if equivalent_strain >= failure_strain:
                return i

        return None


def calculate_nonlinear_response(stress_history: List[np.ndarray],
                               elastic_modulus: float,
                               yield_stress: float,
                               hardening_modulus: float = 0.0) -> Dict[str, List[float]]:
    """
    Convenience function to calculate nonlinear stress-strain response

    Args:
        stress_history: List of stress states
        elastic_modulus: Elastic modulus (Pa)
        yield_stress: Yield stress (Pa)
        hardening_modulus: Hardening modulus (Pa)

    Returns:
        Dictionary with strain history and other results
    """
    # Create analyzer
    yield_criterion = VonMisesYield(yield_stress)
    hardening_model = LinearHardening(hardening_modulus) if hardening_modulus > 0 else None

    analyzer = NonlinearStressAnalyzer(
        elastic_modulus=elastic_modulus,
        poisson_ratio=0.3,
        yield_criterion=yield_criterion,
        hardening_model=hardening_model
    )

    # Calculate equivalent strain for each stress state
    strain_history = []
    for stress in stress_history:
        # Simplified: assume uniaxial stress for this convenience function
        equivalent_stress = abs(stress[0])  # First component

        if equivalent_stress <= yield_stress:
            # Elastic
            strain = equivalent_stress / elastic_modulus
        else:
            # Plastic
            elastic_strain = yield_stress / elastic_modulus
            plastic_stress = equivalent_stress - yield_stress
            plastic_strain = plastic_stress / hardening_modulus if hardening_modulus > 0 else 0
            strain = elastic_strain + plastic_strain

        strain_history.append(strain)

    return {
        'strains': strain_history,
        'stresses': [stress[0] for stress in stress_history],
        'plastic_strains': [max(0, s - yield_stress/elastic_modulus) for s in strain_history]
    }


def solve_nonlinear_stress(applied_strain: float, elastic_modulus: float,
                          yield_stress: float, hardening_modulus: float = 0.0,
                          num_increments: int = 10) -> Dict[str, float]:
    """
    Solve nonlinear stress problem for given strain

    Args:
        applied_strain: Applied strain
        elastic_modulus: Elastic modulus (Pa)
        yield_stress: Yield stress (Pa)
        hardening_modulus: Hardening modulus (Pa)
        num_increments: Number of increments

    Returns:
        Dictionary with final stress and strain values
    """
    # Create analyzer
    yield_criterion = VonMisesYield(yield_stress)
    hardening_model = LinearHardening(hardening_modulus) if hardening_modulus > 0 else None

    analyzer = NonlinearStressAnalyzer(
        elastic_modulus=elastic_modulus,
        poisson_ratio=0.3,
        yield_criterion=yield_criterion,
        hardening_model=hardening_model
    )

    # Apply uniaxial strain
    total_strain = np.array([applied_strain, 0, 0, 0, 0, 0])

    # Solve
    solution = analyzer.solve_incremental_plasticity(total_strain, num_increments)

    return {
        'final_stress': solution.final_stress[0] if solution.final_stress is not None else 0,
        'final_strain': solution.final_strain[0] if solution.final_strain is not None else 0,
        'plastic_strain': solution.plastic_strain_history[-1][0] if solution.plastic_strain_history else 0,
        'converged': all(step.converged for step in solution.load_steps)
    }


# Example usage
if __name__ == "__main__":
    # Material properties
    E = 2.1e11  # Pa
    yield_stress = 250e6  # Pa
    hardening_modulus = 2.1e9  # Pa (1% of E)

    # Create analyzer
    yield_criterion = VonMisesYield(yield_stress)
    hardening_model = LinearHardening(hardening_modulus)

    analyzer = NonlinearStressAnalyzer(
        elastic_modulus=E,
        poisson_ratio=0.3,
        yield_criterion=yield_criterion,
        hardening_model=hardening_model
    )

    # Apply uniaxial strain beyond yield
    applied_strain = 0.005  # 0.5% strain
    total_strain = np.array([applied_strain, 0, 0, 0, 0, 0])

    # Solve nonlinear problem
    solution = analyzer.solve_incremental_plasticity(total_strain, num_increments=10)

    print("Nonlinear Analysis Results:")
    print(f"Final stress: {solution.final_stress[0]:.2e} Pa")
    print(f"Final total strain: {solution.final_strain[0]:.6f}")
    print(f"Final plastic strain: {solution.plastic_strain_history[-1][0]:.6f}")
    print(f"All steps converged: {all(step.converged for step in solution.load_steps)}")

    # Calculate plastic work
    plastic_work = analyzer.calculate_plastic_work(solution)
    print(f"Plastic work: {plastic_work:.2e} J/m³")