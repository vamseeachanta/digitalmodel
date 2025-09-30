"""
Fatigue Damage Accumulation Models
=================================

This module implements various fatigue damage accumulation methods for
structural fatigue analysis, including linear and nonlinear damage theories.

Key Features:
- Palmgren-Miner linear damage rule
- Modified Miner's rule with load sequence effects
- Nonlinear damage models (Marco-Starkey, Corten-Dolan)
- Damage interaction models
- Mean stress corrections
- Block loading and variable amplitude analysis
- Fatigue life prediction and safety factors

Author: Digital Model Team
Version: 2.0.0
"""

import numpy as np
import pandas as pd
from typing import Dict, Optional, Union, Tuple, List, Literal, Callable
from dataclasses import dataclass, field
from abc import ABC, abstractmethod
import logging
from pathlib import Path
import json

from .sn_curves import SNCurveBase, MeanStressCorrection

logger = logging.getLogger(__name__)

# Type aliases
DamageResults = Dict[str, Union[float, int, List, Dict]]
CycleData = Union[pd.DataFrame, np.ndarray]


@dataclass
class LoadCycle:
    """Single load cycle with stress range and mean stress"""
    stress_range: float  # MPa
    mean_stress: float = 0.0  # MPa
    count: float = 1.0
    frequency: float = 1.0  # Hz
    temperature: float = 20.0  # Celsius
    environment: str = "air"


@dataclass
class DamageState:
    """Current damage state of a component"""
    total_damage: float = 0.0
    cycle_history: List[LoadCycle] = field(default_factory=list)
    damage_contributions: Dict[str, float] = field(default_factory=dict)
    crack_initiation_damage: float = 0.0
    crack_propagation_damage: float = 0.0
    temperature_effects: float = 1.0
    environment_effects: float = 1.0


class DamageAccumulationBase(ABC):
    """Base class for damage accumulation models"""

    def __init__(self, name: str):
        self.name = name
        self.damage_history: List[DamageResults] = []

    @abstractmethod
    def calculate_damage(self,
                        cycles: CycleData,
                        sn_curve: SNCurveBase,
                        **kwargs) -> DamageResults:
        """Calculate cumulative damage from load cycles"""
        pass

    def reset_history(self):
        """Reset damage calculation history"""
        self.damage_history.clear()


class LinearDamageAccumulation(DamageAccumulationBase):
    """
    Palmgren-Miner linear damage accumulation rule

    D = Σ(ni/Ni) where:
    - ni = number of cycles at stress level i
    - Ni = allowable cycles at stress level i
    - Failure occurs when D ≥ 1.0
    """

    def __init__(self,
                 failure_criterion: float = 1.0,
                 mean_stress_correction: Optional[str] = None):
        """
        Initialize linear damage accumulation

        Parameters
        ----------
        failure_criterion : float, default=1.0
            Damage value at which failure occurs
        mean_stress_correction : str, optional
            Mean stress correction method: 'goodman', 'gerber', 'soderberg'
        """
        super().__init__("Palmgren-Miner")
        self.failure_criterion = failure_criterion
        self.mean_stress_correction = mean_stress_correction

    def calculate_damage(self,
                        cycles: CycleData,
                        sn_curve: SNCurveBase,
                        **kwargs) -> DamageResults:
        """
        Calculate linear cumulative damage

        Parameters
        ----------
        cycles : pd.DataFrame or np.ndarray
            Load cycle data with columns: range, mean, count
        sn_curve : SNCurveBase
            S-N curve for damage calculation
        **kwargs
            Additional parameters

        Returns
        -------
        DamageResults
            Damage calculation results
        """
        # Convert input to DataFrame if needed
        if isinstance(cycles, np.ndarray):
            if cycles.ndim == 1:
                # Assume stress ranges only
                df = pd.DataFrame({
                    'range': cycles,
                    'mean': np.zeros_like(cycles),
                    'count': np.ones_like(cycles)
                })
            else:
                df = pd.DataFrame(cycles, columns=['range', 'mean', 'count'])
        else:
            df = cycles.copy()

        # Ensure required columns exist
        if 'range' not in df.columns:
            raise ValueError("Cycle data must contain 'range' column")
        if 'mean' not in df.columns:
            df['mean'] = 0.0
        if 'count' not in df.columns:
            df['count'] = 1.0

        # Apply mean stress correction if specified
        if self.mean_stress_correction and hasattr(sn_curve, 'material'):
            df = self._apply_mean_stress_correction(df, sn_curve)

        total_damage = 0.0
        damage_contributions = []
        cycles_processed = 0

        for idx, cycle in df.iterrows():
            stress_range = cycle['range']
            n_applied = cycle['count']

            if stress_range <= 0 or n_applied <= 0:
                continue

            # Get allowable cycles from S-N curve
            N_allowable = sn_curve.get_allowable_cycles(stress_range)

            # Calculate damage contribution
            if np.isfinite(N_allowable) and N_allowable > 0:
                damage_increment = n_applied / N_allowable
                total_damage += damage_increment

                damage_contributions.append({
                    'stress_range': stress_range,
                    'cycles_applied': n_applied,
                    'cycles_allowable': N_allowable,
                    'damage_increment': damage_increment,
                    'cumulative_damage': total_damage
                })

            cycles_processed += n_applied

        # Calculate derived metrics
        life_fraction_used = total_damage / self.failure_criterion
        life_fraction_remaining = max(0, 1 - life_fraction_used)
        safety_factor = self.failure_criterion / total_damage if total_damage > 0 else np.inf

        # Estimate remaining life
        if total_damage > 0 and cycles_processed > 0:
            damage_rate = total_damage / cycles_processed
            remaining_damage = self.failure_criterion - total_damage
            if remaining_damage > 0:
                remaining_cycles = remaining_damage / damage_rate
            else:
                remaining_cycles = 0
        else:
            remaining_cycles = np.inf

        results = {
            'method': self.name,
            'total_damage': total_damage,
            'life_fraction_used': life_fraction_used,
            'life_fraction_remaining': life_fraction_remaining,
            'safety_factor': safety_factor,
            'cycles_processed': cycles_processed,
            'estimated_remaining_cycles': remaining_cycles,
            'failure_criterion': self.failure_criterion,
            'damage_contributions': damage_contributions,
            'n_stress_levels': len(damage_contributions)
        }

        # Store in history
        self.damage_history.append(results)

        return results

    def _apply_mean_stress_correction(self,
                                    cycles_df: pd.DataFrame,
                                    sn_curve: SNCurveBase) -> pd.DataFrame:
        """Apply mean stress correction to cycle data"""
        corrected_df = cycles_df.copy()

        # Get material properties
        if not hasattr(sn_curve, 'material') or sn_curve.material is None:
            logger.warning("No material properties available for mean stress correction")
            return corrected_df

        material = sn_curve.material
        correction_method = self.mean_stress_correction.lower()

        for idx, cycle in corrected_df.iterrows():
            stress_amplitude = cycle['range'] / 2
            mean_stress = cycle['mean']

            if correction_method == 'goodman':
                corrected_amplitude = MeanStressCorrection.goodman(
                    stress_amplitude, mean_stress, material.ultimate_strength
                )
            elif correction_method == 'gerber':
                corrected_amplitude = MeanStressCorrection.gerber(
                    stress_amplitude, mean_stress, material.ultimate_strength
                )
            elif correction_method == 'soderberg':
                corrected_amplitude = MeanStressCorrection.soderberg(
                    stress_amplitude, mean_stress, material.yield_strength
                )
            else:
                logger.warning(f"Unknown mean stress correction: {correction_method}")
                continue

            # Update stress range
            corrected_df.loc[idx, 'range'] = 2 * corrected_amplitude

        return corrected_df


class ModifiedMinersRule(LinearDamageAccumulation):
    """
    Modified Miner's rule accounting for load sequence effects

    Applies different damage rates depending on accumulated damage level
    and loading sequence.
    """

    def __init__(self,
                 failure_criterion: float = 1.0,
                 damage_acceleration_threshold: float = 0.3,
                 acceleration_factor: float = 1.5,
                 sequence_sensitive: bool = True):
        """
        Initialize modified Miner's rule

        Parameters
        ----------
        failure_criterion : float, default=1.0
            Damage value at which failure occurs
        damage_acceleration_threshold : float, default=0.3
            Damage level above which acceleration occurs
        acceleration_factor : float, default=1.5
            Factor by which damage rate increases after threshold
        sequence_sensitive : bool, default=True
            Whether to consider load sequence effects
        """
        super().__init__(failure_criterion)
        self.name = "Modified Miner's Rule"
        self.acceleration_threshold = damage_acceleration_threshold
        self.acceleration_factor = acceleration_factor
        self.sequence_sensitive = sequence_sensitive

    def calculate_damage(self,
                        cycles: CycleData,
                        sn_curve: SNCurveBase,
                        **kwargs) -> DamageResults:
        """Calculate damage using modified Miner's rule"""
        # Convert to DataFrame
        if isinstance(cycles, np.ndarray):
            df = pd.DataFrame(cycles, columns=['range', 'mean', 'count'])
        else:
            df = cycles.copy()

        # Sort by stress range if sequence sensitive
        if self.sequence_sensitive:
            # High-to-low sequence is more damaging
            df = df.sort_values('range', ascending=False).reset_index(drop=True)

        total_damage = 0.0
        damage_contributions = []

        for idx, cycle in df.iterrows():
            stress_range = cycle['range']
            n_applied = cycle['count']

            if stress_range <= 0 or n_applied <= 0:
                continue

            # Get allowable cycles
            N_allowable = sn_curve.get_allowable_cycles(stress_range)

            if np.isfinite(N_allowable) and N_allowable > 0:
                # Calculate base damage increment
                base_damage = n_applied / N_allowable

                # Apply modification factor based on current damage level
                if total_damage > self.acceleration_threshold:
                    modification_factor = self.acceleration_factor
                else:
                    modification_factor = 1.0

                damage_increment = base_damage * modification_factor
                total_damage += damage_increment

                damage_contributions.append({
                    'stress_range': stress_range,
                    'cycles_applied': n_applied,
                    'cycles_allowable': N_allowable,
                    'base_damage': base_damage,
                    'modification_factor': modification_factor,
                    'damage_increment': damage_increment,
                    'cumulative_damage': total_damage
                })

        # Calculate results using parent class method, then modify
        results = super().calculate_damage(cycles, sn_curve, **kwargs)
        results['method'] = self.name
        results['total_damage'] = total_damage
        results['damage_contributions'] = damage_contributions
        results['modification_parameters'] = {
            'acceleration_threshold': self.acceleration_threshold,
            'acceleration_factor': self.acceleration_factor,
            'sequence_sensitive': self.sequence_sensitive
        }

        return results


class NonlinearDamageAccumulation(DamageAccumulationBase):
    """
    Nonlinear damage accumulation models

    Implements various nonlinear theories including Marco-Starkey
    and Corten-Dolan models.
    """

    def __init__(self,
                 model: Literal['marco_starkey', 'corten_dolan', 'double_linear'] = 'marco_starkey',
                 model_parameters: Optional[Dict] = None):
        """
        Initialize nonlinear damage model

        Parameters
        ----------
        model : str, default='marco_starkey'
            Nonlinear model type
        model_parameters : dict, optional
            Model-specific parameters
        """
        super().__init__(f"Nonlinear-{model}")
        self.model = model
        self.parameters = model_parameters or {}

    def calculate_damage(self,
                        cycles: CycleData,
                        sn_curve: SNCurveBase,
                        **kwargs) -> DamageResults:
        """Calculate nonlinear cumulative damage"""
        if isinstance(cycles, np.ndarray):
            df = pd.DataFrame(cycles, columns=['range', 'mean', 'count'])
        else:
            df = cycles.copy()

        if self.model == 'marco_starkey':
            return self._marco_starkey_damage(df, sn_curve)
        elif self.model == 'corten_dolan':
            return self._corten_dolan_damage(df, sn_curve)
        elif self.model == 'double_linear':
            return self._double_linear_damage(df, sn_curve)
        else:
            raise ValueError(f"Unknown nonlinear model: {self.model}")

    def _marco_starkey_damage(self,
                             cycles_df: pd.DataFrame,
                             sn_curve: SNCurveBase) -> DamageResults:
        """
        Marco-Starkey nonlinear damage model

        D = Σ[(ni/Ni)^αi] where αi depends on stress level
        """
        # Default parameters
        alpha_base = self.parameters.get('alpha_base', 0.4)
        stress_dependency = self.parameters.get('stress_dependency', 0.5)

        total_damage = 0.0
        damage_contributions = []

        # Get reference stress for normalization
        stress_ranges = cycles_df['range'].values
        max_stress = np.max(stress_ranges)

        for idx, cycle in cycles_df.iterrows():
            stress_range = cycle['range']
            n_applied = cycle['count']

            if stress_range <= 0 or n_applied <= 0:
                continue

            N_allowable = sn_curve.get_allowable_cycles(stress_range)

            if np.isfinite(N_allowable) and N_allowable > 0:
                # Calculate stress-dependent exponent
                stress_ratio = stress_range / max_stress
                alpha = alpha_base + stress_dependency * stress_ratio

                # Nonlinear damage increment
                damage_ratio = n_applied / N_allowable
                damage_increment = damage_ratio ** alpha
                total_damage += damage_increment

                damage_contributions.append({
                    'stress_range': stress_range,
                    'cycles_applied': n_applied,
                    'cycles_allowable': N_allowable,
                    'alpha': alpha,
                    'damage_increment': damage_increment,
                    'cumulative_damage': total_damage
                })

        return self._format_results(total_damage, damage_contributions, cycles_df)

    def _corten_dolan_damage(self,
                            cycles_df: pd.DataFrame,
                            sn_curve: SNCurveBase) -> DamageResults:
        """
        Corten-Dolan nonlinear damage model

        Considers stress level interactions
        """
        # Sort by stress level for sequential calculation
        sorted_df = cycles_df.sort_values('range', ascending=False)

        total_damage = 0.0
        damage_contributions = []

        for idx, cycle in sorted_df.iterrows():
            stress_range = cycle['range']
            n_applied = cycle['count']

            if stress_range <= 0 or n_applied <= 0:
                continue

            N_allowable = sn_curve.get_allowable_cycles(stress_range)

            if np.isfinite(N_allowable) and N_allowable > 0:
                # Corten-Dolan modification based on previous damage
                if total_damage > 0:
                    # Reduce effective cycles based on accumulated damage
                    damage_factor = 1 + total_damage
                    effective_cycles = n_applied * damage_factor
                else:
                    effective_cycles = n_applied

                damage_increment = effective_cycles / N_allowable
                total_damage += damage_increment

                damage_contributions.append({
                    'stress_range': stress_range,
                    'cycles_applied': n_applied,
                    'effective_cycles': effective_cycles,
                    'cycles_allowable': N_allowable,
                    'damage_increment': damage_increment,
                    'cumulative_damage': total_damage
                })

        return self._format_results(total_damage, damage_contributions, cycles_df)

    def _double_linear_damage(self,
                             cycles_df: pd.DataFrame,
                             sn_curve: SNCurveBase) -> DamageResults:
        """
        Double linear damage model

        Different damage rates for initiation and propagation phases
        """
        initiation_fraction = self.parameters.get('initiation_fraction', 0.1)
        propagation_rate = self.parameters.get('propagation_rate', 2.0)

        total_damage = 0.0
        damage_contributions = []

        for idx, cycle in cycles_df.iterrows():
            stress_range = cycle['range']
            n_applied = cycle['count']

            if stress_range <= 0 or n_applied <= 0:
                continue

            N_allowable = sn_curve.get_allowable_cycles(stress_range)

            if np.isfinite(N_allowable) and N_allowable > 0:
                linear_damage = n_applied / N_allowable

                # Check if in initiation or propagation phase
                if total_damage < initiation_fraction:
                    # Initiation phase - linear damage
                    damage_increment = linear_damage
                    phase = 'initiation'
                else:
                    # Propagation phase - accelerated damage
                    damage_increment = linear_damage * propagation_rate
                    phase = 'propagation'

                total_damage += damage_increment

                damage_contributions.append({
                    'stress_range': stress_range,
                    'cycles_applied': n_applied,
                    'cycles_allowable': N_allowable,
                    'phase': phase,
                    'damage_increment': damage_increment,
                    'cumulative_damage': total_damage
                })

        return self._format_results(total_damage, damage_contributions, cycles_df)

    def _format_results(self,
                       total_damage: float,
                       damage_contributions: List[Dict],
                       cycles_df: pd.DataFrame) -> DamageResults:
        """Format results for nonlinear damage calculation"""
        cycles_processed = cycles_df['count'].sum()

        results = {
            'method': self.name,
            'total_damage': total_damage,
            'life_fraction_used': total_damage,  # Assuming failure at D=1
            'life_fraction_remaining': max(0, 1 - total_damage),
            'safety_factor': 1.0 / total_damage if total_damage > 0 else np.inf,
            'cycles_processed': cycles_processed,
            'damage_contributions': damage_contributions,
            'model_parameters': self.parameters
        }

        return results


class CriticalPlaneAnalysis:
    """
    Critical plane fatigue analysis for multiaxial loading

    Implements various critical plane models including:
    - Findley criterion
    - Brown-Miller criterion
    - Smith-Watson-Topper (SWT) parameter
    """

    def __init__(self, criterion: str = 'findley'):
        """
        Initialize critical plane analysis

        Parameters
        ----------
        criterion : str, default='findley'
            Critical plane criterion: 'findley', 'brown_miller', 'swt'
        """
        self.criterion = criterion

    def calculate_equivalent_stress(self,
                                  stress_tensor_history: np.ndarray,
                                  material_constants: Dict[str, float]) -> float:
        """
        Calculate equivalent uniaxial stress for multiaxial loading

        Parameters
        ----------
        stress_tensor_history : np.ndarray
            Time history of stress tensor components [σxx, σyy, σzz, τxy, τyz, τzx]
        material_constants : dict
            Material constants for the chosen criterion

        Returns
        -------
        float
            Equivalent uniaxial stress range
        """
        if self.criterion == 'findley':
            return self._findley_criterion(stress_tensor_history, material_constants)
        elif self.criterion == 'brown_miller':
            return self._brown_miller_criterion(stress_tensor_history, material_constants)
        elif self.criterion == 'swt':
            return self._swt_criterion(stress_tensor_history, material_constants)
        else:
            raise ValueError(f"Unknown criterion: {self.criterion}")

    def _findley_criterion(self,
                          stress_history: np.ndarray,
                          constants: Dict[str, float]) -> float:
        """Findley critical plane criterion"""
        # Simplified implementation - would need full 3D analysis for production use
        k = constants.get('k', 0.5)  # Material constant

        # Extract principal stresses (simplified)
        sigma_max = np.max(stress_history[:, 0])
        sigma_min = np.min(stress_history[:, 0])
        tau_max = np.max(np.abs(stress_history[:, 3]))  # Maximum shear

        # Findley parameter
        sigma_n_max = (sigma_max + sigma_min) / 2
        findley_param = tau_max + k * sigma_n_max

        return 2 * findley_param  # Convert to equivalent stress range

    def _brown_miller_criterion(self,
                               stress_history: np.ndarray,
                               constants: Dict[str, float]) -> float:
        """Brown-Miller critical plane criterion"""
        # Simplified implementation
        return np.max(stress_history[:, 0]) - np.min(stress_history[:, 0])

    def _swt_criterion(self,
                      stress_history: np.ndarray,
                      constants: Dict[str, float]) -> float:
        """Smith-Watson-Topper parameter"""
        # Simplified implementation
        sigma_max = np.max(stress_history[:, 0])
        delta_sigma = np.max(stress_history[:, 0]) - np.min(stress_history[:, 0])

        swt_param = sigma_max * delta_sigma / 2
        return np.sqrt(swt_param)


def compare_damage_methods(cycles: CycleData,
                          sn_curve: SNCurveBase,
                          methods: Optional[List[str]] = None) -> pd.DataFrame:
    """
    Compare different damage accumulation methods

    Parameters
    ----------
    cycles : CycleData
        Load cycle data
    sn_curve : SNCurveBase
        S-N curve for analysis
    methods : list, optional
        Methods to compare. If None, uses default set.

    Returns
    -------
    pd.DataFrame
        Comparison results
    """
    if methods is None:
        methods = ['linear', 'modified_miners', 'marco_starkey']

    results = []

    for method in methods:
        if method == 'linear':
            calculator = LinearDamageAccumulation()
        elif method == 'modified_miners':
            calculator = ModifiedMinersRule()
        elif method == 'marco_starkey':
            calculator = NonlinearDamageAccumulation('marco_starkey')
        else:
            logger.warning(f"Unknown method: {method}")
            continue

        try:
            result = calculator.calculate_damage(cycles, sn_curve)
            results.append({
                'method': method,
                'total_damage': result['total_damage'],
                'safety_factor': result['safety_factor'],
                'life_fraction_used': result['life_fraction_used']
            })
        except Exception as e:
            logger.error(f"Error calculating damage for {method}: {e}")

    return pd.DataFrame(results)


if __name__ == "__main__":
    # Example usage and testing
    from .sn_curves import get_dnv_curve

    # Create test data
    test_cycles = pd.DataFrame({
        'range': [100, 80, 60, 40, 20],
        'mean': [10, 5, 0, -5, -10],
        'count': [1000, 2000, 5000, 10000, 50000]
    })

    # Get S-N curve
    sn_curve = get_dnv_curve('D')

    # Test different damage methods
    linear_calc = LinearDamageAccumulation()
    linear_result = linear_calc.calculate_damage(test_cycles, sn_curve)

    modified_calc = ModifiedMinersRule()
    modified_result = modified_calc.calculate_damage(test_cycles, sn_curve)

    nonlinear_calc = NonlinearDamageAccumulation('marco_starkey')
    nonlinear_result = nonlinear_calc.calculate_damage(test_cycles, sn_curve)

    # Compare results
    comparison = compare_damage_methods(test_cycles, sn_curve)

    print("Damage Accumulation Comparison:")
    print(comparison)
    print(f"\nLinear damage: {linear_result['total_damage']:.4f}")
    print(f"Modified Miner's: {modified_result['total_damage']:.4f}")
    print(f"Nonlinear damage: {nonlinear_result['total_damage']:.4f}")

    print("\nDamage accumulation module test completed successfully!")