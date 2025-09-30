"""
Fatigue Analysis Engine
=======================

This module provides a comprehensive fatigue analysis engine that integrates
S-N curves, damage accumulation, rainflow counting, and frequency domain methods
into a unified interface for structural fatigue assessment.

Key Features:
- Complete fatigue analysis workflow
- Time-domain and frequency-domain analysis
- Multiple S-N curve standards and custom curves
- Various damage accumulation methods
- Statistical analysis and safety assessments
- Engineering validation and quality checks
- Comprehensive reporting and visualization

Legacy Code Integration:
- Migrated multi-slope S-N curve calculations
- Enhanced linear slope calculations
- Improved shear data analysis methods
- Preserved engineering accuracy

Author: Digital Model Team
Version: 2.0.0
"""

import numpy as np
import pandas as pd
from typing import Dict, Optional, Union, Tuple, List, Literal, Any
from dataclasses import dataclass, field
from pathlib import Path
import logging
import json
from datetime import datetime
import warnings

from .sn_curves import (
    SNCurveBase, PowerLawSNCurve, BilinearSNCurve, StandardSNCurves,
    get_dnv_curve, get_api_curve, get_bs_curve, MaterialProperties,
    MeanStressCorrection, ThicknessCorrection
)
from .damage_accumulation import (
    DamageAccumulationBase, LinearDamageAccumulation, ModifiedMinersRule,
    NonlinearDamageAccumulation, LoadCycle, DamageState, CriticalPlaneAnalysis
)
from .rainflow import RainflowCounter, RainflowParameters, rainflow_count
from .frequency_domain import (
    FrequencyDomainBase, DirlikMethod, TovoBenasciuttiMethod,
    NarrowBandMethod, ResponsePSDCalculator
)

logger = logging.getLogger(__name__)

# Type aliases
AnalysisInput = Union[np.ndarray, pd.Series, pd.DataFrame]
AnalysisResults = Dict[str, Any]


@dataclass
class FatigueAnalysisConfig:
    """Configuration for fatigue analysis"""
    # S-N curve settings
    sn_standard: str = 'DNV'
    sn_curve_class: str = 'D'
    custom_sn_curve: Optional[SNCurveBase] = None
    thickness: Optional[float] = None

    # Material properties
    material: Optional[MaterialProperties] = None

    # Analysis method
    analysis_method: Literal['time_domain', 'frequency_domain', 'both'] = 'time_domain'

    # Damage accumulation
    damage_method: Literal['linear', 'modified_miners', 'nonlinear'] = 'linear'
    nonlinear_model: str = 'marco_starkey'
    mean_stress_correction: Optional[str] = None

    # Rainflow counting
    rainflow_gate_value: float = 0.05
    rainflow_gate_type: str = 'relative'

    # Frequency domain settings
    frequency_method: str = 'dirlik'

    # Safety and validation
    safety_factor_target: float = 2.0
    validation_checks: bool = True

    # Output settings
    detailed_output: bool = True
    generate_plots: bool = False


@dataclass
class FatigueValidationResults:
    """Results from engineering validation checks"""
    is_valid: bool = True
    warnings: List[str] = field(default_factory=list)
    errors: List[str] = field(default_factory=list)
    recommendations: List[str] = field(default_factory=list)

    def add_warning(self, message: str):
        """Add validation warning"""
        self.warnings.append(message)

    def add_error(self, message: str):
        """Add validation error"""
        self.errors.append(message)
        self.is_valid = False

    def add_recommendation(self, message: str):
        """Add engineering recommendation"""
        self.recommendations.append(message)


class MultislopeSNCurve(SNCurveBase):
    """
    Multi-slope S-N curve implementation based on legacy code patterns

    Supports up to 4 different slopes with transition points, matching
    the structure found in the legacy FatigueBasiccurve.py module.
    """

    def __init__(self,
                 name: str,
                 slopes: List[float],
                 constants: List[float],
                 transition_cycles: List[float],
                 fatigue_limit: float = 0.0,
                 material: Optional[MaterialProperties] = None):
        """
        Initialize multi-slope S-N curve

        Parameters
        ----------
        name : str
            Curve identifier
        slopes : list of float
            Slope values (m1, m2, m3, m4)
        constants : list of float
            Fatigue constants (A1, A2, A3, A4)
        transition_cycles : list of float
            Cycle points where slopes change
        fatigue_limit : float, default=0.0
            Constant amplitude fatigue limit
        material : MaterialProperties, optional
            Material properties
        """
        super().__init__(name, material)

        if len(slopes) != len(constants):
            raise ValueError("Number of slopes must match number of constants")

        self.slopes = slopes
        self.constants = constants
        self.transition_cycles = sorted(transition_cycles)
        self.fatigue_limit = fatigue_limit

        # Calculate transition stresses for continuity
        self.transition_stresses = []
        for i, cycles in enumerate(self.transition_cycles):
            if i < len(self.constants):
                stress = (self.constants[i] / cycles) ** (1/self.slopes[i])
                self.transition_stresses.append(stress)

    def get_allowable_cycles(self, stress_range: Union[float, np.ndarray]) -> Union[float, np.ndarray]:
        """Calculate allowable cycles using appropriate slope"""
        S = np.asarray(stress_range, dtype=float)
        N = np.zeros_like(S)

        # Below fatigue limit
        below_limit = S <= self.fatigue_limit
        N[below_limit] = np.inf

        # Apply appropriate slope based on stress level
        for i in range(len(self.slopes)):
            if i == 0:
                # First slope for high stress
                mask = S > self.fatigue_limit
                if len(self.transition_stresses) > 0:
                    mask &= S > self.transition_stresses[0]
            elif i < len(self.transition_stresses):
                # Intermediate slopes
                mask = (S <= self.transition_stresses[i-1]) & (S > self.transition_stresses[i])
            else:
                # Last slope for low stress
                mask = S <= self.transition_stresses[-1] if self.transition_stresses else (S > self.fatigue_limit)

            if np.any(mask):
                N[mask] = self.constants[i] * (S[mask] ** (-self.slopes[i]))

        return N if isinstance(stress_range, np.ndarray) else float(N)

    def get_stress_range(self, cycles: Union[float, np.ndarray]) -> Union[float, np.ndarray]:
        """Calculate stress range for given cycles (inverse function)"""
        N = np.asarray(cycles, dtype=float)
        finite_mask = np.isfinite(N)

        S = np.full_like(N, self.fatigue_limit, dtype=float)

        if np.any(finite_mask):
            # Determine which slope to use based on cycle count
            for i, transition_cycles in enumerate(self.transition_cycles):
                if i < len(self.slopes):
                    if i == 0:
                        mask = finite_mask & (N <= transition_cycles)
                    else:
                        prev_cycles = self.transition_cycles[i-1] if i > 0 else np.inf
                        mask = finite_mask & (N > prev_cycles) & (N <= transition_cycles)

                    if np.any(mask):
                        S[mask] = (self.constants[i] / N[mask]) ** (1/self.slopes[i])

            # Handle cycles beyond last transition
            if len(self.transition_cycles) > 0:
                last_mask = finite_mask & (N > self.transition_cycles[-1])
                if np.any(last_mask):
                    last_slope_idx = len(self.slopes) - 1
                    S[last_mask] = (self.constants[last_slope_idx] / N[last_mask]) ** (1/self.slopes[last_slope_idx])

        return S if isinstance(cycles, np.ndarray) else float(S)


class FatigueAnalysisEngine:
    """
    Comprehensive fatigue analysis engine

    Provides a unified interface for fatigue analysis incorporating
    lessons learned from legacy code while using modern implementations.
    """

    def __init__(self, config: Optional[FatigueAnalysisConfig] = None):
        """
        Initialize fatigue analysis engine

        Parameters
        ----------
        config : FatigueAnalysisConfig, optional
            Analysis configuration. If None, uses defaults.
        """
        self.config = config or FatigueAnalysisConfig()
        self.results_history: List[AnalysisResults] = []
        self.validation_history: List[FatigueValidationResults] = []

        # Initialize components
        self._setup_sn_curve()
        self._setup_damage_calculator()
        self._setup_rainflow_counter()

    def _setup_sn_curve(self):
        """Setup S-N curve based on configuration"""
        if self.config.custom_sn_curve:
            self.sn_curve = self.config.custom_sn_curve
        else:
            # Get standard curve
            if self.config.sn_standard.upper() == 'DNV':
                self.sn_curve = get_dnv_curve(self.config.sn_curve_class)
            elif self.config.sn_standard.upper() == 'API':
                self.sn_curve = get_api_curve(self.config.sn_curve_class)
            elif self.config.sn_standard.upper() == 'BS':
                self.sn_curve = get_bs_curve(self.config.sn_curve_class)
            else:
                raise ValueError(f"Unknown S-N standard: {self.config.sn_standard}")

            # Apply thickness correction if specified
            if self.config.thickness and self.config.thickness != 25.0:
                self.sn_curve = ThicknessCorrection.apply_thickness_effect(
                    self.sn_curve, self.config.thickness
                )

    def _setup_damage_calculator(self):
        """Setup damage accumulation calculator"""
        if self.config.damage_method == 'linear':
            self.damage_calculator = LinearDamageAccumulation(
                mean_stress_correction=self.config.mean_stress_correction
            )
        elif self.config.damage_method == 'modified_miners':
            self.damage_calculator = ModifiedMinersRule()
        elif self.config.damage_method == 'nonlinear':
            self.damage_calculator = NonlinearDamageAccumulation(
                model=self.config.nonlinear_model
            )
        else:
            raise ValueError(f"Unknown damage method: {self.config.damage_method}")

    def _setup_rainflow_counter(self):
        """Setup rainflow counting parameters"""
        self.rainflow_params = RainflowParameters(
            gate_value=self.config.rainflow_gate_value,
            gate_type=self.config.rainflow_gate_type,
            small_cycle_removal=True,
            cycle_combination=True
        )
        self.rainflow_counter = RainflowCounter(self.rainflow_params)

    def analyze_time_series(self,
                           stress_time_series: AnalysisInput,
                           time: Optional[np.ndarray] = None,
                           mean_stress: Optional[AnalysisInput] = None) -> AnalysisResults:
        """
        Perform complete time-domain fatigue analysis

        Parameters
        ----------
        stress_time_series : array-like
            Stress time series data
        time : array-like, optional
            Time vector. If None, assumes unit spacing.
        mean_stress : array-like, optional
            Mean stress time series. If None, assumes zero mean.

        Returns
        -------
        AnalysisResults
            Comprehensive analysis results
        """
        logger.info("Starting time-domain fatigue analysis")

        # Validate inputs
        validation = self._validate_time_series_input(stress_time_series, time, mean_stress)

        if not validation.is_valid:
            raise ValueError(f"Input validation failed: {validation.errors}")

        # Convert inputs to numpy arrays
        stress = np.asarray(stress_time_series)
        if time is None:
            time = np.arange(len(stress))
        else:
            time = np.asarray(time)

        if mean_stress is not None:
            mean_stress = np.asarray(mean_stress)

        # Perform rainflow counting
        logger.info("Performing rainflow cycle counting")
        if mean_stress is not None:
            cycles_result = self.rainflow_counter.extract_cycles_with_means(
                stress, mean_stress
            )
        else:
            cycles_result = self.rainflow_counter.count_cycles(stress)

        cycles_df = cycles_result['cycles']

        # Calculate fatigue damage
        logger.info("Calculating fatigue damage")
        damage_result = self.damage_calculator.calculate_damage(cycles_df, self.sn_curve)

        # Perform additional analysis
        analysis_stats = self._calculate_analysis_statistics(stress, cycles_df, damage_result)

        # Engineering validation
        engineering_validation = self._perform_engineering_validation(
            stress, cycles_df, damage_result
        )

        # Compile results
        results = {
            'analysis_type': 'time_domain',
            'timestamp': datetime.now().isoformat(),
            'configuration': self.config,
            'input_statistics': {
                'n_points': len(stress),
                'duration': float(time[-1] - time[0]) if len(time) > 1 else 0.0,
                'sampling_rate': len(stress) / (time[-1] - time[0]) if len(time) > 1 else 1.0,
                'stress_range': (float(np.min(stress)), float(np.max(stress))),
                'stress_std': float(np.std(stress)),
                'stress_mean': float(np.mean(stress))
            },
            'rainflow_results': cycles_result,
            'damage_results': damage_result,
            'analysis_statistics': analysis_stats,
            'validation': engineering_validation,
            'sn_curve_info': {
                'name': self.sn_curve.name,
                'type': type(self.sn_curve).__name__
            }
        }

        # Store in history
        self.results_history.append(results)
        self.validation_history.append(engineering_validation)

        logger.info("Time-domain fatigue analysis completed")
        return results

    def analyze_psd(self,
                   stress_psd: np.ndarray,
                   frequency: np.ndarray,
                   duration: float = 3600.0) -> AnalysisResults:
        """
        Perform frequency-domain fatigue analysis

        Parameters
        ----------
        stress_psd : np.ndarray
            Power spectral density of stress
        frequency : np.ndarray
            Frequency vector (Hz)
        duration : float, default=3600.0
            Analysis duration in seconds

        Returns
        -------
        AnalysisResults
            Frequency-domain analysis results
        """
        logger.info("Starting frequency-domain fatigue analysis")

        # Validate inputs
        validation = self._validate_psd_input(stress_psd, frequency)
        if not validation.is_valid:
            raise ValueError(f"Input validation failed: {validation.errors}")

        # Setup frequency domain method
        if self.config.frequency_method.lower() == 'dirlik':
            freq_method = DirlikMethod()
        elif self.config.frequency_method.lower() == 'tovo_benasciutti':
            freq_method = TovoBenasciuttiMethod()
        elif self.config.frequency_method.lower() == 'narrow_band':
            freq_method = NarrowBandMethod()
        else:
            raise ValueError(f"Unknown frequency method: {self.config.frequency_method}")

        # Calculate spectral moments and damage
        logger.info(f"Using {self.config.frequency_method} method")
        freq_result = freq_method.calculate_damage(stress_psd, frequency, self.sn_curve)

        # Scale by duration
        total_damage = freq_result['damage_rate'] * duration

        # Calculate equivalent cycles per second
        equivalent_cycles_per_sec = freq_result.get('equivalent_cycles_per_sec', 0.0)
        total_equivalent_cycles = equivalent_cycles_per_sec * duration

        # Engineering validation for frequency domain
        engineering_validation = self._perform_frequency_validation(
            stress_psd, frequency, freq_result
        )

        # Compile results
        results = {
            'analysis_type': 'frequency_domain',
            'timestamp': datetime.now().isoformat(),
            'configuration': self.config,
            'input_statistics': {
                'frequency_range': (float(frequency[0]), float(frequency[-1])),
                'n_frequency_points': len(frequency),
                'peak_frequency': float(frequency[np.argmax(stress_psd)]),
                'rms_stress': float(np.sqrt(np.trapz(stress_psd, frequency))),
                'duration': duration
            },
            'frequency_results': freq_result,
            'damage_results': {
                'method': f"Frequency Domain - {self.config.frequency_method}",
                'damage_rate': freq_result['damage_rate'],
                'total_damage': total_damage,
                'safety_factor': 1.0 / total_damage if total_damage > 0 else np.inf,
                'equivalent_cycles_total': total_equivalent_cycles,
                'equivalent_cycles_per_sec': equivalent_cycles_per_sec
            },
            'validation': engineering_validation,
            'sn_curve_info': {
                'name': self.sn_curve.name,
                'type': type(self.sn_curve).__name__
            }
        }

        # Store in history
        self.results_history.append(results)
        self.validation_history.append(engineering_validation)

        logger.info("Frequency-domain fatigue analysis completed")
        return results

    def calculate_high_stress_range_life(self, stress_ranges: List[float]) -> Dict[str, Dict]:
        """
        Calculate fatigue life for specific high stress ranges

        This method recreates the functionality from LinearslopeCal.py
        for calculating life at specific stress levels.

        Parameters
        ----------
        stress_ranges : list of float
            Stress ranges to analyze (MPa)

        Returns
        -------
        dict
            Life calculations for each stress range
        """
        logger.info("Calculating high stress range fatigue life")

        results = {}

        for stress_range in stress_ranges:
            if stress_range <= 0:
                continue

            # Get allowable cycles from S-N curve
            N_allowable = self.sn_curve.get_allowable_cycles(stress_range)

            # Calculate safety factors
            safety_factor = self.config.safety_factor_target
            N_design = N_allowable / safety_factor if np.isfinite(N_allowable) else np.inf

            results[f"{stress_range}_MPa"] = {
                'stress_range': stress_range,
                'allowable_cycles': float(N_allowable),
                'design_cycles': float(N_design),
                'safety_factor': safety_factor,
                'is_above_fatigue_limit': stress_range > self.sn_curve.fatigue_limit,
                'fatigue_limit': self.sn_curve.fatigue_limit
            }

        return results

    def analyze_shear_data(self,
                          high_stress: float = 1000.0,
                          low_stress_range: Tuple[float, float] = (1.0, 16.0)) -> Dict[str, Any]:
        """
        Perform shear data analysis similar to Shear7dataCal.py

        Parameters
        ----------
        high_stress : float, default=1000.0
            High stress range for analysis (MPa)
        low_stress_range : tuple, default=(1.0, 16.0)
            Range of low stress values to analyze

        Returns
        -------
        dict
            Shear analysis results
        """
        logger.info("Performing shear data analysis")

        # High stress range calculation
        high_stress_cycles = self.sn_curve.get_allowable_cycles(high_stress)

        # Low stress range calculations
        low_stress_values = np.linspace(low_stress_range[0], low_stress_range[1], 15)
        low_stress_results = []

        for low_stress in low_stress_values:
            if low_stress > 0:
                cycles = self.sn_curve.get_allowable_cycles(low_stress)
                low_stress_results.append({
                    'stress': float(low_stress),
                    'cycles': float(cycles),
                    'is_finite': np.isfinite(cycles)
                })

        results = {
            'high_stress_analysis': {
                'stress_range': high_stress,
                'allowable_cycles': float(high_stress_cycles),
                'is_finite': np.isfinite(high_stress_cycles)
            },
            'low_stress_analysis': low_stress_results,
            'curve_characteristics': {
                'fatigue_limit': self.sn_curve.fatigue_limit,
                'curve_name': self.sn_curve.name
            }
        }

        return results

    def _validate_time_series_input(self,
                                  stress: AnalysisInput,
                                  time: Optional[np.ndarray],
                                  mean_stress: Optional[AnalysisInput]) -> FatigueValidationResults:
        """Validate time series input data"""
        validation = FatigueValidationResults()

        # Check stress data
        stress_arr = np.asarray(stress)
        if len(stress_arr) < 10:
            validation.add_error("Stress time series too short (minimum 10 points)")

        if not np.all(np.isfinite(stress_arr)):
            validation.add_error("Stress time series contains NaN or infinite values")

        # Check time vector if provided
        if time is not None:
            time_arr = np.asarray(time)
            if len(time_arr) != len(stress_arr):
                validation.add_error("Time vector length must match stress data length")

            if not np.all(np.diff(time_arr) > 0):
                validation.add_warning("Time vector is not strictly increasing")

        # Check mean stress if provided
        if mean_stress is not None:
            mean_arr = np.asarray(mean_stress)
            if len(mean_arr) != len(stress_arr):
                validation.add_error("Mean stress length must match stress data length")

        # Engineering checks
        stress_range = np.max(stress_arr) - np.min(stress_arr)
        if stress_range < 1.0:
            validation.add_warning("Very small stress range detected - check units (MPa expected)")

        if stress_range > 1000.0:
            validation.add_warning("Very large stress range detected - verify data scaling")

        return validation

    def _validate_psd_input(self,
                           psd: np.ndarray,
                           frequency: np.ndarray) -> FatigueValidationResults:
        """Validate PSD input data"""
        validation = FatigueValidationResults()

        if len(psd) != len(frequency):
            validation.add_error("PSD and frequency arrays must have same length")

        if not np.all(psd >= 0):
            validation.add_error("PSD values must be non-negative")

        if not np.all(frequency >= 0):
            validation.add_error("Frequency values must be non-negative")

        if frequency[0] > 0.01:
            validation.add_warning("PSD starts at high frequency - may miss low-frequency content")

        if frequency[-1] < 10.0:
            validation.add_warning("PSD ends at low frequency - may miss high-frequency content")

        return validation

    def _calculate_analysis_statistics(self,
                                     stress: np.ndarray,
                                     cycles: pd.DataFrame,
                                     damage_result: Dict) -> Dict[str, Any]:
        """Calculate additional analysis statistics"""
        stats = {}

        # Stress statistics
        stats['stress_statistics'] = {
            'mean': float(np.mean(stress)),
            'std': float(np.std(stress)),
            'skewness': float(self._calculate_skewness(stress)),
            'kurtosis': float(self._calculate_kurtosis(stress)),
            'range': float(np.max(stress) - np.min(stress))
        }

        # Cycle statistics
        if len(cycles) > 0:
            stats['cycle_statistics'] = {
                'total_cycles': float(cycles['count'].sum()),
                'unique_ranges': len(cycles),
                'max_range': float(cycles['range'].max()),
                'min_range': float(cycles['range'].min()),
                'mean_range': float(np.average(cycles['range'], weights=cycles['count'])),
                'most_damaging_range': float(
                    cycles.loc[cycles['range'].idxmax(), 'range']
                ) if len(cycles) > 0 else 0.0
            }

        # Damage distribution
        if 'damage_contributions' in damage_result:
            contributions = damage_result['damage_contributions']
            if contributions:
                damages = [c['damage_increment'] for c in contributions]
                stress_ranges = [c['stress_range'] for c in contributions]

                stats['damage_distribution'] = {
                    'max_contribution': float(np.max(damages)),
                    'top_10_percent_contribution': float(
                        np.sum(sorted(damages, reverse=True)[:max(1, len(damages)//10)])
                    ),
                    'most_damaging_stress': float(
                        stress_ranges[np.argmax(damages)]
                    ) if damages else 0.0
                }

        return stats

    def _perform_engineering_validation(self,
                                      stress: np.ndarray,
                                      cycles: pd.DataFrame,
                                      damage_result: Dict) -> FatigueValidationResults:
        """Perform engineering validation checks"""
        validation = FatigueValidationResults()

        # Check damage level
        total_damage = damage_result.get('total_damage', 0.0)
        if total_damage > 1.0:
            validation.add_warning("Total damage exceeds 1.0 - structure may have failed")
        elif total_damage > 0.5:
            validation.add_warning("High damage level detected - detailed inspection recommended")

        # Check safety factor
        safety_factor = damage_result.get('safety_factor', np.inf)
        if safety_factor < self.config.safety_factor_target:
            validation.add_error(
                f"Safety factor {safety_factor:.2f} below target {self.config.safety_factor_target}"
            )
        elif safety_factor < self.config.safety_factor_target * 1.5:
            validation.add_warning("Safety factor close to minimum acceptable level")

        # Check stress levels
        if len(cycles) > 0:
            max_stress = cycles['range'].max()
            if hasattr(self.sn_curve, 'material') and self.sn_curve.material:
                yield_stress = self.sn_curve.material.yield_strength
                if max_stress > 0.8 * yield_stress:
                    validation.add_warning("High stress levels approaching yield strength")

        # Check for fatigue limit exceedance
        above_limit_cycles = cycles[cycles['range'] > self.sn_curve.fatigue_limit]
        if len(above_limit_cycles) > 0:
            total_above_limit = above_limit_cycles['count'].sum()
            total_cycles = cycles['count'].sum()
            percent_above = (total_above_limit / total_cycles) * 100

            if percent_above > 50:
                validation.add_warning(
                    f"{percent_above:.1f}% of cycles exceed fatigue limit"
                )

        # Add recommendations
        if total_damage < 0.1:
            validation.add_recommendation("Structure in good condition - maintain regular inspection schedule")
        elif total_damage < 0.5:
            validation.add_recommendation("Consider increased inspection frequency")
        else:
            validation.add_recommendation("Detailed structural assessment recommended")

        return validation

    def _perform_frequency_validation(self,
                                    psd: np.ndarray,
                                    frequency: np.ndarray,
                                    freq_result: Dict) -> FatigueValidationResults:
        """Perform frequency domain specific validation"""
        validation = FatigueValidationResults()

        # Check PSD characteristics
        rms_stress = np.sqrt(np.trapz(psd, frequency))
        peak_freq = frequency[np.argmax(psd)]

        if rms_stress > 100:
            validation.add_warning("High RMS stress level detected")

        if peak_freq > 50:
            validation.add_warning("High frequency content may indicate measurement noise")

        # Check spectral parameters
        if 'spectral_moments' in freq_result:
            moments = freq_result['spectral_moments']
            if moments.get('irregularity_factor', 0) > 0.9:
                validation.add_warning("Highly irregular spectrum - may need time domain analysis")

        return validation

    @staticmethod
    def _calculate_skewness(data: np.ndarray) -> float:
        """Calculate skewness of data"""
        if len(data) < 3:
            return 0.0

        mean = np.mean(data)
        std = np.std(data, ddof=1)
        if std == 0:
            return 0.0

        return np.mean(((data - mean) / std) ** 3)

    @staticmethod
    def _calculate_kurtosis(data: np.ndarray) -> float:
        """Calculate kurtosis of data"""
        if len(data) < 4:
            return 0.0

        mean = np.mean(data)
        std = np.std(data, ddof=1)
        if std == 0:
            return 0.0

        return np.mean(((data - mean) / std) ** 4) - 3.0

    def generate_report(self,
                       analysis_result: AnalysisResults,
                       output_path: Optional[Path] = None) -> str:
        """
        Generate comprehensive analysis report

        Parameters
        ----------
        analysis_result : dict
            Results from fatigue analysis
        output_path : Path, optional
            Path to save report. If None, returns as string.

        Returns
        -------
        str
            Report content (if output_path is None)
        """
        report_lines = []

        # Header
        report_lines.extend([
            "=" * 80,
            "FATIGUE ANALYSIS REPORT",
            "=" * 80,
            f"Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}",
            f"Analysis Type: {analysis_result['analysis_type'].upper()}",
            ""
        ])

        # Configuration
        config = analysis_result['configuration']
        report_lines.extend([
            "ANALYSIS CONFIGURATION",
            "-" * 40,
            f"S-N Curve: {config.sn_standard}-{config.sn_curve_class}",
            f"Damage Method: {config.damage_method}",
            f"Analysis Method: {config.analysis_method}",
            ""
        ])

        # Input Statistics
        input_stats = analysis_result['input_statistics']
        report_lines.extend([
            "INPUT DATA SUMMARY",
            "-" * 40
        ])

        if analysis_result['analysis_type'] == 'time_domain':
            report_lines.extend([
                f"Data Points: {input_stats['n_points']:,}",
                f"Duration: {input_stats['duration']:.2f} seconds",
                f"Stress Range: {input_stats['stress_range'][0]:.1f} to {input_stats['stress_range'][1]:.1f} MPa",
                f"Mean Stress: {input_stats['stress_mean']:.2f} MPa",
                f"Stress Std Dev: {input_stats['stress_std']:.2f} MPa",
                ""
            ])
        else:  # frequency_domain
            report_lines.extend([
                f"Frequency Range: {input_stats['frequency_range'][0]:.3f} to {input_stats['frequency_range'][1]:.1f} Hz",
                f"Peak Frequency: {input_stats['peak_frequency']:.2f} Hz",
                f"RMS Stress: {input_stats['rms_stress']:.2f} MPa",
                f"Duration: {input_stats['duration']:.0f} seconds",
                ""
            ])

        # Damage Results
        damage_result = analysis_result['damage_results']
        report_lines.extend([
            "FATIGUE DAMAGE ASSESSMENT",
            "-" * 40,
            f"Total Damage: {damage_result['total_damage']:.6f}",
            f"Safety Factor: {damage_result['safety_factor']:.2f}",
            f"Life Fraction Used: {damage_result.get('life_fraction_used', 0):.1%}",
            ""
        ])

        # Engineering Assessment
        total_damage = damage_result['total_damage']
        if total_damage < 0.1:
            assessment = "ACCEPTABLE - Low damage, structure is safe"
        elif total_damage < 0.5:
            assessment = "MONITOR - Moderate damage, increase inspection frequency"
        elif total_damage < 1.0:
            assessment = "CONCERN - High damage, detailed inspection required"
        else:
            assessment = "CRITICAL - Damage exceeds limit, immediate action required"

        report_lines.extend([
            "ENGINEERING ASSESSMENT",
            "-" * 40,
            f"Assessment: {assessment}",
            ""
        ])

        # Validation Results
        validation = analysis_result['validation']
        if validation.warnings or validation.errors:
            report_lines.extend([
                "VALIDATION RESULTS",
                "-" * 40
            ])

            for error in validation.errors:
                report_lines.append(f"ERROR: {error}")

            for warning in validation.warnings:
                report_lines.append(f"WARNING: {warning}")

            report_lines.append("")

        # Recommendations
        if validation.recommendations:
            report_lines.extend([
                "RECOMMENDATIONS",
                "-" * 40
            ])

            for i, rec in enumerate(validation.recommendations, 1):
                report_lines.append(f"{i}. {rec}")

            report_lines.append("")

        # Footer
        report_lines.extend([
            "=" * 80,
            "End of Report",
            "=" * 80
        ])

        report_content = "\n".join(report_lines)

        if output_path:
            with open(output_path, 'w') as f:
                f.write(report_content)
            logger.info(f"Report saved to {output_path}")
            return None
        else:
            return report_content


# Convenience functions for quick analysis
def quick_time_domain_analysis(stress_time_series: AnalysisInput,
                              sn_standard: str = 'DNV',
                              sn_curve_class: str = 'D',
                              **kwargs) -> AnalysisResults:
    """
    Quick time-domain fatigue analysis with default settings

    Parameters
    ----------
    stress_time_series : array-like
        Stress time series data
    sn_standard : str, default='DNV'
        S-N curve standard
    sn_curve_class : str, default='D'
        S-N curve class
    **kwargs
        Additional configuration parameters

    Returns
    -------
    AnalysisResults
        Analysis results
    """
    config = FatigueAnalysisConfig(
        sn_standard=sn_standard,
        sn_curve_class=sn_curve_class,
        **kwargs
    )

    engine = FatigueAnalysisEngine(config)
    return engine.analyze_time_series(stress_time_series)


def quick_frequency_domain_analysis(stress_psd: np.ndarray,
                                   frequency: np.ndarray,
                                   duration: float = 3600.0,
                                   sn_standard: str = 'DNV',
                                   sn_curve_class: str = 'D',
                                   **kwargs) -> AnalysisResults:
    """
    Quick frequency-domain fatigue analysis with default settings

    Parameters
    ----------
    stress_psd : np.ndarray
        Power spectral density of stress
    frequency : np.ndarray
        Frequency vector
    duration : float, default=3600.0
        Analysis duration in seconds
    sn_standard : str, default='DNV'
        S-N curve standard
    sn_curve_class : str, default='D'
        S-N curve class
    **kwargs
        Additional configuration parameters

    Returns
    -------
    AnalysisResults
        Analysis results
    """
    config = FatigueAnalysisConfig(
        sn_standard=sn_standard,
        sn_curve_class=sn_curve_class,
        analysis_method='frequency_domain',
        **kwargs
    )

    engine = FatigueAnalysisEngine(config)
    return engine.analyze_psd(stress_psd, frequency, duration)


if __name__ == "__main__":
    # Example usage and testing
    import matplotlib.pyplot as plt

    # Generate test data
    np.random.seed(42)
    time = np.linspace(0, 100, 10000)
    stress = 50 * np.sin(0.1 * time) + 30 * np.sin(0.5 * time) + 10 * np.random.randn(len(time))

    # Quick analysis
    logger.info("Running quick time-domain analysis")
    result = quick_time_domain_analysis(stress)

    print("=" * 60)
    print("FATIGUE ANALYSIS EXAMPLE")
    print("=" * 60)
    print(f"Total Damage: {result['damage_results']['total_damage']:.6f}")
    print(f"Safety Factor: {result['damage_results']['safety_factor']:.2f}")
    print(f"Total Cycles: {result['rainflow_results']['statistics']['total_cycles']:.0f}")
    print(f"Unique Stress Ranges: {result['rainflow_results']['statistics']['unique_ranges']}")

    # Generate and print report
    engine = FatigueAnalysisEngine()
    report = engine.generate_report(result)
    print("\n" + "=" * 60)
    print("DETAILED REPORT")
    print("=" * 60)
    print(report)

    print("\nFatigue analysis engine test completed successfully!")