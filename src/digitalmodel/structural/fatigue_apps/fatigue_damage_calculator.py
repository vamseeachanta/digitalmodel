#!/usr/bin/env python3
"""
Fatigue Damage Calculator using S-N Curves and Miner's Rule

This module calculates fatigue damage from rainflow counting results using:
- S-N curves (stress-life relationships)
- Miner's linear damage accumulation rule
- Mean stress corrections (optional)

References:
    - ABS Guide for Fatigue Assessment of Offshore Structures
    - DNV-RP-C203: Fatigue Design of Offshore Steel Structures
"""

import numpy as np
import pandas as pd
from typing import Dict, Optional, Tuple, List
from dataclasses import dataclass
import logging

logger = logging.getLogger(__name__)


@dataclass
class SNCurveParameters:
    """S-N curve parameters for fatigue analysis"""
    name: str               # Curve name (e.g., "ABS E in Air")
    log_a1: float          # Log of coefficient A for N < threshold
    m1: float              # Slope for N < threshold
    log_a2: Optional[float] = None  # Log of coefficient A for N >= threshold
    m2: Optional[float] = None      # Slope for N >= threshold
    threshold: Optional[float] = None  # Cycle threshold for bi-linear curve
    stress_units: str = "MPa"  # Stress units
    
    @property
    def a1(self) -> float:
        """Get coefficient A1"""
        return 10 ** self.log_a1
    
    @property
    def a2(self) -> float:
        """Get coefficient A2"""
        return 10 ** self.log_a2 if self.log_a2 is not None else self.a1


class FatigueDamageCalculator:
    """
    Calculates fatigue damage using S-N curves and Miner's rule
    """
    
    # Predefined S-N curves
    CURVES = {
        'ABS_E_AIR': SNCurveParameters(
            name="ABS E in Air",
            log_a1=12.018,  # log(1.04e12)
            m1=3.0,
            log_a2=11.170,   # log(1.48e11)
            m2=5.0,
            threshold=1e6,
            stress_units="MPa"
        ),
        'ABS_F_AIR': SNCurveParameters(
            name="ABS F in Air",
            log_a1=11.610,
            m1=3.0,
            log_a2=10.970,
            m2=5.0,
            threshold=1e6,
            stress_units="MPa"
        ),
        'DNV_D_AIR': SNCurveParameters(
            name="DNV D in Air",
            log_a1=12.164,
            m1=3.0,
            log_a2=15.606,
            m2=5.0,
            threshold=1e7,
            stress_units="MPa"
        ),
        'DNV_C_SEAWATER': SNCurveParameters(
            name="DNV C in Seawater with CP",
            log_a1=11.972,
            m1=3.0,
            stress_units="MPa"
        )
    }
    
    def __init__(self, sn_curve: Optional[SNCurveParameters] = None,
                 scf: float = 1.0,
                 design_life_years: float = 20.0):
        """
        Initialize the fatigue damage calculator
        
        Args:
            sn_curve: S-N curve parameters (default: ABS E in Air)
            scf: Stress Concentration Factor (default: 1.0)
            design_life_years: Design life in years for fatigue assessment
        """
        self.sn_curve = sn_curve or self.CURVES['ABS_E_AIR']
        self.scf = scf
        self.design_life_years = design_life_years
        self.damage_results = []
        
        logger.info(f"Initialized fatigue calculator with {self.sn_curve.name} curve, SCF={scf}")
    
    def cycles_to_failure(self, stress_range: float) -> float:
        """
        Calculate number of cycles to failure for a given stress range
        
        Args:
            stress_range: Stress range in MPa
            
        Returns:
            Number of cycles to failure
        """
        # Apply stress concentration factor
        stress = stress_range * self.scf
        
        # Use appropriate S-N curve segment
        if self.sn_curve.threshold and self.sn_curve.m2:
            # Bi-linear S-N curve
            if stress <= self.sn_curve.threshold:
                # High cycle regime
                n_failure = self.sn_curve.a2 * (stress ** -self.sn_curve.m2)
            else:
                # Low cycle regime
                n_failure = self.sn_curve.a1 * (stress ** -self.sn_curve.m1)
        else:
            # Single slope S-N curve
            n_failure = self.sn_curve.a1 * (stress ** -self.sn_curve.m1)
        
        return n_failure
    
    def calculate_damage(self, stress_ranges: np.ndarray, 
                        cycle_counts: np.ndarray,
                        time_duration: float = 200.0) -> float:
        """
        Calculate fatigue damage using Miner's rule
        
        Args:
            stress_ranges: Array of stress ranges (MPa)
            cycle_counts: Array of cycle counts
            time_duration: Duration of the time series (seconds)
            
        Returns:
            Total fatigue damage (dimensionless)
        """
        if len(stress_ranges) == 0:
            return 0.0
        
        total_damage = 0.0
        damage_contributions = []
        
        for stress_range, n_cycles in zip(stress_ranges, cycle_counts):
            if stress_range > 0:  # Ignore zero ranges
                # Calculate cycles to failure
                n_failure = self.cycles_to_failure(stress_range)
                
                # Calculate damage contribution (Miner's rule)
                damage = n_cycles / n_failure
                total_damage += damage
                
                damage_contributions.append({
                    'stress_range': stress_range,
                    'cycles': n_cycles,
                    'cycles_to_failure': n_failure,
                    'damage': damage,
                    'damage_percent': damage * 100
                })
        
        # Store detailed results
        self.damage_results = damage_contributions
        
        # Scale to annual damage if needed
        if time_duration > 0:
            seconds_per_year = 365.25 * 24 * 3600
            scaling_factor = seconds_per_year / time_duration
            annual_damage = total_damage * scaling_factor
        else:
            annual_damage = total_damage
        
        logger.info(f"Total damage: {total_damage:.6e}, Annual damage: {annual_damage:.6e}")
        
        return total_damage
    
    def calculate_fatigue_life(self, stress_ranges: np.ndarray,
                               cycle_counts: np.ndarray,
                               time_duration: float = 200.0,
                               occurrence_weight: float = 1.0) -> Dict[str, float]:
        """
        Calculate fatigue life from stress ranges and cycle counts
        
        Args:
            stress_ranges: Array of stress ranges (MPa)
            cycle_counts: Array of cycle counts
            time_duration: Duration of the time series (seconds)
            occurrence_weight: Weighting factor for this load case (0-1)
            
        Returns:
            Dictionary with fatigue life results
        """
        # Calculate damage
        damage = self.calculate_damage(stress_ranges, cycle_counts, time_duration)
        
        # Apply occurrence weighting
        weighted_damage = damage * occurrence_weight
        
        # Scale to annual damage
        seconds_per_year = 365.25 * 24 * 3600
        annual_damage = weighted_damage * (seconds_per_year / time_duration)
        
        # Calculate fatigue life
        if annual_damage > 0:
            fatigue_life = 1.0 / annual_damage
        else:
            fatigue_life = float('inf')
        
        # Calculate design factor (ratio of life to design life)
        design_factor = fatigue_life / self.design_life_years
        
        results = {
            'damage': damage,
            'weighted_damage': weighted_damage,
            'annual_damage': annual_damage,
            'fatigue_life_years': fatigue_life,
            'design_life_years': self.design_life_years,
            'design_factor': design_factor,
            'passes_check': design_factor >= 1.0
        }
        
        return results
    
    def mean_stress_correction(self, stress_ranges: np.ndarray,
                              mean_stresses: np.ndarray,
                              method: str = 'goodman') -> np.ndarray:
        """
        Apply mean stress correction to stress ranges
        
        Args:
            stress_ranges: Array of stress ranges
            mean_stresses: Array of mean stresses
            method: Correction method ('goodman', 'gerber', 'soderberg')
            
        Returns:
            Corrected stress ranges
        """
        # Ultimate tensile strength (typical for steel)
        uts = 500.0  # MPa
        yield_strength = 355.0  # MPa
        
        corrected_ranges = np.zeros_like(stress_ranges)
        
        for i, (s_range, s_mean) in enumerate(zip(stress_ranges, mean_stresses)):
            if method == 'goodman':
                # Goodman correction
                corrected = s_range / (1 - s_mean / uts)
            elif method == 'gerber':
                # Gerber parabolic correction
                corrected = s_range / (1 - (s_mean / uts) ** 2)
            elif method == 'soderberg':
                # Soderberg correction (conservative)
                corrected = s_range / (1 - s_mean / yield_strength)
            else:
                corrected = s_range
            
            corrected_ranges[i] = corrected
        
        return corrected_ranges
    
    def get_damage_histogram(self, n_bins: int = 20) -> pd.DataFrame:
        """
        Get histogram of damage contributions
        
        Args:
            n_bins: Number of histogram bins
            
        Returns:
            DataFrame with damage histogram
        """
        if not self.damage_results:
            return pd.DataFrame()
        
        df = pd.DataFrame(self.damage_results)
        
        # Create stress range bins
        df['stress_bin'] = pd.cut(df['stress_range'], bins=n_bins)
        
        # Aggregate by bin
        histogram = df.groupby('stress_bin').agg({
            'cycles': 'sum',
            'damage': 'sum',
            'damage_percent': 'sum'
        }).reset_index()
        
        return histogram
    
    def export_results(self, filename: str):
        """
        Export detailed damage results to CSV
        
        Args:
            filename: Output CSV filename
        """
        if not self.damage_results:
            logger.warning("No damage results to export")
            return
        
        df = pd.DataFrame(self.damage_results)
        
        # Add summary statistics
        with open(filename, 'w') as f:
            f.write(f"# Fatigue Damage Analysis Results\n")
            f.write(f"# S-N Curve: {self.sn_curve.name}\n")
            f.write(f"# SCF: {self.scf}\n")
            f.write(f"# Total Damage: {df['damage'].sum():.6e}\n")
            f.write(f"# Number of Stress Ranges: {len(df)}\n")
            
            df.to_csv(f, index=False)
        
        logger.info(f"Damage results exported to {filename}")


def calculate_combined_damage(configurations: Dict[str, Dict[str, float]],
                              weights: Optional[Dict[str, float]] = None) -> Dict[str, float]:
    """
    Calculate combined fatigue damage for multiple configurations
    
    Args:
        configurations: Dictionary of configuration damages
        weights: Optional weights for each configuration
        
    Returns:
        Combined fatigue life results
    """
    if not configurations:
        return {}
    
    # Default equal weights
    if weights is None:
        weights = {config: 1.0 / len(configurations) 
                  for config in configurations}
    
    # Calculate weighted damage
    total_annual_damage = 0.0
    
    for config, results in configurations.items():
        weight = weights.get(config, 1.0)
        total_annual_damage += results['annual_damage'] * weight
    
    # Calculate combined fatigue life
    if total_annual_damage > 0:
        combined_life = 1.0 / total_annual_damage
    else:
        combined_life = float('inf')
    
    return {
        'combined_annual_damage': total_annual_damage,
        'combined_fatigue_life': combined_life,
        'configurations': configurations,
        'weights': weights
    }


# Example usage
if __name__ == "__main__":
    # Example stress ranges and counts from rainflow
    stress_ranges = np.array([50, 100, 150, 200, 250])  # MPa
    cycle_counts = np.array([1000, 500, 100, 50, 10])
    
    # Initialize calculator
    calculator = FatigueDamageCalculator(
        sn_curve=FatigueDamageCalculator.CURVES['ABS_E_AIR'],
        scf=1.2,
        design_life_years=25
    )
    
    # Calculate fatigue life
    results = calculator.calculate_fatigue_life(
        stress_ranges=stress_ranges,
        cycle_counts=cycle_counts,
        time_duration=200.0,
        occurrence_weight=0.05
    )
    
    print("Fatigue Analysis Results:")
    print(f"  Annual Damage: {results['annual_damage']:.3e}")
    print(f"  Fatigue Life: {results['fatigue_life_years']:.1f} years")
    print(f"  Design Factor: {results['design_factor']:.2f}")
    print(f"  Passes Check: {results['passes_check']}")