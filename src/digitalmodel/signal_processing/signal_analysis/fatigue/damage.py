"""
Fatigue damage calculation module

Implements various fatigue damage accumulation methods and S-N curve models.
"""

import numpy as np
import pandas as pd
from typing import Union, Dict, Optional, Tuple
import logging

logger = logging.getLogger(__name__)


class FatigueDamageCalculator:
    """
    Fatigue damage calculator using various methods
    
    Supports Miner's rule, modified Miner's rule, and other damage accumulation methods.
    """
    
    def __init__(self, method: str = 'miners'):
        """
        Initialize fatigue damage calculator
        
        Parameters
        ----------
        method : str, default='miners'
            Damage accumulation method: 'miners', 'modified_miners', 'nonlinear'
        """
        self.method = method
        self.damage_history = []
    
    def calculate_damage(self, cycles_df: pd.DataFrame, 
                        sn_curve: Dict,
                        mean_stress_correction: Optional[str] = None) -> Dict:
        """
        Calculate fatigue damage from rainflow cycles
        
        Parameters
        ----------
        cycles_df : pd.DataFrame
            Rainflow cycles with columns: range, mean, count
        sn_curve : dict
            S-N curve parameters
        mean_stress_correction : str, optional
            Mean stress correction: None, 'goodman', 'soderberg', 'gerber'
            
        Returns
        -------
        dict
            Damage results including total damage and life fraction
        """
        logger.info(f"Calculating fatigue damage using {self.method} method")
        
        # Apply mean stress correction if specified
        if mean_stress_correction:
            cycles_df = self._apply_mean_stress_correction(
                cycles_df, sn_curve, mean_stress_correction
            )
        
        # Calculate damage based on method
        if self.method == 'miners':
            damage = self._miners_rule(cycles_df, sn_curve)
        elif self.method == 'modified_miners':
            damage = self._modified_miners_rule(cycles_df, sn_curve)
        elif self.method == 'nonlinear':
            damage = self._nonlinear_damage(cycles_df, sn_curve)
        else:
            raise ValueError(f"Unknown damage method: {self.method}")
        
        # Calculate life metrics
        results = {
            'total_damage': damage,
            'life_fraction_used': damage,
            'life_fraction_remaining': max(0, 1 - damage),
            'cycles_to_failure': self._estimate_cycles_to_failure(cycles_df, sn_curve, damage),
            'safety_factor': 1.0 / damage if damage > 0 else float('inf')
        }
        
        # Store in history
        self.damage_history.append(results)
        
        return results
    
    def _miners_rule(self, cycles_df: pd.DataFrame, sn_curve: Dict) -> float:
        """
        Calculate damage using Miner's rule (linear damage accumulation)
        
        Parameters
        ----------
        cycles_df : pd.DataFrame
            Rainflow cycles
        sn_curve : dict
            S-N curve parameters
            
        Returns
        -------
        float
            Total damage
        """
        total_damage = 0.0
        
        for _, cycle in cycles_df.iterrows():
            stress_range = cycle['range']
            n_cycles = cycle['count']
            
            # Calculate allowable cycles from S-N curve
            N_allowable = self._calculate_allowable_cycles(stress_range, sn_curve)
            
            # Accumulate damage
            if N_allowable > 0:
                damage = n_cycles / N_allowable
                total_damage += damage
        
        return total_damage
    
    def _modified_miners_rule(self, cycles_df: pd.DataFrame, sn_curve: Dict) -> float:
        """
        Calculate damage using modified Miner's rule
        
        Accounts for load sequence effects.
        
        Parameters
        ----------
        cycles_df : pd.DataFrame
            Rainflow cycles
        sn_curve : dict
            S-N curve parameters
            
        Returns
        -------
        float
            Total damage
        """
        # Sort cycles by range (largest first for conservative estimate)
        sorted_cycles = cycles_df.sort_values('range', ascending=False).copy()
        
        total_damage = 0.0
        damage_factor = 1.0  # Modification factor
        
        for _, cycle in sorted_cycles.iterrows():
            stress_range = cycle['range']
            n_cycles = cycle['count']
            
            # Apply modification factor based on accumulated damage
            if total_damage > 0.3:
                damage_factor = 1.2  # Accelerated damage after 30%
            
            # Calculate allowable cycles
            N_allowable = self._calculate_allowable_cycles(stress_range, sn_curve)
            
            # Accumulate damage with modification
            if N_allowable > 0:
                damage = damage_factor * n_cycles / N_allowable
                total_damage += damage
        
        return total_damage
    
    def _nonlinear_damage(self, cycles_df: pd.DataFrame, sn_curve: Dict) -> float:
        """
        Calculate damage using nonlinear damage accumulation
        
        Parameters
        ----------
        cycles_df : pd.DataFrame
            Rainflow cycles
        sn_curve : dict
            S-N curve parameters
            
        Returns
        -------
        float
            Total damage
        """
        # Implement Marco-Starkey or similar nonlinear model
        total_damage = 0.0
        
        # Sort by increasing stress range
        sorted_cycles = cycles_df.sort_values('range').copy()
        
        for _, cycle in sorted_cycles.iterrows():
            stress_range = cycle['range']
            n_cycles = cycle['count']
            
            # Calculate allowable cycles
            N_allowable = self._calculate_allowable_cycles(stress_range, sn_curve)
            
            if N_allowable > 0:
                # Nonlinear damage function
                damage_ratio = n_cycles / N_allowable
                
                # Marco-Starkey model: D = (n/N)^alpha
                alpha = 0.4 * (stress_range / sn_curve.get('fatigue_limit', 100))
                damage = damage_ratio ** alpha
                
                total_damage += damage
        
        return total_damage
    
    def _calculate_allowable_cycles(self, stress_range: float, sn_curve: Dict) -> float:
        """
        Calculate allowable cycles from S-N curve
        
        Parameters
        ----------
        stress_range : float
            Stress range
        sn_curve : dict
            S-N curve parameters
            
        Returns
        -------
        float
            Allowable number of cycles
        """
        curve_type = sn_curve.get('type', 'power_law')
        
        if curve_type == 'power_law':
            # N = A * S^(-m)
            A = sn_curve.get('A', 1e12)
            m = sn_curve.get('m', 3.0)
            
            # Check fatigue limit
            fatigue_limit = sn_curve.get('fatigue_limit', 0)
            if stress_range <= fatigue_limit:
                return float('inf')
            
            N = A * (stress_range ** (-m))
            
        elif curve_type == 'bilinear':
            # Two-slope S-N curve
            A1 = sn_curve.get('A1', 1e12)
            m1 = sn_curve.get('m1', 3.0)
            A2 = sn_curve.get('A2', 1e15)
            m2 = sn_curve.get('m2', 5.0)
            N_transition = sn_curve.get('N_transition', 1e6)
            S_transition = (A1 / N_transition) ** (1 / m1)
            
            if stress_range > S_transition:
                N = A1 * (stress_range ** (-m1))
            else:
                N = A2 * (stress_range ** (-m2))
                
        elif curve_type == 'basquin':
            # Basquin's equation: S = sigma_f * (2N)^b
            sigma_f = sn_curve.get('sigma_f', 1000)  # Fatigue strength coefficient
            b = sn_curve.get('b', -0.1)  # Fatigue strength exponent
            
            N = 0.5 * ((stress_range / sigma_f) ** (1 / b))
            
        else:
            raise ValueError(f"Unknown S-N curve type: {curve_type}")
        
        return max(1, N)  # Ensure at least 1 cycle
    
    def _apply_mean_stress_correction(self, cycles_df: pd.DataFrame,
                                     sn_curve: Dict,
                                     method: str) -> pd.DataFrame:
        """
        Apply mean stress correction to cycles
        
        Parameters
        ----------
        cycles_df : pd.DataFrame
            Rainflow cycles
        sn_curve : dict
            S-N curve parameters
        method : str
            Correction method: 'goodman', 'soderberg', 'gerber'
            
        Returns
        -------
        pd.DataFrame
            Corrected cycles
        """
        corrected_df = cycles_df.copy()
        
        # Material properties
        S_ut = sn_curve.get('ultimate_strength', 1000)
        S_y = sn_curve.get('yield_strength', 800)
        
        for idx, cycle in corrected_df.iterrows():
            S_a = cycle['range'] / 2  # Alternating stress
            S_m = cycle['mean']  # Mean stress
            
            if method == 'goodman':
                # Modified Goodman: S_a_eq = S_a / (1 - S_m/S_ut)
                if S_m < S_ut:
                    S_a_eq = S_a / (1 - S_m / S_ut)
                else:
                    S_a_eq = float('inf')
                    
            elif method == 'soderberg':
                # Soderberg: S_a_eq = S_a / (1 - S_m/S_y)
                if S_m < S_y:
                    S_a_eq = S_a / (1 - S_m / S_y)
                else:
                    S_a_eq = float('inf')
                    
            elif method == 'gerber':
                # Gerber: S_a_eq = S_a / (1 - (S_m/S_ut)^2)
                if S_m < S_ut:
                    S_a_eq = S_a / (1 - (S_m / S_ut) ** 2)
                else:
                    S_a_eq = float('inf')
                    
            else:
                raise ValueError(f"Unknown mean stress correction: {method}")
            
            # Update equivalent stress range
            corrected_df.loc[idx, 'range'] = 2 * S_a_eq
        
        return corrected_df
    
    def _estimate_cycles_to_failure(self, cycles_df: pd.DataFrame,
                                   sn_curve: Dict,
                                   current_damage: float) -> float:
        """
        Estimate remaining cycles to failure
        
        Parameters
        ----------
        cycles_df : pd.DataFrame
            Current loading cycles
        sn_curve : dict
            S-N curve parameters
        current_damage : float
            Current damage level
            
        Returns
        -------
        float
            Estimated cycles to failure
        """
        if current_damage >= 1.0:
            return 0
        
        if current_damage == 0:
            return float('inf')
        
        # Calculate damage rate per cycle
        total_cycles = cycles_df['count'].sum()
        if total_cycles > 0:
            damage_rate = current_damage / total_cycles
            remaining_damage = 1.0 - current_damage
            cycles_to_failure = remaining_damage / damage_rate
        else:
            cycles_to_failure = float('inf')
        
        return cycles_to_failure
    
    def calculate_equivalent_stress(self, cycles_df: pd.DataFrame,
                                   reference_cycles: float = 2e6) -> float:
        """
        Calculate equivalent constant amplitude stress
        
        Parameters
        ----------
        cycles_df : pd.DataFrame
            Rainflow cycles
        reference_cycles : float
            Reference number of cycles
            
        Returns
        -------
        float
            Equivalent stress range
        """
        # Using inverse power law
        m = 3.0  # Default slope
        
        numerator = 0
        denominator = 0
        
        for _, cycle in cycles_df.iterrows():
            stress_range = cycle['range']
            n_cycles = cycle['count']
            
            numerator += n_cycles * (stress_range ** m)
            denominator += n_cycles
        
        if denominator > 0:
            S_eq = (numerator / reference_cycles) ** (1 / m)
        else:
            S_eq = 0
        
        return S_eq