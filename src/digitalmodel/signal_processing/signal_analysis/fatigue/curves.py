"""
S-N curve implementations for fatigue analysis

Provides various S-N curve models and material databases.
"""

import numpy as np
import pandas as pd
from typing import Dict, Optional, Union, Tuple
import logging

from digitalmodel.fatigue import c203_sn_tables as _c203

logger = logging.getLogger(__name__)


class SNCurve:
    """
    S-N curve model for fatigue analysis
    
    Supports various curve types and standards (DNV, API, BS, etc.)
    """

    # Standard DNV curves (DNV-RP-C203), in air, from
    # digitalmodel.fatigue.c203_sn_tables, verified against DNV-RP-C203 (2011)
    # Table 2-1 (#2165): A = 10^log a1, m = m1 above the knee at 1e7 cycles,
    # A2 = 10^log a2 (tabulated), m2 = 5 below it. No cut-off: DNV-RP-C203
    # (2011) section 2.4 has none for variable-amplitude loading; the tabulated
    # fatigue limit at 1e7 cycles is kept for reporting only. A1/m1 repeat
    # A/m so that FatigueDamageCalculator reads the same curve from
    # get_curve_parameters().
    DNV_CURVES = {
        name: {
            "A": 10.0**p.log_a1_air,
            "m": p.m1,
            "A1": 10.0**p.log_a1_air,
            "m1": p.m1,
            "A2": 10.0**p.log_a2,
            "m2": p.m2,
            "fatigue_limit": p.fatigue_limit_mpa,
            "N_transition": _c203.N_KNEE_AIR,
            "cutoff": False,
        }
        for name, p in _c203.BILINEAR.items()
    }

    # API RP 2A curves
    API_CURVES = {
        'X': {'A': 1.01e12, 'm': 3.0, 'fatigue_limit': 48.0},
        'X_prime': {'A': 1.52e11, 'm': 3.0, 'fatigue_limit': 32.7},
        'WJ': {'A': 4.24e11, 'm': 3.17, 'fatigue_limit': 0},  # Welded joints
    }

    # BS 7608 curves
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

    def __init__(self, curve_type: str = 'power_law', **params):
        """
        Initialize S-N curve
        
        Parameters
        ----------
        curve_type : str
            Type of curve: 'power_law', 'bilinear', 'basquin', 'standard'
        **params
            Curve parameters or standard curve name
        """
        self.curve_type = curve_type
        self.params = params

        # Load standard curve if specified
        if curve_type == 'standard':
            self._load_standard_curve()

    def _load_standard_curve(self):
        """Load parameters for standard curve"""
        standard = self.params.get('standard', 'DNV')
        curve_class = self.params.get('class', 'D')

        if standard.upper() == 'DNV':
            if curve_class in self.DNV_CURVES:
                self.params.update(self.DNV_CURVES[curve_class])
                self.curve_type = 'bilinear'  # DNV curves are bilinear
            else:
                raise ValueError(f"Unknown DNV curve class: {curve_class}")

        elif standard.upper() == 'API':
            if curve_class in self.API_CURVES:
                self.params.update(self.API_CURVES[curve_class])
                self.curve_type = 'power_law'
            else:
                raise ValueError(f"Unknown API curve class: {curve_class}")

        elif standard.upper() == 'BS':
            if curve_class in self.BS_CURVES:
                self.params.update(self.BS_CURVES[curve_class])
                self.curve_type = 'power_law'
            else:
                raise ValueError(f"Unknown BS curve class: {curve_class}")
        else:
            raise ValueError(f"Unknown standard: {standard}")

        logger.info(f"Loaded {standard} {curve_class} curve")

    def get_allowable_cycles(self, stress_range: Union[float, np.ndarray]) -> Union[float, np.ndarray]:
        """
        Calculate allowable cycles for given stress range
        
        Parameters
        ----------
        stress_range : float or array-like
            Stress range(s) in MPa
            
        Returns
        -------
        float or array
            Allowable number of cycles
        """
        stress_range = np.asarray(stress_range)

        if self.curve_type == 'power_law':
            N = self._power_law(stress_range)
        elif self.curve_type == 'bilinear':
            N = self._bilinear(stress_range)
        elif self.curve_type == 'basquin':
            N = self._basquin(stress_range)
        else:
            raise ValueError(f"Unknown curve type: {self.curve_type}")

        return N

    def _power_law(self, S: np.ndarray) -> np.ndarray:
        """
        Power law S-N curve: N = A * S^(-m)
        
        Parameters
        ----------
        S : np.ndarray
            Stress range
            
        Returns
        -------
        np.ndarray
            Allowable cycles
        """
        A = self.params.get('A', 1e12)
        m = self.params.get('m', 3.0)
        fatigue_limit = self.params.get('fatigue_limit', 0)

        N = np.zeros_like(S)

        # Above fatigue limit
        above_limit = S > fatigue_limit
        N[above_limit] = A * (S[above_limit] ** (-m))

        # Below fatigue limit - infinite life
        N[~above_limit] = np.inf

        return N

    def _bilinear(self, S: np.ndarray) -> np.ndarray:
        """
        Bilinear S-N curve (two slopes)
        
        Parameters
        ----------
        S : np.ndarray
            Stress range
            
        Returns
        -------
        np.ndarray
            Allowable cycles
        """
        A = self.params.get('A', 1e12)
        m = self.params.get('m', 3.0)
        fatigue_limit = self.params.get('fatigue_limit', 0)
        N_transition = self.params.get('N_transition', 1e7)

        # Second slope parameters (for high cycle fatigue)
        m2 = self.params.get('m2', 5.0)

        # Calculate transition stress
        S_transition = (A / N_transition) ** (1 / m)

        N = np.zeros_like(S)

        # High stress regime (low cycle)
        high_stress = S > S_transition
        N[high_stress] = A * (S[high_stress] ** (-m))

        # A curve without a cut-off (DNV-RP-C203) continues on the second
        # segment all the way down; otherwise the fatigue limit applies.
        cutoff = self.params.get("cutoff", True)
        if not cutoff:
            fatigue_limit = 0.0

        # Medium stress regime (high cycle)
        medium_stress = (S <= S_transition) & (S > fatigue_limit)
        if "A2" in self.params:
            N[medium_stress] = self.params["A2"] * (S[medium_stress] ** (-m2))
        elif m2 != m:
            # Calculate A2 to ensure continuity
            A2 = N_transition * (S_transition ** m2)
            N[medium_stress] = A2 * (S[medium_stress] ** (-m2))
        else:
            N[medium_stress] = A * (S[medium_stress] ** (-m))

        # Below fatigue limit
        N[S <= fatigue_limit] = np.inf

        return N

    def _basquin(self, S: np.ndarray) -> np.ndarray:
        """
        Basquin's equation: S = sigma_f * (2N)^b
        
        Parameters
        ----------
        S : np.ndarray
            Stress range
            
        Returns
        -------
        np.ndarray
            Allowable cycles
        """
        sigma_f = self.params.get('sigma_f', 1000)  # Fatigue strength coefficient
        b = self.params.get('b', -0.1)  # Fatigue strength exponent

        N = 0.5 * ((S / sigma_f) ** (1 / b))

        return N

    def get_stress_range(self, n_cycles: Union[float, np.ndarray]) -> Union[float, np.ndarray]:
        """
        Calculate stress range for given number of cycles (inverse S-N)
        
        Parameters
        ----------
        n_cycles : float or array-like
            Number of cycles
            
        Returns
        -------
        float or array
            Stress range in MPa
        """
        n_cycles = np.asarray(n_cycles)

        if self.curve_type == 'power_law':
            A = self.params.get('A', 1e12)
            m = self.params.get('m', 3.0)
            S = (A / n_cycles) ** (1 / m)

        elif self.curve_type == 'bilinear':
            A = self.params.get('A', 1e12)
            m = self.params.get('m', 3.0)
            N_transition = self.params.get('N_transition', 1e7)
            m2 = self.params.get('m2', 5.0)

            S_transition = (A / N_transition) ** (1 / m)

            S = np.zeros_like(n_cycles, dtype=float)

            # Low cycle regime
            low_cycle = n_cycles <= N_transition
            S[low_cycle] = (A / n_cycles[low_cycle]) ** (1 / m)

            # High cycle regime
            high_cycle = n_cycles > N_transition
            if "A2" in self.params:
                S[high_cycle] = (self.params["A2"] / n_cycles[high_cycle]) ** (1 / m2)
            elif m2 != m:
                A2 = N_transition * (S_transition ** m2)
                S[high_cycle] = (A2 / n_cycles[high_cycle]) ** (1 / m2)
            else:
                S[high_cycle] = (A / n_cycles[high_cycle]) ** (1 / m)

        elif self.curve_type == 'basquin':
            sigma_f = self.params.get('sigma_f', 1000)
            b = self.params.get('b', -0.1)
            S = sigma_f * (2 * n_cycles) ** b

        else:
            raise ValueError(f"Unknown curve type: {self.curve_type}")

        return S

    def plot_curve(self, n_min: float = 1e3, n_max: float = 1e8,
                  stress_unit: str = 'MPa') -> Tuple[np.ndarray, np.ndarray]:
        """
        Generate data for plotting S-N curve
        
        Parameters
        ----------
        n_min : float
            Minimum number of cycles
        n_max : float
            Maximum number of cycles
        stress_unit : str
            Stress unit for display
            
        Returns
        -------
        n_cycles : np.ndarray
            Cycle array
        stress_range : np.ndarray
            Stress range array
        """
        n_cycles = np.logspace(np.log10(n_min), np.log10(n_max), 100)
        stress_range = self.get_stress_range(n_cycles)

        # Remove infinite life points for plotting
        finite_mask = np.isfinite(stress_range)
        n_cycles = n_cycles[finite_mask]
        stress_range = stress_range[finite_mask]

        return n_cycles, stress_range

    def get_curve_parameters(self) -> Dict:
        """
        Get curve parameters dictionary
        
        Returns
        -------
        dict
            Curve parameters
        """
        return {
            'type': self.curve_type,
            **self.params
        }

    @classmethod
    def from_test_data(cls, test_data: pd.DataFrame, 
                       curve_type: str = 'power_law') -> 'SNCurve':
        """
        Fit S-N curve from test data
        
        Parameters
        ----------
        test_data : pd.DataFrame
            Test data with columns: stress_range, cycles_to_failure
        curve_type : str
            Type of curve to fit
            
        Returns
        -------
        SNCurve
            Fitted S-N curve
        """
        # Log-log linear regression for power law
        log_S = np.log10(test_data['stress_range'].values)
        log_N = np.log10(test_data['cycles_to_failure'].values)

        # Fit: log(N) = log(A) - m * log(S)
        coeffs = np.polyfit(log_S, log_N, 1)
        m = -coeffs[0]
        A = 10 ** coeffs[1]

        # Estimate fatigue limit (if data available)
        if len(test_data) > 10:
            # Use 10% lowest stress as estimate
            fatigue_limit = test_data['stress_range'].quantile(0.1)
        else:
            fatigue_limit = 0

        params = {
            'A': A,
            'm': m,
            'fatigue_limit': fatigue_limit
        }

        logger.info(f"Fitted S-N curve: A={A:.2e}, m={m:.2f}")

        return cls(curve_type=curve_type, **params)
