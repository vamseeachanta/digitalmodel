"""
Frequency Domain Fatigue Analysis
=================================

This module implements frequency domain methods for fatigue analysis,
particularly useful for random vibration and spectral loading conditions.

Key Features:
- Power Spectral Density (PSD) based fatigue analysis
- Dirlik method for non-Gaussian random processes
- Tovo-Benasciutti method for broadband random loading
- Narrow-band and wide-band approximations
- Single moment and dual moment methods
- Response PSD calculation from input PSD and transfer functions
- Integration with rainflow counting for validation

Author: Digital Model Team
Version: 2.0.0
"""

import numpy as np
import pandas as pd
from typing import Dict, Optional, Union, Tuple, List, Literal, Callable
from dataclasses import dataclass
from abc import ABC, abstractmethod
import logging
import warnings

# Optional scipy import
try:
    from scipy import integrate, interpolate, signal
    SCIPY_AVAILABLE = True
except ImportError:
    SCIPY_AVAILABLE = False
    warnings.warn("SciPy not available. Frequency domain analysis will have limited functionality.")
    # Create dummy modules
    class DummyModule:
        def __getattr__(self, name):
            def dummy_func(*args, **kwargs):
                raise ImportError(f"SciPy not available for {name}")
            return dummy_func

    integrate = DummyModule()
    interpolate = DummyModule()
    signal = DummyModule()

from .sn_curves import SNCurveBase
from .damage_accumulation import LinearDamageAccumulation

logger = logging.getLogger(__name__)

# Type aliases
FrequencyArray = np.ndarray
PSDArray = np.ndarray
TransferFunction = Union[np.ndarray, Callable[[np.ndarray], np.ndarray]]


@dataclass
class SpectralMoments:
    """Spectral moments of a PSD"""
    m0: float  # 0th moment (variance)
    m1: float  # 1st moment
    m2: float  # 2nd moment
    m4: float  # 4th moment
    lambda1: float  # m1/m0 (central frequency)
    lambda2: float  # m2/m0 (rms frequency)
    nu: float  # Irregularity factor
    alpha: float  # Bandwidth parameter
    q: float  # Bandwidth parameter (Vanmarcke)


@dataclass
class FrequencyDomainResult:
    """Results from frequency domain fatigue analysis"""
    method: str
    damage_rate: float  # Damage per unit time
    expected_cycles_per_second: float
    fatigue_life: float  # Time to failure
    equivalent_stress: float  # Equivalent constant amplitude stress
    spectral_moments: SpectralMoments
    parameters: Dict


class FrequencyDomainBase(ABC):
    """Base class for frequency domain fatigue methods"""

    def __init__(self, name: str):
        self.name = name

    @abstractmethod
    def calculate_damage_rate(self,
                             frequency: FrequencyArray,
                             psd: PSDArray,
                             sn_curve: SNCurveBase) -> FrequencyDomainResult:
        """Calculate fatigue damage rate from PSD"""
        pass

    def _calculate_spectral_moments(self,
                                   frequency: FrequencyArray,
                                   psd: PSDArray,
                                   max_moment: int = 4) -> SpectralMoments:
        """
        Calculate spectral moments from PSD

        Parameters
        ----------
        frequency : np.ndarray
            Frequency array (Hz)
        psd : np.ndarray
            Power spectral density
        max_moment : int, default=4
            Maximum moment to calculate

        Returns
        -------
        SpectralMoments
            Calculated spectral moments
        """
        # Ensure positive frequency
        freq = np.asarray(frequency)
        psd_vals = np.asarray(psd)

        # Calculate moments using integration
        moments = {}
        for i in range(max_moment + 1):
            if i == 0:
                integrand = psd_vals
            else:
                integrand = (2 * np.pi * freq) ** i * psd_vals

            moments[f'm{i}'] = np.trapezoid(integrand, freq)

        # Calculate derived parameters
        m0, m1, m2, m4 = moments['m0'], moments['m1'], moments['m2'], moments['m4']

        # Avoid division by zero
        if m0 <= 0:
            logger.warning("Zero or negative variance in PSD")
            m0 = 1e-10

        lambda1 = m1 / m0  # Central frequency
        lambda2 = np.sqrt(m2 / m0)  # RMS frequency

        # Irregularity factor (Rice)
        if m2 > 0:
            nu = np.sqrt(m0 * m2) / m1 if m1 > 0 else 0
        else:
            nu = 0

        # Bandwidth parameters
        if m2 > 0 and m4 > 0:
            alpha = (m2 / np.sqrt(m0 * m4))  # Wirsching-Light
            q = np.sqrt((m1 ** 2) / (m0 * m2))  # Vanmarcke
        else:
            alpha = 1.0
            q = 1.0

        return SpectralMoments(
            m0=float(m0), m1=float(m1), m2=float(m2), m4=float(m4),
            lambda1=float(lambda1), lambda2=float(lambda2),
            nu=float(nu), alpha=float(alpha), q=float(q)
        )

    def _calculate_zero_crossing_rate(self, moments: SpectralMoments) -> float:
        """Calculate expected zero crossing rate"""
        if moments.m0 <= 0 or moments.m2 <= 0:
            return 0.0
        return (1 / (2 * np.pi)) * np.sqrt(moments.m2 / moments.m0)

    def _calculate_peak_rate(self, moments: SpectralMoments) -> float:
        """Calculate expected peak rate"""
        if moments.m0 <= 0 or moments.m4 <= 0:
            return 0.0
        return (1 / (2 * np.pi)) * np.sqrt(moments.m4 / moments.m2)


class NarrowBandMethod(FrequencyDomainBase):
    """
    Narrow-band approximation for frequency domain fatigue

    Assumes all cycles have the same frequency (single-mode response).
    Conservative for broadband processes.
    """

    def __init__(self):
        super().__init__("Narrow-band")

    def calculate_damage_rate(self,
                             frequency: FrequencyArray,
                             psd: PSDArray,
                             sn_curve: SNCurveBase) -> FrequencyDomainResult:
        """Calculate damage rate using narrow-band approximation"""
        moments = self._calculate_spectral_moments(frequency, psd)

        # Expected zero crossing rate (cycles per second)
        zero_crossing_rate = self._calculate_zero_crossing_rate(moments)

        # RMS stress
        sigma_rms = np.sqrt(moments.m0)

        # For narrow-band, all cycles have range = 2 * sigma_rms
        stress_range = 2 * sigma_rms

        # Get allowable cycles from S-N curve
        N_allowable = sn_curve.get_allowable_cycles(stress_range)

        # Damage rate (damage per second)
        if np.isfinite(N_allowable) and N_allowable > 0:
            damage_rate = zero_crossing_rate / N_allowable
            fatigue_life = 1.0 / damage_rate if damage_rate > 0 else np.inf
        else:
            damage_rate = 0.0
            fatigue_life = np.inf

        return FrequencyDomainResult(
            method=self.name,
            damage_rate=damage_rate,
            expected_cycles_per_second=zero_crossing_rate,
            fatigue_life=fatigue_life,
            equivalent_stress=stress_range,
            spectral_moments=moments,
            parameters={'approximation': 'narrow-band'}
        )


class DirlikMethod(FrequencyDomainBase):
    """
    Dirlik method for frequency domain fatigue analysis

    Empirical method for wide-band random processes with good accuracy
    for various bandwidth parameters.
    """

    def __init__(self):
        super().__init__("Dirlik")

    def calculate_damage_rate(self,
                             frequency: FrequencyArray,
                             psd: PSDArray,
                             sn_curve: SNCurveBase) -> FrequencyDomainResult:
        """Calculate damage rate using Dirlik method"""
        moments = self._calculate_spectral_moments(frequency, psd)

        # Dirlik parameters
        nu = self._calculate_zero_crossing_rate(moments)
        np_rate = self._calculate_peak_rate(moments)

        gamma = np_rate / nu if nu > 0 else 1.0
        q = moments.q

        # Dirlik coefficients - need careful handling of edge cases
        xm = moments.m1 / moments.m0 * np.sqrt(moments.m2 / moments.m4) if moments.m4 > 0 else 0.0
        D1 = 2 * (xm - q ** 2) / (1 + q ** 2)
        D1 = max(0.0, min(1.0, D1))  # Clamp to valid range

        # R calculation with safety for division
        denominator = 1 - D1 - q + D1 * q
        if abs(denominator) > 1e-10:
            R1 = (q - D1 - q * D1) / denominator
        else:
            R1 = 0.25  # Default value when denominator is too small

        R1 = max(0.01, min(1.0, R1))  # Clamp R to valid range to avoid numerical issues

        D2_denom = 1 - R1
        if abs(D2_denom) > 1e-10:
            D2 = (1 - D1 - R1) / D2_denom
        else:
            D2 = 0.0

        D2 = max(0.0, D2)
        D3 = max(0.0, 1 - D1 - D2)

        # RMS stress
        sigma = np.sqrt(moments.m0)

        # Numerical integration parameters
        s_max = 8.0  # Maximum stress range (in units of sigma)
        n_points = 1000
        s_normalized = np.linspace(0, s_max, n_points)
        ds = s_normalized[1] - s_normalized[0]

        # Dirlik probability density function
        def dirlik_pdf(s):
            """Dirlik PDF for normalized stress range"""
            if hasattr(s, '__len__'):
                pdf = np.zeros_like(s)
                mask = s > 0
                s_pos = s[mask]
            else:
                if s <= 0:
                    return 0.0
                s_pos = s
                pdf = None

            # Three components of Dirlik formula
            if D1 > 0:
                term1 = (D1 / (2 * R1)) * np.exp(-s_pos / (2 * R1))
            else:
                term1 = 0

            term2 = (D2 * s_pos / (2 ** 1.5)) * np.exp(-s_pos ** 2 / 8)

            if D3 > 0:
                term3 = D3 * s_pos * np.exp(-s_pos ** 2 / 2)
            else:
                term3 = 0

            result = term1 + term2 + term3

            if pdf is not None:
                pdf[mask] = result
                return pdf
            else:
                return result

        # Calculate PDF values
        pdf_values = dirlik_pdf(s_normalized)

        # Calculate damage rate by integration
        damage_rate = 0.0
        for i, s_norm in enumerate(s_normalized):
            if s_norm <= 0 or pdf_values[i] <= 0:
                continue

            # Actual stress range
            stress_range = s_norm * sigma

            # Get allowable cycles
            N_allowable = sn_curve.get_allowable_cycles(stress_range)

            if np.isfinite(N_allowable) and N_allowable > 0:
                # Damage contribution
                damage_rate += nu * pdf_values[i] * ds / N_allowable

        fatigue_life = 1.0 / damage_rate if damage_rate > 0 else np.inf

        # Calculate equivalent stress for reference
        if hasattr(sn_curve, 'm'):
            m = sn_curve.m
        else:
            m = 3.0  # Default slope

        # Equivalent stress using moment method
        moment_ratio = np.trapezoid(
            pdf_values * (s_normalized ** m), s_normalized
        ) * ds

        # Calculate equivalent stress with safety check for non-positive values
        if moment_ratio > 0 and nu > 0:
            equivalent_stress = sigma * (moment_ratio * nu) ** (1 / m)
        else:
            equivalent_stress = 2 * sigma  # Default to narrow-band approximation

        return FrequencyDomainResult(
            method=self.name,
            damage_rate=damage_rate,
            expected_cycles_per_second=nu,
            fatigue_life=fatigue_life,
            equivalent_stress=equivalent_stress,
            spectral_moments=moments,
            parameters={
                'D1': D1, 'D2': D2, 'D3': D3, 'R1': R1,
                'gamma': gamma, 'q': q
            }
        )


class TovoBenasciuttiMethod(FrequencyDomainBase):
    """
    Tovo-Benasciutti method for frequency domain fatigue

    Weighted combination of narrow-band and Wirsching-Light corrections.
    """

    def __init__(self):
        super().__init__("Tovo-Benasciutti")

    def calculate_damage_rate(self,
                             frequency: FrequencyArray,
                             psd: PSDArray,
                             sn_curve: SNCurveBase) -> FrequencyDomainResult:
        """Calculate damage rate using Tovo-Benasciutti method"""
        moments = self._calculate_spectral_moments(frequency, psd)

        # Get S-N curve slope
        if hasattr(sn_curve, 'm'):
            m = sn_curve.m
        else:
            m = 3.0

        # Spectral parameters
        alpha1 = moments.lambda1 / np.sqrt(moments.m0 * moments.m2) if moments.m2 > 0 else 1.0
        alpha2 = moments.lambda2 / np.sqrt(moments.m2) if moments.m2 > 0 else 1.0

        # Bandwidth parameter
        alpha075 = (moments.m1 ** 2) / (moments.m0 * moments.m2) if moments.m2 > 0 else 1.0

        # Expected rates
        nu = self._calculate_zero_crossing_rate(moments)
        np_rate = self._calculate_peak_rate(moments)

        # RMS stress
        sigma = np.sqrt(moments.m0)

        # Narrow-band damage rate
        stress_range_nb = 2 * sigma
        N_allowable_nb = sn_curve.get_allowable_cycles(stress_range_nb)

        if np.isfinite(N_allowable_nb) and N_allowable_nb > 0:
            damage_rate_nb = nu / N_allowable_nb
        else:
            damage_rate_nb = 0.0

        # Wirsching-Light correction factor
        a = 0.926 - 0.033 * m
        b = 1.587 * m - 2.323

        if alpha075 > 0:
            correction_wl = a + (1 - a) * (alpha075 ** b)
        else:
            correction_wl = 1.0

        damage_rate_wl = correction_wl * damage_rate_nb

        # Tovo-Benasciutti weighting factor
        if alpha075 < 1.0:
            w = (alpha075 - alpha1) / (1 - alpha1) if alpha1 < 1.0 else 0.0
        else:
            w = 1.0

        w = max(0.0, min(1.0, w))  # Ensure 0 <= w <= 1

        # Combined damage rate
        damage_rate = w * damage_rate_wl + (1 - w) * damage_rate_nb

        fatigue_life = 1.0 / damage_rate if damage_rate > 0 else np.inf

        # Equivalent stress
        if damage_rate > 0:
            equivalent_cycles_per_sec = nu
            # Solve for equivalent stress: damage_rate = nu / N_eq
            if hasattr(sn_curve, 'A'):
                A = sn_curve.A
                equivalent_stress = (A * nu / damage_rate) ** (1 / m)
            else:
                equivalent_stress = stress_range_nb
        else:
            equivalent_stress = 0.0

        return FrequencyDomainResult(
            method=self.name,
            damage_rate=damage_rate,
            expected_cycles_per_second=nu,
            fatigue_life=fatigue_life,
            equivalent_stress=equivalent_stress,
            spectral_moments=moments,
            parameters={
                'weighting_factor': w,
                'alpha075': alpha075,
                'correction_factor': correction_wl,
                'narrowband_damage': damage_rate_nb,
                'wirsching_light_damage': damage_rate_wl
            }
        )


class SingleMomentMethod(FrequencyDomainBase):
    """
    Single moment method using only m0 and m2

    Simple method suitable for preliminary analysis.
    """

    def __init__(self):
        super().__init__("Single-Moment")

    def calculate_damage_rate(self,
                             frequency: FrequencyArray,
                             psd: PSDArray,
                             sn_curve: SNCurveBase) -> FrequencyDomainResult:
        """Calculate damage rate using single moment method"""
        moments = self._calculate_spectral_moments(frequency, psd, max_moment=2)

        # Expected zero crossing rate
        nu = self._calculate_zero_crossing_rate(moments)

        # RMS stress
        sigma = np.sqrt(moments.m0)

        # Get S-N curve parameters
        if hasattr(sn_curve, 'm'):
            m = sn_curve.m
        else:
            m = 3.0

        # Single moment approximation for damage rate
        # Based on Rayleigh distribution assumption
        stress_range_factor = 2 * sigma * (2 * np.log(2)) ** (1 / 2)

        # Calculate damage rate using gamma function approximation
        from scipy.special import gamma

        gamma_factor = gamma(1 + m / 2)
        effective_stress = stress_range_factor * (gamma_factor ** (1 / m))

        N_allowable = sn_curve.get_allowable_cycles(effective_stress)

        if np.isfinite(N_allowable) and N_allowable > 0:
            damage_rate = nu * gamma_factor / N_allowable
        else:
            damage_rate = 0.0

        fatigue_life = 1.0 / damage_rate if damage_rate > 0 else np.inf

        return FrequencyDomainResult(
            method=self.name,
            damage_rate=damage_rate,
            expected_cycles_per_second=nu,
            fatigue_life=fatigue_life,
            equivalent_stress=effective_stress,
            spectral_moments=moments,
            parameters={
                'gamma_factor': gamma_factor,
                'stress_range_factor': stress_range_factor
            }
        )


class ResponsePSDCalculator:
    """
    Calculate response PSD from input PSD and transfer function

    Used for structural response analysis under random excitation.
    """

    def __init__(self):
        self.last_calculation = None

    def calculate_response_psd(self,
                              input_frequency: FrequencyArray,
                              input_psd: PSDArray,
                              transfer_function: TransferFunction,
                              response_frequency: Optional[FrequencyArray] = None) -> Tuple[FrequencyArray, PSDArray]:
        """
        Calculate response PSD from input PSD and transfer function

        Parameters
        ----------
        input_frequency : np.ndarray
            Input frequency array
        input_psd : np.ndarray
            Input power spectral density
        transfer_function : callable or np.ndarray
            Transfer function H(f) or array of |H(f)|^2
        response_frequency : np.ndarray, optional
            Desired response frequency array

        Returns
        -------
        freq_response : np.ndarray
            Response frequency array
        psd_response : np.ndarray
            Response power spectral density
        """
        # Use input frequency if response frequency not specified
        if response_frequency is None:
            freq_response = input_frequency.copy()
        else:
            freq_response = np.asarray(response_frequency)

        # Interpolate input PSD to response frequency grid
        psd_interp = interpolate.interp1d(
            input_frequency, input_psd,
            kind='linear', bounds_error=False, fill_value=0.0
        )
        psd_input_interp = psd_interp(freq_response)

        # Calculate transfer function values
        if callable(transfer_function):
            H_values = transfer_function(freq_response)
            # Assume transfer function returns complex values
            H_squared = np.abs(H_values) ** 2
        else:
            # Assume already |H(f)|^2
            if len(transfer_function) != len(freq_response):
                # Interpolate transfer function
                tf_interp = interpolate.interp1d(
                    input_frequency, transfer_function,
                    kind='linear', bounds_error=False, fill_value=0.0
                )
                H_squared = tf_interp(freq_response)
            else:
                H_squared = np.asarray(transfer_function)

        # Calculate response PSD
        psd_response = psd_input_interp * H_squared

        # Store calculation for reference
        self.last_calculation = {
            'input_frequency': input_frequency,
            'input_psd': input_psd,
            'response_frequency': freq_response,
            'response_psd': psd_response,
            'transfer_function_squared': H_squared
        }

        return freq_response, psd_response

    def sdof_transfer_function(self,
                              frequency: FrequencyArray,
                              natural_frequency: float,
                              damping_ratio: float) -> np.ndarray:
        """
        Single degree of freedom transfer function

        Parameters
        ----------
        frequency : np.ndarray
            Frequency array
        natural_frequency : float
            Natural frequency (Hz)
        damping_ratio : float
            Damping ratio (0 < zeta < 1)

        Returns
        -------
        np.ndarray
            Complex transfer function values
        """
        omega = 2 * np.pi * frequency
        omega_n = 2 * np.pi * natural_frequency

        # Transfer function for displacement response
        H = 1 / (omega_n ** 2 - omega ** 2 + 2j * damping_ratio * omega_n * omega)

        return H

    def mdof_transfer_function(self,
                              frequency: FrequencyArray,
                              modal_frequencies: List[float],
                              modal_damping: List[float],
                              mode_shapes: Optional[np.ndarray] = None) -> np.ndarray:
        """
        Multi degree of freedom transfer function (modal superposition)

        Parameters
        ----------
        frequency : np.ndarray
            Frequency array
        modal_frequencies : list
            Natural frequencies for each mode
        modal_damping : list
            Damping ratios for each mode
        mode_shapes : np.ndarray, optional
            Mode shape participation factors

        Returns
        -------
        np.ndarray
            Complex transfer function values
        """
        H_total = np.zeros(len(frequency), dtype=complex)

        for i, (fn, zeta) in enumerate(zip(modal_frequencies, modal_damping)):
            # Modal transfer function
            H_modal = self.sdof_transfer_function(frequency, fn, zeta)

            # Apply mode shape participation if provided
            if mode_shapes is not None:
                participation = mode_shapes[i] if len(mode_shapes) > i else 1.0
            else:
                participation = 1.0

            H_total += participation * H_modal

        return H_total


def compare_frequency_methods(frequency: FrequencyArray,
                             psd: PSDArray,
                             sn_curve: SNCurveBase) -> pd.DataFrame:
    """
    Compare different frequency domain fatigue methods

    Parameters
    ----------
    frequency : np.ndarray
        Frequency array
    psd : np.ndarray
        Power spectral density
    sn_curve : SNCurveBase
        S-N curve for analysis

    Returns
    -------
    pd.DataFrame
        Comparison of methods
    """
    methods = [
        NarrowBandMethod(),
        DirlikMethod(),
        TovoBenasciuttiMethod(),
        SingleMomentMethod()
    ]

    results = []

    for method in methods:
        try:
            result = method.calculate_damage_rate(frequency, psd, sn_curve)
            results.append({
                'method': result.method,
                'damage_rate': result.damage_rate,
                'fatigue_life': result.fatigue_life,
                'equivalent_stress': result.equivalent_stress,
                'cycles_per_second': result.expected_cycles_per_second
            })
        except Exception as e:
            logger.error(f"Error in {method.name}: {e}")

    return pd.DataFrame(results)


if __name__ == "__main__":
    # Example usage and testing
    import matplotlib.pyplot as plt
    from .sn_curves import get_dnv_curve

    # Create test PSD (broadband random process)
    freq = np.linspace(1, 100, 1000)

    # White noise with resonance peak
    psd_base = 1e-3 * np.ones_like(freq)  # Base level

    # Add resonance peak at 20 Hz
    resonance_freq = 20.0
    Q = 50  # Quality factor
    resonance_peak = 1.0 / (1 + Q**2 * (freq/resonance_freq - resonance_freq/freq)**2)
    psd = psd_base + 0.1 * resonance_peak

    # Get S-N curve
    sn_curve = get_dnv_curve('D')

    # Compare methods
    comparison = compare_frequency_methods(freq, psd, sn_curve)

    print("Frequency Domain Fatigue Analysis Comparison:")
    print(comparison)

    # Test response PSD calculation
    calc = ResponsePSDCalculator()

    # SDOF system at resonance
    H = calc.sdof_transfer_function(freq, 20.0, 0.05)
    freq_resp, psd_resp = calc.calculate_response_psd(freq, psd, H)

    # Plot results
    fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(10, 8))

    # Input and response PSD
    ax1.loglog(freq, psd, 'b-', label='Input PSD')
    ax1.loglog(freq_resp, psd_resp, 'r-', label='Response PSD')
    ax1.set_xlabel('Frequency (Hz)')
    ax1.set_ylabel('PSD')
    ax1.set_title('Input vs Response PSD')
    ax1.legend()
    ax1.grid(True, alpha=0.3)

    # Transfer function
    ax2.loglog(freq, np.abs(H)**2)
    ax2.set_xlabel('Frequency (Hz)')
    ax2.set_ylabel('|H(f)|²')
    ax2.set_title('Transfer Function')
    ax2.grid(True, alpha=0.3)

    plt.tight_layout()
    plt.show()

    print("Frequency domain fatigue module test completed successfully!")