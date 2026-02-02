#!/usr/bin/env python3
"""
ABOUTME: VIV-induced fatigue damage calculator using stress ranges from VIV
amplitudes and S-N curve methodology per DNV standards.
"""

import numpy as np
from typing import Optional
import logging

from .models import (
    TubularMember,
    VIVFatigueResult
)

logger = logging.getLogger(__name__)


class VIVFatigueCalculator:
    """
    Calculate VIV-induced fatigue damage.

    Uses S-N curve methodology similar to fatigue_analysis module,
    but specific to VIV-induced cyclic stresses.
    """

    # S-N curve parameters (DNV-RP-C203)
    # log(N) = log(a) - m * log(S)
    SN_CURVES = {
        # Curve: (log_a, m, S_ref)
        'DNV-B1': (15.117, 4.0, 106.97),  # Welded connections
        'DNV-C': (14.832, 3.5, 106.97),
        'DNV-C1': (14.477, 3.0, 106.97),
        'DNV-C2': (13.957, 3.0, 106.97),
        'DNV-D': (14.885, 3.0, 106.97),    # Tubular joints
        'DNV-E': (14.265, 3.0, 106.97),
        'DNV-F': (13.635, 3.0, 106.97),
        'DNV-F1': (13.195, 3.0, 106.97),
        'DNV-F3': (12.476, 3.0, 106.97),
        'DNV-G': (13.845, 3.0, 106.97),
        'DNV-W1': (13.617, 3.0, 106.97),   # Offshore wind
        'DNV-W2': (13.155, 3.0, 106.97),
        'DNV-W3': (12.436, 3.0, 106.97),
    }

    def __init__(self):
        """Initialize VIV fatigue calculator."""
        pass

    def calculate_stress_range(
        self,
        viv_amplitude: float,
        member: TubularMember,
        location: str = "crown"
    ) -> float:
        """
        Calculate stress range from VIV amplitude.

        For a circular cylinder in bending:
        σ = M * c / I
        where M = EI * (∂²w/∂x²)

        Simplified: σ ≈ E * A / D * k²
        where A = amplitude, D = diameter, k = wave number

        Args:
            viv_amplitude: VIV amplitude (m or in diameters)
            member: Tubular member properties
            location: "crown" or "springline"

        Returns:
            Stress range (MPa)
        """
        # If amplitude given as ratio of diameter
        if viv_amplitude < 5.0:  # Assume it's A/D ratio
            A = viv_amplitude * member.outer_diameter
        else:
            A = viv_amplitude

        D = member.outer_diameter
        t = member.wall_thickness
        E = member.material.youngs_modulus

        # Radius to neutral axis
        if location == "crown":
            c = D / 2  # Maximum stress at outer fiber
        else:  # springline
            c = D / 2

        # Second moment of area
        I = member.second_moment_of_area

        # Curvature approximation for vibration
        # κ ≈ A * (π / L)²  for first mode
        L = member.length
        kappa = A * (np.pi / L)**2

        # Bending stress range
        # Δσ = 2 * E * c * κ  (factor of 2 for full cycle)
        stress_range = 2 * E * c * kappa / 1e6  # Convert to MPa

        # Alternative simplified formula (more conservative)
        # Δσ ≈ E * (A/D) * (D/L)²
        # stress_range = E * (A/D) * (D/L)**2 / 1e6

        return stress_range

    def calculate_damage(
        self,
        stress_range: float,
        frequency: float,
        duration: float,
        sn_curve: str = "DNV-D",
        thickness_correction: bool = False,
        thickness: Optional[float] = None
    ) -> float:
        """
        Calculate fatigue damage from VIV.

        Uses Palmgren-Miner rule: D = n / N

        Args:
            stress_range: Stress range (MPa)
            frequency: VIV frequency (Hz)
            duration: Duration of VIV exposure (seconds)
            sn_curve: S-N curve name
            thickness_correction: Apply thickness effect correction
            thickness: Thickness (mm) for correction

        Returns:
            Fatigue damage (Miner's sum)
        """
        if sn_curve not in self.SN_CURVES:
            raise ValueError(f"Unknown S-N curve: {sn_curve}")

        log_a, m, S_ref = self.SN_CURVES[sn_curve]

        # Thickness correction factor (DNV-RP-C203)
        if thickness_correction and thickness is not None:
            # t_eff correction
            t_ref = 25.0  # mm
            if thickness > t_ref:
                k_t = (t_ref / thickness) ** 0.25
            else:
                k_t = 1.0
            stress_range = stress_range * k_t

        # Number of cycles
        n = frequency * duration

        # Allowable cycles from S-N curve
        # N = 10^(log_a - m * log(S/S_ref))
        if stress_range > 0:
            log_N = log_a - m * np.log10(stress_range / S_ref)
            N = 10 ** log_N
        else:
            N = float('inf')

        # Damage
        if N > 0:
            damage = n / N
        else:
            damage = 0.0

        return damage

    def calculate_fatigue_life(
        self,
        stress_range: float,
        frequency: float,
        sn_curve: str = "DNV-D",
        target_damage: float = 1.0
    ) -> float:
        """
        Calculate fatigue life until target damage is reached.

        Args:
            stress_range: Stress range (MPa)
            frequency: VIV frequency (Hz)
            sn_curve: S-N curve name
            target_damage: Target damage (default 1.0 for failure)

        Returns:
            Fatigue life (years)
        """
        if sn_curve not in self.SN_CURVES:
            raise ValueError(f"Unknown S-N curve: {sn_curve}")

        log_a, m, S_ref = self.SN_CURVES[sn_curve]

        # Allowable cycles
        if stress_range > 0:
            log_N = log_a - m * np.log10(stress_range / S_ref)
            N_allow = 10 ** log_N
        else:
            return float('inf')

        # Time to reach target damage
        # D = n / N_allow = f * t / N_allow
        # t = D * N_allow / f
        t_seconds = target_damage * N_allow / frequency

        # Convert to years
        seconds_per_year = 365.25 * 24 * 3600
        t_years = t_seconds / seconds_per_year

        return t_years

    def analyze_viv_fatigue(
        self,
        member: TubularMember,
        viv_amplitude_ratio: float,
        viv_frequency: float,
        duration_hours: float,
        sn_curve: str = "DNV-D"
    ) -> VIVFatigueResult:
        """
        Complete VIV fatigue analysis.

        Args:
            member: Tubular member
            viv_amplitude_ratio: VIV amplitude as ratio of diameter (A/D)
            viv_frequency: VIV frequency (Hz)
            duration_hours: Duration of VIV exposure (hours)
            sn_curve: S-N curve to use

        Returns:
            VIVFatigueResult with damage and life
        """
        # Calculate stress range
        stress_range = self.calculate_stress_range(
            viv_amplitude_ratio,
            member,
            location="crown"
        )

        # Duration in seconds
        duration_seconds = duration_hours * 3600

        # Calculate damage
        damage = self.calculate_damage(
            stress_range,
            viv_frequency,
            duration_seconds,
            sn_curve=sn_curve,
            thickness_correction=True,
            thickness=member.wall_thickness * 1000  # m to mm
        )

        # Number of cycles
        num_cycles = viv_frequency * duration_seconds

        # Fatigue life
        life_years = self.calculate_fatigue_life(
            stress_range,
            viv_frequency,
            sn_curve=sn_curve
        )

        return VIVFatigueResult(
            stress_range=stress_range,
            frequency=viv_frequency,
            duration=duration_seconds,
            num_cycles=num_cycles,
            fatigue_damage=damage,
            fatigue_life_years=life_years,
            sn_curve=sn_curve
        )

    def cumulative_damage(
        self,
        stress_ranges: list,
        frequencies: list,
        durations: list,
        sn_curve: str = "DNV-D"
    ) -> float:
        """
        Calculate cumulative fatigue damage from multiple VIV conditions.

        Args:
            stress_ranges: List of stress ranges (MPa)
            frequencies: List of VIV frequencies (Hz)
            durations: List of durations (seconds)
            sn_curve: S-N curve

        Returns:
            Total cumulative damage
        """
        total_damage = 0.0

        for S_range, freq, duration in zip(stress_ranges, frequencies, durations):
            damage = self.calculate_damage(
                S_range,
                freq,
                duration,
                sn_curve=sn_curve
            )
            total_damage += damage

        return total_damage
