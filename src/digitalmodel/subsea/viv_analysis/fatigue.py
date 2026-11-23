#!/usr/bin/env python3
"""
ABOUTME: VIV-induced fatigue damage calculator using stress ranges from VIV
amplitudes and S-N curve methodology per DNV standards.
"""

from dataclasses import asdict
import math

import numpy as np
from typing import Optional
import logging

from .models import (
    TubularMember,
    VIVFatigueResult
)

from digitalmodel.citations import validate_citation
from digitalmodel.fatigue.c203_sn_tables import T_REF_WELDED_MM
from .sn_parameters import SN_CITATION, SN_CURVES

logger = logging.getLogger(__name__)


class VIVFatigueCalculator:
    """
    Calculate VIV-induced fatigue damage.

    Uses S-N curve methodology similar to fatigue_analysis module,
    but specific to VIV-induced cyclic stresses.
    """

    # In-air curves only: DNV-RP-C203 October 2011, Table 2-1.
    SN_CURVES = SN_CURVES

    def __init__(self):
        self.citations = (SN_CITATION,)
        self._citation_validated = False

    def calculate_allowable_cycles(self, stress_range: float, sn_curve: str = "DNV-D") -> float:
        """Endurance in cycles, with stress range in MPa (C203 Eq. 2.4.1).

        Use the tabulated second branch below the 10^7-cycle knee. The rounded
        intercepts produce a small discontinuity at the knee (under 0.2%).
        No constant-amplitude endurance cutoff is applied to VIV damage.
        """
        if sn_curve not in self.SN_CURVES:
            raise ValueError(f"Unknown S-N curve: {sn_curve}")
        if not self._citation_validated:
            validate_citation(SN_CITATION)
            self._citation_validated = True
        if not math.isfinite(stress_range) or stress_range < 0:
            raise ValueError("stress_range must be finite and nonnegative")
        if stress_range == 0:
            return math.inf
        curve = self.SN_CURVES[sn_curve]
        log_n = curve.log_a1 - curve.m1 * math.log10(stress_range)
        if log_n > math.log10(curve.knee):
            log_n = curve.log_a2 - curve.m2 * math.log10(stress_range)
        return 10 ** log_n

    def _corrected_stress(self, stress_range, sn_curve, apply, thickness):
        """C203 (2011) Eq. (2.4.3), Table 2-1: S_eff=S*(t/t_ref)^k."""
        if sn_curve not in self.SN_CURVES:
            raise ValueError(f"Unknown S-N curve: {sn_curve}")
        if not apply:
            return stress_range
        if thickness is None or not math.isfinite(thickness) or thickness <= 0:
            raise ValueError("thickness must be finite and positive when correction is requested")
        factor = max(1.0, thickness / T_REF_WELDED_MM) ** self.SN_CURVES[sn_curve].k
        return stress_range * factor

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
        E = member.material.youngs_modulus_pa

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
            frequency: VIV frequency (Hz); zero produces no damage
            duration: Duration of VIV exposure (seconds)
            sn_curve: S-N curve name
            thickness_correction: Apply thickness effect correction
            thickness: Thickness (mm) for correction

        Returns:
            Fatigue damage (Miner's sum)
        """
        stress_range = self._corrected_stress(
            stress_range, sn_curve, thickness_correction, thickness,
        )
        if not math.isfinite(frequency) or frequency < 0:
            raise ValueError("frequency must be finite and nonnegative")
        if not math.isfinite(duration) or duration < 0:
            raise ValueError("duration must be finite and nonnegative")
        allowable_cycles = self.calculate_allowable_cycles(stress_range, sn_curve)
        # C203 (2011) Eq. (2.2.1): Palmgren-Miner D = n/N.
        return frequency * duration / allowable_cycles

    def calculate_fatigue_life(
        self,
        stress_range: float,
        frequency: float,
        sn_curve: str = "DNV-D",
        target_damage: float = 1.0,
        thickness_correction: bool = False,
        thickness: Optional[float] = None
    ) -> float:
        """
        Calculate fatigue life until target damage is reached.

        Args:
            stress_range: Stress range (MPa)
            frequency: VIV frequency (Hz); zero gives infinite fatigue life
            sn_curve: S-N curve name
            target_damage: Target damage (default 1.0 for failure)

        Returns:
            Fatigue life (years)
        """
        stress_range = self._corrected_stress(
            stress_range, sn_curve, thickness_correction, thickness,
        )
        if not math.isfinite(frequency) or frequency < 0:
            raise ValueError("frequency must be finite and nonnegative")
        if not math.isfinite(target_damage) or target_damage <= 0:
            raise ValueError("target_damage must be finite and positive")
        allowable_cycles = self.calculate_allowable_cycles(stress_range, sn_curve)
        if frequency == 0:
            return math.inf
        # C203 (2011) Eq. (2.2.1), rearranged: t = D*N/f.
        return target_damage * allowable_cycles / frequency / (365.25 * 24 * 3600)

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
            sn_curve=sn_curve,
            thickness_correction=True,
            thickness=member.wall_thickness * 1000,
        )

        return VIVFatigueResult(
            stress_range=stress_range,
            frequency=viv_frequency,
            duration=duration_seconds,
            num_cycles=num_cycles,
            fatigue_damage=damage,
            fatigue_life_years=life_years,
            sn_curve=sn_curve,
            citations=[asdict(citation) for citation in self.citations],
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
