#!/usr/bin/env python3
"""
ABOUTME: VIV susceptibility screening using reduced velocity criteria, lock-in
detection, and safety factor evaluation per DNV-RP-C205 and DNV-RP-F105.
"""

import numpy as np
from typing import List, Optional, Tuple
import logging

from .models import (
    TubularMember,
    CurrentProfile,
    FluidProperties,
    ReducedVelocityResult,
    VIVScreeningResult,
    DesignCode,
    LOCK_IN_RANGES
)
from .frequency_calculator import FrequencyCalculator
from .vortex_shedding import VortexSheddingAnalyzer

logger = logging.getLogger(__name__)


class VIVScreening:
    """
    VIV susceptibility screening per design codes.

    Implements reduced velocity criteria and lock-in detection
    per DNV-RP-C205, DNV-RP-F105, and API RP 2A.
    """

    def __init__(
        self,
        fluid: Optional[FluidProperties] = None,
        design_code: DesignCode = DesignCode.DNV_RP_C205
    ):
        """
        Initialize VIV screening.

        Args:
            fluid: Fluid properties
            design_code: Design code for criteria
        """
        self.fluid = fluid or FluidProperties()
        self.design_code = design_code
        self.freq_calc = FrequencyCalculator(fluid)
        self.shedding_calc = VortexSheddingAnalyzer(fluid)

    def reduced_velocity(
        self,
        current_velocity: float,
        natural_frequency: float,
        diameter: float
    ) -> float:
        """
        Calculate reduced velocity.

        V_r = V / (f_n * D)

        Args:
            current_velocity: Current velocity (m/s)
            natural_frequency: Natural frequency (Hz)
            diameter: Cylinder diameter (m)

        Returns:
            Reduced velocity (dimensionless)
        """
        if natural_frequency <= 0:
            raise ValueError("Natural frequency must be positive")
        if diameter <= 0:
            raise ValueError("Diameter must be positive")

        V_r = current_velocity / (natural_frequency * diameter)
        return V_r

    def check_lock_in(
        self,
        reduced_velocity: float,
        vr_min: float = 4.0,
        vr_max: float = 8.0
    ) -> Tuple[bool, float, str]:
        """
        Check if reduced velocity is in lock-in range.

        Args:
            reduced_velocity: Reduced velocity V_r
            vr_min: Minimum lock-in reduced velocity (default 4.0)
            vr_max: Maximum lock-in reduced velocity (default 8.0)

        Returns:
            Tuple of (is_lock_in, margin, status)
            - is_lock_in: True if in lock-in range
            - margin: Distance from lock-in range (positive = inside)
            - status: "safe", "marginal", or "lock-in"
        """
        # Check if in lock-in range
        in_range = vr_min <= reduced_velocity <= vr_max

        if in_range:
            # Inside lock-in range
            # Margin = minimum distance to boundaries
            margin_lower = reduced_velocity - vr_min
            margin_upper = vr_max - reduced_velocity
            margin = min(margin_lower, margin_upper)
            status = "lock-in"

        elif reduced_velocity < vr_min:
            # Below lock-in range
            margin = vr_min - reduced_velocity  # Negative (safe)
            if margin < 1.0:
                status = "marginal"  # Close to lock-in
            else:
                status = "safe"

        else:  # reduced_velocity > vr_max
            # Above lock-in range
            margin = reduced_velocity - vr_max  # Positive but above range
            if margin < 1.0:
                status = "marginal"
            else:
                status = "safe"

        return in_range, margin, status

    def calculate_safety_factor(
        self,
        reduced_velocity: float,
        vr_min: float = 4.0,
        vr_max: float = 8.0
    ) -> float:
        """
        Calculate safety factor against VIV lock-in.

        SF = min(V_r / V_r_min, V_r_max / V_r)

        Args:
            reduced_velocity: Reduced velocity
            vr_min: Minimum lock-in V_r
            vr_max: Maximum lock-in V_r

        Returns:
            Safety factor (> 1.0 is safe)
        """
        if reduced_velocity <= 0:
            return 0.0

        # Safety factor to reach lower bound
        sf_lower = vr_min / reduced_velocity

        # Safety factor to reach upper bound
        sf_upper = reduced_velocity / vr_max

        # Minimum is governing
        sf = min(sf_lower, sf_upper)

        return sf

    def screen_member(
        self,
        member: TubularMember,
        current_velocity: float,
        mode: int = 1,
        strouhal_number: float = 0.2,
        vr_min: float = 4.0,
        vr_max: float = 8.0
    ) -> VIVScreeningResult:
        """
        Screen tubular member for VIV susceptibility.

        Args:
            member: Tubular member
            current_velocity: Current velocity (m/s)
            mode: Mode number to check
            strouhal_number: Strouhal number
            vr_min: Minimum lock-in reduced velocity
            vr_max: Maximum lock-in reduced velocity

        Returns:
            VIVScreeningResult with susceptibility assessment
        """
        # Calculate natural frequency
        freq_result = self.freq_calc.calculate_natural_frequency(
            member,
            mode=mode,
            include_added_mass=True
        )
        f_n = freq_result.frequency

        # Calculate vortex shedding frequency
        shedding_result = self.shedding_calc.analyze_member(
            member,
            current_velocity,
            strouhal_number=strouhal_number
        )
        f_s = shedding_result.shedding_frequency

        # Reduced velocity
        V_r = self.reduced_velocity(
            current_velocity,
            f_n,
            member.outer_diameter
        )

        # Lock-in check
        is_lock_in, margin, status = self.check_lock_in(V_r, vr_min, vr_max)

        # Safety factor
        sf = self.calculate_safety_factor(V_r, vr_min, vr_max)

        # Susceptibility
        is_susceptible = is_lock_in or status == "marginal"

        # Recommendation
        if status == "lock-in":
            recommendation = "VIV suppression REQUIRED - in lock-in range"
        elif status == "marginal":
            recommendation = "VIV suppression RECOMMENDED - near lock-in"
        else:
            if sf < 1.5:
                recommendation = "Monitor VIV - low safety factor"
            else:
                recommendation = "VIV risk acceptable"

        return VIVScreeningResult(
            member_name=member.name,
            natural_frequency=f_n,
            shedding_frequency=f_s,
            reduced_velocity=V_r,
            is_susceptible=is_susceptible,
            lock_in_status=status,
            safety_factor=sf,
            recommendation=recommendation,
            details={
                'mode': mode,
                'current_velocity': current_velocity,
                'diameter': member.outer_diameter,
                'frequency_ratio': f_s / f_n if f_n > 0 else 0.0,
                'reynolds_number': shedding_result.reynolds_number,
                'vr_range': (vr_min, vr_max),
                'lock_in_margin': margin
            }
        )

    def screen_multiple_modes(
        self,
        member: TubularMember,
        current_velocity: float,
        n_modes: int = 3,
        strouhal_number: float = 0.2
    ) -> List[VIVScreeningResult]:
        """
        Screen multiple vibration modes for VIV.

        Args:
            member: Tubular member
            current_velocity: Current velocity (m/s)
            n_modes: Number of modes to check
            strouhal_number: Strouhal number

        Returns:
            List of screening results for each mode
        """
        results = []

        for mode in range(1, n_modes + 1):
            result = self.screen_member(
                member,
                current_velocity,
                mode=mode,
                strouhal_number=strouhal_number
            )
            results.append(result)

        return results

    def screen_with_current_profile(
        self,
        member: TubularMember,
        current_profile: CurrentProfile,
        mode: int = 1,
        use_max_current: bool = True
    ) -> VIVScreeningResult:
        """
        Screen member with current profile (uses maximum current).

        Args:
            member: Tubular member
            current_profile: Current velocity profile
            mode: Mode number
            use_max_current: Use maximum current from profile

        Returns:
            VIVScreeningResult using representative current
        """
        # Get current velocities along length
        depths = np.linspace(0, member.length, 20)
        velocities = [current_profile.velocity_at_depth(d) for d in depths]

        # Use maximum (most conservative)
        if use_max_current:
            V_max = max(velocities)
        else:
            # Use RMS
            V_max = np.sqrt(np.mean(np.array(velocities)**2))

        return self.screen_member(member, V_max, mode=mode)

    def critical_current_velocity(
        self,
        member: TubularMember,
        mode: int = 1,
        vr_target: float = 5.0
    ) -> float:
        """
        Calculate critical current velocity for given reduced velocity.

        V_crit = V_r * f_n * D

        Args:
            member: Tubular member
            mode: Mode number
            vr_target: Target reduced velocity (e.g., 5.0 for lock-in)

        Returns:
            Critical current velocity (m/s)
        """
        # Natural frequency
        freq_result = self.freq_calc.calculate_natural_frequency(
            member,
            mode=mode,
            include_added_mass=True
        )
        f_n = freq_result.frequency
        D = member.outer_diameter

        V_crit = vr_target * f_n * D
        return V_crit
