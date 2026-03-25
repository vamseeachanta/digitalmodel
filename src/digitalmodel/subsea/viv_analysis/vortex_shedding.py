#!/usr/bin/env python3
"""
ABOUTME: Vortex shedding frequency calculator using Strouhal number relationship
with current profile handling and Reynolds number effects.
"""

import numpy as np
from typing import List, Optional
import logging

from .models import (
    TubularMember,
    CurrentProfile,
    FluidProperties,
    VortexSheddingResult,
    STROUHAL_NUMBERS
)

logger = logging.getLogger(__name__)


class VortexSheddingAnalyzer:
    """
    Analyze vortex shedding frequencies for tubular members.

    Uses Strouhal number relationship: f_s = St * V / D
    """

    def __init__(self, fluid: Optional[FluidProperties] = None):
        """
        Initialize vortex shedding analyzer.

        Args:
            fluid: Fluid properties (defaults to seawater)
        """
        self.fluid = fluid or FluidProperties()

    def shedding_frequency(
        self,
        diameter: float,
        velocity: float,
        strouhal_number: float = 0.2
    ) -> float:
        """
        Calculate vortex shedding frequency.

        f_s = St * V / D

        Args:
            diameter: Cylinder diameter (m)
            velocity: Current velocity (m/s)
            strouhal_number: Strouhal number (dimensionless, typically 0.2)

        Returns:
            Shedding frequency (Hz)
        """
        if diameter <= 0:
            raise ValueError("Diameter must be positive")
        if velocity < 0:
            raise ValueError("Velocity cannot be negative")

        f_s = strouhal_number * velocity / diameter
        return f_s

    def reynolds_number(
        self,
        diameter: float,
        velocity: float
    ) -> float:
        """
        Calculate Reynolds number.

        Re = V * D / ν

        Args:
            diameter: Cylinder diameter (m)
            velocity: Flow velocity (m/s)

        Returns:
            Reynolds number (dimensionless)
        """
        Re = velocity * diameter / self.fluid.kinematic_viscosity
        return Re

    def strouhal_from_reynolds(
        self,
        reynolds_number: float,
        surface_roughness: str = 'smooth'
    ) -> float:
        """
        Get Strouhal number based on Reynolds number and surface condition.

        For most offshore applications (Re > 1e5), St ≈ 0.2

        Args:
            reynolds_number: Reynolds number
            surface_roughness: 'smooth', 'rough', 'straked', etc.

        Returns:
            Strouhal number
        """
        # Get base Strouhal number
        base_st = STROUHAL_NUMBERS.get(surface_roughness, 0.2)

        # For subcritical flow (Re < 2e5), use base value
        # For supercritical (Re > 3.5e6), St can vary
        # For most offshore: 1e5 < Re < 1e7, St ≈ 0.2

        if reynolds_number < 1e3:
            # Very low Re, shedding may not be coherent
            logger.warning(f"Low Reynolds number ({reynolds_number:.0f}), vortex shedding may not be coherent")

        return base_st

    def analyze_member(
        self,
        member: TubularMember,
        current_velocity: float,
        strouhal_number: Optional[float] = None,
        depth: Optional[float] = None
    ) -> VortexSheddingResult:
        """
        Analyze vortex shedding for a tubular member.

        Args:
            member: Tubular member properties
            current_velocity: Current velocity (m/s)
            strouhal_number: Strouhal number (if None, use Reynolds-dependent)
            depth: Depth for reporting (m, optional)

        Returns:
            VortexSheddingResult with shedding frequency and Re
        """
        D = member.outer_diameter

        # Reynolds number
        Re = self.reynolds_number(D, current_velocity)

        # Strouhal number
        if strouhal_number is None:
            strouhal_number = self.strouhal_from_reynolds(Re)

        # Shedding frequency
        f_s = self.shedding_frequency(D, current_velocity, strouhal_number)

        return VortexSheddingResult(
            diameter=D,
            current_velocity=current_velocity,
            strouhal_number=strouhal_number,
            shedding_frequency=f_s,
            reynolds_number=Re,
            depth=depth
        )

    def analyze_with_profile(
        self,
        member: TubularMember,
        current_profile: CurrentProfile,
        n_points: int = 20,
        strouhal_number: Optional[float] = None
    ) -> List[VortexSheddingResult]:
        """
        Analyze vortex shedding along member length with current profile.

        Args:
            member: Tubular member
            current_profile: Current velocity profile
            n_points: Number of points along length
            strouhal_number: Strouhal number (if None, use Re-dependent)

        Returns:
            List of VortexSheddingResult at different depths
        """
        results = []

        # Depths along member (assuming vertical or near-vertical)
        depths = np.linspace(0, member.length, n_points)

        for depth in depths:
            velocity = current_profile.velocity_at_depth(depth)
            result = self.analyze_member(
                member,
                velocity,
                strouhal_number=strouhal_number,
                depth=depth
            )
            results.append(result)

        return results

    def dominant_shedding_frequency(
        self,
        shedding_results: List[VortexSheddingResult]
    ) -> float:
        """
        Get dominant shedding frequency from profile analysis.

        Uses maximum frequency (highest current) as representative.

        Args:
            shedding_results: List of shedding results along length

        Returns:
            Dominant shedding frequency (Hz)
        """
        if not shedding_results:
            raise ValueError("No shedding results provided")

        # Return maximum frequency (corresponds to maximum current)
        max_freq = max(r.shedding_frequency for r in shedding_results)
        return max_freq

    def rms_shedding_frequency(
        self,
        shedding_results: List[VortexSheddingResult]
    ) -> float:
        """
        Calculate RMS shedding frequency along length.

        Args:
            shedding_results: List of shedding results

        Returns:
            RMS frequency (Hz)
        """
        if not shedding_results:
            raise ValueError("No shedding results provided")

        frequencies = [r.shedding_frequency for r in shedding_results]
        f_rms = np.sqrt(np.mean(np.array(frequencies)**2))
        return f_rms
