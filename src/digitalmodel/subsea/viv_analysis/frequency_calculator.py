#!/usr/bin/env python3
"""
ABOUTME: Natural frequency calculator for tubular members using beam vibration
theory with added mass effects and different boundary conditions.
"""

import numpy as np
from typing import List, Optional
import logging

from .models import (
    TubularMember,
    FluidProperties,
    NaturalFrequencyResult,
    BoundaryCondition
)

logger = logging.getLogger(__name__)


class FrequencyCalculator:
    """
    Calculate natural frequencies for tubular members.

    Implements beam vibration equations for different boundary conditions
    with added mass effects from surrounding fluid.
    """

    # Eigenvalues (λL) for different boundary conditions and modes
    # Source: Blevins "Flow-Induced Vibration"
    EIGENVALUES = {
        BoundaryCondition.PINNED_PINNED: [
            np.pi, 2*np.pi, 3*np.pi, 4*np.pi, 5*np.pi, 6*np.pi
        ],
        BoundaryCondition.FIXED_FIXED: [
            4.730, 7.853, 10.996, 14.137, 17.279, 20.420
        ],
        BoundaryCondition.FIXED_PINNED: [
            3.927, 7.069, 10.210, 13.352, 16.493, 19.635
        ],
        BoundaryCondition.FREE_FREE: [
            4.730, 7.853, 10.996, 14.137, 17.279, 20.420
        ],
        BoundaryCondition.CANTILEVER: [
            1.875, 4.694, 7.855, 10.996, 14.137, 17.279
        ],
    }

    def __init__(self, fluid: Optional[FluidProperties] = None):
        """
        Initialize frequency calculator.

        Args:
            fluid: Fluid properties for added mass (defaults to seawater)
        """
        self.fluid = fluid or FluidProperties()
        self.g = 9.81  # m/s²

    def calculate_natural_frequency(
        self,
        member: TubularMember,
        mode: int = 1,
        include_added_mass: bool = True
    ) -> NaturalFrequencyResult:
        """
        Calculate natural frequency for a tubular member.

        Uses Euler-Bernoulli beam theory:
        f_n = (λ_n/L)² / (2π) * sqrt(EI / (m + m_a))

        For tension-controlled (risers):
        f_n = (n / 2L) * sqrt(T / (m + m_a))

        Args:
            member: Tubular member properties
            mode: Mode number (1 = fundamental, 2 = second, etc.)
            include_added_mass: Include fluid added mass effect

        Returns:
            NaturalFrequencyResult with frequency and properties
        """
        if mode < 1 or mode > 6:
            raise ValueError("Mode number must be between 1 and 6")

        # Tension-controlled (riser with axial tension)
        if member.boundary_condition == BoundaryCondition.TENSION_CONTROLLED:
            if member.top_tension is None:
                raise ValueError("top_tension must be specified for tension-controlled member")
            return self._frequency_tension_controlled(member, mode, include_added_mass)

        # Standard beam vibration
        return self._frequency_beam_vibration(member, mode, include_added_mass)

    def _frequency_beam_vibration(
        self,
        member: TubularMember,
        mode: int,
        include_added_mass: bool
    ) -> NaturalFrequencyResult:
        """Calculate frequency using beam vibration equations."""

        # Get eigenvalue for this mode and boundary condition
        eigenvalues = self.EIGENVALUES.get(member.boundary_condition)
        if eigenvalues is None:
            raise ValueError(f"Unsupported boundary condition: {member.boundary_condition}")

        lambda_n = eigenvalues[mode - 1]

        # Effective length
        L_eff = member.length * member.effective_length_factor

        # Material properties
        E = member.material.youngs_modulus
        I = member.second_moment_of_area

        # Mass per unit length
        m_struct = member.mass_per_length

        # Added mass from fluid
        if include_added_mass:
            # Added mass: m_a = ρ_fluid * C_a * π * D² / 4
            D = member.outer_diameter
            m_added = (
                self.fluid.density *
                self.fluid.added_mass_coefficient *
                np.pi * D**2 / 4
            )
            m_total = m_struct + m_added
            mass_ratio = m_total / m_struct
        else:
            m_total = m_struct
            mass_ratio = 1.0

        # Natural frequency (Hz)
        # f_n = (λ_n / L)² / (2π) * sqrt(EI / m_total)
        omega_n = (lambda_n / L_eff)**2 * np.sqrt(E * I / m_total)
        f_n = omega_n / (2 * np.pi)

        # Period
        T_n = 1 / f_n

        # Wavelength (approximate for mode 1)
        wavelength = 2 * L_eff / mode

        return NaturalFrequencyResult(
            member_name=member.name,
            mode_number=mode,
            frequency=f_n,
            period=T_n,
            angular_frequency=omega_n,
            wavelength=wavelength,
            boundary_condition=member.boundary_condition.value,
            includes_added_mass=include_added_mass,
            effective_mass_ratio=mass_ratio
        )

    def _frequency_tension_controlled(
        self,
        member: TubularMember,
        mode: int,
        include_added_mass: bool
    ) -> NaturalFrequencyResult:
        """
        Calculate frequency for tension-controlled member (riser).

        For a string under tension: f_n = (n / 2L) * sqrt(T / m)
        """
        L = member.length
        T = member.top_tension  # type: ignore

        # Mass per unit length
        m_struct = member.mass_per_length

        # Added mass
        if include_added_mass:
            D = member.outer_diameter
            m_added = (
                self.fluid.density *
                self.fluid.added_mass_coefficient *
                np.pi * D**2 / 4
            )
            m_total = m_struct + m_added
            mass_ratio = m_total / m_struct
        else:
            m_total = m_struct
            mass_ratio = 1.0

        # Natural frequency
        # f_n = (n / 2L) * sqrt(T / m_total)
        f_n = (mode / (2 * L)) * np.sqrt(T / m_total)

        # Angular frequency
        omega_n = 2 * np.pi * f_n

        # Period
        T_n = 1 / f_n

        # Wavelength
        wavelength = 2 * L / mode

        return NaturalFrequencyResult(
            member_name=member.name,
            mode_number=mode,
            frequency=f_n,
            period=T_n,
            angular_frequency=omega_n,
            wavelength=wavelength,
            boundary_condition="tension-controlled",
            includes_added_mass=include_added_mass,
            effective_mass_ratio=mass_ratio
        )

    def calculate_multiple_modes(
        self,
        member: TubularMember,
        n_modes: int = 5,
        include_added_mass: bool = True
    ) -> List[NaturalFrequencyResult]:
        """
        Calculate natural frequencies for multiple modes.

        Args:
            member: Tubular member
            n_modes: Number of modes to calculate
            include_added_mass: Include added mass effect

        Returns:
            List of NaturalFrequencyResult for each mode
        """
        results = []
        for mode in range(1, n_modes + 1):
            result = self.calculate_natural_frequency(
                member,
                mode=mode,
                include_added_mass=include_added_mass
            )
            results.append(result)

        return results

    def calculate_effective_tension(
        self,
        top_tension: float,
        member: TubularMember,
        depth: float,
        include_weight: bool = True
    ) -> float:
        """
        Calculate effective tension at given depth along riser.

        T_eff(z) = T_top - w_eff * z

        Args:
            top_tension: Tension at top (N)
            member: Tubular member properties
            depth: Depth below top (m, positive down)
            include_weight: Include weight in water

        Returns:
            Effective tension (N)
        """
        if not include_weight:
            return top_tension

        # Weight per unit length in water
        # w = (ρ_steel * A_steel - ρ_water * A_displaced) * g
        A_steel = member.cross_sectional_area
        D_outer = member.outer_diameter
        A_displaced = np.pi * D_outer**2 / 4

        w_eff = (
            member.material.density * A_steel -
            self.fluid.density * A_displaced
        ) * self.g

        # Effective tension decreases with depth
        T_eff = top_tension - w_eff * depth

        return T_eff

    def check_stability(
        self,
        member: TubularMember,
        min_tension: float = 0.0
    ) -> bool:
        """
        Check if member has positive tension throughout (for risers).

        Args:
            member: Tubular member
            min_tension: Minimum required tension (N)

        Returns:
            True if tension is positive throughout
        """
        if member.boundary_condition != BoundaryCondition.TENSION_CONTROLLED:
            return True

        if member.top_tension is None:
            return False

        # Check tension at bottom
        T_bottom = self.calculate_effective_tension(
            member.top_tension,
            member,
            depth=member.length
        )

        return T_bottom >= min_tension
