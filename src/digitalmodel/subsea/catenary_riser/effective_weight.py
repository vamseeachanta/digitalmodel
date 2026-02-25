#!/usr/bin/env python3
"""
ABOUTME: Effective weight calculations for riser analysis including steel,
coating, contents, and buoyancy per DNV-OS-F201.
"""

from .models import RiserConfiguration, BuoyancyModule, EffectiveWeightResult


class EffectiveWeightCalculator:
    """
    Calculate effective weight for risers.

    Effective weight = w_steel + w_coating + w_contents - w_buoyancy

    Positive = sink, Negative = float
    """

    def __init__(self, gravity: float = 9.81):
        """Initialize calculator"""
        self.g = gravity

    def calculate(self, riser: RiserConfiguration) -> EffectiveWeightResult:
        """
        Calculate complete effective weight breakdown.

        Args:
            riser: Riser configuration

        Returns:
            EffectiveWeightResult with all components
        """
        # Component weights
        w_steel = riser.steel_weight_per_length
        w_coating = riser.coating_weight_per_length
        w_contents = riser.contents_weight_per_length
        w_buoyancy = riser.buoyancy_per_length

        # Net effective weight
        w_eff = w_steel + w_coating + w_contents - w_buoyancy

        # Total dry weight (in air)
        w_dry = w_steel + w_coating + w_contents

        # Total submerged weight
        w_sub = w_dry - w_buoyancy

        return EffectiveWeightResult(
            steel_weight=w_steel,
            coating_weight=w_coating,
            contents_weight=w_contents,
            buoyancy=w_buoyancy,
            effective_weight=w_eff,
            total_dry_weight=w_dry,
            total_submerged_weight=w_sub,
        )

    def calculate_with_buoyancy_module(
        self,
        riser: RiserConfiguration,
        buoyancy_module: BuoyancyModule
    ) -> EffectiveWeightResult:
        """
        Calculate effective weight with buoyancy module.

        Args:
            riser: Base riser configuration
            buoyancy_module: Buoyancy module

        Returns:
            EffectiveWeightResult including buoyancy module effect
        """
        # Base calculation
        base_result = self.calculate(riser)

        # Buoyancy module adds upward force
        buoy_force = buoyancy_module.buoyancy_force_per_length(riser.external_fluid)

        # Net effective weight in buoyancy section
        w_eff_buoy = base_result.effective_weight + buoy_force

        return EffectiveWeightResult(
            steel_weight=base_result.steel_weight,
            coating_weight=base_result.coating_weight,
            contents_weight=base_result.contents_weight,
            buoyancy=base_result.buoyancy,
            effective_weight=base_result.effective_weight,
            total_dry_weight=base_result.total_dry_weight,
            total_submerged_weight=base_result.total_submerged_weight,
            buoyancy_module_force=buoy_force,
            effective_weight_with_buoyancy=w_eff_buoy,
        )
