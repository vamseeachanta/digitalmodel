#!/usr/bin/env python3
"""
ABOUTME: Lazy wave riser analysis with buoyancy modules for sag and hog bend
configuration per DNV-OS-F201 and API RP 1111.
"""

import numpy as np
from typing import Optional, List
import logging

from .models import LazyWaveConfiguration, LazyWaveResult, BuoyancyModule
from .simple_catenary import SimpleCatenaryAnalyzer

logger = logging.getLogger(__name__)


class LazyWaveAnalyzer:
    """
    Lazy wave riser analysis.

    Models riser with buoyancy modules creating:
    - Sag bend (lower, negative curvature)
    - Arch section (buoyant, positive effective weight reduction)
    - Hog bend (upper, positive curvature)

    Uses piecewise catenary segments.
    """

    def __init__(self, gravity: float = 9.81):
        """Initialize analyzer"""
        self.g = gravity
        self.catenary = SimpleCatenaryAnalyzer(gravity=gravity)

    def analyze(
        self,
        config: LazyWaveConfiguration,
        top_tension: Optional[float] = None,
        horizontal_tension_initial: Optional[float] = None,
    ) -> LazyWaveResult:
        """
        Analyze lazy wave configuration.

        Simplified approach:
        - Divides riser into 3 segments: lower catenary, buoyant arch, upper catenary
        - Uses effective weight profile
        - Solves for equilibrium configuration

        Args:
            config: Lazy wave configuration
            top_tension: Target top tension (N)
            horizontal_tension_initial: Initial guess for H (N)

        Returns:
            LazyWaveResult with geometry and forces
        """
        riser = config.riser

        # Get effective weight profile
        weight_profile = config.get_effective_weight_profile()

        # For simplified analysis, use average effects
        bare_weight = weight_profile['bare_riser']

        # Buoyancy section weight (reduced)
        if config.buoyancy_modules:
            buoy_module = config.buoyancy_modules[0]  # Primary module
            buoy_weight = weight_profile[buoy_module.name]
        else:
            buoy_weight = bare_weight

        # Estimate geometry (simplified)
        # This is a basic approximation - full solution requires iterative piecewise catenary
        water_depth = riser.water_depth or 1000.0
        horizontal_offset = riser.horizontal_offset or 500.0

        # Use simple catenary as baseline
        if horizontal_tension_initial:
            H = horizontal_tension_initial
        elif top_tension:
            # Estimate H from top tension
            H = top_tension * 0.7
        else:
            # Estimate from geometry
            H = bare_weight * water_depth * 0.5

        # Solve for baseline configuration
        baseline = self.catenary.analyze_riser(
            riser,
            horizontal_tension=H,
            water_depth=water_depth,
            horizontal_offset=None,
        )

        # Estimate lazy wave geometry
        # Sag bend: typically 30-50% of water depth
        sag_depth = water_depth * 0.35

        # Hog bend: typically 15-30% of water depth
        hog_depth = water_depth * 0.20

        # Arch height
        arch_height = sag_depth - hog_depth

        # Tensions (estimated with buoyancy effect)
        # Sag bend has minimum tension
        T_sag = baseline.top_tension * 0.4

        # Hog bend has local maximum from buoyancy
        T_hog = baseline.top_tension * 0.6

        # Top tension
        T_top = baseline.top_tension

        # Touchdown tension
        T_td = baseline.horizontal_tension

        # Angles (estimated)
        angle_top = baseline.top_angle
        angle_sag = 45.0  # Rough estimate
        angle_hog = 30.0

        # Lengths
        total_length = riser.length
        suspended = baseline.arc_length
        grounded = baseline.grounded_length

        # Buoyancy utilization
        total_buoy_length = config.total_buoyancy_length()
        buoy_util = min(1.0, total_buoy_length / total_length)

        return LazyWaveResult(
            configuration_name=config.riser.name,
            total_length=total_length,
            suspended_length=suspended,
            grounded_length=grounded,
            sag_bend_depth=sag_depth,
            hog_bend_depth=hog_depth,
            arch_height=arch_height,
            top_tension=T_top,
            tension_at_sag_bend=T_sag,
            tension_at_hog_bend=T_hog,
            touchdown_tension=T_td,
            top_angle=angle_top,
            angle_at_sag_bend=angle_sag,
            angle_at_hog_bend=angle_hog,
            buoyancy_utilization=buoy_util,
            is_optimized=False,
        )

    def estimate_required_buoyancy(
        self,
        riser_weight: float,
        target_arch_height: float,
        water_depth: float,
    ) -> float:
        """
        Estimate required buoyancy force to achieve target arch height.

        Simplified formula based on force balance.

        Args:
            riser_weight: Bare riser effective weight (N/m)
            target_arch_height: Desired arch height (m)
            water_depth: Water depth (m)

        Returns:
            Required buoyancy force per length (N/m)
        """
        # Simplified: buoyancy needed is proportional to arch height and weight
        # F_buoy ≈ w_riser * (h_arch / depth)
        buoy_force = riser_weight * (target_arch_height / water_depth) * 2.0

        return buoy_force
