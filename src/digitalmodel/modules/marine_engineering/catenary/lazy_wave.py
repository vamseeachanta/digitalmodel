"""
Lazy-wave catenary analysis for riser configurations.

Handles multi-segment catenary with buoyancy modules:
- Sag section (weight-dominated, hang-off to buoyancy)
- Hog section (buoyancy-dominated, with buoyancy modules)
- Touch-down section (buoyancy to seabed)

This module ports the legacy lazy-wave implementation from
digitalmodel.catenary.catenaryMethods with improved type safety
and modern Python patterns.

Mathematical Background:
-----------------------
Lazy-wave risers use buoyancy modules to create a wave-like configuration:
1. Sag section: Catenary under self-weight from vessel to buoyancy start
2. Hog section: Inverse catenary (buoyancy > weight) creating upward arc
3. Touchdown: Final catenary section to seabed

The configuration is solved by:
- Computing bend radii based on weight ratios
- Calculating arc lengths and horizontal distances for each segment
- Summing total forces and geometry

References:
----------
Legacy implementation: catenaryMethods.py sagHogEquation (lines 93-153)
                      catenaryMethods.py lazyWaveCatenaryEquation (lines 156-194)
"""

from dataclasses import dataclass
from typing import Optional, List, Dict, Any
import math


@dataclass
class LazyWaveSegment:
    """Single segment of lazy-wave catenary.

    Attributes:
        arc_length (float): Arc length S along the catenary [m]
        horizontal_distance (float): Horizontal projection X [m]
        vertical_distance (float): Vertical span d [m]
        bend_radius (float): Catenary bend radius (a = H/w) [m]
        weight_per_length (float): Effective weight per length [N/m]
                                   (can be negative for buoyancy sections)
    """
    arc_length: float
    horizontal_distance: float
    vertical_distance: float
    bend_radius: float
    weight_per_length: float


@dataclass
class LazyWaveConfiguration:
    """Complete lazy-wave riser configuration.

    Attributes:
        hangoff_angle (float): Departure angle from vessel [degrees]
        hangoff_below_msl (float): Hang-off depth below mean sea level [m]
        hog_bend_above_seabed (float): Hog bend elevation above seabed [m]
        sag_bend_elevation (float): Sag bend elevation above seabed [m]
        weight_without_buoyancy (float): Bare riser weight per length w [N/m]
        weight_with_buoyancy (float): Effective weight with buoyancy modules w_buoy [N/m]
                                     (typically negative)
        vertical_distance (float): Total vertical span (hang-off to seabed) [m]
        hangoff_bend_radius (float): Initial bend radius at hang-off [m]
    """
    hangoff_angle: float
    hangoff_below_msl: float
    hog_bend_above_seabed: float
    sag_bend_elevation: float
    weight_without_buoyancy: float
    weight_with_buoyancy: float
    vertical_distance: float
    hangoff_bend_radius: float


@dataclass
class LazyWaveResults:
    """Lazy-wave analysis results.

    Attributes:
        hangoff_to_sag (LazyWaveSegment): Hang-off to sag bend section
        sag_to_buoyancy (LazyWaveSegment): Sag bend to buoyancy start
        buoyancy_to_hog (LazyWaveSegment): Buoyancy start to hog bend
        hog_to_buoyancy_end (LazyWaveSegment): Hog bend to buoyancy end
        buoyancy_to_touchdown (LazyWaveSegment): Buoyancy end to touchdown
        total_arc_length (float): Total arc length from hang-off to TDP [m]
        total_horizontal_distance (float): Total horizontal distance [m]
        horizontal_force (float): Horizontal force Fh [N]
        vertical_force (float): Vertical force at hang-off Fv [N]
        segments (List[LazyWaveSegment]): All segments for plotting
        summary (Dict[str, Any]): Summary data matching legacy format
    """
    hangoff_to_sag: LazyWaveSegment
    sag_to_buoyancy: LazyWaveSegment
    buoyancy_to_hog: LazyWaveSegment
    hog_to_buoyancy_end: LazyWaveSegment
    buoyancy_to_touchdown: LazyWaveSegment
    total_arc_length: float
    total_horizontal_distance: float
    horizontal_force: float
    vertical_force: float
    segments: List[LazyWaveSegment]
    summary: Dict[str, Any]


class LazyWaveSolver:
    """Multi-segment lazy-wave catenary solver.

    This solver implements the lazy-wave riser analysis from the legacy
    catenaryMethods module, preserving all numerical calculations while
    providing a modern type-safe interface.

    The solver computes:
    1. Hang-off section using standard catenary with departure angle
    2. Sag-to-hog sections using weighted bend radii
    3. Force balance at hang-off point

    Mathematical formulas are ported exactly from legacy implementation.
    """

    def solve(self, config: LazyWaveConfiguration) -> LazyWaveResults:
        """
        Solve lazy-wave catenary configuration.

        Ports sagHogEquation and lazyWaveCatenaryEquation from legacy
        catenaryMethods.py (lines 93-194).

        Parameters:
            config (LazyWaveConfiguration): Lazy-wave riser configuration

        Returns:
            LazyWaveResults: Complete lazy-wave solution

        Mathematical Steps:
        ------------------
        1. Compute hang-off section using departure angle
        2. Compute sag-to-buoyancy section (weight-dominated)
        3. Compute buoyancy-to-hog section (buoyancy-dominated)
        4. Compute hog-to-buoyancy section (returning to weight)
        5. Compute buoyancy-to-touchdown section (final catenary)
        6. Sum forces and geometry
        """

        # Step 1: Hang-off to Sag Section
        # Uses standard catenary equation with departure angle q
        # Legacy: lines 158-160
        hangoff_section = self._solve_hangoff_section(config)

        # Step 2: Sag-Hog-Buoyancy Sections
        # Legacy: sagHogEquation lines 93-153
        sag_hog_results = self._solve_sag_hog_sections(config)

        # Step 3: Compute Forces
        # Legacy: lines 164-165
        Fh = config.hangoff_bend_radius * config.weight_without_buoyancy
        Fv = Fh + config.weight_without_buoyancy * hangoff_section.arc_length

        # Step 4: Create Summary (matches legacy format)
        # Legacy: lines 167-191
        hangoff_to_buoyancy = LazyWaveSegment(
            arc_length=hangoff_section.arc_length + sag_hog_results['sag_to_buoyancy'].arc_length,
            horizontal_distance=hangoff_section.horizontal_distance + sag_hog_results['sag_to_buoyancy'].horizontal_distance,
            vertical_distance=hangoff_section.vertical_distance + sag_hog_results['sag_to_buoyancy'].vertical_distance,
            bend_radius=hangoff_section.bend_radius,
            weight_per_length=config.weight_without_buoyancy
        )

        buoyancy_section = LazyWaveSegment(
            arc_length=sag_hog_results['buoyancy_to_hog'].arc_length + sag_hog_results['hog_to_buoyancy'].arc_length,
            horizontal_distance=sag_hog_results['buoyancy_to_hog'].horizontal_distance + sag_hog_results['hog_to_buoyancy'].horizontal_distance,
            vertical_distance=sag_hog_results['buoyancy_to_hog'].vertical_distance + sag_hog_results['hog_to_buoyancy'].vertical_distance,
            bend_radius=sag_hog_results['buoyancy_to_hog'].bend_radius,
            weight_per_length=config.weight_with_buoyancy
        )

        buoyancy_to_touchdown = sag_hog_results['buoyancy_to_touchdown']

        total_arc = hangoff_to_buoyancy.arc_length + buoyancy_section.arc_length + buoyancy_to_touchdown.arc_length
        total_horizontal = hangoff_to_buoyancy.horizontal_distance + buoyancy_section.horizontal_distance + buoyancy_to_touchdown.horizontal_distance

        # Create summary dict matching legacy format
        summary = {
            'HangOffToBuoyancy': {
                'S': hangoff_to_buoyancy.arc_length,
                'X': hangoff_to_buoyancy.horizontal_distance
            },
            'Buoyancy': {
                'S': buoyancy_section.arc_length,
                'X': buoyancy_section.horizontal_distance
            },
            'BuoyancyToTouchDown': {
                'S': buoyancy_to_touchdown.arc_length,
                'X': buoyancy_to_touchdown.horizontal_distance
            },
            'HangoffToTDP': {
                'S': total_arc,
                'X': total_horizontal
            },
            'Fh': Fh,
            'Fv': Fv
        }

        # Collect all segments
        segments = [
            hangoff_section,
            sag_hog_results['sag_to_buoyancy'],
            sag_hog_results['buoyancy_to_hog'],
            sag_hog_results['hog_to_buoyancy'],
            sag_hog_results['buoyancy_to_touchdown']
        ]

        return LazyWaveResults(
            hangoff_to_sag=hangoff_section,
            sag_to_buoyancy=sag_hog_results['sag_to_buoyancy'],
            buoyancy_to_hog=sag_hog_results['buoyancy_to_hog'],
            hog_to_buoyancy_end=sag_hog_results['hog_to_buoyancy'],
            buoyancy_to_touchdown=buoyancy_to_touchdown,
            total_arc_length=total_arc,
            total_horizontal_distance=total_horizontal,
            horizontal_force=Fh,
            vertical_force=Fv,
            segments=segments,
            summary=summary
        )

    def _solve_hangoff_section(self, config: LazyWaveConfiguration) -> LazyWaveSegment:
        """
        Solve hang-off section using departure angle.

        Ports catenaryEquation with q (angle) parameter.
        Legacy: lines 51-63 of catenaryMethods.py

        Equations:
            tanq = tan(90° - q)
            BendRadius = d * cos(90° - q) / (1 - cos(90° - q))
            S = BendRadius * tanq
            X = BendRadius * asinh(tanq)

        Parameters:
            config (LazyWaveConfiguration): Configuration with hangoff_angle

        Returns:
            LazyWaveSegment: Hang-off section geometry
        """
        q = config.hangoff_angle
        d = config.hangoff_below_msl

        # Convert to radians and compute complementary angle
        angle_rad = math.radians(90 - q)

        tanq = math.tan(angle_rad)
        cos_angle = math.cos(angle_rad)

        # Bend radius formula
        BendRadius = d * cos_angle / (1 - cos_angle)

        # Arc length
        S = BendRadius * tanq

        # Horizontal distance
        X = BendRadius * math.asinh(tanq)

        return LazyWaveSegment(
            arc_length=S,
            horizontal_distance=X,
            vertical_distance=d,
            bend_radius=BendRadius,
            weight_per_length=config.weight_without_buoyancy
        )

    def _solve_sag_hog_sections(self, config: LazyWaveConfiguration) -> Dict[str, LazyWaveSegment]:
        """
        Solve sag-hog-buoyancy sections.

        Ports sagHogEquation from legacy catenaryMethods.py (lines 93-153).

        Computes four segments:
        1. Sag to Buoyancy: Weight-dominated section
        2. Buoyancy to Hog: Buoyancy-dominated upward section
        3. Hog to Buoyancy: Returning section
        4. Buoyancy to TouchDown: Final catenary

        Each uses catenary formulas with appropriate weight ratios.

        Parameters:
            config (LazyWaveConfiguration): Configuration

        Returns:
            Dict with keys: 'sag_to_buoyancy', 'buoyancy_to_hog',
                           'hog_to_buoyancy', 'buoyancy_to_touchdown'
        """
        w = config.weight_without_buoyancy
        w_buoy = config.weight_with_buoyancy
        hog_elev = config.hog_bend_above_seabed
        sag_elev = config.sag_bend_elevation
        initial_bend_radius = config.hangoff_bend_radius

        # Sag to Buoyancy Configuration
        # Legacy: lines 94-106
        BendRadius_sag = initial_bend_radius
        d_sag = (
            (hog_elev - sag_elev) * abs(w_buoy) /
            (abs(w_buoy) + abs(w))
        )
        X_sag = BendRadius_sag * math.acosh(d_sag / BendRadius_sag + 1)
        S_sag = BendRadius_sag * math.sinh(X_sag / BendRadius_sag)

        sag_to_buoyancy = LazyWaveSegment(
            arc_length=S_sag,
            horizontal_distance=X_sag,
            vertical_distance=d_sag,
            bend_radius=BendRadius_sag,
            weight_per_length=w
        )

        # Buoyancy to Hog Configuration
        # Legacy: lines 108-124
        BendRadius_buoy_to_hog = (
            BendRadius_sag * w / abs(w_buoy)
        )
        d_buoy_to_hog = (
            (hog_elev - sag_elev) * abs(w) /
            (abs(w_buoy) + abs(w))
        )
        X_buoy_to_hog = BendRadius_buoy_to_hog * math.acosh(d_buoy_to_hog / BendRadius_buoy_to_hog + 1)
        S_buoy_to_hog = BendRadius_buoy_to_hog * math.sinh(X_buoy_to_hog / BendRadius_buoy_to_hog)

        buoyancy_to_hog = LazyWaveSegment(
            arc_length=S_buoy_to_hog,
            horizontal_distance=X_buoy_to_hog,
            vertical_distance=d_buoy_to_hog,
            bend_radius=BendRadius_buoy_to_hog,
            weight_per_length=w_buoy
        )

        # Hog to Buoyancy Configuration
        # Legacy: lines 126-137
        d_hog_to_buoy = (
            hog_elev * abs(w) /
            (abs(w_buoy) + abs(w))
        )
        X_hog_to_buoy = BendRadius_buoy_to_hog * math.acosh(d_hog_to_buoy / BendRadius_buoy_to_hog + 1)
        S_hog_to_buoy = BendRadius_buoy_to_hog * math.sinh(X_hog_to_buoy / BendRadius_buoy_to_hog)

        hog_to_buoyancy = LazyWaveSegment(
            arc_length=S_hog_to_buoy,
            horizontal_distance=X_hog_to_buoy,
            vertical_distance=d_hog_to_buoy,
            bend_radius=BendRadius_buoy_to_hog,
            weight_per_length=w_buoy
        )

        # Buoyancy to TouchDown Configuration
        # Legacy: lines 139-151
        BendRadius_touchdown = initial_bend_radius
        d_touchdown = (
            hog_elev * abs(w_buoy) /
            (abs(w_buoy) + abs(w))
        )
        X_touchdown = BendRadius_touchdown * math.acosh(d_touchdown / BendRadius_touchdown + 1)
        S_touchdown = BendRadius_touchdown * math.sinh(X_touchdown / BendRadius_touchdown)

        buoyancy_to_touchdown = LazyWaveSegment(
            arc_length=S_touchdown,
            horizontal_distance=X_touchdown,
            vertical_distance=d_touchdown,
            bend_radius=BendRadius_touchdown,
            weight_per_length=w
        )

        return {
            'sag_to_buoyancy': sag_to_buoyancy,
            'buoyancy_to_hog': buoyancy_to_hog,
            'hog_to_buoyancy': hog_to_buoyancy,
            'buoyancy_to_touchdown': buoyancy_to_touchdown
        }

    def to_legacy_dict(self, results: LazyWaveResults, config: LazyWaveConfiguration) -> Dict[str, Any]:
        """
        Convert modern results to legacy dict format for backward compatibility.

        Matches the output structure of lazyWaveCatenaryEquation.

        Parameters:
            results (LazyWaveResults): Modern results
            config (LazyWaveConfiguration): Input configuration

        Returns:
            dict: Legacy format matching original catenaryMethods output
        """
        return {
            'HangOff': {
                'S': results.hangoff_to_sag.arc_length,
                'X': results.hangoff_to_sag.horizontal_distance,
                'BendRadius': results.hangoff_to_sag.bend_radius,
                'd': config.hangoff_below_msl,
                'q': config.hangoff_angle
            },
            'SagToBuoyancy': {
                'd': results.sag_to_buoyancy.vertical_distance,
                'S': results.sag_to_buoyancy.arc_length,
                'X': results.sag_to_buoyancy.horizontal_distance,
                'BendRadius': results.sag_to_buoyancy.bend_radius
            },
            'BuoyancyToHog': {
                'd': results.buoyancy_to_hog.vertical_distance,
                'S': results.buoyancy_to_hog.arc_length,
                'X': results.buoyancy_to_hog.horizontal_distance,
                'BendRadius': results.buoyancy_to_hog.bend_radius
            },
            'HogToBuoyancy': {
                'd': results.hog_to_buoyancy_end.vertical_distance,
                'S': results.hog_to_buoyancy_end.arc_length,
                'X': results.hog_to_buoyancy_end.horizontal_distance,
                'BendRadius': results.hog_to_buoyancy_end.bend_radius
            },
            'BuoyancyToTouchDown': {
                'd': results.buoyancy_to_touchdown.vertical_distance,
                'S': results.buoyancy_to_touchdown.arc_length,
                'X': results.buoyancy_to_touchdown.horizontal_distance,
                'BendRadius': results.buoyancy_to_touchdown.bend_radius
            },
            'Summary': results.summary,
            'WeightPerUnitLengthWithOutBuoyancy': config.weight_without_buoyancy,
            'WeightPerUnitLengthWithBuoyancy': config.weight_with_buoyancy,
            'HogBendAboveSeabed': config.hog_bend_above_seabed,
            'SagBendElevationAboveSeabed': config.sag_bend_elevation
        }
