"""
Lazy-wave catenary analysis for riser configurations.

Handles multi-segment catenary with buoyancy modules:
- Sag section (weight-dominated, hang-off to buoyancy)
- Hog section (buoyancy-dominated, with buoyancy modules)
- Touch-down section (buoyancy to seabed)

This module ports the legacy lazy-wave implementation from
digitalmodel.subsea.catenary.catenaryMethods with improved type safety
and modern Python patterns.

Mathematical Background:
-----------------------
Lazy-wave risers use buoyancy modules to create a wave-like configuration:
1. Sag section: Catenary under self-weight from vessel to buoyancy start
2. Hog section: Inverse catenary (buoyancy > weight) creating upward arc
3. Touchdown: Final catenary section to seabed

The configuration is solved by:
- Deriving the hang-off bend radius from the hang-off vertical span and departure angle
- Computing the remaining bend radii from weight ratios
- Calculating arc lengths and horizontal distances for each segment
- Summing total forces and geometry

Water-depth closure
-------------------
The construction closes on the seabed exactly, by definition of the hang-off span
rather than by iteration. With the hang-off point taken as the vertical datum and
the seabed ``vertical_distance`` below it, the five segment spans are::

    d1 = vertical_distance - sag_bend_elevation            (down: hang-off -> sag bend)
    d2 = (hog - sag) * |w_b| / (|w_b| + w)                 (up:   sag bend -> buoyancy)
    d3 = (hog - sag) * w     / (|w_b| + w)                 (up:   buoyancy -> hog bend)
    d4 = hog * w     / (|w_b| + w)                         (down: hog bend -> buoyancy)
    d5 = hog * |w_b| / (|w_b| + w)                         (down: buoyancy -> touchdown)

so that ``d2 + d3 == hog - sag`` and ``d4 + d5 == hog``, and the net descent

    d1 - d2 - d3 + d4 + d5 == vertical_distance

identically. Setting ``d1`` to anything other than ``vertical_distance -
sag_bend_elevation`` breaks closure and returns a riser that never reaches bottom;
``LazyWaveResults.vertical_closure_error`` reports the residual and the solver
refuses to return a configuration that does not close (issue #1949, defect 2).

Sign convention
---------------
``weight_without_buoyancy`` (w) is the submerged weight of the bare riser and is
positive (net downward). ``weight_with_buoyancy`` (w_b) is the net effective weight
over the buoyed length and must be negative (net upward) -- that is what makes the
hog bend arc upward. A non-negative w_b describes a "buoyed" section that is still
net-heavy: no hog bend can form and there is no lazy-wave configuration to report.
Such a configuration is rejected rather than silently folded onto its mirror image
by ``abs()`` (issue #1949, defect 1).

Hang-off bend radius
--------------------
There is exactly one bend radius at hang-off and it is *derived* from the hang-off
vertical span and the departure angle. It is reused verbatim for the sag and
touchdown segments and for the force balance. It is not a free input; supplying an
inconsistent value is rejected (issue #1949, defect 3).

References:
----------
Legacy implementation: catenaryMethods.py sagHogEquation (lines 93-153)
                      catenaryMethods.py lazyWaveCatenaryEquation (lines 156-194)
"""

from dataclasses import dataclass
from typing import Optional, List, Dict, Any
import math


#: Relative tolerance when checking a caller-supplied hang-off bend radius against
#: the value derived from (vertical span, departure angle).
BEND_RADIUS_REL_TOL = 1e-6

#: Absolute tolerance [m] on the water-depth closure residual. The construction is
#: exact, so this only guards against future regressions and float accumulation.
CLOSURE_ABS_TOL = 1e-6


class LazyWaveConfigurationError(ValueError):
    """A lazy-wave configuration is not physically realisable.

    Raised at construction time so that an invalid configuration can never reach
    the solver. Optimisers searching buoyancy configurations (issue #1947) should
    catch this and score the candidate infeasible rather than treat it as a crash.
    """


def derive_hangoff_bend_radius(vertical_span: float, hangoff_angle: float) -> float:
    """Bend radius of the hang-off catenary section.

    The catenary through a departure angle ``q`` from the vertical, descending a
    vertical span ``d``, has bend radius::

        R = d * cos(90 - q) / (1 - cos(90 - q))

    This is the only bend radius at hang-off; the sag and touchdown segments reuse
    it and the force balance is built on it.

    Parameters:
        vertical_span (float): Vertical drop from hang-off to the sag bend [m].
        hangoff_angle (float): Departure angle from the vertical [degrees].

    Returns:
        float: Bend radius [m].

    Raises:
        LazyWaveConfigurationError: If the span or angle is outside the valid range.
    """
    if not 0.0 < hangoff_angle < 90.0:
        raise LazyWaveConfigurationError(
            f"hangoff_angle must be strictly between 0 and 90 degrees from the "
            f"vertical, got {hangoff_angle}."
        )
    if vertical_span <= 0.0:
        raise LazyWaveConfigurationError(
            f"hang-off vertical span must be positive, got {vertical_span}. The "
            f"hang-off point must sit above the sag bend."
        )

    angle_rad = math.radians(90.0 - hangoff_angle)
    cos_angle = math.cos(angle_rad)
    return vertical_span * cos_angle / (1.0 - cos_angle)


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

    Validated on construction: an instance of this class is always a physically
    realisable lazy-wave configuration that closes on the seabed. Invalid input
    raises :class:`LazyWaveConfigurationError`.

    Attributes:
        hangoff_angle (float): Departure angle from the vertical at the vessel
            [degrees]. Strictly between 0 and 90.
        hangoff_below_msl (float): Depth of the hang-off point below mean sea level
            [m]. This is a *reporting datum* used to place the configuration
            relative to the waterline; it is not a catenary span and does not enter
            the geometry solve. The vertical span that does is
            ``vertical_distance``.
        hog_bend_above_seabed (float): Hog bend elevation above seabed [m]. Must
            exceed ``sag_bend_elevation``.
        sag_bend_elevation (float): Sag bend elevation above seabed [m]. Must be
            positive and below the hang-off point.
        weight_without_buoyancy (float): Submerged weight per length of the bare
            riser w [N/m]. Must be positive (net downward).
        weight_with_buoyancy (float): Net effective weight per length over the
            buoyed length w_buoy [N/m]. Must be negative (net upward) -- see the
            module docstring on the sign convention.
        vertical_distance (float): Vertical span from the hang-off point down to
            the seabed [m]. Drives the hang-off section and the water-depth
            closure. Must exceed ``sag_bend_elevation``.
        hangoff_bend_radius (Optional[float]): Bend radius at hang-off [m].
            Derived from (``vertical_distance - sag_bend_elevation``,
            ``hangoff_angle``) when omitted, which is the normal usage. If a value
            is supplied it is checked against the derived one and an inconsistent
            value is rejected -- it is a cross-check, not a free parameter.
    """
    hangoff_angle: float
    hangoff_below_msl: float
    hog_bend_above_seabed: float
    sag_bend_elevation: float
    weight_without_buoyancy: float
    weight_with_buoyancy: float
    vertical_distance: float
    hangoff_bend_radius: Optional[float] = None

    @property
    def hangoff_vertical_span(self) -> float:
        """Vertical drop from the hang-off point to the sag bend [m].

        This is the closure identity: the hang-off section must descend exactly far
        enough to land on a sag bend sitting ``sag_bend_elevation`` above a seabed
        that is ``vertical_distance`` below the hang-off point.
        """
        return self.vertical_distance - self.sag_bend_elevation

    def __post_init__(self) -> None:
        if self.weight_without_buoyancy <= 0.0:
            raise LazyWaveConfigurationError(
                f"weight_without_buoyancy must be positive (the submerged bare riser "
                f"is net-heavy), got {self.weight_without_buoyancy} N/m."
            )

        if self.weight_with_buoyancy >= 0.0:
            raise LazyWaveConfigurationError(
                f"weight_with_buoyancy must be negative (net buoyant) for a lazy-wave "
                f"configuration, got {self.weight_with_buoyancy} N/m. A non-negative "
                f"value describes buoyancy modules that leave the section net-heavy, "
                f"so no upward hog bend can form and there is no lazy-wave "
                f"configuration to report. Increase the buoyancy until the net "
                f"effective weight over the buoyed length is negative."
            )

        if self.sag_bend_elevation <= 0.0:
            raise LazyWaveConfigurationError(
                f"sag_bend_elevation must be positive (the sag bend sits above the "
                f"seabed), got {self.sag_bend_elevation} m."
            )

        if self.hog_bend_above_seabed <= self.sag_bend_elevation:
            raise LazyWaveConfigurationError(
                f"hog_bend_above_seabed ({self.hog_bend_above_seabed} m) must exceed "
                f"sag_bend_elevation ({self.sag_bend_elevation} m); the hog bend is "
                f"the crest of the wave and the sag bend its trough."
            )

        if self.hangoff_below_msl < 0.0:
            raise LazyWaveConfigurationError(
                f"hangoff_below_msl must be non-negative, got "
                f"{self.hangoff_below_msl} m."
            )

        if self.hangoff_vertical_span <= 0.0:
            raise LazyWaveConfigurationError(
                f"vertical_distance ({self.vertical_distance} m) must exceed "
                f"sag_bend_elevation ({self.sag_bend_elevation} m). The hang-off "
                f"point sits {self.hangoff_vertical_span} m above the sag bend, so "
                f"the configuration cannot close on the seabed."
            )

        derived = derive_hangoff_bend_radius(
            vertical_span=self.hangoff_vertical_span,
            hangoff_angle=self.hangoff_angle,
        )

        if self.hangoff_bend_radius is not None and not math.isclose(
            self.hangoff_bend_radius, derived, rel_tol=BEND_RADIUS_REL_TOL
        ):
            raise LazyWaveConfigurationError(
                f"supplied hangoff_bend_radius {self.hangoff_bend_radius} m is "
                f"inconsistent with the {derived} m derived from a "
                f"{self.hangoff_vertical_span} m hang-off span at "
                f"{self.hangoff_angle} degrees. The hang-off bend radius is not a "
                f"free parameter; omit it and it will be derived."
            )

        # Single source of truth for every downstream segment and the force balance.
        self.hangoff_bend_radius = derived


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
        vertical_closure_error (float): Residual of the water-depth closure [m] --
            the net descent of the returned geometry minus the configured
            ``vertical_distance``. Zero to machine precision for any configuration
            the solver returns; a non-zero value would mean the riser does not
            reach the seabed.
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
    vertical_closure_error: float = 0.0


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

        # Step 3b: Water-depth closure check (issue #1949, defect 2).
        # Net descent from the hang-off point: down over the hang-off, hog-to-buoyancy
        # and touchdown segments, up over the two segments that climb to the hog bend.
        net_descent = (
            hangoff_section.vertical_distance
            - sag_hog_results['sag_to_buoyancy'].vertical_distance
            - sag_hog_results['buoyancy_to_hog'].vertical_distance
            + sag_hog_results['hog_to_buoyancy'].vertical_distance
            + buoyancy_to_touchdown.vertical_distance
        )
        closure_error = net_descent - config.vertical_distance
        if abs(closure_error) > CLOSURE_ABS_TOL:
            raise LazyWaveConfigurationError(
                f"lazy-wave geometry does not close on the seabed: net descent "
                f"{net_descent} m against a vertical_distance of "
                f"{config.vertical_distance} m (residual {closure_error} m). "
                f"Refusing to return a riser that never reaches bottom."
            )

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
            'Fv': Fv,
            'VerticalClosureError': closure_error
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
            summary=summary,
            vertical_closure_error=closure_error
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

        The vertical span ``d`` is the drop from the hang-off point to the sag bend,
        ``vertical_distance - sag_bend_elevation``. It is *not* ``hangoff_below_msl``,
        which positions the hang-off relative to the waterline and plays no part in
        the geometry (issue #1949, defect 2). The bend radius is the single derived
        value carried on the configuration (issue #1949, defect 3).

        Parameters:
            config (LazyWaveConfiguration): Configuration with hangoff_angle

        Returns:
            LazyWaveSegment: Hang-off section geometry
        """
        q = config.hangoff_angle
        d = config.hangoff_vertical_span

        # Convert to radians and compute complementary angle
        angle_rad = math.radians(90 - q)

        tanq = math.tan(angle_rad)

        # Derived on the configuration; reused by the sag and touchdown segments
        # and by the force balance so that one radius governs the whole solution.
        BendRadius = config.hangoff_bend_radius

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
        # Signs are guaranteed by LazyWaveConfiguration.__post_init__: w > 0 (net
        # heavy) and w_buoy < 0 (net buoyant). The magnitudes below are therefore
        # unambiguous, where the previous `abs()` calls silently accepted a
        # net-heavy "buoyed" section and returned its mirror image (issue #1949,
        # defect 1). For a valid configuration these are numerically identical to
        # the legacy expressions, so legacy equivalence is preserved exactly.
        w = config.weight_without_buoyancy
        w_buoy = config.weight_with_buoyancy
        w_mag = w
        w_buoy_mag = -w_buoy
        hog_elev = config.hog_bend_above_seabed
        sag_elev = config.sag_bend_elevation
        initial_bend_radius = config.hangoff_bend_radius

        # Sag to Buoyancy Configuration
        # Legacy: lines 94-106
        BendRadius_sag = initial_bend_radius
        d_sag = (
            (hog_elev - sag_elev) * w_buoy_mag /
            (w_buoy_mag + w_mag)
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
            BendRadius_sag * w_mag / w_buoy_mag
        )
        d_buoy_to_hog = (
            (hog_elev - sag_elev) * w_mag /
            (w_buoy_mag + w_mag)
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
            hog_elev * w_mag /
            (w_buoy_mag + w_mag)
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
            hog_elev * w_buoy_mag /
            (w_buoy_mag + w_mag)
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
                # The legacy HangOff['d'] is the hang-off -> sag bend span, not the
                # depth below MSL (issue #1949, defect 2).
                'd': results.hangoff_to_sag.vertical_distance,
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
            'SagBendElevationAboveSeabed': config.sag_bend_elevation,
            'VerticalDistance': config.vertical_distance,
            'HangoffBelowMeanSeaLevel': config.hangoff_below_msl
        }
