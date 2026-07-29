# ABOUTME: Rod buckling analysis for sucker rod pump diagnostics.
# ABOUTME: Detects sinusoidal and helical buckling based on axial load distribution.

from typing import Literal, Optional, Sequence, Tuple
import numpy as np

from .models import (
    DynacardAnalysisContext,
    CardData,
    RodBucklingAnalysis,
)
from .base import BaseCalculator
from .constants import STEEL_DENSITY_LB_PER_FT3
from .exceptions import DynacardException, ValidationError

LoadDatum = Literal["net_pump_load", "vendor_analysis"]
InclinationProfile = Sequence[Tuple[float, float]]


class RodBucklingCalculator(BaseCalculator[RodBucklingAnalysis]):
    """
    Calculates rod buckling conditions for sucker rod pump systems.

    Rod buckling occurs when compressive loads exceed critical thresholds.
    Two buckling modes are considered:
    - Sinusoidal buckling: Initial buckling mode with wave-like deformation
    - Helical buckling: More severe mode with corkscrew deformation

    The analysis calculates:
    - Buckling tendency (effective axial load) considering buoyancy effects
    - Paslay-Dawson critical buckling loads, when hole inclination and
      rod/tubing radial clearance are supplied
    - Buckling verdicts, derived solely from those thresholds

    It does NOT report a neutral-point depth or any other depth-resolved
    quantity: a dynamometer card is indexed by stroke phase at a single depth,
    so no depth can be recovered from it (see _summarise_compression).
    """

    STEEL_DENSITY = STEEL_DENSITY_LB_PER_FT3
    VENDOR_FLUID_LOAD_REL_TOL = 0.01

    def _create_result(self) -> RodBucklingAnalysis:
        return RodBucklingAnalysis()

    def calculate(
        self,
        downhole_card: Optional[CardData] = None,
        inclination_deg: Optional[float] = None,
        tubing_id: Optional[float] = None,
        *,
        load_datum: LoadDatum,
        inclination_profile: Optional[InclinationProfile] = None,
        vendor_downstroke_load: Optional[float] = None,
        vendor_upstroke_load: Optional[float] = None,
        vendor_fluid_load: Optional[float] = None,
    ) -> RodBucklingAnalysis:
        """
        Perform rod buckling analysis.

        Args:
            downhole_card: Optional downhole card data. If not provided,
                          uses a simplified analysis based on surface card.
            inclination_deg: Hole inclination from vertical at the section of
                          interest, in degrees. Required (with ``tubing_id``)
                          by the Paslay-Dawson critical-load formula. Without
                          it the critical loads - and therefore the buckling
                          verdicts - are reported as None.
            tubing_id: Tubing inside diameter in inches, used with the rod
                          diameter to obtain the radial clearance the
                          Paslay-Dawson formula needs.
            load_datum: Explicit convention for the supplied/estimated loads.
            inclination_profile: ``(measured_depth_ft, inclination_deg)``
                          points covering the bottom rod section.
            vendor_downstroke_load: Vendor downstroke load datum, lb.
            vendor_upstroke_load: Vendor upstroke load, lb.
            vendor_fluid_load: Vendor fluid load (Fo), lb.

        Returns:
            RodBucklingAnalysis with buckling detection results.

        Raises:
            ValidationError: If load data or rod string data is missing/invalid.
        """
        # Record what actually ran, rather than relying on a default string.
        self.result.analysis_method = (
            "downhole_card" if downhole_card is not None
            else "surface_card_estimate"
        )

        # Use provided card or estimate from surface card
        if downhole_card is not None:
            loads = np.array(downhole_card.load)
        else:
            # Estimate downhole loads from surface card
            loads = self._estimate_downhole_loads()

        loads = np.asarray(loads, dtype=float)
        if not np.all(np.isfinite(loads)):
            raise ValidationError(
                "Load data must contain finite values",
                field="load_data",
            )

        loads = self._normalise_load_datum(
            loads,
            load_datum=load_datum,
            vendor_downstroke_load=vendor_downstroke_load,
            vendor_upstroke_load=vendor_upstroke_load,
            vendor_fluid_load=vendor_fluid_load,
        )

        if len(loads) == 0:
            raise ValidationError(
                "No load data available for buckling analysis",
                field="load_data",
                details={"error_type": "empty_data"}
            )

        # Calculate rod string properties
        rod_length = self.ctx.rod_length  # feet
        if rod_length <= 0:
            raise ValidationError(
                "Rod string length must be positive",
                field="rod_length",
                details={"value": rod_length}
            )

        # Calculate critical buckling loads
        self._calculate_critical_loads(
            inclination_deg,
            tubing_id,
            inclination_profile,
        )

        # Calculate buckling tendency along the rod
        buckling_tendency = self._calculate_buckling_tendency(loads)

        # Summarise the compressive part of the tendency (loads only - see
        # _summarise_compression for why no depths are produced)
        self._summarise_compression(buckling_tendency)

        # Detect buckling conditions
        self._detect_buckling(buckling_tendency)

        return self.result

    def _normalise_load_datum(
        self,
        loads: np.ndarray,
        *,
        load_datum: LoadDatum,
        vendor_downstroke_load: Optional[float],
        vendor_upstroke_load: Optional[float],
        vendor_fluid_load: Optional[float],
    ) -> np.ndarray:
        """Return loads on the physical net-pump-load datum."""
        if load_datum == "net_pump_load":
            return loads
        if load_datum != "vendor_analysis":
            raise ValidationError(
                "load_datum must be 'net_pump_load' or 'vendor_analysis'",
                field="load_datum",
                details={"value": load_datum},
            )

        metadata = {
            "downstroke": vendor_downstroke_load,
            "upstroke": vendor_upstroke_load,
            "fluid_load": vendor_fluid_load,
        }
        missing = [name for name, value in metadata.items() if value is None]
        if missing:
            raise ValidationError(
                "Vendor datum correction requires downstroke, upstroke, "
                f"and fluid-load metadata; missing: {', '.join(missing)}",
                field="vendor_load_metadata",
                details={"missing": missing},
            )

        values = np.asarray(list(metadata.values()), dtype=float)
        if not np.all(np.isfinite(values)):
            raise ValidationError(
                "Vendor load metadata must contain finite values",
                field="vendor_load_metadata",
            )

        downstroke, upstroke, fluid_load = values
        if fluid_load <= 0.0:
            raise ValidationError(
                "Vendor fluid load (Fo) must be positive",
                field="vendor_load_metadata",
                details={"fluid_load": float(fluid_load)},
            )
        verified_fluid_load = upstroke - downstroke
        scale = max(abs(fluid_load), 1.0)
        relative_error = abs(verified_fluid_load - fluid_load) / scale
        if relative_error > self.VENDOR_FLUID_LOAD_REL_TOL:
            raise ValidationError(
                "Vendor load metadata is inconsistent: upstroke minus "
                "downstroke does not agree with fluid load (Fo)",
                field="vendor_load_metadata",
                details={"relative_error": relative_error},
            )

        return loads - downstroke

    def _estimate_downhole_loads(self) -> np.ndarray:
        """
        Estimate downhole loads from surface card data.

        Uses the simple approximation:
        F_downhole ≈ F_surface - W_rod (on upstroke)
        F_downhole ≈ F_surface (on downstroke, after fluid transfer)
        """
        surface_loads = np.array(self.ctx.surface_card.load)
        if len(surface_loads) == 0:
            return np.array([])

        # Calculate buoyant rod weight
        rod_weight = self.ctx.rod_weight
        fluid_density = self.ctx.fluid_density
        buoyancy_factor = 1.0 - fluid_density / self.STEEL_DENSITY
        buoyant_weight = rod_weight * buoyancy_factor

        # Simple approximation: downhole load is surface load minus buoyant weight
        # This is a rough estimate - full simulation is needed for accuracy
        downhole_loads = surface_loads - buoyant_weight

        return downhole_loads

    def _calculate_critical_loads(
        self,
        inclination_deg: Optional[float] = None,
        tubing_id: Optional[float] = None,
        inclination_profile: Optional[InclinationProfile] = None,
    ) -> None:
        """
        Calculate Paslay-Dawson critical buckling loads for the rod string.

        Sinusoidal buckling threshold (Dawson & Paslay, 1984)::

            F_cr = 2 * sqrt( E * I * w * sin(alpha) / r_c )

        with units  psi * in^4 * (lb/in) / in  ->  lb^2, so the square root is
        a force in pounds. Helical buckling follows at 2*sqrt(2) ~ 2.828 times
        the sinusoidal threshold (Chen, Lin & Cheatham, 1990).

        Both ``sin(alpha)`` (hole inclination) and ``r_c`` (rod/tubing radial
        clearance) are required. They are properties of the wellbore, not of
        the card, so when they are not supplied the thresholds are left as None
        rather than substituted with a guess.

        Args:
            inclination_deg: Hole inclination from vertical, degrees (> 0).
            tubing_id: Tubing inside diameter, inches.
        """
        if len(self.ctx.rod_string) == 0:
            return

        scalar_inputs = {
            "inclination_deg": inclination_deg,
            "tubing_id": tubing_id,
        }
        non_finite = [
            name
            for name, value in scalar_inputs.items()
            if value is not None and not np.isfinite(value)
        ]
        if non_finite:
            raise ValidationError(
                "Wellbore inputs must contain finite values",
                field=non_finite[0],
                details={"non_finite": non_finite},
            )

        inclinations = self._bottom_section_inclinations(
            inclination_deg,
            inclination_profile,
        )
        if inclinations is None or tubing_id is None:
            self.result.warning_message = (
                "Critical buckling loads not computed: the Paslay-Dawson "
                "formula requires bottom-section inclination and rod/tubing "
                "radial clearance (tubing ID), which card data does not supply."
            )
            return

        # Compression occurs adjacent to the pump, in the bottom rod section.
        section = self.ctx.rod_string[-1]

        # Rod geometry
        d = section.diameter  # inches
        inertia = np.pi * d ** 4 / 64.0  # in^4

        # Material properties
        E = section.modulus_of_elasticity  # psi

        # Buoyant weight per unit length
        buoyancy_factor = 1.0 - self.ctx.fluid_density / self.STEEL_DENSITY
        W_b = section.weight_per_foot * buoyancy_factor / 12.0  # lbs/in

        # Radial clearance between rod body and tubing wall, inches
        r_c = (tubing_id - d) / 2.0

        sin_alpha = float(np.min(np.sin(np.radians(inclinations))))

        if r_c <= 0.0:
            self.result.warning_message = (
                f"Critical buckling loads not computed: tubing ID {tubing_id} in "
                f"leaves no radial clearance around a {d} in rod."
            )
            return

        if sin_alpha <= 0.0:
            self.result.warning_message = (
                "Critical buckling loads not computed: the Paslay-Dawson "
                "formula degenerates at zero inclination. A vertical section "
                "needs a Lubinski-type vertical buckling criterion instead."
            )
            return

        if W_b <= 0.0:
            self.result.warning_message = (
                "Critical buckling loads not computed: buoyant rod weight is "
                "not positive, so the rod is not gravity-stabilised."
            )
            return

        F_cr_sinusoidal = 2.0 * np.sqrt(
            E * inertia * W_b * sin_alpha / r_c
        )

        # Helical buckling threshold = 2*sqrt(2) x sinusoidal (Chen-Lin-Cheatham)
        F_cr_helical = 2.0 * np.sqrt(2.0) * F_cr_sinusoidal

        self.result.sinusoidal_critical_load = float(F_cr_sinusoidal)
        self.result.helical_critical_load = float(F_cr_helical)

    def _bottom_section_inclinations(
        self,
        inclination_deg: Optional[float],
        profile: Optional[InclinationProfile],
    ) -> Optional[np.ndarray]:
        """Return inclinations sampled across the bottom rod's depth interval."""
        if inclination_deg is not None and profile is not None:
            raise ValidationError(
                "Supply either inclination_deg or inclination_profile, not both",
                field="inclination_profile",
            )
        if profile is None:
            if inclination_deg is None:
                return None
            return np.asarray([inclination_deg], dtype=float)
        if len(profile) < 2:
            raise ValidationError(
                "inclination_profile requires at least two depth points",
                field="inclination_profile",
            )

        points = np.asarray(profile, dtype=float)
        if points.ndim != 2 or points.shape[1] != 2 or not np.all(np.isfinite(points)):
            raise ValidationError(
                "inclination_profile must contain finite (depth, inclination) pairs",
                field="inclination_profile",
            )
        depths = points[:, 0]
        if np.any(np.diff(depths) <= 0.0):
            raise ValidationError(
                "inclination_profile depths must be strictly increasing",
                field="inclination_profile",
            )

        section_end = self.ctx.pump.depth
        section_start = section_end - self.ctx.rod_string[-1].length
        if depths[0] > section_start or depths[-1] < section_end:
            raise ValidationError(
                "inclination_profile must cover the bottom rod depth interval",
                field="inclination_profile",
                details={"section_start": section_start, "section_end": section_end},
            )

        inside = points[(depths > section_start) & (depths < section_end), 1]
        endpoints = np.interp(
            [section_start, section_end],
            depths,
            points[:, 1],
        )
        inclinations = np.concatenate(([endpoints[0]], inside, [endpoints[1]]))
        if np.any((inclinations < 0.0) | (inclinations > 180.0)):
            raise ValidationError(
                "inclinations must be between 0 and 180 degrees",
                field="inclination_profile",
            )
        return inclinations

    def _calculate_buckling_tendency(self, loads: np.ndarray) -> np.ndarray:
        """Calculate effective axial load for a solid submerged rod."""
        if len(loads) == 0:
            return np.array([])

        # A solid rod has no bore on which an internal-pressure ballooning term
        # can act. The fully submerged external pressure is already represented
        # through buoyant weight in the Paslay-Dawson threshold.
        buckling_tendency = loads.copy()

        self.result.min_buckling_tendency = float(np.min(buckling_tendency))
        self.result.max_buckling_tendency = float(np.max(buckling_tendency))

        return buckling_tendency

    def _summarise_compression(self, buckling_tendency: np.ndarray) -> None:
        """
        Summarise the compressive part of the buckling tendency.

        Deliberately produces NO depth-resolved quantities.

        A dynamometer card is a closed loop indexed by STROKE PHASE: successive
        samples are successive instants within one pump cycle, all describing
        the SAME point on the rod string (the polished rod, or the pump). They
        are not samples down the length of the rod. Interpolating a
        "neutral point depth" from the position of a sample within the card
        therefore maps a time axis onto a depth axis - a category error that
        supplying a genuine measured downhole card does not fix, because that
        card is still a single-depth time series.

        Producing a neutral-point depth, a compression start depth, or a
        compression length requires an axial load profile resolved along the
        rod (a wave-equation solution over the rod string, or a downhole gauge
        survey). None of those is available here, so those fields stay None
        rather than being fabricated from the card index.

        Only ``max_compressive_load`` is set: it is a LOAD read off the load
        axis, needs no depth information, and is 0.0 when the effective axial
        load never goes compressive (an evaluated zero, distinct from the
        None default meaning "never computed").
        """
        # Depth-resolved outputs are not derivable from a card - keep as None.
        self.result.neutral_point_depth = None
        self.result.neutral_point_fraction = None
        self.result.compression_depth_start = None
        self.result.compression_length = None

        if len(buckling_tendency) == 0:
            return

        min_tendency = float(np.min(buckling_tendency))
        self.result.max_compressive_load = max(0.0, -min_tendency)

    def _detect_buckling(self, buckling_tendency: np.ndarray) -> None:
        """
        Detect sinusoidal and helical buckling conditions.

        The verdicts are derived from the computed critical-load thresholds
        only, and are compared against the same quantity the thresholds
        describe - the effective axial load (buckling tendency), never the raw
        card loads. When the thresholds could not be computed, the verdicts are
        None ("unknown"), not False and not a hard-coded load rule.
        """
        sinusoidal_critical = self.result.sinusoidal_critical_load
        helical_critical = self.result.helical_critical_load

        if sinusoidal_critical is None or helical_critical is None:
            # No threshold -> no verdict. Reported as unknown.
            self.result.sinusoidal_buckling_detected = None
            self.result.helical_buckling_detected = None
            return

        if len(buckling_tendency) == 0:
            return

        # Maximum compressive tendency (most negative value), as a magnitude
        compressive_load = max(0.0, -float(np.min(buckling_tendency)))

        self.result.sinusoidal_buckling_detected = (
            compressive_load > sinusoidal_critical
        )
        self.result.helical_buckling_detected = compressive_load > helical_critical


def calculate_rod_buckling(
    context: DynacardAnalysisContext,
    downhole_card: Optional[CardData] = None,
    raise_on_error: bool = False,
    inclination_deg: Optional[float] = None,
    tubing_id: Optional[float] = None,
    *,
    load_datum: LoadDatum,
    inclination_profile: Optional[InclinationProfile] = None,
    vendor_downstroke_load: Optional[float] = None,
    vendor_upstroke_load: Optional[float] = None,
    vendor_fluid_load: Optional[float] = None,
) -> RodBucklingAnalysis:
    """
    Convenience function to calculate rod buckling analysis.

    Args:
        context: Complete analysis context with rod string and pump data.
        downhole_card: Optional downhole card data for more accurate analysis.
        raise_on_error: If True, raises exceptions on validation errors.
                       If False, returns result with warning message set.
        inclination_deg: Hole inclination from vertical (degrees). Required,
                       with tubing_id, for the Paslay-Dawson critical loads.
        tubing_id: Tubing inside diameter (inches).
        load_datum: ``net_pump_load`` or ``vendor_analysis``.
        inclination_profile: Depth/inclination points covering the bottom rod.
        vendor_downstroke_load: Vendor downstroke datum, lb.
        vendor_upstroke_load: Vendor upstroke load, lb.
        vendor_fluid_load: Vendor fluid load (Fo), lb.

    Returns:
        RodBucklingAnalysis with buckling detection results. Critical loads and
        buckling verdicts are None unless inclination_deg and tubing_id are
        supplied.

    Raises:
        ValidationError: If raise_on_error=True and validation fails.
    """
    calculator = RodBucklingCalculator(context)
    keyword_args = {
        "load_datum": load_datum,
        "inclination_profile": inclination_profile,
        "vendor_downstroke_load": vendor_downstroke_load,
        "vendor_upstroke_load": vendor_upstroke_load,
        "vendor_fluid_load": vendor_fluid_load,
    }
    if raise_on_error:
        return calculator.calculate(
            downhole_card,
            inclination_deg,
            tubing_id,
            **keyword_args,
        )

    try:
        return calculator.calculate(
            downhole_card,
            inclination_deg,
            tubing_id,
            **keyword_args,
        )
    except DynacardException as e:
        calculator.result.warning_message = e.message
        return calculator.result


def estimate_neutral_point(
    surface_load_max: float,
    surface_load_min: float,
    rod_weight: float,
    fluid_density: float = 62.4,
    rod_length: float = 5000.0,
) -> float:
    """
    Estimate neutral point depth from surface card loads.

    Simplified estimation for quick analysis.

    Args:
        surface_load_max: Peak polished rod load (lbs)
        surface_load_min: Minimum polished rod load (lbs)
        rod_weight: Total rod string weight (lbs)
        fluid_density: Fluid density (lbs/ft^3)
        rod_length: Total rod string length (feet)

    Returns:
        Estimated neutral point depth in feet from surface.
    """
    # Buoyancy factor
    steel_density = 490.0
    buoyancy_factor = 1.0 - fluid_density / steel_density

    # Buoyant rod weight
    buoyant_weight = rod_weight * buoyancy_factor

    # Fluid load estimate (difference between upstroke and downstroke)
    fluid_load = surface_load_max - surface_load_min - buoyant_weight

    # Neutral point estimation
    # At neutral point: weight above = fluid load below
    if buoyant_weight > 0:
        neutral_fraction = fluid_load / buoyant_weight
        neutral_depth = rod_length * max(0.0, min(1.0, neutral_fraction))
    else:
        neutral_depth = rod_length

    return neutral_depth


def calculate_critical_buckling_load(
    rod_diameter: float,
    modulus: float = 30500000.0,
    weight_per_foot: float = 0.0,
    fluid_density: float = 62.4,
    inclination_deg: Optional[float] = None,
    tubing_id: Optional[float] = None,
) -> Tuple[Optional[float], Optional[float]]:
    """
    Calculate Paslay-Dawson critical buckling loads for a rod section.

    Sinusoidal threshold (Dawson & Paslay, 1984)::

        F_cr = 2 * sqrt( E * I * w * sin(alpha) / r_c )

    Units: psi * in^4 * (lb/in) / in = lb^2, so the root is a force in pounds.
    Helical buckling follows at 2*sqrt(2) ~ 2.828 x sinusoidal
    (Chen, Lin & Cheatham, 1990).

    Both hole inclination and rod/tubing radial clearance are required inputs
    of the formula. When either is missing the thresholds are undefined and
    ``(None, None)`` is returned - no dimensionally-inconsistent substitute is
    reported.

    Args:
        rod_diameter: Rod diameter in inches.
        modulus: Modulus of elasticity in psi.
        weight_per_foot: Rod weight per foot (calculated if 0).
        fluid_density: Fluid density in lbs/ft^3.
        inclination_deg: Hole inclination from vertical in degrees (> 0).
        tubing_id: Tubing inside diameter in inches (> rod_diameter).

    Returns:
        Tuple of (sinusoidal_critical_load, helical_critical_load) in lbs, or
        (None, None) when the required inputs are unavailable or degenerate.
    """
    if inclination_deg is None or tubing_id is None:
        return (None, None)

    # Calculate rod properties
    r = rod_diameter / 2.0
    inertia = np.pi * rod_diameter ** 4 / 64.0  # in^4

    # Calculate weight if not provided
    if weight_per_foot == 0.0:
        steel_density = 490.0  # lbs/ft^3
        weight_per_foot = np.pi * r ** 2 * steel_density / 144.0

    # Buoyant weight per inch
    buoyancy_factor = 1.0 - fluid_density / 490.0
    W_b = weight_per_foot * buoyancy_factor / 12.0  # lbs/in

    # Radial clearance between rod body and tubing wall, inches
    r_c = (tubing_id - rod_diameter) / 2.0
    sin_alpha = float(np.sin(np.radians(inclination_deg)))

    if W_b <= 0.0 or r_c <= 0.0 or sin_alpha <= 0.0:
        return (None, None)

    F_cr_sinusoidal = 2.0 * np.sqrt(
        modulus * inertia * W_b * sin_alpha / r_c
    )
    F_cr_helical = 2.0 * np.sqrt(2.0) * F_cr_sinusoidal

    return (float(F_cr_sinusoidal), float(F_cr_helical))
