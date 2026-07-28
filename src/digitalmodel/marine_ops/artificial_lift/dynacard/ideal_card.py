# ABOUTME: Ideal/reference card generation for sucker rod pump diagnostics.
# ABOUTME: Generates theoretical pump cards for comparison with measured cards.

from typing import Optional, Tuple
import numpy as np

from .models import (
    DynacardAnalysisContext,
    CardData,
    IdealCardAnalysis,
    FluidLoadAnalysis,
)
from .base import BaseCalculator
from .constants import STEEL_DENSITY_LB_PER_FT3
from .exceptions import DynacardException, ValidationError, invalid_value_error


# Number of samples used when resampling a card branch onto a common
# normalised-position grid for loop-aware comparison.
BRANCH_RESAMPLE_POINTS = 64


def _split_closed_loop_branches(
    position: np.ndarray,
    load: np.ndarray,
) -> Optional[Tuple[Tuple[np.ndarray, np.ndarray], Tuple[np.ndarray, np.ndarray]]]:
    """
    Split a closed dynamometer loop into its upstroke and downstroke branches.

    A dynamometer card is a closed loop: load is a *double-valued* function of
    position (one value on the upstroke, another on the downstroke). Any
    comparison that treats the whole loop as a single-valued function of
    position is meaningless. Splitting at the position extremes yields two
    branches that ARE single-valued in position and can be compared.

    Args:
        position: Position samples in traversal (time) order.
        load: Load samples in traversal (time) order.

    Returns:
        ((up_position, up_load), (down_position, down_load)) where the first
        branch is the traversal segment running from minimum to maximum
        position, or None if the loop is degenerate (too few samples, or zero
        position range).
    """
    pos = np.asarray(position, dtype=float)
    ld = np.asarray(load, dtype=float)

    if pos.size != ld.size or pos.size < 4:
        return None

    # Drop an explicit closing point (last sample duplicates the first).
    if pos[0] == pos[-1] and ld[0] == ld[-1]:
        pos = pos[:-1]
        ld = ld[:-1]

    n = pos.size
    if n < 4:
        return None

    i_min = int(np.argmin(pos))
    i_max = int(np.argmax(pos))
    if pos[i_max] - pos[i_min] <= 0.0:
        return None

    idx = np.arange(n)
    up_idx = np.roll(idx, -i_min)
    up_len = (i_max - i_min) % n
    if up_len < 1:
        return None
    up_idx = up_idx[: up_len + 1]

    down_idx = np.roll(idx, -i_max)
    down_len = (i_min - i_max) % n
    if down_len < 1:
        return None
    down_idx = down_idx[: down_len + 1]

    if up_idx.size < 2 or down_idx.size < 2:
        return None

    return (
        (pos[up_idx], ld[up_idx]),
        (pos[down_idx], ld[down_idx]),
    )


def _resample_branch(
    branch_position: np.ndarray,
    branch_load: np.ndarray,
    grid: np.ndarray,
    p_min: float,
    p_span: float,
) -> Optional[np.ndarray]:
    """
    Resample a single branch onto a normalised-position grid.

    The branch is sorted by position (a branch is monotonic in position up to
    measurement noise) and duplicate positions are averaged, so that
    ``np.interp`` receives a strictly increasing abscissa.

    Args:
        branch_position: Positions along the branch.
        branch_load: Loads along the branch.
        grid: Normalised positions (0-1) to sample at.
        p_min: Minimum position of the parent card.
        p_span: Position range of the parent card.

    Returns:
        Loads sampled at ``grid``, or None if the branch is degenerate.
    """
    if p_span <= 0.0:
        return None

    x = (np.asarray(branch_position, dtype=float) - p_min) / p_span
    y = np.asarray(branch_load, dtype=float)

    order = np.argsort(x, kind="stable")
    x = x[order]
    y = y[order]

    x_unique, inverse = np.unique(x, return_inverse=True)
    if x_unique.size < 2:
        return None
    counts = np.bincount(inverse)
    sums = np.bincount(inverse, weights=y)
    y_unique = sums / counts

    return np.interp(grid, x_unique, y_unique)


def _branch_profiles(
    position,
    load,
    num_samples: int = BRANCH_RESAMPLE_POINTS,
) -> Optional[Tuple[np.ndarray, np.ndarray]]:
    """
    Reduce a closed card to (upstroke, downstroke) load profiles.

    Both profiles are sampled on the same normalised-position grid (0 = bottom
    of stroke, 1 = top of stroke), which makes the comparison position-aware
    and independent of sample count, sample spacing and starting index.

    Args:
        position: Card positions in traversal order.
        load: Card loads in traversal order.
        num_samples: Number of grid samples per branch.

    Returns:
        (upstroke_loads, downstroke_loads) or None if the card is degenerate.
    """
    branches = _split_closed_loop_branches(np.asarray(position, dtype=float),
                                           np.asarray(load, dtype=float))
    if branches is None:
        return None

    (up_pos, up_load), (down_pos, down_load) = branches
    all_pos = np.concatenate([up_pos, down_pos])
    p_min = float(np.min(all_pos))
    p_span = float(np.max(all_pos) - p_min)

    grid = np.linspace(0.0, 1.0, num_samples)
    up_profile = _resample_branch(up_pos, up_load, grid, p_min, p_span)
    down_profile = _resample_branch(down_pos, down_load, grid, p_min, p_span)

    if up_profile is None or down_profile is None:
        return None

    return up_profile, down_profile


def _closed_loop_shape_similarity(
    measured_position,
    measured_load,
    ideal_position,
    ideal_load,
) -> Optional[float]:
    """
    Position-aware, sign-preserving shape similarity between two closed cards.

    Each card is split into its upstroke and downstroke branches, both branches
    are resampled onto a common normalised-position grid, and the loads are
    normalised (centred on the card mean, scaled by the card load range) so
    that shape rather than magnitude is compared. The score is derived from the
    RMS normalised distance ``d`` between the two cards as ``1 / (1 + d)``.

    Properties (none of which the previous index-wise correlation had):
      * 1.0 only for geometrically identical normalised loops, regardless of
        sample count, sample spacing or starting index.
      * Strictly monotonic in distance, so it never saturates: an inverted card
        (loads reflected about the mean, distance ~2x the card amplitude)
        always scores strictly LOWER than an unrelated card (distance ~1.4x),
        which in turn scores lower than a matching card.
      * Uses position, so a card whose branches are swapped is not scored as
        identical.

    Returns:
        Similarity in (0, 1], or None when either card is degenerate (fewer
        than four samples, zero position range or zero load range). None means
        "not computable" - it is never reported as a number.
    """
    measured = _branch_profiles(measured_position, measured_load)
    ideal = _branch_profiles(ideal_position, ideal_load)
    if measured is None or ideal is None:
        return None

    normalised = []
    for profiles in (measured, ideal):
        stacked = np.concatenate(profiles)
        scale = float(np.max(stacked) - np.min(stacked))
        if scale <= 0.0:
            return None
        centre = float(np.mean(stacked))
        normalised.append((stacked - centre) / scale)

    diff = normalised[0] - normalised[1]
    distance = float(np.sqrt(np.mean(diff ** 2)))

    return float(1.0 / (1.0 + distance))


class IdealCardCalculator(BaseCalculator[IdealCardAnalysis]):
    """
    Generates ideal/reference dynacard for comparison with measured cards.

    The ideal card represents theoretical pump behavior under perfect conditions:
    - 100% pump fillage (or specified fillage)
    - No gas interference
    - Proper valve operation
    - No rod buckling or pump-off

    The ideal pump card is approximately rectangular:
    - Upstroke: Rod carries fluid load + buoyant weight
    - Downstroke: Rod carries only buoyant weight

    Comparing measured cards to ideal cards helps identify pump problems.
    """

    # Physical constants
    STEEL_DENSITY = STEEL_DENSITY_LB_PER_FT3

    def _create_result(self) -> IdealCardAnalysis:
        return IdealCardAnalysis()

    def calculate(self) -> IdealCardAnalysis:
        """
        Execute ideal card generation with default parameters.

        This method satisfies the BaseCalculator abstract interface.
        For full control over generation parameters, use generate() directly.

        Returns:
            IdealCardAnalysis with ideal card data and comparison metrics.
        """
        return self.generate()

    def generate(
        self,
        fillage: float = 1.0,
        fluid_load: Optional[float] = None,
        num_points: int = 100,
    ) -> IdealCardAnalysis:
        """
        Generate ideal pump and surface cards.

        Args:
            fillage: Assumed pump fillage (0-1). Default 1.0 (100%).
            fluid_load: Fluid load in lbs. If None, calculated from context.
            num_points: Number of points in the card.

        Returns:
            IdealCardAnalysis with ideal card data and comparison metrics.

        Raises:
            ValidationError: If fillage or num_points are invalid.
        """
        if not 0.0 <= fillage <= 1.0:
            raise invalid_value_error("fillage", fillage, "must be between 0 and 1")

        if num_points < 4:
            raise invalid_value_error("num_points", num_points, "must be at least 4")

        self.result.fillage_assumed = fillage
        self.result.num_time_points = num_points
        # Provenance: this generator uses closed-form rectangular/ramped cards,
        # not a wave-equation simulation. Assigned explicitly so the field is
        # not merely a model default that happens to read as provenance.
        self.result.generation_method = "simplified"

        # Calculate fluid load if not provided
        if fluid_load is None:
            fluid_load = self._calculate_fluid_load()

        self.result.ideal_fluid_load = fluid_load

        # Calculate buoyant rod weight
        buoyant_weight = self._calculate_buoyant_weight()

        # Calculate stroke length
        stroke_length = self._calculate_stroke_length()
        self.result.ideal_stroke_length = stroke_length

        # Generate ideal pump card (rectangular for 100% fillage)
        self._generate_ideal_pump_card(
            fluid_load=fluid_load,
            buoyant_weight=buoyant_weight,
            stroke_length=stroke_length,
            fillage=fillage,
            num_points=num_points,
        )

        # Generate ideal surface card
        self._generate_ideal_surface_card(
            fluid_load=fluid_load,
            buoyant_weight=buoyant_weight,
            stroke_length=stroke_length,
            num_points=num_points,
        )

        # Calculate card metrics
        self._calculate_card_metrics()

        # Calculate deviation from measured card
        self._calculate_deviation_from_measured()

        return self.result

    def _calculate_fluid_load(self) -> float:
        """Calculate theoretical fluid load from context."""
        pump_area = np.pi * (self.ctx.pump.diameter / 2) ** 2  # in^2
        pump_depth = self.ctx.pump.depth  # feet
        fluid_density = self.ctx.fluid_density  # lbs/ft^3

        # Fluid gradient in psi/ft (fluid density / 144)
        fluid_gradient = fluid_density / 144.0

        # Fluid load = pump area * pressure at pump
        # Pressure at pump = fluid gradient * pump depth
        pressure = fluid_gradient * pump_depth  # psi
        fluid_load = pump_area * pressure  # lbs

        return float(fluid_load)

    def _calculate_buoyant_weight(self) -> float:
        """Calculate buoyant rod weight."""
        rod_weight = self.ctx.rod_weight  # lbs
        fluid_density = self.ctx.fluid_density  # lbs/ft^3

        buoyancy_factor = 1.0 - fluid_density / self.STEEL_DENSITY
        buoyant_weight = rod_weight * buoyancy_factor

        return float(buoyant_weight)

    def _calculate_stroke_length(self) -> float:
        """Calculate effective stroke length."""
        # Use surface unit stroke if available
        if self.ctx.surface_unit.stroke_length > 0:
            return self.ctx.surface_unit.stroke_length

        # Otherwise estimate from surface card
        if self.ctx.surface_card and len(self.ctx.surface_card.position) > 0:
            positions = np.array(self.ctx.surface_card.position)
            return float(np.max(positions) - np.min(positions))

        # Default stroke length
        return 100.0  # inches

    def _generate_ideal_pump_card(
        self,
        fluid_load: float,
        buoyant_weight: float,
        stroke_length: float,
        fillage: float,
        num_points: int,
    ) -> None:
        """
        Generate ideal pump card.

        For 100% fillage, the ideal pump card is rectangular:
        - Bottom: Unloaded position (downstroke)
        - Top: Full fluid load (upstroke)
        - Left: Bottom of stroke
        - Right: Top of stroke

        For incomplete fillage, the card is modified with a transition region.
        """
        # Calculate load levels
        min_load = 0.0  # Unloaded (traveling valve open)
        max_load = fluid_load * fillage  # Full fluid load

        # Generate position array (0 to stroke_length)
        positions = np.linspace(0, stroke_length, num_points // 2)

        # Create ideal pump card (rectangular loop)
        pump_positions = []
        pump_loads = []

        # Upstroke: Bottom to top, carrying fluid load
        for pos in positions:
            pump_positions.append(float(pos))
            pump_loads.append(float(max_load))

        # Downstroke: Top to bottom, unloaded
        for pos in reversed(positions):
            pump_positions.append(float(pos))
            pump_loads.append(float(min_load))

        # Close the loop
        pump_positions.append(pump_positions[0])
        pump_loads.append(pump_loads[0])

        self.result.ideal_pump_position = pump_positions
        self.result.ideal_pump_load = pump_loads

    def _generate_ideal_surface_card(
        self,
        fluid_load: float,
        buoyant_weight: float,
        stroke_length: float,
        num_points: int,
    ) -> None:
        """
        Generate ideal surface card.

        The surface card includes:
        - Buoyant rod weight (always present)
        - Fluid load (only during upstroke)
        - Dynamic effects from wave equation (simplified as smooth transitions)
        """
        # Calculate load levels
        min_load = buoyant_weight  # Downstroke: just rod weight
        max_load = buoyant_weight + fluid_load  # Upstroke: rod + fluid

        # Generate position array
        positions = np.linspace(0, stroke_length, num_points // 2)

        # Create ideal surface card with rounded corners
        surface_positions = []
        surface_loads = []

        # Upstroke: Increasing load with smooth transition
        transition_points = max(3, num_points // 10)
        for i, pos in enumerate(positions):
            surface_positions.append(float(pos))

            # Smooth transition at start of upstroke
            if i < transition_points:
                # Transition from min to max load
                frac = i / transition_points
                load = min_load + (max_load - min_load) * frac
            else:
                load = max_load

            surface_loads.append(float(load))

        # Downstroke: Decreasing load with smooth transition
        for i, pos in enumerate(reversed(positions)):
            surface_positions.append(float(pos))

            if i < transition_points:
                # Transition from max to min load
                frac = i / transition_points
                load = max_load - (max_load - min_load) * frac
            else:
                load = min_load

            surface_loads.append(float(load))

        # Close the loop
        surface_positions.append(surface_positions[0])
        surface_loads.append(surface_loads[0])

        self.result.ideal_surface_position = surface_positions
        self.result.ideal_surface_load = surface_loads

    def _calculate_card_metrics(self) -> None:
        """
        Calculate scalar metrics for the ideal card.

        Pump-domain and surface-domain scalars are reported separately and
        named for their domain. They are NOT interchangeable: the pump card
        carries fluid load only, while the surface card additionally carries
        the buoyant rod weight, so pump peak load is far below the polished rod
        peak load for the same well. Comparing a measured PPRL against
        ``ideal_peak_load`` (a pump-domain number) produces a meaningless
        multiple; compare it against ``ideal_surface_peak_load``.
        """
        if self.result.ideal_pump_load:
            pump_loads = np.array(self.result.ideal_pump_load)
            pump_positions = np.array(self.result.ideal_pump_position)

            self.result.ideal_peak_load = float(np.max(pump_loads))
            self.result.ideal_min_load = float(np.min(pump_loads))
            self.result.ideal_card_area = self._shoelace_area(pump_positions, pump_loads)

        if self.result.ideal_surface_load:
            surface_loads = np.array(self.result.ideal_surface_load)
            surface_positions = np.array(self.result.ideal_surface_position)

            self.result.ideal_surface_peak_load = float(np.max(surface_loads))
            self.result.ideal_surface_min_load = float(np.min(surface_loads))
            self.result.ideal_surface_card_area = self._shoelace_area(
                surface_positions, surface_loads
            )

    @staticmethod
    def _shoelace_area(positions: np.ndarray, loads: np.ndarray) -> float:
        """Enclosed area of a closed card via the shoelace formula (in-lbs)."""
        n = len(positions)
        area = 0.0
        for i in range(n - 1):
            j = (i + 1) % n
            area += (positions[j] - positions[i]) * (loads[j] + loads[i])
        return float(abs(area * 0.5))

    def _calculate_deviation_from_measured(self) -> None:
        """
        Calculate deviation between the ideal and measured surface cards.

        Both cards are closed loops, so load is a double-valued function of
        position. The comparison is therefore done branch by branch (upstroke
        against upstroke, downstroke against downstroke) on a common
        normalised-position grid. Deviation metrics are set to None whenever a
        card is too degenerate to split, rather than being reported as a number
        computed against an ill-defined reference.
        """
        self.result.load_deviation_rms = None
        self.result.stroke_length_difference = None
        self.result.shape_similarity = None

        if not self.ctx.surface_card or len(self.ctx.surface_card.position) == 0:
            self.result.warning_message = "No measured card available for comparison"
            return

        measured_pos = np.array(self.ctx.surface_card.position, dtype=float)
        measured_load = np.array(self.ctx.surface_card.load, dtype=float)

        ideal_pos = np.array(self.result.ideal_surface_position, dtype=float)
        ideal_load = np.array(self.result.ideal_surface_load, dtype=float)

        # Stroke length difference (a single scalar difference, not an RMS).
        measured_stroke = float(np.max(measured_pos) - np.min(measured_pos))
        self.result.stroke_length_difference = float(
            abs(measured_stroke - self.result.ideal_stroke_length)
        )

        measured_profiles = _branch_profiles(measured_pos, measured_load)
        ideal_profiles = _branch_profiles(ideal_pos, ideal_load)

        if measured_profiles is None or ideal_profiles is None:
            self.result.warning_message = (
                "Measured or ideal card could not be split into upstroke/downstroke "
                "branches; load deviation and shape similarity are not computable"
            )
            return

        # Loop-aware RMS load deviation, in lbs, over both branches.
        diff = np.concatenate(
            [
                measured_profiles[0] - ideal_profiles[0],
                measured_profiles[1] - ideal_profiles[1],
            ]
        )
        self.result.load_deviation_rms = float(np.sqrt(np.mean(diff ** 2)))

        self.result.shape_similarity = _closed_loop_shape_similarity(
            measured_pos, measured_load, ideal_pos, ideal_load
        )


def generate_ideal_card(
    context: DynacardAnalysisContext,
    fillage: float = 1.0,
    fluid_load: Optional[float] = None,
    num_points: int = 100,
    raise_on_error: bool = False,
) -> IdealCardAnalysis:
    """
    Convenience function to generate ideal dynacard.

    Args:
        context: Complete analysis context.
        fillage: Assumed pump fillage (0-1).
        fluid_load: Fluid load in lbs. If None, calculated.
        num_points: Number of points in the card.
        raise_on_error: If True, raises exceptions on validation errors.
                       If False, returns result with warning message set.

    Returns:
        IdealCardAnalysis with ideal card data.

    Raises:
        ValidationError: If raise_on_error=True and validation fails.
    """
    calculator = IdealCardCalculator(context)
    if raise_on_error:
        return calculator.generate(
            fillage=fillage,
            fluid_load=fluid_load,
            num_points=num_points,
        )

    try:
        return calculator.generate(
            fillage=fillage,
            fluid_load=fluid_load,
            num_points=num_points,
        )
    except DynacardException as e:
        calculator.result.warning_message = e.message
        return calculator.result


def calculate_shape_similarity(
    measured_card: CardData,
    ideal_card: CardData,
) -> Optional[float]:
    """
    Calculate shape similarity between two closed dynamometer cards.

    Both cards are compared as closed loops using position as well as load:
    each is split into upstroke and downstroke branches, resampled onto a
    common normalised-position grid, and compared after load normalisation.
    See :func:`_closed_loop_shape_similarity` for the metric definition.

    This deliberately does NOT use an index-wise correlation coefficient. That
    metric ignored position entirely (so it depended on where the trace happened
    to start) and was clamped at zero (so a load-inverted card - a real
    diagnostic signal - scored identically to an unrelated card).

    Args:
        measured_card: Measured dynacard data.
        ideal_card: Ideal/reference dynacard data.

    Returns:
        Similarity score in (0, 1] where 1.0 means geometrically identical
        after normalisation, or None when either card is degenerate (fewer
        than four samples, zero position range or zero load range).
    """
    if not measured_card.load or not ideal_card.load:
        return None

    return _closed_loop_shape_similarity(
        measured_card.position,
        measured_card.load,
        ideal_card.position,
        ideal_card.load,
    )


def calculate_ideal_fluid_load(
    pump_diameter: float,
    pump_depth: float,
    fluid_density: float = 62.4,
) -> float:
    """
    Calculate theoretical fluid load for given pump parameters.

    Args:
        pump_diameter: Pump plunger diameter in inches.
        pump_depth: Pump setting depth in feet.
        fluid_density: Fluid density in lbs/ft^3.

    Returns:
        Fluid load in lbs.
    """
    pump_area = np.pi * (pump_diameter / 2) ** 2  # in^2
    fluid_gradient = fluid_density / 144.0  # psi/ft
    pressure = fluid_gradient * pump_depth  # psi
    fluid_load = pump_area * pressure  # lbs

    return float(fluid_load)


def generate_rectangular_pump_card(
    stroke_length: float,
    fluid_load: float,
    num_points: int = 100,
) -> Tuple[np.ndarray, np.ndarray]:
    """
    Generate a simple rectangular pump card.

    This represents the theoretical ideal pump card with 100% fillage.

    Args:
        stroke_length: Pump stroke length in inches.
        fluid_load: Fluid load in lbs.
        num_points: Number of points per side.

    Returns:
        Tuple of (positions, loads) arrays.
    """
    half_points = num_points // 2

    # Upstroke: bottom to top at full load
    pos_up = np.linspace(0, stroke_length, half_points)
    load_up = np.full(half_points, fluid_load)

    # Downstroke: top to bottom at zero load
    pos_down = np.linspace(stroke_length, 0, half_points)
    load_down = np.zeros(half_points)

    positions = np.concatenate([pos_up, pos_down])
    loads = np.concatenate([load_up, load_down])

    return positions, loads
