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
        """Calculate metrics for the ideal card."""
        if not self.result.ideal_pump_load:
            return

        pump_loads = np.array(self.result.ideal_pump_load)
        pump_positions = np.array(self.result.ideal_pump_position)

        self.result.ideal_peak_load = float(np.max(pump_loads))
        self.result.ideal_min_load = float(np.min(pump_loads))

        # Calculate card area using shoelace formula
        n = len(pump_positions)
        area = 0.0
        for i in range(n - 1):
            j = (i + 1) % n
            area += (pump_positions[j] - pump_positions[i]) * (pump_loads[j] + pump_loads[i])
        self.result.ideal_card_area = float(abs(area * 0.5))

    def _calculate_deviation_from_measured(self) -> None:
        """Calculate deviation between ideal and measured cards."""
        if not self.ctx.surface_card or len(self.ctx.surface_card.position) == 0:
            self.result.warning_message = "No measured card available for comparison"
            return

        measured_pos = np.array(self.ctx.surface_card.position)
        measured_load = np.array(self.ctx.surface_card.load)

        # Normalize measured card to same number of points
        ideal_pos = np.array(self.result.ideal_surface_position)
        ideal_load = np.array(self.result.ideal_surface_load)

        # Interpolate ideal card to measured positions
        ideal_load_interp = np.interp(
            measured_pos,
            ideal_pos,
            ideal_load,
            period=self.result.ideal_stroke_length,
        )

        # Calculate RMS deviations
        load_diff = measured_load - ideal_load_interp
        self.result.load_deviation_rms = float(np.sqrt(np.mean(load_diff ** 2)))

        # Position deviation (stroke length comparison)
        measured_stroke = float(np.max(measured_pos) - np.min(measured_pos))
        ideal_stroke = self.result.ideal_stroke_length
        self.result.position_deviation_rms = float(abs(measured_stroke - ideal_stroke))

        # Shape similarity (correlation coefficient)
        if len(measured_load) > 2 and np.std(measured_load) > 0 and np.std(ideal_load_interp) > 0:
            correlation = np.corrcoef(measured_load, ideal_load_interp)[0, 1]
            # Convert to 0-1 similarity (handle negative correlation)
            self.result.shape_similarity = float(max(0.0, correlation))
        else:
            self.result.shape_similarity = 0.0


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
) -> float:
    """
    Calculate shape similarity between measured and ideal cards.

    Uses correlation coefficient to compare card shapes.

    Args:
        measured_card: Measured dynacard data.
        ideal_card: Ideal/reference dynacard data.

    Returns:
        Similarity score 0-1 (1 = identical).
    """
    if not measured_card.load or not ideal_card.load:
        return 0.0

    measured_load = np.array(measured_card.load)
    ideal_load = np.array(ideal_card.load)

    # Interpolate to same length if needed
    if len(measured_load) != len(ideal_load):
        x_measured = np.linspace(0, 1, len(measured_load))
        x_ideal = np.linspace(0, 1, len(ideal_load))
        ideal_load = np.interp(x_measured, x_ideal, ideal_load)

    # Calculate correlation
    if np.std(measured_load) > 0 and np.std(ideal_load) > 0:
        correlation = np.corrcoef(measured_load, ideal_load)[0, 1]
        return float(max(0.0, correlation))

    return 0.0


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
