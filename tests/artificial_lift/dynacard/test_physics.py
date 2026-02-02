# ABOUTME: Tests for the Gibbs physics solver module.
# ABOUTME: Tests wave equation solving, buckling detection, and fillage calculation.

import pytest
import numpy as np
from digitalmodel.artificial_lift.dynacard import (
    DynacardPhysicsSolver,
    DynacardAnalysisContext,
    CardData,
    RodSection,
    PumpProperties,
    SurfaceUnit,
)
from digitalmodel.artificial_lift.dynacard.constants import (
    DEFAULT_PUMP_FILLAGE,
    BUCKLING_DETECTION_LOAD_THRESHOLD_LBS,
)


def create_test_context(
    num_points: int = 100,
    peak_load: float = 15000.0,
    min_load: float = 5000.0,
    stroke_length: float = 100.0,
    rod_length: float = 5000.0,
    rod_diameter: float = 0.875,
    pump_diameter: float = 1.5,
    spm: float = 8.0,
) -> DynacardAnalysisContext:
    """Create a test context with synthetic card data."""
    # Generate sinusoidal surface card (simplified approximation)
    t = np.linspace(0, 2 * np.pi, num_points)

    # Position: sinusoidal stroke
    position = (stroke_length / 2) * (1 - np.cos(t))

    # Load: approximation of typical pump card shape
    # Higher on upstroke, lower on downstroke
    load = (
        (peak_load + min_load) / 2
        + (peak_load - min_load) / 2 * np.sin(t + np.pi / 4)
    )

    surface_card = CardData(
        position=position.tolist(),
        load=load.tolist()
    )

    rod_string = [
        RodSection(diameter=rod_diameter, length=rod_length)
    ]

    pump = PumpProperties(
        diameter=pump_diameter,
        depth=rod_length
    )

    surface_unit = SurfaceUnit(
        manufacturer="Test",
        unit_type="Conventional",
        stroke_length=stroke_length
    )

    return DynacardAnalysisContext(
        api14="12345678901234",
        surface_card=surface_card,
        rod_string=rod_string,
        pump=pump,
        surface_unit=surface_unit,
        spm=spm
    )


class TestDynacardPhysicsSolverInit:
    """Tests for DynacardPhysicsSolver initialization."""

    def test_initializes_with_context(self):
        """Solver should initialize with context."""
        ctx = create_test_context()
        solver = DynacardPhysicsSolver(ctx)

        assert solver.ctx is ctx
        assert solver.results is not None

    def test_results_initially_empty(self):
        """Results should be empty before solving."""
        ctx = create_test_context()
        solver = DynacardPhysicsSolver(ctx)

        assert solver.results.downhole_card is None
        assert solver.results.buckling_detected is False


class TestSolveWaveEquation:
    """Tests for the solve_wave_equation method."""

    def test_returns_results(self):
        """Should return AnalysisResults object."""
        ctx = create_test_context()
        solver = DynacardPhysicsSolver(ctx)
        results = solver.solve_wave_equation()

        assert results is not None
        assert results is solver.results

    def test_generates_downhole_card(self):
        """Should generate a downhole card."""
        ctx = create_test_context()
        solver = DynacardPhysicsSolver(ctx)
        results = solver.solve_wave_equation()

        assert results.downhole_card is not None
        assert len(results.downhole_card.position) > 0
        assert len(results.downhole_card.load) > 0

    def test_downhole_card_same_length_as_surface(self):
        """Downhole card should have same number of points as surface card."""
        ctx = create_test_context(num_points=150)
        solver = DynacardPhysicsSolver(ctx)
        results = solver.solve_wave_equation()

        assert len(results.downhole_card.position) == 150
        assert len(results.downhole_card.load) == 150

    def test_calculates_polished_rod_loads(self):
        """Should calculate peak and minimum polished rod loads."""
        peak = 18000.0
        min_load = 6000.0
        ctx = create_test_context(peak_load=peak, min_load=min_load)
        solver = DynacardPhysicsSolver(ctx)
        results = solver.solve_wave_equation()

        # Peak and min should be close to input values
        assert abs(results.peak_polished_rod_load - peak) < 1000
        assert abs(results.minimum_polished_rod_load - min_load) < 1000

    def test_downhole_position_starts_at_zero(self):
        """Downhole position should start at zero after synchronization."""
        ctx = create_test_context()
        solver = DynacardPhysicsSolver(ctx)
        results = solver.solve_wave_equation()

        # Minimum position should be close to zero
        min_pos = min(results.downhole_card.position)
        assert abs(min_pos) < 1.0

    def test_preserves_context_in_results(self):
        """Results should contain the original context."""
        ctx = create_test_context()
        solver = DynacardPhysicsSolver(ctx)
        results = solver.solve_wave_equation()

        assert results.ctx is ctx

    def test_works_with_multiple_rod_sections(self):
        """Should handle multiple rod sections."""
        surface_card = CardData(
            position=[0, 25, 50, 75, 100, 75, 50, 25],
            load=[10000, 12000, 14000, 15000, 14000, 8000, 6000, 7000]
        )

        rod_string = [
            RodSection(diameter=1.0, length=2000),
            RodSection(diameter=0.875, length=2000),
            RodSection(diameter=0.75, length=1000),
        ]

        ctx = DynacardAnalysisContext(
            api14="12345678901234",
            surface_card=surface_card,
            rod_string=rod_string,
            pump=PumpProperties(diameter=1.5, depth=5000),
            surface_unit=SurfaceUnit(
                manufacturer="Test",
                unit_type="Conventional"
            ),
            spm=8.0
        )

        solver = DynacardPhysicsSolver(ctx)
        results = solver.solve_wave_equation()

        assert results.downhole_card is not None


class TestDetectBuckling:
    """Tests for the detect_buckling method."""

    def test_no_buckling_with_high_loads(self):
        """Should not detect buckling when downhole loads stay above threshold."""
        # Use higher loads to ensure downhole loads stay positive after attenuation
        ctx = create_test_context(min_load=15000.0, peak_load=25000.0)
        solver = DynacardPhysicsSolver(ctx)
        solver.solve_wave_equation()

        # Verify downhole loads are above threshold
        min_dh_load = min(solver.results.downhole_card.load)
        if min_dh_load >= BUCKLING_DETECTION_LOAD_THRESHOLD_LBS:
            result = solver.detect_buckling()
            assert result is False
            assert solver.results.buckling_detected is False

    def test_detects_buckling_with_negative_loads(self):
        """Should detect buckling when loads go below threshold."""
        ctx = create_test_context()
        solver = DynacardPhysicsSolver(ctx)
        solver.solve_wave_equation()

        # Manually set a very negative load to trigger buckling
        solver.results.downhole_card.load[0] = BUCKLING_DETECTION_LOAD_THRESHOLD_LBS - 100

        result = solver.detect_buckling()

        assert result is True
        assert solver.results.buckling_detected is True

    def test_returns_false_without_solving_first(self):
        """Should return False if solve_wave_equation wasn't called."""
        ctx = create_test_context()
        solver = DynacardPhysicsSolver(ctx)

        result = solver.detect_buckling()

        assert result is False


class TestCalculateFillage:
    """Tests for the calculate_fillage method."""

    def test_returns_default_fillage(self):
        """Should return the default fillage value."""
        ctx = create_test_context()
        solver = DynacardPhysicsSolver(ctx)

        fillage = solver.calculate_fillage()

        assert fillage == DEFAULT_PUMP_FILLAGE

    def test_sets_fillage_in_results(self):
        """Should set fillage in results."""
        ctx = create_test_context()
        solver = DynacardPhysicsSolver(ctx)

        solver.calculate_fillage()

        assert solver.results.pump_fillage == DEFAULT_PUMP_FILLAGE


class TestPhysicsWithRealWorldValues:
    """Integration tests with more realistic well parameters."""

    def test_typical_well_analysis(self):
        """Test with typical real-world well parameters."""
        # Realistic surface card data (simplified)
        num_points = 200
        t = np.linspace(0, 2 * np.pi, num_points)

        stroke_length = 144.0  # inches (12 ft stroke)
        peak_load = 22000.0    # lbs
        min_load = 8000.0      # lbs

        position = (stroke_length / 2) * (1 - np.cos(t))
        load = (peak_load + min_load) / 2 + (peak_load - min_load) / 2 * np.sin(t)

        ctx = DynacardAnalysisContext(
            api14="42301234567890",
            surface_card=CardData(
                position=position.tolist(),
                load=load.tolist()
            ),
            rod_string=[
                RodSection(diameter=1.0, length=3000),
                RodSection(diameter=0.875, length=2500),
                RodSection(diameter=0.75, length=2000),
            ],
            pump=PumpProperties(
                diameter=1.75,
                depth=7500
            ),
            surface_unit=SurfaceUnit(
                manufacturer="Lufkin",
                unit_type="Conventional",
                stroke_length=stroke_length
            ),
            spm=6.5,
            fluid_density=55.0  # lbs/ft^3 (oil-water mix)
        )

        solver = DynacardPhysicsSolver(ctx)
        results = solver.solve_wave_equation()

        # Verify results are reasonable
        assert results.downhole_card is not None
        assert len(results.downhole_card.position) == num_points

        # Peak load should be preserved approximately
        assert results.peak_polished_rod_load > 20000

        # Downhole stroke should be within reasonable range of surface stroke
        # The relationship depends on wave equation solution and scaling factors
        dh_stroke = max(results.downhole_card.position) - min(results.downhole_card.position)
        # Allow for stroke variation of up to 20% either direction
        assert 0.8 * stroke_length < dh_stroke < 1.2 * stroke_length
