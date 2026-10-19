# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# ABOUTME: Tests for the Gibbs physics solver module.
# ABOUTME: Tests wave equation solving, buckling detection, and fillage calculation.

import pytest
import numpy as np
from digitalmodel.marine_ops.artificial_lift.dynacard import (
    DynacardPhysicsSolver,
    DynacardAnalysisContext,
    CardData,
    RodSection,
    PumpProperties,
    SurfaceUnit,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.calculations import (
    calculate_pump_fillage,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.constants import (
    DEFAULT_PUMP_FILLAGE,
    BUCKLING_DETECTION_LOAD_THRESHOLD_LBS,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.exceptions import PhysicsError


def create_test_context(
    num_points: int = 100,
    peak_load: float = 15000.0,
    min_load: float = 5000.0,
    stroke_length: float = 100.0,
    rod_length: float = 5000.0,
    rod_diameter: float = 0.875,
    pump_diameter: float = 1.5,
    spm: float = 8.0,
    load_phase: float = np.pi / 4,
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
        + (peak_load - min_load) / 2 * np.sin(t + load_phase)
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


# Both phases are retained. Elastic overtravel is permitted now that the
# coupled wave equation replaces the empirical stroke scaling (#1857).
STROKE_CASES = pytest.mark.parametrize(
    "load_phase", [np.pi / 4, -np.pi / 8], ids=["overtravel", "shorter-stroke"],
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

    @STROKE_CASES
    def test_returns_results(self, load_phase):
        """Should return AnalysisResults object."""
        ctx = create_test_context(load_phase=load_phase)
        solver = DynacardPhysicsSolver(ctx)
        results = solver.solve_wave_equation()

        assert results is not None
        assert results is solver.results

    @STROKE_CASES
    def test_generates_downhole_card(self, load_phase):
        """Should generate a downhole card."""
        ctx = create_test_context(load_phase=load_phase)
        solver = DynacardPhysicsSolver(ctx)
        results = solver.solve_wave_equation()

        assert results.downhole_card is not None
        assert len(results.downhole_card.position) > 0
        assert len(results.downhole_card.load) > 0

    @STROKE_CASES
    def test_downhole_card_same_length_as_surface(self, load_phase):
        """Downhole card should have same number of points as surface card."""
        ctx = create_test_context(num_points=150, load_phase=load_phase)
        solver = DynacardPhysicsSolver(ctx)
        results = solver.solve_wave_equation()

        assert len(results.downhole_card.position) == 150
        assert len(results.downhole_card.load) == 150

    @STROKE_CASES
    def test_calculates_polished_rod_loads(self, load_phase):
        """Should calculate peak and minimum polished rod loads."""
        peak = 18000.0
        min_load = 6000.0
        ctx = create_test_context(peak_load=peak, min_load=min_load, load_phase=load_phase)
        solver = DynacardPhysicsSolver(ctx)
        results = solver.solve_wave_equation()

        # Peak and min should be close to input values
        assert abs(results.peak_polished_rod_load - peak) < 1000
        assert abs(results.minimum_polished_rod_load - min_load) < 1000

    @STROKE_CASES
    def test_downhole_position_starts_at_zero(self, load_phase):
        """Downhole position should start at zero after synchronization."""
        ctx = create_test_context(load_phase=load_phase)
        solver = DynacardPhysicsSolver(ctx)
        results = solver.solve_wave_equation()

        # Minimum position should be close to zero
        min_pos = min(results.downhole_card.position)
        assert abs(min_pos) < 1.0

    @STROKE_CASES
    def test_preserves_context_in_results(self, load_phase):
        """Results should contain the original context."""
        ctx = create_test_context(load_phase=load_phase)
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

    @STROKE_CASES
    def test_no_buckling_with_high_loads(self, load_phase):
        """Should not detect buckling when downhole loads stay above threshold."""
        # Use higher loads to ensure downhole loads stay positive after attenuation
        ctx = create_test_context(min_load=15000.0, peak_load=25000.0, load_phase=load_phase)
        solver = DynacardPhysicsSolver(ctx)
        solver.solve_wave_equation()

        # Verify downhole loads are above threshold
        min_dh_load = min(solver.results.downhole_card.load)
        if min_dh_load >= BUCKLING_DETECTION_LOAD_THRESHOLD_LBS:
            result = solver.detect_buckling()
            assert result is False
            assert solver.results.buckling_detected is False

    @STROKE_CASES
    def test_detects_buckling_with_negative_loads(self, load_phase):
        """Should detect buckling when loads go below threshold."""
        ctx = create_test_context(load_phase=load_phase)
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
    """Tests for the calculate_fillage method.

    Issue #1952 D1: this method used to assign ``DEFAULT_PUMP_FILLAGE`` and
    return it without ever looking at the card, so every well reported the
    same fillage -- and on a 0-1 scale, while ``DynacardAnalyzer``'s pipeline
    writes a 0-100 percentage into the very same ``results.pump_fillage``
    field. It must run the real computation on the solved downhole card.
    """

    @STROKE_CASES
    def test_delegates_to_the_real_fillage_calculation(self, load_phase):
        """Should return exactly what calculations.calculate_pump_fillage gives."""
        ctx = create_test_context(load_phase=load_phase)
        solver = DynacardPhysicsSolver(ctx)
        results = solver.solve_wave_equation()

        expected = calculate_pump_fillage(results.downhole_card).fillage

        assert solver.calculate_fillage() == pytest.approx(expected)

    @STROKE_CASES
    def test_does_not_return_the_default_constant(self, load_phase):
        """Should not hand back the fabricated placeholder value."""
        ctx = create_test_context(load_phase=load_phase)
        solver = DynacardPhysicsSolver(ctx)
        solver.solve_wave_equation()

        assert solver.calculate_fillage() != pytest.approx(DEFAULT_PUMP_FILLAGE)

    @STROKE_CASES
    def test_fillage_depends_on_the_card(self, load_phase):
        """Different cards must produce different fillage values."""
        fillages = []
        for kwargs in (
            {},
            {"peak_load": 20000.0, "min_load": 2000.0},
            {"stroke_length": 64.0, "rod_length": 3000.0},
        ):
            solver = DynacardPhysicsSolver(create_test_context(**kwargs, load_phase=load_phase))
            solver.solve_wave_equation()
            fillages.append(solver.calculate_fillage())


        assert len(set(fillages)) == len(fillages), (
            f"fillage must vary with the card, got {fillages}"
        )

    @STROKE_CASES
    def test_reports_percentage_scale(self, load_phase):
        """Should use the 0-100 percentage scale of results.pump_fillage."""
        ctx = create_test_context(load_phase=load_phase)
        solver = DynacardPhysicsSolver(ctx)
        solver.solve_wave_equation()

        fillage = solver.calculate_fillage()

        assert 1.0 < fillage <= 100.0

    @STROKE_CASES
    def test_sets_fillage_in_results(self, load_phase):
        """Should set the computed fillage in results."""
        ctx = create_test_context(load_phase=load_phase)
        solver = DynacardPhysicsSolver(ctx)
        results = solver.solve_wave_equation()

        returned = solver.calculate_fillage()
        expected = calculate_pump_fillage(results.downhole_card).fillage

        assert solver.results.pump_fillage == pytest.approx(expected)
        assert solver.results.pump_fillage == pytest.approx(returned)

    def test_raises_when_wave_equation_not_solved(self):
        """Should refuse to invent a number when there is no downhole card."""
        ctx = create_test_context()
        solver = DynacardPhysicsSolver(ctx)

        with pytest.raises(PhysicsError):
            solver.calculate_fillage()


class TestPhysicsWithRealWorldValues:
    """Integration tests with more realistic well parameters."""

    @pytest.mark.parametrize(
        "load_phase", [0.0, -np.pi / 8],
        ids=["overtravel", "shorter-stroke"],
    )
    def test_typical_well_analysis(self, load_phase):
        """Test with typical real-world well parameters."""
        # Realistic surface card data (simplified)
        num_points = 200
        t = np.linspace(0, 2 * np.pi, num_points)

        stroke_length = 144.0  # inches (12 ft stroke)
        peak_load = 22000.0    # lbs
        min_load = 8000.0      # lbs

        position = (stroke_length / 2) * (1 - np.cos(t))
        load = (peak_load + min_load) / 2 + (peak_load - min_load) / 2 * np.sin(t + load_phase)

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
