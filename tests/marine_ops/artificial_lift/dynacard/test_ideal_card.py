# ABOUTME: Unit tests for ideal card generation module.
# ABOUTME: Validates ideal card generation, shape similarity, and comparison metrics.

import math
import pytest
import numpy as np
from pathlib import Path

from digitalmodel.marine_ops.artificial_lift.dynacard.data_loader import (
    load_from_json_file,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.ideal_card import (
    IdealCardCalculator,
    generate_ideal_card,
    calculate_shape_similarity,
    calculate_ideal_fluid_load,
    generate_rectangular_pump_card,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.models import (
    DynacardAnalysisContext,
    CardData,
    RodSection,
    PumpProperties,
    SurfaceUnit,
    IdealCardAnalysis,
)


# Test data directory
TEST_DATA_DIR = Path(__file__).parent / "testdata"


class TestIdealCardCalculator:
    """Tests for the IdealCardCalculator class."""

    def test_calculator_initialization(self):
        """Test calculator can be initialized with valid context."""
        context = self._create_test_context()
        calculator = IdealCardCalculator(context)
        assert calculator.ctx == context
        assert calculator.result is not None

    def test_generate_returns_analysis(self):
        """Test that generate returns IdealCardAnalysis."""
        context = self._create_test_context()
        calculator = IdealCardCalculator(context)
        result = calculator.generate()
        assert isinstance(result, IdealCardAnalysis)

    def test_ideal_pump_card_generated(self):
        """Test that ideal pump card is generated."""
        context = self._create_test_context()
        calculator = IdealCardCalculator(context)
        result = calculator.generate()

        assert len(result.ideal_pump_position) > 0
        assert len(result.ideal_pump_load) > 0
        assert len(result.ideal_pump_position) == len(result.ideal_pump_load)

    def test_ideal_surface_card_generated(self):
        """Test that ideal surface card is generated."""
        context = self._create_test_context()
        calculator = IdealCardCalculator(context)
        result = calculator.generate()

        assert len(result.ideal_surface_position) > 0
        assert len(result.ideal_surface_load) > 0
        assert len(result.ideal_surface_position) == len(result.ideal_surface_load)

    def test_positive_fluid_load(self):
        """Test that calculated fluid load is positive."""
        context = self._create_test_context()
        calculator = IdealCardCalculator(context)
        result = calculator.generate()

        assert result.ideal_fluid_load > 0

    def test_stroke_length_positive(self):
        """Test that stroke length is positive."""
        context = self._create_test_context()
        calculator = IdealCardCalculator(context)
        result = calculator.generate()

        assert result.ideal_stroke_length > 0

    def test_card_area_positive(self):
        """Test that card area is positive."""
        context = self._create_test_context()
        calculator = IdealCardCalculator(context)
        result = calculator.generate()

        assert result.ideal_card_area > 0

    def test_custom_fillage(self):
        """Test generation with custom fillage."""
        context = self._create_test_context()
        calculator = IdealCardCalculator(context)

        result_full = calculator.generate(fillage=1.0)
        result_half = IdealCardCalculator(context).generate(fillage=0.5)

        # Half fillage should have lower peak load
        assert result_half.ideal_peak_load < result_full.ideal_peak_load
        assert result_half.fillage_assumed == 0.5

    def test_custom_fluid_load(self):
        """Test generation with custom fluid load."""
        context = self._create_test_context()
        calculator = IdealCardCalculator(context)
        result = calculator.generate(fluid_load=5000.0)

        assert result.ideal_fluid_load == 5000.0

    def test_custom_num_points(self):
        """Test generation with custom number of points."""
        context = self._create_test_context()
        calculator = IdealCardCalculator(context)
        result = calculator.generate(num_points=200)

        assert result.num_time_points == 200
        # Card should have approximately num_points entries
        assert len(result.ideal_pump_position) > 150

    def test_peak_load_equals_fluid_load(self):
        """Test that pump card peak load equals fluid load."""
        context = self._create_test_context()
        calculator = IdealCardCalculator(context)
        result = calculator.generate(fillage=1.0)

        # For 100% fillage, peak pump load should equal fluid load
        assert abs(result.ideal_peak_load - result.ideal_fluid_load) < 1.0

    def test_min_load_is_zero(self):
        """Test that pump card min load is approximately zero."""
        context = self._create_test_context()
        calculator = IdealCardCalculator(context)
        result = calculator.generate()

        # Unloaded pump should have near-zero load
        assert result.ideal_min_load < 100  # Allow small tolerance

    def test_deviation_calculated(self):
        """Test that deviation from measured card is calculated."""
        context = self._create_test_context()
        calculator = IdealCardCalculator(context)
        result = calculator.generate()

        # Should have deviation metrics
        assert result.load_deviation_rms is not None
        assert result.load_deviation_rms >= 0
        assert result.stroke_length_difference is not None
        assert result.stroke_length_difference >= 0

    def test_stroke_length_difference_is_a_difference_not_an_rms(self):
        """
        Regression: the field formerly called ``position_deviation_rms`` was
        never an RMS - it is |measured stroke - ideal stroke|. It is now named
        for what it computes, and no RMS-named position field survives.
        """
        context = self._create_test_context()
        context.surface_unit.stroke_length = 90.0
        result = IdealCardCalculator(context).generate()

        measured_pos = context.surface_card.position
        expected = abs((max(measured_pos) - min(measured_pos)) - 90.0)
        assert result.stroke_length_difference == pytest.approx(expected)
        assert not hasattr(result, "position_deviation_rms")

    def test_shape_similarity_in_valid_range(self):
        """Test that shape similarity is in valid range."""
        context = self._create_test_context()
        calculator = IdealCardCalculator(context)
        result = calculator.generate()

        assert result.shape_similarity is not None
        assert 0 < result.shape_similarity <= 1.0

    def test_deviation_against_own_ideal_card_is_zero(self):
        """
        Regression for the scrambled-reference bug.

        Deviation used to be measured with ``np.interp(..., period=stroke)``
        against the ideal card's NON-MONOTONIC position array. numpy sorts the
        abscissa when ``period`` is given, collapsing the closed loop into a
        single-valued function, so even a measured card that IS the ideal card
        reported a large deviation. Feeding the ideal surface card back in as
        the measured card must now produce ~zero deviation and a perfect
        shape match.
        """
        context = self._create_test_context()
        ideal = IdealCardCalculator(context).generate()

        context.surface_card = CardData(
            position=list(ideal.ideal_surface_position),
            load=list(ideal.ideal_surface_load),
        )
        result = IdealCardCalculator(context).generate()

        load_scale = ideal.ideal_surface_peak_load - ideal.ideal_surface_min_load
        assert result.load_deviation_rms is not None
        assert result.load_deviation_rms < 0.01 * load_scale
        assert result.shape_similarity == pytest.approx(1.0, abs=1e-9)

    def test_surface_domain_scalars_exposed(self):
        """
        Surface-domain scalars must exist and must not be confused with the
        pump-domain ones: the pump card carries fluid load only, the surface
        card also carries the buoyant rod weight.
        """
        context = self._create_test_context()
        result = IdealCardCalculator(context).generate()

        # Pump domain: fluid load only, unloaded on the downstroke.
        assert result.ideal_peak_load == pytest.approx(result.ideal_fluid_load, abs=1.0)
        assert result.ideal_min_load == pytest.approx(0.0, abs=1e-9)

        # Surface domain: rod weight is carried on both strokes.
        assert result.ideal_surface_min_load > 0
        assert result.ideal_surface_peak_load > result.ideal_peak_load
        assert result.ideal_surface_peak_load == pytest.approx(
            result.ideal_surface_min_load + result.ideal_fluid_load, abs=1.0
        )
        assert result.ideal_surface_card_area > 0

    def test_generation_method_assigned_by_calculator(self):
        """generation_method must be provenance, not an untouched model default."""
        context = self._create_test_context()
        calculator = IdealCardCalculator(context)
        calculator.result.generation_method = "sentinel"
        result = calculator.generate()

        assert result.generation_method == "simplified"

    def test_no_unassigned_damping_provenance_field(self):
        """
        ``damping_coefficient`` was a model default (0.1) that the calculator
        never set and no damping model ever used. It must not exist.
        """
        result = IdealCardCalculator(self._create_test_context()).generate()
        assert not hasattr(result, "damping_coefficient")

    def test_empty_surface_card_handles_gracefully(self):
        """Test handling when no measured card for comparison."""
        context = self._create_test_context()
        context.surface_card = CardData(position=[], load=[])
        calculator = IdealCardCalculator(context)
        result = calculator.generate()

        # Should still generate ideal card
        assert len(result.ideal_pump_position) > 0
        assert result.warning_message != ""

    def _create_test_context(self) -> DynacardAnalysisContext:
        """Create a test context with valid data."""
        return DynacardAnalysisContext(
            api14="TEST-IDEAL-001",
            surface_card=CardData(
                position=[0, 25, 50, 75, 100, 75, 50, 25],
                load=[5000, 7000, 10000, 12000, 10000, 7000, 5000, 4000],
            ),
            rod_string=[
                RodSection(diameter=0.875, length=2500.0),
                RodSection(diameter=0.75, length=2500.0),
            ],
            pump=PumpProperties(diameter=1.75, depth=5000.0),
            surface_unit=SurfaceUnit(stroke_length=100.0),
            spm=6.0,
            runtime=24.0,
            fluid_density=55.0,
        )


class TestGenerateIdealCard:
    """Tests for generate_ideal_card convenience function."""

    def test_returns_ideal_card_analysis(self):
        """Test that convenience function returns IdealCardAnalysis."""
        context = self._create_test_context()
        result = generate_ideal_card(context)
        assert isinstance(result, IdealCardAnalysis)

    def test_with_fillage(self):
        """Test convenience function with fillage parameter."""
        context = self._create_test_context()
        result = generate_ideal_card(context, fillage=0.8)
        assert result.fillage_assumed == 0.8

    def test_with_fluid_load(self):
        """Test convenience function with fluid load parameter."""
        context = self._create_test_context()
        result = generate_ideal_card(context, fluid_load=6000.0)
        assert result.ideal_fluid_load == 6000.0

    def _create_test_context(self) -> DynacardAnalysisContext:
        """Create a test context."""
        return DynacardAnalysisContext(
            api14="TEST-IDEAL-002",
            surface_card=CardData(
                position=[0, 50, 100, 50],
                load=[5000, 8000, 12000, 7000],
            ),
            rod_string=[RodSection(diameter=0.875, length=5000.0)],
            pump=PumpProperties(diameter=1.75, depth=5000.0),
            surface_unit=SurfaceUnit(stroke_length=100.0),
            spm=6.0,
            runtime=24.0,
        )


class TestCalculateShapeSimilarity:
    """Tests for calculate_shape_similarity function."""

    def test_identical_cards_similarity_1(self):
        """Test that identical cards have similarity near 1."""
        card = CardData(
            position=[0, 50, 100, 50],
            load=[5000, 8000, 12000, 7000],
        )
        similarity = calculate_shape_similarity(card, card)
        assert similarity > 0.99

    def test_different_cards_lower_similarity(self):
        """Test that different cards have lower similarity."""
        card1 = CardData(
            position=[0, 50, 100, 50],
            load=[5000, 8000, 12000, 7000],
        )
        card2 = CardData(
            position=[0, 50, 100, 50],
            load=[10000, 5000, 8000, 12000],  # Different pattern
        )
        similarity = calculate_shape_similarity(card1, card2)
        assert similarity < 0.9

    def test_empty_cards_return_none(self):
        """
        Empty cards are not comparable. Unavailable must be None, never a
        plausible-looking 0.0 (which used to be indistinguishable from a real
        'completely dissimilar' score).
        """
        card1 = CardData(position=[], load=[])
        card2 = CardData(position=[], load=[])
        assert calculate_shape_similarity(card1, card2) is None

    def test_degenerate_card_returns_none(self):
        """A card with too few samples cannot be split into branches."""
        card1 = CardData(
            position=[0, 50, 100, 50],
            load=[5000, 8000, 12000, 7000],
        )
        degenerate = CardData(position=[0, 50, 100], load=[1, 2, 3])
        assert calculate_shape_similarity(card1, degenerate) is None

    def test_flat_load_card_returns_none(self):
        """Zero load range gives no shape to compare."""
        card1 = CardData(
            position=[0, 50, 100, 50],
            load=[5000, 8000, 12000, 7000],
        )
        flat = CardData(position=[0, 50, 100, 50], load=[1000, 1000, 1000, 1000])
        assert calculate_shape_similarity(card1, flat) is None

    def test_different_length_cards(self):
        """Test similarity with different length cards."""
        card1 = CardData(
            position=[0, 25, 50, 75, 100, 75, 50, 25],
            load=[5000, 6000, 8000, 10000, 12000, 10000, 8000, 6000],
        )
        card2 = CardData(
            position=[0, 50, 100, 50],
            load=[5000, 9000, 12000, 8000],
        )
        similarity = calculate_shape_similarity(card1, card2)
        # Should handle different lengths
        assert similarity is not None
        assert 0 < similarity <= 1.0

    def test_similarity_range(self):
        """Test that similarity is always in valid range."""
        card1 = CardData(
            position=[0, 50, 100, 50],
            load=[5000, 8000, 12000, 7000],
        )
        card2 = CardData(
            position=[0, 50, 100, 50],
            load=[1000, 2000, 3000, 1500],
        )
        similarity = calculate_shape_similarity(card1, card2)
        assert similarity is not None
        assert 0 < similarity <= 1.0

    def test_inverted_card_does_not_score_like_an_unrelated_card(self):
        """
        THE regression test for the clamped correlation.

        ``max(0.0, r)`` mapped a perfectly inverted card (r = -1, a real
        diagnostic signal) and an unrelated card (r ~ 0) to the SAME score of
        0.0. A load-inverted card is roughly twice as far from the reference as
        an unrelated one, so it must score strictly lower - and neither may be
        confused with the 'not computable' answer.
        """
        base = CardData(
            position=[0, 25, 50, 75, 100, 75, 50, 25],
            load=[5000, 7000, 10000, 12000, 10000, 7000, 5000, 4000],
        )
        mean_load = sum(base.load) / len(base.load)
        inverted = CardData(
            position=list(base.position),
            load=[2 * mean_load - v for v in base.load],
        )
        unrelated = CardData(
            position=list(base.position),
            load=[9000, 4000, 11000, 5000, 12000, 6000, 10000, 8000],
        )

        sim_self = calculate_shape_similarity(base, base)
        sim_inverted = calculate_shape_similarity(base, inverted)
        sim_unrelated = calculate_shape_similarity(base, unrelated)

        assert sim_self == pytest.approx(1.0)
        assert sim_inverted is not None and sim_unrelated is not None
        # The defect: these two were both exactly 0.0.
        assert sim_inverted != pytest.approx(sim_unrelated, abs=1e-3)
        assert sim_inverted < sim_unrelated < sim_self

    def test_similarity_is_position_aware_not_index_aligned(self):
        """
        The old metric correlated load arrays by index and used no position at
        all, so re-indexing the SAME closed loop changed the score. Rotating
        the starting sample of a card describes the identical loop and must
        score 1.0.
        """
        base = CardData(
            position=[0, 25, 50, 75, 100, 75, 50, 25],
            load=[5000, 7000, 10000, 12000, 10000, 7000, 5000, 4000],
        )
        rotated = CardData(
            position=base.position[3:] + base.position[:3],
            load=base.load[3:] + base.load[:3],
        )

        assert calculate_shape_similarity(base, rotated) == pytest.approx(1.0)

    def test_upstroke_downstroke_swap_is_penalised(self):
        """
        Traversing the same geometry backwards swaps which branch is the
        upstroke - physically a different card - so it must score below 1.0
        while still being computable.
        """
        base = CardData(
            position=[0, 25, 50, 75, 100, 75, 50, 25],
            load=[5000, 7000, 10000, 12000, 10000, 7000, 5000, 4000],
        )
        reversed_card = CardData(
            position=list(reversed(base.position)),
            load=list(reversed(base.load)),
        )

        similarity = calculate_shape_similarity(base, reversed_card)
        assert similarity is not None
        assert 0 < similarity < 1.0


class TestCalculateIdealFluidLoad:
    """Tests for calculate_ideal_fluid_load function."""

    def test_positive_fluid_load(self):
        """Test that fluid load is positive."""
        fluid_load = calculate_ideal_fluid_load(
            pump_diameter=1.75,
            pump_depth=5000.0,
            fluid_density=55.0,
        )
        assert fluid_load > 0

    def test_larger_pump_higher_load(self):
        """Test that larger pump diameter gives higher load."""
        load_small = calculate_ideal_fluid_load(pump_diameter=1.5, pump_depth=5000.0)
        load_large = calculate_ideal_fluid_load(pump_diameter=2.0, pump_depth=5000.0)
        assert load_large > load_small

    def test_deeper_pump_higher_load(self):
        """Test that deeper pump gives higher load."""
        load_shallow = calculate_ideal_fluid_load(pump_diameter=1.75, pump_depth=3000.0)
        load_deep = calculate_ideal_fluid_load(pump_diameter=1.75, pump_depth=7000.0)
        assert load_deep > load_shallow

    def test_heavier_fluid_higher_load(self):
        """Test that heavier fluid gives higher load."""
        load_light = calculate_ideal_fluid_load(
            pump_diameter=1.75, pump_depth=5000.0, fluid_density=50.0
        )
        load_heavy = calculate_ideal_fluid_load(
            pump_diameter=1.75, pump_depth=5000.0, fluid_density=70.0
        )
        assert load_heavy > load_light

    def test_known_values(self):
        """Test with known values for verification."""
        # 1.75" pump at 5000ft with 62.4 lb/ft3 water
        # Area = pi * (0.875)^2 = 2.405 in^2
        # Pressure = (62.4/144) * 5000 = 2166.7 psi
        # Load = 2.405 * 2166.7 = 5211 lbs (approximately)
        fluid_load = calculate_ideal_fluid_load(
            pump_diameter=1.75,
            pump_depth=5000.0,
            fluid_density=62.4,
        )
        assert 5000 < fluid_load < 5500  # Allow some tolerance


class TestGenerateRectangularPumpCard:
    """Tests for generate_rectangular_pump_card function."""

    def test_returns_arrays(self):
        """Test that function returns position and load arrays."""
        positions, loads = generate_rectangular_pump_card(
            stroke_length=100.0,
            fluid_load=5000.0,
        )
        assert isinstance(positions, np.ndarray)
        assert isinstance(loads, np.ndarray)

    def test_correct_length(self):
        """Test that arrays have expected length."""
        positions, loads = generate_rectangular_pump_card(
            stroke_length=100.0,
            fluid_load=5000.0,
            num_points=100,
        )
        assert len(positions) == 100
        assert len(loads) == 100

    def test_position_range(self):
        """Test that positions span stroke length."""
        stroke = 120.0
        positions, loads = generate_rectangular_pump_card(
            stroke_length=stroke,
            fluid_load=5000.0,
        )
        assert np.min(positions) == 0.0
        assert np.max(positions) == stroke

    def test_load_levels(self):
        """Test that loads have correct levels."""
        fluid_load = 6000.0
        positions, loads = generate_rectangular_pump_card(
            stroke_length=100.0,
            fluid_load=fluid_load,
        )
        # Should have two load levels: 0 and fluid_load
        unique_loads = np.unique(loads)
        assert len(unique_loads) == 2
        assert 0.0 in unique_loads
        assert fluid_load in unique_loads


class TestIdealCardAnalysisModel:
    """Tests for IdealCardAnalysis model."""

    def test_analysis_defaults(self):
        """Test IdealCardAnalysis default values."""
        analysis = IdealCardAnalysis()
        assert analysis.ideal_surface_position == []
        assert analysis.ideal_surface_load == []
        assert analysis.ideal_pump_position == []
        assert analysis.ideal_pump_load == []
        assert analysis.ideal_peak_load == 0.0
        assert analysis.fillage_assumed == 1.0
        assert analysis.generation_method == "simplified"

    def test_analysis_with_values(self):
        """Test IdealCardAnalysis with actual values."""
        analysis = IdealCardAnalysis(
            ideal_pump_position=[0, 50, 100, 50],
            ideal_pump_load=[5000, 5000, 0, 0],
            ideal_peak_load=5000.0,
            ideal_min_load=0.0,
            ideal_stroke_length=100.0,
            ideal_fluid_load=5000.0,
            fillage_assumed=1.0,
        )
        assert analysis.ideal_peak_load == 5000.0
        assert analysis.ideal_fluid_load == 5000.0

    def test_analysis_serialization(self):
        """Test that IdealCardAnalysis can be serialized."""
        analysis = IdealCardAnalysis(
            ideal_pump_position=[0, 50, 100, 50],
            ideal_pump_load=[5000, 5000, 0, 0],
            ideal_peak_load=5000.0,
        )

        data = analysis.model_dump()
        assert data["ideal_peak_load"] == 5000.0
        assert data["ideal_pump_position"] == [0, 50, 100, 50]


class TestIdealCardWithRealData:
    """Tests using real well card data."""

    @pytest.fixture
    def well_7699227(self):
        """Load well 7699227 test data."""
        filepath = TEST_DATA_DIR / "7699227.json"
        if filepath.exists():
            return load_from_json_file(filepath)
        pytest.skip("Test data file 7699227.json not found")

    def test_ideal_card_generation_7699227(self, well_7699227):
        """Test ideal card generation with well 7699227 data."""
        result = generate_ideal_card(well_7699227)

        # Should complete generation
        assert result.generation_method == "simplified"
        assert len(result.ideal_pump_position) > 0

    def test_fluid_load_positive_7699227(self, well_7699227):
        """Test that fluid load is positive with real data."""
        result = generate_ideal_card(well_7699227)

        assert result.ideal_fluid_load > 0

    def test_shape_similarity_calculated_7699227(self, well_7699227):
        """Test shape similarity is calculated with real data."""
        result = generate_ideal_card(well_7699227)

        # Should have valid similarity
        assert result.shape_similarity is not None
        assert 0 < result.shape_similarity <= 1.0

    def test_surface_scalar_is_the_comparable_one_7699227(self, well_7699227):
        """
        Domain trap guard: the measured PPRL belongs to the SURFACE domain.
        Comparing it against the pump-domain ``ideal_peak_load`` invites a
        bogus multiple (~4x here); the surface-domain scalar is the right
        reference and lands in the same ballpark as the measurement.
        """
        result = generate_ideal_card(well_7699227)
        measured_pprl = max(well_7699227.surface_card.load)

        assert result.ideal_surface_peak_load > result.ideal_peak_load
        assert 0.5 < result.ideal_surface_peak_load / measured_pprl < 2.0

    def test_card_area_calculated_7699227(self, well_7699227):
        """Test card area is calculated with real data."""
        result = generate_ideal_card(well_7699227)

        assert result.ideal_card_area > 0
