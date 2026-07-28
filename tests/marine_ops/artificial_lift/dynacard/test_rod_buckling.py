# ABOUTME: Unit tests for rod buckling analysis module.
# ABOUTME: Validates buckling detection, neutral point, and critical load calculations.

import math
import pytest
import numpy as np
from pathlib import Path

from digitalmodel.marine_ops.artificial_lift.dynacard.data_loader import (
    load_from_json_file,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.rod_buckling import (
    RodBucklingCalculator,
    calculate_rod_buckling,
    estimate_neutral_point,
    calculate_critical_buckling_load,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.models import (
    DynacardAnalysisContext,
    CardData,
    RodSection,
    PumpProperties,
    SurfaceUnit,
    RodBucklingAnalysis,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.exceptions import ValidationError


# Test data directory
TEST_DATA_DIR = Path(__file__).parent / "testdata"

# Wellbore inputs the Paslay-Dawson critical-load formula requires. They are
# properties of the hole, not of the card, so they must be supplied explicitly.
DEVIATED_INCLINATION_DEG = 30.0
TUBING_ID_IN = 2.441  # 2-7/8 in tubing


class TestRodBucklingCalculator:
    """Tests for the RodBucklingCalculator class."""

    def test_calculator_initialization(self):
        """Test calculator can be initialized with valid context."""
        context = self._create_test_context()
        calculator = RodBucklingCalculator(context)
        assert calculator.ctx == context
        assert calculator.result is not None

    def test_calculate_returns_analysis(self):
        """Test that calculate returns RodBucklingAnalysis."""
        context = self._create_test_context()
        calculator = RodBucklingCalculator(context)
        result = calculator.calculate()
        assert isinstance(result, RodBucklingAnalysis)

    def test_critical_loads_calculated(self):
        """Test that critical buckling loads are calculated when inputs allow."""
        context = self._create_test_context()
        calculator = RodBucklingCalculator(context)
        result = calculator.calculate(
            inclination_deg=DEVIATED_INCLINATION_DEG,
            tubing_id=TUBING_ID_IN,
        )

        # Should have positive critical loads
        assert result.sinusoidal_critical_load > 0
        assert result.helical_critical_load > 0

    def test_critical_loads_none_without_inclination_and_clearance(self):
        """Paslay-Dawson needs inclination + radial clearance; else report None.

        Regression test for the old ``1.94*sqrt(E*I*W_b)`` expression, which is
        dimensionally lb*in^0.5 - not a force - and was reported unconditionally.
        """
        context = self._create_test_context()
        result = RodBucklingCalculator(context).calculate()

        assert result.sinusoidal_critical_load is None
        assert result.helical_critical_load is None
        assert "inclination" in result.warning_message.lower()

        # Missing either one alone is still not enough
        only_inc = RodBucklingCalculator(context).calculate(inclination_deg=30.0)
        assert only_inc.sinusoidal_critical_load is None
        only_id = RodBucklingCalculator(context).calculate(tubing_id=TUBING_ID_IN)
        assert only_id.sinusoidal_critical_load is None

    def test_critical_load_is_a_force_in_pounds(self):
        """Critical load must scale like sqrt(E*I*w*sin(alpha)/r_c) in pounds."""
        context = self._create_test_context()
        result = RodBucklingCalculator(context).calculate(
            inclination_deg=DEVIATED_INCLINATION_DEG,
            tubing_id=TUBING_ID_IN,
        )

        # Reproduce the formula independently for the weakest (0.625 in) section
        d = 0.625
        section = next(s for s in context.rod_string if s.diameter == d)
        I = math.pi * d ** 4 / 64.0
        buoyancy = 1.0 - context.fluid_density / 490.0
        w = section.weight_per_foot * buoyancy / 12.0
        r_c = (TUBING_ID_IN - d) / 2.0
        expected = 2.0 * math.sqrt(
            section.modulus_of_elasticity
            * I
            * w
            * math.sin(math.radians(DEVIATED_INCLINATION_DEG))
            / r_c
        )
        assert abs(result.sinusoidal_critical_load - expected) < 1e-6 * expected

        # Doubling sin(alpha) must raise the threshold by sqrt(2), which the old
        # inclination-free formula could not do at all.
        shallow = RodBucklingCalculator(context).calculate(
            inclination_deg=math.degrees(math.asin(0.25)),
            tubing_id=TUBING_ID_IN,
        )
        steeper = RodBucklingCalculator(context).calculate(
            inclination_deg=math.degrees(math.asin(0.50)),
            tubing_id=TUBING_ID_IN,
        )
        ratio = steeper.sinusoidal_critical_load / shallow.sinusoidal_critical_load
        assert abs(ratio - math.sqrt(2.0)) < 1e-6

    def test_vertical_hole_reports_no_critical_load(self):
        """Paslay-Dawson degenerates at zero inclination - must not fake it."""
        context = self._create_test_context()
        result = RodBucklingCalculator(context).calculate(
            inclination_deg=0.0,
            tubing_id=TUBING_ID_IN,
        )
        assert result.sinusoidal_critical_load is None
        assert result.helical_critical_load is None
        assert result.sinusoidal_buckling_detected is None

    def test_helical_greater_than_sinusoidal(self):
        """Test that helical critical load > sinusoidal critical load."""
        context = self._create_test_context()
        calculator = RodBucklingCalculator(context)
        result = calculator.calculate(
            inclination_deg=DEVIATED_INCLINATION_DEG,
            tubing_id=TUBING_ID_IN,
        )

        # Helical buckling requires higher load than sinusoidal
        assert result.helical_critical_load > result.sinusoidal_critical_load

    def test_helical_approximately_283_times_sinusoidal(self):
        """Test that helical ≈ 2.83 × sinusoidal (2*sqrt(2), Chen-Lin-Cheatham)."""
        context = self._create_test_context()
        calculator = RodBucklingCalculator(context)
        result = calculator.calculate(
            inclination_deg=DEVIATED_INCLINATION_DEG,
            tubing_id=TUBING_ID_IN,
        )

        ratio = result.helical_critical_load / result.sinusoidal_critical_load
        assert abs(ratio - 2.83) < 0.01
        assert abs(ratio - 2.0 * math.sqrt(2.0)) < 1e-9

    def test_neutral_point_depth_not_derivable_from_a_card(self):
        """No neutral-point DEPTH may be invented from a card array.

        A card is indexed by stroke phase at one depth, so mapping a sample
        index onto a depth is a category error. These fields must be None.
        """
        context = self._create_test_context()
        result = RodBucklingCalculator(context).calculate()

        assert result.neutral_point_depth is None
        assert result.neutral_point_fraction is None
        assert result.compression_depth_start is None
        assert result.compression_length is None

        # Supplying a real downhole card does not change that
        downhole_card = CardData(
            position=[0, 50, 100, 50],
            load=[-1000, 2000, 5000, 2000],
        )
        result = RodBucklingCalculator(context).calculate(downhole_card=downhole_card)
        assert result.neutral_point_depth is None
        assert result.compression_length is None

    def test_analysis_method_records_what_actually_ran(self):
        """analysis_method must be assigned, not left at a default string."""
        context = self._create_test_context()

        estimated = RodBucklingCalculator(context).calculate()
        assert estimated.analysis_method == "surface_card_estimate"

        downhole_card = CardData(
            position=[0, 50, 100, 50],
            load=[1000, 2000, 3000, 2000],
        )
        measured = RodBucklingCalculator(context).calculate(downhole_card=downhole_card)
        assert measured.analysis_method == "downhole_card"

    def test_no_buckling_with_tensile_loads(self):
        """Test no buckling detected with all tensile loads."""
        context = self._create_test_context()
        # Create surface card with all positive (tensile) loads
        context.surface_card = CardData(
            position=[0, 50, 100, 50],
            load=[8000, 10000, 12000, 10000],  # All high tensile
        )
        calculator = RodBucklingCalculator(context)
        result = calculator.calculate(
            inclination_deg=DEVIATED_INCLINATION_DEG,
            tubing_id=TUBING_ID_IN,
        )

        # With high tensile loads, no buckling expected
        assert result.analysis_method == "surface_card_estimate"
        assert result.max_compressive_load == 0.0
        assert result.sinusoidal_buckling_detected is False
        assert result.helical_buckling_detected is False

    def test_buckling_with_compressive_loads(self):
        """Test buckling detection with compressive downhole loads."""
        context = self._create_test_context()
        # Create downhole card with compression
        downhole_card = CardData(
            position=[0, 50, 100, 50],
            load=[-1000, 2000, 5000, 2000],  # Negative = compression
        )
        calculator = RodBucklingCalculator(context)
        result = calculator.calculate(
            downhole_card=downhole_card,
            inclination_deg=DEVIATED_INCLINATION_DEG,
            tubing_id=TUBING_ID_IN,
        )

        # Should detect buckling with significant compression
        assert result.max_compressive_load > 0
        assert result.sinusoidal_buckling_detected or result.max_compressive_load > 0

    def test_buckling_verdict_is_none_without_thresholds(self):
        """No threshold means no verdict - never a guess, never False."""
        context = self._create_test_context()
        downhole_card = CardData(
            position=[0, 50, 100, 50],
            load=[-5000, 1000, 3000, 1000],  # Strong compression
        )
        result = RodBucklingCalculator(context).calculate(downhole_card=downhole_card)

        assert result.sinusoidal_buckling_detected is None
        assert result.helical_buckling_detected is None
        # The load magnitude itself is still reported
        assert result.max_compressive_load > 0

    def test_buckling_verdict_follows_threshold_not_raw_load_rule(self):
        """A compression below the critical threshold must NOT trip buckling.

        Regression test for the hard-coded ``min_load < -500`` rule, which
        tested RAW loads while the threshold branch tested buckling TENDENCY,
        so a tendency safely under the critical load still returned True.
        """
        context = self._create_test_context()
        # Raw loads far below -500 lb, but the effective axial load (tendency)
        # after the buoyancy/pressure correction stays modest.
        downhole_card = CardData(
            position=[0, 50, 100, 50],
            load=[-700, 1000, 3000, 1000],
        )
        result = RodBucklingCalculator(context).calculate(
            downhole_card=downhole_card,
            inclination_deg=DEVIATED_INCLINATION_DEG,
            tubing_id=TUBING_ID_IN,
        )

        assert min(downhole_card.load) < -500  # the old rule would have tripped
        assert result.max_compressive_load < result.sinusoidal_critical_load
        assert result.sinusoidal_buckling_detected is False
        assert result.helical_buckling_detected is False

    def test_buckling_trips_when_threshold_is_exceeded(self):
        """Above the critical threshold the verdict must be True."""
        context = self._create_test_context()
        result_ref = RodBucklingCalculator(context).calculate(
            inclination_deg=DEVIATED_INCLINATION_DEG,
            tubing_id=TUBING_ID_IN,
        )
        critical = result_ref.sinusoidal_critical_load

        # Push the compressive tendency well past the sinusoidal threshold
        downhole_card = CardData(
            position=[0, 50, 100, 50],
            load=[-(critical * 10.0), 1000, 3000, 1000],
        )
        result = RodBucklingCalculator(context).calculate(
            downhole_card=downhole_card,
            inclination_deg=DEVIATED_INCLINATION_DEG,
            tubing_id=TUBING_ID_IN,
        )

        assert result.max_compressive_load > critical
        assert result.sinusoidal_buckling_detected is True

    def test_buckling_tendency_calculated(self):
        """Test that buckling tendency min/max are calculated."""
        context = self._create_test_context()
        calculator = RodBucklingCalculator(context)
        result = calculator.calculate()

        # Buckling tendency should be calculated
        assert result.min_buckling_tendency != 0 or result.max_buckling_tendency != 0

    def test_empty_surface_card_raises_validation_error(self):
        """Test that empty surface card raises ValidationError."""
        context = self._create_test_context()
        context.surface_card = CardData(position=[], load=[])
        calculator = RodBucklingCalculator(context)

        with pytest.raises(ValidationError) as exc_info:
            calculator.calculate()

        assert "load" in exc_info.value.message.lower() or "data" in exc_info.value.message.lower()

    def test_zero_rod_length_raises_validation_error(self):
        """Test that zero rod length raises ValidationError."""
        context = self._create_test_context()
        context.rod_string = []  # No rod sections
        calculator = RodBucklingCalculator(context)

        with pytest.raises(ValidationError) as exc_info:
            calculator.calculate()

        assert "rod" in exc_info.value.message.lower() or "length" in exc_info.value.message.lower()

    def test_convenience_function_handles_errors_gracefully(self):
        """Test that convenience function returns result with warning on error."""
        context = self._create_test_context()
        context.surface_card = CardData(position=[], load=[])

        # Use convenience function with raise_on_error=False (default)
        result = calculate_rod_buckling(context, raise_on_error=False)

        # Should return result with warning message set
        assert result.warning_message != ""

    def test_with_custom_downhole_card(self):
        """Test calculation with provided downhole card."""
        context = self._create_test_context()
        custom_card = CardData(
            position=[0, 25, 50, 75, 100, 75, 50, 25],
            load=[2000, 3000, 4000, 5000, 4000, 3000, 2000, 1000],
        )
        calculator = RodBucklingCalculator(context)
        result = calculator.calculate(downhole_card=custom_card)

        # Should complete analysis, recording that a downhole card was used
        assert result.analysis_method == "downhole_card"

    def _create_test_context(self) -> DynacardAnalysisContext:
        """Create a test context with valid data."""
        return DynacardAnalysisContext(
            api14="TEST-BUCK-001",
            surface_card=CardData(
                position=[0, 50, 100, 50],
                load=[5000, 8000, 12000, 7000],
            ),
            rod_string=[
                RodSection(diameter=0.875, length=2000.0),
                RodSection(diameter=0.75, length=2000.0),
                RodSection(diameter=0.625, length=1000.0),
            ],
            pump=PumpProperties(diameter=1.75, depth=5000.0),
            surface_unit=SurfaceUnit(),
            spm=6.0,
            runtime=24.0,
            fluid_density=55.0,
        )


class TestConvenienceFunction:
    """Tests for calculate_rod_buckling convenience function."""

    def test_returns_rod_buckling_analysis(self):
        """Test that convenience function returns RodBucklingAnalysis."""
        context = self._create_test_context()
        result = calculate_rod_buckling(context)
        assert isinstance(result, RodBucklingAnalysis)

    def test_with_downhole_card(self):
        """Test convenience function with downhole card."""
        context = self._create_test_context()
        downhole_card = CardData(
            position=[0, 50, 100, 50],
            load=[1000, 2000, 3000, 2000],
        )
        result = calculate_rod_buckling(context, downhole_card=downhole_card)
        assert isinstance(result, RodBucklingAnalysis)

    def _create_test_context(self) -> DynacardAnalysisContext:
        """Create a test context."""
        return DynacardAnalysisContext(
            api14="TEST-BUCK-002",
            surface_card=CardData(
                position=[0, 50, 100, 50],
                load=[5000, 8000, 12000, 7000],
            ),
            rod_string=[RodSection(diameter=0.875, length=5000.0)],
            pump=PumpProperties(diameter=1.75, depth=5000.0),
            surface_unit=SurfaceUnit(),
            spm=6.0,
            runtime=24.0,
        )


class TestEstimateNeutralPoint:
    """Tests for estimate_neutral_point function."""

    def test_basic_calculation(self):
        """Test basic neutral point estimation."""
        neutral = estimate_neutral_point(
            surface_load_max=12000,
            surface_load_min=5000,
            rod_weight=4000,
            fluid_density=55.0,
            rod_length=5000.0,
        )
        assert 0 <= neutral <= 5000

    def test_high_fluid_load_gives_deeper_neutral(self):
        """Test that higher fluid load gives deeper neutral point."""
        neutral_low = estimate_neutral_point(
            surface_load_max=10000,
            surface_load_min=8000,
            rod_weight=4000,
            rod_length=5000.0,
        )
        neutral_high = estimate_neutral_point(
            surface_load_max=15000,
            surface_load_min=5000,
            rod_weight=4000,
            rod_length=5000.0,
        )
        # Higher fluid load should give deeper neutral point
        assert neutral_high >= neutral_low

    def test_zero_rod_weight_returns_full_length(self):
        """Test that zero rod weight returns full rod length."""
        neutral = estimate_neutral_point(
            surface_load_max=10000,
            surface_load_min=5000,
            rod_weight=0,
            rod_length=5000.0,
        )
        # With no rod weight, neutral point is at bottom
        assert neutral == 5000.0

    def test_heavy_rod_gives_shallower_neutral(self):
        """Test that heavier rod gives shallower neutral point."""
        neutral_light = estimate_neutral_point(
            surface_load_max=12000,
            surface_load_min=5000,
            rod_weight=2000,
            rod_length=5000.0,
        )
        neutral_heavy = estimate_neutral_point(
            surface_load_max=12000,
            surface_load_min=5000,
            rod_weight=6000,
            rod_length=5000.0,
        )
        # Heavier rod should have shallower neutral point
        assert neutral_heavy <= neutral_light

    def test_returns_within_bounds(self):
        """Test neutral point is always within rod length."""
        for load_max in [8000, 12000, 16000]:
            for load_min in [3000, 5000, 7000]:
                neutral = estimate_neutral_point(
                    surface_load_max=load_max,
                    surface_load_min=load_min,
                    rod_weight=4000,
                    rod_length=5000.0,
                )
                assert 0 <= neutral <= 5000.0


class TestCalculateCriticalBucklingLoad:
    """Tests for calculate_critical_buckling_load function."""

    def test_returns_tuple(self):
        """Test that function returns tuple of two values."""
        result = calculate_critical_buckling_load(rod_diameter=0.875)
        assert isinstance(result, tuple)
        assert len(result) == 2

    def test_returns_none_without_wellbore_inputs(self):
        """Without inclination + tubing ID the thresholds are undefined."""
        assert calculate_critical_buckling_load(rod_diameter=0.875) == (None, None)
        assert calculate_critical_buckling_load(
            rod_diameter=0.875, inclination_deg=30.0
        ) == (None, None)
        assert calculate_critical_buckling_load(
            rod_diameter=0.875, tubing_id=TUBING_ID_IN
        ) == (None, None)
        # Degenerate geometry is also refused rather than approximated
        assert calculate_critical_buckling_load(
            rod_diameter=0.875, inclination_deg=0.0, tubing_id=TUBING_ID_IN
        ) == (None, None)
        assert calculate_critical_buckling_load(
            rod_diameter=2.5, inclination_deg=30.0, tubing_id=TUBING_ID_IN
        ) == (None, None)

    def test_positive_critical_loads(self):
        """Test that critical loads are positive."""
        sinusoidal, helical = self._critical(rod_diameter=0.875)
        assert sinusoidal > 0
        assert helical > 0

    def test_helical_greater_than_sinusoidal(self):
        """Test that helical > sinusoidal critical load."""
        sinusoidal, helical = self._critical(rod_diameter=0.875)
        assert helical > sinusoidal
        assert abs(helical / sinusoidal - 2.0 * math.sqrt(2.0)) < 1e-9

    def test_larger_diameter_higher_critical_load(self):
        """Test that larger diameter gives higher critical load."""
        small_sin, small_hel = self._critical(rod_diameter=0.625)
        large_sin, large_hel = self._critical(rod_diameter=1.0)

        # Larger diameter should have higher critical loads
        assert large_sin > small_sin
        assert large_hel > small_hel

    def test_with_custom_modulus(self):
        """Test with custom modulus of elasticity."""
        default_sin, _ = self._critical(rod_diameter=0.875)
        high_sin, _ = self._critical(
            rod_diameter=0.875,
            modulus=35000000.0,  # Higher modulus
        )

        # Higher modulus should give higher critical load
        assert high_sin > default_sin

    def test_with_different_fluid_densities(self):
        """Test with different fluid densities."""
        light_sin, _ = self._critical(
            rod_diameter=0.875,
            fluid_density=50.0,  # Light fluid
        )
        heavy_sin, _ = self._critical(
            rod_diameter=0.875,
            fluid_density=70.0,  # Heavy fluid
        )

        # Different fluid densities affect buoyancy
        assert light_sin != heavy_sin

    def test_common_rod_sizes(self):
        """Test critical loads for common rod sizes."""
        common_sizes = [0.625, 0.75, 0.875, 1.0, 1.125]

        prev_sinusoidal = 0
        for diameter in common_sizes:
            sinusoidal, helical = self._critical(rod_diameter=diameter)
            # Each larger size should have higher critical load
            assert sinusoidal > prev_sinusoidal
            prev_sinusoidal = sinusoidal

    @staticmethod
    def _critical(**kwargs):
        """Call the function with the wellbore inputs the formula requires."""
        kwargs.setdefault("inclination_deg", DEVIATED_INCLINATION_DEG)
        kwargs.setdefault("tubing_id", TUBING_ID_IN)
        return calculate_critical_buckling_load(**kwargs)


class TestRodBucklingAnalysisModel:
    """Tests for RodBucklingAnalysis model."""

    def test_analysis_defaults(self):
        """Every field defaults to None, i.e. "not computed" - never 0.0/False.

        A 0.0 or False default is indistinguishable from a genuinely computed
        zero or a genuine "no buckling" verdict.
        """
        analysis = RodBucklingAnalysis()
        assert analysis.sinusoidal_buckling_detected is None
        assert analysis.helical_buckling_detected is None
        assert analysis.neutral_point_depth is None
        assert analysis.neutral_point_fraction is None
        assert analysis.max_compressive_load is None
        assert analysis.compression_depth_start is None
        assert analysis.compression_length is None
        assert analysis.sinusoidal_critical_load is None
        assert analysis.helical_critical_load is None
        assert analysis.min_buckling_tendency is None
        assert analysis.max_buckling_tendency is None
        assert analysis.analysis_method is None

    def test_analysis_with_values(self):
        """Test RodBucklingAnalysis with actual values."""
        analysis = RodBucklingAnalysis(
            sinusoidal_buckling_detected=True,
            helical_buckling_detected=False,
            neutral_point_depth=3500.0,
            neutral_point_fraction=0.7,
            max_compressive_load=1500.0,
            sinusoidal_critical_load=800.0,
            helical_critical_load=2264.0,
        )
        assert analysis.sinusoidal_buckling_detected is True
        assert analysis.helical_buckling_detected is False
        assert analysis.neutral_point_depth == 3500.0

    def test_analysis_serialization(self):
        """Test that RodBucklingAnalysis can be serialized."""
        analysis = RodBucklingAnalysis(
            sinusoidal_buckling_detected=True,
            neutral_point_depth=3500.0,
            max_compressive_load=1500.0,
        )

        data = analysis.model_dump()
        assert data["sinusoidal_buckling_detected"] is True
        assert data["neutral_point_depth"] == 3500.0
        assert data["max_compressive_load"] == 1500.0


class TestRodBucklingWithRealData:
    """Tests using real well card data."""

    @pytest.fixture
    def well_7699227(self):
        """Load well 7699227 test data."""
        filepath = TEST_DATA_DIR / "7699227.json"
        if filepath.exists():
            return load_from_json_file(filepath)
        pytest.skip("Test data file 7699227.json not found")

    def test_buckling_calculation_7699227(self, well_7699227):
        """Test buckling calculation with well 7699227 data."""
        result = calculate_rod_buckling(well_7699227)

        # Should complete analysis
        assert result.analysis_method == "surface_card_estimate"

    def test_critical_loads_positive_7699227(self, well_7699227):
        """Test that critical loads are positive with real data."""
        result = calculate_rod_buckling(
            well_7699227,
            inclination_deg=DEVIATED_INCLINATION_DEG,
            tubing_id=TUBING_ID_IN,
        )

        assert result.sinusoidal_critical_load > 0
        assert result.helical_critical_load > 0

    def test_no_neutral_point_depth_from_real_card_7699227(self, well_7699227):
        """Real card data still cannot yield a neutral-point depth."""
        result = calculate_rod_buckling(well_7699227)

        assert result.neutral_point_depth is None
        assert result.neutral_point_fraction is None
        assert result.compression_depth_start is None
        assert result.compression_length is None

    def test_buckling_tendency_calculated_7699227(self, well_7699227):
        """Test buckling tendency is calculated."""
        result = calculate_rod_buckling(well_7699227)

        # Should have buckling tendency values
        assert (
            result.min_buckling_tendency != 0
            or result.max_buckling_tendency != 0
        )
