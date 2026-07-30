# ABOUTME: Unit tests for rod buckling analysis module.
# ABOUTME: Validates buckling detection, neutral point, and critical load calculations.

import json
import math
import pytest
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
REAL_CARD_DIR = Path(__file__).parents[1] / "test_data"

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
        result = calculator.calculate(load_datum="net_pump_load")
        assert isinstance(result, RodBucklingAnalysis)

    def test_load_datum_is_required_and_keyword_only(self):
        """A card's zero convention must never be inferred."""
        context = self._create_test_context()
        calculator = RodBucklingCalculator(context)

        with pytest.raises(TypeError):
            calculator.calculate()
        with pytest.raises(TypeError):
            calculator.calculate(None, None, None, "net_pump_load")

    def test_vendor_datum_requires_complete_consistent_metadata(self):
        """Vendor-datum loads without a verified offset must be rejected."""
        context = self._create_test_context()
        calculator = RodBucklingCalculator(context)

        with pytest.raises(ValidationError, match="downstroke"):
            calculator.calculate(load_datum="vendor_analysis")

        with pytest.raises(ValidationError, match="inconsistent"):
            calculator.calculate(
                load_datum="vendor_analysis",
                vendor_downstroke_load=-100.0,
                vendor_upstroke_load=900.0,
                vendor_fluid_load=700.0,
            )

        with pytest.raises(ValidationError, match="positive"):
            calculator.calculate(
                load_datum="vendor_analysis",
                vendor_downstroke_load=-100.0,
                vendor_upstroke_load=-200.0,
                vendor_fluid_load=-100.0,
            )

    def test_non_finite_loads_are_rejected(self):
        """NaN loads must not fall through to a false not-buckled verdict."""
        context = self._create_test_context()
        card = CardData(position=[0, 1], load=[math.nan, 100.0])

        with pytest.raises(ValidationError, match="finite"):
            RodBucklingCalculator(context).calculate(
                downhole_card=card,
                load_datum="net_pump_load",
            )

    def test_vendor_datum_rejects_sub_pound_relative_mismatch(self):
        """The 1% consistency tolerance remains relative below one pound."""
        context = self._create_test_context()

        with pytest.raises(ValidationError, match="inconsistent"):
            RodBucklingCalculator(context).calculate(
                load_datum="vendor_analysis",
                vendor_downstroke_load=0.0,
                vendor_upstroke_load=-0.001,
                vendor_fluid_load=0.001,
            )

    def test_vendor_correction_overflow_is_rejected(self):
        """Finite inputs that overflow during datum correction remain invalid."""
        context = self._create_test_context()
        card = CardData(position=[0, 1], load=[1e308, 0.0])

        with pytest.raises(ValidationError, match="finite"):
            RodBucklingCalculator(context).calculate(
                downhole_card=card,
                load_datum="vendor_analysis",
                vendor_downstroke_load=-1e308,
                vendor_upstroke_load=0.0,
                vendor_fluid_load=1e308,
            )

    def test_vendor_datum_requires_vendor_downhole_card(self):
        """A vendor-card offset cannot be applied to estimated surface loads."""
        context = self._create_test_context()

        with pytest.raises(ValidationError, match="downhole card"):
            RodBucklingCalculator(context).calculate(
                load_datum="vendor_analysis",
                vendor_downstroke_load=-100.0,
                vendor_upstroke_load=900.0,
                vendor_fluid_load=1000.0,
            )

    @pytest.mark.parametrize(
        ("inclination_deg", "tubing_id"),
        [(math.nan, TUBING_ID_IN), (30.0, math.inf)],
    )
    def test_non_finite_scalar_wellbore_inputs_are_rejected(
        self,
        inclination_deg,
        tubing_id,
    ):
        """NaN/Inf geometry must not create NaN thresholds and false verdicts."""
        context = self._create_test_context()

        with pytest.raises(ValidationError, match="finite"):
            RodBucklingCalculator(context).calculate(
                inclination_deg=inclination_deg,
                tubing_id=tubing_id,
                load_datum="net_pump_load",
            )

    @pytest.mark.parametrize("inclination_deg", [-1.0, 181.0])
    def test_scalar_inclination_outside_physical_range_is_rejected(
        self,
        inclination_deg,
    ):
        """Scalar inclination obeys the same physical range as a profile."""
        context = self._create_test_context()

        with pytest.raises(ValidationError, match="between 0 and 180"):
            RodBucklingCalculator(context).calculate(
                inclination_deg=inclination_deg,
                tubing_id=TUBING_ID_IN,
                load_datum="net_pump_load",
            )

    def test_net_pump_load_has_no_solid_rod_pressure_shift(self):
        """A solid submerged rod has no bore ballooning correction."""
        context = self._create_test_context()
        card = CardData(
            position=[0, 1, 2],
            load=[-700.0, 100.0, 3000.0],
        )

        result = RodBucklingCalculator(context).calculate(
            downhole_card=card,
            load_datum="net_pump_load",
        )

        assert result.min_buckling_tendency == -700.0
        assert result.max_buckling_tendency == 3000.0
        assert result.max_compressive_load == 700.0

    def test_vendor_analysis_datum_zeros_the_downstroke_leg(self):
        """The vendor downstroke load is the card's datum offset."""
        context = self._create_test_context()
        card = CardData(
            position=[0, 1, 2],
            load=[-1755.0, 2210.0, -1200.0],
        )

        result = RodBucklingCalculator(context).calculate(
            downhole_card=card,
            load_datum="vendor_analysis",
            vendor_downstroke_load=-1755.0,
            vendor_upstroke_load=2210.0,
            vendor_fluid_load=3965.0,
        )

        assert result.min_buckling_tendency == 0.0
        assert result.max_buckling_tendency == 3965.0

    def test_bottom_section_controls_critical_load(self):
        """A smaller rod above a bottom sinker bar must not set the threshold."""
        context = self._create_test_context()
        context.rod_string = [
            RodSection(diameter=0.625, length=4000.0),
            RodSection(diameter=1.5, length=1000.0),
        ]

        result = RodBucklingCalculator(context).calculate(
            inclination_deg=DEVIATED_INCLINATION_DEG,
            tubing_id=2.5,
            load_datum="net_pump_load",
        )

        bottom = context.rod_string[-1]
        diameter = bottom.diameter
        inertia = math.pi * diameter ** 4 / 64.0
        weight = bottom.weight_per_foot * (
            1.0 - context.fluid_density / 490.0
        ) / 12.0
        clearance = (2.5 - diameter) / 2.0
        expected = 2.0 * math.sqrt(
            bottom.modulus_of_elasticity
            * inertia
            * weight
            * math.sin(math.radians(DEVIATED_INCLINATION_DEG))
            / clearance
        )
        assert result.sinusoidal_critical_load == pytest.approx(expected)

    def test_bottom_section_is_evaluated_across_inclination_profile(self):
        """The weakest point across a varying bottom taper sets the threshold."""
        context = self._create_test_context()
        context.pump.depth = 5500.0
        profile = [(0.0, 10.0), (4500.0, 42.0), (5500.0, 77.0)]

        result = RodBucklingCalculator(context).calculate(
            tubing_id=TUBING_ID_IN,
            load_datum="net_pump_load",
            inclination_profile=profile,
        )

        bottom = context.rod_string[-1]
        diameter = bottom.diameter
        inertia = math.pi * diameter ** 4 / 64.0
        weight = bottom.weight_per_foot * (
            1.0 - context.fluid_density / 490.0
        ) / 12.0
        clearance = (TUBING_ID_IN - diameter) / 2.0
        expected = 2.0 * math.sqrt(
            bottom.modulus_of_elasticity
            * inertia
            * weight
            * math.sin(math.radians(42.0))
            / clearance
        )
        assert result.sinusoidal_critical_load == pytest.approx(expected)

    def test_critical_loads_calculated(self):
        """Test that critical buckling loads are calculated when inputs allow."""
        context = self._create_test_context()
        calculator = RodBucklingCalculator(context)
        result = calculator.calculate(
            inclination_deg=DEVIATED_INCLINATION_DEG,
            tubing_id=TUBING_ID_IN,
            load_datum="net_pump_load",
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
        result = RodBucklingCalculator(context).calculate(
            load_datum="net_pump_load"
        )

        assert result.sinusoidal_critical_load is None
        assert result.helical_critical_load is None
        assert "inclination" in result.warning_message.lower()

        # Missing either one alone is still not enough
        only_inc = RodBucklingCalculator(context).calculate(
            inclination_deg=30.0,
            load_datum="net_pump_load",
        )
        assert only_inc.sinusoidal_critical_load is None
        only_id = RodBucklingCalculator(context).calculate(
            tubing_id=TUBING_ID_IN,
            load_datum="net_pump_load",
        )
        assert only_id.sinusoidal_critical_load is None

    def test_critical_load_is_a_force_in_pounds(self):
        """Critical load must scale like sqrt(E*I*w*sin(alpha)/r_c) in pounds."""
        context = self._create_test_context()
        result = RodBucklingCalculator(context).calculate(
            inclination_deg=DEVIATED_INCLINATION_DEG,
            tubing_id=TUBING_ID_IN,
            load_datum="net_pump_load",
        )

        # Reproduce the formula independently for the bottom (0.625 in) section
        d = 0.625
        section = next(s for s in context.rod_string if s.diameter == d)
        inertia = math.pi * d ** 4 / 64.0
        buoyancy = 1.0 - context.fluid_density / 490.0
        w = section.weight_per_foot * buoyancy / 12.0
        r_c = (TUBING_ID_IN - d) / 2.0
        expected = 2.0 * math.sqrt(
            section.modulus_of_elasticity
            * inertia
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
            load_datum="net_pump_load",
        )
        steeper = RodBucklingCalculator(context).calculate(
            inclination_deg=math.degrees(math.asin(0.50)),
            tubing_id=TUBING_ID_IN,
            load_datum="net_pump_load",
        )
        ratio = steeper.sinusoidal_critical_load / shallow.sinusoidal_critical_load
        assert abs(ratio - math.sqrt(2.0)) < 1e-6

    def test_vertical_hole_reports_no_critical_load(self):
        """Paslay-Dawson degenerates at zero inclination - must not fake it."""
        context = self._create_test_context()
        result = RodBucklingCalculator(context).calculate(
            inclination_deg=0.0,
            tubing_id=TUBING_ID_IN,
            load_datum="net_pump_load",
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
            load_datum="net_pump_load",
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
            load_datum="net_pump_load",
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
        result = RodBucklingCalculator(context).calculate(
            load_datum="net_pump_load"
        )

        assert result.neutral_point_depth is None
        assert result.neutral_point_fraction is None
        assert result.compression_depth_start is None
        assert result.compression_length is None

        # Supplying a real downhole card does not change that
        downhole_card = CardData(
            position=[0, 50, 100, 50],
            load=[-1000, 2000, 5000, 2000],
        )
        result = RodBucklingCalculator(context).calculate(
            downhole_card=downhole_card,
            load_datum="net_pump_load",
        )
        assert result.neutral_point_depth is None
        assert result.compression_length is None

    def test_analysis_method_records_what_actually_ran(self):
        """analysis_method must be assigned, not left at a default string."""
        context = self._create_test_context()

        estimated = RodBucklingCalculator(context).calculate(
            load_datum="net_pump_load"
        )
        assert estimated.analysis_method == "surface_card_estimate"

        downhole_card = CardData(
            position=[0, 50, 100, 50],
            load=[1000, 2000, 3000, 2000],
        )
        measured = RodBucklingCalculator(context).calculate(
            downhole_card=downhole_card,
            load_datum="net_pump_load",
        )
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
            load_datum="net_pump_load",
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
            load_datum="net_pump_load",
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
        result = RodBucklingCalculator(context).calculate(
            downhole_card=downhole_card,
            load_datum="net_pump_load",
        )

        assert result.sinusoidal_buckling_detected is None
        assert result.helical_buckling_detected is None
        # The load magnitude itself is still reported
        assert result.max_compressive_load > 0

    def test_buckling_verdict_follows_threshold_not_raw_load_rule(self):
        """A compression below the critical threshold must NOT trip buckling.

        The verdict compares the compressive pump load to the critical load,
        not merely to zero.
        """
        context = self._create_test_context()
        downhole_card = CardData(
            position=[0, 50, 100, 50],
            load=[-100, 1000, 3000, 1000],
        )
        result = RodBucklingCalculator(context).calculate(
            downhole_card=downhole_card,
            inclination_deg=DEVIATED_INCLINATION_DEG,
            tubing_id=TUBING_ID_IN,
            load_datum="net_pump_load",
        )

        assert result.max_compressive_load < result.sinusoidal_critical_load
        assert result.sinusoidal_buckling_detected is False
        assert result.helical_buckling_detected is False

    def test_buckling_trips_when_threshold_is_exceeded(self):
        """Above the critical threshold the verdict must be True."""
        context = self._create_test_context()
        result_ref = RodBucklingCalculator(context).calculate(
            inclination_deg=DEVIATED_INCLINATION_DEG,
            tubing_id=TUBING_ID_IN,
            load_datum="net_pump_load",
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
            load_datum="net_pump_load",
        )

        assert result.max_compressive_load > critical
        assert result.sinusoidal_buckling_detected is True

    def test_buckling_tendency_calculated(self):
        """Test that buckling tendency min/max are calculated."""
        context = self._create_test_context()
        calculator = RodBucklingCalculator(context)
        result = calculator.calculate(load_datum="net_pump_load")

        # Buckling tendency should be calculated
        assert result.min_buckling_tendency != 0 or result.max_buckling_tendency != 0

    def test_empty_surface_card_raises_validation_error(self):
        """Test that empty surface card raises ValidationError."""
        context = self._create_test_context()
        context.surface_card = CardData(position=[], load=[])
        calculator = RodBucklingCalculator(context)

        with pytest.raises(ValidationError) as exc_info:
            calculator.calculate(load_datum="net_pump_load")

        assert "load" in exc_info.value.message.lower() or "data" in exc_info.value.message.lower()

    def test_zero_rod_length_raises_validation_error(self):
        """Test that zero rod length raises ValidationError."""
        context = self._create_test_context()
        context.rod_string = []  # No rod sections
        calculator = RodBucklingCalculator(context)

        with pytest.raises(ValidationError) as exc_info:
            calculator.calculate(load_datum="net_pump_load")

        assert "rod" in exc_info.value.message.lower() or "length" in exc_info.value.message.lower()

    def test_convenience_function_handles_errors_gracefully(self):
        """Test that convenience function returns result with warning on error."""
        context = self._create_test_context()
        context.surface_card = CardData(position=[], load=[])

        # Use convenience function with raise_on_error=False (default)
        result = calculate_rod_buckling(
            context,
            raise_on_error=False,
            load_datum="net_pump_load",
        )

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
        result = calculator.calculate(
            downhole_card=custom_card,
            load_datum="net_pump_load",
        )

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
        result = calculate_rod_buckling(
            context,
            load_datum="net_pump_load",
        )
        assert isinstance(result, RodBucklingAnalysis)

    def test_with_downhole_card(self):
        """Test convenience function with downhole card."""
        context = self._create_test_context()
        downhole_card = CardData(
            position=[0, 50, 100, 50],
            load=[1000, 2000, 3000, 2000],
        )
        result = calculate_rod_buckling(
            context,
            downhole_card=downhole_card,
            load_datum="net_pump_load",
        )
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

    @pytest.mark.parametrize(
        ("field", "value"),
        [
            ("rod_diameter", math.nan),
            ("modulus", math.inf),
            ("weight_per_foot", math.nan),
            ("fluid_density", math.inf),
            ("inclination_deg", math.nan),
            ("tubing_id", math.inf),
        ],
    )
    def test_non_finite_inputs_are_rejected(self, field, value):
        """The public helper must not return NaN critical loads."""
        inputs = {
            "rod_diameter": 0.875,
            "inclination_deg": DEVIATED_INCLINATION_DEG,
            "tubing_id": TUBING_ID_IN,
        }
        inputs[field] = value

        with pytest.raises(ValidationError, match="finite"):
            calculate_critical_buckling_load(**inputs)

    @pytest.mark.parametrize("inclination_deg", [-1.0, 181.0])
    def test_out_of_range_inclination_is_rejected(self, inclination_deg):
        """The public helper rejects non-physical inclination angles."""
        with pytest.raises(ValidationError, match="between 0 and 180"):
            calculate_critical_buckling_load(
                rod_diameter=0.875,
                inclination_deg=inclination_deg,
                tubing_id=TUBING_ID_IN,
            )

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

    def test_real_card_verdict_changes_between_load_datums(self):
        """The cleansed well 005 card is safe only after datum correction."""
        filepath = REAL_CARD_DIR / "cleansed_well_005.json"
        raw = json.loads(filepath.read_text())
        context = load_from_json_file(filepath)
        downhole_raw = raw["downholeCard"]
        downhole_card = CardData(
            position=downhole_raw["Position"],
            load=downhole_raw["Load"],
        )
        vendor = raw["downholeCardAnalysis"]
        profile = [
            (point["MD"], point["Inclination"])
            for point in raw["surveyData"]
        ]

        uncorrected = calculate_rod_buckling(
            context,
            downhole_card=downhole_card,
            tubing_id=TUBING_ID_IN,
            load_datum="net_pump_load",
            inclination_profile=profile,
            raise_on_error=True,
        )
        corrected = calculate_rod_buckling(
            context,
            downhole_card=downhole_card,
            tubing_id=TUBING_ID_IN,
            load_datum="vendor_analysis",
            inclination_profile=profile,
            vendor_downstroke_load=float(vendor["Fluid Load Downstroke"]),
            vendor_upstroke_load=float(vendor["Fluid Load Upstroke"]),
            vendor_fluid_load=float(vendor["Fluid Load"]),
            raise_on_error=True,
        )

        assert uncorrected.max_compressive_load == pytest.approx(2032.21176)
        # Datum correction: -2032.21176 - (-1755) = -277.21176 lb.
        assert corrected.max_compressive_load == pytest.approx(277.21176)
        # Bottom 0.75-in section at the minimum 35.94-degree inclination:
        # 2*sqrt(30.5e6*(pi*0.75^4/64)
        #        *(1.634*(1-62.4025742248/490)/12)
        #        *sin(35.94deg)/((2.441-0.75)/2))
        # = 395.3498 lb.
        assert corrected.sinusoidal_critical_load == pytest.approx(
            395.3498,
            abs=0.0001,
        )
        assert uncorrected.sinusoidal_buckling_detected is True
        assert corrected.sinusoidal_buckling_detected is False
        assert corrected.neutral_point_depth is None
        assert corrected.compression_length is None

    def test_buckling_calculation_7699227(self, well_7699227):
        """Test buckling calculation with well 7699227 data."""
        result = calculate_rod_buckling(
            well_7699227,
            load_datum="net_pump_load",
        )

        # Should complete analysis
        assert result.analysis_method == "surface_card_estimate"

    def test_critical_loads_positive_7699227(self, well_7699227):
        """Test that critical loads are positive with real data."""
        result = calculate_rod_buckling(
            well_7699227,
            inclination_deg=DEVIATED_INCLINATION_DEG,
            tubing_id=TUBING_ID_IN,
            load_datum="net_pump_load",
        )

        assert result.sinusoidal_critical_load > 0
        assert result.helical_critical_load > 0

    def test_no_neutral_point_depth_from_real_card_7699227(self, well_7699227):
        """Real card data still cannot yield a neutral-point depth."""
        result = calculate_rod_buckling(
            well_7699227,
            load_datum="net_pump_load",
        )

        assert result.neutral_point_depth is None
        assert result.neutral_point_fraction is None
        assert result.compression_depth_start is None
        assert result.compression_length is None

    def test_buckling_tendency_calculated_7699227(self, well_7699227):
        """Test buckling tendency is calculated."""
        result = calculate_rod_buckling(
            well_7699227,
            load_datum="net_pump_load",
        )

        # Should have buckling tendency values
        assert (
            result.min_buckling_tendency != 0
            or result.max_buckling_tendency != 0
        )
