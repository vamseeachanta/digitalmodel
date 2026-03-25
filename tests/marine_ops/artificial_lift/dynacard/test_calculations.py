# ABOUTME: Unit tests for dynacard calculations module.
# ABOUTME: Validates fluid load, plunger travel, pump fillage, tubing gradient, CPIP, and production.

import pytest
import numpy as np

from digitalmodel.marine_ops.artificial_lift.dynacard.calculations import (
    calculate_fluid_load,
    calculate_max_plunger_travel,
    calculate_effective_plunger_travel,
    calculate_pump_fillage,
    calculate_tubing_gradient,
    calculate_cpip,
    calculate_theoretical_production,
    run_p1_calculations,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.models import (
    DynacardAnalysisContext,
    CardData,
    RodSection,
    PumpProperties,
    SurfaceUnit,
    FluidLoadAnalysis,
    PumpFillageAnalysis,
    CPIPAnalysis,
    ProductionAnalysis,
    WellTestData,
    InputParameters,
)


# =============================================================================
# HELPERS
# =============================================================================

def _create_sinusoidal_card(n=100):
    """Create a standard sinusoidal downhole card for testing."""
    t = np.linspace(0, 2 * np.pi, n)
    pos = 100 * (1 - np.cos(t))  # 0 to 200 inches
    load = 10000 + 3000 * np.sin(t)  # typical downhole loads
    return CardData(position=pos.tolist(), load=load.tolist())


def _create_test_context():
    """Create a standard DynacardAnalysisContext for testing."""
    t = np.linspace(0, 2 * np.pi, 100)
    pos = 100 * (1 - np.cos(t))
    load = 25000 + 5000 * np.sin(t)
    return DynacardAnalysisContext(
        api14="TEST-WELL-001",
        surface_card=CardData(position=pos.tolist(), load=load.tolist()),
        rod_string=[RodSection(diameter=1.0, length=5000)],
        pump=PumpProperties(diameter=1.75, depth=5000),
        surface_unit=SurfaceUnit(),
        spm=10.0,
    )


# =============================================================================
# FLUID LOAD TESTS
# =============================================================================

class TestFluidLoad:
    """Tests for calculate_fluid_load function."""

    def test_calculate_fluid_load_avg(self):
        """Average method returns FluidLoadAnalysis with positive fluid_load."""
        card = _create_sinusoidal_card()
        result = calculate_fluid_load(card, method='avg')

        assert isinstance(result, FluidLoadAnalysis)
        assert result.fluid_load > 0

    def test_calculate_fluid_load_med(self):
        """Median method returns a valid FluidLoadAnalysis."""
        card = _create_sinusoidal_card()
        result = calculate_fluid_load(card, method='med')

        assert isinstance(result, FluidLoadAnalysis)

    def test_calculate_fluid_load_2pt(self):
        """Two-point method returns FluidLoadAnalysis with nonzero fluid_load."""
        card = _create_sinusoidal_card()
        result = calculate_fluid_load(card, method='2pt')

        assert isinstance(result, FluidLoadAnalysis)
        assert result.fluid_load != 0.0

    def test_calculate_fluid_load_invalid_method(self):
        """Invalid method name raises ValueError."""
        card = _create_sinusoidal_card()

        with pytest.raises(ValueError, match="Unknown method"):
            calculate_fluid_load(card, method='invalid')

    def test_fluid_load_upstroke_greater_than_downstroke(self):
        """For a normal sinusoidal card, upstroke load exceeds downstroke load."""
        card = _create_sinusoidal_card()
        result = calculate_fluid_load(card, method='avg')

        assert result.upstroke_load > result.downstroke_load


# =============================================================================
# PLUNGER TRAVEL TESTS
# =============================================================================

class TestPlungerTravel:
    """Tests for calculate_max_plunger_travel and calculate_effective_plunger_travel."""

    def test_max_plunger_travel(self):
        """Gross stroke matches max(position) - min(position)."""
        card = _create_sinusoidal_card()
        gross_stroke = calculate_max_plunger_travel(card)

        expected = max(card.position) - min(card.position)
        assert gross_stroke == pytest.approx(expected, rel=1e-6)

    def test_effective_plunger_travel_returns_tuple(self):
        """Effective plunger travel returns a (float, int) tuple."""
        card = _create_sinusoidal_card()
        result = calculate_effective_plunger_travel(card)

        assert isinstance(result, tuple)
        assert len(result) == 2
        net_stroke, br_idx = result
        assert isinstance(net_stroke, float)
        assert isinstance(br_idx, (int, np.integer))

    def test_effective_plunger_travel_less_than_gross(self):
        """Net stroke is at most equal to gross stroke."""
        card = _create_sinusoidal_card()
        gross_stroke = calculate_max_plunger_travel(card)
        net_stroke, _ = calculate_effective_plunger_travel(card)

        assert net_stroke <= gross_stroke + 1e-9


# =============================================================================
# PUMP FILLAGE TESTS
# =============================================================================

class TestPumpFillage:
    """Tests for calculate_pump_fillage function."""

    def test_pump_fillage_normal_card(self):
        """Fillage for a normal sinusoidal card is between 0 and 100."""
        card = _create_sinusoidal_card()
        result = calculate_pump_fillage(card)

        assert 0.0 <= result.fillage <= 100.0

    def test_pump_fillage_result_fields(self):
        """Result has positive gross_stroke, non-negative net_stroke, and 4 corners."""
        card = _create_sinusoidal_card()
        result = calculate_pump_fillage(card)

        assert result.gross_stroke > 0
        assert result.net_stroke >= 0
        assert len(result.corners) == 4

    def test_pump_fillage_zero_stroke(self):
        """Card with identical positions produces fillage of 0."""
        card = CardData(position=[50.0] * 20, load=list(range(20)))
        result = calculate_pump_fillage(card)

        assert result.fillage == 0.0


# =============================================================================
# TUBING GRADIENT TESTS
# =============================================================================

class TestTubingGradient:
    """Tests for calculate_tubing_gradient function."""

    def test_tubing_gradient_defaults(self):
        """Default parameters (35 API, 1.03 water SG, 50% WC) yield expected values."""
        tg, fd = calculate_tubing_gradient()

        # oil_sg = 141.5 / (35 + 131.5) = 0.8498
        oil_sg = 141.5 / (35.0 + 131.5)
        # mixed_sg = 1.03 * 0.5 + 0.8498 * 0.5 = 0.9399
        mixed_sg = 1.03 * 0.5 + oil_sg * 0.5
        # tg = 0.433 * 0.9399
        expected_tg = 0.433 * mixed_sg
        # fd = 0.9399 * 62.4
        expected_fd = mixed_sg * 62.4

        assert tg == pytest.approx(expected_tg, rel=0.01)
        assert fd == pytest.approx(expected_fd, rel=0.01)

    def test_tubing_gradient_100_percent_water(self):
        """100% water cut yields gradient based on water SG only."""
        tg, fd = calculate_tubing_gradient(water_cut=100.0)

        expected_tg = 0.433 * 1.03
        assert tg == pytest.approx(expected_tg, rel=0.01)

    def test_tubing_gradient_zero_water_cut(self):
        """0% water cut yields gradient based on oil SG only."""
        tg, fd = calculate_tubing_gradient(water_cut=0.0)

        oil_sg = 141.5 / (35.0 + 131.5)
        expected_tg = 0.433 * oil_sg
        assert tg == pytest.approx(expected_tg, rel=0.01)


# =============================================================================
# CPIP TESTS
# =============================================================================

class TestCPIP:
    """Tests for calculate_cpip function."""

    def test_cpip_with_defaults(self):
        """Without well_test or input_params, uses default water_cut=50 and tubing_pressure=100."""
        ctx = _create_test_context()
        card = _create_sinusoidal_card()
        result = calculate_cpip(ctx, card)

        # Verify tubing gradient matches default 50% water cut
        expected_tg, _ = calculate_tubing_gradient(water_cut=50.0)
        assert result.tubing_gradient == pytest.approx(expected_tg, rel=0.01)

        # PDP = 100 + tg * 5000 (default tubing_pressure + gradient * depth)
        expected_pdp = 100.0 + expected_tg * 5000.0
        assert result.pump_discharge_pressure == pytest.approx(expected_pdp, rel=0.01)

    def test_cpip_with_well_test(self):
        """Water cut from WellTestData is used in calculation."""
        ctx = _create_test_context()
        ctx.well_test = WellTestData(water_cut=80.0)
        card = _create_sinusoidal_card()
        result = calculate_cpip(ctx, card)

        expected_tg, _ = calculate_tubing_gradient(water_cut=80.0)
        assert result.tubing_gradient == pytest.approx(expected_tg, rel=0.01)

    def test_cpip_with_input_params(self):
        """Tubing pressure from InputParameters changes PDP."""
        ctx = _create_test_context()
        ctx.input_params = InputParameters(strokes_per_minute=10, tubing_pressure=200.0)
        card = _create_sinusoidal_card()
        result = calculate_cpip(ctx, card)

        expected_tg, _ = calculate_tubing_gradient(water_cut=50.0)
        expected_pdp = 200.0 + expected_tg * 5000.0
        assert result.pump_discharge_pressure == pytest.approx(expected_pdp, rel=0.01)

    def test_cpip_result_fields(self):
        """All CPIPAnalysis fields are populated (non-zero)."""
        ctx = _create_test_context()
        card = _create_sinusoidal_card()
        result = calculate_cpip(ctx, card)

        assert isinstance(result, CPIPAnalysis)
        assert result.pump_intake_pressure != 0.0
        assert result.pump_discharge_pressure != 0.0
        assert result.fluid_density != 0.0
        assert result.tubing_gradient != 0.0

    def test_cpip_pdp_positive(self):
        """Pump discharge pressure is positive (tubing_pressure + gradient * depth)."""
        ctx = _create_test_context()
        card = _create_sinusoidal_card()
        result = calculate_cpip(ctx, card)

        assert result.pump_discharge_pressure > 0


# =============================================================================
# THEORETICAL PRODUCTION TESTS
# =============================================================================

class TestTheoreticalProduction:
    """Tests for calculate_theoretical_production function."""

    def test_production_with_defaults(self):
        """Without well_test, runtime defaults to 24h and efficiency to pump.efficiency * 100."""
        ctx = _create_test_context()
        card = _create_sinusoidal_card()
        fillage = calculate_pump_fillage(card)
        result = calculate_theoretical_production(ctx, fillage)

        assert isinstance(result, ProductionAnalysis)
        assert result.pump_efficiency == pytest.approx(ctx.pump.efficiency * 100.0, rel=0.01)

    def test_production_with_well_test(self):
        """With WellTestData, pump_efficiency is based on actual vs theoretical."""
        ctx = _create_test_context()
        ctx.well_test = WellTestData(oil_rate=50.0, water_rate=100.0, runtime=20.0)
        card = _create_sinusoidal_card()
        fillage = calculate_pump_fillage(card)
        result = calculate_theoretical_production(ctx, fillage)

        # Pump efficiency = (actual / theoretical) * 100
        actual_production = 50.0 + 100.0
        if result.theoretical_production > 0:
            expected_eff = (actual_production / result.theoretical_production) * 100.0
            assert result.pump_efficiency == pytest.approx(expected_eff, rel=0.01)

    def test_production_gross_greater_than_net(self):
        """Gross displacement is at least as large as net displacement."""
        ctx = _create_test_context()
        card = _create_sinusoidal_card()
        fillage = calculate_pump_fillage(card)
        result = calculate_theoretical_production(ctx, fillage)

        assert result.gross_displacement >= result.net_displacement - 1e-9

    def test_production_positive(self):
        """Theoretical production is positive for normal inputs."""
        ctx = _create_test_context()
        card = _create_sinusoidal_card()
        fillage = calculate_pump_fillage(card)
        result = calculate_theoretical_production(ctx, fillage)

        assert result.theoretical_production > 0


# =============================================================================
# RUN P1 CALCULATIONS TESTS
# =============================================================================

class TestRunP1Calculations:
    """Tests for run_p1_calculations orchestration function."""

    def test_run_p1_returns_all_keys(self):
        """Result dict contains all four expected keys."""
        ctx = _create_test_context()
        card = _create_sinusoidal_card()
        result = run_p1_calculations(ctx, card)

        assert 'fluid_load' in result
        assert 'fillage' in result
        assert 'cpip' in result
        assert 'production' in result

    def test_run_p1_result_types(self):
        """Each result value is the correct analysis type."""
        ctx = _create_test_context()
        card = _create_sinusoidal_card()
        result = run_p1_calculations(ctx, card)

        assert isinstance(result['fluid_load'], FluidLoadAnalysis)
        assert isinstance(result['fillage'], PumpFillageAnalysis)
        assert isinstance(result['cpip'], CPIPAnalysis)
        assert isinstance(result['production'], ProductionAnalysis)

    def test_run_p1_integration(self):
        """All results are internally consistent across calculations."""
        ctx = _create_test_context()
        card = _create_sinusoidal_card()
        result = run_p1_calculations(ctx, card)

        # Fluid load should be positive for a normal card
        assert result['fluid_load'].fluid_load > 0

        # Fillage should be in valid range
        assert 0.0 <= result['fillage'].fillage <= 100.0

        # PDP should be positive
        assert result['cpip'].pump_discharge_pressure > 0

        # Production should be positive
        assert result['production'].theoretical_production > 0

        # Net displacement should use fillage from the fillage analysis
        fillage_fraction = result['fillage'].fillage / 100.0
        expected_net = result['production'].gross_displacement * fillage_fraction
        assert result['production'].net_displacement == pytest.approx(expected_net, rel=0.01)
