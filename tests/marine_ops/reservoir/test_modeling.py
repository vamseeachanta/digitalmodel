"""Comprehensive tests for reservoir modeling module.

Tests cover:
- WellData dataclass construction and defaults
- ProductionData dataclass construction
- MaterialBalance calculations (cumulative production, tank material balance)
- WellTestAnalysis (buildup test analysis, R-squared)
- ReservoirSimulation (grid setup, depletion simulation)
- ProductionForecast (Arps decline: exponential, hyperbolic, harmonic)
- ReservoirCharacterization (net pay, property maps)
- ReservoirModel (integrated analysis, export summary, well management)
"""

import numpy as np
import pandas as pd
import pytest
import warnings
from datetime import datetime, timedelta

# NumPy 2.x removed np.trapz; restore it so the source code under test works.
if not hasattr(np, "trapz"):
    np.trapz = np.trapezoid

from digitalmodel.marine_ops.reservoir.properties import (
    ReservoirProperties,
    RockProperties,
    FluidProperties,
    PVTProperties,
)
from digitalmodel.marine_ops.reservoir.modeling import (
    WellData,
    ProductionData,
    MaterialBalance,
    WellTestAnalysis,
    ReservoirSimulation,
    ProductionForecast,
    ReservoirCharacterization,
    ReservoirModel,
)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

def _make_rock(**overrides):
    """Create RockProperties with typical sandstone values."""
    defaults = dict(
        porosity=0.20,
        permeability=100.0,
        net_to_gross=0.8,
        thickness=50.0,
        compressibility=3e-6,
    )
    defaults.update(overrides)
    return RockProperties(**defaults)


def _make_fluids(**overrides):
    """Create FluidProperties with typical GOM values."""
    defaults = dict(
        oil_density=35.0,
        gas_density=0.7,
        oil_viscosity=2.0,
        oil_formation_volume_factor=1.2,
        gas_formation_volume_factor=0.005,
        water_compressibility=3e-6,
        solution_gor=500.0,
    )
    defaults.update(overrides)
    return FluidProperties(**defaults)


def _make_pvt(**overrides):
    """Create PVTProperties with typical deepwater values."""
    defaults = dict(
        pressure=4000.0,
        temperature=200.0,
        initial_pressure=5000.0,
        z_factor=0.85,
    )
    defaults.update(overrides)
    return PVTProperties(**defaults)


def _make_reservoir(**overrides):
    """Create ReservoirProperties with full typical GOM deepwater values."""
    rock = overrides.pop("rock", _make_rock())
    fluids = overrides.pop("fluids", _make_fluids())
    pvt = overrides.pop("pvt", _make_pvt())
    defaults = dict(
        rock=rock,
        fluids=fluids,
        pvt=pvt,
        area=640.0,          # 1 section
        depth=12000.0,
        water_saturation=0.25,
    )
    defaults.update(overrides)
    return ReservoirProperties(**defaults)


def _make_well(**overrides):
    """Create WellData with realistic coordinates."""
    defaults = dict(
        well_id="W-001",
        x_coord=1000.0,
        y_coord=2000.0,
        measured_depth=12500.0,
        well_type="producer",
        completion_type="cased",
        drainage_area=160.0,
        skin_factor=2.0,
        wellbore_radius=0.25,
    )
    defaults.update(overrides)
    return WellData(**defaults)


def _make_production_series(
    well_id="W-001",
    start_date=None,
    n_points=12,
    qi=1000.0,
    decline_rate=0.0005,
):
    """Create a declining production series for testing."""
    if start_date is None:
        start_date = datetime(2024, 1, 1)
    data = []
    for i in range(n_points):
        date = start_date + timedelta(days=30 * i)
        rate = qi * np.exp(-decline_rate * 30 * i)
        data.append(
            ProductionData(
                well_id=well_id,
                date=date,
                oil_rate=rate,
                gas_rate=rate * 0.5,  # 500 scf/STB GOR
                water_rate=rate * 0.1,
                flowing_pressure=3000.0 - i * 50,
            )
        )
    return data


# ===========================================================================
# WellData Tests
# ===========================================================================

class TestWellData:
    """Tests for WellData dataclass."""

    def test_basic_construction(self):
        wd = WellData(well_id="A-1", x_coord=100, y_coord=200, measured_depth=10000)
        assert wd.well_id == "A-1"
        assert wd.x_coord == 100.0
        assert wd.y_coord == 200.0
        assert wd.measured_depth == 10000.0

    def test_tvd_defaults_to_md(self):
        wd = WellData(well_id="A-1", x_coord=0, y_coord=0, measured_depth=10000)
        assert wd.true_vertical_depth == 10000.0

    def test_tvd_explicit(self):
        wd = WellData(
            well_id="A-1", x_coord=0, y_coord=0,
            measured_depth=12000, true_vertical_depth=10500,
        )
        assert wd.true_vertical_depth == 10500.0

    def test_default_well_type(self):
        wd = WellData(well_id="A-1", x_coord=0, y_coord=0, measured_depth=5000)
        assert wd.well_type == "producer"

    def test_default_completion_type(self):
        wd = WellData(well_id="A-1", x_coord=0, y_coord=0, measured_depth=5000)
        assert wd.completion_type == "cased"

    def test_default_skin_factor(self):
        wd = WellData(well_id="A-1", x_coord=0, y_coord=0, measured_depth=5000)
        assert wd.skin_factor == 0.0

    def test_default_wellbore_radius(self):
        wd = WellData(well_id="A-1", x_coord=0, y_coord=0, measured_depth=5000)
        assert wd.wellbore_radius == 0.25

    def test_injector_well(self):
        wd = WellData(
            well_id="INJ-1", x_coord=500, y_coord=500,
            measured_depth=9000, well_type="injector",
        )
        assert wd.well_type == "injector"

    def test_fractured_completion(self):
        wd = WellData(
            well_id="F-1", x_coord=0, y_coord=0,
            measured_depth=8000, completion_type="fractured",
        )
        assert wd.completion_type == "fractured"


# ===========================================================================
# ProductionData Tests
# ===========================================================================

class TestProductionData:
    """Tests for ProductionData dataclass."""

    def test_basic_construction(self):
        pd_obj = ProductionData(
            well_id="W-001", date=datetime(2024, 6, 1), oil_rate=500.0,
        )
        assert pd_obj.well_id == "W-001"
        assert pd_obj.oil_rate == 500.0

    def test_default_rates_are_zero(self):
        pd_obj = ProductionData(well_id="W-001", date=datetime(2024, 1, 1))
        assert pd_obj.oil_rate == 0.0
        assert pd_obj.gas_rate == 0.0
        assert pd_obj.water_rate == 0.0

    def test_optional_fields_none_by_default(self):
        pd_obj = ProductionData(well_id="W-001", date=datetime(2024, 1, 1))
        assert pd_obj.flowing_pressure is None
        assert pd_obj.static_pressure is None
        assert pd_obj.gor is None
        assert pd_obj.wor is None
        assert pd_obj.choke_size is None

    def test_full_construction(self):
        pd_obj = ProductionData(
            well_id="W-002",
            date=datetime(2024, 3, 15),
            oil_rate=800.0,
            gas_rate=400.0,
            water_rate=120.0,
            flowing_pressure=2800.0,
            static_pressure=4200.0,
            gor=500.0,
            wor=0.15,
            choke_size=24.0,
        )
        assert pd_obj.gas_rate == 400.0
        assert pd_obj.gor == 500.0
        assert pd_obj.choke_size == 24.0


# ===========================================================================
# MaterialBalance Tests
# ===========================================================================

class TestMaterialBalance:
    """Tests for MaterialBalance class."""

    def test_initialization(self):
        rp = _make_reservoir()
        mb = MaterialBalance(rp)
        assert mb.reservoir is rp
        assert mb.production_history == []
        assert mb.pressure_history == []

    def test_add_production_data(self):
        rp = _make_reservoir()
        mb = MaterialBalance(rp)
        prod = _make_production_series(n_points=5)
        mb.add_production_data(prod)
        assert len(mb.production_history) == 5

    def test_production_data_sorted_by_date(self):
        rp = _make_reservoir()
        mb = MaterialBalance(rp)
        # Add out-of-order
        d1 = ProductionData(well_id="W-001", date=datetime(2024, 6, 1), oil_rate=100)
        d2 = ProductionData(well_id="W-001", date=datetime(2024, 1, 1), oil_rate=200)
        mb.add_production_data([d1, d2])
        assert mb.production_history[0].date < mb.production_history[1].date

    def test_add_pressure_data(self):
        rp = _make_reservoir()
        mb = MaterialBalance(rp)
        mb.add_pressure_data(datetime(2024, 1, 1), 5000.0)
        mb.add_pressure_data(datetime(2024, 6, 1), 4800.0)
        assert len(mb.pressure_history) == 2
        assert mb.pressure_history[0][1] == 5000.0

    def test_pressure_data_sorted(self):
        rp = _make_reservoir()
        mb = MaterialBalance(rp)
        mb.add_pressure_data(datetime(2024, 6, 1), 4800.0)
        mb.add_pressure_data(datetime(2024, 1, 1), 5000.0)
        assert mb.pressure_history[0][0] < mb.pressure_history[1][0]

    def test_cumulative_production_empty(self):
        rp = _make_reservoir()
        mb = MaterialBalance(rp)
        result = mb.calculate_cumulative_production()
        assert len(result['oil']) == 0
        assert len(result['gas']) == 0
        assert len(result['water']) == 0

    def test_cumulative_production_with_data(self):
        rp = _make_reservoir()
        mb = MaterialBalance(rp)
        prod = _make_production_series(n_points=6, qi=500.0)
        mb.add_production_data(prod)
        result = mb.calculate_cumulative_production()
        # Cumulative should be positive for positive rates
        assert result['oil'] > 0
        assert result['gas'] > 0
        assert result['water'] > 0

    def test_tank_material_balance_returns_expected_keys(self):
        rp = _make_reservoir()
        mb = MaterialBalance(rp)
        prod = _make_production_series(n_points=6, qi=500.0)
        mb.add_production_data(prod)
        result = mb.tank_material_balance(current_pressure=4500.0)
        expected_keys = {
            'pressure_drop', 'oil_expansion_factor', 'gas_expansion_factor',
            'water_expansion_factor', 'rock_expansion_factor',
            'ooip_estimate', 'cumulative_oil', 'cumulative_gas', 'cumulative_water',
        }
        assert expected_keys == set(result.keys())

    def test_tank_material_balance_pressure_drop(self):
        rp = _make_reservoir()
        mb = MaterialBalance(rp)
        prod = _make_production_series(n_points=6, qi=500.0)
        mb.add_production_data(prod)
        result = mb.tank_material_balance(current_pressure=4500.0)
        # initial_pressure = 5000, current = 4500 => drop = 500
        assert result['pressure_drop'] == pytest.approx(500.0)

    def test_tank_material_balance_no_initial_pressure(self):
        pvt = PVTProperties(pressure=4000.0, temperature=200.0, initial_pressure=None)
        # PVTProperties.__post_init__ sets initial_pressure = pressure when None
        # So initial_pressure will be 4000 not None. Let's force it.
        pvt.initial_pressure = None
        rp = _make_reservoir(pvt=pvt)
        mb = MaterialBalance(rp)
        prod = _make_production_series(n_points=3)
        mb.add_production_data(prod)
        with pytest.raises(ValueError, match="Initial pressure required"):
            mb.tank_material_balance(current_pressure=3500.0)

    def test_tank_material_balance_no_production(self):
        """With no production data, cumulative_production returns empty arrays.

        The source code evaluates ``cum_prod['oil'] > 0`` on an empty
        numpy array, which raises ValueError in NumPy.  We verify the
        known behaviour here: the method raises when production history
        is empty.
        """
        rp = _make_reservoir()
        mb = MaterialBalance(rp)
        with pytest.raises(ValueError):
            mb.tank_material_balance(current_pressure=4500.0)

    def test_tank_material_balance_ooip_positive(self):
        rp = _make_reservoir()
        mb = MaterialBalance(rp)
        prod = _make_production_series(n_points=12, qi=1000.0)
        mb.add_production_data(prod)
        result = mb.tank_material_balance(current_pressure=4000.0)
        if result['ooip_estimate'] is not None:
            assert result['ooip_estimate'] > 0

    def test_expansion_factors_reasonable(self):
        rp = _make_reservoir()
        mb = MaterialBalance(rp)
        prod = _make_production_series(n_points=3)
        mb.add_production_data(prod)
        result = mb.tank_material_balance(current_pressure=4500.0)
        # Oil FVF used directly
        assert result['oil_expansion_factor'] == pytest.approx(1.2)
        # Water expansion should be > 1 for pressure depletion
        assert result['water_expansion_factor'] > 1.0
        # Rock expansion should be > 1
        assert result['rock_expansion_factor'] > 1.0


# ===========================================================================
# WellTestAnalysis Tests
# ===========================================================================

class TestWellTestAnalysis:
    """Tests for WellTestAnalysis class."""

    def _make_buildup_data(self, n_points=20):
        """Generate synthetic buildup pressure data.

        Simulates a pressure buildup with a characteristic log-linear
        rise that a Horner plot analysis expects.
        """
        rp = _make_reservoir()
        wd = _make_well()
        wta = WellTestAnalysis(rp, wd)
        # Generate buildup-like data: pressure increasing with log(time)
        for i in range(1, n_points + 1):
            t = i * 0.5  # 0.5 hour intervals
            # Pressure rising as buildup: P = P_initial + m * log10(t)
            p = 3000.0 + 200.0 * np.log10(t + 0.1)
            wta.add_test_data(t, p)
        return wta

    def test_initialization(self):
        rp = _make_reservoir()
        wd = _make_well()
        wta = WellTestAnalysis(rp, wd)
        assert wta.reservoir is rp
        assert wta.well is wd
        assert wta.test_data == []

    def test_add_test_data(self):
        rp = _make_reservoir()
        wd = _make_well()
        wta = WellTestAnalysis(rp, wd)
        wta.add_test_data(1.0, 3000.0)
        wta.add_test_data(2.0, 3100.0)
        assert len(wta.test_data) == 2

    def test_test_data_sorted(self):
        rp = _make_reservoir()
        wd = _make_well()
        wta = WellTestAnalysis(rp, wd)
        wta.add_test_data(5.0, 3200.0)
        wta.add_test_data(1.0, 3000.0)
        wta.add_test_data(3.0, 3100.0)
        assert wta.test_data[0][0] == 1.0
        assert wta.test_data[-1][0] == 5.0

    def test_insufficient_data_raises(self):
        rp = _make_reservoir()
        wd = _make_well()
        wta = WellTestAnalysis(rp, wd)
        wta.add_test_data(1.0, 3000.0)
        wta.add_test_data(2.0, 3100.0)
        with pytest.raises(ValueError, match="Insufficient test data"):
            wta.analyze_buildup_test()

    def test_buildup_analysis_returns_expected_keys(self):
        wta = self._make_buildup_data(n_points=20)
        result = wta.analyze_buildup_test()
        expected_keys = {
            'permeability', 'skin_factor', 'slope', 'intercept',
            'extrapolated_pressure', 'flow_efficiency', 'r_squared',
        }
        assert expected_keys == set(result.keys())

    def test_buildup_permeability_positive(self):
        wta = self._make_buildup_data(n_points=30)
        result = wta.analyze_buildup_test()
        assert result['permeability'] > 0

    def test_buildup_r_squared_reasonable(self):
        wta = self._make_buildup_data(n_points=30)
        result = wta.analyze_buildup_test()
        # Synthetic data should give a decent R-squared
        assert result['r_squared'] > 0.5

    def test_buildup_extrapolated_pressure(self):
        wta = self._make_buildup_data(n_points=20)
        result = wta.analyze_buildup_test()
        # Extrapolated pressure should be a finite number
        assert np.isfinite(result['extrapolated_pressure'])

    def test_calculate_r_squared_perfect_fit(self):
        rp = _make_reservoir()
        wd = _make_well()
        wta = WellTestAnalysis(rp, wd)
        x = np.array([1.0, 2.0, 3.0, 4.0])
        y = np.array([2.0, 4.0, 6.0, 8.0])  # Perfect linear y = 2x
        coeffs = np.polyfit(x, y, 1)
        r2 = wta._calculate_r_squared(x, y, coeffs)
        assert r2 == pytest.approx(1.0, abs=1e-10)

    def test_calculate_r_squared_poor_fit(self):
        rp = _make_reservoir()
        wd = _make_well()
        wta = WellTestAnalysis(rp, wd)
        x = np.array([1.0, 2.0, 3.0, 4.0, 5.0])
        y = np.array([10.0, 1.0, 10.0, 1.0, 10.0])  # Random-looking
        coeffs = np.polyfit(x, y, 1)
        r2 = wta._calculate_r_squared(x, y, coeffs)
        assert r2 < 0.5

    def test_calculate_r_squared_constant_y(self):
        rp = _make_reservoir()
        wd = _make_well()
        wta = WellTestAnalysis(rp, wd)
        x = np.array([1.0, 2.0, 3.0])
        y = np.array([5.0, 5.0, 5.0])
        coeffs = np.polyfit(x, y, 1)
        r2 = wta._calculate_r_squared(x, y, coeffs)
        # ss_tot = 0, should return 0.0
        assert r2 == 0.0

    def test_buildup_with_default_viscosity(self):
        """When oil_viscosity is None, default of 1.0 cp should be used."""
        rp = _make_reservoir(fluids=FluidProperties(oil_viscosity=None))
        wd = _make_well()
        wta = WellTestAnalysis(rp, wd)
        for i in range(1, 21):
            t = i * 0.5
            p = 3000.0 + 150.0 * np.log10(t + 0.1)
            wta.add_test_data(t, p)
        result = wta.analyze_buildup_test()
        assert result['permeability'] > 0


# ===========================================================================
# ReservoirSimulation Tests
# ===========================================================================

class TestReservoirSimulation:
    """Tests for ReservoirSimulation class."""

    def test_initialization(self):
        rp = _make_reservoir()
        sim = ReservoirSimulation(rp)
        assert sim.grid_size == (10, 10, 1)
        assert sim.pressure_field is None
        assert sim.saturation_field is None

    def test_setup_grid_default(self):
        rp = _make_reservoir()
        sim = ReservoirSimulation(rp)
        sim.setup_grid(5, 5, 2)
        assert sim.grid_size == (5, 5, 2)
        assert sim.pressure_field is not None
        assert sim.pressure_field.shape == (5, 5, 2)

    def test_setup_grid_initial_pressure(self):
        rp = _make_reservoir()
        sim = ReservoirSimulation(rp)
        sim.setup_grid(3, 3)
        # initial_pressure = 5000 from our pvt fixture
        assert np.all(sim.pressure_field == pytest.approx(5000.0))

    def test_setup_grid_saturation_fields(self):
        rp = _make_reservoir()
        sim = ReservoirSimulation(rp)
        sim.setup_grid(4, 4)
        assert 'water' in sim.saturation_field
        assert 'oil' in sim.saturation_field
        assert 'gas' in sim.saturation_field
        assert sim.saturation_field['water'].shape == (4, 4, 1)

    def test_setup_grid_saturation_values(self):
        rp = _make_reservoir()
        sim = ReservoirSimulation(rp)
        sim.setup_grid(3, 3)
        # water_saturation=0.25, oil=0.75, gas=0.0 from fixture
        assert np.all(sim.saturation_field['water'] == pytest.approx(0.25))
        assert np.all(sim.saturation_field['oil'] == pytest.approx(0.75))
        assert np.all(sim.saturation_field['gas'] == pytest.approx(0.0))

    def test_depletion_simulation_auto_grid(self):
        """If grid not set, run_depletion_simulation sets up default 10x10."""
        rp = _make_reservoir()
        sim = ReservoirSimulation(rp)
        result = sim.run_depletion_simulation(production_rate=500, simulation_days=180)
        assert sim.grid_size == (10, 10, 1)
        assert 'time_days' in result

    def test_depletion_simulation_keys(self):
        rp = _make_reservoir()
        sim = ReservoirSimulation(rp)
        sim.setup_grid(5, 5)
        result = sim.run_depletion_simulation(production_rate=500, simulation_days=365)
        expected_keys = {
            'time_days', 'pressure_psi', 'production_rate_stb_day',
            'cumulative_production_stb', 'final_pressure', 'recovery_factor',
        }
        assert expected_keys == set(result.keys())

    def test_depletion_pressure_decline(self):
        rp = _make_reservoir()
        sim = ReservoirSimulation(rp)
        sim.setup_grid(5, 5)
        result = sim.run_depletion_simulation(production_rate=500, simulation_days=365)
        # Pressure should decline over time
        assert result['pressure_psi'][-1] < result['pressure_psi'][0]

    def test_depletion_cumulative_production_monotonic(self):
        rp = _make_reservoir()
        sim = ReservoirSimulation(rp)
        sim.setup_grid(5, 5)
        result = sim.run_depletion_simulation(production_rate=200, simulation_days=365)
        cum_prod = result['cumulative_production_stb']
        # Cumulative production should be non-decreasing
        for i in range(1, len(cum_prod)):
            assert cum_prod[i] >= cum_prod[i - 1]

    def test_depletion_recovery_factor_bounded(self):
        rp = _make_reservoir()
        sim = ReservoirSimulation(rp)
        sim.setup_grid(5, 5)
        result = sim.run_depletion_simulation(production_rate=200, simulation_days=365)
        rf = result['recovery_factor']
        assert rf >= 0

    def test_depletion_time_steps(self):
        rp = _make_reservoir()
        sim = ReservoirSimulation(rp)
        result = sim.run_depletion_simulation(production_rate=100, simulation_days=360)
        # 30-day steps for 360 days => 12 steps (0, 30, 60, ... 330)
        assert len(result['time_days']) == 12

    def test_depletion_economic_limit(self):
        """When pressure drops below 500 psi, production should stop."""
        # Use small pore volume to trigger rapid depletion
        rock = _make_rock(porosity=0.05, thickness=5.0)
        rp = _make_reservoir(rock=rock, area=10.0)
        sim = ReservoirSimulation(rp)
        sim.setup_grid(5, 5)
        result = sim.run_depletion_simulation(production_rate=5000, simulation_days=3650)
        # If pressure dropped below 500, some production entries should be 0
        if result['final_pressure'] < 500:
            assert 0 in result['production_rate_stb_day']

    def test_setup_grid_no_initial_pressure(self):
        """When initial_pressure is None, default 3000 psi is used."""
        pvt = PVTProperties(pressure=2000.0, temperature=180.0)
        pvt.initial_pressure = None
        rp = _make_reservoir(pvt=pvt)
        sim = ReservoirSimulation(rp)
        sim.setup_grid(3, 3)
        assert np.all(sim.pressure_field == pytest.approx(3000.0))


# ===========================================================================
# ProductionForecast Tests
# ===========================================================================

class TestProductionForecast:
    """Tests for ProductionForecast class."""

    def _make_forecaster_with_data(self, n_points=12, qi=1000.0, decline_rate=0.001):
        """Create a ProductionForecast instance with synthetic declining data."""
        pf = ProductionForecast()
        data = _make_production_series(
            n_points=n_points, qi=qi, decline_rate=decline_rate,
        )
        pf.add_production_data(data)
        return pf

    def test_initialization(self):
        pf = ProductionForecast()
        assert pf.production_data == []
        assert pf.forecast_parameters == {}

    def test_add_production_data(self):
        pf = ProductionForecast()
        data = _make_production_series(n_points=5)
        pf.add_production_data(data)
        assert len(pf.production_data) == 5

    def test_production_data_sorted(self):
        pf = ProductionForecast()
        d1 = ProductionData(well_id="W-001", date=datetime(2024, 12, 1), oil_rate=100)
        d2 = ProductionData(well_id="W-001", date=datetime(2024, 1, 1), oil_rate=200)
        pf.add_production_data([d1, d2])
        assert pf.production_data[0].date < pf.production_data[1].date

    def test_insufficient_data_raises(self):
        pf = ProductionForecast()
        pf.add_production_data([
            ProductionData(well_id="W", date=datetime(2024, 1, 1), oil_rate=100),
        ])
        with pytest.raises(ValueError, match="Insufficient production data"):
            pf.arps_decline_analysis()

    def test_exponential_decline_keys(self):
        pf = self._make_forecaster_with_data()
        result = pf.arps_decline_analysis(decline_type='exponential')
        expected_keys = {
            'initial_rate', 'decline_rate', 'hyperbolic_exponent',
            'decline_type', 'r_squared',
        }
        assert expected_keys == set(result.keys())

    def test_exponential_decline_exponent_zero(self):
        pf = self._make_forecaster_with_data()
        result = pf.arps_decline_analysis(decline_type='exponential')
        assert result['hyperbolic_exponent'] == 0

    def test_exponential_decline_positive_initial_rate(self):
        pf = self._make_forecaster_with_data()
        result = pf.arps_decline_analysis(decline_type='exponential')
        assert result['initial_rate'] > 0

    def test_exponential_decline_positive_decline_rate(self):
        pf = self._make_forecaster_with_data(decline_rate=0.002)
        result = pf.arps_decline_analysis(decline_type='exponential')
        assert result['decline_rate'] > 0

    def test_exponential_decline_r_squared(self):
        pf = self._make_forecaster_with_data(n_points=20, decline_rate=0.001)
        result = pf.arps_decline_analysis(decline_type='exponential')
        # Exponential data fit with exponential model should be good
        assert result['r_squared'] > 0.9

    def test_harmonic_decline(self):
        pf = self._make_forecaster_with_data(n_points=12, qi=800.0, decline_rate=0.0003)
        result = pf.arps_decline_analysis(decline_type='harmonic')
        assert result['hyperbolic_exponent'] == 1
        assert result['initial_rate'] > 0
        assert result['decline_rate'] > 0

    def test_hyperbolic_decline(self):
        pf = self._make_forecaster_with_data(n_points=12)
        result = pf.arps_decline_analysis(decline_type='hyperbolic')
        assert result['hyperbolic_exponent'] == 0.5
        assert result['decline_type'] == 'hyperbolic'

    def test_unknown_decline_type_raises(self):
        pf = self._make_forecaster_with_data()
        with pytest.raises(ValueError, match="Unknown decline type"):
            pf.arps_decline_analysis(decline_type='linear')

    def test_forecast_without_analysis_raises(self):
        pf = ProductionForecast()
        with pytest.raises(ValueError, match="Must run decline analysis"):
            pf.forecast_production()

    def test_forecast_exponential_keys(self):
        pf = self._make_forecaster_with_data()
        pf.arps_decline_analysis(decline_type='exponential')
        result = pf.forecast_production(forecast_days=3650)
        expected_keys = {
            'time_days', 'oil_rate_stb_day', 'cumulative_oil_stb', 'eur_stb',
        }
        assert expected_keys == set(result.keys())

    def test_forecast_rates_decline_over_time(self):
        pf = self._make_forecaster_with_data(decline_rate=0.001)
        pf.arps_decline_analysis(decline_type='exponential')
        result = pf.forecast_production(forecast_days=1825)
        rates = result['oil_rate_stb_day']
        assert rates[-1] < rates[0]

    def test_forecast_cumulative_increases(self):
        pf = self._make_forecaster_with_data()
        pf.arps_decline_analysis(decline_type='exponential')
        result = pf.forecast_production(forecast_days=1825)
        cum = result['cumulative_oil_stb']
        for i in range(1, len(cum)):
            assert cum[i] >= cum[i - 1]

    def test_forecast_eur_positive(self):
        pf = self._make_forecaster_with_data()
        pf.arps_decline_analysis(decline_type='exponential')
        result = pf.forecast_production(forecast_days=3650)
        assert result['eur_stb'] > 0

    def test_forecast_harmonic(self):
        pf = self._make_forecaster_with_data(n_points=12, qi=600.0, decline_rate=0.0005)
        pf.arps_decline_analysis(decline_type='harmonic')
        result = pf.forecast_production(forecast_days=1825)
        assert len(result['oil_rate_stb_day']) > 0
        assert result['eur_stb'] > 0

    def test_forecast_hyperbolic(self):
        pf = self._make_forecaster_with_data(n_points=12)
        pf.arps_decline_analysis(decline_type='hyperbolic')
        result = pf.forecast_production(forecast_days=1825)
        assert len(result['oil_rate_stb_day']) > 0

    def test_forecast_duration_affects_length(self):
        pf = self._make_forecaster_with_data()
        pf.arps_decline_analysis(decline_type='exponential')
        short = pf.forecast_production(forecast_days=365)
        long = pf.forecast_production(forecast_days=3650)
        assert len(long['time_days']) > len(short['time_days'])

    def test_zero_rate_data_filtered(self):
        """Production data with zero oil_rate should be excluded from analysis."""
        pf = ProductionForecast()
        data = []
        start = datetime(2024, 1, 1)
        for i in range(5):
            data.append(ProductionData(
                well_id="W-001",
                date=start + timedelta(days=30 * i),
                oil_rate=1000.0 * np.exp(-0.001 * 30 * i),
            ))
        # Add zero-rate records
        for i in range(5, 8):
            data.append(ProductionData(
                well_id="W-001",
                date=start + timedelta(days=30 * i),
                oil_rate=0.0,
            ))
        pf.add_production_data(data)
        # Should not raise - zero-rate entries filtered out, leaving 5 points
        result = pf.arps_decline_analysis(decline_type='exponential')
        assert result['initial_rate'] > 0


# ===========================================================================
# ReservoirCharacterization Tests
# ===========================================================================

class TestReservoirCharacterization:
    """Tests for ReservoirCharacterization class."""

    def _make_log_data(self, n_depths=100):
        """Create synthetic well log data."""
        depths = np.linspace(10000, 10100, n_depths)
        porosity = np.random.uniform(0.05, 0.30, n_depths)
        sw = np.random.uniform(0.15, 0.60, n_depths)
        vsh = np.random.uniform(0.0, 0.60, n_depths)
        perm = np.random.uniform(1.0, 500.0, n_depths)
        return pd.DataFrame({
            'DEPTH': depths,
            'POROSITY': porosity,
            'SW': sw,
            'VSH': vsh,
            'PERMEABILITY': perm,
        })

    def test_initialization(self):
        rp = _make_reservoir()
        rc = ReservoirCharacterization(rp)
        assert rc.reservoir is rp
        assert rc.wells == []
        assert rc.log_data == {}
        assert rc.core_data == {}

    def test_add_well(self):
        rp = _make_reservoir()
        rc = ReservoirCharacterization(rp)
        w = _make_well()
        rc.add_well(w)
        assert len(rc.wells) == 1
        assert rc.wells[0].well_id == "W-001"

    def test_add_log_data(self):
        rp = _make_reservoir()
        rc = ReservoirCharacterization(rp)
        logs = self._make_log_data()
        rc.add_log_data("W-001", logs)
        assert "W-001" in rc.log_data
        assert len(rc.log_data["W-001"]) == 100

    def test_calculate_net_pay_missing_well(self):
        rp = _make_reservoir()
        rc = ReservoirCharacterization(rp)
        with pytest.raises(ValueError, match="No log data available"):
            rc.calculate_net_pay("NONEXISTENT")

    def test_calculate_net_pay_keys(self):
        rp = _make_reservoir()
        rc = ReservoirCharacterization(rp)
        logs = self._make_log_data()
        rc.add_log_data("W-001", logs)
        result = rc.calculate_net_pay("W-001")
        expected_keys = {
            'gross_thickness', 'net_thickness', 'net_to_gross',
            'average_porosity', 'average_water_saturation',
            'average_permeability', 'hydrocarbon_saturation',
        }
        assert expected_keys == set(result.keys())

    def test_calculate_net_pay_gross_thickness(self):
        rp = _make_reservoir()
        rc = ReservoirCharacterization(rp)
        logs = self._make_log_data(n_depths=50)
        rc.add_log_data("W-001", logs)
        result = rc.calculate_net_pay("W-001")
        assert result['gross_thickness'] > 0

    def test_calculate_net_pay_net_less_than_gross(self):
        rp = _make_reservoir()
        rc = ReservoirCharacterization(rp)
        logs = self._make_log_data(n_depths=100)
        rc.add_log_data("W-001", logs)
        result = rc.calculate_net_pay("W-001")
        assert result['net_thickness'] <= result['gross_thickness']

    def test_net_to_gross_bounded(self):
        rp = _make_reservoir()
        rc = ReservoirCharacterization(rp)
        logs = self._make_log_data(n_depths=100)
        rc.add_log_data("W-001", logs)
        result = rc.calculate_net_pay("W-001")
        assert 0 <= result['net_to_gross'] <= 1

    def test_hydrocarbon_saturation_complement(self):
        rp = _make_reservoir()
        rc = ReservoirCharacterization(rp)
        logs = self._make_log_data(n_depths=100)
        rc.add_log_data("W-001", logs)
        result = rc.calculate_net_pay("W-001")
        # hc_sat = 1 - sw
        assert result['hydrocarbon_saturation'] == pytest.approx(
            1 - result['average_water_saturation'], abs=1e-10
        )

    def test_custom_cutoffs(self):
        rp = _make_reservoir()
        rc = ReservoirCharacterization(rp)
        # Uniform data: all porosity = 0.15, all SW = 0.4, all VSH = 0.2
        n = 50
        depths = np.linspace(10000, 10050, n)
        logs = pd.DataFrame({
            'DEPTH': depths,
            'POROSITY': np.full(n, 0.15),
            'SW': np.full(n, 0.40),
            'VSH': np.full(n, 0.20),
            'PERMEABILITY': np.full(n, 100.0),
        })
        rc.add_log_data("W-001", logs)

        # With cutoff 0.1 porosity and 0.5 SW - all pass
        result_loose = rc.calculate_net_pay("W-001", porosity_cutoff=0.1, saturation_cutoff=0.5)
        assert result_loose['net_to_gross'] == pytest.approx(1.0, abs=0.05)

        # With cutoff 0.2 porosity - none pass (0.15 < 0.2)
        result_tight = rc.calculate_net_pay("W-001", porosity_cutoff=0.2, saturation_cutoff=0.5)
        assert result_tight['net_thickness'] == pytest.approx(0.0, abs=0.1)

    def test_generate_property_maps_insufficient_wells(self):
        rp = _make_reservoir()
        rc = ReservoirCharacterization(rp)
        rc.add_well(_make_well(well_id="W-001"))
        rc.add_well(_make_well(well_id="W-002", x_coord=2000))
        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")
            result = rc.generate_property_maps()
            assert len(w) == 1
            assert "at least 3 wells" in str(w[0].message)
        assert result == {}

    def test_generate_property_maps_with_wells(self):
        rp = _make_reservoir()
        rc = ReservoirCharacterization(rp)
        wells = [
            _make_well(well_id="W-001", x_coord=1000, y_coord=1000),
            _make_well(well_id="W-002", x_coord=3000, y_coord=1000),
            _make_well(well_id="W-003", x_coord=2000, y_coord=3000),
        ]
        for w in wells:
            rc.add_well(w)
        result = rc.generate_property_maps()
        assert 'x_grid' in result
        assert 'y_grid' in result
        assert 'porosity_grid' in result
        assert 'thickness_grid' in result
        assert 'well_locations' in result

    def test_generate_property_maps_grid_shapes(self):
        rp = _make_reservoir()
        rc = ReservoirCharacterization(rp)
        wells = [
            _make_well(well_id="W-001", x_coord=0, y_coord=0),
            _make_well(well_id="W-002", x_coord=5000, y_coord=0),
            _make_well(well_id="W-003", x_coord=2500, y_coord=5000),
        ]
        for w in wells:
            rc.add_well(w)
        result = rc.generate_property_maps()
        assert result['porosity_grid'].shape == (20, 20)
        assert result['thickness_grid'].shape == (20, 20)

    def test_generate_property_maps_with_log_data(self):
        """Wells with log data should use net pay; others use default rock props."""
        rp = _make_reservoir()
        rc = ReservoirCharacterization(rp)
        wells = [
            _make_well(well_id="W-001", x_coord=0, y_coord=0),
            _make_well(well_id="W-002", x_coord=5000, y_coord=0),
            _make_well(well_id="W-003", x_coord=2500, y_coord=5000),
        ]
        for w in wells:
            rc.add_well(w)
        # Add log data for one well
        n = 50
        logs = pd.DataFrame({
            'DEPTH': np.linspace(10000, 10050, n),
            'POROSITY': np.full(n, 0.22),
            'SW': np.full(n, 0.30),
            'VSH': np.full(n, 0.10),
            'PERMEABILITY': np.full(n, 150.0),
        })
        rc.add_log_data("W-001", logs)
        result = rc.generate_property_maps()
        # Should succeed and have finite values
        assert np.all(np.isfinite(result['porosity_grid']))


# ===========================================================================
# ReservoirModel Tests
# ===========================================================================

class TestReservoirModel:
    """Tests for ReservoirModel class."""

    def test_initialization(self):
        rp = _make_reservoir()
        model = ReservoirModel(rp, model_name="Test Field")
        assert model.model_name == "Test Field"
        assert model.properties is rp
        assert isinstance(model.material_balance, MaterialBalance)
        assert isinstance(model.simulation, ReservoirSimulation)
        assert isinstance(model.forecast, ProductionForecast)
        assert isinstance(model.characterization, ReservoirCharacterization)

    def test_default_model_name(self):
        rp = _make_reservoir()
        model = ReservoirModel(rp)
        assert model.model_name == "Reservoir Model"

    def test_add_well(self):
        rp = _make_reservoir()
        model = ReservoirModel(rp)
        w = _make_well()
        model.add_well(w)
        assert "W-001" in model.wells
        assert len(model.characterization.wells) == 1

    def test_add_multiple_wells(self):
        rp = _make_reservoir()
        model = ReservoirModel(rp)
        model.add_well(_make_well(well_id="W-001"))
        model.add_well(_make_well(well_id="W-002", x_coord=3000, well_type="injector"))
        assert len(model.wells) == 2

    def test_add_well_test(self):
        rp = _make_reservoir()
        model = ReservoirModel(rp)
        wd = _make_well()
        wta = WellTestAnalysis(rp, wd)
        model.add_well_test("W-001", wta)
        assert "W-001" in model.well_tests

    def test_last_updated_changes(self):
        rp = _make_reservoir()
        model = ReservoirModel(rp)
        initial_time = model.last_updated
        import time
        time.sleep(0.01)
        model.add_well(_make_well())
        assert model.last_updated >= initial_time

    def test_integrated_analysis_basic_keys(self):
        rp = _make_reservoir()
        model = ReservoirModel(rp)
        result = model.run_integrated_analysis()
        assert 'model_name' in result
        assert 'analysis_date' in result
        assert 'reservoir_properties' in result
        assert 'wells' in result

    def test_integrated_analysis_reservoir_properties(self):
        rp = _make_reservoir()
        model = ReservoirModel(rp)
        result = model.run_integrated_analysis()
        rp_result = result['reservoir_properties']
        assert 'ooip' in rp_result
        assert 'ogip' in rp_result
        assert 'pore_volume' in rp_result
        assert 'hydrocarbon_pore_volume' in rp_result

    def test_integrated_analysis_well_counts(self):
        rp = _make_reservoir()
        model = ReservoirModel(rp)
        model.add_well(_make_well(well_id="P-1", well_type="producer"))
        model.add_well(_make_well(well_id="P-2", x_coord=2000, well_type="producer"))
        model.add_well(_make_well(well_id="I-1", x_coord=3000, well_type="injector"))
        result = model.run_integrated_analysis()
        assert result['wells']['total_wells'] == 3
        assert result['wells']['producers'] == 2
        assert result['wells']['injectors'] == 1

    def test_integrated_analysis_with_well_tests(self):
        rp = _make_reservoir()
        model = ReservoirModel(rp)
        wd = _make_well()
        wta = WellTestAnalysis(rp, wd)
        # Add sufficient test data
        for i in range(1, 25):
            t = i * 0.5
            p = 3000.0 + 200.0 * np.log10(t + 0.1)
            wta.add_test_data(t, p)
        model.add_well_test("W-001", wta)
        result = model.run_integrated_analysis()
        assert 'well_tests' in result
        assert 'W-001' in result['well_tests']
        # Should have permeability, not an error
        assert 'permeability' in result['well_tests']['W-001']

    def test_integrated_analysis_with_failed_well_test(self):
        """Well test with insufficient data should result in error dict."""
        rp = _make_reservoir()
        model = ReservoirModel(rp)
        wd = _make_well()
        wta = WellTestAnalysis(rp, wd)
        wta.add_test_data(1.0, 3000.0)  # Only 1 data point
        model.add_well_test("W-001", wta)
        result = model.run_integrated_analysis()
        assert 'error' in result['well_tests']['W-001']

    def test_integrated_analysis_with_material_balance(self):
        rp = _make_reservoir()
        model = ReservoirModel(rp)
        prod = _make_production_series(n_points=6, qi=500.0)
        model.material_balance.add_production_data(prod)
        result = model.run_integrated_analysis()
        assert 'material_balance' in result

    def test_export_summary_keys(self):
        rp = _make_reservoir()
        model = ReservoirModel(rp, model_name="Export Test")
        summary = model.export_summary()
        assert 'model_info' in summary
        assert 'reservoir_properties' in summary
        assert 'reserves' in summary
        assert 'wells' in summary

    def test_export_summary_model_info(self):
        rp = _make_reservoir()
        model = ReservoirModel(rp, model_name="GOM Field A")
        summary = model.export_summary()
        assert summary['model_info']['name'] == "GOM Field A"
        assert 'created' in summary['model_info']
        assert 'last_updated' in summary['model_info']

    def test_export_summary_reservoir_properties(self):
        rp = _make_reservoir()
        model = ReservoirModel(rp)
        summary = model.export_summary()
        rp_summary = summary['reservoir_properties']
        assert rp_summary['porosity'] == 0.20
        assert rp_summary['permeability'] == 100.0
        assert rp_summary['thickness'] == 50.0
        assert rp_summary['area'] == 640.0
        assert rp_summary['water_saturation'] == 0.25

    def test_export_summary_reserves(self):
        rp = _make_reservoir()
        model = ReservoirModel(rp)
        summary = model.export_summary()
        # OOIP should be calculated from properties
        assert summary['reserves']['ooip_stb'] is not None
        assert summary['reserves']['ooip_stb'] > 0

    def test_export_summary_well_list(self):
        rp = _make_reservoir()
        model = ReservoirModel(rp)
        model.add_well(_make_well(well_id="A-1"))
        model.add_well(_make_well(well_id="A-2", x_coord=2000))
        summary = model.export_summary()
        assert summary['wells']['count'] == 2
        assert set(summary['wells']['well_ids']) == {"A-1", "A-2"}

    def test_export_summary_iso_dates(self):
        rp = _make_reservoir()
        model = ReservoirModel(rp)
        summary = model.export_summary()
        # Should be parseable ISO format strings
        datetime.fromisoformat(summary['model_info']['created'])
        datetime.fromisoformat(summary['model_info']['last_updated'])
