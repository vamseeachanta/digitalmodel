#!/usr/bin/env python3
"""Test Framework for Reservoir Analysis Modules.

This module provides comprehensive tests for the digitalmodel.reservoir package
including unit tests, integration tests, and validation tests.

Test Categories:
- Unit tests for individual components
- Integration tests for workflows
- Validation tests against known results
- Performance tests for large datasets
"""

import unittest
import numpy as np
import pandas as pd
from datetime import datetime, timedelta
from typing import Dict, List, Any
import warnings

# Import modules to test
try:
    from digitalmodel.reservoir import (
        ReservoirProperties,
        RockProperties,
        FluidProperties,
        PVTProperties,
        create_sandstone_reservoir,
        create_carbonate_reservoir,
        calculate_porosity,
        calculate_permeability,
        calculate_water_saturation,
        ReservoirModel,
        MaterialBalance,
        WellTestAnalysis,
        ProductionForecast,
        ReservoirAnalysis,
        LogAnalysis,
        StratigraphicAnalysis,
        WellData,
        ProductionData,
        LogCurve,
        StratigraphicLayer
    )
    RESERVOIR_MODULES_AVAILABLE = True
except ImportError as e:
    RESERVOIR_MODULES_AVAILABLE = False
    print(f"Warning: Could not import reservoir modules: {e}")


class TestRockProperties(unittest.TestCase):
    """Test rock properties calculations."""
    
    def setUp(self):
        """Set up test fixtures."""
        if not RESERVOIR_MODULES_AVAILABLE:
            self.skipTest("Reservoir modules not available")
    
    def test_rock_properties_creation(self):
        """Test basic rock properties creation."""
        rock = RockProperties(
            porosity=0.2,
            permeability=100,
            thickness=50
        )
        
        self.assertEqual(rock.porosity, 0.2)
        self.assertEqual(rock.permeability, 100)
        self.assertEqual(rock.thickness, 50)
        self.assertEqual(rock.net_to_gross, 1.0)  # Default value
    
    def test_rock_properties_validation(self):
        """Test validation of rock properties."""
        # Test invalid porosity
        with self.assertRaises(ValueError):
            RockProperties(porosity=1.5, permeability=100)
        
        with self.assertRaises(ValueError):
            RockProperties(porosity=-0.1, permeability=100)
        
        # Test invalid permeability
        with self.assertRaises(ValueError):
            RockProperties(porosity=0.2, permeability=-10)
        
        # Test invalid net-to-gross
        with self.assertRaises(ValueError):
            RockProperties(porosity=0.2, permeability=100, net_to_gross=1.5)
    
    def test_bulk_density_calculation(self):
        """Test bulk density calculation."""
        rock = RockProperties(
            porosity=0.2,
            permeability=100,
            grain_density=2.65
        )
        
        bulk_density = rock.calculate_bulk_density()
        expected = (1 - 0.2) * 2.65 + 0.2 * 1.0  # grain + water
        self.assertAlmostEqual(bulk_density, expected, places=3)
    
    def test_formation_factor_calculation(self):
        """Test formation factor calculation."""
        rock = RockProperties(
            porosity=0.2,
            permeability=100,
            cementation_exponent=2.0
        )
        
        formation_factor = rock.calculate_formation_factor()
        expected = 0.2 ** (-2.0)  # Archie's equation
        self.assertAlmostEqual(formation_factor, expected, places=3)


class TestFluidProperties(unittest.TestCase):
    """Test fluid properties calculations."""
    
    def setUp(self):
        """Set up test fixtures."""
        if not RESERVOIR_MODULES_AVAILABLE:
            self.skipTest("Reservoir modules not available")
    
    def test_api_to_specific_gravity(self):
        """Test API gravity to specific gravity conversion."""
        fluids = FluidProperties(oil_density=30)  # 30 API
        
        sg = fluids.api_to_specific_gravity()
        expected = 141.5 / (30 + 131.5)
        self.assertAlmostEqual(sg, expected, places=4)
    
    def test_oil_density_calculation(self):
        """Test oil density calculation from API."""
        fluids = FluidProperties(oil_density=35, water_density=1.0)
        
        density = fluids.calculate_oil_density_from_api()
        sg = 141.5 / (35 + 131.5)
        expected = sg * 1.0
        self.assertAlmostEqual(density, expected, places=4)


class TestReservoirProperties(unittest.TestCase):
    """Test integrated reservoir properties."""
    
    def setUp(self):
        """Set up test fixtures."""
        if not RESERVOIR_MODULES_AVAILABLE:
            self.skipTest("Reservoir modules not available")
        
        self.rock = RockProperties(
            porosity=0.2,
            permeability=100,
            thickness=50,
            net_to_gross=0.8
        )
        
        self.fluids = FluidProperties(
            oil_formation_volume_factor=1.2
        )
        
        self.pvt = PVTProperties(
            pressure=3000,
            temperature=180
        )
        
        self.reservoir = ReservoirProperties(
            rock=self.rock,
            fluids=self.fluids,
            pvt=self.pvt,
            area=640,  # acres
            water_saturation=0.3
        )
    
    def test_saturation_validation(self):
        """Test saturation normalization and validation."""
        # Should auto-calculate oil saturation
        self.assertAlmostEqual(self.reservoir.oil_saturation, 0.7, places=3)
        self.assertAlmostEqual(self.reservoir.gas_saturation, 0.0, places=3)
        
        # Total saturation should be 1.0
        total = (self.reservoir.water_saturation + 
                self.reservoir.oil_saturation + 
                self.reservoir.gas_saturation)
        self.assertAlmostEqual(total, 1.0, places=3)
    
    def test_pore_volume_calculation(self):
        """Test pore volume calculation."""
        pv = self.reservoir.calculate_pore_volume()
        expected = 640 * 50 * 0.2 * 0.8  # area * thickness * porosity * ntg
        self.assertAlmostEqual(pv, expected, places=1)
    
    def test_ooip_calculation(self):
        """Test OOIP calculation."""
        ooip = self.reservoir.calculate_ooip()
        
        # Manual calculation
        pv = 640 * 50 * 0.2 * 0.8  # acre-ft
        hcpv = pv * 0.7  # oil saturation
        res_bbl = hcpv * 7758  # acre-ft to bbl
        expected_ooip = res_bbl / 1.2  # divide by Bo
        
        self.assertAlmostEqual(ooip, expected_ooip, delta=1000)
    
    def test_helper_functions(self):
        """Test quick setup helper functions."""
        sandstone = create_sandstone_reservoir(
            porosity=0.18,
            permeability=75,
            area=320,
            thickness=60
        )
        
        self.assertEqual(sandstone.rock.porosity, 0.18)
        self.assertEqual(sandstone.rock.permeability, 75)
        self.assertEqual(sandstone.area, 320)
        self.assertEqual(sandstone.rock.thickness, 60)
        
        carbonate = create_carbonate_reservoir(
            porosity=0.15,
            permeability=25,
            area=480,
            thickness=45
        )
        
        self.assertEqual(carbonate.rock.grain_density, 2.71)  # Carbonate default


class TestUtilityFunctions(unittest.TestCase):
    """Test utility calculation functions."""
    
    def setUp(self):
        """Set up test fixtures."""
        if not RESERVOIR_MODULES_AVAILABLE:
            self.skipTest("Reservoir modules not available")
    
    def test_porosity_calculation(self):
        """Test porosity calculation from logs."""
        neutron_por = 0.20
        density_por = 0.18
        
        # Test with shale correction
        porosity = calculate_porosity(neutron_por, density_por, shale_correction=True)
        expected = np.sqrt((neutron_por**2 + density_por**2) / 2)
        self.assertAlmostEqual(porosity, expected, places=4)
        
        # Test without shale correction
        porosity_simple = calculate_porosity(neutron_por, density_por, shale_correction=False)
        expected_simple = (neutron_por + density_por) / 2
        self.assertAlmostEqual(porosity_simple, expected_simple, places=4)
    
    def test_permeability_correlations(self):
        """Test permeability correlation methods."""
        porosity = 0.2
        
        # Test Kozeny-Carman
        perm_kc = calculate_permeability(porosity, method="kozeny_carman")
        self.assertGreater(perm_kc, 0)
        
        # Test Timur
        perm_timur = calculate_permeability(porosity, method="timur", irreducible_water_saturation=0.2)
        self.assertGreater(perm_timur, 0)
        
        # Test invalid method
        with self.assertRaises(ValueError):
            calculate_permeability(porosity, method="invalid_method")
    
    def test_water_saturation_calculation(self):
        """Test water saturation calculation methods."""
        rw = 0.1
        rt = 10.0
        porosity = 0.2
        
        # Test Archie's method
        sw = calculate_water_saturation(rw, rt, porosity, method="archie")
        self.assertGreaterEqual(sw, 0)
        self.assertLessEqual(sw, 1)
        
        # Test invalid method
        with self.assertRaises(ValueError):
            calculate_water_saturation(rw, rt, porosity, method="invalid_method")


class TestLogAnalysis(unittest.TestCase):
    """Test well log analysis capabilities."""
    
    def setUp(self):
        """Set up test fixtures."""
        if not RESERVOIR_MODULES_AVAILABLE:
            self.skipTest("Reservoir modules not available")
        
        # Create synthetic log data
        self.depths = np.arange(2000, 2100, 0.5)
        n_points = len(self.depths)
        
        np.random.seed(42)  # Reproducible results
        
        self.gr_values = 50 + 10 * np.random.normal(0, 1, n_points)
        self.rt_values = 20 + 5 * np.random.normal(0, 1, n_points)
        self.rhob_values = 2.3 + 0.1 * np.random.normal(0, 1, n_points)
        self.nphi_values = 0.18 + 0.03 * np.random.normal(0, 1, n_points)
        
        # Clip to realistic ranges
        self.gr_values = np.clip(self.gr_values, 20, 120)
        self.rt_values = np.clip(self.rt_values, 1, 100)
        self.rhob_values = np.clip(self.rhob_values, 2.0, 2.8)
        self.nphi_values = np.clip(self.nphi_values, 0.05, 0.45)
        
        # Create log analysis
        self.log_analysis = LogAnalysis('TEST-WELL')
        
        # Add curves
        self.log_analysis.add_curve(LogCurve('GR', self.depths, self.gr_values, 'API'))
        self.log_analysis.add_curve(LogCurve('RT', self.depths, self.rt_values, 'ohm-m'))
        self.log_analysis.add_curve(LogCurve('RHOB', self.depths, self.rhob_values, 'g/cm3'))
        self.log_analysis.add_curve(LogCurve('NPHI', self.depths, self.nphi_values, 'fraction'))
    
    def test_log_curve_creation(self):
        """Test log curve data container."""
        curve = LogCurve('GR', self.depths, self.gr_values, 'API')
        
        self.assertEqual(curve.name, 'GR')
        self.assertEqual(curve.units, 'API')
        self.assertEqual(len(curve.depth), len(curve.values))
        
        # Test valid data extraction
        valid_depth, valid_values = curve.get_valid_data()
        self.assertEqual(len(valid_depth), len(valid_values))
    
    def test_facies_calculation(self):
        """Test facies calculation from logs."""
        try:
            facies = self.log_analysis.calculate_facies_from_logs(n_clusters=3)
            
            self.assertIsInstance(facies, np.ndarray)
            self.assertGreater(len(facies), 0)
            
            # Check facies values are in expected range
            self.assertTrue(np.all(facies >= 0))
            self.assertTrue(np.all(facies < 3))
            
        except Exception as e:
            self.fail(f"Facies calculation failed: {e}")
    
    def test_porosity_calculation(self):
        """Test porosity calculation from logs."""
        try:
            porosity = self.log_analysis.calculate_porosity_from_logs()
            
            self.assertIsInstance(porosity, np.ndarray)
            self.assertGreater(len(porosity), 0)
            
            # Check porosity values are reasonable
            valid_porosity = porosity[porosity != -999.25]
            if len(valid_porosity) > 0:
                self.assertTrue(np.all(valid_porosity >= 0))
                self.assertTrue(np.all(valid_porosity <= 0.5))
            
        except Exception as e:
            self.fail(f"Porosity calculation failed: {e}")
    
    def test_water_saturation_calculation(self):
        """Test water saturation calculation from logs."""
        try:
            # First calculate porosity (required for water saturation)
            self.log_analysis.calculate_porosity_from_logs()
            
            sw = self.log_analysis.calculate_water_saturation(rw=0.1)
            
            self.assertIsInstance(sw, np.ndarray)
            self.assertGreater(len(sw), 0)
            
            # Check water saturation values are reasonable
            valid_sw = sw[sw != -999.25]
            if len(valid_sw) > 0:
                self.assertTrue(np.all(valid_sw >= 0))
                self.assertTrue(np.all(valid_sw <= 1))
            
        except Exception as e:
            self.fail(f"Water saturation calculation failed: {e}")
    
    def test_net_pay_calculation(self):
        """Test net pay calculation."""
        try:
            net_pay = self.log_analysis.calculate_net_pay(
                porosity_cutoff=0.1,
                sw_cutoff=0.6
            )
            
            self.assertIsInstance(net_pay, dict)
            
            required_keys = [
                'gross_thickness', 'net_thickness', 'net_to_gross',
                'average_porosity', 'average_water_saturation'
            ]
            
            for key in required_keys:
                self.assertIn(key, net_pay)
                self.assertIsInstance(net_pay[key], (int, float))
            
            # Validate ranges
            self.assertGreaterEqual(net_pay['net_to_gross'], 0)
            self.assertLessEqual(net_pay['net_to_gross'], 1)
            self.assertLessEqual(net_pay['net_thickness'], net_pay['gross_thickness'])
            
        except Exception as e:
            self.fail(f"Net pay calculation failed: {e}")


class TestMaterialBalance(unittest.TestCase):
    """Test material balance calculations."""
    
    def setUp(self):
        """Set up test fixtures."""
        if not RESERVOIR_MODULES_AVAILABLE:
            self.skipTest("Reservoir modules not available")
        
        self.reservoir = create_sandstone_reservoir(
            porosity=0.2,
            permeability=100,
            water_saturation=0.3,
            area=640,
            thickness=50,
            pressure=3000,
            temperature=180
        )
        
        self.material_balance = MaterialBalance(self.reservoir)
        
        # Create production data
        start_date = datetime(2020, 1, 1)
        self.production_data = []
        
        for i in range(365):
            date = start_date + timedelta(days=i)
            oil_rate = 100 * np.exp(-i / 1000)  # Declining production
            
            prod_data = ProductionData(
                well_id='TEST-WELL',
                date=date,
                oil_rate=oil_rate,
                gas_rate=oil_rate * 500,  # GOR = 500
                water_rate=oil_rate * 0.1
            )
            self.production_data.append(prod_data)
    
    def test_production_data_addition(self):
        """Test adding production data."""
        self.material_balance.add_production_data(self.production_data)
        
        self.assertEqual(len(self.material_balance.production_history), 365)
        
        # Check data is sorted by date
        dates = [p.date for p in self.material_balance.production_history]
        self.assertEqual(dates, sorted(dates))
    
    def test_cumulative_production_calculation(self):
        """Test cumulative production calculation."""
        self.material_balance.add_production_data(self.production_data)
        
        cum_prod = self.material_balance.calculate_cumulative_production()
        
        self.assertIn('oil', cum_prod)
        self.assertIn('gas', cum_prod)
        self.assertIn('water', cum_prod)
        
        # Cumulative should be positive
        self.assertGreater(cum_prod['oil'], 0)
        self.assertGreater(cum_prod['gas'], 0)
        self.assertGreater(cum_prod['water'], 0)
    
    def test_tank_material_balance(self):
        """Test tank-type material balance calculation."""
        self.material_balance.add_production_data(self.production_data)
        
        current_pressure = 2800  # Pressure after production
        
        try:
            mb_results = self.material_balance.tank_material_balance(current_pressure)
            
            self.assertIsInstance(mb_results, dict)
            
            required_keys = [
                'pressure_drop', 'cumulative_oil', 'cumulative_gas',
                'oil_expansion_factor', 'gas_expansion_factor'
            ]
            
            for key in required_keys:
                self.assertIn(key, mb_results)
            
            # Pressure drop should be positive
            self.assertGreater(mb_results['pressure_drop'], 0)
            
        except Exception as e:
            self.fail(f"Material balance calculation failed: {e}")


class TestProductionForecast(unittest.TestCase):
    """Test production forecasting capabilities."""
    
    def setUp(self):
        """Set up test fixtures."""
        if not RESERVOIR_MODULES_AVAILABLE:
            self.skipTest("Reservoir modules not available")
        
        self.forecast = ProductionForecast()
        
        # Create synthetic production history with known decline
        start_date = datetime(2020, 1, 1)
        self.production_data = []
        
        qi = 200  # Initial rate
        D = 0.1 / 365  # Decline rate per day
        
        for i in range(365):
            date = start_date + timedelta(days=i)
            oil_rate = qi * np.exp(-D * i)  # Exponential decline
            
            prod_data = ProductionData(
                well_id='FORECAST-WELL',
                date=date,
                oil_rate=oil_rate
            )
            self.production_data.append(prod_data)
    
    def test_decline_analysis(self):
        """Test Arps decline curve analysis."""
        self.forecast.add_production_data(self.production_data)
        
        try:
            decline_params = self.forecast.arps_decline_analysis(decline_type='exponential')
            
            self.assertIsInstance(decline_params, dict)
            
            required_keys = [
                'initial_rate', 'decline_rate', 'hyperbolic_exponent', 
                'decline_type', 'r_squared'
            ]
            
            for key in required_keys:
                self.assertIn(key, decline_params)
            
            # Check reasonable values
            self.assertGreater(decline_params['initial_rate'], 0)
            self.assertGreater(decline_params['decline_rate'], 0)
            self.assertEqual(decline_params['decline_type'], 'exponential')
            
        except Exception as e:
            self.fail(f"Decline analysis failed: {e}")
    
    def test_production_forecast(self):
        """Test production forecasting."""
        self.forecast.add_production_data(self.production_data)
        
        try:
            # First run decline analysis
            self.forecast.arps_decline_analysis(decline_type='exponential')
            
            # Then forecast
            forecast_results = self.forecast.forecast_production(forecast_days=1000)
            
            self.assertIsInstance(forecast_results, dict)
            
            required_keys = [
                'time_days', 'oil_rate_stb_day', 
                'cumulative_oil_stb', 'eur_stb'
            ]
            
            for key in required_keys:
                self.assertIn(key, forecast_results)
                self.assertIsInstance(forecast_results[key], list)
            
            # Check that rates decline over time
            rates = forecast_results['oil_rate_stb_day']
            self.assertGreater(rates[0], rates[-1])
            
            # Cumulative should increase
            cumulative = forecast_results['cumulative_oil_stb']
            self.assertLess(cumulative[0], cumulative[-1])
            
        except Exception as e:
            self.fail(f"Production forecasting failed: {e}")


class TestWellData(unittest.TestCase):
    """Test well data container."""
    
    def setUp(self):
        """Set up test fixtures."""
        if not RESERVOIR_MODULES_AVAILABLE:
            self.skipTest("Reservoir modules not available")
    
    def test_well_data_creation(self):
        """Test well data creation and validation."""
        well = WellData(
            well_id='TEST-001',
            x_coord=1000,
            y_coord=2000,
            measured_depth=2500,
            true_vertical_depth=2450,
            well_type='producer'
        )
        
        self.assertEqual(well.well_id, 'TEST-001')
        self.assertEqual(well.x_coord, 1000)
        self.assertEqual(well.y_coord, 2000)
        self.assertEqual(well.measured_depth, 2500)
        self.assertEqual(well.true_vertical_depth, 2450)
        self.assertEqual(well.well_type, 'producer')
    
    def test_tvd_auto_assignment(self):
        """Test automatic TVD assignment when not provided."""
        well = WellData(
            well_id='TEST-002',
            x_coord=1000,
            y_coord=2000,
            measured_depth=2500
        )
        
        # Should auto-assign TVD = MD when not provided
        self.assertEqual(well.true_vertical_depth, well.measured_depth)


class TestReservoirModel(unittest.TestCase):
    """Test integrated reservoir model."""
    
    def setUp(self):
        """Set up test fixtures."""
        if not RESERVOIR_MODULES_AVAILABLE:
            self.skipTest("Reservoir modules not available")
        
        self.reservoir = create_sandstone_reservoir(
            porosity=0.2,
            permeability=100,
            water_saturation=0.3,
            area=640,
            thickness=50
        )
        
        self.model = ReservoirModel(self.reservoir, "Test Model")
    
    def test_model_creation(self):
        """Test reservoir model creation."""
        self.assertEqual(self.model.model_name, "Test Model")
        self.assertIsInstance(self.model.properties, ReservoirProperties)
        self.assertIsInstance(self.model.material_balance, MaterialBalance)
        self.assertEqual(len(self.model.wells), 0)
    
    def test_well_addition(self):
        """Test adding wells to model."""
        well_data = WellData(
            well_id='PROD-001',
            x_coord=1000,
            y_coord=1000,
            measured_depth=2500,
            well_type='producer'
        )
        
        self.model.add_well(well_data)
        
        self.assertEqual(len(self.model.wells), 1)
        self.assertIn('PROD-001', self.model.wells)
    
    def test_integrated_analysis(self):
        """Test integrated analysis workflow."""
        # Add a well
        well_data = WellData(
            well_id='PROD-001',
            x_coord=1000,
            y_coord=1000,
            measured_depth=2500
        )
        self.model.add_well(well_data)
        
        try:
            results = self.model.run_integrated_analysis()
            
            self.assertIsInstance(results, dict)
            self.assertIn('model_name', results)
            self.assertIn('reservoir_properties', results)
            self.assertIn('wells', results)
            
            # Check reservoir properties
            res_props = results['reservoir_properties']
            self.assertIn('ooip', res_props)
            self.assertIn('pore_volume', res_props)
            
            # Check well summary
            wells = results['wells']
            self.assertEqual(wells['total_wells'], 1)
            
        except Exception as e:
            self.fail(f"Integrated analysis failed: {e}")
    
    def test_model_export(self):
        """Test model summary export."""
        summary = self.model.export_summary()
        
        self.assertIsInstance(summary, dict)
        
        required_sections = ['model_info', 'reservoir_properties', 'reserves', 'wells']
        for section in required_sections:
            self.assertIn(section, summary)
        
        # Check model info
        model_info = summary['model_info']
        self.assertEqual(model_info['name'], "Test Model")
        self.assertIn('created', model_info)
        self.assertIn('last_updated', model_info)


class TestIntegrationWorkflows(unittest.TestCase):
    """Test end-to-end integration workflows."""
    
    def setUp(self):
        """Set up test fixtures."""
        if not RESERVOIR_MODULES_AVAILABLE:
            self.skipTest("Reservoir modules not available")
    
    def test_complete_reservoir_analysis_workflow(self):
        """Test complete reservoir analysis from properties to forecasting."""
        try:
            # 1. Create reservoir properties
            reservoir = create_sandstone_reservoir(
                porosity=0.18,
                permeability=75,
                water_saturation=0.35,
                area=320,
                thickness=60,
                pressure=2800
            )
            
            # 2. Create reservoir model
            model = ReservoirModel(reservoir, "Integration Test Model")
            
            # 3. Add wells
            for i in range(3):
                well = WellData(
                    well_id=f'WELL-{i+1:03d}',
                    x_coord=1000 + i * 1000,
                    y_coord=1000,
                    measured_depth=2500
                )
                model.add_well(well)
            
            # 4. Add production data
            start_date = datetime(2020, 1, 1)
            production_data = []
            
            for i in range(100):  # 100 days of production
                date = start_date + timedelta(days=i)
                oil_rate = 50 * np.exp(-i / 200)
                
                prod_data = ProductionData(
                    well_id='WELL-001',
                    date=date,
                    oil_rate=oil_rate,
                    gas_rate=oil_rate * 600
                )
                production_data.append(prod_data)
            
            model.material_balance.add_production_data(production_data)
            
            # 5. Run integrated analysis
            results = model.run_integrated_analysis()
            
            # Validate results
            self.assertIsInstance(results, dict)
            self.assertEqual(results['wells']['total_wells'], 3)
            self.assertGreater(results['reservoir_properties']['ooip'], 0)
            
            # 6. Run simulation
            sim_results = model.simulation.run_depletion_simulation(
                production_rate=50,
                simulation_days=365
            )
            
            self.assertIsInstance(sim_results, dict)
            self.assertIn('pressure_psi', sim_results)
            self.assertIn('cumulative_production_stb', sim_results)
            
            # 7. Test forecasting
            forecast = ProductionForecast()
            forecast.add_production_data(production_data)
            
            decline_params = forecast.arps_decline_analysis()
            forecast_results = forecast.forecast_production(forecast_days=1000)
            
            self.assertGreater(forecast_results['eur_stb'], 0)
            
            print("\n=== Integration Test Results ===")
            print(f"Model: {results['model_name']}")
            print(f"Wells: {results['wells']['total_wells']}")
            print(f"OOIP: {results['reservoir_properties']['ooip']:,.0f} STB")
            print(f"Final Pressure: {sim_results['final_pressure']:.0f} psi")
            print(f"EUR Forecast: {forecast_results['eur_stb']:,.0f} STB")
            print(f"Recovery Factor: {sim_results['recovery_factor']:.1%}")
            
        except Exception as e:
            self.fail(f"Integration workflow failed: {e}")


def run_performance_tests():
    """Run performance tests for large datasets."""
    if not RESERVOIR_MODULES_AVAILABLE:
        print("Skipping performance tests - reservoir modules not available")
        return
    
    print("\n=== Performance Tests ===")
    
    import time
    
    # Test large log dataset processing
    start_time = time.time()
    
    # Create large synthetic dataset
    depths = np.arange(1000, 3000, 0.1)  # 20,000 points
    n_points = len(depths)
    
    np.random.seed(42)
    gr_values = 50 + 20 * np.sin(depths / 100) + 10 * np.random.normal(0, 1, n_points)
    rt_values = 20 * np.exp((depths - 1000) / 2000) + 5 * np.random.normal(0, 1, n_points)
    rhob_values = 2.3 + 0.2 * np.sin(depths / 150) + 0.05 * np.random.normal(0, 1, n_points)
    nphi_values = 0.18 + 0.08 * np.sin(depths / 120) + 0.02 * np.random.normal(0, 1, n_points)
    
    # Create log analysis
    log_analysis = LogAnalysis('PERF-TEST')
    log_analysis.add_curve(LogCurve('GR', depths, gr_values, 'API'))
    log_analysis.add_curve(LogCurve('RT', depths, rt_values, 'ohm-m'))
    log_analysis.add_curve(LogCurve('RHOB', depths, rhob_values, 'g/cm3'))
    log_analysis.add_curve(LogCurve('NPHI', depths, nphi_values, 'fraction'))
    
    # Test calculations
    try:
        facies = log_analysis.calculate_facies_from_logs(n_clusters=5)
        porosity = log_analysis.calculate_porosity_from_logs()
        sw = log_analysis.calculate_water_saturation()
        net_pay = log_analysis.calculate_net_pay()
        
        elapsed_time = time.time() - start_time
        
        print(f"Large dataset processing ({n_points:,} points):")
        print(f"  Time: {elapsed_time:.2f} seconds")
        print(f"  Facies points: {len(facies):,}")
        print(f"  Porosity points: {len(porosity):,}")
        print(f"  Net thickness: {net_pay['net_thickness']:.1f} ft")
        
    except Exception as e:
        print(f"Performance test failed: {e}")


def main():
    """Run all tests."""
    print("Reservoir Analysis Test Framework")
    print("=================================\n")
    
    if not RESERVOIR_MODULES_AVAILABLE:
        print("ERROR: Reservoir modules not available for testing.")
        print("Please ensure the digitalmodel.reservoir package is properly installed.")
        return
    
    # Create test suite
    test_suite = unittest.TestSuite()
    
    # Add test cases
    test_classes = [
        TestRockProperties,
        TestFluidProperties,
        TestReservoirProperties,
        TestUtilityFunctions,
        TestLogAnalysis,
        TestMaterialBalance,
        TestProductionForecast,
        TestWellData,
        TestReservoirModel,
        TestIntegrationWorkflows
    ]
    
    for test_class in test_classes:
        tests = unittest.TestLoader().loadTestsFromTestCase(test_class)
        test_suite.addTests(tests)
    
    # Run tests
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(test_suite)
    
    # Run performance tests
    run_performance_tests()
    
    # Summary
    print(f"\n=== Test Summary ===")
    print(f"Tests run: {result.testsRun}")
    print(f"Failures: {len(result.failures)}")
    print(f"Errors: {len(result.errors)}")
    
    if result.failures:
        print("\nFailures:")
        for test, traceback in result.failures:
            print(f"  {test}: {traceback.split(chr(10))[-2]}")
    
    if result.errors:
        print("\nErrors:")
        for test, traceback in result.errors:
            print(f"  {test}: {traceback.split(chr(10))[-2]}")
    
    success_rate = (result.testsRun - len(result.failures) - len(result.errors)) / result.testsRun * 100
    print(f"\nSuccess rate: {success_rate:.1f}%")
    
    return result.wasSuccessful()


if __name__ == "__main__":
    success = main()
    exit(0 if success else 1)
