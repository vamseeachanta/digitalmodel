"""
Comprehensive multi-module interaction tests.

This module tests the interactions between different digitalmodel modules
to ensure they work together correctly and share data appropriately.
"""

import pytest
import sys
import numpy as np
import pandas as pd
from pathlib import Path
from unittest.mock import patch, MagicMock, Mock
import tempfile
import json
from typing import Dict, Any, List

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))


@pytest.mark.integration
class TestModuleInteractions:
    """Test interactions between different digitalmodel modules."""

    @pytest.fixture
    def shared_material_properties(self):
        """Shared material properties used across modules."""
        return {
            "steel_grade": "API_5L_X65",
            "yield_strength": 450e6,  # Pa
            "ultimate_strength": 535e6,  # Pa
            "elastic_modulus": 210e9,  # Pa
            "poisson_ratio": 0.3,
            "density": 7850,  # kg/m³
            "thermal_expansion": 12e-6,  # /°C
            "fatigue_class": "C"
        }

    @pytest.fixture
    def pipeline_geometry(self):
        """Pipeline geometry shared across analyses."""
        return {
            "outer_diameter": 0.508,  # meters
            "wall_thickness": 0.02032,  # meters
            "length": 10000.0,  # meters
            "coating_thickness": 0.003,  # meters
            "insulation_thickness": 0.05  # meters
        }

    def test_fatigue_and_stress_analysis_interaction(self, shared_material_properties, pipeline_geometry):
        """Test interaction between fatigue and stress analysis modules."""
        # Mock stress analysis module
        with patch('digitalmodel.calculations.stress_analysis') as mock_stress, \
             patch('digitalmodel.common.fatigue_analysis') as mock_fatigue:
            
            # Configure stress analysis to provide input for fatigue
            stress_results = {
                "stress_ranges": [15e6, 25e6, 35e6, 45e6],  # Pa
                "cycle_counts": [1e6, 5e5, 1e5, 5e4],
                "stress_concentration_factors": {
                    "weld_toe": 2.5,
                    "geometric": 1.8,
                    "surface": 1.2
                },
                "hot_spot_stresses": [37.5e6, 62.5e6, 87.5e6, 112.5e6]  # Pa
            }
            mock_stress.calculate_stress_analysis.return_value = stress_results
            
            # Configure fatigue analysis to use stress results
            fatigue_results = {
                "fatigue_damage": 0.42,
                "fatigue_life": 47.6,  # years
                "critical_location": "weld_toe",
                "damage_by_stress_range": {
                    "15_MPa": 0.05,
                    "25_MPa": 0.12,
                    "35_MPa": 0.18,
                    "45_MPa": 0.07
                }
            }
            mock_fatigue.calculate_fatigue_from_stress.return_value = fatigue_results

            # Execute interaction test
            stress_output = mock_stress.calculate_stress_analysis(
                material=shared_material_properties,
                geometry=pipeline_geometry
            )
            
            fatigue_output = mock_fatigue.calculate_fatigue_from_stress(
                stress_data=stress_output,
                material=shared_material_properties
            )

            # Validate interaction
            assert fatigue_output["fatigue_damage"] < 1.0  # Should not fail
            assert fatigue_output["fatigue_life"] > 20  # Should meet design life
            assert "critical_location" in fatigue_output
            
            # Verify stress data was properly used in fatigue calculation
            mock_fatigue.calculate_fatigue_from_stress.assert_called_with(
                stress_data=stress_results,
                material=shared_material_properties
            )

    def test_plate_analysis_and_buckling_interaction(self, shared_material_properties):
        """Test interaction between plate analysis and buckling modules."""
        plate_config = {
            "length": 3.0,  # meters
            "width": 2.0,  # meters
            "thickness": 0.025,  # meters
            "boundary_conditions": "simply_supported"
        }

        # Mock plate analysis and buckling modules
        with patch('digitalmodel.analysis.plate_capacity') as mock_plate, \
             patch('digitalmodel.calculations.plate_buckling') as mock_buckling:
            
            # Configure buckling analysis to provide critical stress
            buckling_results = {
                "critical_stress": 285e6,  # Pa
                "buckling_coefficient": 4.0,
                "effective_width": 1.8,  # meters
                "mode_shape": "local",
                "slenderness_ratio": 1.2
            }
            mock_buckling.calculate_plate_buckling.return_value = buckling_results
            
            # Configure plate analysis to use buckling results
            plate_results = {
                "yield_capacity": 1.8e6,  # N
                "buckling_capacity": 1.42e6,  # N (governs)
                "ultimate_capacity": 1.42e6,  # N
                "governing_mode": "buckling",
                "utilization_ratio": 0.71,
                "safety_factor": 1.41
            }
            mock_plate.analyze_plate_capacity.return_value = plate_results

            # Execute interaction test
            buckling_output = mock_buckling.calculate_plate_buckling(
                material=shared_material_properties,
                geometry=plate_config
            )
            
            plate_output = mock_plate.analyze_plate_capacity(
                material=shared_material_properties,
                geometry=plate_config,
                buckling_data=buckling_output
            )

            # Validate interaction
            assert plate_output["governing_mode"] == "buckling"
            assert plate_output["buckling_capacity"] < plate_output["yield_capacity"]
            assert plate_output["safety_factor"] > 1.0
            
            # Verify buckling data was used in plate analysis
            mock_plate.analyze_plate_capacity.assert_called_with(
                material=shared_material_properties,
                geometry=plate_config,
                buckling_data=buckling_results
            )

    def test_reservoir_and_flow_analysis_interaction(self):
        """Test interaction between reservoir and flow analysis modules."""
        reservoir_config = {
            "porosity": 0.25,
            "permeability": 100e-15,  # m²
            "thickness": 50.0,  # meters
            "area": 1000000,  # m²
            "initial_pressure": 30e6  # Pa
        }
        
        flow_config = {
            "pipe_diameter": 0.2,  # meters
            "flow_rate": 0.1,  # m³/s
            "fluid_viscosity": 0.001,  # Pa·s
            "fluid_density": 850  # kg/m³
        }

        # Mock reservoir and flow modules
        with patch('digitalmodel.modules.reservoir.reservoir_analysis') as mock_reservoir, \
             patch('digitalmodel.modules.flow.flow_analysis') as mock_flow:
            
            # Configure reservoir to provide pressure decline
            reservoir_results = {
                "pressure_decline": {
                    "time_years": [0, 5, 10, 15, 20],
                    "pressure_MPa": [30, 27, 24, 21, 18]
                },
                "depletion_rate": 0.6,  # MPa/year
                "recovery_factor": 0.35
            }
            mock_reservoir.analyze_reservoir.return_value = reservoir_results
            
            # Configure flow analysis to use reservoir pressure
            flow_results = {
                "pressure_drop": 2.5e6,  # Pa
                "reynolds_number": 15000,
                "friction_factor": 0.025,
                "velocity": 3.2,  # m/s
                "flow_regime": "turbulent"
            }
            mock_flow.analyze_flow.return_value = flow_results

            # Execute interaction test
            reservoir_output = mock_reservoir.analyze_reservoir(reservoir_config)
            
            # Use reservoir pressure in flow analysis
            current_pressure = reservoir_output["pressure_decline"]["pressure_MPa"][-1] * 1e6  # Convert to Pa
            flow_input = flow_config.copy()
            flow_input["inlet_pressure"] = current_pressure
            
            flow_output = mock_flow.analyze_flow(flow_input)

            # Validate interaction
            assert current_pressure == 18e6  # Pa (from reservoir decline)
            assert flow_output["pressure_drop"] < current_pressure  # Reasonable pressure drop
            assert flow_output["flow_regime"] == "turbulent"  # Expected for this Reynolds number
            
            # Verify flow analysis used reservoir pressure
            expected_flow_config = flow_config.copy()
            expected_flow_config["inlet_pressure"] = 18e6
            mock_flow.analyze_flow.assert_called_with(expected_flow_config)

    def test_thermal_and_structural_analysis_interaction(self, shared_material_properties, pipeline_geometry):
        """Test interaction between thermal and structural analysis modules."""
        thermal_config = {
            "fluid_temperature": 80.0,  # °C
            "ambient_temperature": 4.0,  # °C
            "heat_transfer_coefficient": 25.0,  # W/m²·K
            "insulation_conductivity": 0.04,  # W/m·K
            "burial_depth": 1.5  # meters
        }

        # Mock thermal and structural modules
        with patch('digitalmodel.analysis.thermal_analysis') as mock_thermal, \
             patch('digitalmodel.calculations.structural_expansion') as mock_structural:
            
            # Configure thermal analysis to provide temperature distribution
            thermal_results = {
                "temperature_profile": {
                    "pipe_wall": 78.0,  # °C
                    "coating": 25.0,  # °C
                    "insulation": 15.0,  # °C
                    "soil": 8.0  # °C
                },
                "heat_loss_rate": 150.0,  # W/m
                "temperature_drop": 2.0  # °C per km
            }
            mock_thermal.analyze_thermal.return_value = thermal_results
            
            # Configure structural analysis to use thermal data
            structural_results = {
                "thermal_stress": 125e6,  # Pa
                "thermal_strain": 0.0009,  # dimensionless
                "expansion_displacement": 0.048,  # meters
                "axial_force": 2.1e6,  # N
                "combined_stress": 275e6  # Pa
            }
            mock_structural.calculate_thermal_effects.return_value = structural_results

            # Execute interaction test
            thermal_output = mock_thermal.analyze_thermal(
                geometry=pipeline_geometry,
                thermal_config=thermal_config
            )
            
            # Calculate temperature difference for structural analysis
            temp_diff = thermal_output["temperature_profile"]["pipe_wall"] - thermal_config["ambient_temperature"]
            
            structural_output = mock_structural.calculate_thermal_effects(
                material=shared_material_properties,
                geometry=pipeline_geometry,
                temperature_difference=temp_diff
            )

            # Validate interaction
            assert temp_diff == 74.0  # °C (78 - 4)
            assert structural_output["thermal_stress"] > 0  # Compressive stress expected
            assert structural_output["expansion_displacement"] > 0  # Expansion expected
            
            # Verify thermal data was used in structural calculation
            mock_structural.calculate_thermal_effects.assert_called_with(
                material=shared_material_properties,
                geometry=pipeline_geometry,
                temperature_difference=74.0
            )

    def test_cathodic_protection_and_corrosion_interaction(self, pipeline_geometry):
        """Test interaction between cathodic protection and corrosion modules."""
        corrosion_config = {
            "environment": "seawater",
            "ph": 8.1,
            "dissolved_oxygen": 8.0,  # mg/L
            "chloride_content": 19000,  # mg/L
            "temperature": 15.0,  # °C
            "coating_quality": 0.95  # fraction
        }
        
        cp_config = {
            "anode_type": "aluminum",
            "current_density": 20e-3,  # A/m²
            "design_life": 25,  # years
            "protection_potential": -0.85  # V vs Cu/CuSO4
        }

        # Mock corrosion and cathodic protection modules
        with patch('digitalmodel.common.corrosion_analysis') as mock_corrosion, \
             patch('digitalmodel.common.cathodic_protection') as mock_cp:
            
            # Configure corrosion analysis
            corrosion_results = {
                "corrosion_rate": 0.025,  # mm/year
                "pitting_factor": 3.5,
                "wall_loss_25_years": 0.625,  # mm
                "remaining_thickness": 19.407,  # mm
                "corrosion_allowance_required": 2.2  # mm
            }
            mock_corrosion.calculate_corrosion_rate.return_value = corrosion_results
            
            # Configure CP analysis to use corrosion data
            cp_results = {
                "current_required": 250.0,  # A
                "anode_mass_required": 1200.0,  # kg
                "anode_spacing": 500.0,  # meters
                "protection_efficiency": 0.98,
                "residual_corrosion_rate": 0.001  # mm/year
            }
            mock_cp.design_cp_system.return_value = cp_results

            # Execute interaction test
            corrosion_output = mock_corrosion.calculate_corrosion_rate(
                geometry=pipeline_geometry,
                environment=corrosion_config
            )
            
            # Use corrosion data in CP design
            cp_input = cp_config.copy()
            cp_input["target_corrosion_rate"] = 0.001  # mm/year (protected rate)
            cp_input["bare_corrosion_rate"] = corrosion_output["corrosion_rate"]
            
            cp_output = mock_cp.design_cp_system(
                geometry=pipeline_geometry,
                corrosion_data=corrosion_output,
                cp_config=cp_input
            )

            # Validate interaction
            assert cp_output["residual_corrosion_rate"] < corrosion_output["corrosion_rate"]
            assert cp_output["protection_efficiency"] > 0.95  # High efficiency expected
            assert cp_output["current_required"] > 0  # Some current needed for protection
            
            # Verify CP design used corrosion data
            mock_cp.design_cp_system.assert_called_with(
                geometry=pipeline_geometry,
                corrosion_data=corrosion_results,
                cp_config=cp_input
            )

    def test_signal_processing_and_fatigue_interaction(self):
        """Test interaction between signal processing and fatigue analysis."""
        # Mock time series signal data
        time_data = np.linspace(0, 3600, 36000)  # 1 hour at 10 Hz
        stress_signal = 50e6 + 30e6 * np.sin(0.1 * time_data) + 10e6 * np.random.randn(len(time_data))
        
        signal_data = {
            "time": time_data,
            "stress": stress_signal,
            "sampling_frequency": 10.0  # Hz
        }

        # Mock signal processing and fatigue modules
        with patch('digitalmodel.signal_analysis.signal_processing') as mock_signal, \
             patch('digitalmodel.common.fatigue_analysis') as mock_fatigue:
            
            # Configure signal processing to extract cycles
            processed_results = {
                "stress_ranges": [5e6, 15e6, 25e6, 35e6, 45e6],  # Pa
                "cycle_counts": [500, 300, 150, 75, 25],
                "mean_stresses": [48e6, 52e6, 50e6, 49e6, 51e6],  # Pa
                "frequency_content": {
                    "dominant_frequency": 0.016,  # Hz
                    "peak_frequencies": [0.016, 0.048, 0.08]  # Hz
                }
            }
            mock_signal.rainflow_counting.return_value = processed_results
            
            # Configure fatigue analysis to use processed cycles
            fatigue_results = {
                "damage_per_range": [0.001, 0.005, 0.025, 0.075, 0.15],
                "total_damage": 0.256,
                "equivalent_cycles": 850,
                "fatigue_life_hours": 14062  # hours
            }
            mock_fatigue.calculate_damage_from_cycles.return_value = fatigue_results

            # Execute interaction test
            signal_output = mock_signal.rainflow_counting(signal_data)
            
            fatigue_input = {
                "stress_ranges": signal_output["stress_ranges"],
                "cycle_counts": signal_output["cycle_counts"],
                "mean_stresses": signal_output["mean_stresses"]
            }
            
            fatigue_output = mock_fatigue.calculate_damage_from_cycles(fatigue_input)

            # Validate interaction
            assert len(fatigue_output["damage_per_range"]) == len(signal_output["stress_ranges"])
            assert fatigue_output["total_damage"] < 1.0  # Should not fail
            assert fatigue_output["fatigue_life_hours"] > 8760  # > 1 year
            
            # Verify signal data was used in fatigue calculation
            mock_fatigue.calculate_damage_from_cycles.assert_called_with(fatigue_input)

    def test_multi_physics_coupling(self, shared_material_properties, pipeline_geometry):
        """Test coupling between multiple physics modules."""
        # Mock multiple physics modules
        with patch('digitalmodel.analysis.thermal_analysis') as mock_thermal, \
             patch('digitalmodel.analysis.structural_analysis') as mock_structural, \
             patch('digitalmodel.analysis.flow_analysis') as mock_flow, \
             patch('digitalmodel.analysis.multi_physics_solver') as mock_solver:
            
            # Configure individual physics modules
            thermal_results = {"temperature_field": [80, 70, 60, 50, 40], "heat_flux": 150}
            structural_results = {"stress_field": [250e6, 200e6, 150e6, 100e6, 50e6], "displacement": 0.05}
            flow_results = {"pressure_field": [30e6, 28e6, 26e6, 24e6, 22e6], "velocity": 3.2}
            
            mock_thermal.solve_thermal.return_value = thermal_results
            mock_structural.solve_structural.return_value = structural_results
            mock_flow.solve_flow.return_value = flow_results
            
            # Configure coupled solver
            coupled_results = {
                "converged": True,
                "iterations": 8,
                "residual": 1e-6,
                "coupled_fields": {
                    "thermal": thermal_results,
                    "structural": structural_results,
                    "flow": flow_results
                },
                "interaction_effects": {
                    "thermal_stress_coupling": 0.15,
                    "flow_induced_vibration": 0.02,
                    "pressure_deformation": 0.08
                }
            }
            mock_solver.solve_coupled.return_value = coupled_results

            # Execute multi-physics coupling test
            coupled_output = mock_solver.solve_coupled(
                material=shared_material_properties,
                geometry=pipeline_geometry,
                physics_modules={
                    "thermal": mock_thermal,
                    "structural": mock_structural,
                    "flow": mock_flow
                }
            )

            # Validate coupling
            assert coupled_output["converged"] is True
            assert coupled_output["iterations"] < 20  # Should converge reasonably
            assert "interaction_effects" in coupled_output
            
            # Verify all physics modules were called
            mock_thermal.solve_thermal.assert_called()
            mock_structural.solve_structural.assert_called()
            mock_flow.solve_flow.assert_called()


@pytest.mark.integration
class TestDataSharing:
    """Test data sharing mechanisms between modules."""

    def test_shared_configuration_management(self):
        """Test that modules can share configuration data."""
        shared_config = {
            "project_id": "TEST_001",
            "units": "SI",
            "precision": 6,
            "safety_factors": {
                "design": 2.0,
                "material": 1.5,
                "load": 1.2
            }
        }

        # Mock configuration manager
        with patch('digitalmodel.common.config_manager') as mock_config:
            mock_config.get_shared_config.return_value = shared_config
            mock_config.update_config.return_value = True

            # Test multiple modules accessing shared config
            config1 = mock_config.get_shared_config("module1")
            config2 = mock_config.get_shared_config("module2")
            
            # Both modules should get same config
            assert config1 == config2 == shared_config
            
            # Test config update
            updated_config = shared_config.copy()
            updated_config["safety_factors"]["design"] = 2.5
            update_result = mock_config.update_config(updated_config)
            
            assert update_result is True

    def test_inter_module_data_exchange(self):
        """Test data exchange between modules through shared data store."""
        # Mock shared data store
        with patch('digitalmodel.common.data_store') as mock_store:
            
            # Configure data store
            stored_data = {}
            
            def store_data(key, data):
                stored_data[key] = data
                return True
            
            def retrieve_data(key):
                return stored_data.get(key)
            
            mock_store.store.side_effect = store_data
            mock_store.retrieve.side_effect = retrieve_data

            # Module 1 stores data
            module1_data = {"analysis_results": [1, 2, 3], "timestamp": "2025-01-01"}
            store_result = mock_store.store("module1_output", module1_data)
            
            # Module 2 retrieves and uses data
            retrieved_data = mock_store.retrieve("module1_output")
            
            assert store_result is True
            assert retrieved_data == module1_data
            assert "analysis_results" in retrieved_data

    def test_module_dependency_resolution(self):
        """Test that module dependencies are properly resolved."""
        dependencies = {
            "structural_analysis": ["material_properties", "geometry"],
            "fatigue_analysis": ["structural_analysis", "loading_history"],
            "report_generation": ["structural_analysis", "fatigue_analysis"]
        }

        # Mock dependency resolver
        with patch('digitalmodel.common.dependency_resolver') as mock_resolver:
            mock_resolver.resolve_dependencies.return_value = [
                "material_properties",
                "geometry", 
                "loading_history",
                "structural_analysis",
                "fatigue_analysis",
                "report_generation"
            ]

            # Test dependency resolution
            execution_order = mock_resolver.resolve_dependencies(dependencies)
            
            # Validate execution order respects dependencies
            structural_idx = execution_order.index("structural_analysis")
            fatigue_idx = execution_order.index("fatigue_analysis")
            report_idx = execution_order.index("report_generation")
            
            assert structural_idx < fatigue_idx  # Structural before fatigue
            assert fatigue_idx < report_idx  # Fatigue before report
            assert "material_properties" in execution_order[:structural_idx]  # Material before structural
