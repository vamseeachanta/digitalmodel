"""
Realistic engineering test data sets for integration testing.

This module provides comprehensive test data that represents real-world
engineering scenarios for pipelines, plates, fatigue, and reservoir analysis.
"""

import numpy as np
import pandas as pd
from typing import Dict, Any, List
import json
from pathlib import Path


class EngineeringTestDataGenerator:
    """Generate realistic engineering test data for integration tests."""
    
    def __init__(self, seed: int = 42):
        """Initialize with random seed for reproducible data."""
        np.random.seed(seed)
        self.seed = seed
    
    def generate_pipeline_scenarios(self) -> Dict[str, Any]:
        """Generate realistic pipeline analysis scenarios."""
        return {
            "north_sea_export": {
                "description": "North Sea oil export pipeline",
                "environment": {
                    "water_depth": 120.0,
                    "seabed_type": "soft_clay",
                    "wave_height_100yr": 12.5,
                    "wave_period_100yr": 14.0,
                    "current_velocity_surface": 1.8,
                    "current_velocity_bottom": 0.4,
                    "temperature_seabed": 6.0,
                    "soil_resistance": 15.0  # kPa
                },
                "pipeline": {
                    "outer_diameter": 0.762,  # 30 inch
                    "wall_thickness": 0.0254,  # 1 inch
                    "length": 85000.0,  # 85 km
                    "material_grade": "API_5L_X65",
                    "yield_strength": 450e6,
                    "ultimate_strength": 535e6,
                    "elastic_modulus": 207e9,
                    "coating_type": "concrete",
                    "coating_thickness": 0.075
                },
                "operating_conditions": {
                    "design_pressure": 20e6,
                    "operating_pressure": 16e6,
                    "fluid_temperature": 85.0,
                    "fluid_density": 850.0
                },
                "critical_parameters": {
                    "lateral_buckling_force": 1.8e6,
                    "upheaval_buckling_length": 2800.0,
                    "free_span_allowable": 45.0,
                    "fatigue_life_target": 25.0
                }
            },
            
            "gulf_of_mexico_gas": {
                "description": "Gulf of Mexico gas transmission pipeline",
                "environment": {
                    "water_depth": 1800.0,
                    "seabed_type": "deep_water_mud",
                    "wave_height_100yr": 15.2,
                    "wave_period_100yr": 16.5,
                    "current_velocity_surface": 2.1,
                    "current_velocity_bottom": 0.2,
                    "temperature_seabed": 4.5,
                    "soil_resistance": 8.0
                },
                "pipeline": {
                    "outer_diameter": 0.610,  # 24 inch
                    "wall_thickness": 0.0318,  # 1.25 inch
                    "length": 125000.0,  # 125 km
                    "material_grade": "API_5L_X70",
                    "yield_strength": 485e6,
                    "ultimate_strength": 570e6,
                    "elastic_modulus": 207e9,
                    "coating_type": "3LPE",
                    "coating_thickness": 0.003
                },
                "operating_conditions": {
                    "design_pressure": 30e6,
                    "operating_pressure": 25e6,
                    "fluid_temperature": 45.0,
                    "fluid_density": 180.0  # Gas density
                },
                "critical_parameters": {
                    "lateral_buckling_force": 2.5e6,
                    "upheaval_buckling_length": 3200.0,
                    "free_span_allowable": 60.0,
                    "fatigue_life_target": 30.0
                }
            },
            
            "shallow_water_flow": {
                "description": "Shallow water flowline",
                "environment": {
                    "water_depth": 45.0,
                    "seabed_type": "sand",
                    "wave_height_100yr": 8.5,
                    "wave_period_100yr": 10.0,
                    "current_velocity_surface": 1.2,
                    "current_velocity_bottom": 0.8,
                    "temperature_seabed": 12.0,
                    "soil_resistance": 25.0
                },
                "pipeline": {
                    "outer_diameter": 0.324,  # 12.75 inch
                    "wall_thickness": 0.0159,  # 0.625 inch
                    "length": 15000.0,  # 15 km
                    "material_grade": "API_5L_X60",
                    "yield_strength": 415e6,
                    "ultimate_strength": 520e6,
                    "elastic_modulus": 207e9,
                    "coating_type": "FBE",
                    "coating_thickness": 0.0005
                },
                "operating_conditions": {
                    "design_pressure": 12e6,
                    "operating_pressure": 8e6,
                    "fluid_temperature": 65.0,
                    "fluid_density": 900.0
                },
                "critical_parameters": {
                    "lateral_buckling_force": 0.8e6,
                    "upheaval_buckling_length": 1200.0,
                    "free_span_allowable": 25.0,
                    "fatigue_life_target": 20.0
                }
            }
        }
    
    def generate_plate_scenarios(self) -> Dict[str, Any]:
        """Generate realistic plate analysis scenarios."""
        return {
            "fpso_deck_plating": {
                "description": "FPSO deck plating under equipment loads",
                "geometry": {
                    "length": 6.0,
                    "width": 4.0,
                    "thickness": 0.025,
                    "stiffener_spacing": 0.8,
                    "stiffener_type": "HP_section",
                    "plate_configuration": "stiffened_panel"
                },
                "material": {
                    "grade": "AH36",
                    "yield_strength": 355e6,
                    "ultimate_strength": 490e6,
                    "elastic_modulus": 206e9,
                    "poisson_ratio": 0.3,
                    "density": 7850
                },
                "loading": {
                    "equipment_load": 150000,  # N/m²
                    "deck_live_load": 5000,    # N/m²
                    "snow_load": 1200,         # N/m²
                    "wind_load": 800,          # N/m²
                    "total_design_load": 180000,
                    "load_factors": {
                        "dead": 1.2,
                        "live": 1.6,
                        "environmental": 1.3
                    }
                },
                "boundary_conditions": "fixed_edges",
                "design_criteria": {
                    "deflection_limit": "L/250",
                    "stress_utilization": 0.8,
                    "buckling_factor": 2.0
                }
            },
            
            "ship_bottom_plating": {
                "description": "Ship bottom plating under hydrostatic pressure",
                "geometry": {
                    "length": 8.0,
                    "width": 3.5,
                    "thickness": 0.018,
                    "stiffener_spacing": 0.7,
                    "stiffener_type": "T_bar",
                    "plate_configuration": "grillage"
                },
                "material": {
                    "grade": "AH32",
                    "yield_strength": 315e6,
                    "ultimate_strength": 440e6,
                    "elastic_modulus": 206e9,
                    "poisson_ratio": 0.3,
                    "density": 7850
                },
                "loading": {
                    "hydrostatic_pressure": 98000,  # N/m² (10m head)
                    "slamming_pressure": 245000,    # N/m²
                    "cargo_pressure": 45000,        # N/m²
                    "total_design_pressure": 290000,
                    "load_factors": {
                        "hydrostatic": 1.0,
                        "slamming": 1.3,
                        "cargo": 1.2
                    }
                },
                "boundary_conditions": "simply_supported",
                "design_criteria": {
                    "deflection_limit": "L/300",
                    "stress_utilization": 0.75,
                    "buckling_factor": 2.5
                }
            },
            
            "offshore_platform_deck": {
                "description": "Offshore platform deck plating",
                "geometry": {
                    "length": 12.0,
                    "width": 8.0,
                    "thickness": 0.032,
                    "stiffener_spacing": 1.0,
                    "stiffener_type": "I_beam",
                    "plate_configuration": "orthotropic_deck"
                },
                "material": {
                    "grade": "S355",
                    "yield_strength": 355e6,
                    "ultimate_strength": 510e6,
                    "elastic_modulus": 210e9,
                    "poisson_ratio": 0.3,
                    "density": 7850
                },
                "loading": {
                    "dead_load": 8000,        # N/m²
                    "live_load": 12000,       # N/m²
                    "crane_load": 250000,     # N concentrated
                    "wind_load": 1500,       # N/m²
                    "seismic_load": 0.15,     # g acceleration
                    "load_factors": {
                        "dead": 1.2,
                        "live": 1.6,
                        "crane": 1.5,
                        "environmental": 1.3
                    }
                },
                "boundary_conditions": "elastic_supports",
                "design_criteria": {
                    "deflection_limit": "L/400",
                    "stress_utilization": 0.85,
                    "buckling_factor": 1.8
                }
            }
        }
    
    def generate_fatigue_scenarios(self) -> Dict[str, Any]:
        """Generate realistic fatigue analysis scenarios."""
        return {
            "offshore_jacket_joint": {
                "description": "Offshore jacket K-joint fatigue analysis",
                "geometry": {
                    "joint_type": "tubular_K_joint",
                    "chord_diameter": 1.5,
                    "chord_thickness": 0.05,
                    "brace1_diameter": 0.8,
                    "brace1_thickness": 0.032,
                    "brace2_diameter": 0.7,
                    "brace2_thickness": 0.028,
                    "angle_between_braces": 60.0,
                    "chord_stress_factor": 1.0
                },
                "material": {
                    "grade": "S355",
                    "yield_strength": 355e6,
                    "fatigue_class": "T",
                    "thickness_effect": True,
                    "reference_thickness": 0.025
                },
                "loading": {
                    "stress_history_type": "wave_induced",
                    "significant_stress_ranges": [5, 10, 15, 20, 25, 30, 35, 40, 45, 50],  # MPa
                    "annual_cycles": [5e6, 2e6, 8e5, 4e5, 2e5, 1e5, 5e4, 2e4, 1e4, 5e3],
                    "mean_stress": 25e6,  # Pa
                    "stress_concentration_factor": 3.2,
                    "design_life": 25  # years
                },
                "environment": {
                    "condition": "seawater_with_cathodic_protection",
                    "temperature": 15.0,
                    "ph": 8.1,
                    "dissolved_oxygen": 8.0,
                    "protection_potential": -0.85
                },
                "analysis_parameters": {
                    "s_n_curve": "DNV_T_curve",
                    "safety_factor": 3.0,
                    "inspection_intervals": [5, 10, 15, 20],
                    "acceptance_criteria": "DNV_GL_fatigue"
                }
            },
            
            "ship_hatch_corner": {
                "description": "Ship cargo hatch corner fatigue analysis",
                "geometry": {
                    "joint_type": "welded_corner",
                    "plate_thickness": 0.025,
                    "stiffener_thickness": 0.015,
                    "corner_radius": 0.05,
                    "weld_throat_thickness": 0.008,
                    "stress_concentration_geometry": "re_entrant_corner"
                },
                "material": {
                    "grade": "AH36",
                    "yield_strength": 355e6,
                    "fatigue_class": "F2",
                    "thickness_effect": True,
                    "reference_thickness": 0.025
                },
                "loading": {
                    "stress_history_type": "cargo_cycling",
                    "stress_ranges": [8, 12, 16, 20, 25, 30, 35, 40],  # MPa
                    "cycle_counts_per_voyage": [50, 30, 20, 15, 10, 8, 5, 2],
                    "voyages_per_year": 24,
                    "mean_stress": 40e6,  # Pa
                    "stress_concentration_factor": 2.8,
                    "design_life": 20  # years
                },
                "environment": {
                    "condition": "seawater_splash_zone",
                    "temperature_range": [-10, 40],
                    "humidity": 85.0,
                    "salt_spray": True,
                    "coating_system": "two_pack_epoxy"
                },
                "analysis_parameters": {
                    "s_n_curve": "IIW_FAT_71",
                    "safety_factor": 2.0,
                    "inspection_intervals": [2.5, 5, 10, 15],
                    "acceptance_criteria": "Classification_society"
                }
            },
            
            "pipeline_girth_weld": {
                "description": "Subsea pipeline girth weld fatigue",
                "geometry": {
                    "joint_type": "circumferential_weld",
                    "pipe_diameter": 0.508,
                    "pipe_thickness": 0.025,
                    "weld_cap_height": 0.003,
                    "weld_toe_radius": 0.001,
                    "misalignment": 0.002,
                    "ovality": 0.01
                },
                "material": {
                    "grade": "API_5L_X65",
                    "yield_strength": 450e6,
                    "fatigue_class": "C",
                    "thickness_effect": True,
                    "reference_thickness": 0.025
                },
                "loading": {
                    "stress_history_type": "vortex_induced_vibration",
                    "stress_ranges": [2, 4, 6, 8, 10, 12, 15, 18, 20],  # MPa
                    "cycle_frequencies": [0.1, 0.2, 0.3, 0.5, 0.8, 1.2, 1.8, 2.5, 3.0],  # Hz
                    "annual_cycles": [3e7, 6e7, 9e7, 1.5e8, 2.5e8, 3.8e8, 5.7e8, 7.9e8, 9.5e8],
                    "mean_stress": 180e6,  # Pa (hoop stress)
                    "stress_concentration_factor": 1.8,
                    "design_life": 30  # years
                },
                "environment": {
                    "condition": "seawater_immersed",
                    "temperature": 4.0,
                    "depth": 150.0,
                    "cathodic_protection": True,
                    "marine_growth": True
                },
                "analysis_parameters": {
                    "s_n_curve": "DNV_C_curve",
                    "safety_factor": 10.0,
                    "inspection_intervals": [10, 20],
                    "acceptance_criteria": "DNV_submarine_pipeline"
                }
            }
        }
    
    def generate_reservoir_scenarios(self) -> Dict[str, Any]:
        """Generate realistic reservoir analysis scenarios."""
        return {
            "north_sea_oil_field": {
                "description": "North Sea oil reservoir depletion analysis",
                "reservoir_properties": {
                    "porosity": 0.22,
                    "permeability": 150e-15,  # m² (150 mD)
                    "thickness": 45.0,  # meters
                    "area": 25e6,  # m² (25 km²)
                    "depth": 3200.0,  # meters
                    "rock_compressibility": 4.5e-10,  # Pa⁻¹
                    "water_saturation": 0.25
                },
                "fluid_properties": {
                    "oil_density": 850.0,  # kg/m³
                    "oil_viscosity": 0.002,  # Pa·s (2 cP)
                    "oil_compressibility": 1.2e-9,  # Pa⁻¹
                    "gas_oil_ratio": 120.0,  # m³/m³
                    "formation_volume_factor": 1.35,
                    "bubble_point_pressure": 18e6  # Pa
                },
                "initial_conditions": {
                    "pressure": 32e6,  # Pa
                    "temperature": 85.0,  # °C
                    "oil_in_place": 125e6,  # m³
                    "water_drive": "weak",
                    "gas_cap": False
                },
                "production_scenario": {
                    "wells": {
                        "producers": 12,
                        "injectors": 8,
                        "well_spacing": 800.0  # meters
                    },
                    "rates": {
                        "oil_rate_per_well": 150.0,  # m³/day
                        "water_injection_rate": 200.0,  # m³/day
                        "target_recovery_factor": 0.45
                    },
                    "constraints": {
                        "minimum_pressure": 15e6,  # Pa
                        "maximum_water_cut": 0.85,
                        "economic_limit": 50.0  # m³/day oil
                    }
                }
            },
            
            "gulf_coast_gas_field": {
                "description": "Gulf Coast gas reservoir depletion",
                "reservoir_properties": {
                    "porosity": 0.18,
                    "permeability": 75e-15,  # m² (75 mD)
                    "thickness": 25.0,  # meters
                    "area": 15e6,  # m² (15 km²)
                    "depth": 2800.0,  # meters
                    "rock_compressibility": 3.8e-10,  # Pa⁻¹
                    "water_saturation": 0.35
                },
                "fluid_properties": {
                    "gas_density": 180.0,  # kg/m³
                    "gas_viscosity": 0.000015,  # Pa·s
                    "gas_compressibility": 8.5e-6,  # Pa⁻¹
                    "z_factor": 0.85,
                    "formation_volume_factor": 0.004,
                    "critical_pressure": 4.6e6  # Pa
                },
                "initial_conditions": {
                    "pressure": 28e6,  # Pa
                    "temperature": 75.0,  # °C
                    "gas_in_place": 8.5e9,  # m³
                    "water_drive": "none",
                    "condensate": True
                },
                "production_scenario": {
                    "wells": {
                        "producers": 8,
                        "injectors": 0,
                        "well_spacing": 1200.0  # meters
                    },
                    "rates": {
                        "gas_rate_per_well": 85000.0,  # m³/day
                        "target_recovery_factor": 0.75
                    },
                    "constraints": {
                        "minimum_pressure": 7e6,  # Pa
                        "maximum_drawdown": 10e6,  # Pa
                        "economic_limit": 15000.0  # m³/day gas
                    }
                }
            },
            
            "tight_oil_play": {
                "description": "Unconventional tight oil reservoir",
                "reservoir_properties": {
                    "porosity": 0.08,
                    "permeability": 0.5e-15,  # m² (0.5 mD - tight)
                    "thickness": 15.0,  # meters
                    "area": 50e6,  # m² (50 km²)
                    "depth": 2500.0,  # meters
                    "rock_compressibility": 6.2e-10,  # Pa⁻¹
                    "water_saturation": 0.15
                },
                "fluid_properties": {
                    "oil_density": 750.0,  # kg/m³ (light oil)
                    "oil_viscosity": 0.0008,  # Pa·s (0.8 cP)
                    "oil_compressibility": 1.8e-9,  # Pa⁻¹
                    "gas_oil_ratio": 250.0,  # m³/m³
                    "formation_volume_factor": 1.55,
                    "bubble_point_pressure": 22e6  # Pa
                },
                "initial_conditions": {
                    "pressure": 25e6,  # Pa
                    "temperature": 65.0,  # °C
                    "oil_in_place": 45e6,  # m³
                    "water_drive": "none",
                    "natural_fractures": True
                },
                "production_scenario": {
                    "wells": {
                        "horizontal_wells": 25,
                        "fracture_stages": 20,
                        "well_spacing": 400.0,  # meters
                        "lateral_length": 1500.0  # meters
                    },
                    "rates": {
                        "initial_oil_rate": 50.0,  # m³/day
                        "decline_rate": 0.65,  # annual
                        "target_recovery_factor": 0.12
                    },
                    "constraints": {
                        "minimum_pressure": 8e6,  # Pa
                        "economic_limit": 3.0,  # m³/day oil
                        "fracture_pressure": 30e6  # Pa
                    }
                }
            }
        }
    
    def generate_time_series_data(self, duration_hours: float = 24.0, sample_rate: float = 1.0) -> Dict[str, np.ndarray]:
        """Generate realistic time series data for various parameters."""
        n_points = int(duration_hours * 3600 * sample_rate)
        time = np.linspace(0, duration_hours * 3600, n_points)
        
        # Wave-induced stress time series
        wave_stress = (
            50e6 +  # Mean stress
            30e6 * np.sin(2 * np.pi * time / 12.0) +  # 12-second waves
            15e6 * np.sin(2 * np.pi * time / 8.5) +   # 8.5-second waves
            8e6 * np.sin(2 * np.pi * time / 6.2) +    # 6.2-second waves
            5e6 * np.random.randn(n_points)            # Random component
        )
        
        # Current-induced stress
        current_stress = (
            20e6 * np.sin(2 * np.pi * time / (6 * 3600)) +  # Semi-diurnal tide
            10e6 * np.sin(2 * np.pi * time / (12.4 * 3600)) +  # Diurnal tide
            3e6 * np.random.randn(n_points)
        )
        
        # Temperature variation
        temperature = (
            15.0 +  # Mean temperature
            5.0 * np.sin(2 * np.pi * time / (24 * 3600)) +  # Daily variation
            2.0 * np.sin(2 * np.pi * time / (365 * 24 * 3600)) +  # Seasonal (approx)
            0.5 * np.random.randn(n_points)
        )
        
        # Pressure variation
        pressure = (
            15e6 +  # Mean pressure
            1e6 * np.sin(2 * np.pi * time / (8 * 3600)) +  # Operational cycles
            0.5e6 * np.random.randn(n_points)
        )
        
        return {
            "time": time,
            "wave_stress": wave_stress,
            "current_stress": current_stress,
            "total_stress": wave_stress + current_stress,
            "temperature": temperature,
            "pressure": pressure,
            "displacement": np.cumsum(np.random.randn(n_points)) * 0.001,  # Random walk
            "strain": (wave_stress + current_stress) / 210e9  # Elastic strain
        }
    
    def generate_material_database(self) -> Dict[str, Any]:
        """Generate comprehensive material properties database."""
        return {
            "steel_grades": {
                "API_5L_X65": {
                    "yield_strength": 450e6,
                    "ultimate_strength": 535e6,
                    "elastic_modulus": 207e9,
                    "poisson_ratio": 0.27,
                    "density": 7850,
                    "thermal_expansion": 12e-6,
                    "fatigue_class": "C",
                    "fracture_toughness": 150e3,  # Pa√m
                    "charpy_energy": 40  # J at -10°C
                },
                "API_5L_X70": {
                    "yield_strength": 485e6,
                    "ultimate_strength": 570e6,
                    "elastic_modulus": 207e9,
                    "poisson_ratio": 0.27,
                    "density": 7850,
                    "thermal_expansion": 12e-6,
                    "fatigue_class": "C",
                    "fracture_toughness": 140e3,
                    "charpy_energy": 35
                },
                "S355": {
                    "yield_strength": 355e6,
                    "ultimate_strength": 510e6,
                    "elastic_modulus": 210e9,
                    "poisson_ratio": 0.3,
                    "density": 7850,
                    "thermal_expansion": 12e-6,
                    "fatigue_class": "D",
                    "fracture_toughness": 120e3,
                    "charpy_energy": 27
                },
                "AH36": {
                    "yield_strength": 355e6,
                    "ultimate_strength": 490e6,
                    "elastic_modulus": 206e9,
                    "poisson_ratio": 0.3,
                    "density": 7850,
                    "thermal_expansion": 12e-6,
                    "fatigue_class": "F2",
                    "fracture_toughness": 110e3,
                    "charpy_energy": 34
                }
            },
            "environmental_conditions": {
                "north_sea": {
                    "temperature_range": [4, 18],
                    "salinity": 3.5,
                    "ph": 8.1,
                    "dissolved_oxygen": 8.0,
                    "corrosion_rate": 0.025  # mm/year
                },
                "gulf_of_mexico": {
                    "temperature_range": [4, 28],
                    "salinity": 3.6,
                    "ph": 8.0,
                    "dissolved_oxygen": 6.5,
                    "corrosion_rate": 0.030
                },
                "arctic": {
                    "temperature_range": [-2, 8],
                    "salinity": 3.2,
                    "ph": 8.2,
                    "dissolved_oxygen": 10.0,
                    "corrosion_rate": 0.015
                }
            }
        }
    
    def save_scenarios_to_files(self, output_dir: Path):
        """Save all generated scenarios to JSON files."""
        output_dir.mkdir(exist_ok=True)
        
        scenarios = {
            "pipeline_scenarios": self.generate_pipeline_scenarios(),
            "plate_scenarios": self.generate_plate_scenarios(),
            "fatigue_scenarios": self.generate_fatigue_scenarios(),
            "reservoir_scenarios": self.generate_reservoir_scenarios(),
            "material_database": self.generate_material_database()
        }
        
        # Save time series data
        time_series = self.generate_time_series_data(duration_hours=168.0, sample_rate=0.1)  # 1 week at 0.1 Hz
        
        for name, data in scenarios.items():
            file_path = output_dir / f"{name}.json"
            with open(file_path, 'w') as f:
                json.dump(data, f, indent=2)
        
        # Save time series as CSV for easier processing
        time_series_df = pd.DataFrame(time_series)
        time_series_df.to_csv(output_dir / "time_series_data.csv", index=False)
        
        print(f"Test scenarios saved to {output_dir}")


if __name__ == "__main__":
    # Generate test data when run directly
    generator = EngineeringTestDataGenerator(seed=42)
    output_path = Path(__file__).parent
    generator.save_scenarios_to_files(output_path)
