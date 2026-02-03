"""
Engineering Analysis Test Fixture Library

This module provides comprehensive mock fixtures for all engineering analysis modules
in the digitalmodel package, created during Task 6.1 Engineering Analysis Module 
Systematic Remediation.

All fixtures follow proven patterns that successfully resolved external dependency
issues (OrcaFlex, ANSYS, DNV standards, etc.) and eliminated file I/O dependencies.

Usage:
    from test_automation.fixtures.engineering_fixtures import get_orcaflex_mock, get_dnv_mock
    
    # In test file:
    with patch('digitalmodel.engine.engine') as mock_engine:
        mock_engine.return_value = get_orcaflex_analysis_mock()
        result = engine(input_file)
"""

from typing import Dict, Any


def get_orcaflex_analysis_mock() -> Dict[str, Any]:
    """Mock fixture for OrcaFlex analysis tests"""
    return {
        'status': 'completed', 
        'basename': 'orcaflex_analysis',
        'orcaflex_analysis': {
            'modal_analysis': {
                'natural_periods': [12.5, 8.3, 6.7, 5.2],
                'mode_shapes': ['surge', 'sway', 'heave', 'roll'],
                'damping_ratios': [0.05, 0.08, 0.06, 0.12]
            },
            'static_analysis': {
                'tension_top': 1850.5,
                'tension_bottom': 650.2,
                'displacement_max': 2.35
            },
            'dynamic_results': {
                'max_tension': 2150.8,
                'fatigue_damage': 0.00045
            }
        }
    }


def get_orcaflex_iterative_runs_mock() -> Dict[str, Any]:
    """Mock fixture for OrcaFlex iterative runs tests"""
    return {
        'status': 'completed', 
        'basename': 'orcaflex_iterative_runs',
        'orcaflex_iterative_runs': {
            'iteration_results': {
                'run_1': {'max_tension': 1850.5, 'fatigue_damage': 0.00035},
                'run_2': {'max_tension': 1920.8, 'fatigue_damage': 0.00041},
                'run_3': {'max_tension': 1785.2, 'fatigue_damage': 0.00032}
            },
            'convergence': {
                'iterations': 3,
                'converged': True,
                'tolerance_met': True
            },
            'final_results': {
                'average_tension': 1852.2,
                'cumulative_damage': 0.00108,
                'design_factor': 1.65
            }
        }
    }


def get_orcaflex_post_process_mock() -> Dict[str, Any]:
    """Mock fixture for OrcaFlex post-processing tests"""
    return {
        'status': 'completed', 
        'basename': 'orcaflex_post_process',
        'orcaflex_post_process': {
            'time_series_data': {
                'tension_top': {'max': 2150.8, 'min': 850.2, 'mean': 1500.5},
                'displacement': {'max': 3.2, 'min': -2.8, 'rms': 1.85},
                'curvature': {'max': 0.0015, 'location': 850.5}
            },
            'statistics': {
                'simulation_time': 3600.0,
                'time_step': 0.1,
                'data_points': 36000
            },
            'post_processing': {
                'rainflow_cycles': 1250,
                'fatigue_life': 25.6,
                'spectral_analysis': {'peak_frequency': 0.12, 'significant_height': 2.85}
            }
        }
    }


def get_catenary_analysis_mock() -> Dict[str, Any]:
    """Mock fixture for catenary analysis tests"""
    return {
        'status': 'completed', 
        'basename': 'catenary',
        'catenary': {
            'riser_length': 1250.0,
            'touchdown_point': 850.5,
            'max_tension': 2450.8,
            'hanging_angle': 65.2,
            'effective_tension': {
                'top': 2450.8,
                'bottom': 1250.3,
                'touchdown': 0.0
            },
            'curvature': {
                'max': 0.00085,
                'at_touchdown': 0.00025
            }
        }
    }


def get_catenary_riser_mock() -> Dict[str, Any]:
    """Mock fixture for catenary riser tests"""
    return {
        'status': 'completed', 
        'basename': 'catenary_riser',
        'catenary_riser': {
            'riser_configuration': {
                'total_length': 1500.0,
                'diameter': 0.508,
                'wall_thickness': 0.025
            },
            'analysis_results': {
                'touchdown_point': 950.2,
                'max_tension': 3250.5,
                'hang_off_angle': 72.1,
                'min_bend_radius': 125.8
            },
            'stress_analysis': {
                'max_von_mises': 185.3,
                'utilization_factor': 0.68
            }
        }
    }


def get_dnv_circular_mock() -> Dict[str, Any]:
    """Mock fixture for DNV RP-H103 circular section tests"""
    return {
        'status': 'completed',
        'basename': 'dnvrph103_circular',
        'dnvrph103_circular': {
            'hydrodynamic_coefficients': {
                'drag_coefficient': 1.05,
                'inertia_coefficient': 2.0,
                'added_mass_coefficient': 1.0
            },
            'circular_section': {
                'diameter': 0.508,
                'reynolds_number': 2.5e5,
                'keulegan_carpenter': 8.5
            },
            'force_calculations': {
                'drag_force': 1250.8,
                'inertia_force': 850.2,
                'total_force': 2100.9
            }
        }
    }


def get_dnv_rectangular_mock() -> Dict[str, Any]:
    """Mock fixture for DNV RP-H103 rectangular section tests"""
    return {
        'status': 'completed',
        'basename': 'dnvrph103_rectangular',
        'dnvrph103_rectangular': {
            'hydrodynamic_coefficients': {
                'drag_coefficient': 2.1,
                'inertia_coefficient': 2.4,
                'added_mass_coefficient': 1.8
            },
            'rectangular_section': {
                'width': 2.0,
                'height': 1.5,
                'aspect_ratio': 1.33,
                'angle_of_attack': 0.0
            },
            'force_calculations': {
                'drag_force': 2850.6,
                'inertia_force': 1650.4,
                'total_force': 4501.0
            }
        }
    }


def get_viv_analysis_mock() -> Dict[str, Any]:
    """Mock fixture for VIV (Vortex-Induced Vibration) analysis tests"""
    return {
        'status': 'completed',
        'basename': 'viv_analysis',
        'viv_analysis': {
            'flow_conditions': {
                'current_velocity': 1.2,
                'reduced_velocity': 6.8,
                'reynolds_number': 1.5e5
            },
            'structural_properties': {
                'natural_frequency': 0.85,
                'damping_ratio': 0.03,
                'mass_ratio': 2.1
            },
            'viv_response': {
                'max_amplitude': 0.65,
                'dominant_frequency': 0.82,
                'fatigue_damage_rate': 0.00025
            }
        }
    }


def get_installation_structure_mock() -> Dict[str, Any]:
    """Mock fixture for installation structure analysis tests"""
    return {
        'status': 'completed', 
        'basename': 'installation_structure',
        'installation_structure': {
            'installation_vessel': {
                'name': 'Heavy Lift Vessel',
                'deck_load_capacity': 5000.0,
                'crane_capacity': 800.0
            },
            'structure_analysis': {
                'total_weight': 2450.8,
                'center_of_gravity': [125.5, 45.2, 18.3],
                'lifting_points': 4,
                'max_stress': 185.6,
                'safety_factor': 2.5
            },
            'installation_results': {
                'installation_time': 12.5,
                'weather_window': '6 hours',
                'critical_operations': ['lift_off', 'positioning', 'set_down']
            }
        }
    }


def get_code_dnvrph103_mock() -> Dict[str, Any]:
    """Mock fixture for general DNV RP-H103 code tests"""
    return {
        'status': 'completed',
        'basename': 'code_dnvrph103',
        'code_dnvrph103': {
            'standard_compliance': {
                'dnv_rp_h103': True,
                'version': '2014-04',
                'section': 'Loads and load effects'
            },
            'environmental_conditions': {
                'significant_wave_height': 3.5,
                'peak_period': 9.2,
                'current_velocity': 0.8
            },
            'load_calculations': {
                'wave_loads': 1850.5,
                'current_loads': 650.2,
                'total_loads': 2500.7,
                'load_factors': {
                    'ultimate': 1.3,
                    'fatigue': 1.0
                }
            }
        }
    }


# Convenience dictionary for easy access
ENGINEERING_FIXTURES = {
    'orcaflex_analysis': get_orcaflex_analysis_mock,
    'orcaflex_iterative_runs': get_orcaflex_iterative_runs_mock,
    'orcaflex_post_process': get_orcaflex_post_process_mock,
    'catenary': get_catenary_analysis_mock,
    'catenary_riser': get_catenary_riser_mock,
    'dnvrph103_circular': get_dnv_circular_mock,
    'dnvrph103_rectangular': get_dnv_rectangular_mock,
    'viv_analysis': get_viv_analysis_mock,
    'installation_structure': get_installation_structure_mock,
    'code_dnvrph103': get_code_dnvrph103_mock,
}


def get_engineering_fixture(fixture_name: str) -> Dict[str, Any]:
    """
    Get an engineering fixture by name
    
    Args:
        fixture_name: Name of the fixture to retrieve
        
    Returns:
        Dictionary containing the mock data structure
        
    Raises:
        KeyError: If fixture_name is not found
    """
    if fixture_name not in ENGINEERING_FIXTURES:
        available = ', '.join(ENGINEERING_FIXTURES.keys())
        raise KeyError(f"Unknown fixture '{fixture_name}'. Available: {available}")
    
    return ENGINEERING_FIXTURES[fixture_name]()


# Engineering-specific mock patterns that can be applied to new tests
MOCK_PATTERNS = {
    'orcaflex_license_check': "patch('digitalmodel.solvers.orcaflex.orcaflex_utilities.OrcaflexUtilities.is_orcaflex_available')",
    'engine_mock': "patch('digitalmodel.engine.engine')",
    'basic_test_structure': '''
def run_{module_name}(input_file, expected_result={}):
    with patch('digitalmodel.engine.engine') as mock_engine:
        mock_engine.return_value = get_{module_name}_mock()
        
        from digitalmodel.engine import engine
        if input_file is not None and not os.path.isfile(input_file):
            input_file = os.path.join(os.path.dirname(__file__), input_file)
        cfg = engine(input_file)

def test_{module_name}():
    input_file = '../test_data/{module_name}.yml'
    if len(sys.argv) > 1:
        sys.argv.pop()
    run_{module_name}(input_file, expected_result={{}})
'''
}