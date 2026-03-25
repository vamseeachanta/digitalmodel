"""Cathodic protection module test fixtures."""
import pytest

@pytest.fixture
def cp_design_parameters():
    """Sample cathodic protection design parameters."""
    return {
        'structure_area': 5000,  # m2
        'design_life': 25,  # years
        'current_density_initial': 150,  # mA/m2
        'current_density_mean': 90,  # mA/m2
        'current_density_final': 70,  # mA/m2
        'coating_breakdown_factor': 0.05,  # 5%
        'utilization_factor': 0.8,
        'anode_type': 'Al-Zn-In',
    }

@pytest.fixture
def anode_properties():
    """Sample anode properties."""
    return {
        'Al-Zn-In': {
            'capacity': 2750,  # Ah/kg
            'potential': -1.05,  # V vs Ag/AgCl
            'efficiency': 0.90,
            'density': 2700,  # kg/m3
        },
        'Zn': {
            'capacity': 780,  # Ah/kg
            'potential': -1.03,  # V
            'efficiency': 0.95,
            'density': 7140,  # kg/m3
        }
    }