"""Mooring module test fixtures."""
import pytest

@pytest.fixture
def mooring_line_properties():
    """Mooring line properties."""
    return {
        'type': 'chain',
        'diameter': 0.147,  # m (147mm)
        'grade': 'R4',
        'breaking_load': 15000,  # kN
        'weight_in_air': 380,  # kg/m
        'weight_in_water': 330,  # kg/m
        'axial_stiffness': 1.2e9,  # N
    }

@pytest.fixture
def mooring_configuration():
    """Mooring system configuration."""
    return {
        'pattern': 'spread',
        'number_of_lines': 12,
        'groups': 4,
        'pretension': 1500,  # kN
        'angle_between_lines': 30,  # degrees
        'fairlead_radius': 50,  # m
        'anchor_radius': 1500,  # m
    }

@pytest.fixture
def vessel_data():
    """Vessel data for mooring analysis."""
    return {
        'displacement': 150000,  # tonnes
        'length': 300,  # m
        'beam': 60,  # m
        'draft': 20,  # m
        'gm': 5,  # m
        'wind_area_x': 3000,  # m2
        'wind_area_y': 8000,  # m2
        'current_coefficients': {
            'cx': 0.6,
            'cy': 2.5,
            'cm': 8.0,
        }
    }