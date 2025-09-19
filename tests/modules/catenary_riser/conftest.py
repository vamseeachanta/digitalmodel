"""Catenary riser module test fixtures."""
import pytest
import numpy as np

@pytest.fixture
def catenary_properties():
    """Sample catenary riser properties."""
    return {
        'length': 2000,  # m
        'outer_diameter': 0.2794,  # m (11")
        'wall_thickness': 0.0127,  # m
        'weight_in_air': 1500,  # N/m
        'weight_in_water': 1200,  # N/m
        'bending_stiffness': 1e6,  # N.m2
        'axial_stiffness': 1e9,  # N
    }

@pytest.fixture
def catenary_environment():
    """Environmental conditions for catenary analysis."""
    return {
        'water_depth': 1500,  # m
        'current_profile': [
            {'depth': 0, 'velocity': 1.5},
            {'depth': 50, 'velocity': 1.2},
            {'depth': 200, 'velocity': 0.8},
            {'depth': 1500, 'velocity': 0.2},
        ],
        'vessel_offset': 150,  # m
    }