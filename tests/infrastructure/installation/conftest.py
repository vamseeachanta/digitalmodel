"""Installation module test fixtures."""
import pytest

@pytest.fixture
def installation_vessel():
    """Installation vessel specifications."""
    return {
        'name': 'Heavy Lift Vessel',
        'crane_capacity': 5000,  # tonnes
        'deck_area': 5000,  # m2
        'deck_load': 20,  # tonnes/m2
        'dp_class': 3,
        'accommodation': 150,  # persons
    }

@pytest.fixture
def structure_properties():
    """Structure properties for installation."""
    return {
        'type': 'jacket',
        'weight_in_air': 2500,  # tonnes
        'weight_in_water': 2200,  # tonnes
        'dimensions': {
            'height': 150,  # m
            'footprint': 50,  # m x 50 m
        },
        'cog': {  # Center of gravity
            'x': 0,
            'y': 0,
            'z': 75,  # m from base
        }
    }

@pytest.fixture
def environmental_window():
    """Weather window for installation."""
    return {
        'significant_wave_height_max': 2.5,  # m
        'wave_period_range': (6, 12),  # seconds
        'wind_speed_max': 15,  # m/s
        'current_speed_max': 1.0,  # m/s
    }