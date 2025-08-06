"""
Global pytest configuration and fixtures for test automation.

This file provides mock implementations for complex dependencies
to enable testing without requiring full environment setup.
"""

import pytest
import sys
from unittest.mock import MagicMock, Mock, patch


@pytest.fixture(autouse=True)
def mock_digitalmodel_dependencies():
    """
    Auto-applied fixture that mocks digitalmodel dependencies.
    
    This fixture runs before every test and sets up mocks for:
    - digitalmodel.engine.engine
    - assetutilities dependencies
    - Other complex imports that may not be available
    """
    
    # Mock digitalmodel.engine with more comprehensive mocking
    mock_engine = MagicMock()
    mock_engine.return_value = {
        'basename': 'transformation',
        'type': 'eigen',
        'status': 'mocked',
        'inputs': None
    }
    
    # Mock the entire digitalmodel package hierarchy
    sys.modules['digitalmodel'] = MagicMock()
    sys.modules['digitalmodel.engine'] = MagicMock()
    sys.modules['digitalmodel.engine'].engine = mock_engine
    sys.modules['digitalmodel.common'] = MagicMock()
    sys.modules['digitalmodel.common.fatigue_analysis'] = MagicMock()
    sys.modules['digitalmodel.modules'] = MagicMock()
    sys.modules['digitalmodel.modules.time_series'] = MagicMock()
    sys.modules['digitalmodel.modules.time_series.time_series_analysis'] = MagicMock()
    sys.modules['digitalmodel.modules.time_series.time_series_components'] = MagicMock()
    
    # Mock assetutilities to prevent scrapy import issues
    mock_assetutilities = MagicMock()
    sys.modules['assetutilities'] = mock_assetutilities
    sys.modules['assetutilities.engine'] = MagicMock()
    sys.modules['assetutilities.engine'].engine = MagicMock()
    
    # Mock scrapy specifically
    sys.modules['scrapy'] = MagicMock()
    
    # Mock other potentially problematic imports
    sys.modules['orcfxapi'] = MagicMock()
    sys.modules['ansys'] = MagicMock()
    
    yield
    
    # Cleanup is handled automatically by pytest


@pytest.fixture
def sample_engine_config():
    """Provide sample engine configuration for tests."""
    return {
        'input_file': 'test.yml',
        'process_results': True,
        'status': 'completed'
    }


@pytest.fixture
def mock_file_operations():
    """Mock file operations for tests that don't need real files."""
    with patch('os.path.isfile') as mock_isfile, \
         patch('os.path.exists') as mock_exists:
        
        mock_isfile.return_value = True
        mock_exists.return_value = True
        
        yield {
            'isfile': mock_isfile,
            'exists': mock_exists
        }


# Mock imports that commonly cause issues
def mock_problematic_imports():
    """Mock imports that commonly cause test failures."""
    problematic_modules = [
        'scrapy',
        'orcfxapi', 
        'ansys.mapdl',
        'ansys.dpf',
        'assetutilities.common.webscraping.web_scraping',
        'assetutilities.common.webscraping.scrapper_scrapy'
    ]
    
    for module_name in problematic_modules:
        if module_name not in sys.modules:
            sys.modules[module_name] = MagicMock()


# Apply mocks at module level
mock_problematic_imports()