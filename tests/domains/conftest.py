"""Root conftest for all module tests."""
import os
import sys
from pathlib import Path
import pytest
import tempfile
import shutil

# Add project root to path for imports
PROJECT_ROOT = Path(__file__).parent.parent.parent
if str(PROJECT_ROOT) not in sys.path:
    sys.path.insert(0, str(PROJECT_ROOT))

# Add src directory to path
SRC_DIR = PROJECT_ROOT / "src"
if str(SRC_DIR) not in sys.path:
    sys.path.insert(0, str(SRC_DIR))

@pytest.fixture
def test_data_dir():
    """Return path to test data directory."""
    return Path(__file__).parent / "test_data"

@pytest.fixture
def temp_dir():
    """Create a temporary directory for test outputs."""
    temp_dir = tempfile.mkdtemp()
    yield Path(temp_dir)
    # Cleanup after test
    shutil.rmtree(temp_dir, ignore_errors=True)

@pytest.fixture
def config_dir():
    """Return path to test configurations directory."""
    return Path(__file__).parent / "test_configs"

@pytest.fixture
def mock_file(temp_dir):
    """Create a mock file for testing."""
    def _mock_file(filename, content=""):
        filepath = temp_dir / filename
        filepath.write_text(content)
        return filepath
    return _mock_file

@pytest.fixture
def sample_csv_data():
    """Provide sample CSV data for testing."""
    return """time,value1,value2
0.0,1.0,2.0
0.1,1.1,2.1
0.2,1.2,2.2
0.3,1.3,2.3
0.4,1.4,2.4
"""

@pytest.fixture
def sample_yaml_config():
    """Provide sample YAML configuration for testing."""
    return """
analysis:
  type: test_analysis
  parameters:
    param1: 100
    param2: 200
  options:
    debug: true
    output_dir: ./output
"""

@pytest.fixture
def fixture_path():
    """Return path to fixtures directory."""
    return Path(__file__).parent / "fixtures"