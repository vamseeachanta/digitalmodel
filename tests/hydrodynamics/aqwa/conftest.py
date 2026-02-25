"""AQWA module test fixtures."""
import pytest
from pathlib import Path

@pytest.fixture
def aqwa_test_data_dir():
    """Return path to AQWA test data directory."""
    return Path(__file__).parent / "test_data"

@pytest.fixture
def sample_aqwa_dat():
    """Sample AQWA .dat file content."""
    return """AQWA-LINE
TITLE Sample AQWA Model
RESTART 2 0
OPTIONS ILIN IPLT ILTF
1 1 1
END
"""

@pytest.fixture
def sample_aqwa_lis():
    """Sample AQWA .lis file content."""
    return """
    AQWA LISTING FILE
    =================
    
    Analysis completed successfully
    Maximum response: 10.5 m
    """