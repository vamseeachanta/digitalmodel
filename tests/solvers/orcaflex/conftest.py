"""OrcaFlex module test fixtures."""
import pytest
from pathlib import Path

@pytest.fixture
def orcaflex_test_data_dir():
    """Return path to OrcaFlex test data directory."""
    return Path(__file__).parent / "test_data"

@pytest.fixture
def sample_orcaflex_dat():
    """Sample OrcaFlex .dat file content."""
    return """[General]
UnitsSystem=SI
SimulationTimeOrigin=0
StartTime=0
StageCount=2
BuildUpDuration=100
Stage1Duration=100
Stage2Duration=3600

[Environment]
WaterDepth=100
SeabedModel=Flat
"""

@pytest.fixture
def sample_orcaflex_yml():
    """Sample OrcaFlex YAML configuration."""
    return """
General:
  UnitsSystem: SI
  SimulationTimeOrigin: 0
  
Environment:
  WaterDepth: 100
  SeabedModel: Flat
  
Lines:
  - Name: Line1
    Length: 500
    TargetSegmentLength: 10
"""

@pytest.fixture
def mock_orcaflex_model():
    """Create a mock OrcaFlex model object."""
    class MockModel:
        def __init__(self):
            self.general = MockGeneral()
            self.environment = MockEnvironment()
            
        def RunSimulation(self):
            pass
            
        def SaveData(self, filename):
            pass
    
    class MockGeneral:
        def __init__(self):
            self.SimulationTimeOrigin = 0
            
    class MockEnvironment:
        def __init__(self):
            self.WaterDepth = 100
            
    return MockModel()