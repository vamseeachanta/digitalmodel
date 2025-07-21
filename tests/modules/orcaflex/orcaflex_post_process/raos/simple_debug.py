"""Simple debug approach for raos test."""
import sys
import os

# Mock OrcFxAPI before any imports
class MockGeneral:
    def __init__(self):
        self.StageEndTime = [3600.0]
        self.ImplicitUseVariableTimeStep = "No"
        self.ImplicitConstantTimeStep = 0.1
        self.ImplicitVariableMaxTimeStep = 0.1
        
class MockState:
    def __init__(self):
        self._name_ = "SimulationStopped"
        
class MockTimeStatus:
    def __init__(self):
        self.CurrentTime = 3600.0

class MockOrcFxAPI:
    class Model:
        def __init__(self, *args, **kwargs):
            self.general = MockGeneral()
            self.simulationComplete = True
            self.state = MockState()
            self.simulationStartTime = "2023-01-01 00:00:00"
            self.simulationStopTime = "2023-01-01 01:00:00"
            self.simulationTimeStatus = MockTimeStatus()
            
        def LoadData(self, *args, **kwargs):
            pass
            
        def RunSimulation(self, *args, **kwargs):
            pass
            
        def SaveSimulation(self, *args, **kwargs):
            pass
            
        def SaveData(self, *args, **kwargs):
            pass

sys.modules['OrcFxAPI'] = MockOrcFxAPI()

# Clear sys.argv to prevent argument conflicts
sys.argv = ['simple_debug.py']

# Now import and run
from digitalmodel.engine import engine

def simple_test():
    """Simple test without extra arguments."""
    input_file = 'raos.yml'
    print(f"Running engine with: {input_file}")
    
    try:
        result = engine(input_file)
        print("SUCCESS: Engine completed!")
        print(f"Result keys: {list(result.keys())}")
        
        # Check if we have the expected structure
        if 'orcaflex_post_process' in result:
            print("Found orcaflex_post_process in results")
            if 'time_series' in result['orcaflex_post_process']:
                print("Found time_series in results")
                print(f"Time series data: {result['orcaflex_post_process']['time_series']}")
            else:
                print("No time_series found in results")
        else:
            print("No orcaflex_post_process found in results")
            
    except Exception as e:
        print(f"ERROR: {e}")
        import traceback
        traceback.print_exc()

if __name__ == "__main__":
    simple_test()