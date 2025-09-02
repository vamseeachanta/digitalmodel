"""Inspect the actual data structure to understand the KeyError."""
import sys
import os

# Mock OrcFxAPI
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
sys.argv = ['inspect_debug.py']

# Monkey patch the function to debug
from digitalmodel.modules.orcaflex.orcaflex_utilities import OrcaflexUtilities

original_get_load_matrix = OrcaflexUtilities.get_load_matrix_with_filenames

def debug_get_load_matrix_with_filenames(self, cfg):
    print("=== DEBUG: get_load_matrix_with_filenames called ===")
    print(f"cfg.file_management keys: {list(cfg.file_management.keys())}")
    if "input_files" in cfg.file_management:
        print(f"input_files keys: {list(cfg.file_management['input_files'].keys())}")
        for key, value in cfg.file_management["input_files"].items():
            print(f"  {key}: {value}")
    else:
        print("No input_files found in file_management")
    
    # Try to fix the key issue
    if "input_files" in cfg.file_management:
        input_files = cfg.file_management["input_files"]
        if "sim" not in input_files and "sim" in [k for k in input_files.keys()]:
            print("Found 'sim' files, proceeding...")
            return original_get_load_matrix(self, cfg)
        else:
            print("Creating empty sim files list")
            # Create empty sim files list to avoid KeyError
            cfg.file_management["input_files"]["sim"] = []
            return original_get_load_matrix(self, cfg)
    else:
        print("No input_files at all, creating empty structure")
        cfg.file_management["input_files"] = {"sim": []}
        return original_get_load_matrix(self, cfg)

# Monkey patch
OrcaflexUtilities.get_load_matrix_with_filenames = debug_get_load_matrix_with_filenames

# Now run the test
from digitalmodel.engine import engine

def debug_test():
    input_file = 'raos.yml'
    print(f"Running engine with: {input_file}")
    
    try:
        result = engine(input_file)
        print("SUCCESS: Engine completed!")
        return result
    except Exception as e:
        print(f"ERROR: {e}")
        import traceback
        traceback.print_exc()
        return None

if __name__ == "__main__":
    debug_test()