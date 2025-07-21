"""Debug version to understand file discovery issues."""
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

# Now import and debug
from digitalmodel.engine import engine
from assetutilities.common.yml_utilities import ymlInput

def debug_file_discovery():
    """Debug what files are being discovered."""
    input_file = 'raos.yml'
    print(f"Loading config from: {input_file}")
    
    # Load the config manually to see what's happening
    from assetutilities.common.ApplicationManager import ConfigureApplicationInputs
    from assetutilities.common.yml_utilities import WorkingWithYAML
    from assetutilities.common.update_deep import AttributeDict
    
    app_manager = ConfigureApplicationInputs()
    wwyaml = WorkingWithYAML()
    
    inputfile, cfg_argv_dict = app_manager.validate_arguments_run_methods(input_file)
    cfg = wwyaml.ymlInput(inputfile, updateYml=None)
    cfg = AttributeDict(cfg)
    
    print(f"Config loaded. Basename: {cfg.get('basename', 'NOT FOUND')}")
    print(f"File management flag: {cfg.get('file_management', {}).get('flag', 'NOT FOUND')}")
    print(f"Input directory: {cfg.get('file_management', {}).get('input_directory', 'NOT FOUND')}")
    
    # Check what files exist
    import glob
    yml_dir = cfg.get('file_management', {}).get('input_directory', 'yml')
    print(f"\nChecking directory: {yml_dir}")
    
    for ext in ['yml', 'yaml', 'sim', 'dat', 'txt']:
        pattern = os.path.join(yml_dir, f"*.{ext}")
        files = glob.glob(pattern)
        print(f"  *.{ext}: {files}")
    
    # Now try to run the engine and see where it fails
    try:
        result = engine(input_file)
        print("Engine completed successfully!")
        return result
    except Exception as e:
        print(f"Engine failed with error: {e}")
        import traceback
        traceback.print_exc()
        return None

if __name__ == "__main__":
    debug_file_discovery()