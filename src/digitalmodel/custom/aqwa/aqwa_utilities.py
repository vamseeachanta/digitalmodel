import os
import logging
import subprocess

from digitalmodel.common.orcaflex_model_utilities import OrcaflexModelUtilities

from assetutilities.common.data import SaveData
save_data = SaveData()

try:
    import OrcFxAPI
except:
    print("OrcFxAPI not available")
from collections import OrderedDict

from assetutilities.common.yml_utilities import ymlInput
from assetutilities.common.file_management import FileManagement

fm = FileManagement()


class AqwaUtilities:

    def __init__(self):
        pass

    def is_license_available(self):
        #TODO
        try:
            model = OrcFxAPI.Model()
            print("Orcaflex license is available")
            return True
        except:
            print("Orcaflex license is NOT available")
            raise Exception("Orcaflex license is NOT available .... FAIL")
            return False

    def get_aqwareader_exe(self, cfg):
        ANSYSInstallDir = cfg['software']['ANSYSInstallDir']
        aqwareader_exe = os.path.join(ANSYSInstallDir, "aisol", "bin", "winx64", "AqwaReader.exe")
        if not os.path.isfile(aqwareader_exe):
            raise Exception(f"AqwaReader.exe not found in {aqwareader_exe}")

        self.aqwareader_exe = aqwareader_exe

    def get_workbench_bat(self, cfg):
        ANSYSInstallDir = cfg['software']['ANSYSInstallDir']
        workbench_bat = os.path.join(ANSYSInstallDir, "aisol", "workbench.bat")
        if not os.path.isfile(workbench_bat):
            raise Exception(f"workbench.bat not found in {workbench_bat}")

        self.workbench_bat = workbench_bat

    def get_aqwa_exe(self, cfg):
        ANSYSInstallDir = cfg['software']['ANSYSInstallDir']
        aqwa_exe = os.path.join(ANSYSInstallDir, "aqwa", "bin", "winx64", "aqwa.exe")
        if not os.path.isfile(aqwa_exe):
            raise Exception(f"aqwa.exe not found in {aqwa_exe}")

        self.aqwa_exe = aqwa_exe
        
        return aqwa_exe

    def run_aqwa_analysis_as_process(self, cfg):
        #TODO
        aqwa_exe = self.get_aqwa_exe(cfg)
        return aqwa_exe

    def run_aqwa_analysis_as_subprocess(self, cfg, input_file, process='subprocess'):
        aqwa_exe = self.get_aqwa_exe(cfg)
        # Currently the time to startup client is sufficent for socket to be ready. 
        # possibly write out a temp.bat and send that to detached subprocess.
        # args = ['ping 127.0.0.1 -n 1', '&&', aqwa_exe, '/nowind', input_file]
        # args = ['timeout', '5', '&&', aqwa_exe, '/nowind', input_file]
        args = [aqwa_exe, '/nowind', input_file]
        logging.info(f"AQWA process running for {input_file} ... START")

        if process == 'detached':
            DETACHED_PROCESS = 0x00000008
            aqwa_process = subprocess.Popen(args,
                        stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, creationflags=DETACHED_PROCESS)
        elif process == 'subprocess':
            aqwa_process = subprocess.Popen(args,
                        stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            aqwa_out, err = aqwa_process.communicate()

        logging.info(f"AQWA process running for {input_file} ... ... ... ... COMPLETE")

