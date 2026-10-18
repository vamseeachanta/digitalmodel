import logging
import os
import subprocess
import re
from pathlib import Path

from assetutilities.common.data import SaveData


save_data = SaveData()

# Third party imports
from assetutilities.common.file_management import FileManagement

fm = FileManagement()


class AqwaUtilities:
    def __init__(self) -> None:
        pass

    def is_license_available(self):
        # TODO
        try:
            import OrcFxAPI

            model = OrcFxAPI.Model()
            print("Orcaflex license is available")
            return True
        except:
            print("Orcaflex license is NOT available")
            raise Exception("Orcaflex license is NOT available .... FAIL")
            return False

    def get_aqwareader_exe(self, cfg):
        ANSYSInstallDir = cfg["software"]["ANSYSInstallDir"]
        aqwareader_exe = os.path.join(
            ANSYSInstallDir, "aisol", "bin", "winx64", "AqwaReader.exe"
        )
        if not os.path.isfile(aqwareader_exe):
            raise Exception(f"AqwaReader.exe not found in {aqwareader_exe}")

        self.aqwareader_exe = aqwareader_exe

        return aqwareader_exe

    def get_workbench_bat(self, cfg):
        ANSYSInstallDir = cfg["software"]["ANSYSInstallDir"]
        workbench_bat = os.path.join(ANSYSInstallDir, "aisol", "workbench.bat")
        if not os.path.isfile(workbench_bat):
            raise Exception(f"workbench.bat not found in {workbench_bat}")

        self.workbench_bat = workbench_bat

        return workbench_bat

    def get_aqwa_exe(self, cfg):
        ANSYSInstallDir = cfg["software"]["ANSYSInstallDir"]
        aqwa_exe = os.path.join(ANSYSInstallDir, "aqwa", "bin", "winx64", "aqwa.exe")
        if not os.path.isfile(aqwa_exe):
            raise Exception(f"aqwa.exe not found in {aqwa_exe}")

        self.aqwa_exe = aqwa_exe

        return aqwa_exe

    def run_aqwa_analysis_as_process(self, cfg):
        # TODO
        aqwa_exe = self.get_aqwa_exe(cfg)
        return aqwa_exe

    def run_aqwa_analysis_as_subprocess(self, cfg, input_file, process="subprocess"):
        """Return captured exit/MES evidence; execution requires explicit opt-in."""
        if cfg.get("execution", {}).get("allow_solver") is not True:
            raise ValueError("Native execution is disabled; provide qualified external evidence")
        if process != "subprocess":
            raise ValueError("Detached execution cannot establish completion evidence")
        mes_path = Path(input_file).with_suffix(".MES")
        before = mes_path.stat().st_mtime_ns if mes_path.exists() else None
        result = subprocess.run(
            [self.get_aqwa_exe(cfg), "/nowind", str(input_file)],
            capture_output=True, text=True, check=False,
            creationflags=getattr(subprocess, "CREATE_NO_WINDOW", 0),
        )
        errors, warnings = [], []
        status = "MISSING"
        if mes_path.exists():
            content = mes_path.read_text(encoding="utf-8", errors="strict")
            errors = [line for line in content.splitlines() if re.search(r"\b(ERROR|FATAL)\b", line, re.I)]
            warnings = [line for line in content.splitlines() if re.search(r"\bWARNING\b", line, re.I)]
            status = "FAIL" if errors or warnings else "PASS"
            if not content.strip():
                status = "EMPTY"
            if before == mes_path.stat().st_mtime_ns:
                status = "STALE"
        return {"returncode": result.returncode, "stdout": result.stdout,
                "stderr": result.stderr, "mes_status": status,
                "errors": errors, "warnings": warnings}
