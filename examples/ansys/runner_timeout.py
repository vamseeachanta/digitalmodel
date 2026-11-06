"""Demonstrate failure and retained evidence using Python, without a solver.

Run as a module with a new output directory. The timeout and sleep durations are
synthetic demonstration inputs, not solver or engineering acceptance limits.
"""

import argparse
import json
from pathlib import Path
import sys
from unittest.mock import patch

from digitalmodel.ansys.runner import ANSYSRunner, run_ansys
from digitalmodel.ansys.runner_process import run_process

_SYNTHETIC = """
from pathlib import Path
import subprocess
import sys
import time
child = subprocess.Popen([sys.executable, '-c', 'import time; time.sleep(60)'])
Path('child.pid').write_text(str(child.pid), encoding='ascii')
Path('model.out').write_text('*** FATAL *** synthetic failure', encoding='utf-8')
print('partial output', flush=True)
time.sleep(60)
"""


def demonstrate_timeout(directory: Path) -> dict:
    """Inputs: synthetic fatal process tree and one-second execution budget.

    Method: exercise the real runner and supervisor, substituting only the
    executable discovery and solver command. Answer: failed, retaining evidence.
    """
    directory = Path(directory)
    directory.mkdir(parents=True, exist_ok=True)
    script = directory / "model.inp"
    with script.open("x", encoding="utf-8") as stream:
        stream.write("! Synthetic input; no solver is launched.\n")

    def synthetic_command(argv, *, cwd, timeout):
        return run_process([sys.executable, "-c", _SYNTHETIC], cwd=cwd, timeout=timeout)

    with patch.object(ANSYSRunner, "_detect_executable", return_value=Path(sys.executable)):
        with patch("digitalmodel.ansys.runner._run_solver_process", synthetic_command):
            result = run_ansys(script, output_dir=directory / "output", timeout_seconds=1.0)
    return {
        "status": result.status.value,
        "timeout_observed": "timeout" in (result.error_message or "").lower(),
        "fatal_log_retained": result.log_file is not None
        and "*** FATAL ***" in result.log_file.read_text(encoding="utf-8"),
        "partial_output_retained": "partial output" in result.stdout,
    }


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output_directory", type=Path)
    args = parser.parse_args()
    print(json.dumps(demonstrate_timeout(args.output_directory), indent=2))
