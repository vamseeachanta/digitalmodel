# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Exercise continuation without loading or running a solver.

Basis: OpenFOAM v13 User Guide (2025), section 4.4.2, Time control:
https://doc.cfd.direct/openfoam/user-guide-v13/controldict
Synthetic times are hand-selected: latest=1, requested=3; 2 < 3,
3 = 3, 4 > 3, and 3e0 = 3. No engineering coefficients are used.
Success requires exit zero AND numeric last logged time >= requested time.
Polling deadlines are test infrastructure limits, not acceptance tolerances.
"""

import os
from pathlib import Path
import subprocess
import time

import pytest


SCRIPT = Path(__file__).resolve().parents[2] / "scripts/cfd/continue_case.sh"


@pytest.fixture
def continuation(tmp_path):
    """Replace every solver-facing command before invoking the real wrapper."""
    case = tmp_path / "case"
    (case / "processor0/1").mkdir(parents=True)
    (case / "system").mkdir()
    (case / "PAUSED").touch()
    (case / "CONT_DONE").write_text("stale success\n")
    (case / "CONT_FAILED").write_text("stale failure\n")
    (case / "log.fakeSolver").write_text("previous log\n")
    bindir = tmp_path / "bin"
    bindir.mkdir()
    commands = {
        "foamDictionary": '''#!/usr/bin/env python3
import pathlib, sys
path = pathlib.Path(sys.argv[-1])
key, value = sys.argv[2], sys.argv[4]
lines = path.read_text().splitlines()
path.write_text("\\n".join(
    f"{key} {value};" if line.startswith(key + " ") else line
    for line in lines) + "\\n")
''',
        "mpirun": '''#!/usr/bin/env bash
cp system/controlDict launched-controlDict
if ! grep -q '^stopAt endTime;' system/controlDict; then
  echo 'Time = 2'
else
  printf '%s' "$TEST_LOG"
fi
exit "$TEST_RC"
''',
        "pgrep": "#!/usr/bin/env bash\nexit 1\n",
        "sleep": "#!/usr/bin/env bash\nexit 0\n",
    }
    for name, content in commands.items():
        path = bindir / name
        path.write_text(content)
        path.chmod(0o755)
    bashrc = tmp_path / "bashrc"
    bashrc.write_text(":\n")
    env = {key: value for key, value in os.environ.items()
           if key not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"}}
    env.update(PATH=f"{bindir}:{env['PATH']}", WM_BASHRC=str(bashrc),
               DM_CFD_ROOT=str(tmp_path), DM_CFD_CAMPAIGN="test",
               DM_CFD_MPIRUN_FLAGS="")

    def run(log="Time = 3\n", rc="0", pause="writeNow"):
        (case / "system/controlDict").write_text(
            f"application fakeSolver;\nstartFrom startTime;\n"
            f"endTime 1;\nstopAt {pause};\n")
        env.update(TEST_LOG=log, TEST_RC=rc)
        result = subprocess.run(["bash", str(SCRIPT), str(case), "3"],
                                env=env, capture_output=True, text=True,
                                timeout=10)
        assert result.returncode == 0, result.stderr
        deadline = time.monotonic() + 10
        while time.monotonic() < deadline:
            markers = (case / "CONT_DONE", case / "CONT_FAILED")
            if any(p.exists() for p in markers) and not (case / "run.pid").exists():
                return case
            time.sleep(0.01)
        pytest.fail("continuation did not write a terminal marker and clear run.pid")

    return run


@pytest.mark.parametrize("pause", ["writeNow", "nextWrite"])
def test_resume_restores_time_controls_before_launch(continuation, pause):
    case = continuation(pause=pause)
    controls = (case / "launched-controlDict").read_text()
    assert "startFrom latestTime;" in controls
    assert "stopAt endTime;" in controls
    assert "endTime 3;" in controls
    assert not (case / "PAUSED").exists()
    assert (case / "CONT_DONE").exists()
    assert not (case / "CONT_FAILED").exists()
    assert any(p.read_text() == "previous log\n"
               for p in case.glob("log.fakeSolver.to1.*"))


@pytest.mark.parametrize("last", ["3", "4", "3e0"])
def test_reached_target_is_done(continuation, last):
    case = continuation(log=f"Time = 2\nTime = {last}\n")
    assert (case / "CONT_DONE").exists()
    assert not (case / "CONT_FAILED").exists()


@pytest.mark.parametrize("log", ["Time = 2\n", "", "Time = nan\n",
                                 "Time = 3\nTime = invalid\n"])
def test_zero_exit_without_target_evidence_fails(continuation, log):
    case = continuation(log=log)
    assert not (case / "CONT_DONE").exists()
    failure = (case / "CONT_FAILED").read_text()
    assert "target=3" in failure
    assert "rc=0" in failure


def test_nonzero_exit_fails_even_at_target(continuation):
    case = continuation(rc="7")
    assert not (case / "CONT_DONE").exists()
    assert "rc=7" in (case / "CONT_FAILED").read_text()
