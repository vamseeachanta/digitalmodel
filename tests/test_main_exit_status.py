"""``python -m digitalmodel <input.yml>`` must be able to report failure.

Issue #1631, premise 13. ``main()`` ends with a bare ``engine()`` whose return
value is discarded and there is no ``sys.exit`` on the engine-contract path, so
the command exits 0 no matter what the engine concluded -- on any host,
licensed or not. Deckhand is a thin ``subprocess.run`` wrapper, so that exit
status is the entire success signal.

The end-to-end pair at the bottom is the anti-vacuity guard required by D7: it
patches ``run_orcawave`` to inject a *completed* solve, so it exercises the
solver success path and the verdict gate together. Neither test's result
depends on whether ``OrcFxAPI`` imports on the runner.
"""

from __future__ import annotations

import os
import shutil
import subprocess
import tempfile
import sys
from pathlib import Path
from types import SimpleNamespace
from unittest.mock import patch

import pytest
import yaml

REPO_ROOT = Path(__file__).resolve().parents[1]
UNIT_BOX = REPO_ROOT / "examples" / "hydrodynamics" / "diffraction" / "unit_box_rao"


# --- entrypoint-level: the exit path itself -------------------------------


def _run_main_with_engine_returning(cfg: dict, tmp_path: Path) -> int:
    """Drive ``main()`` with ``engine()`` stubbed, and return the exit status.

    Returns 0 when ``main()`` completes without raising ``SystemExit``, which
    is what the shell observes.
    """
    import digitalmodel.__main__ as entry

    input_file = tmp_path / "input.yml"
    input_file.write_text("basename: diffraction\n")

    with patch.object(entry, "engine", return_value=cfg):
        with patch.object(sys, "argv", ["digitalmodel", str(input_file)]):
            try:
                entry.main()
            except SystemExit as exc:
                return int(exc.code or 0)
    return 0


def test_main_exits_non_zero_on_fail_verdict(tmp_path: Path) -> None:
    """Measured on origin/main @ 0fdeea67: exit status 0 while carrying FAIL."""
    cfg = {
        "basename": "diffraction",
        "diffraction": {
            "validation_verdict": "FAIL",
            "output_directory": str(tmp_path),
            "outputs": {"diffraction_results_json": str(tmp_path / "r.json")},
        },
    }

    assert _run_main_with_engine_returning(cfg, tmp_path) == 1


def test_main_exits_zero_on_warning_verdict(tmp_path: Path) -> None:
    """D3 inverse-defect guard. Five of the seven validated runs are WARNING;
    refusing them would make the lane unusable."""
    cfg = {
        "basename": "diffraction",
        "diffraction": {
            "validation_verdict": "WARNING",
            "output_directory": str(tmp_path),
            "outputs": {"diffraction_results_json": str(tmp_path / "r.json")},
        },
    }

    assert _run_main_with_engine_returning(cfg, tmp_path) == 0


def test_main_exits_zero_when_workflow_declares_no_verdict(tmp_path: Path) -> None:
    """Back-compat: un-migrated workflows are unaffected."""
    cfg = {"basename": "vertical_riser", "vertical_riser": {"done": True}}

    assert _run_main_with_engine_returning(cfg, tmp_path) == 0


def test_main_writes_the_sidecar_for_a_completed_run(tmp_path: Path) -> None:
    cfg = {
        "basename": "diffraction",
        "diffraction": {
            "validation_verdict": "WARNING",
            "output_directory": str(tmp_path),
            "outputs": {"diffraction_results_json": str(tmp_path / "r.json")},
        },
    }

    _run_main_with_engine_returning(cfg, tmp_path)

    assert (tmp_path / "run_verdict.json").exists() is True


# --- end-to-end: real engine, real router, injected completed solve --------


def _solve_input(tmp_path: Path) -> Path:
    """Write a real engine input driving a real OrcaWave solve request."""
    work = tmp_path / "case"
    work.mkdir()
    shutil.copy(UNIT_BOX / "spec.yml", work / "spec.yml")
    shutil.copy(UNIT_BOX / "unit_box.gdf", work / "unit_box.gdf")

    input_file = work / "run.yml"
    input_file.write_text(
        yaml.safe_dump(
            {
                "basename": "diffraction",
                "Analysis": {"result_folder": str(tmp_path / "results")},
                "diffraction": {
                    "operation": "run_orcawave",
                    "spec": "spec.yml",
                    "output_directory": "out",
                    "dry_run": False,
                },
            }
        )
    )
    return input_file


# The driver runs in a real child process so the assertion is on the process
# exit status itself -- the whole of the signal Deckhand consumes
# (licensed_run_agent_runtime.py:37-50 is a thin subprocess.run wrapper). An
# in-process SystemExit assertion would prove less.
_DRIVER = '''
import sys
from types import SimpleNamespace
from unittest.mock import patch

from digitalmodel.hydrodynamics.diffraction.orcawave_runner import RunStatus

verdict, out_dir, input_file = sys.argv[1], sys.argv[2], sys.argv[3]

# status="completed" -- the solve really succeeded. This exercises the solver
# success path and the verdict gate together, so it cannot pass vacuously on a
# host where OrcFxAPI happens to import.
completed = SimpleNamespace(
    status=RunStatus("completed"),
    output_dir=out_dir,
    input_file="UnitBoxRAO.yml",
    modular_files=[],
    mesh_files=[],
    error_message=None,
    validation_verdict=verdict,
    validation_issues=[],
    diffraction_results=SimpleNamespace(
        to_dict=lambda: {"vessel_name": "UnitBoxRAO", "raos": {"frequencies": [0.5]}}
    ),
)

with patch(
    "digitalmodel.hydrodynamics.diffraction.orcawave_runner.run_orcawave",
    return_value=completed,
):
    sys.argv = ["digitalmodel", input_file]
    import digitalmodel.__main__ as entry
    try:
        entry.main()
    except SystemExit:
        raise
    except BaseException:
        # A crash must never be mistakable for a refusal. Exit 1 is the
        # refusal code; anything unexpected gets its own code so a broken
        # driver cannot make the FAIL assertion pass vacuously.
        import traceback
        traceback.print_exc()
        sys.exit(99)
'''


def _run_main_end_to_end(verdict: str) -> int:
    # Deliberately NOT pytest's tmp_path: engine._running_under_pytest()
    # (engine.py:80-86) scans sys.argv for the substring "pytest", and
    # pytest's tmp_path contains it, which would silently push the child down
    # the under-test argument branch instead of the shipped one.
    tmp_path = Path(tempfile.mkdtemp(prefix="dm1631-e2e-"))
    input_file = _solve_input(tmp_path)
    out_dir = tmp_path / "results" / "out"

    driver = tmp_path / "drive.py"
    driver.write_text(_DRIVER)

    # Mirror the sibling-checkout resolution tests/conftest.py:52-65 performs,
    # which a child process does not inherit.
    search = [str(REPO_ROOT / "src")]
    for sibling in ("assetutilities/src", "aceengineercode"):
        candidate = (REPO_ROOT.parent / sibling).resolve()
        if candidate.exists():
            search.append(str(candidate))

    env = dict(os.environ)
    # engine.py:113-121 takes a different argument-parsing branch when it
    # detects pytest. The child must look like a real Deckhand invocation
    # (`uv run python -m digitalmodel <input>`), not like a test process, or
    # this stops being an end-to-end check of the shipped path.
    for marker in [k for k in env if k.startswith("PYTEST")]:
        env.pop(marker)
    env["PYTHONPATH"] = os.pathsep.join(
        search + [env.get("PYTHONPATH", "")]
    ).rstrip(os.pathsep)

    proc = subprocess.run(
        [sys.executable, str(driver), verdict, str(out_dir), str(input_file)],
        capture_output=True,
        text=True,
        env=env,
        cwd=str(tmp_path),
        timeout=300,
    )
    if proc.returncode == 99:
        raise AssertionError(
            "driver crashed instead of completing or refusing:\n"
            + proc.stderr[-4000:]
        )
    return proc.returncode


def test_completed_solve_with_fail_verdict_refuses_end_to_end() -> None:
    """The acceptance shape for the two FAIL runs in the queue.

    The solve *completed* -- this is not a solver-unavailable path. The verdict
    is what refuses.
    """
    assert _run_main_end_to_end("FAIL") == 1


def test_completed_solve_with_pass_verdict_succeeds_end_to_end() -> None:
    """Anti-vacuity partner: a gate never observed green is unproven."""
    assert _run_main_end_to_end("PASS") == 0


def test_completed_solve_with_warning_verdict_succeeds_end_to_end() -> None:
    """The five WARNING runs must keep passing."""
    assert _run_main_end_to_end("WARNING") == 0
