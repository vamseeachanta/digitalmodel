#!/usr/bin/env python3
"""
ABOUTME: The non-circular oracle for issue #1959 -- a freshly emitted, unpatched
interFoam case must actually start under the solver named in its own controlDict
and advance at least one timestep.

Design decision D3: a test that asserts "the emitted dict contains the keys the
contract lists" cannot fail when the emitter renders from that same contract. It
proves consistency, never correctness. The only oracle whose verdict does not
depend on the author having guessed the requirement list right is the solver
itself, so that is what this file runs.

The anti-vacuity tests do not need OpenFOAM and always run. The solver start is
skipped where interFoam is not installed -- a skip proves nothing, which is why
the acceptance criterion for #1959 demands a committed log artifact rather than
a green tick.
"""

import shutil
import subprocess
from pathlib import Path

import pytest

from digitalmodel.solvers.openfoam.models import CaseType, OpenFOAMCase
from digitalmodel.solvers.openfoam.case_builder import OpenFOAMCaseBuilder
from digitalmodel.solvers.openfoam.smoke import (
    NotRunnable,
    assert_log_advanced,
    sha256_manifest,
)


# ============================================================================
# Anti-vacuity -- these run everywhere and need no solver
# ============================================================================


class TestStartedButStalledIsNotAPass:
    """"No FATAL in the log" is satisfiable by a solver that does nothing."""

    def test_single_timestep_log_raises(self):
        """A log with one Time = 0 and no fatal marker is not a pass."""
        log = "Starting time loop\n\nTime = 0\n\nEnd\n"
        with pytest.raises(NotRunnable):
            assert_log_advanced(log)

    def test_empty_log_raises(self):
        """A log with no Time lines at all is not a pass."""
        with pytest.raises(NotRunnable):
            assert_log_advanced("")

    def test_fatal_io_error_raises(self):
        """The original #1959 failure mode must be rejected."""
        log = (
            "Time = 0\nTime = 0.001\n"
            "--> FOAM FATAL IO ERROR:\nEntry 'cAlpha' not found\n"
        )
        with pytest.raises(NotRunnable):
            assert_log_advanced(log)

    def test_non_advancing_repeated_time_raises(self):
        """Two Time lines that do not advance are not progress."""
        log = "Time = 0\nTime = 0\n"
        with pytest.raises(NotRunnable):
            assert_log_advanced(log)

    def test_two_advancing_timesteps_returns_last_time(self):
        """A genuinely advancing log returns the last time reached."""
        log = "Time = 0.001\nTime = 0.002\nEnd\n"
        assert assert_log_advanced(log) == "0.002"


class TestManifestDetectsPatching:
    """The manifest is what makes the run evidence about the builder."""

    def test_manifest_changes_when_a_file_is_patched(self, tmp_path):
        """Overwriting one emitted file changes the manifest."""
        case = OpenFOAMCase.for_case_type(CaseType.SLOSHING, name="manifest_probe")
        case_dir = OpenFOAMCaseBuilder(case).build(tmp_path)
        before = sha256_manifest(case_dir)
        (case_dir / "system" / "fvSolution").write_text("patched\n")
        assert sha256_manifest(case_dir) != before

    def test_manifest_is_stable_when_untouched(self, tmp_path):
        """An untouched tree hashes identically twice."""
        case = OpenFOAMCase.for_case_type(CaseType.SLOSHING, name="manifest_probe2")
        case_dir = OpenFOAMCaseBuilder(case).build(tmp_path)
        assert sha256_manifest(case_dir) == sha256_manifest(case_dir)

    def test_manifest_covers_fv_solution(self, tmp_path):
        """fvSolution -- the file this issue is about -- is in the manifest."""
        case = OpenFOAMCase.for_case_type(CaseType.SLOSHING, name="manifest_probe3")
        case_dir = OpenFOAMCaseBuilder(case).build(tmp_path)
        assert "system/fvSolution" in sha256_manifest(case_dir)


# ============================================================================
# Layer 2 -- the real solver start
# ============================================================================

_HAS_INTERFOAM = shutil.which("interFoam") is not None
_HAS_BLOCKMESH = shutil.which("blockMesh") is not None

requires_openfoam = pytest.mark.skipif(
    not (_HAS_INTERFOAM and _HAS_BLOCKMESH),
    reason="interFoam/blockMesh not installed; Layer 2 runs on the CFD node",
)


@requires_openfoam
def test_emitted_case_starts_and_advances(tmp_path):
    """A freshly emitted, unpatched interFoam case starts and advances.

    No file is modified between emission and solver start -- the manifest is
    recorded immediately after build() and re-verified immediately before
    interFoam is invoked, so this is evidence about the builder rather than
    about a tree someone touched.
    """
    case = OpenFOAMCase.for_case_type(CaseType.SLOSHING, name="runnable_probe")
    # A half-full tank: the run must be genuinely two-phase, or it would not
    # exercise the alpha equation this issue is about.
    case.fill_level = 0.5
    case.solver_config.end_time = 10 * case.solver_config.delta_t
    case_dir = OpenFOAMCaseBuilder(case).build(tmp_path)

    emitted = sha256_manifest(case_dir)

    subprocess.run(["blockMesh"], cwd=case_dir, check=True, capture_output=True)
    subprocess.run(["setFields"], cwd=case_dir, check=True, capture_output=True)

    # blockMesh/setFields legitimately add constant/polyMesh and rewrite 0/.
    # The dictionaries this issue is about must be untouched.
    for rel in ("system/fvSolution", "system/fvSchemes", "system/controlDict"):
        assert sha256_manifest(case_dir)[rel] == emitted[rel]

    proc = subprocess.run(
        ["interFoam"], cwd=case_dir, capture_output=True, text=True, timeout=900
    )
    log = proc.stdout + proc.stderr
    (case_dir / "log.interFoam").write_text(log)

    assert_log_advanced(log)
