"""Mock mode must be asked for, never inferred from a failed import.

Issue #1631, D5/D5a/D6. Five OrcaFlex surfaces infer mock mode from
``ORCAFLEX_AVAILABLE`` being false and then report success:

    A  run_to_sim.py:74           mock branch sets success=True
    B  universal_runner.py:118    except ImportError -> self.mock_mode = True
    C  core/model_interface.py:163  use_mock or not ORCFXAPI_AVAILABLE
    D  run_to_sim_cli.py:109      auto args.mock = True, then exit 0
    E  universal/batch_processor.py:270  forged .sim at the real output path

plus the surface the issue's three-file list omits entirely
(``orcaflex_utilities`` / ``opp.post_process_router``), which is the path
behind ``lr_acma_ff132001b7ad`` -- the queued run that returned no artifacts
and still reported ``returncode: 0``.

Every test here patches the availability flag explicitly rather than relying on
whether ``OrcFxAPI`` imports on the runner. A test whose result depends on the
host is not a test.
"""

from __future__ import annotations

from pathlib import Path
from unittest.mock import patch

import pytest


# --- path A: run_to_sim ----------------------------------------------------


def test_run_to_sim_raises_without_license_and_without_opt_in() -> None:
    from digitalmodel.solvers.orcaflex.core.exceptions import LicenseError
    from digitalmodel.solvers.orcaflex import run_to_sim

    with patch.object(run_to_sim, "ORCAFLEX_AVAILABLE", False):
        with pytest.raises(LicenseError):
            run_to_sim.OrcaFlexModelRunner(mock_mode=False)


def test_run_to_sim_mock_still_works_when_explicitly_requested(tmp_path: Path) -> None:
    """D4: unlicensed development stays possible when it is asked for."""
    from digitalmodel.solvers.orcaflex import run_to_sim

    with patch.object(run_to_sim, "ORCAFLEX_AVAILABLE", False):
        runner = run_to_sim.OrcaFlexModelRunner(mock_mode=True)

    assert runner.mock_mode is True


def test_run_to_sim_mock_does_not_report_a_sim_file_it_never_wrote(
    tmp_path: Path,
) -> None:
    """The mock branch reports ``sim_output`` for a path it never writes."""
    from digitalmodel.solvers.orcaflex import run_to_sim

    model = tmp_path / "model.yml"
    model.write_text("{}\n")

    with patch.object(run_to_sim, "ORCAFLEX_AVAILABLE", False):
        runner = run_to_sim.OrcaFlexModelRunner(mock_mode=True)
        result = runner.run_single_model(model, output_dir=tmp_path)

    assert Path(result["sim_output"]).exists() is True


def test_run_to_sim_mock_does_not_log_real_mode(tmp_path: Path) -> None:
    """``run_batch`` logs ``Mode: REAL`` for a mock run because ``mock_mode``
    was never mutated on the auto-fallback path."""
    from digitalmodel.solvers.orcaflex import run_to_sim

    with patch.object(run_to_sim, "ORCAFLEX_AVAILABLE", False):
        runner = run_to_sim.OrcaFlexModelRunner(mock_mode=True)

    assert runner.describe_mode() == "MOCK"


# --- path B: universal_runner ----------------------------------------------


def test_universal_runner_does_not_infer_mock_from_importerror() -> None:
    from digitalmodel.solvers.orcaflex.core.exceptions import LicenseError
    from digitalmodel.solvers.orcaflex.universal import universal_runner

    with patch.object(universal_runner, "_orcaflex_api_available", return_value=False):
        with pytest.raises(LicenseError):
            universal_runner.UniversalOrcaFlexRunner(mock_mode=False)


def test_universal_runner_mock_artifact_is_written_under_a_mock_directory(
    tmp_path: Path,
) -> None:
    """D6 signal 1 of 3: a distinct location."""
    from digitalmodel.solvers.orcaflex.universal import universal_runner

    runner = universal_runner.UniversalOrcaFlexRunner(mock_mode=True)
    model = tmp_path / "model.dat"
    model.write_text("dummy\n")

    result = runner._process_single_model(model, tmp_path / "out")

    assert Path(result["sim_file"]).parent.name == "out_mock"


def test_universal_runner_mock_artifact_name_carries_the_mock_infix(
    tmp_path: Path,
) -> None:
    """D6 signal 2 of 3: the filename itself. Asserted separately so that
    removing one signal cannot silently pass."""
    from digitalmodel.solvers.orcaflex.universal import universal_runner

    runner = universal_runner.UniversalOrcaFlexRunner(mock_mode=True)
    model = tmp_path / "model.dat"
    model.write_text("dummy\n")

    result = runner._process_single_model(model, tmp_path / "out")

    assert Path(result["sim_file"]).name == "model.mock.sim"


def test_universal_runner_mock_artifact_keeps_the_in_band_marker(
    tmp_path: Path,
) -> None:
    """D6 signal 3 of 3: the body."""
    from digitalmodel.solvers.orcaflex.universal import universal_runner

    runner = universal_runner.UniversalOrcaFlexRunner(mock_mode=True)
    model = tmp_path / "model.dat"
    model.write_text("dummy\n")

    result = runner._process_single_model(model, tmp_path / "out")

    assert Path(result["sim_file"]).read_text().startswith("Mock simulation for")


# --- path C: core/model_interface ------------------------------------------


def test_model_interface_no_module_license_error_is_reachable() -> None:
    """``:163`` forces ``use_mock`` exactly when ``ORCFXAPI_AVAILABLE`` is
    false, which makes the ``NO_MODULE`` LicenseError at ``:182-188``
    provably dead code. This test resurrects it."""
    from digitalmodel.solvers.orcaflex.core.exceptions import LicenseError
    from digitalmodel.solvers.orcaflex.core import model_interface

    with patch.object(model_interface, "ORCFXAPI_AVAILABLE", False):
        with patch.dict("os.environ", {}, clear=False) as _:
            import os

            os.environ.pop("ORCAFLEX_FORCE_MOCK", None)
            os.environ.pop("ORCAFLEX_SKIP_REAL", None)
            with pytest.raises(LicenseError) as excinfo:
                model_interface.OrcaFlexModelWrapper(use_mock=False)

    assert excinfo.value.error_code == "NO_MODULE"


def test_model_interface_env_opt_in_still_yields_mock() -> None:
    """D4: the CI escape hatch that ``:156-163`` already honours survives."""
    from digitalmodel.solvers.orcaflex.core import model_interface

    with patch.object(model_interface, "ORCFXAPI_AVAILABLE", False):
        with patch.dict("os.environ", {"ORCAFLEX_FORCE_MOCK": "1"}):
            wrapper = model_interface.OrcaFlexModelWrapper(use_mock=False)

    assert wrapper.use_mock is True


def test_model_interface_explicit_use_mock_still_yields_mock() -> None:
    from digitalmodel.solvers.orcaflex.core import model_interface

    with patch.object(model_interface, "ORCFXAPI_AVAILABLE", False):
        wrapper = model_interface.OrcaFlexModelWrapper(use_mock=True)

    assert wrapper.use_mock is True


def test_mock_extract_results_does_not_return_random_numbers() -> None:
    """``:570-573`` returns ``np.random.randn(100)`` as engineering results."""
    from digitalmodel.solvers.orcaflex.core.exceptions import ModelError
    from digitalmodel.solvers.orcaflex.core import model_interface
    from digitalmodel.solvers.orcaflex.core.model_interface import ModelState

    with patch.object(model_interface, "ORCFXAPI_AVAILABLE", False):
        wrapper = model_interface.OrcaFlexModelWrapper(use_mock=True)
    wrapper._state = ModelState.STATIC_COMPLETE

    with pytest.raises(ModelError, match="mock"):
        wrapper.extract_results("Line1", "Effective Tension")


def test_mock_save_model_does_not_create_a_bare_artifact_at_the_real_path(
    tmp_path: Path,
) -> None:
    """``:509-511`` does ``output_path.touch()``, leaving an empty ``.sim`` at
    the path a real solve would have written."""
    from digitalmodel.solvers.orcaflex.core import model_interface
    from digitalmodel.solvers.orcaflex.core.model_interface import ModelState

    with patch.object(model_interface, "ORCFXAPI_AVAILABLE", False):
        wrapper = model_interface.OrcaFlexModelWrapper(use_mock=True)
    wrapper._state = ModelState.STATIC_COMPLETE

    real_path = tmp_path / "model.sim"
    wrapper.save_model(real_path)

    assert real_path.exists() is False


# --- path D: run_to_sim_cli -------------------------------------------------


def test_run_to_sim_cli_does_not_auto_enable_mock(tmp_path: Path) -> None:
    """``:109-112`` flips ``args.mock`` and then exits 0 on mock success."""
    import sys

    from digitalmodel.solvers.orcaflex import run_to_sim_cli

    argv = ["run-to-sim", "--directory", str(tmp_path), "--all"]
    with patch.object(run_to_sim_cli, "ORCAFLEX_AVAILABLE", False):
        with patch.object(sys, "argv", argv):
            with pytest.raises(SystemExit) as excinfo:
                run_to_sim_cli.main()

    assert excinfo.value.code == 1


# --- path E: batch_processor ------------------------------------------------


def test_batch_processor_mock_artifact_is_written_under_a_mock_directory(
    tmp_path: Path,
) -> None:
    from digitalmodel.solvers.orcaflex.universal.batch_processor import BatchProcessor

    processor = BatchProcessor(mock_mode=True)
    model = tmp_path / "model.dat"
    model.write_text("dummy\n")

    result = processor._process_single(model, tmp_path / "out")

    assert Path(result["sim_file"]).parent.name == "out_mock"


def test_batch_processor_mock_artifact_name_carries_the_mock_infix(
    tmp_path: Path,
) -> None:
    from digitalmodel.solvers.orcaflex.universal.batch_processor import BatchProcessor

    processor = BatchProcessor(mock_mode=True)
    model = tmp_path / "model.dat"
    model.write_text("dummy\n")

    result = processor._process_single(model, tmp_path / "out")

    assert Path(result["sim_file"]).name == "model.mock.sim"


# --- D5a: the surface the issue's fix list omits ----------------------------


def test_strength_post_refuses_without_license() -> None:
    """``opp.post_process_router:172-175`` prints and returns ``cfg``.

    This is the path behind ``lr_acma_ff132001b7ad``: the run produced no
    artifacts, and the result JSON has no ``returned_files`` key at all, yet
    it recorded ``returncode: 0``. None of the three modules the issue names
    is on this call path.
    """
    from digitalmodel.solvers.orcaflex.core.exceptions import LicenseError
    from digitalmodel.solvers.orcaflex.opp import OrcaFlexPostProcess
    from digitalmodel.solvers.orcaflex.orcaflex_utilities import OrcaflexUtilities

    with patch.object(OrcaflexUtilities, "is_orcaflex_available", return_value=False):
        with pytest.raises(LicenseError):
            OrcaFlexPostProcess().post_process_router({"meta": {"basename": "orcaflex"}})


def test_strength_post_mock_opt_in_does_not_refuse() -> None:
    """D4: the explicit opt-in keeps unlicensed inspection possible."""
    from digitalmodel.solvers.orcaflex.opp import OrcaFlexPostProcess
    from digitalmodel.solvers.orcaflex.orcaflex_utilities import OrcaflexUtilities

    cfg = {
        "meta": {"basename": "orcaflex"},
        "orcaflex": {"postprocess": {}},
    }
    with patch.object(OrcaflexUtilities, "is_orcaflex_available", return_value=False):
        with patch.dict("os.environ", {"ORCAFLEX_FORCE_MOCK": "1"}):
            returned = OrcaFlexPostProcess().post_process_router(cfg)

    assert returned is cfg


# --- D5 hazard guard: the raise must be at call time, not import time -------


def test_import_of_orcaflex_package_still_succeeds_without_license() -> None:
    """``solvers/orcaflex/__init__.py:20-41`` wraps every subimport in
    ``try/except ImportError`` and degrades the export to ``None``.

    An import-time raise would be swallowed there and would resurface as a
    ``NoneType`` error at some later, unrelated call site -- strictly worse
    than today's behaviour.
    """
    import importlib

    module = importlib.import_module("digitalmodel.solvers.orcaflex")

    assert module is not None
