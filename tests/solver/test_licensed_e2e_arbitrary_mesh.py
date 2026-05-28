"""Licensed end-to-end acceptance test for the arbitrary-mesh OrcaWave workflow.

Issue: #610 (Thread A). Related: #468 (smoke pattern), #611 (output contract),
#622 / #625 (runtime adapter + validation), #628 (live-attrs verification debt).

What this test proves
---------------------
On a licensed OrcFxAPI host it exercises the *full* DigitalModel -> OrcaWave
chain from a repo-managed mesh + ``spec.yml`` fixture pair and exits 0:

    spec.yml + unit_box.gdf                       (repo fixture pair)
      -> DiffractionSpec.from_yaml(...)            (DigitalModel pipeline)
      -> OrcaWaveRunner(use_api=True).run(...)
           -> OrcaWaveBackend.generate_single()    (the .yml input package)
           -> OrcFxAPI.Diffraction(.yml).Calculate()
           -> SaveResults() -> .owr  + SaveData() -> _data.dat
           -> _diffraction_to_results(...)         (#625 live-attrs adapter,
                                                     with .owr LoadResults fallback)
           -> validation_runner verdict            (#625)
      -> SaveResultsSpreadsheet(.owr) -> .xlsx      (see "xlsx note" below)

It then asserts non-empty frequencies, headings, hydrostatic data (when the
solver produced it), at least one populated RAO/result array, and that the run
produced an auto-validation verdict (the #628 live-``Diffraction``-attrs-after-
``Calculate()`` assumption is exercised here; the ``.owr LoadResults`` fallback
covers the other case).

Skip behaviour (acceptance criterion 5)
---------------------------------------
``OrcFxAPI`` is Windows-only and licensed. On Linux / CI it is absent, so the
module-level ``skipif`` SKIPS every test cleanly. Importing this module must
never raise on a license-free host: ``OrcFxAPI`` is imported lazily inside the
fixture, never at module scope. Confirmed on dev-primary (ace-linux-1, no
license): ``pytest`` reports SKIPPED, not failed/errored.

How licensed-win-1 runs this
-----------------------------
``licensed-win-1`` pulls this repo via the shared git remote (its scheduler
checks out the merged branch) and runs the solver suite, e.g.::

    set PYTHONPATH=src
    uv run python -m pytest tests/solver/test_licensed_e2e_arbitrary_mesh.py -v

Because ``OrcFxAPI`` is importable there, the ``skipif`` is inactive and the
test executes the real ``Diffraction(...).Calculate()`` chain and must exit 0.
That run discharges the #610 acceptance criteria *and* the #628 live-attrs
verification debt: this is the first licensed exercise of the assumption that
result arrays are populated after ``Calculate()`` without an explicit
``LoadResults()``. If the live attributes turn out to be empty, the adapter's
mandatory ``.owr LoadResults`` fallback keeps the test green while flagging the
divergence for #628 follow-up (the verdict + populated arrays still assert).

xlsx note (output-contract gap, see PR body / #611)
---------------------------------------------------
``OrcaWaveRunner`` (the #625 runtime) saves ``.owr`` + ``_data.dat`` and
populates ``RunResult.owr_path`` / ``data_file``, but it does **not** yet
populate ``RunResult.xlsx_path`` -- that field is reserved for a later
exporter phase (locked decision D2, documented on the dataclass). To satisfy
acceptance criterion 3 ("record the .xlsx path") this test exports the
spreadsheet itself from the saved ``.owr`` via
``SaveResultsSpreadsheet`` -- the same pattern ``tests/solver/smoke_test.py``
already uses -- rather than monkeypatching the runner. The exported path is
recorded alongside the runtime-provided paths.
"""
from __future__ import annotations

import importlib.util
from pathlib import Path

import pytest

from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
from digitalmodel.hydrodynamics.diffraction.orcawave_runner import (
    OrcaWaveRunner,
    RunConfig,
    RunStatus,
)

# ---------------------------------------------------------------------------
# Skip gate (criterion 5): no OrcFxAPI -> SKIP cleanly, never error on import.
# ---------------------------------------------------------------------------

_HAS_ORCFXAPI = importlib.util.find_spec("OrcFxAPI") is not None

pytestmark = pytest.mark.skipif(
    not _HAS_ORCFXAPI,
    reason="OrcFxAPI not available (licensed Windows host only; see #610)",
)

# Repo-managed mesh + spec fixture pair (criterion 2).
_FIXTURE_DIR = (
    Path(__file__).resolve().parents[2]
    / "tests"
    / "hydrodynamics"
    / "diffraction"
    / "fixtures"
    / "acceptance_610"
)
_SPEC_PATH = _FIXTURE_DIR / "spec.yml"
_MESH_PATH = _FIXTURE_DIR / "unit_box.gdf"


@pytest.fixture(scope="module")
def e2e_result(tmp_path_factory):
    """Run the full licensed chain once and return (RunResult, xlsx_path).

    ``OrcFxAPI`` is imported lazily here, so collecting/importing this module on
    a license-free host never touches the binding.
    """
    import OrcFxAPI  # noqa: F401  (presence already gated by skipif)

    assert _SPEC_PATH.exists(), f"Fixture spec missing: {_SPEC_PATH}"
    assert _MESH_PATH.exists(), f"Fixture mesh missing: {_MESH_PATH}"

    output_dir = tmp_path_factory.mktemp("acceptance_610")

    spec = DiffractionSpec.from_yaml(str(_SPEC_PATH))
    runner = OrcaWaveRunner(
        RunConfig(
            output_dir=output_dir,
            use_api=True,
            dry_run=False,
            validate_outputs=True,
            generate_modular=True,
            copy_mesh_files=True,
        )
    )
    result = runner.run(spec, spec_path=_SPEC_PATH)

    # Export the .xlsx ourselves from the saved .owr (see module "xlsx note").
    # The runtime does not populate RunResult.xlsx_path yet.
    xlsx_path: Path | None = None
    if result.status == RunStatus.COMPLETED and result.owr_path:
        xlsx_path = Path(result.owr_path).with_suffix(".xlsx")
        diff = OrcFxAPI.Diffraction()
        diff.LoadResults(str(Path(result.owr_path).resolve()))
        diff.SaveResultsSpreadsheet(str(xlsx_path.resolve()))

    return result, xlsx_path


def test_run_completed(e2e_result):
    """Criterion 1: the chain runs on the licensed host and the run completes.

    A DRY_RUN status here would mean OrcFxAPI was somehow not used despite being
    importable -- that is a failure on the licensed host.
    """
    result, _ = e2e_result
    assert result.status == RunStatus.COMPLETED, (
        f"Expected COMPLETED, got {result.status}: {result.error_message}"
    )
    assert result.return_code == 0


def test_artifact_paths_recorded(e2e_result):
    """Criterion 3: generated .yml, copied mesh, .owr (+ .xlsx) paths recorded."""
    result, xlsx_path = e2e_result

    # Generated single-file .yml input package (from the DigitalModel pipeline).
    assert result.input_file is not None
    assert result.input_file.exists()
    assert result.input_file.suffix == ".yml"

    # Copied/converted mesh in the output dir.
    assert result.mesh_files, "no mesh files recorded on RunResult"
    assert any(m.exists() for m in result.mesh_files)
    assert any(m.name == _MESH_PATH.name for m in result.mesh_files)

    # .owr produced by SaveResults().
    assert result.owr_path is not None
    assert result.owr_path.exists()
    assert result.owr_path.stat().st_size > 0

    # _data.dat for reproducibility.
    assert result.data_file is not None
    assert result.data_file.exists()

    # .xlsx exported test-side (runtime does not set RunResult.xlsx_path yet).
    assert xlsx_path is not None
    assert xlsx_path.exists()
    assert xlsx_path.stat().st_size > 0


def test_frequencies_and_headings_non_empty(e2e_result):
    """Criterion 4 (part 1): non-empty frequencies and headings."""
    result, _ = e2e_result
    dr = result.diffraction_results
    assert dr is not None, "runtime did not attach DiffractionResults"

    assert dr.raos.surge.frequencies.count > 0
    assert len(dr.raos.surge.frequencies.values) > 0
    assert dr.raos.surge.headings.count > 0
    assert len(dr.raos.surge.headings.values) > 0


def test_result_array_populated(e2e_result):
    """Criterion 4 (part 2): at least one RAO/result array is populated."""
    result, _ = e2e_result
    dr = result.diffraction_results
    assert dr is not None

    # RAO magnitude array for at least one DOF must be non-empty.
    surge_mag = dr.raos.surge.magnitude
    assert surge_mag is not None
    assert getattr(surge_mag, "size", len(surge_mag)) > 0

    # Added-mass matrices (one per frequency) must be present too.
    assert len(dr.added_mass.matrices) > 0
    assert len(dr.damping.matrices) > 0


def test_hydrostatic_data(e2e_result):
    """Criterion 4 (part 3): hydrostatic data is present when produced.

    Hydrostatics are an optional OrcaWave output (the adapter returns ``None``
    if the solver did not compute them). When present, the core fields must be
    populated; this is asserted as a soft requirement so a solver config that
    legitimately omits hydrostatics does not fail the acceptance run, while a
    present-but-empty block does.
    """
    result, _ = e2e_result
    dr = result.diffraction_results
    assert dr is not None

    hs = dr.hydrostatics
    if hs is None:
        pytest.skip(
            "Solver produced no hydrostatics for this case; "
            "core RAO/result coverage asserted by other tests"
        )
    assert hs.mass is not None
    assert hs.stiffness_matrix is not None
    assert getattr(hs.stiffness_matrix, "size", 1) > 0


def test_auto_validation_verdict(e2e_result):
    """Criterion 6: the run produced an auto-validation verdict.

    Exercises the #625 validation wiring on the live host. The verdict must be
    one of the canonical strings and must NOT be SKIPPED -- a SKIPPED verdict on
    a completed licensed run means no DiffractionResults reached the validator,
    which would indicate the #628 live-attrs adapter produced nothing.
    """
    result, _ = e2e_result
    assert result.validation_verdict in {"PASS", "WARNING", "FAIL", "ERROR"}, (
        f"verdict was {result.validation_verdict!r}; expected a non-SKIPPED "
        f"canonical verdict on a completed licensed run. issues="
        f"{result.validation_issues}"
    )
    # A report should have been written for an executed validation.
    assert result.validation_report is not None
