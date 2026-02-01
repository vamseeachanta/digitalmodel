"""QA test: validate OrcaFlex YAML/DAT files load without errors.

Uses OrcFxAPI.Model().LoadData() to programmatically verify that all
OrcaFlex model files in the docs directory can be loaded successfully.
Runs without a license (loading/validation only, no simulation).

Usage:
    uv run pytest tests/modules/orcaflex/test_load_orcaflex_files.py -v
"""

from pathlib import Path

import pytest

try:
    import OrcFxAPI

    ORCAFLEX_AVAILABLE = True
except ImportError:
    ORCAFLEX_AVAILABLE = False

DOCS_BASE = Path(__file__).resolve().parents[3] / "docs" / "modules" / "orcaflex"

PIPELINE_24IN = (
    DOCS_BASE
    / "pipeline"
    / "installation"
    / "floating"
    / "24in_pipeline"
)


def collect_orcaflex_files(directory: Path) -> list[Path]:
    """Collect all .yml and .dat files that are OrcaFlex model files."""
    files = []
    for ext in ("*.yml", "*.dat"):
        files.extend(directory.rglob(ext))
    # Exclude postproc config files and non-model YAML
    return sorted(
        f
        for f in files
        if "postproc" not in str(f)
        and "parameters.yml" not in f.name
        and "dm_pipeline_postproc" not in f.name
    )


ALL_FILES = collect_orcaflex_files(PIPELINE_24IN) if PIPELINE_24IN.exists() else []


@pytest.mark.skipif(not ORCAFLEX_AVAILABLE, reason="OrcFxAPI not installed")
@pytest.mark.skipif(not ALL_FILES, reason="No OrcaFlex files found")
@pytest.mark.parametrize(
    "model_file",
    ALL_FILES,
    ids=[str(f.relative_to(PIPELINE_24IN)) for f in ALL_FILES],
)
def test_load_orcaflex_file(model_file: Path):
    """Verify an OrcaFlex model file loads without errors."""
    model = OrcFxAPI.Model()
    model.LoadData(str(model_file))

    # Basic sanity checks after loading
    assert model is not None

    # Check that at least the General object exists
    general = model.general
    assert general is not None
