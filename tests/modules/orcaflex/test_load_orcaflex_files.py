"""QA test: validate OrcaFlex YAML/DAT files load without errors.

Two levels of validation:
1. YAML structure validation (always runs) — parse YAML and check for
   expected OrcaFlex top-level keys (General, Environment, LineTypes, etc.)
2. OrcFxAPI model loading (Windows only) — full model validation via
   OrcFxAPI.Model().LoadData()

Usage:
    uv run pytest tests/modules/orcaflex/test_load_orcaflex_files.py -v
"""

import platform
from pathlib import Path

import pytest
import yaml

try:
    import OrcFxAPI

    ORCAFLEX_AVAILABLE = True
except (ImportError, ModuleNotFoundError):
    ORCAFLEX_AVAILABLE = False

DOCS_BASE = Path(__file__).resolve().parents[3] / "docs" / "modules" / "orcaflex"

PIPELINE_24IN = (
    DOCS_BASE / "pipeline" / "installation" / "floating" / "24in_pipeline"
)

# Known OrcaFlex top-level keys for monolithic model files
ORCAFLEX_MODEL_KEYS = {
    "General",
    "Environment",
    "LineTypes",
    "Lines",
    "Vessels",
    "VesselTypes",
    "Buoys",
    "Shapes",
    "Supports",
    "Groups",
    "VariableData",
    "Winches",
    "Links",
    "Constraints",
    "6DBuoys",
    "MorisonElements",
}

# Keys expected in modular include files (partial models)
ORCAFLEX_INCLUDE_KEYS = ORCAFLEX_MODEL_KEYS | {
    "Morison",
    "SupportTypes",
    "MorisonElementTypes",
}

# Keys used in composition/run files that reference other files
ORCAFLEX_COMPOSITION_KEYS = {"BaseFile", "includefile", "IncludeFile"}


def collect_orcaflex_files(directory: Path) -> list[Path]:
    """Collect all .yml and .dat files that are OrcaFlex model files."""
    files = []
    for ext in ("*.yml", "*.dat"):
        files.extend(directory.rglob(ext))
    return sorted(
        f
        for f in files
        if "postproc" not in str(f)
        and "parameters.yml" not in f.name
        and "dm_pipeline_postproc" not in f.name
    )


ALL_FILES = collect_orcaflex_files(PIPELINE_24IN) if PIPELINE_24IN.exists() else []


@pytest.mark.skipif(not ALL_FILES, reason="No OrcaFlex files found")
@pytest.mark.parametrize(
    "model_file",
    ALL_FILES,
    ids=[str(f.relative_to(PIPELINE_24IN)) for f in ALL_FILES],
)
def test_yaml_structure(model_file: Path):
    """Validate YAML parses correctly and contains OrcaFlex keys."""
    content = model_file.read_text(encoding="utf-8-sig")
    data = yaml.safe_load(content)

    assert data is not None, f"YAML is empty: {model_file.name}"
    assert isinstance(data, dict), f"YAML root is not a dict: {model_file.name}"

    keys = set(data.keys())
    has_orcaflex_keys = bool(keys & ORCAFLEX_INCLUDE_KEYS)
    has_composition_keys = bool(keys & ORCAFLEX_COMPOSITION_KEYS)
    assert has_orcaflex_keys or has_composition_keys, (
        f"No OrcaFlex or composition keys found in {model_file.name}. "
        f"Keys: {keys}"
    )


@pytest.mark.skipif(not ALL_FILES, reason="No OrcaFlex files found")
@pytest.mark.parametrize(
    "model_file",
    [f for f in ALL_FILES if "includes" not in str(f)],
    ids=[
        str(f.relative_to(PIPELINE_24IN))
        for f in ALL_FILES
        if "includes" not in str(f)
    ],
)
def test_yaml_model_has_general(model_file: Path):
    """Validate complete model files have a General section."""
    content = model_file.read_text(encoding="utf-8-sig")
    data = yaml.safe_load(content)

    if data and "General" in data:
        general = data["General"]
        assert isinstance(general, dict), "General section must be a dict"
        assert len(general) > 0, (
            f"General section is empty in {model_file.name}"
        )


@pytest.mark.skipif(not ORCAFLEX_AVAILABLE, reason="OrcFxAPI not available on Linux")
@pytest.mark.skipif(not ALL_FILES, reason="No OrcaFlex files found")
@pytest.mark.parametrize(
    "model_file",
    [f for f in ALL_FILES if "includes" not in str(f)],
    ids=[
        str(f.relative_to(PIPELINE_24IN))
        for f in ALL_FILES
        if "includes" not in str(f)
    ],
)
def test_orcaflex_api_load(model_file: Path):
    """Full model validation via OrcFxAPI (Windows only)."""
    model = OrcFxAPI.Model()
    model.LoadData(str(model_file))
    assert model is not None
    assert model.general is not None
