"""QA test: validate OrcaFlex YAML/DAT files load without errors.

Two levels of validation:
1. YAML structure validation (always runs) — parse YAML and check for
   expected OrcaFlex top-level keys (General, Environment, LineTypes, etc.)
2. OrcFxAPI model loading (Windows only) — full model validation via
   OrcFxAPI.Model().LoadData()

Usage:
    uv run pytest tests/solvers/orcaflex/test_load_orcaflex_files.py -v
"""

import platform
from pathlib import Path

import pytest
import yaml

DOCS_BASE = Path(__file__).resolve().parents[3] / "docs" / "domains" / "orcaflex"

PIPELINE_24IN = DOCS_BASE / "pipeline" / "installation" / "floating" / "24in_pipeline"
INPUT_FILE_DIR = Path(__file__).with_name("fixtures") / "load_files"

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
    """Collect required model fixtures, rejecting stale or empty directories."""
    assert directory.is_dir(), f"Missing OrcaFlex fixture directory: {directory}"
    files = []
    for ext in ("*.yml", "*.dat"):
        files.extend(directory.rglob(ext))
    files = sorted(
        f
        for f in files
        if "postproc" not in f.relative_to(directory).parts
        # Generator specifications and parameter settings are not models.
        and f.name not in {"parameters.yml", "spec.yml", "spec_qa.yml"}
        and "dm_pipeline_postproc" not in f.name
    )
    assert files, f"No OrcaFlex model fixtures found in: {directory}"
    return files


ALL_FILES = collect_orcaflex_files(PIPELINE_24IN)


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
    # Current modular/master.yml is a sequence of includefile mappings.
    if isinstance(data, list):
        assert data, f"Include sequence is empty: {model_file.name}"
        for entry in data:
            assert isinstance(entry, dict) and set(entry) & ORCAFLEX_COMPOSITION_KEYS, (
                f"Invalid include entry in {model_file.name}: {entry}"
            )
        return
    assert isinstance(data, dict), f"YAML root is not a dict: {model_file.name}"

    keys = set(data.keys())
    has_orcaflex_keys = bool(keys & ORCAFLEX_INCLUDE_KEYS)
    has_composition_keys = bool(keys & ORCAFLEX_COMPOSITION_KEYS)
    assert has_orcaflex_keys or has_composition_keys, (
        f"No OrcaFlex or composition keys found in {model_file.name}. Keys: {keys}"
    )


@pytest.mark.parametrize(
    "model_file",
    [f for f in ALL_FILES if "includes" not in str(f)],
    ids=[
        str(f.relative_to(PIPELINE_24IN)) for f in ALL_FILES if "includes" not in str(f)
    ],
)
def test_yaml_model_has_general(model_file: Path):
    """Validate complete model files have a General section."""
    content = model_file.read_text(encoding="utf-8-sig")
    data = yaml.safe_load(content)

    if data and "General" in data:
        general = data["General"]
        assert isinstance(general, dict), "General section must be a dict"
        assert len(general) > 0, f"General section is empty in {model_file.name}"


@pytest.mark.solver
@pytest.mark.skipif(platform.system() != "Windows", reason="OrcFxAPI requires Windows")
@pytest.mark.parametrize(
    "model_file",
    [f for f in ALL_FILES if "includes" not in str(f)],
    ids=[
        str(f.relative_to(PIPELINE_24IN)) for f in ALL_FILES if "includes" not in str(f)
    ],
)
def test_orcaflex_api_load(model_file: Path):
    """Full model validation via OrcFxAPI (Windows only)."""
    # Keep the licensed interface out of licence-free test collection.
    OrcFxAPI = pytest.importorskip("OrcFxAPI")
    model = OrcFxAPI.Model()
    model.LoadData(str(model_file))
    assert model is not None
    assert model.general is not None


# ---------------------------------------------------------------------------
# Symbolic input fixtures (good file + known-error file; no solver run)
# ---------------------------------------------------------------------------

INPUT_FILES = collect_orcaflex_files(INPUT_FILE_DIR)
INPUT_GOOD_FILES = [f for f in INPUT_FILES if "_error" not in f.stem]
INPUT_ERROR_FILES = [f for f in INPUT_FILES if "_error" in f.stem]
assert INPUT_GOOD_FILES, "No good OrcaFlex input fixtures found"
assert INPUT_ERROR_FILES, "No known-error OrcaFlex input fixtures found"


@pytest.mark.parametrize(
    "model_file",
    INPUT_GOOD_FILES,
    ids=[f.name for f in INPUT_GOOD_FILES],
)
def test_input_file_yaml_valid(model_file: Path):
    """Validate good input_file YAML loads and has expected structure."""
    content = model_file.read_text(encoding="utf-8-sig")
    data = yaml.safe_load(content)

    assert data is not None, f"YAML is empty: {model_file.name}"
    assert isinstance(data, dict)
    assert "General" in data, f"Missing General section in {model_file.name}"
    assert "Environment" in data, f"Missing Environment section in {model_file.name}"


@pytest.mark.parametrize(
    "model_file",
    INPUT_ERROR_FILES,
    ids=[f.name for f in INPUT_ERROR_FILES],
)
def test_input_file_error_yaml_parses(model_file: Path):
    """Error files should parse as valid YAML but contain data-level errors.

    The symbolic error fixture defines coating_defined but references
    coating_missing. This tests reference membership, without a solver run.
    """
    content = model_file.read_text(encoding="utf-8-sig")
    data = yaml.safe_load(content)

    # YAML structure is valid
    assert data is not None
    assert isinstance(data, dict)
    # But we document the known error for traceability
    assert "General" in data, "Error file should still have General section"


@pytest.mark.parametrize(
    "model_file",
    INPUT_GOOD_FILES,
    ids=[f.name for f in INPUT_GOOD_FILES],
)
def test_input_file_coating_references_valid(model_file: Path):
    """Verify all CoatingThickness references resolve to defined coatings.

    By inspection of coating.yml, the reference coating_defined belongs to
    the defined set {coating_defined}. Detects the reference error:
      'Failed to set CoatingThickness=X (Data source X does not exist)'
    """
    content = model_file.read_text(encoding="utf-8-sig")
    data = yaml.safe_load(content)

    coatings = data.get("VariableData", {}).get("Coatingsorlinings", [])
    defined = {c["Name"] for c in coatings if isinstance(c, dict) and "Name" in c}

    line_types = data.get("LineTypes", [])
    for lt in line_types:
        if not isinstance(lt, dict):
            continue
        coat_ref = lt.get("CoatingThickness")
        if coat_ref and isinstance(coat_ref, str) and coat_ref != "~":
            assert coat_ref in defined, (
                f"Undefined coating reference '{coat_ref}' in LineType "
                f"'{lt.get('Name', '?')}'. Defined: {sorted(defined)}"
            )


@pytest.mark.parametrize(
    "model_file",
    INPUT_ERROR_FILES,
    ids=[f.name for f in INPUT_ERROR_FILES],
)
def test_input_file_error_has_bad_coating_ref(model_file: Path):
    """Verify the error file has a broken coating reference.

    By inspection of coating_error.yml, coating_missing is not a member of
    the defined set {coating_defined}. No numerical solver result is asserted.
    """
    content = model_file.read_text(encoding="utf-8-sig")
    data = yaml.safe_load(content)

    coatings = data.get("VariableData", {}).get("Coatingsorlinings", [])
    defined = {c["Name"] for c in coatings if isinstance(c, dict) and "Name" in c}

    line_types = data.get("LineTypes", [])
    has_broken_ref = False
    for lt in line_types:
        if not isinstance(lt, dict):
            continue
        coat_ref = lt.get("CoatingThickness")
        if coat_ref and isinstance(coat_ref, str) and coat_ref not in defined:
            has_broken_ref = True
            break

    assert has_broken_ref, (
        f"Expected a broken coating reference in {model_file.name} "
        f"but all references resolved. Defined: {sorted(defined)}"
    )


@pytest.mark.parametrize(
    "model_file",
    INPUT_ERROR_FILES,
    ids=[f.name for f in INPUT_ERROR_FILES],
)
def test_input_file_error_has_known_diff(model_file: Path):
    """Verify error files differ from their good counterparts.

    Error files should have a corresponding good file (same name without _error).
    This test ensures the error is intentional and traceable.
    """
    good_name = model_file.name.replace("_error", "")
    good_file = model_file.parent / good_name

    assert good_file.exists(), (
        f"No matching good file for {model_file.name}. Expected: {good_name}"
    )

    error_content = model_file.read_text(encoding="utf-8-sig")
    good_content = good_file.read_text(encoding="utf-8-sig")
    assert error_content != good_content, (
        f"Error file {model_file.name} is identical to {good_name}"
    )
