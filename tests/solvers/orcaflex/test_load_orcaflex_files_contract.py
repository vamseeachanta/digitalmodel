"""Regression contracts for fixture discovery, without importing a solver."""

import builtins
import runpy
from pathlib import Path

import pytest


@pytest.fixture
def load_file_tests(monkeypatch):
    """Collection must not attempt to import the optional licensed interface."""
    original_import = builtins.__import__

    def guarded_import(name, *args, **kwargs):
        assert name != "OrcFxAPI", "Fixture collection must not import OrcFxAPI"
        return original_import(name, *args, **kwargs)

    monkeypatch.setattr(builtins, "__import__", guarded_import)
    return runpy.run_path(str(Path(__file__).with_name("test_load_orcaflex_files.py")))


def test_current_fixture_sets_are_populated(load_file_tests):
    """Issue #2549 requires each licence-free parameter set to execute."""
    assert load_file_tests["ALL_FILES"]
    assert load_file_tests["INPUT_GOOD_FILES"]
    assert load_file_tests["INPUT_ERROR_FILES"]


def test_missing_fixture_directory_fails(load_file_tests, tmp_path):
    """An absent required fixture directory must fail instead of skipping."""
    with pytest.raises(AssertionError, match="fixture directory"):
        load_file_tests["collect_orcaflex_files"](tmp_path / "missing")


def test_empty_fixture_directory_fails(load_file_tests, tmp_path):
    """An empty required fixture directory must fail instead of skipping."""
    with pytest.raises(AssertionError, match="No OrcaFlex model fixtures"):
        load_file_tests["collect_orcaflex_files"](tmp_path)


def test_discovery_excludes_auxiliary_files(load_file_tests, tmp_path):
    """Expected paths are enumerated by hand, independently of discovery.

    Models (including optimize.yml) and includes are inputs; specifications,
    parameters and postprocessing files configure other tools.
    """
    expected = [
        Path("includes/part.yml"),
        Path("model.dat"),
        Path("model.yml"),
        Path("optimize.yml"),
    ]
    auxiliary = [
        Path("spec.yml"),
        Path("spec_qa.yml"),
        Path("inputs/parameters.yml"),
        Path("postproc/result.yml"),
        Path("dm_pipeline_postproc.yml"),
    ]
    for relative in expected + auxiliary:
        target = tmp_path / relative
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text("General: {UnitsSystem: SI}\n", encoding="utf-8")

    actual = load_file_tests["collect_orcaflex_files"](tmp_path)
    assert [path.relative_to(tmp_path) for path in actual] == expected


def test_only_auxiliary_files_fails(load_file_tests, tmp_path):
    """An auxiliary-only directory cannot stand in for model coverage."""
    (tmp_path / "parameters.yml").write_text("{}", encoding="utf-8")
    with pytest.raises(AssertionError, match="No OrcaFlex model fixtures"):
        load_file_tests["collect_orcaflex_files"](tmp_path)


def test_include_sequence_is_valid_structure(load_file_tests, tmp_path):
    """The modular/master.yml fixture uses a sequence of include mappings."""
    model_file = tmp_path / "master.yml"
    model_file.write_text("- includefile: includes/general.yml\n", encoding="utf-8")
    load_file_tests["test_yaml_structure"](model_file)


@pytest.mark.parametrize("content", ["[]", "- unrelated: value", "- scalar"])
def test_invalid_include_sequence_fails(load_file_tests, tmp_path, content):
    """Empty sequences, unknown keys and scalar entries are not compositions."""
    model_file = tmp_path / "master.yml"
    model_file.write_text(content, encoding="utf-8")
    with pytest.raises(AssertionError):
        load_file_tests["test_yaml_structure"](model_file)
