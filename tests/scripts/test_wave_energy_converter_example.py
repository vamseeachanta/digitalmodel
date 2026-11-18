"""Import and execution regression coverage for the WEC example."""

import logging
import runpy
import subprocess
import sys
from dataclasses import is_dataclass
from pathlib import Path
from typing import get_origin, get_type_hints

import pandas as pd
import pytest


EXAMPLE = (
    Path(__file__).resolve().parents[2]
    / "examples/domains/workflows/07_wave_energy_converter.py"
)
COLUMNS = [
    "wave_height",
    "wave_force",
    "max_tension",
    "safety_factor",
    "fatigue_damage",
    "status",
]


@pytest.fixture
def example(monkeypatch):
    """Load without running main; import must leave process state unchanged."""
    monkeypatch.setattr(sys, "path", sys.path.copy())
    original_path = sys.path.copy()
    original_handlers = logging.getLogger().handlers.copy()
    namespace = runpy.run_path(str(EXAMPLE))
    assert sys.path == original_path
    assert logging.getLogger().handlers == original_handlers
    return namespace


def test_wave_energy_converter_imports_and_resolves_annotations(example):
    """Resolve annotations eagerly, including on Python with deferred annotations.

    Python 3.11 typing documentation, typing.Dict: built-in dict is supported.
    No illustrative engineering equation is evaluated by this import check.
    """
    assert is_dataclass(example["WECDevice"])
    analyze = example["analyze_wec_mooring"]
    assert callable(analyze)
    hints = get_type_hints(analyze)
    assert hints["wec"] is example["WECDevice"]
    return_type = hints["return"]
    assert (get_origin(return_type) or return_type) is dict


def assert_workbook(output_dir):
    """Check serialization, not the validity of the illustrative calculations.

    The existing main sample grid is 1 through 10 m inclusive at unit spacing.
    Independent row count: (10 - 1) / 1 + 1 = 10. These are example inputs,
    not a sourced design envelope or engineering acceptance limits.
    """
    workbook = output_dir / "wec_analysis.xlsx"
    assert workbook.is_file()
    frame = pd.read_excel(workbook)
    assert list(frame.columns) == COLUMNS
    assert frame["wave_height"].tolist() == list(range(1, 11))
    assert frame.notna().all().all()
    assert set(frame["status"]) <= {"PASS", "FAIL"}
    assert list(output_dir.iterdir()) == [workbook]


def test_main_writes_workbook_to_requested_directory(example, tmp_path):
    """The output argument supports an isolated destination that does not exist."""
    output_dir = tmp_path / "nested" / "results"
    example["main"](output_dir=output_dir)
    assert_workbook(output_dir)


def test_script_writes_workbook_from_another_working_directory(tmp_path):
    """Execute the real entry point with real dependencies and a relative output."""
    result = subprocess.run(
        [sys.executable, str(EXAMPLE), "--output-dir", "results"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        timeout=30,  # Software smoke-test time budget, not an engineering limit.
        check=False,
    )
    assert result.returncode == 0, result.stderr
    assert_workbook(tmp_path / "results")
    assert {path.name for path in tmp_path.iterdir()} == {"results"}


def test_script_help_describes_unqualified_example_without_writing_output(tmp_path):
    """Command-line help states the example's limits without running analysis."""
    script = tmp_path / EXAMPLE.name
    script.write_bytes(EXAMPLE.read_bytes())
    result = subprocess.run(
        [sys.executable, str(script), "--help"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        timeout=30,  # Software smoke-test time budget, not an engineering limit.
        check=False,
    )
    assert result.returncode == 0, result.stderr
    help_text = " ".join(result.stdout.split())
    assert "Illustrative WEC mooring sweep; writes wec_analysis.xlsx." in help_text
    assert "Not qualified against any standard." in help_text
    assert "--output-dir" in help_text
    for claim in ("Standards:", "DNV", "EMEC", "IEC", "design and fatigue analysis"):
        assert claim not in help_text
    assert list(tmp_path.iterdir()) == [script]


def test_script_default_output_is_preserved(tmp_path):
    """A temporary script copy checks the original script-relative destination."""
    script = tmp_path / EXAMPLE.name
    script.write_bytes(EXAMPLE.read_bytes())
    result = subprocess.run(
        [sys.executable, str(script)],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        timeout=30,  # Software smoke-test time budget, not an engineering limit.
        check=False,
    )
    assert result.returncode == 0, result.stderr
    assert_workbook(tmp_path / "outputs" / "07_wec")


def test_import_in_fresh_process_has_no_side_effects(tmp_path):
    """A fresh interpreter also exposes logging configuration at import time."""
    code = """
import logging
import runpy
import sys
from pathlib import Path

path_before = sys.path.copy()
handlers_before = logging.getLogger().handlers.copy()
runpy.run_path(sys.argv[1])
assert sys.path == path_before
assert logging.getLogger().handlers == handlers_before
assert not list(Path.cwd().iterdir())
"""
    result = subprocess.run(
        [sys.executable, "-c", code, str(EXAMPLE)],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        timeout=30,  # Software smoke-test time budget, not an engineering limit.
        check=False,
    )
    assert result.returncode == 0, result.stderr
