# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Execute the documented ASCII triangle example against hand geometry."""

import re
import sys
from pathlib import Path

import numpy as np
import pytest


DOCUMENT = (
    Path(__file__).resolve().parents[3]
    / "src/digitalmodel/hydrodynamics/bemrosetta/docs"
    / "worked-ascii-triangle-round-trip.md"
)
# Source: the document's Numerical comparison criterion. No relative allowance.
ATOL = 1e-12


def _execute_example(directory, monkeypatch, offset=None):
    """Run the sole Python fence verbatim with a supplied temporary directory."""
    blocks = re.findall(
        r"^```python\n(.*?)^```$", DOCUMENT.read_text(encoding="utf-8"), re.M | re.S
    )
    assert len(blocks) == 1, "The example must have exactly one Python block"
    directory.mkdir()
    monkeypatch.chdir(directory)
    arguments = [str(DOCUMENT), str(directory)]
    if offset is not None:
        arguments.extend(["--offset", *(str(value) for value in offset)])
    monkeypatch.setattr(sys, "argv", arguments)
    namespace = {"__name__": "__main__"}
    exec(compile(blocks[0], str(DOCUMENT), "exec"), namespace)
    # The supplied directory contains the sole generated artifact, in ASCII.
    assert list(directory.rglob("*")) == [directory / "triangle.stl"]
    text = (directory / "triangle.stl").read_text(encoding="ascii")
    assert text.startswith("solid ")
    assert text.count("facet normal ") == 1
    assert text.count("vertex ") == 3
    return namespace["results"]


def _assert_geometry(results, expected_bounds):
    """Check both meshes against the independent derivation in the document."""
    for stage in ("before", "after"):
        geometry = results[stage]
        # Hand calculation: base 5 - 1 = 4, height 5 - 2 = 3;
        # half the rectangle area is (4 * 3) / 2 = 6 square units.
        assert geometry["area"] == pytest.approx(6.0, rel=0, abs=ATOL)
        np.testing.assert_allclose(
            geometry["bounds"], expected_bounds, rtol=0, atol=ATOL
        )
        # Ordered edges (4, 0, 0), (0, 3, 0) give (0, 0, 12): +z.
        np.testing.assert_allclose(
            geometry["normals"], [[0.0, 0.0, 1.0]], rtol=0, atol=ATOL
        )


def test_documented_ascii_stl_round_trip(tmp_path, monkeypatch):
    """Check area, extrema and signed unit normal without vertex comparisons."""
    results = _execute_example(tmp_path / "original", monkeypatch)
    # Coordinate-wise extrema of A=(1,2,3), B=(5,2,3), C=(1,5,3).
    _assert_geometry(results, [[1.0, 2.0, 3.0], [5.0, 5.0, 3.0]])


def test_documented_ascii_stl_translation(tmp_path, monkeypatch):
    """Translation changes extrema but preserves area and normal direction."""
    original = _execute_example(tmp_path / "original", monkeypatch)
    # Assumed offset from the document: (10, -4, 2). Adding it by hand gives
    # A'=(11,-2,5), B'=(15,-2,5), C'=(11,1,5); edges remain unchanged.
    translated = _execute_example(
        tmp_path / "translated", monkeypatch, offset=(10.0, -4.0, 2.0)
    )
    _assert_geometry(original, [[1.0, 2.0, 3.0], [5.0, 5.0, 3.0]])
    _assert_geometry(translated, [[11.0, -2.0, 5.0], [15.0, 1.0, 5.0]])
    assert translated["after"]["area"] == pytest.approx(
        original["after"]["area"], rel=0, abs=ATOL
    )
