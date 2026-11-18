# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Execute the documented example and check its independent Cartesian results."""

import json
import math
import os
import re
import subprocess
import sys
from pathlib import Path

import numpy as np
import pytest


ROOT = Path(__file__).resolve().parents[3]
DOCUMENT = ROOT / (
    "src/digitalmodel/hydrodynamics/hull_library/docs/worked-panel-scaling.md"
)
# Numerical comparison budget derived in the document's Tolerance basis section.
ABS_TOL = 1e-12


@pytest.fixture(scope="module")
def documented_example(tmp_path_factory):
    """Run the sole Python block verbatim in a separate process and directory."""
    document = DOCUMENT.read_text(encoding="utf-8")
    blocks = re.findall(r"^```python\n(.*?)^```\s*$", document, re.MULTILINE | re.DOTALL)
    assert len(blocks) == 1, "The example must have exactly one Python block"
    workdir = tmp_path_factory.mktemp("documented-panel-scaling")
    script = workdir / "example.py"
    script.write_text(blocks[0], encoding="utf-8")
    output_dir = workdir / "results"
    env = os.environ.copy()
    env["PYTHONPATH"] = str(ROOT / "src")
    env["PYTHONDONTWRITEBYTECODE"] = "1"
    completed = subprocess.run(
        [sys.executable, str(script), str(output_dir)],
        cwd=workdir,
        env=env,
        capture_output=True,
        text=True,
        # An operational bound, allowing a minute for cold package imports.
        timeout=60,
    )
    assert completed.returncode == 0, completed.stdout + completed.stderr
    assert {p.relative_to(workdir) for p in workdir.rglob("*") if p.is_file()} == {
        Path("example.py"),
        Path("results/panel-scaling.json"),
    }
    return json.loads((output_dir / "panel-scaling.json").read_text(encoding="utf-8"))


def test_documented_panel_scaling(documented_example):
    """Check vertices, reference points, areas and topology from the derivation."""
    # All expected numbers are hand-derived in the document's Derivation section.
    expected = {
        "uniform": (
            [[2, 4, -6], [10, 4, -6], [10, 10, 2], [2, 10, 2]],
            [4, -2, 6],
            80.0,
        ),
        "parametric": (
            [[3, 4, -1.5], [15, 4, -1.5], [15, 10, 0.5], [3, 10, 0.5]],
            [6, -2, 1.5],
            12 * math.sqrt(40),
        ),
    }
    for kind, (vertices, reference, area) in expected.items():
        actual = documented_example[kind]
        np.testing.assert_allclose(actual["vertices"], vertices, rtol=0, atol=ABS_TOL)
        np.testing.assert_allclose(
            actual["reference_point"], reference, rtol=0, atol=ABS_TOL
        )
        np.testing.assert_allclose(actual["panel_areas"], [area], rtol=0, atol=ABS_TOL)
        assert actual["total_area"] == pytest.approx(area, rel=0, abs=ABS_TOL)
        assert actual["panels"] == [[0, 1, 2, 3]]
        assert actual["n_vertices"] == 4
        assert actual["n_panels"] == 1


def test_documented_scaling_preserves_input(documented_example):
    """Check every source array and metadata before and after each scaling call."""
    before = documented_example["source_before"]
    # Source geometry and metadata are assumptions and hand results in the document.
    assert before["vertices"] == [[1, 2, -3], [5, 2, -3], [5, 5, 1], [1, 5, 1]]
    assert before["panels"] == [[0, 1, 2, 3]]
    assert before["reference_point"] == [2, -1, 3]
    assert before["panel_areas"] == [20.0]
    assert before["panel_centers"] == [[3, 3.5, -1]]
    np.testing.assert_allclose(before["normals"], [[0, -0.8, 0.6]], rtol=0, atol=ABS_TOL)
    assert before["metadata"] == {
        "basis": "synthetic assumptions",
        "units": {"coordinates": "m", "area": "m^2"},
    }
    assert before["name"] == "synthetic_rectangle"
    assert before["format_origin"] == "unknown"
    assert before["symmetry_plane"] is None
    assert documented_example["source_after_uniform"] == before
    assert documented_example["source_after_parametric"] == before
