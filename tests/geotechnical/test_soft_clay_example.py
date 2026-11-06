# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Reproduce the illustrative report; continuum control is in test_py_workflow."""

from pathlib import Path
import runpy

import pytest


EXAMPLE = Path(__file__).resolve().parents[2] / "examples/geotechnical/soft_clay_pile.py"


def test_example_equilibrium_softening_and_mesh_convergence():
    """The two meshes must agree within a declared 2% numerical error budget.

    This numerical repeatability target is not a material/design acceptance
    criterion. Absolute nonlinear deflection has no published validation case;
    that limitation must be visible in the example's report.
    """
    namespace = runpy.run_path(str(EXAMPLE))
    coarse = namespace["run_example"](intervals=120)
    fine = namespace["run_example"](intervals=240)
    for a, b in zip(coarse["cases"], fine["cases"]):
        assert a["head_deflection_m"] == pytest.approx(b["head_deflection_m"], rel=0.02)
        assert abs(b["force_balance_kn"]) < 1e-4
        assert abs(b["moment_balance_kn_m"]) < 1e-3
        assert b["residual_kn"] < 1e-5
    assert fine["cases"][1]["head_deflection_m"] > fine["cases"][0]["head_deflection_m"]
    assert fine["status"]["cyclic_material_calibration"] == "NOT_EVALUATED"
    assert fine["status"]["api_table_conformance"] == "NOT_EVALUATED"
    report = namespace["render_report"](fine)
    assert "Revision history" in report
    assert "assumed, based on previous experience" in report
    assert "NOT_EVALUATED" in report


@pytest.mark.parametrize("intervals", [0, 3, 1.5, True])
def test_example_rejects_invalid_mesh(intervals):
    namespace = runpy.run_path(str(EXAMPLE))
    with pytest.raises(ValueError):
        namespace["run_example"](intervals=intervals)
