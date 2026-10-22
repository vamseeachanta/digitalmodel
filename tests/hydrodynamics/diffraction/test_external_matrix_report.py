# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Rebuild the external matrix report and check independent arithmetic."""

import importlib.util
import json
from pathlib import Path

import numpy as np


ROOT = Path(__file__).resolve().parents[3]
REPORT = ROOT / "docs/reports/modules/diffraction/external-matrices"


def test_report_rebuild_and_independent_answers(tmp_path):
    """K_G=1000*diag(2,3,5,7,11,13), r=(0,0,2).

    Expanding 2(x+2*pitch)^2 + 3(y-2*roll)^2 gives K_O/1000 diagonal
    (2,3,5,19,19,13), K15/1000=4 and K24/1000=-6. AQWA transfer
    back to G cancels the offset; its SI damping remains the input diagonal.
    OrcaWave emits K_O/1000 in kN units. All numbers are synthetic inputs
    or independent polynomial expansion, not physical acceptance criteria.
    """
    path = ROOT / "scripts/reporting/build_diffraction_external_matrices_report.py"
    spec = importlib.util.spec_from_file_location("external_matrix_report", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    module.build(tmp_path / "first", tmp_path / "native-first")
    module.build(tmp_path / "second", tmp_path / "native-second")
    for name in ("report.html", "changes.json", "results.json"):
        actual = (tmp_path / "first" / name).read_bytes()
        assert actual == (tmp_path / "second" / name).read_bytes()
        assert actual == (REPORT / name).read_bytes()
    results = json.loads((tmp_path / "first/results.json").read_text())
    expected = np.diag([2000., 3000., 5000., 19000., 19000., 13000.])
    expected[0, 4] = expected[4, 0] = 4000.
    expected[1, 3] = expected[3, 1] = -6000.
    np.testing.assert_array_equal(results["body_origin_matrix"], expected)
    np.testing.assert_array_equal(results["aqwa_record"]["native_values"],
                                  np.diag([2000., 3000., 5000., 7000., 11000., 13000.]))
    assert "NOT_EVALUATED" in results["aqwa_stiffness_refusal"]
    html = (tmp_path / "first/report.html").read_text()
    for required in ("Revision history", "Inputs", "Method", "Worked example",
                     "Limits of use", "NOT_EVALUATED", "FIDP", "BodyExternalDamping",
                     "N.s/rad", "N.m/m", "<svg", "Sources"):
        assert required in html
    assert (html.index("<h2>Summary") < html.index("<h3>Revision history")
            < html.index("<h2>Inputs"))
