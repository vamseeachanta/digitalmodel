# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Rebuild the separate diffraction report section and check its hand counts."""

from pathlib import Path
import runpy

ROOT = Path(__file__).resolve().parents[3]


def test_report_rebuild(tmp_path):
    """Hand enumeration: 33 explicit cards, 33 rows; one deletion leaves 32."""
    builder = runpy.run_path(str(ROOT / "scripts/reporting/build_aqwa_lis_grid_report.py"))
    data = builder["build"](tmp_path / "report", tmp_path / "cases")
    assert data["declared"] == data["parsed"] == 33
    assert data["damaged"] == {"reason": "count_mismatch", "expected": 33, "actual": 32}
    assert data["baseline_gap_count"] == 10
    assert data["old_window_count"] < data["parsed"]
    for name in ("report.html", "changes.json", "evidence.json"):
        assert (tmp_path / "report" / name).read_bytes() == (
            ROOT / "docs/reports/modules/diff/lis-grid" / name).read_bytes()
    text = (tmp_path / "report/report.html").read_text()
    assert text.index("Summary") < text.index("Revision history") < text.index("Inputs")
    assert text.count('data-period-row=') == 33
    assert "NOT_EVALUATED" in text
    assert 'role="img"' in text
    assert 'data-count="33"' in text
