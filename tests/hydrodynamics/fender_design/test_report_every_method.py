"""The study writes its charts, results, workbook and report for every
supported method selected in the generic case.

The report's C_e worked example uses the common illustrative geometry (PIANC
2002 4.2.4 form from the ship and berth), which every method shares; each
method's own geometry fields are listed only where that method gives them
(UFC has no R or phi, IS 4651 no C_b or K). Outputs go to a temporary
directory; the generic case and its fictional fender rows are read only.
"""

from __future__ import annotations

import json
import sys
from pathlib import Path

import pytest

from digitalmodel.hydrodynamics import fender_design as fd

ROOT = Path(__file__).resolve().parents[3]
EXAMPLE = ROOT / "examples" / "hydrodynamics" / "fender_design"
sys.path.insert(0, str(EXAMPLE))

pytest.importorskip("yaml")
pytest.importorskip("matplotlib")
pytest.importorskip("openpyxl")
pytest.importorskip("markdown")

import yaml  # noqa: E402

import fender_study  # noqa: E402
from fender_case import compute, load_case  # noqa: E402


def _case_file(tmp_path: Path, method: str) -> Path:
    """The generic case with only the selected method changed, its database
    paths made absolute so the copy reads the repository rows."""
    case = yaml.safe_load((EXAMPLE / "generic_case.yaml").read_text(encoding="utf-8"))
    case["design"]["method"] = method
    for k in ("catalogue", "curves"):
        case["database"][k] = str((EXAMPLE / case["database"][k]).resolve())
    p = tmp_path / f"case-{method}.yaml"
    p.write_text(yaml.safe_dump(case, sort_keys=False), encoding="utf-8")
    return p


def _generic_result(method: str):
    case = load_case(EXAMPLE / "generic_case.yaml")
    case["design"]["method"] = method
    return compute(case, citations="record")


@pytest.mark.parametrize("method", list(fd.METHODS))
def test_report_generated_for_every_method(tmp_path, method):
    out = tmp_path / "out"
    rc = fender_study.main([
        "--case", str(_case_file(tmp_path, method)), "--out", str(out),
        "--template", str(EXAMPLE / "report_template.md"), "--stem", "r",
        "--allow-unresolved-citations",
    ])
    assert rc == 0
    for name in ("r.md", "r.html", "r.xlsx", "results.json"):
        assert (out / name).stat().st_size > 0, name
    data = json.loads((out / "results.json").read_text(encoding="utf-8"))
    assert data["selected"] == method
    md = (out / "r.md").read_text(encoding="utf-8")
    assert "{{" not in md
    # the common geometry is the same whatever the method
    common = fd.eccentricity(_generic_result(method).ship, _generic_result(method).berth)
    assert f"C_b {common['cb']:.3f}, K {common['k_m']:.2f} m, R {common['r_m']:.2f} m" in md
    assert f"C_e {common['ce']:.3f}" in md


def test_geometry_values_separate_common_and_method_fields():
    ufc = fender_study.geometry_values(_generic_result("ufc"))
    pianc = fender_study.geometry_values(_generic_result("pianc2002"))
    is4651 = fender_study.geometry_values(_generic_result("is4651"))
    # common illustrative geometry: identical for every method
    keys = ("cb", "K", "R", "phi", "ce", "ce_err")
    assert {k: ufc[k] for k in keys} == {k: pianc[k] for k in keys} == {k: is4651[k] for k in keys}
    assert pianc["R"] == "47.76" and pianc["ce"] == "0.595"
    # method fields: only those the method gives
    assert "`a_m` 45.000" in ufc["selected_geometry"] and "`cb_berthing`" in ufc["selected_geometry"]
    assert "`r_m`" not in ufc["selected_geometry"] and "`phi_deg`" not in ufc["selected_geometry"]
    assert "`l_m` 45.000" in is4651["selected_geometry"] and "`cb`" not in is4651["selected_geometry"]
    assert "`k_m`" not in is4651["selected_geometry"]
    assert ufc["selected_method"] == "ufc" and is4651["selected_method"] == "is4651"


def test_selected_pianc_geometry_matches_the_common_values():
    res = _generic_result("pianc2002")
    v = fender_study.geometry_values(res)
    g = res.selected.geometry
    assert v["cb"] == f"{g['cb']:.3f}" and v["K"] == f"{g['k_m']:.2f}"
    assert v["R"] == f"{g['r_m']:.2f}" and v["phi"] == f"{g['phi_deg']:.1f}" and v["ce"] == f"{g['ce']:.3f}"
