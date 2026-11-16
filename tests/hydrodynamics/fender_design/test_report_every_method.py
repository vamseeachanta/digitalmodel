"""The study writes its charts, results, workbook and report for every
supported method selected in the generic case.

The report's C_e worked example uses the common illustrative geometry (PIANC
2002 4.2.4 form from the ship and berth), which every method shares; each
method's own geometry fields are listed only where that method gives them
(UFC has no R or phi, IS 4651 no C_b or K). The summary energy and the
sensitivity table caption name the selected method, and only the sweep charts
generated for that method are embedded. Outputs go to a temporary directory;
the generic case and its fictional fender rows are read only.
"""

from __future__ import annotations

import json
import re
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
    # the summary energy and the Table 5 caption are those of the selected method
    sel = _generic_result(method).selected
    label = fd.METHODS[method].label
    assert sel.label == label and data["design_energy_knm"] == pytest.approx(sel.design_energy_knm)
    row = next(m for m in data["methods"] if m["method"] == method)
    assert row["design"] == pytest.approx(sel.design_energy_knm)
    summary = (f"For the generic case the design energy by the selected method, {label}, is\n"
               f"  {fender_study._f(sel.design_energy_knm)} kNm (normal energy "
               f"{fender_study._f(sel.normal_energy_knm)} kNm).")
    assert summary in md
    assert "PIANC 2002 design energy" not in md
    assert (f"Table 5 – One-at-a-time sensitivity of the design energy by the selected method,\n"
            f"{label}, for the generic case.") in md
    assert "(PIANC 2002, generic case)" not in md
    # every embedded image exists; the sweep charts are exactly those generated
    targets = re.findall(r"!\[[^\]]*\]\(([^)]+)\)", md)
    assert targets
    for target in targets:
        assert (out / target).is_file(), target
    made = {f"charts/{name}" for key, name in data["charts"].items() if key.startswith("sweep_")}
    assert {t for t in targets if t.startswith("charts/sweep_")} == made
    assert made == {f"charts/sweep_{s['parameter']}.png" for s in data["sweeps"]}
    # figures are numbered 1..n without gaps or repeats, one per image
    numbers = [int(n) for n in re.findall(r"^Figure (\d+) –", md, flags=re.M)]
    assert sorted(numbers) == list(range(1, len(numbers) + 1))
    assert len(numbers) == len(targets)


EXPECTED_SWEEP_CHARTS = {
    "pianc2002": ("velocity_m_s", "displacement_t", "water_depth_m", "contact_x_over_lbp",
                  "berthing_angle_deg", "cs", "factor"),
    "wg211": ("velocity_m_s", "displacement_t", "water_depth_m", "contact_x_over_lbp",
              "berthing_angle_deg", "factor"),
    "ufc": ("velocity_m_s", "displacement_t", "water_depth_m", "contact_x_over_lbp",
            "berthing_angle_deg", "cc", "factor"),
    "is4651": ("velocity_m_s", "displacement_t", "water_depth_m", "contact_x_over_lbp",
               "berthing_angle_deg"),
}


@pytest.mark.parametrize("method", sorted(EXPECTED_SWEEP_CHARTS))
def test_sweep_figures_follow_the_method(method):
    res = _generic_result(method)
    made = {f"sweep_{s.parameter}": f"sweep_{s.parameter}.png" for s in res.sweeps}
    v = fender_study.sweep_figure_values(res, made)
    embedded = re.findall(r"\(charts/sweep_(\w+)\.png\)", v["sweep_figures"])
    assert tuple(embedded) == EXPECTED_SWEEP_CHARTS[method]
    first = fender_study.FIRST_SWEEP_FIGURE
    numbers = [int(n) for n in re.findall(r"^Figure (\d+) –", v["sweep_figures"], flags=re.M)]
    assert numbers == list(range(first, first + len(embedded)))
    assert v["fig_sizes"] == str(first + len(embedded))
    # a chart that was not written is not embedded
    del made["sweep_displacement_t"]
    v = fender_study.sweep_figure_values(res, made)
    assert "sweep_displacement_t" not in v["sweep_figures"]


@pytest.mark.parametrize("method", list(fd.METHODS))
def test_selected_label_names_the_selected_method(method):
    v = fender_study.method_values(_generic_result(method))
    assert v["selected_label"] == fd.METHODS[method].label


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


@pytest.mark.parametrize("basis", list(fd.UFC_F_BASES))
def test_report_states_both_ufc_f_readings(tmp_path, basis):
    p = _case_file(tmp_path, "pianc2002")
    case = yaml.safe_load(p.read_text(encoding="utf-8"))
    case["conditions"]["ufc_f_basis"] = basis
    p.write_text(yaml.safe_dump(case, sort_keys=False), encoding="utf-8")
    out = tmp_path / "out"
    assert fender_study.main(["--case", str(p), "--out", str(out), "--template",
                              str(EXAMPLE / "report_template.md"), "--stem", "r",
                              "--allow-unresolved-citations"]) == 0
    res = compute(load_case(p), citations="record")
    e = {b: fd.energy_by_method(fd.Case(res.ship, res.berth, **{**case["conditions"], "ufc_f_basis": b}),
                                method="ufc", citations="off").design_energy_knm for b in fd.UFC_F_BASES}
    assert e["berthing_coefficient"] != pytest.approx(e["block_coefficient"], rel=1e-3)
    md = (out / "r.md").read_text(encoding="utf-8")
    assert f"This build uses `{basis}`." in md
    assert (f"{e['berthing_coefficient']:,.1f} kNm under the berthing-coefficient reading and "
            f"{e['block_coefficient']:,.1f} kNm under the") in " ".join(md.split())
    data = json.loads((out / "results.json").read_text(encoding="utf-8"))
    assert data["ufc_f_basis"] == basis
    assert data["ufc_design_energy_knm_by_f_reading"] == pytest.approx(e)
    ufc = next(m for m in res.methods if m.method == "ufc")
    assert ufc.design_energy_knm == pytest.approx(e[basis])
