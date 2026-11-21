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

import itertools
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
    # the summary energy and the Table 7 caption are those of the selected method
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
    assert (f"Table 7 – One-at-a-time sensitivity of the design energy by the selected method,\n"
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
    # ... and in the order they appear (gate P3 at 2b6bb8a0: Figures 2 and 3 came before 1)
    assert numbers == list(range(1, len(numbers) + 1))
    # the methods chart is the one numbered 5 (guide revision 6: Figures 1 and 2 are the summary and the codes)
    assert "Figure 5 – Utilisation of the base fender by method" in md
    assert "Figure 1 – What the guide holds" in md and "Figure 2 – Governing bodies and codes" in md
    # the HTML page draws every chart from its data; the Markdown keeps the static image
    page = (out / "r.html").read_text(encoding="utf-8")
    assert page.count('<figure class="ifig"') == len(targets) and 'src="charts/' not in page.split("<noscript>")[0]
    assert page.count('data-spec="') >= len(made) * 2 and "<script>" in page
    assert "**" not in md                                   # no bold: only table header rows stand apart
    # Table 7: no e-notation and no bare "-" unit (gate P3 at 2b6bb8a0)
    lines = md.splitlines()
    head = next(i for i, ln in enumerate(lines) if ln.startswith("| Input | Group | Base | Credible range"))
    sweep_rows = list(itertools.takewhile(lambda ln: ln.startswith("| "), lines[head + 2:]))
    assert len(sweep_rows) == len(data["sweeps"])
    for ln in sweep_rows:
        assert not re.search(r"\d[eE][+-]\d", ln), ln
        assert not re.search(r"\d -( |\|)", ln), ln
    # assumptions in one table, each marked
    assert "Table 14 – Assumed values" in md
    assert md.count("ASSUMED - to be confirmed") >= 5


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
    # guide revision 6: project and design sensitivities first, then the sizes figure, then the
    # operational uncertainties; every generated chart is embedded once
    design = re.findall(r"\(charts/sweep_(\w+)\.png\)", v["sweep_figures_design"])
    oper = re.findall(r"\(charts/sweep_(\w+)\.png\)", v["sweep_figures_oper"])
    assert all(fender_study.sweep_group(x) == "design" for x in design)
    assert all(fender_study.sweep_group(x) == "operational" for x in oper)
    assert sorted(design + oper) == sorted(EXPECTED_SWEEP_CHARTS[method])
    assert set(design) == set(EXPECTED_SWEEP_CHARTS[method]) & {"factor", "cs", "cc"}
    first = fender_study.FIRST_SWEEP_FIGURE
    d_numbers = [int(n) for n in re.findall(r"^Figure (\d+) –", v["sweep_figures_design"], flags=re.M)]
    o_numbers = [int(n) for n in re.findall(r"^Figure (\d+) –", v["sweep_figures_oper"], flags=re.M)]
    assert d_numbers == list(range(first, first + len(design)))
    assert v["fig_sizes"] == str(first + len(design))
    assert o_numbers == list(range(first + len(design) + 1, first + len(design) + 1 + len(oper)))
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


def test_utilisation_never_rounds_onto_the_limit():
    u = fender_study._util
    assert u(0.97263) == "0.973"
    assert u(1.0004) == "1.0004"            # gate P3 at 2b6bb8a0: printed "1.00" beside "not rated"
    assert u(0.99996) == "0.99996"
    assert u(1.0) == "1.000" and u(1.104) == "1.104" and u(None) == "-"
    s = fender_study._sig
    assert s(61_688.6) == "61,689" and s(60_000.0) == "60,000" and s(13.21197) == "13.21"
    assert s(0.1131535) == "0.1132" and s(0.25855) == "0.2586" and s(0.0) == "0"
    assert fender_study._unit("-") == "" and fender_study._unit("t") == " t"


def test_generic_report_states_the_failed_checks_and_the_solved_crossing(tmp_path):
    out = tmp_path / "out"
    assert fender_study.main(["--case", str(_case_file(tmp_path, "pianc2002")), "--out", str(out),
                              "--template", str(EXAMPLE / "report_template.md"), "--stem", "r",
                              "--allow-unresolved-citations"]) == 0
    md = (out / "r.md").read_text(encoding="utf-8")
    flat = " ".join(md.split())
    # summary: the energy and hull-pressure failures and the reaction margin of the base fender;
    # energy at the 10 % pneumatic tolerance (TFADM p. 76), with the 0 % GEA verdict beside it
    assert ("energy utilisation 1.081 against 1.0 at the 10 % pneumatic energy tolerance (not met; "
            "0.973, met, at the 0 % of SFT p. 26") in flat
    assert "design hull pressure 231 kPa against the 200 kPa limit (not met, 15.5 % over)" in flat
    # margins to three significant figures (5 / 1,600 = 0.3125 %; 60 / 1,600 = 3.75 %)
    assert "design reaction 1,595 kN against the 1,600 kN limit (met, margin 0.313 %)" in flat
    assert "design reaction 1,540 kN, margin 3.75 %, hull pressure 198 kPa, margin 1.00 %" in flat
    # revision 6: the guide; the verdict change of revision 5 is in the revision notes
    assert md.splitlines()[0] == "# Fender design guide (revision 6)"
    assert "Universal workbook" not in md and "workbook" not in md.split("## Appendix C.")[0]
    assert "Revision 5 changes a verdict of revision 4" in flat
    # Table 4: the UFC base sits on the Cg step at the quarter point; both sides are given
    assert ("UFC 4-152-01 (2017 C1): the base sits on a coefficient step; first contact 1 mm nearer "
            "midships gives 846.9 kNm (+9.6 %)") in flat
    # part-load reaction on the nominal curve (TFADM p. 76), tolerance once
    assert "read at the energy per fender over the rated energy" in flat
    assert "The base fender therefore fails the energy and hull pressure checks" in flat
    # the size statement agrees with Table 9: only the foam fender meets every check
    assert "only GEN-FM-6 meets every check" in flat
    # Table 9, with the change from the base fender in percent: 900 / 630 = +42.9 %,
    # 0.7565 / 1.0807 = -30.0 %, 1,540 / 1,595 = -3.4 % (hand values; the base row reads 0)
    assert "| GEN-FM-6 | 1,000 | 900 | 0.756 | 1,400 | 1,540 | 198 | yes | yes | yes | +43 | −30 | −3 |" in md
    assert "| GEN-PN-4 (base) | 700 | 630 | 1.081 | 1,450 | 1,595 | 231 | no | yes | no | 0 | 0 | 0 |" in md
    # method sensitivity (Table 10): 1,272.5 / 574.3 = +122 % from the lowest to the highest method
    assert "is 122 % above the lowest" in flat and "Table 10 – Method sensitivity" in md
    # Section 7 groups: project and design, operational, method
    for h in ("### 7.2 Project and design sensitivities", "#### 7.2.2 Neighbouring fender sizes",
              "### 7.3 Operational uncertainties", "### 7.4 Method sensitivity", "## 8. Insights from analyses",
              "## Appendix A. Verification", "## 2. Governing bodies and codes"):
        assert h in md, h
    assert md.index("## Appendix A. Verification") > md.index("## 10. Sources")
    # the database by vendor (Table 6): 4,652 rows in 6 vendors; Trelleborg 1,920 + 32 + 113 = 2,065 rows
    assert "(4,652 rows, 6 vendors, 26 fender types)" in flat
    assert "| Trelleborg Marine and Infrastructure | 2,065 |" in md
    # Section 4.6 records the conflict with both verdicts; Table 14 carries the choice
    assert "1.081 at 10 % (not met) and 0.973 at 0 % (met); this build takes 10 %" in flat
    assert "| Pneumatic energy tolerance 0.10 |" in md
    assert "foam tolerances" not in md                  # foam 0.10 is the printed value, not assumed
    # Section 4.4 states the keel-clearance condition of the solid-quay C_c
    assert "0.9 when KC/D is below 0.15 and 1.0 when KC/D is 0.15 or more" in flat
    # Table 7: displacement with separators, water depth solved in closed form
    assert "| 60,000 t |" in md and "55,520 t" in md
    assert "16.09 m" in md
    assert "closed form: keel-clearance Cm 1.61933" in flat
    # the UFC overlap rule, its digitization limit and the PIANC 2002 reading are stated
    assert "the larger of the two figures" in flat and "±0.015 ft/s" in flat
    assert "4.2.7 (p. 22)" in flat and "4.2.6 (p. 21)" in flat
    assert "with the design adjustments of each fender's type" in flat
    data = json.loads((out / "results.json").read_text(encoding="utf-8"))
    depth = next(s for s in data["sweeps"] if s["parameter"] == "water_depth_m")
    # KC/D = (1.875 - 1.61933) / 0.75 = 0.34089; depth = 12 x 1.34089
    assert depth["limit_value"] == pytest.approx(16.0908, abs=5e-4)
