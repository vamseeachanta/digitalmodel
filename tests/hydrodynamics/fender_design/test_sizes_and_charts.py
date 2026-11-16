"""Neighbouring-size checks carry the case's design adjustments (tolerances,
angular factor, WG211 capacity divisor) before the limit comparison, and the
sensitivity chart shades only the contiguous intervals that are not rated.

Fender rows are the fictional illustrative rows of the generic case.
"""

from __future__ import annotations

import sys
from pathlib import Path

import pytest

from digitalmodel.hydrodynamics import fender_design as fd
from digitalmodel.hydrodynamics.berthing_energy import FenderRating

ROOT = Path(__file__).resolve().parents[3]
EXAMPLE = ROOT / "examples" / "hydrodynamics" / "fender_design"
sys.path.insert(0, str(EXAMPLE))

pytest.importorskip("yaml")
pytest.importorskip("matplotlib")

import fender_charts as fc  # noqa: E402
from fender_case import compute, load_case, size_checks  # noqa: E402

PN4 = FenderRating("GEN-PN-4", "generic", "pneumatic", 2600, 4500, 80, 700.0, 1450.0, 210.0, 60.0, "fictional")
PN5 = FenderRating("GEN-PN-5", "generic", "pneumatic", 3000, 5000, 50, 900.0, 1600.0, 150.0, 60.0, "fictional")
DESIGN = dict(energy_tolerance=0.0, reaction_tolerance=0.10, angular_factor=1.0,
              reaction_limit_kn=1600.0, hull_pressure_limit_kpa=200.0)


def test_neighbour_reaction_includes_tolerance():
    # GEN-PN-5 is rated at 1,600 kN, the limit; with +10 % it is 1,760 kN and exceeds it
    c = {x.fender_id: x for x in size_checks([PN4, PN5], "GEN-PN-4", 500.0, DESIGN)}
    assert c["GEN-PN-5"].rated_reaction_kn == 1600.0
    assert c["GEN-PN-5"].reaction_kn == pytest.approx(1760.0)
    assert c["GEN-PN-5"].meets_reaction is False
    assert c["GEN-PN-5"].hull_pressure_kpa == pytest.approx(165.0)
    assert c["GEN-PN-4"].reaction_kn == pytest.approx(1595.0)
    assert c["GEN-PN-4"].meets_reaction is True
    assert c["GEN-PN-4"].hull_pressure_kpa == pytest.approx(231.0)   # 210 x 1.10 > 200 kPa
    assert c["GEN-PN-4"].meets_pressure is False
    assert c["GEN-PN-4"].is_base and not c["GEN-PN-5"].is_base


def test_neighbour_energy_includes_reductions_and_wg211_divisor():
    d = dict(DESIGN, energy_tolerance=0.10, angular_factor=0.95)
    c = {x.fender_id: x for x in size_checks([PN4, PN5], "GEN-PN-4", 700.0, d, capacity_divisor=1.10)}
    cap = 900.0 * 0.90 * 0.95 / 1.10                 # 699.5 kNm, below the 700 kNm demand
    assert c["GEN-PN-5"].capacity_knm == pytest.approx(cap)
    assert c["GEN-PN-5"].utilisation == pytest.approx(700.0 / cap)
    assert c["GEN-PN-5"].meets_energy is False        # rated 900 kNm alone would pass
    assert not c["GEN-PN-5"].meets_all


def test_no_limit_gives_no_check():
    c = size_checks([PN5], "GEN-PN-5", 500.0, dict(energy_tolerance=0.0, reaction_tolerance=0.10))[0]
    assert c.meets_reaction is None and c.meets_pressure is None
    assert c.meets_all
    assert not c.pressure_missing


def test_missing_hull_pressure_with_a_limit_does_not_meet_it():
    # adequate energy and reaction, no hull-pressure datum, 200 kPa limit: compliance cannot be shown
    row = FenderRating("GEN-FM-X", "generic", "foam", 2800, 5000, None, 1000.0, 1400.0, None, 60.0, "fictional")
    c = size_checks([row], "GEN-FM-X", 500.0, DESIGN)[0]
    assert c.meets_energy and c.meets_reaction is True
    assert c.hull_pressure_kpa is None
    assert c.meets_pressure is False and c.pressure_missing
    assert not c.meets_all
    # the same row as select_fenders treats it: excluded under the limit, kept without one
    from digitalmodel.hydrodynamics.berthing_energy import select_fenders
    kw = dict(reaction_tolerance=0.10, max_reaction_kn=1600.0)
    assert select_fenders([row], 500.0, max_hull_pressure_kpa=200.0, **kw) == []
    assert len(select_fenders([row], 500.0, **kw)) == 1
    # without a pressure limit the missing datum is not checked and does not fail
    free = size_checks([row], "GEN-FM-X", 500.0, dict(DESIGN, hull_pressure_limit_kpa=None))[0]
    assert free.meets_pressure is None and not free.pressure_missing and free.meets_all


def test_generic_case_size_checks_match_the_base_fender_capacity():
    res = compute(load_case(EXAMPLE / "generic_case.yaml"), citations="record")
    checks = {c.fender_id: c for c in res.size_checks}
    base = checks[res.base_fender.fender_id]
    assert base.capacity_knm == pytest.approx(res.capacity_knm)
    assert base.reaction_kn == pytest.approx(res.design.reaction_kn)
    assert checks["GEN-PN-5"].reaction_kn == pytest.approx(1760.0)
    assert checks["GEN-PN-5"].meets_reaction is False
    assert res.citation_mode == "record"


def test_case_citation_mode_defaults_to_strict(tmp_path):
    case = load_case(EXAMPLE / "generic_case.yaml")
    case["citations"] = {"wiki_root": str(tmp_path)}  # a directory without the cited pages
    from digitalmodel.citations.schema import CitationResolutionError
    with pytest.raises(CitationResolutionError):
        compute(case)
    with pytest.raises(ValueError):
        compute(case, citations="lenient")


# ------------------------------------------------------------ chart shading


def test_unrated_intervals_are_contiguous_runs():
    xs = [10, 11, 12, 13, 14, 15, 16]
    rated = [False, False, True, True, False, True, True]
    assert fc.unrated_intervals(xs, rated) == [(10, 11.5), (13.5, 14.5)]
    assert fc.unrated_intervals(xs, [True] * 7) == []
    assert fc.unrated_intervals(xs, [True] * 6 + [False]) == [(15.5, 16)]
    assert fc.unrated_intervals(xs, [False] * 7) == [(10, 16)]
    with pytest.raises(ValueError):
        fc.unrated_intervals(xs, [True])


def test_water_depth_shading_leaves_the_base_point_unshaded():
    res = compute(load_case(EXAMPLE / "generic_case.yaml"), citations="record")
    sw = next(s for s in res.sweeps if s.parameter == "water_depth_m")
    pts = sw.points
    spans = fc.unrated_intervals([p.value for p in pts], [p.reaction_kn is not None for p in pts])
    assert any(p.reaction_kn is None for p in pts) and spans    # shallow water exceeds the rating
    for p in pts:
        shaded = any(lo <= p.value <= hi for lo, hi in spans)
        assert shaded == (p.reaction_kn is None), p.value
    assert sw.at(sw.base_value).reaction_kn is not None
    assert not any(lo <= sw.base_value <= hi for lo, hi in spans)


# ------------------------------------------- curve peak above the rated reaction
# A normalised curve whose reaction exceeds 100 % before the rated point means
# the rated-deflection reaction is not the largest reaction the fender reaches.
# Rows of the curve's fender type then carry the curve peak (second-provider
# review at 944a85c7: limits are checked against the peak, not an end value).
ABOVE = fd.PerformanceCurve("fictional above rated", ((0, 0, 0), (30, 110, 40), (60, 100, 100)),
                            "fictional test curve")
FM6 = FenderRating("GEN-FM-6", "generic", "foam", 2800, 5000, None, 1000.0, 1400.0, 180.0, 60.0, "fictional")


def test_neighbour_reaction_carries_the_curve_peak_for_the_curve_type():
    c = {x.fender_id: x for x in size_checks([PN4, PN5, FM6], "GEN-PN-4", 500.0, DESIGN,
                                             curve=ABOVE, curve_type="pneumatic")}
    assert c["GEN-PN-4"].reaction_kn == pytest.approx(1450.0 * 1.10 * 1.10)   # 1,754.5 kN
    assert c["GEN-PN-4"].meets_reaction is False                               # 1,595 kN alone passes
    assert c["GEN-PN-4"].reaction_peak_factor == pytest.approx(1.10)
    assert c["GEN-PN-5"].reaction_kn == pytest.approx(1600.0 * 1.10 * 1.10)
    # another type: the curve does not describe it, so the rated value stands
    assert c["GEN-FM-6"].reaction_kn == pytest.approx(1540.0)
    assert c["GEN-FM-6"].reaction_peak_factor == 1.0
    # hull pressure stays the rated-deflection value x the tolerance
    assert c["GEN-PN-4"].hull_pressure_kpa == pytest.approx(231.0)


def test_neighbour_reaction_is_unchanged_by_a_curve_peaking_at_rated():
    flat = fd.PerformanceCurve("fictional buckling", ((0, 0, 0), (30, 100, 40), (50, 60, 80), (60, 100, 100)),
                               "fictional test curve")
    a = size_checks([PN4, PN5], "GEN-PN-4", 500.0, DESIGN)
    b = size_checks([PN4, PN5], "GEN-PN-4", 500.0, DESIGN, curve=flat, curve_type="pneumatic")
    assert [x.reaction_kn for x in a] == [x.reaction_kn for x in b]


def test_compute_applies_the_curve_peak_to_sweeps_and_sizes(tmp_path):
    case = load_case(EXAMPLE / "generic_case.yaml")
    plain = compute(case, citations="record")
    curves = tmp_path / "curves.csv"
    curves.write_text(
        "curve,deflection_pct,reaction_pct,energy_pct,source\n"
        + "".join(f"{case['design']['curve']},{d},{r},{e},fictional test curve\n"
                  for d, r, e in ABOVE.points),
        encoding="utf-8")
    case["database"]["curves"] = str(curves)
    res = compute(case, citations="record")
    assert res.reaction_peak_factor(res.base_fender) == pytest.approx(1.10)
    base_type = res.base_fender.fender_type
    for a, b in zip(plain.size_checks, res.size_checks):
        f = next(x for x in res.catalogue if x.fender_id == b.fender_id)
        k = 1.10 if f.fender_type == base_type else 1.0
        assert b.reaction_kn == pytest.approx(a.reaction_kn * k), b.fender_id
    # every sweep reaction below the curve peak's energy share is at least the peak x factor
    for s in res.sweeps:
        for p in s.points:
            if p.reaction_kn is not None and p.energy_per_fender_knm >= 0.40 * s.capacity_knm:
                assert p.reaction_kn == pytest.approx(1.10 * res.base_fender.reaction_kn * res.reaction_factor)


# ------------------------------------------- method comparison: own capacity

#: velocity at which WG211 gives 660 kNm on the generic case: above the WG211
#: design capacity 700 / 1.10 = 636.364 kNm of the base fender, below its 700 kNm
V_660 = 0.115308285
WG211 = "PIANC WG211 (2024)"


def _crossing_case():
    case = load_case(EXAMPLE / "generic_case.yaml")
    case["design"]["velocity_m_s"] = V_660
    return compute(case, citations="record")


def test_method_capacity_carries_each_method_divisor():
    res = _crossing_case()
    by = {r.method: r for r in res.methods}
    assert res.method_capacity_knm(by["pianc2002"]) == pytest.approx(700.0)
    assert res.method_capacity_knm(by["wg211"]) == pytest.approx(636.364, abs=5e-4)
    assert by["wg211"].design_energy_knm == pytest.approx(660.0, abs=1e-3)
    u = res.method_utilisation(by["wg211"])
    assert round(u, 3) == 1.037 and u > 1.0
    assert res.method_utilisation(by["pianc2002"]) == pytest.approx(by["pianc2002"].design_energy_knm / 700.0)


def _bars(ax):
    """{y tick label: bar length} of a horizontal bar chart."""
    labels = {round(t, 6): lab.get_text() for t, lab in zip(ax.get_yticks(), ax.get_yticklabels())}
    return {labels[round(p.get_y() + p.get_height() / 2, 6)]: p.get_width() for p in ax.patches
            if round(p.get_y() + p.get_height() / 2, 6) in labels}


def test_method_chart_plots_utilisation_against_one(tmp_path, monkeypatch):
    import fender_study
    res = _crossing_case()
    figs = {}

    def keep(fig, path):
        figs[Path(path).name] = fig
        return Path(path)
    monkeypatch.setattr(fc, "_save", keep)
    fender_study.method_chart(res, tmp_path / "methods.png", res.base_fender.fender_id)
    ax = figs["methods.png"].axes[0]
    bars = _bars(ax)
    assert len(bars) == len(res.methods)
    # WG211 demand 660 kNm lies between 636.364 and 700 kNm: the bar is over the limit 1.0
    assert bars[WG211] == pytest.approx(660.0 / (700.0 / 1.10), abs=1e-4)
    assert bars[WG211] > 1.0
    for r in res.methods:
        assert bars[r.label] == pytest.approx(res.method_utilisation(r))
    # one limit, utilisation 1.0; no energy line drawn from one method's capacity
    limits = {round(float(ln.get_xdata()[0]), 9) for ln in ax.lines}
    assert limits == {1.0}
    assert "utilisation" in ax.get_xlabel()
    # the bar label says so in words as well (colour is not the only cue)
    assert any(t.get_text().strip().startswith("1.04") and "over capacity" in t.get_text() for t in ax.texts)


def test_methods_table_and_results_carry_the_method_capacity():
    import fender_study
    res = _crossing_case()
    row = next(ln for ln in fender_study.tables(res)["methods_table"].splitlines() if ln.startswith(f"| {WG211}"))
    assert "| 1.04 |" in row
    m = {x["method"]: x for x in fender_study.results_dict(res)["methods"]}
    assert m["wg211"]["capacity"] == pytest.approx(636.364, abs=5e-4)
    assert m["wg211"]["utilisation"] > 1.0
    assert m["pianc2002"]["capacity"] == pytest.approx(700.0)


def test_common_capacity_line_refused_for_mixed_divisors(tmp_path):
    res = _crossing_case()
    with pytest.raises(ValueError, match="method_utilisation_chart"):
        fc.method_chart(res.methods, tmp_path / "m.png", "t", capacity_knm=700.0, capacity_label="cap")


def test_common_capacity_line_kept_for_bars_sharing_one_capacity(tmp_path, monkeypatch):
    # energy steps of one method on one fender (no capacity divisor) keep the energy chart
    class Bar:
        def __init__(self, label, e):
            self.label, self.design_energy_knm = label, e
    figs = []
    monkeypatch.setattr(fc, "_save", lambda fig, path: figs.append(fig) or Path(path))
    fc.method_chart([Bar("a", 500.0), Bar("b", 650.0)], tmp_path / "steps.png", "t",
                    capacity_knm=663.0, capacity_label="cap")
    ax = figs[0].axes[0]
    assert {round(float(ln.get_xdata()[0]), 6) for ln in ax.lines} == {663.0}
    assert ax.get_xlabel() == "design energy (kNm)"


def _ufc_case(ang):
    case = load_case(EXAMPLE / "generic_case.yaml")
    case["design"].update(method="ufc", angular_factor=ang)
    return case


@pytest.mark.parametrize("ang", [0.0, 1.0000001, 1.2])
def test_compute_refuses_angular_factor_outside_0_to_1(ang):
    # UFC demand 773.0 kNm; 1.2 would have raised the 700 kNm rating to 840 kNm (utilisation 0.920)
    with pytest.raises(ValueError, match=r"angular_factor must be in \(0, 1\]"):
        compute(_ufc_case(ang), citations="record")


@pytest.mark.parametrize("ang", [0.0, 1.0000001, 1.2])
def test_size_checks_refuse_angular_factor_outside_0_to_1(ang):
    with pytest.raises(ValueError, match=r"angular_factor must be in \(0, 1\]"):
        size_checks([PN4, PN5], "GEN-PN-4", 500.0, dict(DESIGN, angular_factor=ang))


def test_compute_at_angular_factor_1_keeps_the_rated_capacity():
    res = compute(_ufc_case(1.0), citations="record")
    base = next(c for c in res.size_checks if c.is_base)
    assert res.selected.design_energy_knm == pytest.approx(773.0, abs=0.05)
    assert base.capacity_knm == pytest.approx(700.0) and res.capacity_knm == pytest.approx(700.0)
    assert base.utilisation == pytest.approx(res.selected.design_energy_knm / 700.0)
    assert base.meets_energy is False


# ------------------------------------------------------------- chart layout
# Annotations stay inside the figure, clear of each other and of the limit
# lines; footnotes are not cut (review at ecd8d308: base annotation over the
# water-depth limit label, Cs annotation clipped at the right edge, sizes
# footnote cut at the left and bar labels on the reaction limit line).


def _fig_with(*draw):
    import matplotlib.pyplot as plt
    fig, ax = plt.subplots(figsize=(4.0, 3.0), dpi=100)
    ax.set_xlim(0, 10)
    ax.set_ylim(0, 10)
    for d in draw:
        d(ax)
    fig.tight_layout()
    return fig


def test_layout_check_finds_text_outside_the_figure():
    # figure text: tight_layout does not move it, so it stays cut at the right edge
    fig = _fig_with(lambda ax: ax.figure.text(0.8, 0.5, "a label that runs off the right edge", fontsize=9))
    issues = fc.layout_issues(fig)
    assert any("outside the figure" in i for i in issues), issues


def test_layout_check_finds_a_label_outside_its_axes():
    # a label placed past the right end of the x range (the narrow unrated span at the end of a sweep)
    fig = _fig_with(lambda ax: ax.text(10.2, 1.0, "label past the axes", clip_on=False))
    issues = fc.layout_issues(fig)
    assert any("outside its axes" in i for i in issues), issues


def test_layout_check_finds_text_across_a_limit_line():
    def draw(ax):
        ax.axhline(5.0, color=fc.LIMIT, linestyle="--")
        ax.text(2.0, 5.0, "across the limit", va="center")
    issues = fc.layout_issues(_fig_with(draw))
    assert any("limit line" in i and "across the limit" in i for i in issues), issues


def test_layout_check_finds_overlapping_text():
    def draw(ax):
        ax.text(2.0, 2.0, "first label")
        ax.text(2.2, 2.0, "second label")
    issues = fc.layout_issues(_fig_with(draw))
    assert any("overlaps" in i for i in issues), issues


def test_layout_check_passes_clear_text():
    def draw(ax):
        ax.axhline(5.0, color=fc.LIMIT, linestyle="--")
        ax.text(1.0, 6.0, "above the line")
        ax.text(1.0, 2.0, "below the line")
    assert fc.layout_issues(_fig_with(draw)) == []


def _all_charts(tmp_path, case):
    import warnings

    import fender_study
    res = compute(case, citations="record")
    cnt, _, _ = fender_study.index_counts(ROOT / "data" / "fender_design" / "catalogue_index.csv")
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        made = fender_study.charts(res, tmp_path, res.base_fender.fender_id, cnt)
    return made, [str(w.message) for w in caught if issubclass(w.category, fc.ChartLayoutWarning)]


def test_every_generic_chart_has_clear_annotations(tmp_path):
    made, problems = _all_charts(tmp_path, load_case(EXAMPLE / "generic_case.yaml"))
    assert len(made) == 13                       # methods, Cm, velocity, 7 sweeps, tornado, sizes, database
    assert problems == []


@pytest.mark.parametrize("method", ["ufc", "wg211", "rom", "is4651"])
def test_charts_of_other_selected_methods_have_clear_annotations(tmp_path, method):
    case = load_case(EXAMPLE / "generic_case.yaml")
    case["design"]["method"] = method
    _, problems = _all_charts(tmp_path, case)
    assert problems == []


def test_sweep_chart_keeps_headroom_above_the_limit_and_the_data():
    import matplotlib.pyplot as plt
    res = compute(load_case(EXAMPLE / "generic_case.yaml"), citations="record")
    figs = {}
    orig = fc._finish
    fc._finish = lambda fig, path: figs.setdefault(Path(path).name, fig)
    try:
        sw = next(s for s in res.sweeps if s.parameter == "water_depth_m")
        fc.sweep_chart(sw, "wd.png", "t", "GEN-PN-4")
    finally:
        fc._finish = orig
    ax1, ax2 = figs["wd.png"].axes[:2]
    top = max([p.energy_per_fender_knm for p in sw.points] + [sw.capacity_knm])
    assert ax1.get_ylim()[1] >= 1.25 * top
    rtop = max([p.reaction_kn for p in sw.points if p.reaction_kn is not None] + [sw.reaction_limit_kn])
    assert ax2.get_ylim()[1] >= 1.25 * rtop
    plt.close(figs["wd.png"])
