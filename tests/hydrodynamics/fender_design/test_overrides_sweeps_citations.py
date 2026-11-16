"""Velocity overrides recompute velocity-dependent coefficients; sweeps follow
each method's coefficient rules; method results carry a citation sidecar that
is resolved against the wiki (fail-closed by default; ``record`` and ``off`` are
explicit, flagged opt-outs).

Ship and berth values are the generic round numbers of the method report.
"""

from __future__ import annotations

import math

import pytest

from digitalmodel.citations.schema import CitationResolutionError
from digitalmodel.hydrodynamics import fender_design as fd
from digitalmodel.hydrodynamics.berthing_energy import FenderRating

SHIP = fd.Ship(displacement_t=60_000, lbp_m=180.0, beam_m=32.0, draft_m=12.0, ship_type="bulk", dwt_t=48_000)
BERTH = fd.Berth(water_depth_m=14.0, contact_x_from_bow_m=45.0, berthing_angle_deg=5.0, navigation="b",
                 structure="open", fender_deflection_m=1.56)
CASE = fd.Case(SHIP, BERTH, condition="moderate")
FENDER = FenderRating("GEN-PN-4", "generic", "pneumatic", 2600, 4500, 80, 700.0, 1450.0, 210.0, 60.0,
                      "fictional test row")

# ------------------------------------------------------------ velocity override


def test_rom_override_recomputes_accidental_factor():
    # ROM Table 4.6.4.43: v <= 0.10 m/s multiplies the factor by 1.20 -> 1.50 x 1.20 = 1.80
    own = fd.energy_by_method(CASE, method="rom", citations="off")
    assert own.factor.value == pytest.approx(1.50)
    low = fd.energy_by_method(CASE, method="rom", velocity_m_s=0.08, citations="off")
    assert low.velocity_m_s.source == "caller"
    assert low.factor.value == pytest.approx(1.80)
    assert low.factor.value == pytest.approx(fd.rom_accidental_factor(60_000, True, 0.08, 100))
    assert low.design_energy_knm == pytest.approx(low.normal_energy_knm * 1.80)
    # the same as ROM's own result where its table gives 0.08 m/s (favourable, 60,000 t)
    fav = fd.energy_by_method(fd.Case(SHIP, BERTH, condition="favourable"), method="rom", citations="off")
    assert fav.velocity_m_s.value == pytest.approx(0.08)
    assert low.design_energy_knm == pytest.approx(fav.design_energy_knm, rel=1e-12)
    assert low.factor.value == fav.factor.value and low.cc.value == fav.cc.value


def test_rom_override_recomputes_cc_on_a_solid_quay():
    quay = fd.Berth(water_depth_m=14.0, contact_x_from_bow_m=45.0, berthing_angle_deg=0.0, structure="closed")
    c = fd.Case(SHIP, quay, condition="unfavourable")
    # Cc 0.9 needs v >= 0.20 (p. 271): 0.25 gives 0.9, 0.15 gives 1.0
    assert fd.energy_by_method(c, method="rom", velocity_m_s=0.25, citations="off").cc.value == 0.9
    assert fd.energy_by_method(c, method="rom", velocity_m_s=0.15, citations="off").cc.value == 1.0


def test_velocity_flags_follow_the_override():
    assert not any(f.startswith("below 0.") for f in fd.energy_by_method(CASE, method="rom", citations="off").flags)
    assert any("not recommended" in f for f in fd.energy_by_method(CASE, method="rom", velocity_m_s=0.07, citations="off").flags)
    bs = fd.energy_by_method(CASE, method="bs6349", velocity_m_s=0.07, citations="off")
    assert any("v < 0.08" in f for f in bs.flags)


def test_factor_override_keeps_effective_velocity_coefficients():
    r = fd.energy_by_method(CASE, method="rom", velocity_m_s=0.08, factor=1.3, citations="off")
    assert r.factor.value == 1.3 and r.factor.source == "caller"
    assert r.design_energy_knm == pytest.approx(r.normal_energy_knm * 1.3)


# --------------------------------------------------------------------- sweeps

RANGES = {
    "velocity_m_s": fd.SweepRange(0.06, 0.17, "test"),
    "cs": fd.SweepRange(0.9, 1.0, "test"),
    "cc": fd.SweepRange(0.8, 1.0, "test"),
    "factor": fd.SweepRange(1.25, 1.75, "test"),
}


def test_sweeps_leave_out_coefficients_the_method_removes():
    keys = lambda method: [s.parameter for s in fd.standard_sweeps(CASE, method, FENDER, RANGES, citations="off")]  # noqa: E731
    assert keys("wg211") == ["velocity_m_s", "factor"]
    assert keys("pianc2002") == ["velocity_m_s", "cs", "factor"]      # keel-clearance Cm pairs with Cc = 1
    assert keys("eau") == ["velocity_m_s", "cs", "factor"]
    assert keys("bs6349") == ["velocity_m_s", "cs", "cc", "factor"]
    assert keys("ufc") == ["velocity_m_s", "cc", "factor"]
    assert keys("is4651") == ["velocity_m_s"]
    assert set(fd.inapplicable_sweeps("wg211")) == {"cs", "cc"}
    assert "pairing" in fd.inapplicable_sweeps("pianc2002")["cc"]


def test_sweep_coefficient_ranges_clipped_to_method_rule():
    rom = {s.parameter: s for s in fd.standard_sweeps(CASE, "rom", FENDER, RANGES, citations="off")}
    cc = rom["cc"]
    assert (cc.range_low, cc.range_high) == (0.9, 1.0)
    assert "clipped to 0.9 to 1" in cc.range_basis
    assert min(p.value for p in cc.points) >= 0.9 and max(p.value for p in cc.points) <= 1.0
    bs = {s.parameter: s for s in fd.standard_sweeps(CASE, "bs6349", FENDER, RANGES, citations="off")}
    assert (bs["cc"].range_low, bs["cc"].range_high) == (0.8, 1.0) and "clipped" not in bs["cc"].range_basis


#: Generic case (PIANC 2002, Cs 1.0, 680.9 kNm) against a 600 kNm capacity
#: (second-provider review at 39742345): Cs 0.8 would give 544.7 kNm
#: (utilisation 0.908); the smallest Cs the method gives, 0.9, gives
#: 612.8 kNm (1.021).
CAP_600 = dict(capacity_knm=600.0, citations="off")


@pytest.mark.parametrize("low, high", [(0.7, 0.8), (1.05, 1.2)], ids=["below", "above"])
def test_disjoint_coefficient_range_is_refused(low, high):
    with pytest.raises(ValueError, match="0.9 to 1"):
        fd.standard_sweeps(CASE, "pianc2002", FENDER, {"cs": fd.SweepRange(low, high, "test")}, **CAP_600)


def test_disjoint_range_refused_before_any_point_is_evaluated(monkeypatch):
    # a valid sweep listed first is not computed when a later range is disjoint
    calls = []
    real = fd.energy_by_method

    def counting(*a, **k):
        calls.append(k.get("velocity_m_s"))
        return real(*a, **k)
    monkeypatch.setattr(fd, "energy_by_method", counting)
    ranges = {"velocity_m_s": fd.SweepRange(0.06, 0.17, "test"), "cs": fd.SweepRange(0.7, 0.8, "test")}
    with pytest.raises(ValueError):
        fd.standard_sweeps(CASE, "pianc2002", FENDER, ranges, **CAP_600)
    assert calls == []              # refused before the base result or any swept point


def test_disjoint_range_refused_for_cc():
    with pytest.raises(ValueError, match="0.9 to 1"):
        fd.standard_sweeps(CASE, "rom", FENDER, {"cc": fd.SweepRange(0.7, 0.85, "test")}, citations="off")


def test_range_touching_the_limit_gives_the_limit_point():
    s = fd.standard_sweeps(CASE, "pianc2002", FENDER, {"cs": fd.SweepRange(0.8, 0.9, "test")}, **CAP_600)[0]
    assert (s.range_low, s.range_high) == (0.9, 0.9)
    assert min(p.value for p in s.points) == pytest.approx(0.9)


def test_partly_overlapping_range_is_clipped_and_never_evaluated_outside():
    s = fd.standard_sweeps(CASE, "pianc2002", FENDER, {"cs": fd.SweepRange(0.8, 0.95, "test")}, **CAP_600)[0]
    assert (s.range_low, s.range_high) == (0.9, 0.95)
    assert "clipped to 0.9 to 1" in s.range_basis
    assert min(p.value for p in s.points) >= 0.9 and max(p.value for p in s.points) <= 1.0
    with pytest.raises(KeyError):
        s.at(0.8)
    low = s.at(s.range_low)
    assert round(low.design_energy_knm, 1) == 612.8
    assert round(low.utilisation, 3) == 1.021 and low.utilisation > 1.0


def test_inverted_range_is_refused():
    with pytest.raises(ValueError, match="low .* above high"):
        fd.standard_sweeps(CASE, "pianc2002", FENDER, {"velocity_m_s": fd.SweepRange(0.17, 0.06, "test")},
                           citations="off")
    with pytest.raises(ValueError, match="low .* above high"):
        fd.standard_sweeps(CASE, "pianc2002", FENDER, {"cs": fd.SweepRange(0.95, 0.92, "test")},
                           citations="off")


def test_range_of_an_excluded_sweep_is_not_checked():
    # the method does not sweep Cs at all, so its range is not constrained
    assert [s.parameter for s in fd.standard_sweeps(
        CASE, "wg211", FENDER, {"cs": fd.SweepRange(0.7, 0.8, "test"), "factor": RANGES["factor"]},
        citations="off")] == ["factor"]


def test_velocity_sweep_recomputes_rom_factor():
    sw = next(s for s in fd.standard_sweeps(CASE, "rom", FENDER, RANGES, citations="off") if s.parameter == "velocity_m_s")
    for p in sw.points:
        expect = fd.energy_by_method(CASE, method="rom", velocity_m_s=p.value, citations="off").design_energy_knm
        assert p.design_energy_knm == pytest.approx(expect, rel=1e-12)
    # across 0.10 m/s the factor steps from 1.80 to 1.50
    e = {round(p.value, 6): p.design_energy_knm / p.value**2 for p in sw.points}
    lo = [k for k in e if k <= 0.10]
    hi = [k for k in e if k > 0.10]
    assert e[max(lo)] / e[min(hi)] == pytest.approx(1.80 / 1.50, rel=1e-9)


def test_standard_sweeps_carry_reaction_factor():
    curve = fd.PerformanceCurve("c", ((0, 0, 0), (30, 27, 16), (60, 100, 100)), "fictional")
    kw = dict(curve=curve, reaction_limit_kn=1600.0, capacity_knm=700.0, citations="off")
    nom = fd.standard_sweeps(CASE, "pianc2002", FENDER, RANGES, **kw)
    tol = fd.standard_sweeps(CASE, "pianc2002", FENDER, RANGES, reaction_factor=1.10, **kw)
    for a, b in zip(nom, tol):
        assert b.reaction_factor == 1.10
        for pa, pb in zip(a.points, b.points):
            assert pa.design_energy_knm == pb.design_energy_knm
            if pa.reaction_kn is not None:
                assert pb.reaction_kn == pytest.approx(1.10 * pa.reaction_kn)
                assert pb.hull_pressure_kpa == pytest.approx(1.10 * 210.0)


# ------------------------------------------------- default sweep capacity

#: velocity at which WG211 gives 660 kNm on the generic ship (second-provider
#: review at 6c4bfd70): between the design capacity 700 / 1.10 = 636.364 kNm
#: and the rated energy 700 kNm of the generic fender
V_660 = 0.115308285


def test_wg211_default_sweep_capacity_is_divided_by_gamma_m():
    sweeps = fd.standard_sweeps(CASE, "wg211", FENDER, RANGES, velocity_m_s=V_660, citations="off")
    assert sweeps
    for s in sweeps:
        assert s.capacity_knm == pytest.approx(700.0 / fd.WG211_GAMMA_M)
        assert s.capacity_knm == pytest.approx(636.364, abs=5e-4)
    v = next(s for s in sweeps if s.parameter == "velocity_m_s")
    base = v.at(v.base_value)
    assert base.design_energy_knm == pytest.approx(660.0, abs=1e-3)
    assert base.utilisation == pytest.approx(660.0 / 636.364, abs=1e-4)
    assert round(base.utilisation, 3) == 1.037
    assert base.utilisation > 1.0
    # over capacity: the curve does not rate the reaction there
    assert base.reaction_kn is None
    # utilisation 1 is reached below the base velocity
    assert v.limit_value() < V_660


def test_wg211_default_capacity_equals_the_explicit_design_capacity():
    kw = dict(velocity_m_s=V_660, citations="off")
    dflt = fd.standard_sweeps(CASE, "wg211", FENDER, RANGES, **kw)
    expl = fd.standard_sweeps(CASE, "wg211", FENDER, RANGES, capacity_knm=700.0 / 1.10, **kw)
    for a, b in zip(dflt, expl):
        assert a.capacity_knm == pytest.approx(b.capacity_knm)
        assert [p.utilisation for p in a.points] == pytest.approx([p.utilisation for p in b.points])


def test_explicit_sweep_capacity_is_not_divided_again():
    # a caller's capacity is already the design capacity (fender_case.compute
    # passes rated x reductions / gamma_m): it is used as given
    s = fd.standard_sweeps(CASE, "wg211", FENDER, RANGES, velocity_m_s=V_660, capacity_knm=636.364,
                           citations="off")[0]
    assert s.capacity_knm == 636.364


def test_wg33_default_sweep_capacity_is_unchanged():
    # PIANC 2002 (WG33) has no material divisor: the default stays the rated energy
    sweeps = fd.standard_sweeps(CASE, "pianc2002", FENDER, RANGES, citations="off")
    for s in sweeps:
        assert s.capacity_knm == 700.0
    v = next(s for s in sweeps if s.parameter == "velocity_m_s")
    base = v.at(v.base_value)
    assert base.utilisation == pytest.approx(base.energy_per_fender_knm / 700.0)
    assert base.utilisation == pytest.approx(0.9726, abs=1e-4)
    # at the WG211 crossing velocity a WG33 utilisation is energy / 700 as before
    w = fd.standard_sweeps(CASE, "pianc2002", FENDER, RANGES, velocity_m_s=V_660, citations="off")[0]
    assert w.at(w.base_value).utilisation == pytest.approx(w.at(w.base_value).design_energy_knm / 700.0)


# ------------------------------------------------------------------ citations


def _page(root, citation, **override):
    fm = dict(code_id=citation.code_id, publisher=citation.publisher, revision=citation.revision)
    fm.update(override)
    path = root / citation.wiki_path
    path.parent.mkdir(parents=True, exist_ok=True)
    body = "".join(f'{k}: "{v}"\n' for k, v in fm.items())
    path.write_text(f"---\n{body}---\n\n# {citation.code_id}\n", encoding="utf-8")


def test_every_method_coefficient_source_has_a_citation_target():
    for r in fd.compare_methods(CASE, citations="off"):
        for c in r.coefficients():
            assert c.source in fd.CITATION_TARGETS, (r.method, c.name, c.source)
        assert r.citations, r.method
        assert {c.code_id for c in r.citations} == {code for code, _ in r.citation_status}


def test_default_entry_point_fails_closed_on_missing_page(tmp_path):
    # calculation-output citation contract 4.1: a missing page raises at calculation time
    with pytest.raises(CitationResolutionError, match="PIANC-WG33-2002.*page_missing"):
        fd.energy_by_method(CASE, method="pianc2002", citation_root=tmp_path)
    with pytest.raises(CitationResolutionError, match="page_missing"):
        fd.energy_by_method(SHIP, BERTH, "ufc", citation_root=tmp_path)
    # compare_methods does not swallow it (it skips only ValueError / KeyError)
    with pytest.raises(CitationResolutionError):
        fd.compare_methods(CASE, citation_root=tmp_path)


def test_default_entry_point_fails_closed_without_a_wiki(tmp_path, monkeypatch):
    # no override and LLM_WIKI_PATH pointing at a directory that is not a wiki clone
    monkeypatch.setenv("LLM_WIKI_PATH", str(tmp_path))
    with pytest.raises(CitationResolutionError):
        fd.energy_by_method(CASE, method="pianc2002")
    assert fd.resolve_citations.__kwdefaults__["mode"] == "strict"
    assert fd.energy_by_method.__kwdefaults__["citations"] == "strict"


def test_record_opt_out_flags_missing_page(tmp_path):
    r = fd.energy_by_method(CASE, method="pianc2002", citations="record", citation_root=tmp_path)
    assert r.citation_status and all(s == "unresolved: page_missing" for _, s in r.citation_status)
    assert not r.citations_resolved
    flag = next(f for f in r.flags if f.startswith("citation not resolved"))
    assert "PIANC-WG33-2002 (page_missing)" in flag and "explicit opt-out" in flag
    # numbers unchanged by the qualification
    off = fd.energy_by_method(CASE, method="pianc2002", citations="off")
    assert r.design_energy_knm == off.design_energy_knm
    assert all(s == "not checked" for _, s in off.citation_status)
    assert fd.CITATIONS_NOT_CHECKED in off.flags


def test_strict_mode_fails_closed_on_missing_page(tmp_path):
    with pytest.raises(CitationResolutionError, match="PIANC-WG33-2002.*page_missing"):
        fd.energy_by_method(CASE, method="pianc2002", citations="strict", citation_root=tmp_path)


def test_resolved_page_clears_the_flag_in_the_default_mode(tmp_path):
    _page(tmp_path, fd.citation_for("pianc2002"))
    r = fd.energy_by_method(CASE, method="pianc2002", citation_root=tmp_path)
    assert r.citations_resolved
    assert all(s == "resolved" for _, s in r.citation_status)
    assert not any(f.startswith("citation not resolved") or f == fd.CITATIONS_NOT_CHECKED for f in r.flags)


@pytest.mark.parametrize("field,value", [("revision", "2024"), ("publisher", "Other"), ("code_id", "PIANC-WG33")])
@pytest.mark.parametrize("mode", ["strict", "record"])
def test_mismatched_page_fails_closed_in_every_checking_mode(tmp_path, field, value, mode):
    _page(tmp_path, fd.citation_for("pianc2002"), **{field: value})
    with pytest.raises(CitationResolutionError, match="frontmatter_mismatch"):
        fd.energy_by_method(CASE, method="pianc2002", citations=mode, citation_root=tmp_path)
    # a mismatch is not swallowed by compare_methods either
    with pytest.raises(CitationResolutionError):
        fd.compare_methods(CASE, methods=["pianc2002"], citations=mode, citation_root=tmp_path)


def test_page_without_frontmatter_fails_closed(tmp_path):
    c = fd.citation_for("rom-2.0-11")
    path = tmp_path / c.wiki_path
    path.parent.mkdir(parents=True)
    path.write_text("# ROM 2.0-11\n", encoding="utf-8")
    with pytest.raises(CitationResolutionError, match="frontmatter_missing"):
        fd.energy_by_method(CASE, method="rom", citations="record", citation_root=tmp_path)


def test_caller_values_carry_no_citation():
    r = fd.energy_by_method(CASE, method="wg211", velocity_m_s=0.12, factor=1.5, citations="off")
    sections = {c.section for c in r.citations}
    assert all(c.source != "caller" for c in r.coefficients() if c.name not in ("velocity_m_s", "partial_energy_factor"))
    assert r.velocity_m_s.source == "caller" and r.factor.source == "caller"
    # WG211 Cs and Cc still cite WG211; the keel-clearance Cm cites WG33
    assert {c.code_id for c in r.citations} == {"PIANC-WG211-2024", "PIANC-WG33-2002"}
    assert sections


def test_citation_mode_validated():
    with pytest.raises(ValueError):
        fd.energy_by_method(CASE, method="pianc2002", citations="lenient")


# ------------------------------------------- UFC Cc sweep: dependent added mass

#: The generic ship at a solid quay, parallel approach (5 deg): UFC Cc 0.8.
CLOSED = fd.Case(SHIP, fd.Berth(water_depth_m=14.0, contact_x_from_bow_m=45.0, berthing_angle_deg=5.0,
                                navigation="b", structure="closed", fender_deflection_m=1.56),
                 condition="moderate")


def _ufc_by_hand(cc):
    """UFC 4-152-01 (2017, Change 1 2024) 5-2.1.1 worked by hand for CLOSED.

    E = 1/2 M V^2 Cb Cm (Eqs. 5-1, 5-2), Cb = Ce Cg Cd Cc (p. 106);
    Cm = C_M0 + (C_m1 - C_M0)(T/d)^3.5, C_M0 = 1.3 + 1.5 T/B,
    C_m1 = F [12.4 (T/B)^0.3 - 50 T/L], F = 1.5 Cb for Cb < 0.6, else 0.9,
    "Cb = berthing coefficient as defined previously" (p. 109). Design energy
    is 1.5 x normal (5-1.5.3).
    """
    m, l, b, t, d = 60_000.0, 180.0, 32.0, 12.0, 14.0
    c_bl = m / (l * b * t * 1.025)                  # block coefficient 0.8469
    k = (0.19 * c_bl + 0.11) * l                    # 48.763 m
    a = 90.0 - 45.0                                 # contact 45 m from bow, 45 m from midships
    ce = k * k / (a * a + k * k)                    # 0.54007
    cg, cd = 0.95, 1.0                              # at the quarter point; larger ship
    # Fig. 5-4, moderate: 0.35 ft/s at 50,000 LT, 0.30 ft/s at 70,000 LT, log-log
    lt = m / 1.0160469088
    s = math.log(lt / 50_000) / math.log(70_000 / 50_000)
    v = math.exp(math.log(0.35) + s * math.log(0.30 / 0.35)) * 0.3048   # 0.09885 m/s
    cb = ce * cg * cd * cc
    f = 1.5 * cb if cb < 0.6 else 0.9
    cm0 = 1.3 + 1.5 * t / b
    cm1 = f * (12.4 * (t / b) ** 0.3 - 50.0 * t / l)
    cm = cm0 + (cm1 - cm0) * (t / d) ** 3.5
    return 1.5 * 0.5 * m * v * v * cb * cm, cm


def test_ufc_hand_check_closed_berth():
    e08, cm08 = _ufc_by_hand(0.8)
    e10, cm10 = _ufc_by_hand(1.0)
    assert (e08, cm08) == (pytest.approx(522.767, abs=1e-3), pytest.approx(2.89655, abs=1e-5))
    assert (e10, cm10) == (pytest.approx(773.023, abs=1e-3), pytest.approx(3.42653, abs=1e-5))
    base = fd.energy_by_method(CLOSED, method="ufc", citations="off")
    assert base.cc.value == 0.8
    assert base.design_energy_knm == pytest.approx(e08, rel=1e-9)
    assert base.cm.value == pytest.approx(cm08, rel=1e-9)


def test_ufc_cc_override_recomputes_added_mass():
    e10, cm10 = _ufc_by_hand(1.0)
    r = fd.energy_by_method(CLOSED, method="ufc", cc=1.0, citations="off")
    assert r.cc.value == 1.0 and r.cc.source == "caller"
    assert r.cm.value == pytest.approx(cm10, rel=1e-9)
    assert r.geometry["cb_berthing"] == pytest.approx(r.ce.value * 0.95 * 1.0 * 1.0, rel=1e-12)
    assert r.design_energy_knm == pytest.approx(e10, rel=1e-9)
    # the same as UFC's own result where its rule gives Cc 1.0 (open berth)
    open_ = fd.energy_by_method(CASE, method="ufc", citations="off")
    assert open_.cc.value == 1.0
    assert r.design_energy_knm == pytest.approx(open_.design_energy_knm, rel=1e-12)


def test_ufc_cc_sweep_recomputes_cm_and_crosses_capacity():
    # Codex review at 319e3d45: holding Cm gave 653.5 kNm (0.934) at Cc 1.0; recomputed it is 773.0 (1.104)
    sw = next(s for s in fd.standard_sweeps(CLOSED, "ufc", FENDER, {"cc": fd.SweepRange(0.8, 1.0, "test")},
                                            capacity_knm=700.0, citations="off"))
    assert sw.parameter == "cc"
    for p in sw.points:
        assert p.design_energy_knm == pytest.approx(_ufc_by_hand(p.value)[0], rel=1e-9), p.value
    hi = sw.at(sw.range_high)
    assert hi.design_energy_knm == pytest.approx(773.023, abs=1e-3)
    assert hi.utilisation == pytest.approx(773.023 / 700.0, abs=1e-6)
    assert hi.utilisation > 1.0 > sw.at(sw.base_value).utilisation
    # capacity reached at Cc 0.9454 (bisection of the hand formula); the linear scaling never reached it
    assert sw.limit_value() == pytest.approx(0.9454, abs=0.002)


def test_cc_override_scales_linearly_where_nothing_depends_on_cc():
    bs = fd.energy_by_method(CLOSED, method="bs6349", citations="off")
    hi = fd.energy_by_method(CLOSED, method="bs6349", cc=1.0, citations="off")
    assert bs.cc.value == 0.8 and hi.cm.value == bs.cm.value
    assert hi.design_energy_knm == pytest.approx(bs.design_energy_knm / 0.8, rel=1e-12)


@pytest.mark.parametrize("method", ["pianc2002", "eau", "wg211", "is4651"])
def test_cc_override_refused_where_the_method_fixes_cc(method):
    with pytest.raises(ValueError, match="Cc"):
        fd.energy_by_method(CASE, method=method, cc=0.9, citations="off")


# ------------------------------------------------ sweeps: citation enforcement


def _counting_rejector(calls):
    def reject(citation, repo_root=None):
        calls.append(citation.code_id)
        raise CitationResolutionError(code_id=citation.code_id, wiki_path=citation.wiki_path,
                                      reason="frontmatter_mismatch: forced by the test")
    return reject


@pytest.mark.parametrize("mode", ["strict", "record"])
def test_standard_sweeps_call_the_resolver_and_raise_when_validation_rejects(monkeypatch, mode):
    calls = []
    monkeypatch.setattr(fd, "validate_citation", _counting_rejector(calls))
    with pytest.raises(CitationResolutionError, match="frontmatter_mismatch"):
        fd.standard_sweeps(CASE, "pianc2002", FENDER, RANGES, citations=mode)
    assert calls


def test_standard_sweeps_fail_closed_by_default(tmp_path):
    assert fd.standard_sweeps.__kwdefaults__["citations"] == "strict"
    with pytest.raises(CitationResolutionError, match="page_missing"):
        fd.standard_sweeps(CASE, "pianc2002", FENDER, RANGES, citation_root=tmp_path)


def test_standard_sweeps_record_mode_carries_the_qualification(tmp_path):
    sweeps = fd.standard_sweeps(CASE, "ufc", FENDER, RANGES, citations="record", citation_root=tmp_path)
    assert sweeps
    for s in sweeps:
        assert s.citation_status and all(st == "unresolved: page_missing" for _, st in s.citation_status)
        assert any(f.startswith("citation not resolved") and "UFC-4-152-01" in f for f in s.flags)


def test_standard_sweeps_resolved_pages_carry_no_flag(tmp_path):
    for c in fd.energy_by_method(CASE, method="ufc", citations="off").citations:
        _page(tmp_path, c)
    for s in fd.standard_sweeps(CASE, "ufc", FENDER, RANGES, citation_root=tmp_path):
        assert s.citation_status and all(st == "resolved" for _, st in s.citation_status)
        assert not s.flags


def test_standard_sweeps_off_is_explicit_and_flagged(monkeypatch):
    calls = []
    monkeypatch.setattr(fd, "validate_citation", _counting_rejector(calls))
    sweeps = fd.standard_sweeps(CASE, "pianc2002", FENDER, RANGES, citations="off")
    assert sweeps and not calls
    for s in sweeps:
        assert fd.CITATIONS_NOT_CHECKED in s.flags
        assert all(st == "not checked" for _, st in s.citation_status)


def test_standard_sweeps_reuse_a_checked_base(tmp_path, monkeypatch):
    base = fd.energy_by_method(CASE, method="ufc", citations="record", citation_root=tmp_path)
    calls = []
    monkeypatch.setattr(fd, "validate_citation", _counting_rejector(calls))
    sweeps = fd.standard_sweeps(CASE, "ufc", FENDER, RANGES, base=base, citations="record",
                                citation_root=tmp_path)
    # every swept result cites a subset of the base's sources, already checked
    assert not calls
    assert all(s.citation_status == base.citation_status for s in sweeps)
    assert all(s.base_design_energy_knm == pytest.approx(base.design_energy_knm, rel=1e-12) for s in sweeps)


def test_standard_sweeps_refuse_a_base_that_does_not_meet_the_mode(tmp_path):
    unchecked = fd.energy_by_method(CASE, method="ufc", citations="off")
    with pytest.raises(ValueError, match="not checked"):
        fd.standard_sweeps(CASE, "ufc", FENDER, RANGES, base=unchecked)
    raw = fd.METHODS["ufc"].evaluate(CASE, "smallest", None)       # no citation sidecar at all
    with pytest.raises(ValueError, match="not checked"):
        fd.standard_sweeps(CASE, "ufc", FENDER, RANGES, base=raw, citations="record")
    # an explicit opt-out accepts it, and says so
    assert all(fd.CITATIONS_NOT_CHECKED in s.flags
               for s in fd.standard_sweeps(CASE, "ufc", FENDER, RANGES, base=unchecked, citations="off"))
    recorded = fd.energy_by_method(CASE, method="ufc", citations="record", citation_root=tmp_path)
    with pytest.raises(CitationResolutionError, match="page_missing"):
        fd.standard_sweeps(CASE, "ufc", FENDER, RANGES, base=recorded, citation_root=tmp_path)


def test_standard_sweeps_refuse_a_base_of_another_case(tmp_path):
    other = fd.energy_by_method(CASE, method="rom", citations="record", citation_root=tmp_path)
    with pytest.raises(ValueError, match="method"):
        fd.standard_sweeps(CASE, "ufc", FENDER, RANGES, base=other, citations="record", citation_root=tmp_path)
    stale = fd.energy_by_method(CASE, method="ufc", velocity_m_s=0.12, citations="record", citation_root=tmp_path)
    with pytest.raises(ValueError, match="does not match"):
        fd.standard_sweeps(CASE, "ufc", FENDER, RANGES, base=stale, citations="record", citation_root=tmp_path)
