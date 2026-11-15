"""Velocity overrides recompute velocity-dependent coefficients; sweeps follow
each method's coefficient rules; method results carry a citation sidecar that
is resolved against the wiki (fail-closed by default; ``record`` and ``off`` are
explicit, flagged opt-outs).

Ship and berth values are the generic round numbers of the method report.
"""

from __future__ import annotations

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
    keys = lambda method: [s.parameter for s in fd.standard_sweeps(CASE, method, FENDER, RANGES)]  # noqa: E731
    assert keys("wg211") == ["velocity_m_s", "factor"]
    assert keys("pianc2002") == ["velocity_m_s", "cs", "factor"]      # keel-clearance Cm pairs with Cc = 1
    assert keys("eau") == ["velocity_m_s", "cs", "factor"]
    assert keys("bs6349") == ["velocity_m_s", "cs", "cc", "factor"]
    assert keys("ufc") == ["velocity_m_s", "cc", "factor"]
    assert keys("is4651") == ["velocity_m_s"]
    assert set(fd.inapplicable_sweeps("wg211")) == {"cs", "cc"}
    assert "pairing" in fd.inapplicable_sweeps("pianc2002")["cc"]


def test_sweep_coefficient_ranges_clipped_to_method_rule():
    rom = {s.parameter: s for s in fd.standard_sweeps(CASE, "rom", FENDER, RANGES)}
    cc = rom["cc"]
    assert (cc.range_low, cc.range_high) == (0.9, 1.0)
    assert "clipped to 0.9 to 1" in cc.range_basis
    assert min(p.value for p in cc.points) >= 0.9 and max(p.value for p in cc.points) <= 1.0
    bs = {s.parameter: s for s in fd.standard_sweeps(CASE, "bs6349", FENDER, RANGES)}
    assert (bs["cc"].range_low, bs["cc"].range_high) == (0.8, 1.0) and "clipped" not in bs["cc"].range_basis


def test_velocity_sweep_recomputes_rom_factor():
    sw = next(s for s in fd.standard_sweeps(CASE, "rom", FENDER, RANGES) if s.parameter == "velocity_m_s")
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
    kw = dict(curve=curve, reaction_limit_kn=1600.0, capacity_knm=700.0)
    nom = fd.standard_sweeps(CASE, "pianc2002", FENDER, RANGES, **kw)
    tol = fd.standard_sweeps(CASE, "pianc2002", FENDER, RANGES, reaction_factor=1.10, **kw)
    for a, b in zip(nom, tol):
        assert b.reaction_factor == 1.10
        for pa, pb in zip(a.points, b.points):
            assert pa.design_energy_knm == pb.design_energy_knm
            if pa.reaction_kn is not None:
                assert pb.reaction_kn == pytest.approx(1.10 * pa.reaction_kn)
                assert pb.hull_pressure_kpa == pytest.approx(1.10 * 210.0)


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
