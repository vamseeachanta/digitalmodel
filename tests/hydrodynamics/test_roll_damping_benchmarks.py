"""Tests for the roll damping benchmark tables, bands and method matrix."""

from __future__ import annotations

import math
import json
import shutil
from dataclasses import asdict
from pathlib import Path

import pytest

from digitalmodel.hydrodynamics import roll_damping_benchmarks as rb


def test_benchmark_cases_load_and_resolve_sources():
    cases = rb.load_benchmark_cases()
    assert len(cases) >= 20
    ids = {c.case_id for c in cases}
    assert {"kim2015-k0-10deg", "gu-day2015-rect-15deg", "kjaer2024-lcog2-07deg"} <= ids
    sources = rb.load_sources()
    assert all(c.source_id in sources for c in cases)


def test_kim_rows_follow_the_published_linear_fit():
    # zeta = r + s * phi_a (phi_a in deg), Kim et al. (2015) Tables 3-6
    fits = {"k0": (0.00846, 0.00031), "k1": (0.01362, 0.00242),
            "k2": (0.01634, 0.00484), "k3": (0.02737, 0.00809)}
    for c in rb.load_benchmark_cases():
        if c.source_id != "kim2015":
            continue
        r, s = fits[c.case_id.split("-")[1]]
        assert c.damping_pct == pytest.approx(100 * (r + s * c.amplitude_deg), abs=0.05)


def test_journee_rows_follow_eq_7_174_in_radians():
    import math
    for c in rb.load_benchmark_cases():
        if c.source_id == "journee-massie2001":
            kappa = 0.0013 * c.b_over_t**2 + 0.5 * math.radians(c.amplitude_deg)
            assert c.damping_pct == pytest.approx(100 * kappa, abs=0.05)


def test_acceptance_bands_are_ordered():
    bands = rb.load_acceptance_criteria()
    assert {"identity", "figure_read", "printed_table_value",
            "damping_ratio_vs_measured", "solver_to_solver_damping"} <= set(bands)
    for b in bands.values():
        assert 0 < b.green_max_pct < b.amber_max_pct
        assert b.rationale


@pytest.mark.parametrize(
    "quantity, calc, ref, colour",
    [
        ("identity", 0.7873, 0.787, "green"),
        ("figure_read", 123.5, 120.0, "green"),
        ("printed_table_value", 0.166, 0.20, "amber"),
        ("figure_read", 21.3, 120.0, "red"),
        ("damping_ratio_vs_measured", 0.82, 1.2, "green"),   # within the 0.5 point floor
        ("damping_ratio_vs_measured", 0.41, 1.0, "red"),
        ("damping_ratio_vs_measured", 14.3, 10.6, "amber"),
    ],
)
def test_verdict_colours(quantity, calc, ref, colour):
    v = rb.verdict(quantity, calc, ref)
    assert v["colour"] == colour
    assert v["deviation_pct"] == pytest.approx(100 * (calc / ref - 1))


def test_verdict_rejects_unknown_quantity_and_zero_reference():
    with pytest.raises(rb.RollDampingDataError):
        rb.verdict("not-a-quantity", 1.0, 1.0)
    with pytest.raises(rb.RollDampingDataError):
        rb.verdict("identity", 1.0, 0.0)


def test_verdict_rejects_negative_values():
    with pytest.raises(rb.RollDampingDataError):
        rb.verdict("damping_ratio_vs_measured", -0.1, 0.1)
    with pytest.raises(rb.RollDampingDataError):
        rb.verdict("damping_ratio_vs_measured", 0.1, -0.1)


@pytest.mark.parametrize(
    "quantity, calc, colour",
    [
        ("figure_read", 110.0, "green"), ("figure_read", 90.0, "green"),
        ("figure_read", 125.0, "amber"), ("figure_read", 75.0, "amber"),
        ("solver_to_solver_damping", 101.0, "green"), ("solver_to_solver_damping", 105.0, "amber"),
        ("solver_to_solver_damping", 95.0, "amber"), ("solver_to_solver_damping", 105.1, "red"),
    ],
)
def test_verdict_band_edges_are_inclusive(quantity, calc, colour):
    assert rb.verdict(quantity, calc, 100.0)["colour"] == colour


@pytest.mark.parametrize(
    "band",
    [
        dict(green_max_pct=10.0, amber_max_pct=math.inf),
        dict(green_max_pct=10.0, amber_max_pct=20.0, abs_floor=math.inf),
        dict(green_max_pct=10.0, amber_max_pct=20.0, abs_floor=-1.0),
        dict(green_max_pct=10.0, amber_max_pct=20.0, abs_floor=math.nan),
        dict(green_max_pct=20.0, amber_max_pct=10.0),
    ],
)
def test_caller_bands_are_validated(band):
    criteria = {"q": rb.Band(quantity="q", description="d", rationale="r", **band)}
    with pytest.raises(rb.RollDampingDataError):
        rb.verdict("q", 1.0, 1.0, criteria=criteria)


@pytest.mark.parametrize(
    "old, new, match",
    [
        ("kim2015-k0-05deg,kim2015,box barge model L 1.00 m B 0.25 m T 0.08 m,3.125", "kim2015-k0-05deg,kim2015,box barge model L 1.00 m B 0.25 m T 0.08 m,-3.125", "b_over_t"),
        ("model,5,1.0,total decay", "model,-5,1.0,total decay", "amplitude_deg"),
        ("Table 3 p. 466,derived from table coefficients,zeta", "Table 3 p. 466,,zeta", "verification"),
    ],
)
def test_loader_rejects_bad_rows(tmp_path: Path, old, new, match):
    shutil.copytree(rb.DEFAULT_DATA_ROOT, tmp_path / "rd")
    path = tmp_path / "rd" / "benchmark_cases.csv"
    text = path.read_text(encoding="utf-8")
    assert old in text
    path.write_text(text.replace(old, new, 1), encoding="utf-8")
    with pytest.raises(rb.RollDampingDataError, match=match):
        rb.load_benchmark_cases(tmp_path / "rd")


def test_method_matrix_covers_core_scenarios():
    rows = rb.load_method_matrix()
    ids = {r.scenario_id for r in rows}
    assert {"barge-sharp-bare", "barge-rounded-bilge", "barge-bilge-keels",
            "wide-shallow-high-kg", "conventional-ship"} <= ids
    for r in rows:
        assert r.primary_method and r.screening_damping_pct and r.basis


def test_keel_recommendation_identifies_the_implemented_drag_fit():
    row = next(r for r in rb.load_method_matrix()
               if r.scenario_id == "barge-bilge-keels")
    assert "ITTC 2024 eq. 48" in row.primary_method
    assert "ITTC 2024 eq. 48" in row.digitalmodel_route
    assert "ITTC 2024 low-Ke fit (eq. 49) is not implemented" in row.digitalmodel_route


def test_wiki_tables_are_pointers_not_values():
    for row in rb.load_wiki_tables():
        assert row["wiki_path"].startswith("wikis/")
        assert set(row) == {"table_id", "source_id", "wiki_path", "content"}


def test_loader_fails_closed_on_unknown_source(tmp_path: Path):
    shutil.copytree(rb.DEFAULT_DATA_ROOT, tmp_path / "rd")
    path = tmp_path / "rd" / "benchmark_cases.csv"
    text = path.read_text(encoding="utf-8").replace("kim2015-k0-05deg,kim2015", "kim2015-k0-05deg,nosuch", 1)
    path.write_text(text, encoding="utf-8")
    with pytest.raises(rb.RollDampingDataError, match="unknown source_id"):
        rb.load_benchmark_cases(tmp_path / "rd")


def test_loader_fails_closed_on_missing_file(tmp_path: Path):
    with pytest.raises(rb.RollDampingDataError, match="missing file"):
        rb.load_acceptance_criteria(tmp_path)


# --------------------------------------------------------------------------
# The generic study script reproduces the report's numbers.
# --------------------------------------------------------------------------
def _study():
    import importlib.util

    path = rb.REPO_ROOT / "examples" / "hydrodynamics" / "roll_damping" / "generic_roll_damping_study.py"
    spec = importlib.util.spec_from_file_location("generic_roll_damping_study", path)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def test_study_reproduction_rows_and_verdicts():
    mod = _study()
    rep = mod.reproduction()
    assert rep["CR"]["chakrabarti2001"] == pytest.approx(123.5, abs=0.1)
    assert rep["CR"]["as_printed"] == pytest.approx(21.3, abs=0.1)
    assert rep["t1_eq35_end_factor"] == pytest.approx(0.166, abs=0.001)
    colours = {r["row"]: r["colour"] for r in mod.reproduction_verdicts(rep) if r["row"] in ("1", "2", "3", "4")}
    assert colours == {"1": "green", "2": "green", "3": "red", "4": "green"}


def test_study_benchmarks_grade_eq35_against_sharp_and_rounded_barges():
    mod = _study()
    rows = mod.grade_benchmarks(mod.literature())
    gu = {r["case"]: r for r in rows if r["case"].startswith("Gu and Day")}
    graded = [r for r in gu.values() if r["graded"]]
    assert len(graded) == 1 and "15 deg, OG 0.0" in graded[0]["case"]
    assert graded[0]["colour"] == "amber"  # +35 % above the measured total at the release amplitude
    assert all(r["colour"] == "bracket" for r in gu.values() if not r["graded"])
    kim_eq35 = [r for r in rows if r["case"].startswith("Kim") and "35" in r["method"]]
    assert all(r["colour"] == "red" for r in kim_eq35)


def test_study_generic_barge_base_case():
    # Independent arithmetic and input basis: data/roll_damping/README.md.
    # Synthetic geometry and inertia assumptions, not a measured benchmark.
    rho, gravity, nu = 1025.0, 9.80665, 1.19e-6
    length, beam, draft, kg = 100.0, 30.0, 5.0, 7.0
    volume = length * beam * draft
    mass = rho * volume
    gm = draft / 2 + beam**2 / (12 * draft) - kg
    stiffness = mass * gravity * gm
    inertia = mass * (0.35 * beam)**2
    added = 0.25 * inertia
    omega = (stiffness / (inertia + added))**0.5
    period = 2 * math.pi / omega
    critical = 2 * (stiffness * (inertia + added))**0.5
    amplitude, og = math.pi / 18, draft - kg
    area = length * (1.7 * draft + beam)
    radius = (1.032 * area / length - 2 * og) / math.pi
    friction = (0.787 * rho * area * radius**2 * (omega * nu)**0.5
                * (1 + 0.00814 * (radius**2 * amplitude**2 * omega / nu)**0.386))
    h0 = beam / (2 * draft)
    eddy = (2 / math.pi * rho * length * draft**4 * amplitude * omega
            * (h0**2 + 1 - og / draft) * (h0**2 + (1 - og / draft)**2))
    damping_pct = 100 * (friction + eddy) / critical
    mod = _study()
    citation_root = Path(__file__).resolve().parents[1] / "citations" / "fixtures"
    parts, z, info = mod.barge_case(mod.BARGE0, citation_root)
    assert info == pytest.approx(dict(GM=gm, wn=omega, Tn=period, I=inertia,
                                    A=added, C=stiffness, m=mass), rel=1e-12)
    assert parts.friction == pytest.approx(friction, rel=1e-12)
    assert parts.eddy == pytest.approx(eddy, rel=1e-12)
    assert parts.lift == parts.wave == parts.bilge_keel == 0
    assert z == pytest.approx(damping_pct, rel=1e-12)
    assert info["Tn"] == pytest.approx(7.27, abs=0.01)
    assert z == pytest.approx(19.21, abs=0.01)


@pytest.mark.parametrize("case_name", ["barge", "ship"])
def test_study_hulls_fail_closed_on_missing_source(tmp_path: Path, case_name):
    from digitalmodel.citations.schema import CitationResolutionError

    mod = _study()
    with pytest.raises(CitationResolutionError, match="page_missing"):
        getattr(mod, f"{case_name}_case")(getattr(mod, f"{case_name.upper()}0"), tmp_path / "missing")


@pytest.mark.parametrize("field", ["code_id", "publisher", "revision"])
def test_study_ship_rejects_mismatched_source(tmp_path: Path, field):
    from digitalmodel.citations.schema import CitationResolutionError
    from digitalmodel.hydrodynamics.roll_damping_components import SOURCE_CITATION

    fixture = Path(__file__).resolve().parents[1] / "citations" / "fixtures"
    page = tmp_path / SOURCE_CITATION.wiki_path
    page.parent.mkdir(parents=True)
    text = (fixture / "knowledge" / SOURCE_CITATION.wiki_path).read_text(encoding="utf-8")
    lines = text.splitlines()
    assert any(line.startswith(f"{field}:") for line in lines)
    page.write_text("\n".join(f"{field}: mismatched" if line.startswith(f"{field}:") else line
                              for line in lines) + "\n", encoding="utf-8")
    mod = _study()
    with pytest.raises(CitationResolutionError, match=f"frontmatter_mismatch:{field}"):
        mod.ship_case(mod.SHIP0, tmp_path)


@pytest.mark.parametrize("keel_breadth, count", [(0.0, 3), (0.45, 4)])
def test_study_ship_resolves_component_citations(keel_breadth, count):
    from digitalmodel.hydrodynamics import roll_damping_components as rc

    mod = _study()
    fixture = Path(__file__).resolve().parents[1] / "citations" / "fixtures"
    parts, z, _ = mod.ship_case(dict(mod.SHIP0, bk=keel_breadth), fixture)
    assert math.isfinite(z)
    assert len(parts.citations) == count
    assert any("sectional eddy" in c.section for c in parts.citations)
    assert any("bilge keel" in c.section for c in parts.citations) == (keel_breadth > 0)
    assert not any("sharp-cornered barge" in c.section for c in parts.citations)
    rc.validate_citations(parts, fixture)


def test_study_ship_uses_default_citation_resolver(monkeypatch):
    mod = _study()
    fixture = Path(__file__).resolve().parents[1] / "citations" / "fixtures"
    monkeypatch.setenv("LLM_WIKI_PATH", str(fixture))
    parts, _, _ = mod.ship_case(mod.SHIP0)
    assert len(parts.citations) == 4


def test_study_serialization_preserves_both_hull_sidecars(monkeypatch):
    from digitalmodel.citations.schema import Citation, validate_citation

    mod = _study()
    fixture = Path(__file__).resolve().parents[1] / "citations" / "fixtures"
    # A conflicting environment also detects sweep calls that drop wiki_root.
    monkeypatch.setenv("LLM_WIKI_PATH", str(fixture / "missing"))
    out = json.loads(json.dumps(mod.illustrative(fixture)))
    for hull, count in (("barge", 5), ("ship", 4)):
        parts, _, _ = getattr(mod, f"{hull}_case")(getattr(mod, f"{hull.upper()}0"), fixture)
        assert out[hull]["base"]["citations"] == [asdict(c) for c in parts.citations]
        assert len(out[hull]["base"]["citations"]) == count
        for row in [out[hull]["base"], *out[hull]["rows"]]:
            assert row["citations"]
            for citation in row["citations"]:
                validate_citation(Citation(**citation), repo_root=fixture)
    for row in out["amp_sweep"]:
        for hull, count in (("barge", 5), ("barge_bk", 6), ("ship", 4), ("ship_no_bk", 3)):
            assert len(row[f"{hull}_citations"]) == count


def test_manifest_matches_tables():
    import hashlib

    import yaml

    manifest = yaml.safe_load((rb.DEFAULT_DATA_ROOT / "manifest.yaml").read_text(encoding="utf-8"))
    for entry in manifest["tables"].values():
        raw = (rb.DEFAULT_DATA_ROOT / entry["file"]).read_bytes()
        assert hashlib.sha256(raw).hexdigest() == entry["sha256"], entry["file"]


def test_partial_vs_total_is_one_sided():
    # a calculated part below the measured total is consistent, not a disagreement
    assert rb.verdict("partial_vs_measured_total", 0.41, 1.0)["colour"] == "green"
    assert rb.verdict("partial_vs_measured_total", 14.3, 10.6)["colour"] == "amber"
    assert rb.verdict("partial_vs_measured_total", 6.24, 1.0)["colour"] == "red"
    assert rb.verdict("damping_ratio_vs_measured", 0.41, 1.0)["colour"] == "red"


def test_sources_reject_truncated_rows(tmp_path: Path):
    shutil.copytree(rb.DEFAULT_DATA_ROOT, tmp_path / "rd")
    path = tmp_path / "rd" / "sources.csv"
    path.write_text(path.read_text(encoding="utf-8") + "kim2015\n", encoding="utf-8")
    with pytest.raises(rb.RollDampingDataError, match="missing or extra cells"):
        rb.load_sources(tmp_path / "rd")


def test_study_keeps_domain_flags():
    mod = _study()
    citation_root = Path(__file__).resolve().parents[1] / "citations" / "fixtures"
    out = mod.illustrative(citation_root)
    by_amp = {r["amp"]: r for r in out["amp_sweep"]}
    assert "amplitude_above_paper_examples" in by_amp[20]["barge_flags"]
    assert "amplitude_above_paper_examples" in by_amp[20]["ship_flags"]
    assert by_amp[10]["barge_flags"] == []
    assert "strip_stations_excluded" in out["ship"]["base"]["flags"]
    ship15 = [r for r in out["ship"]["rows"] if r["label"] == "Roll amplitude 15 deg"][0]
    assert {"amplitude_above_paper_examples", "strip_stations_excluded"} <= set(ship15["flags"])
    flagged = [r for r in out["barge"]["rows"] if r["label"] == "Roll amplitude 15 deg"][0]
    assert "amplitude_above_paper_examples" in flagged["flags"]


def test_one_sided_must_be_boolean(tmp_path: Path):
    shutil.copytree(rb.DEFAULT_DATA_ROOT, tmp_path / "rd")
    path = tmp_path / "rd" / "acceptance_criteria.yaml"
    path.write_text(path.read_text(encoding="utf-8").replace("one_sided: true", 'one_sided: "false"'), encoding="utf-8")
    with pytest.raises(rb.RollDampingDataError, match="boolean"):
        rb.load_acceptance_criteria(tmp_path / "rd")
    criteria = {"q": rb.Band(quantity="q", description="d", rationale="r", green_max_pct=1.0,
                             amber_max_pct=2.0, one_sided="false")}
    with pytest.raises(rb.RollDampingDataError, match="boolean"):
        rb.verdict("q", 0.0, 100.0, criteria=criteria)


@pytest.mark.parametrize("name, loader", [("method_matrix.csv", "load_method_matrix"), ("wiki_tables.csv", "load_wiki_tables")])
def test_matrix_and_wiki_loaders_reject_truncated_rows(tmp_path: Path, name, loader):
    shutil.copytree(rb.DEFAULT_DATA_ROOT, tmp_path / "rd")
    path = tmp_path / "rd" / name
    path.write_text(path.read_text(encoding="utf-8") + "truncated-row\n", encoding="utf-8")
    with pytest.raises(rb.RollDampingDataError, match="missing or extra cells"):
        getattr(rb, loader)(tmp_path / "rd")


def test_wiki_tables_reject_unknown_source(tmp_path: Path):
    shutil.copytree(rb.DEFAULT_DATA_ROOT, tmp_path / "rd")
    path = tmp_path / "rd" / "wiki_tables.csv"
    path.write_text(path.read_text(encoding="utf-8").replace("chakrabarti2001-c1,chakrabarti2001", "chakrabarti2001-c1,nosuch"), encoding="utf-8")
    with pytest.raises(rb.RollDampingDataError, match="unknown source_id"):
        rb.load_wiki_tables(tmp_path / "rd")


def test_duplicate_csv_header_is_rejected(tmp_path: Path):
    shutil.copytree(rb.DEFAULT_DATA_ROOT, tmp_path / "rd")
    path = tmp_path / "rd" / "benchmark_cases.csv"
    lines = path.read_text(encoding="utf-8").splitlines()
    lines = [lines[0] + ",damping_pct"] + [ln + ",2.0" for ln in lines[1:]]
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    with pytest.raises(rb.RollDampingDataError, match="duplicate column"):
        rb.load_benchmark_cases(tmp_path / "rd")


def test_duplicate_yaml_key_is_rejected(tmp_path: Path):
    shutil.copytree(rb.DEFAULT_DATA_ROOT, tmp_path / "rd")
    path = tmp_path / "rd" / "acceptance_criteria.yaml"
    path.write_text(path.read_text(encoding="utf-8").replace("    one_sided: true", "    one_sided: false\n    one_sided: true"), encoding="utf-8")
    with pytest.raises(rb.RollDampingDataError, match="duplicate key"):
        rb.load_acceptance_criteria(tmp_path / "rd")


def test_boolean_numeric_band_fields_are_rejected(tmp_path: Path):
    shutil.copytree(rb.DEFAULT_DATA_ROOT, tmp_path / "rd")
    path = tmp_path / "rd" / "acceptance_criteria.yaml"
    path.write_text(path.read_text(encoding="utf-8").replace("    abs_floor: 0.5", "    abs_floor: true"), encoding="utf-8")
    with pytest.raises(rb.RollDampingDataError, match="must be a number"):
        rb.load_acceptance_criteria(tmp_path / "rd")
    criteria = {"q": rb.Band(quantity="q", description="d", rationale="r", green_max_pct=1.0,
                             amber_max_pct=2.0, abs_floor=True)}
    with pytest.raises(rb.RollDampingDataError, match="must be a number"):
        rb.verdict("q", 0.1, 1.0, criteria=criteria)


def test_no_published_coefficient_table_in_the_study():
    text = (rb.REPO_ROOT / "examples" / "hydrodynamics" / "roll_damping" / "generic_roll_damping_study.py").read_text(encoding="utf-8")
    assert "215.695" not in text  # Q6 values live in the private wiki
    ids = {r["table_id"] for r in rb.load_wiki_tables()}
    assert "ittc2024-simplified-ikeda-q6" in ids


def _fake_q6_wiki(tmp_path: Path, body: str) -> Path:
    rel = {r["table_id"]: r["wiki_path"] for r in rb.load_wiki_tables()}["ittc2024-simplified-ikeda-q6"]
    target = tmp_path / "wiki" / rel
    target.parent.mkdir(parents=True)
    target.write_text(body, encoding="utf-8")
    return tmp_path / "wiki"


# Synthetic coefficients only (not the published table): c0 = 1, others 0.
_Q6_HEADER = "row,c4,c3,c2,c1,c0,note\n"
_Q6_ROWS = "".join(f"{k},0,0,0,0,1,x\n" for k in ("q6_ae", "q6_be1", "q6_be2"))


def test_q6_loader_rejects_duplicate_rows_and_headers(tmp_path: Path):
    mod = _study()
    wiki = _fake_q6_wiki(tmp_path / "a", _Q6_HEADER + _Q6_ROWS + "q6_ae,0,0,0,0,2,dup\n")
    with pytest.raises(rb.RollDampingDataError, match="duplicate Q6 row"):
        mod._load_q6(wiki)
    wiki = _fake_q6_wiki(tmp_path / "b", "row,c4,c3,c2,c1,c0,c0\n" + _Q6_ROWS)
    with pytest.raises(rb.RollDampingDataError, match="bad Q6 header"):
        mod._load_q6(wiki)
    wiki = _fake_q6_wiki(tmp_path / "c", _Q6_HEADER + "q6_ae,0,0,0,0,1,x\n")
    with pytest.raises(rb.RollDampingDataError, match="missing Q6 rows"):
        mod._load_q6(wiki)


def test_container_rows_are_reported_with_and_without_the_wiki(tmp_path: Path):
    mod = _study()
    rows = [r for r in mod.reproduction_verdicts(mod.reproduction()) if r["row"] in ("9", "10")]
    assert len(rows) == 4
    assert all(r["colour"] == "not graded" and "--wiki-root" in r["reason"] for r in rows)
    wiki = _fake_q6_wiki(tmp_path, _Q6_HEADER + _Q6_ROWS)
    rep = mod.reproduction()
    rep["container"] = mod.container_model(wiki)
    rows = [r for r in mod.reproduction_verdicts(rep) if r["row"] in ("9", "10")]
    assert len(rows) == 4
    assert all(r["colour"] in ("green", "amber", "red") for r in rows)


def test_study_main_writes_both_citation_sidecars(tmp_path: Path, monkeypatch):
    import sys

    mod = _study()
    # Synthetic Q6 values and identity metadata exercise output, not source validity.
    wiki = _fake_q6_wiki(tmp_path, _Q6_HEADER + _Q6_ROWS)
    fixture = Path(__file__).resolve().parents[1] / "citations" / "fixtures"
    shutil.copytree(fixture / "knowledge", wiki / "knowledge")
    output = tmp_path / "study.json"
    monkeypatch.setenv("LLM_WIKI_PATH", str(tmp_path / "missing"))
    monkeypatch.setattr(sys, "argv", ["study", "--out", str(output), "--wiki-root", str(wiki)])
    mod.main()
    saved = json.loads(output.read_text(encoding="utf-8"))
    assert len(saved["illustrative"]["barge"]["base"]["citations"]) == 5
    assert len(saved["illustrative"]["ship"]["base"]["citations"]) == 4
    assert all(isinstance(c, dict) for hull in ("barge", "ship")
               for c in saved["illustrative"][hull]["base"]["citations"])
