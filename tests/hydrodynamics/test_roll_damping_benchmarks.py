"""Tests for the roll damping benchmark tables, bands and method matrix."""

from __future__ import annotations

import math
import shutil
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
    assert graded[0]["colour"] == "amber"  # +35 % at the release amplitude
    assert all(r["colour"] == "bracket" for r in gu.values() if not r["graded"])
    kim_eq35 = [r for r in rows if r["case"].startswith("Kim") and "35" in r["method"]]
    assert all(r["colour"] == "red" for r in kim_eq35)


def test_study_generic_barge_base_case():
    mod = _study()
    citation_root = Path(__file__).resolve().parents[1] / "citations" / "fixtures"
    parts, z, info = mod.barge_case(mod.BARGE0, citation_root)
    assert info["Tn"] == pytest.approx(7.27, abs=0.01)
    assert z == pytest.approx(19.21, abs=0.01)


def test_manifest_matches_tables():
    import hashlib

    import yaml

    manifest = yaml.safe_load((rb.DEFAULT_DATA_ROOT / "manifest.yaml").read_text(encoding="utf-8"))
    for entry in manifest["tables"].values():
        raw = (rb.DEFAULT_DATA_ROOT / entry["file"]).read_bytes()
        assert hashlib.sha256(raw).hexdigest() == entry["sha256"], entry["file"]
