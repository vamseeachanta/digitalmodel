"""Committed results, mass basis and report statements of the IEA 15 MW example.

Comparators: Allen et al. (2020), NREL/TP-5000-76773, section 1 and Table 1
(20 m draft; 1,263 t tower, 991 t RNA, 17,839 t platform, 20,093 t system;
6,065 kN mooring vertical pretension) and Table 6 (2,437 kN pretension).
"""
from __future__ import annotations

import importlib.util
import json
import math
from pathlib import Path

import pytest

from digitalmodel.floating_wind.iea15_mass_basis import ALLEN_TABLE_1, mass_basis
from digitalmodel.floating_wind.iea15_orcaflex_spec import build_spec, load_sources

ROOT = Path(__file__).resolve().parents[2]
EXAMPLE = ROOT / "docs" / "domains" / "orcaflex" / "examples" / "fowt" / "iea15-volturnus-s"


def _script(name: str):
    path = ROOT / "scripts" / "fowt" / name
    spec = importlib.util.spec_from_file_location(path.stem, path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@pytest.fixture(scope="module")
def sources():
    return load_sources()


@pytest.fixture(scope="module")
def committed():
    return json.loads((EXAMPLE / "results.json").read_text(encoding="utf-8"))


@pytest.fixture(scope="module")
def recomputed(sources):
    return _script("run_iea15_checks.py").build_results(*sources)


@pytest.fixture(scope="module")
def basis_check(sources):
    return mass_basis(build_spec(*sources), sources[0])


def _assert_same(committed, recomputed, path="results"):
    """Every value, key and list length of the committed file is recomputed."""
    if isinstance(committed, dict):
        assert isinstance(recomputed, dict) and set(committed) == set(recomputed), path
        for key in committed:
            _assert_same(committed[key], recomputed[key], f"{path}.{key}")
    elif isinstance(committed, list):
        assert isinstance(recomputed, list) and len(committed) == len(recomputed), path
        for k, (a, b) in enumerate(zip(committed, recomputed)):
            _assert_same(a, b, f"{path}[{k}]")
    elif isinstance(committed, bool) or committed is None or isinstance(committed, str):
        assert committed == recomputed, path
    else:
        assert recomputed == pytest.approx(committed, rel=1e-6, abs=1e-9), path


# ------------------------------------------------- committed results reproduce
def test_committed_results_reproduce_every_value(committed, recomputed):
    """Periods, equilibria, tensions, stiffness, comparisons and flags, not a sample."""
    _assert_same(committed, json.loads(json.dumps(recomputed)))


def test_committed_comparisons_within_tolerance(committed):
    assert all(c["within_tolerance"] for c in committed["comparisons"])


# ------------------------------------------------------------------ mass basis
def test_allen_table_1_components_sum_to_the_published_system_mass(sources):
    basis, _ = sources
    assert sum(ALLEN_TABLE_1.values()) == pytest.approx(
        basis["published"]["system_mass"]["value"], abs=0.5)


def test_published_mass_and_pretension_balance_the_displacement(sources):
    """At 20 m draft the published unit is in vertical equilibrium: buoyancy =
    weight + vertical pretension, within the rounding of the published values
    (displacement to 1 m3: 5.0 kN; mass to 1 t: 4.9 kN; pretension to 1 kN: 0.5 kN)."""
    basis, _ = sources
    published = basis["published"]
    rho = basis["environment"]["water_density"]["value"]
    g = 9.80665
    buoyancy = rho * published["displacement"]["value"] * g
    weight = published["system_mass"]["value"] * g
    rounding = 0.5 * rho * g + 500.0 * g + 500.0
    assert buoyancy - weight == pytest.approx(6.065e6, abs=rounding)


def test_system_mass_compared_with_allen_table_1(committed, sources):
    basis, _ = sources
    row = next(c for c in committed["comparisons"] if c["quantity"] == "System mass")
    assert row["comparator"] == basis["published"]["system_mass"]["value"]
    assert "Table 1" in row["comparator_source"]
    assert row["deviation"] == pytest.approx(0.00865, abs=5e-5)


def test_sinkage_is_set_by_the_mass_basis(basis_check):
    """With Allen's system mass the still-water heave is within 0.05 m of the
    published 20 m draft; with the model's mass it is not."""
    assert abs(basis_check["still_water_heave_at_published_mass"]) < 0.01
    assert basis_check["still_water_heave"] == pytest.approx(-0.382, abs=0.001)
    assert basis_check["draft_at_model_mass_within_tolerance"] is False
    assert basis_check["draft_at_published_mass_within_tolerance"] is True


def test_draft_and_pretension_comparisons_present(committed):
    names = {c["quantity"]: c for c in committed["comparisons"]}
    draft = names["Still-water heave with the system mass of Allen Table 1"]
    assert draft["comparator"] == 0.0 and draft["tolerance_kind"] == "absolute"
    assert names["Mooring vertical pretension, all lines"]["comparator"] == pytest.approx(6.065e6)
    undisplaced = [c for c in committed["comparisons"]
                   if c["quantity"].startswith("Fairlead tension at the undisplaced position")]
    assert len(undisplaced) == 3
    assert all(abs(c["deviation"]) < 0.001 for c in undisplaced)


def test_committed_mass_basis_discloses_the_failed_draft(committed):
    block = committed["mass_basis"]
    assert block["draft_at_model_mass_within_tolerance"] is False
    assert "0.87 %" in block["disposition"]


# ---------------------------------------------- inertia qualification carried
def test_committed_results_carry_the_inertia_qualification(sources):
    basis, _ = sources
    committed = json.loads((ROOT / "docs" / "domains" / "orcaflex" / "examples" / "fowt"
                            / "iea15-volturnus-s" / "results.json").read_text(encoding="utf-8"))
    qualification = committed["qualification"]
    assert qualification["readiness"] == basis["qualification"]["readiness"]
    assert qualification["use_for_analysis"] == basis["qualification"]["use_for_analysis"]
    assert "RNA" in qualification["natural_periods"]
    assert "assumed" in qualification["natural_periods"]
    readme = (ROOT / "docs" / "domains" / "orcaflex" / "examples" / "fowt"
              / "iea15-volturnus-s" / "README.md").read_text(encoding="utf-8")
    established = readme.split("Established without a licence")[1].split("Not established")[0]
    period_item = next(item for item in established.split("\n- ") if "natural periods" in item)
    assert "assumed" in period_item and "RNA" in period_item


# --------------------------------------------------------- report statements
def _lc0_section() -> str:
    readme = (EXAMPLE / "README.md").read_text(encoding="utf-8")
    return readme.split("### Still-water equilibrium (LC0)")[1].split("### Natural periods")[0]


def test_readme_attributes_the_sinkage_to_the_mass_basis():
    section = _lc0_section()
    for text in ("20,093 t", "20,266.9 t", "0.87 %", "1,263 t", "1,483.1 t", "6,065 kN",
                 "−0.004 m"):
        assert text in section, text


def test_lc0_run_md_cites_the_published_draft(sources):
    module = _script("write_iea15_run_packages.py")
    basis, data = sources
    eq, x = module.expectations(basis, data)
    spec = build_spec(basis, data)
    text = module.run_md("LC0_statics", spec, eq, x, basis)
    heave = next(line for line in text.splitlines() if line.startswith("| Vessel heave"))
    assert "none published" not in heave
    assert "20 m draft" in heave and "20,093 t" in heave and "6,065 kN" in heave
    assert "undisplaced" in text
    assert math.isfinite(x["mass_basis"]["still_water_heave_at_published_mass"])
