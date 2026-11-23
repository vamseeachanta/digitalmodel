"""Tests for the starter cited foam criteria library (#1586).

Contract under test:

- every shipped entry loads into a validated ``FoamCriterion`` whose
  ``Citation`` carries standard, edition and clause (all non-empty);
- editions are explicit — no uncited defaults;
- selection by key works and a typo surfaces the known keys;
- ``verify_against_source`` is an explicit boolean per entry, and flagged
  entries carry the VERIFY marker in the citation note so the flag follows
  the value into output CSVs;
- the routed workflow accepts ``criteria_library`` keys and refuses a
  silent collision with an inline criterion.
"""

import csv
from pathlib import Path

import pytest
import yaml

from digitalmodel.foam_system.criteria_library import (
    LIBRARY_PATH,
    VERIFY_MARKER,
    LibraryEntry,
    get_criterion,
    list_criteria,
    load_criteria_library,
)
from digitalmodel.foam_system.sizing import FoamCriterion
from digitalmodel.foam_system.workflow import router

EXPECTED_STARTER_KEYS = {
    # NFPA 11 (2021) fixed-roof surface / floating-roof seal / spill rates
    "nfpa11_fixed_roof_surface_flash_37_8_to_60c",
    "nfpa11_fixed_roof_surface_flash_below_37_8c",
    "nfpa11_floating_roof_seal_top_of_seal_foam_dam",
    "nfpa11_hydrocarbon_spill_portable",
    # SOLAS FSS Code fixed deck foam rate bases
    "fss_deck_foam_cargo_deck_area",
    "fss_deck_foam_largest_tank_section",
    "fss_deck_foam_largest_monitor_area",
    # IMO MODU Code helideck rate
    "imo_modu_helideck_foam",
}


# -- library contract ---------------------------------------------------------


def test_library_ships_with_module():
    assert LIBRARY_PATH.is_file()
    assert LIBRARY_PATH.parent.name == "data"
    assert LIBRARY_PATH.parent.parent.name == "foam_system"


def test_every_entry_validates_citation_contract():
    library = load_criteria_library()
    assert EXPECTED_STARTER_KEYS <= set(library)
    for key, entry in library.items():
        assert isinstance(entry, LibraryEntry)
        assert entry.key == key
        criterion = entry.criterion
        assert isinstance(criterion, FoamCriterion)
        assert criterion.key == key
        assert criterion.application_rate_lpm_per_m2 > 0.0
        assert criterion.discharge_time_min > 0.0
        # Citation contract: all three mandatory fields present and non-empty.
        citation = criterion.citation
        assert citation.standard.strip(), key
        assert citation.edition.strip(), key
        assert citation.clause.strip(), key
        assert citation.label().startswith(f"{citation.standard} ({citation.edition})")
        # Selection metadata.
        assert entry.application.strip(), key
        assert isinstance(entry.verify_against_source, bool)


def test_raw_yaml_has_no_uncited_defaults():
    payload = yaml.safe_load(LIBRARY_PATH.read_text(encoding="utf-8"))
    assert payload["schema_version"] == 1
    for key, item in payload["criteria"].items():
        citation = item.get("citation")
        assert isinstance(citation, dict), key
        for field in ("standard", "edition", "clause"):
            assert str(citation.get(field, "")).strip(), f"{key}.citation.{field}"
        assert isinstance(item.get("verify_against_source"), bool), key


def test_verify_flag_marks_citation_note():
    library = load_criteria_library()
    flagged = {k for k, e in library.items() if e.verify_against_source}
    confident = set(library) - flagged
    assert flagged, "expected at least one verify_against_source entry"
    assert confident, "expected at least one confident entry"
    for key in flagged:
        assert VERIFY_MARKER in library[key].criterion.citation.note, key
    for key in confident:
        assert VERIFY_MARKER not in library[key].criterion.citation.note, key
    # The FSS Code deck-foam rate bases are verbatim and stable; the NFPA
    # clause numbering and MODU helideck basis must be confirmed.
    assert "fss_deck_foam_largest_tank_section" in confident
    assert "imo_modu_helideck_foam" in flagged


def test_known_values_spot_check():
    library = load_criteria_library()
    deck = library["fss_deck_foam_cargo_deck_area"].criterion
    assert deck.application_rate_lpm_per_m2 == pytest.approx(0.6)
    assert deck.discharge_time_min == pytest.approx(20.0)
    assert deck.citation.standard == "SOLAS FSS Code"
    tank = library["fss_deck_foam_largest_tank_section"].criterion
    assert tank.application_rate_lpm_per_m2 == pytest.approx(6.0)
    fixed_roof = library["nfpa11_fixed_roof_surface_flash_below_37_8c"].criterion
    assert fixed_roof.application_rate_lpm_per_m2 == pytest.approx(4.1)
    assert fixed_roof.discharge_time_min == pytest.approx(55.0)
    assert fixed_roof.citation.standard == "NFPA 11"
    assert fixed_roof.citation.edition == "2021"


# -- selection API -------------------------------------------------------------


def test_get_criterion_by_key():
    criterion = get_criterion("fss_deck_foam_largest_monitor_area")
    assert criterion.application_rate_lpm_per_m2 == pytest.approx(3.0)
    assert "1250" in criterion.citation.note


def test_unknown_key_lists_known_keys():
    with pytest.raises(ValueError, match="known keys") as excinfo:
        get_criterion("nope_not_a_rate")
    assert "fss_deck_foam_cargo_deck_area" in str(excinfo.value)


def test_list_criteria_sorted():
    entries = list_criteria()
    keys = [entry.key for entry in entries]
    assert keys == sorted(keys)
    assert EXPECTED_STARTER_KEYS <= set(keys)


# -- loader validation ---------------------------------------------------------


def _write_library(tmp_path: Path, text: str) -> Path:
    path = tmp_path / "library.yml"
    path.write_text(text, encoding="utf-8")
    return path


def test_loader_rejects_missing_citation(tmp_path):
    path = _write_library(
        tmp_path,
        "schema_version: 1\n"
        "criteria:\n"
        "  bad_entry:\n"
        "    application: some rate\n"
        "    application_rate_lpm_per_m2: 4.1\n"
        "    discharge_time_min: 30.0\n"
        "    verify_against_source: false\n",
    )
    with pytest.raises(ValueError, match="citation is required"):
        load_criteria_library(path)


def test_loader_rejects_missing_edition(tmp_path):
    path = _write_library(
        tmp_path,
        "schema_version: 1\n"
        "criteria:\n"
        "  bad_entry:\n"
        "    application: some rate\n"
        "    application_rate_lpm_per_m2: 4.1\n"
        "    discharge_time_min: 30.0\n"
        "    verify_against_source: false\n"
        "    citation: {standard: NFPA 11, edition: '', clause: x}\n",
    )
    with pytest.raises(ValueError, match="citation.edition"):
        load_criteria_library(path)


def test_loader_rejects_implicit_verify_flag(tmp_path):
    path = _write_library(
        tmp_path,
        "schema_version: 1\n"
        "criteria:\n"
        "  bad_entry:\n"
        "    application: some rate\n"
        "    application_rate_lpm_per_m2: 4.1\n"
        "    discharge_time_min: 30.0\n"
        "    citation: {standard: NFPA 11, edition: '2021', clause: x}\n",
    )
    with pytest.raises(ValueError, match="verify_against_source"):
        load_criteria_library(path)


def test_loader_rejects_wrong_schema_version(tmp_path):
    path = _write_library(tmp_path, "schema_version: 2\ncriteria: {}\n")
    with pytest.raises(ValueError, match="schema_version"):
        load_criteria_library(path)


# -- workflow integration -------------------------------------------------------


def _library_config(tmp_path: Path) -> dict:
    return {
        "basename": "foam_system_sizing",
        "_config_dir_path": str(tmp_path),
        "foam_system_sizing": {
            "criteria_library": [
                "fss_deck_foam_cargo_deck_area",
                "fss_deck_foam_largest_tank_section",
            ],
            "protected_areas": [
                {"name": "cargo deck area basis", "area_m2": 3000.0,
                 "criterion": "fss_deck_foam_cargo_deck_area"},
                {"name": "largest tank sectional area basis", "area_m2": 500.0,
                 "criterion": "fss_deck_foam_largest_tank_section"},
            ],
            "demand_policy": "max",
            "concentrate": {"concentration_percent": 3.0, "reserve_percent": 0.0},
            "output_dir": "results",
        },
    }


def test_workflow_selects_library_entries_by_key(tmp_path):
    out = router(_library_config(tmp_path))["foam_system_sizing"]
    demand = out["demand"]
    # greatest of the FSS rate bases governs: 500 m2 x 6.0 = 3000 L/min
    assert demand["governing_area"] == "largest tank sectional area basis"
    assert demand["design_solution_flow_lpm"] == pytest.approx(3000.0)
    rows = list(csv.DictReader(
        (tmp_path / "results" / "foam_system_sizing_foam_demand.csv").open()
    ))
    assert rows[0]["criterion"] == "fss_deck_foam_cargo_deck_area"
    assert "SOLAS FSS Code (Res. MSC.98(73)" in rows[0]["citation"]
    assert "para. 2.2.2.1" in rows[0]["citation"]


def test_workflow_mixes_library_and_inline_criteria(tmp_path):
    cfg = _library_config(tmp_path)
    settings = cfg["foam_system_sizing"]
    settings["criteria"] = {
        "vendor_monitor": {
            "application_rate_lpm_per_m2": 10.0,
            "discharge_time_min": 30.0,
            "citation": {"standard": "Vendor datasheet", "edition": "Rev A",
                         "clause": "performance table"},
        }
    }
    settings["protected_areas"].append(
        {"name": "vendor area", "area_m2": 100.0, "criterion": "vendor_monitor"}
    )
    out = router(cfg)["foam_system_sizing"]
    names = {row["name"] for row in out["demand"]["per_area"]}
    assert "vendor area" in names


def test_workflow_rejects_library_inline_collision(tmp_path):
    cfg = _library_config(tmp_path)
    cfg["foam_system_sizing"]["criteria"] = {
        "fss_deck_foam_cargo_deck_area": {
            "application_rate_lpm_per_m2": 99.0,
            "discharge_time_min": 1.0,
            "citation": {"standard": "X", "edition": "Y", "clause": "Z"},
        }
    }
    with pytest.raises(ValueError, match="collides with the criteria_library"):
        router(cfg)


def test_workflow_rejects_unknown_library_key(tmp_path):
    cfg = _library_config(tmp_path)
    cfg["foam_system_sizing"]["criteria_library"].append("bogus_key")
    with pytest.raises(ValueError, match="bogus_key"):
        router(cfg)


def test_workflow_rejects_empty_library_list(tmp_path):
    cfg = _library_config(tmp_path)
    cfg["foam_system_sizing"]["criteria_library"] = []
    with pytest.raises(ValueError, match="criteria_library"):
        router(cfg)


def test_verify_marker_flows_into_demand_csv(tmp_path):
    cfg = _library_config(tmp_path)
    settings = cfg["foam_system_sizing"]
    settings["criteria_library"] = ["imo_modu_helideck_foam"]
    settings["protected_areas"] = [
        {"name": "helideck", "area_m2": 300.0, "criterion": "imo_modu_helideck_foam"}
    ]
    router(cfg)
    rows = list(csv.DictReader(
        (tmp_path / "results" / "foam_system_sizing_foam_demand.csv").open()
    ))
    assert VERIFY_MARKER in rows[0]["citation"]
