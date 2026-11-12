"""Value checks for the riser capability inventory and its generated page."""
from __future__ import annotations

import copy
import csv
import importlib.util
import json
from pathlib import Path

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts" / "riser_capability" / "build_riser_inventory.py"
_spec = importlib.util.spec_from_file_location("build_riser_inventory", SCRIPT)
builder = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(builder)

EXPECTED_TYPES = {
    "drilling", "completion_workover", "ttr", "scr", "slwr", "flexible",
    "hybrid_tower_slor", "buoyancy_supported",
}


@pytest.fixture(scope="module")
def inventory() -> dict:
    return builder.load_inventory()


def _write(data: dict, tmp_path: Path) -> Path:
    path = tmp_path / "riser_types.yml"
    path.write_text(yaml.safe_dump(data), encoding="utf-8")
    return path


def test_every_requested_riser_type_has_one_entry(inventory):
    assert {entry["id"] for entry in inventory["riser_types"]} == EXPECTED_TYPES


def test_status_counts_match_the_repository(inventory):
    counts: dict[str, int] = {}
    for entry in inventory["riser_types"]:
        counts[entry["status"]] = counts.get(entry["status"], 0) + 1
    assert counts == {"static_screen": 3, "components_only": 3, "absent": 2}


def test_record_counts_agree_with_an_independent_recount(inventory):
    paths = {row["id"]: ROOT / row["path"] for row in inventory["databases"]}
    with paths["riser_stackup"].open(encoding="utf-8", newline="") as handle:
        stackup = len(list(csv.DictReader(handle)))
    assert builder.record_count(paths["riser_stackup"]) == stackup == 84
    with paths["config_catalog"].open(encoding="utf-8", newline="") as handle:
        catalog = len(list(csv.DictReader(handle)))
    assert builder.record_count(paths["config_catalog"]) == catalog == 28
    line_pipe = json.loads(paths["line_pipe"].read_text(encoding="utf-8"))["records"]
    assert builder.record_count(paths["line_pipe"]) == len(line_pipe)


def test_an_empty_catalogue_is_not_reported_as_populated():
    assert builder.population(0).startswith("empty")
    assert builder.population(3) == "populated"
    assert builder.population(None).startswith("not counted")


def test_a_missing_path_is_rejected(inventory, tmp_path):
    broken = copy.deepcopy(inventory)
    broken["riser_types"][0]["modules"].append("src/digitalmodel/does_not_exist.py")
    with pytest.raises(ValueError, match="does not exist"):
        builder.load_inventory(_write(broken, tmp_path))


def test_an_absent_capability_cannot_list_modules(inventory, tmp_path):
    broken = copy.deepcopy(inventory)
    target = next(e for e in broken["riser_types"] if e["status"] == "absent")
    target["modules"] = ["src/digitalmodel/drilling_riser/stackup.py"]
    with pytest.raises(ValueError, match="absent capability"):
        builder.load_inventory(_write(broken, tmp_path))


def test_an_unknown_database_reference_is_rejected(inventory, tmp_path):
    broken = copy.deepcopy(inventory)
    broken["riser_types"][0]["databases"].append("no_such_table")
    with pytest.raises(ValueError, match="unknown database"):
        builder.load_inventory(_write(broken, tmp_path))


def test_excluded_folders_still_record_origin_as_not_stated(inventory):
    for item in inventory["excluded_model_folders"]["paths"]:
        text = (ROOT / item / "PROVENANCE.md").read_text(encoding="utf-8")
        assert "not stated" in text


def test_generated_page_is_current_and_carries_no_run_approval_wording(inventory):
    text = builder.render(inventory)
    assert builder.OUTPUT.read_text(encoding="utf-8") == text
    lowered = text.lower()
    assert "cleared to run" not in lowered and "approved to run" not in lowered
    assert "## Reproduce" in text
