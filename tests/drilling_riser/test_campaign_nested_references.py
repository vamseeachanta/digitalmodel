"""Source-only nested interface checks; no producer or solver is imported.

Expected field sets and units are manually transcribed from the public source
symbols cited in the inventory. No engineering calculation is evaluated.
"""

import ast
import json
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
INVENTORY = ROOT / "docs/domains/risers/campaign-interface-references.json"


def inventory():
    return json.loads(INVENTORY.read_text(encoding="utf-8"))["nested_interfaces"]


def function(surface, name):
    tree = ast.parse((ROOT / surface["source"]).read_text(encoding="utf-8"))
    return next(
        n for n in tree.body if isinstance(n, ast.FunctionDef) and n.name == name
    )


def keys(mapping):
    return {k.value for k in mapping.keys if k is not None}


def assigned(node, variable):
    return {
        target.slice.value
        for item in ast.walk(node)
        if isinstance(item, ast.Assign)
        for target in item.targets
        if isinstance(target, ast.Subscript)
        and isinstance(target.value, ast.Name)
        and target.value.id == variable
    }


def test_modal_required_and_conditional_fields():
    surface = inventory()["modes"]
    node = function(surface, "riser_modal_periods")
    maps = [n for n in ast.walk(node) if isinstance(n, ast.Dict)]
    base = next(n for n in maps if "period_s" in keys(n))
    extensions = [n for n in maps if "classification" in keys(n)]
    common = set.intersection(*(keys(n) for n in extensions))
    optional = set.union(*(keys(n) for n in extensions)) - common
    assert keys(base) | common == set(surface["required_units"])
    assert optional == set(surface["conditional_units"])
    assert {ast.unparse(n.values[0]) for n in extensions} == {"xm", "r"}
    assert surface["conditional_units"]["paired_mode_number"] == "count"
    assert surface["required_units"]["period_s"] == "s"


def test_fatigue_container_and_station_fields():
    surface = inventory()["fatigue"]
    node = function(surface, "extract")
    maps = [n for n in ast.walk(node) if isinstance(n, ast.Dict)]
    initial = next(n for n in maps if "stations" in keys(n))
    station = next(n for n in maps if "arc_m" in keys(n))
    assert keys(initial) | assigned(node, "doc") == set(surface["document_units"])
    assert keys(station) == set(surface["station_units"])
    unit_value = initial.values[[k.value for k in initial.keys].index("units")]
    assert ast.literal_eval(unit_value) == surface["stress_unit"]


def test_fatigue_point_histogram_fields():
    surface = inventory()["fatigue"]
    node = function(surface, "half_cycle_histogram")
    returned = next(n.value for n in ast.walk(node) if isinstance(n, ast.Return))
    additions = assigned(function(surface, "extract"), "h")
    assert keys(returned) == set(surface["histogram_units"])
    assert additions == set(surface["point_addition_units"])
    assert not keys(returned) & additions


@pytest.mark.parametrize("name", ["modes", "fatigue"])
def test_nested_evidence_remains_unqualified(name):
    surface = inventory()[name]
    assert surface["consumer_evidence_ref"] is None
    assert surface["status"] == "NOT_EVALUATED"
    assert surface["source_revision"]
    assert surface["conditions"]
    assert surface["limitations"]
    for field, value in surface.items():
        if field.endswith("_units"):
            assert all(
                isinstance(unit, str) and unit.strip() for unit in value.values()
            )


def test_worked_field_lookup():
    """Independent symbolic example: a requested modal period maps to seconds;
    fatigue station mean stress maps to kPa. Presence establishes neither private
    consumption nor qualification. These answers follow the cited return maps,
    not a numeric solver result or the test implementation.
    """
    data = inventory()
    example = data["worked_example"]
    assert example["inputs"] == [
        "modes[].period_s",
        "w5.fatigue.stations[].points[].mean_stress",
    ]
    assert example["answer"] == {
        "modes[].period_s": "s",
        "w5.fatigue.stations[].points[].mean_stress": "kPa",
    }
    assert data["modes"]["required_units"]["period_s"] == "s"
    assert data["fatigue"]["point_addition_units"]["mean_stress"] == "kPa"
    assert example["qualification"] == "NOT_EVALUATED"
