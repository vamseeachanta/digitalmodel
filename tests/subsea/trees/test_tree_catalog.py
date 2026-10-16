# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Behaviour of an unpopulated, source-bearing tree registry."""

import importlib
import math

import pytest


def core():
    return importlib.import_module("digitalmodel.subsea.trees.tree_catalog")


def test_registry_does_not_invent_equipment():
    module = core()
    assert module.TreeCatalog().entries == ()
    with pytest.raises(KeyError):
        module.TreeCatalog().get("missing")


def fixture_entry(**changes):
    # Arithmetic fixture only: not a published equipment catalogue entry.
    values = dict(
        key="arithmetic-fixture", configuration="dual-bore", orientation="horizontal",
        production_bore_m=0.1, annulus_bore_m=0.05, rwp_pa=1e6,
        temperature_range_c=(0.0, 80.0), envelope_m=(2.0, 2.0, 3.0),
        dry_mass_kg=1000.0, wet_mass_kg=800.0, valve_count=4,
        source="https://example.org/arithmetic-fixture",
    )
    values.update(changes)
    return core().TreeEntry(**values)


def test_registry_selects_by_configuration_bore_and_pressure():
    entry = fixture_entry()
    registry = core().TreeCatalog((entry,))
    assert registry.get(entry.key) == entry
    assert registry.select("horizontal", 0.09, 1e6) == (entry,)
    assert registry.select("vertical", 0.09, 1e6) == ()
    assert registry.select("horizontal", 0.11, 1e6) == ()
    assert registry.select("horizontal", 0.09, 2e6) == ()
    with pytest.raises(ValueError, match="duplicate"):
        core().TreeCatalog((entry, entry))


@pytest.mark.parametrize("changes", [
    {"production_bore_m": math.nan}, {"rwp_pa": -1},
    {"configuration": "unknown"}, {"orientation": "unknown"},
    {"configuration": "single-bore"}, {"valve_count": 1.5},
    {"envelope_m": (1, 2)}, {"source": ""},
    {"temperature_range_c": (80, 0)}, {"wet_mass_kg": math.inf},
])
def test_invalid_tree_data_rejected(changes):
    with pytest.raises(ValueError):
        fixture_entry(**changes)


def test_package_exports_delivered_tree_surface():
    package = importlib.import_module("digitalmodel.subsea.trees")
    for symbol in ("TreeEntry", "TreeCatalog", "trim_cv", "valve_pressure_drop",
                   "series_pressure_drop", "liquid_choke_flow", "bore_capacity",
                   "flow_handoff", "tree_rating_check", "select_rated_working_pressure"):
        assert symbol in package.__all__
        assert callable(getattr(package, symbol))
