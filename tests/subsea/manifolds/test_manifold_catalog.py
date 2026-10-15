# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Source-bearing manifold registry with no invented published entries."""

import importlib

import pytest


def core():
    return importlib.import_module("digitalmodel.subsea.manifolds.manifold_catalog")


def test_registry_starts_empty():
    assert core().ManifoldCatalog().entries == ()
    with pytest.raises(KeyError):
        core().ManifoldCatalog().get("missing")


def fixture_entry(**changes):
    # Arithmetic fixture only, not manufacturer equipment data.
    values = dict(key="arithmetic-fixture", kind="gathering", slots=4,
                  header_bore_m=0.2, rating_pa=1e6, footprint_m=(3.0, 4.0),
                  dry_mass_kg=1000.0, wet_mass_kg=800.0, tie_in="connector",
                  source="https://example.org/arithmetic-fixture")
    values.update(changes)
    return core().ManifoldEntry(**values)


@pytest.mark.parametrize("kind", ["gathering", "template", "multi-slot", "plem-integrated"])
def test_registry_filters_capacity_and_type(kind):
    entry = fixture_entry(kind=kind)
    registry = core().ManifoldCatalog((entry,))
    assert registry.get(entry.key) == entry
    assert registry.select(kind, 4, 1e6) == (entry,)
    assert registry.select(kind, 5, 1e6) == ()
    assert registry.select(kind, 4, 2e6) == ()
    with pytest.raises(ValueError, match="duplicate"):
        core().ManifoldCatalog((entry, entry))


@pytest.mark.parametrize("changes", [
    {"kind": "unknown"}, {"slots": 0}, {"slots": 1.5},
    {"header_bore_m": float("nan")}, {"source": ""},
    {"footprint_m": (1, -1)}, {"tie_in": ""},
])
def test_invalid_manifold_data(changes):
    with pytest.raises(ValueError):
        fixture_entry(**changes)


def test_package_exports_delivered_manifold_surface():
    package = importlib.import_module("digitalmodel.subsea.manifolds")
    for symbol in ("ManifoldEntry", "ManifoldCatalog", "mudmat_screen", "header_screen",
                   "load_case_screen", "InjectionDemand", "distribute_chemical",
                   "aggregate_flow", "manifold_balance"):
        assert symbol in package.__all__
        assert callable(getattr(package, symbol))
