"""Tests for RAO registry and hull-RAO linking.

ABOUTME: Unit tests for RaoReference, RaoRegistry, and the catalog-linking
workflow that connects RAO diffraction results to hull panel entries.
"""

from __future__ import annotations

import json
from pathlib import Path

import numpy as np
import pytest
import yaml

from digitalmodel.hydrodynamics.hull_library.panel_catalog import (
    PanelCatalog,
    PanelCatalogEntry,
    PanelFormat,
    RaoReference,
)
from digitalmodel.hydrodynamics.hull_library.rao_registry import RaoRegistry
from digitalmodel.hydrodynamics.hull_library.profile_schema import HullType


@pytest.fixture
def raos_dir(tmp_path):
    """Create a temporary raos directory."""
    d = tmp_path / "data" / "hull_library" / "raos"
    d.mkdir(parents=True)
    return d


@pytest.fixture
def sample_rao_data():
    """Create sample RAO arrays."""
    freqs = np.linspace(0.1, 2.0, 20)
    dirs = [0.0, 45.0, 90.0, 135.0, 180.0]
    amps = np.ones((20, 5, 6)) * 0.5
    phases = np.zeros((20, 5, 6))
    return freqs, dirs, amps, phases


class TestRaoReference:
    def test_create_reference(self):
        ref = RaoReference(
            solver="orcawave",
            draft_m=12.0,
            headings_deg=[0.0, 90.0, 180.0],
            n_frequencies=20,
            date="2026-02-13",
            file_path="data/hull_library/raos/test/orcawave_12p0m_2026-02-13.json",
        )
        assert ref.solver == "orcawave"
        assert ref.draft_m == 12.0

    def test_to_dict(self):
        ref = RaoReference(solver="aqwa", draft_m=10.0, file_path="test.json")
        d = ref.to_dict()
        assert d["solver"] == "aqwa"
        assert "loading_condition" not in d  # excluded None fields

    def test_optional_fields(self):
        ref = RaoReference(solver="bemrosetta", draft_m=8.0)
        assert ref.loading_condition is None
        assert ref.headings_deg is None
        assert ref.benchmark_revision is None


class TestRaoRegistry:
    def test_register_creates_file(self, raos_dir, sample_rao_data):
        freqs, dirs, amps, phases = sample_rao_data
        registry = RaoRegistry(raos_dir)
        ref = registry.register_rao(
            hull_id="barge_100x20x10",
            solver="orcawave",
            draft_m=10.0,
            headings_deg=dirs,
            frequencies=freqs,
            amplitudes=amps,
            phases=phases,
        )
        assert ref.solver == "orcawave"
        assert ref.n_frequencies == 20
        # Check file exists
        hull_dir = raos_dir / "barge_100x20x10"
        json_files = list(hull_dir.glob("*.json"))
        assert len(json_files) == 1

    def test_register_saves_registry_yaml(self, raos_dir, sample_rao_data):
        freqs, dirs, amps, phases = sample_rao_data
        registry = RaoRegistry(raos_dir)
        registry.register_rao(
            hull_id="test_hull",
            solver="aqwa",
            draft_m=12.0,
            headings_deg=dirs,
            frequencies=freqs,
            amplitudes=amps,
            phases=phases,
        )
        assert (raos_dir / "registry.yaml").exists()
        with open(raos_dir / "registry.yaml") as f:
            data = yaml.safe_load(f)
        assert "test_hull" in data["hulls"]

    def test_lookup_by_hull_id(self, raos_dir, sample_rao_data):
        freqs, dirs, amps, phases = sample_rao_data
        registry = RaoRegistry(raos_dir)
        registry.register_rao(
            hull_id="barge_test",
            solver="orcawave",
            draft_m=10.0,
            headings_deg=dirs,
            frequencies=freqs,
            amplitudes=amps,
            phases=phases,
        )
        raos = registry.get_raos("barge_test")
        assert len(raos) == 1
        assert raos[0].solver == "orcawave"

    def test_lookup_by_solver(self, raos_dir, sample_rao_data):
        freqs, dirs, amps, phases = sample_rao_data
        registry = RaoRegistry(raos_dir)
        registry.register_rao(
            hull_id="multi_solver",
            solver="orcawave",
            draft_m=10.0,
            headings_deg=dirs,
            frequencies=freqs,
            amplitudes=amps,
            phases=phases,
        )
        registry.register_rao(
            hull_id="multi_solver",
            solver="aqwa",
            draft_m=10.0,
            headings_deg=dirs,
            frequencies=freqs,
            amplitudes=amps,
            phases=phases,
        )
        all_raos = registry.get_raos("multi_solver")
        assert len(all_raos) == 2
        orca_only = registry.get_raos("multi_solver", solver="orcawave")
        assert len(orca_only) == 1

    def test_lookup_by_draft_tolerance(self, raos_dir, sample_rao_data):
        freqs, dirs, amps, phases = sample_rao_data
        registry = RaoRegistry(raos_dir)
        registry.register_rao(
            hull_id="draft_test",
            solver="orcawave",
            draft_m=10.0,
            headings_deg=dirs,
            frequencies=freqs,
            amplitudes=amps,
            phases=phases,
        )
        # Within tolerance (0.3m diff < 0.5m)
        assert len(registry.get_raos("draft_test", draft_m=10.3)) == 1
        # Outside tolerance (0.6m diff > 0.5m)
        assert len(registry.get_raos("draft_test", draft_m=10.6)) == 0

    def test_idempotent_registration(self, raos_dir, sample_rao_data):
        freqs, dirs, amps, phases = sample_rao_data
        registry = RaoRegistry(raos_dir)
        registry.register_rao(
            hull_id="idempotent_test",
            solver="orcawave",
            draft_m=10.0,
            headings_deg=dirs,
            frequencies=freqs,
            amplitudes=amps,
            phases=phases,
        )
        # Register again with same hull+solver+draft
        registry.register_rao(
            hull_id="idempotent_test",
            solver="orcawave",
            draft_m=10.0,
            headings_deg=dirs,
            frequencies=freqs * 2,  # different data
            amplitudes=amps * 2,
            phases=phases,
        )
        raos = registry.get_raos("idempotent_test")
        assert len(raos) == 1  # only one entry

    def test_list_hulls(self, raos_dir, sample_rao_data):
        freqs, dirs, amps, phases = sample_rao_data
        registry = RaoRegistry(raos_dir)
        registry.register_rao(
            "hull_a", "orcawave", 10.0, dirs, freqs, amps, phases
        )
        registry.register_rao(
            "hull_b", "aqwa", 12.0, dirs, freqs, amps, phases
        )
        hulls = registry.list_hulls()
        assert hulls == ["hull_a", "hull_b"]

    def test_load_rao_data(self, raos_dir, sample_rao_data):
        freqs, dirs, amps, phases = sample_rao_data
        registry = RaoRegistry(raos_dir)
        ref = registry.register_rao(
            hull_id="load_test",
            solver="orcawave",
            draft_m=10.0,
            headings_deg=dirs,
            frequencies=freqs,
            amplitudes=amps,
            phases=phases,
        )
        data = registry.load_rao_data(ref)
        assert data is not None
        assert data["hull_id"] == "load_test"
        assert len(data["frequencies_rad_s"]) == 20

    def test_empty_lookup_returns_empty(self, raos_dir):
        registry = RaoRegistry(raos_dir)
        assert registry.get_raos("nonexistent") == []

    def test_registry_persistence(self, raos_dir, sample_rao_data):
        freqs, dirs, amps, phases = sample_rao_data
        registry1 = RaoRegistry(raos_dir)
        registry1.register_rao(
            "persist_test", "orcawave", 10.0, dirs, freqs, amps, phases
        )
        # Create new registry from same dir (simulates restart)
        registry2 = RaoRegistry(raos_dir)
        raos = registry2.get_raos("persist_test")
        assert len(raos) == 1

    def test_benchmark_revision(self, raos_dir, sample_rao_data):
        freqs, dirs, amps, phases = sample_rao_data
        registry = RaoRegistry(raos_dir)
        ref = registry.register_rao(
            hull_id="bench_test",
            solver="orcawave",
            draft_m=10.0,
            headings_deg=dirs,
            frequencies=freqs,
            amplitudes=amps,
            phases=phases,
            benchmark_revision="r4",
        )
        assert ref.benchmark_revision == "r4"


class TestCatalogLinking:
    def test_link_to_catalog(self, raos_dir, sample_rao_data):
        freqs, dirs, amps, phases = sample_rao_data
        registry = RaoRegistry(raos_dir)
        registry.register_rao(
            "hull_a", "orcawave", 10.0, dirs, freqs, amps, phases
        )

        catalog = PanelCatalog(
            entries=[
                PanelCatalogEntry(
                    hull_id="hull_a",
                    hull_type=HullType.BARGE,
                    name="Test Barge",
                    source="test",
                    panel_format=PanelFormat.GDF,
                    file_path="test.gdf",
                ),
                PanelCatalogEntry(
                    hull_id="hull_b",
                    hull_type=HullType.SHIP,
                    name="Test Ship",
                    source="test",
                    panel_format=PanelFormat.GDF,
                    file_path="test.gdf",
                ),
            ]
        )
        updated = registry.link_to_catalog(catalog)
        assert updated == 1
        assert catalog.entries[0].raos is not None
        assert len(catalog.entries[0].raos) == 1
        assert catalog.entries[1].raos is None


class TestPanelCatalogRaoRoundTrip:
    def test_yaml_round_trip_with_raos(self, tmp_path):
        entry = PanelCatalogEntry(
            hull_id="roundtrip_test",
            hull_type=HullType.BARGE,
            name="Round Trip Test",
            source="test",
            panel_format=PanelFormat.GDF,
            file_path="test.gdf",
            raos=[
                RaoReference(
                    solver="orcawave",
                    draft_m=10.0,
                    headings_deg=[0.0, 90.0, 180.0],
                    n_frequencies=20,
                    date="2026-02-13",
                    file_path="data/hull_library/raos/test/orcawave.json",
                ),
            ],
        )
        catalog = PanelCatalog(entries=[entry])
        yaml_path = tmp_path / "test_catalog.yaml"
        catalog.to_yaml(yaml_path)

        loaded = PanelCatalog.from_yaml(yaml_path)
        assert len(loaded.entries) == 1
        assert loaded.entries[0].raos is not None
        assert len(loaded.entries[0].raos) == 1
        assert loaded.entries[0].raos[0].solver == "orcawave"
        assert loaded.entries[0].raos[0].draft_m == 10.0

    def test_yaml_round_trip_without_raos(self, tmp_path):
        """Backward compat: entries without raos still round-trip."""
        entry = PanelCatalogEntry(
            hull_id="no_rao_test",
            hull_type=HullType.SHIP,
            name="No RAO Test",
            source="test",
            panel_format=PanelFormat.GDF,
            file_path="test.gdf",
        )
        catalog = PanelCatalog(entries=[entry])
        yaml_path = tmp_path / "test_no_rao.yaml"
        catalog.to_yaml(yaml_path)

        loaded = PanelCatalog.from_yaml(yaml_path)
        assert loaded.entries[0].raos is None

    def test_csv_export_with_raos(self, tmp_path):
        entry = PanelCatalogEntry(
            hull_id="csv_test",
            hull_type=HullType.BARGE,
            name="CSV Test",
            source="test",
            panel_format=PanelFormat.GDF,
            file_path="test.gdf",
            raos=[
                RaoReference(
                    solver="aqwa", draft_m=8.0, file_path="test.json"
                ),
            ],
        )
        catalog = PanelCatalog(entries=[entry])
        csv_path = tmp_path / "test.csv"
        catalog.to_csv(csv_path)
        content = csv_path.read_text()
        assert "csv_test" in content
