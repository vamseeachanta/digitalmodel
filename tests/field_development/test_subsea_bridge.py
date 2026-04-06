"""Tests for SubseaIQ-to-field-development bridge module (#1861)."""

import pytest
import json
from pathlib import Path

from digitalmodel.field_development.subsea_bridge import (
    SubseaFieldCatalog,
    GoMField,
    AnalogueMatch,
)

# Path to the overnight research JSON
RESEARCH_JSON = Path(__file__).parents[3] / "data" / "field-development" / "subseaiq-scan-latest.json"


@pytest.fixture
def research_data():
    """Load the research JSON if it exists, otherwise return synthetic data."""
    if RESEARCH_JSON.exists():
        with open(RESEARCH_JSON) as f:
            return json.load(f)
    # Synthetic fallback matching the known data format
    return {
        "report_type": "Field Development Intelligence",
        "gom_fields": [
            {"name": "Perdido", "operator": "Shell", "water_depth_m": 2438, "host": "Spar", "year": 2010, "capacity_bopd": 100000},
            {"name": "Appomattox", "operator": "Shell", "water_depth_m": 2250, "host": "Semi-submersible", "year": 2019, "capacity_bopd": 125000},
            {"name": "Atlantis", "operator": "BP", "water_depth_m": 2150, "host": "Semi-submersible", "year": 2007, "capacity_bopd": 200000},
            {"name": "Thunder Horse", "operator": "BP", "water_depth_m": 1844, "host": "Semi-submersible", "year": 2008, "capacity_bopd": 250000},
            {"name": "Mars", "operator": "Shell", "water_depth_m": 896, "host": "TLP", "year": 1996, "capacity_bopd": 100000},
            {"name": "Mad Dog", "operator": "BP", "water_depth_m": 1480, "host": "Semi-submersible", "year": 2022, "capacity_bopd": 140000},
            {"name": "Stones", "operator": "Shell", "water_depth_m": 2900, "host": "ETLP", "year": 2016, "capacity_bopd": 20000},
            {"name": "Lucius", "operator": "LLOG", "water_depth_m": 2270, "host": "ETLP", "year": 2016, "capacity_bopd": 80000},
            {"name": "Ursa", "operator": "Shell", "water_depth_m": 1067, "host": "TLP", "year": 1999, "capacity_bopd": 150000},
            {"name": "Whale", "operator": "Shell", "water_depth_m": 2100, "host": "Spar", "year": 2024, "capacity_bopd": 45000},
        ],
    }


@pytest.fixture
def catalog(research_data):
    """Create a catalog from the research data."""
    # Save to temp file for from_json loader
    tmp = Path("/tmp/test_subseaiq.json")
    with open(tmp, "w") as f:
        json.dump(research_data, f)
    return SubseaFieldCatalog.from_json(str(tmp))


class TestGoMField:
    """Tests for the GoMField dataclass."""

    def test_to_dict(self):
        f = GoMField(name="Test", operator="Shell", water_depth_m=1000,
                     host="Spar", year=2020, capacity_bopd=50000)
        d = f.to_dict()
        assert d["name"] == "Test"
        assert d["operator"] == "Shell"
        assert d["water_depth_m"] == 1000
        assert d["host"] == "Spar"
        assert d["year"] == 2020
        assert d["capacity_bopd"] == 50000


class TestSubseaFieldCatalog:
    """Tests for the SubseaFieldCatalog."""

    def test_load_from_json(self, catalog):
        assert len(catalog.fields) == 10

    def test_query_by_operator(self, catalog):
        shell = catalog.query(operator="Shell")
        assert len(shell) == 6  # Perdido, Appomattox, Mars, Stones, Ursa, Whale

        bp = catalog.query(operator="BP")
        assert len(bp) == 3  # Atlantis, Thunder Horse, Mad Dog

    def test_query_by_host_type(self, catalog):
        spars = catalog.query(host_type="Spar")
        assert len(spars) == 2  # Perdido, Whale

        tlps = catalog.query(host_type="TLP")
        assert len(tlps) == 2  # Mars, Ursa

    def test_query_by_water_depth(self, catalog):
        deep = catalog.query(min_water_depth=2000)
        assert all(f.water_depth_m >= 2000 for f in deep)

        shallow = catalog.query(max_water_depth=1500)
        assert all(f.water_depth_m <= 1500 for f in shallow)

    def test_query_combined(self, catalog):
        """Query by both operator and water depth."""
        results = catalog.query(operator="Shell", min_water_depth=2000)
        assert all(f.operator == "Shell" for f in results)
        assert all(f.water_depth_m >= 2000 for f in results)

    def test_empty_query(self, catalog):
        results = catalog.query(operator="NonExistent")
        assert len(results) == 0

    def test_host_type_distribution(self, catalog):
        dist = catalog.host_type_distribution()
        assert "Spar" in dist
        assert dist["Spar"] == 2
        assert "TLP" in dist
        assert dist["TLP"] == 2
        # Semi-sub should have 4: Appomattox, Atlantis, Thunder Horse, Mad Dog
        assert dist.get("Semi-submersible", 0) == 4

    def test_find_gom_analogue_deepwater(self, catalog):
        """Find analogue for 2200m water depth, 100k bopd."""
        match = catalog.find_gom_analogue(
            water_depth=2200, production_capacity=100000
        )
        assert match is not None
        # Should match a deepwater semi or spar
        assert match.field.water_depth_m >= 1800

    def test_find_gom_analogue_shallow(self, catalog):
        """Find analogue for 800m water depth, 80k bopd."""
        match = catalog.find_gom_analogue(
            water_depth=800, production_capacity=80000
        )
        assert match is not None
        # Should match Mars-like TLP
        assert match.field.water_depth_m < 1500

    def test_analogue_match_score(self, catalog):
        """Analogue match should have a meaningful score."""
        match = catalog.find_gom_analogue(
            water_depth=2400, production_capacity=100000
        )
        assert match is not None
        assert 0.0 < match.score <= 1.0
        # Perdido: 2438m, 100k bopd — should be closest to 2400m, 100k
        assert match.field.name == "Perdido"

    def test_all_operators(self, catalog):
        """List all unique operators."""
        ops = catalog.list_operators()
        assert "Shell" in ops
        assert "BP" in ops
        assert "LLOG" in ops

    def test_export_for_concept_selection(self, catalog):
        """Export data for use by concept_selection module."""
        data = catalog.export_for_concept_selection()
        assert isinstance(data, list)
        assert len(data) == 10
        for field in data:
            assert "name" in field
            assert "water_depth_m" in field
            assert "host" in field
            assert "capacity_bopd" in field

    def test_from_dict(self, research_data):
        """Load catalog from dict instead of file."""
        catalog = SubseaFieldCatalog.from_dict(research_data)
        assert len(catalog.fields) == 10
        assert catalog.fields[0].name == "Perdido"

    def test_field_count(self, catalog):
        assert len(catalog.fields) == 10

    def test_water_depth_range(self, catalog):
        depths = [f.water_depth_m for f in catalog.fields]
        assert min(depths) < 1000  # Mars at 896m
        assert max(depths) > 2800  # Stones at 2900m
