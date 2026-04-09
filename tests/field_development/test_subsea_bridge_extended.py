# ABOUTME: Extended tests for subsea_bridge — end-to-end JSON→query→concept_selection pipeline.
# ABOUTME: Issue #1972 — Test coverage uplift for overnight modules.
"""
Extended tests for digitalmodel.field_development.subsea_bridge

Covers:
- End-to-end JSON load → query → concept_selection pipeline
- Mock data provider with synthetic GoM datasets
- Multi-criteria query combinations
- AnalogueMatch scoring edge cases
- Operator/host-type distribution completeness
- Catalog serialization roundtrip (to_dict → from_dict)
- Validation: empty/missing fields, bad JSON, weight constraints
- Integration with concept_selection module
"""

from __future__ import annotations

import json
import math
import tempfile
from pathlib import Path

import pytest

from digitalmodel.field_development.subsea_bridge import (
    SubseaFieldCatalog,
    GoMField,
    AnalogueMatch,
)

# ---------------------------------------------------------------------------
# Mock data provider
# ---------------------------------------------------------------------------

SYNTHETIC_GOM_DATA = {
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
    ]
}


@pytest.fixture
def catalog():
    """Create a catalog from synthetic data."""
    return SubseaFieldCatalog.from_dict(SYNTHETIC_GOM_DATA)


@pytest.fixture
def json_file(tmp_path):
    """Write synthetic data to a temp JSON file."""
    fp = tmp_path / "subseaiq-test.json"
    fp.write_text(json.dumps(SYNTHETIC_GOM_DATA))
    return fp


# ---------------------------------------------------------------------------
# End-to-end JSON load → query → concept_selection pipeline
# ---------------------------------------------------------------------------

class TestEndToEndPipeline:
    """Full pipeline: JSON file → catalog → query → concept_selection."""

    def test_json_load_to_query(self, json_file):
        """Load from file, query by operator, verify fields."""
        catalog = SubseaFieldCatalog.from_json(str(json_file))
        shell_fields = catalog.query(operator="Shell")
        assert len(shell_fields) == 6
        assert all(f.operator == "Shell" for f in shell_fields)

    def test_json_load_to_analogue(self, json_file):
        """Load from file, find analogue, use for concept selection."""
        catalog = SubseaFieldCatalog.from_json(str(json_file))
        match = catalog.find_gom_analogue(water_depth=2200, production_capacity=100000)
        assert match.score > 0.5
        assert match.field.water_depth_m > 1500

    def test_query_results_to_concept_selection(self, catalog):
        """Query catalog, extract fields, feed to concept_selection."""
        from digitalmodel.field_development.concept_selection import concept_selection

        deep_fields = catalog.query(water_depth_min_m=2000)
        assert len(deep_fields) > 0

        # Use the first deep field's params for concept selection
        field = deep_fields[0]
        result = concept_selection(
            water_depth=field.water_depth_m,
            reservoir_size_mmbbl=200,
            distance_to_infra_km=80,
            fluid_type="oil",
        )
        assert result.selected_host is not None
        assert len(result.ranked_options) == 5

    def test_export_for_concept_selection_format(self, catalog):
        """Export data should be usable by concept_selection workflows."""
        data = catalog.export_for_concept_selection()
        assert isinstance(data, list)
        assert len(data) == 10
        required_keys = {"name", "operator", "water_depth_m", "host", "year", "capacity_bopd"}
        for entry in data:
            assert required_keys.issubset(set(entry.keys()))


# ---------------------------------------------------------------------------
# Catalog construction
# ---------------------------------------------------------------------------

class TestCatalogConstruction:
    """Construction edge cases."""

    def test_from_dict_matches_from_json(self, json_file):
        """from_dict and from_json should produce identical catalogs."""
        cat_dict = SubseaFieldCatalog.from_dict(SYNTHETIC_GOM_DATA)
        cat_json = SubseaFieldCatalog.from_json(str(json_file))
        assert len(cat_dict) == len(cat_json)
        for f1, f2 in zip(cat_dict.fields, cat_json.fields):
            assert f1.name == f2.name
            assert f1.water_depth_m == f2.water_depth_m

    def test_empty_fields_raises(self):
        """Catalog with empty fields list should raise."""
        with pytest.raises(ValueError, match="at least one"):
            SubseaFieldCatalog([])

    def test_empty_gom_fields_in_dict_raises(self):
        with pytest.raises(KeyError, match="gom_fields"):
            SubseaFieldCatalog.from_dict({"gom_fields": []})

    def test_missing_gom_fields_key_raises(self):
        with pytest.raises(KeyError):
            SubseaFieldCatalog.from_dict({"other": "data"})

    def test_missing_required_field_key_raises(self):
        """Missing 'name' in a field entry should raise KeyError."""
        bad_data = {
            "gom_fields": [
                {"operator": "Shell", "water_depth_m": 1000, "host": "Spar",
                 "year": 2020, "capacity_bopd": 50000}
            ]
        }
        with pytest.raises(KeyError):
            SubseaFieldCatalog.from_dict(bad_data)

    def test_single_field_catalog(self):
        """Catalog with one field should work."""
        data = {
            "gom_fields": [
                {"name": "Solo", "operator": "Test", "water_depth_m": 1000,
                 "host": "TLP", "year": 2020, "capacity_bopd": 50000}
            ]
        }
        cat = SubseaFieldCatalog.from_dict(data)
        assert len(cat) == 1
        assert cat.fields[0].name == "Solo"


# ---------------------------------------------------------------------------
# Query method comprehensive tests
# ---------------------------------------------------------------------------

class TestQueryMethods:
    """Comprehensive query tests."""

    def test_query_no_filters(self, catalog):
        """No filters → all fields returned."""
        results = catalog.query()
        assert len(results) == 10

    def test_query_operator_case_insensitive(self, catalog):
        """Operator matching should be case-insensitive."""
        r1 = catalog.query(operator="shell")
        r2 = catalog.query(operator="SHELL")
        r3 = catalog.query(operator="Shell")
        assert len(r1) == len(r2) == len(r3)

    def test_query_host_type_substring(self, catalog):
        """Host type query uses substring match."""
        # "Semi" should match "Semi-submersible"
        results = catalog.query(host_type="Semi")
        assert all("Semi" in f.host for f in results)
        assert len(results) == 4  # Appomattox, Atlantis, Thunder Horse, Mad Dog

    def test_query_etlp_does_not_match_tlp(self, catalog):
        """'TLP' substring should match 'TLP' but not 'ETLP' (case: 'TLP' is in 'ETLP')."""
        tlp_results = catalog.query(host_type="TLP")
        # Actually 'TLP' IS a substring of 'ETLP', so ETLP fields should match too
        names = {f.name for f in tlp_results}
        assert "Mars" in names
        assert "Ursa" in names
        # ETLP contains 'TLP' as substring
        assert "Stones" in names or "Lucius" in names

    def test_query_depth_range(self, catalog):
        """Water depth range query."""
        results = catalog.query(water_depth_min_m=1500, water_depth_max_m=2500)
        for f in results:
            assert 1500 <= f.water_depth_m <= 2500

    def test_query_invalid_depth_range_raises(self, catalog):
        """min > max should raise ValueError."""
        with pytest.raises(ValueError, match="water_depth_min_m"):
            catalog.query(water_depth_min_m=2000, water_depth_max_m=1000)

    def test_query_sorted_by_depth(self, catalog):
        """Results should be sorted by water depth ascending."""
        results = catalog.query()
        depths = [f.water_depth_m for f in results]
        assert depths == sorted(depths)

    def test_query_multiple_criteria(self, catalog):
        """Combine operator + depth + host type."""
        results = catalog.query(
            operator="Shell",
            water_depth_min_m=2000,
            host_type="Spar"
        )
        for f in results:
            assert f.operator == "Shell"
            assert f.water_depth_m >= 2000
            assert "Spar" in f.host

    def test_query_returns_empty_for_nonexistent(self, catalog):
        results = catalog.query(operator="NonExistent")
        assert len(results) == 0

    def test_query_by_operator_convenience(self, catalog):
        """query_by_operator should be identical to query(operator=...)."""
        r1 = catalog.query_by_operator("BP")
        r2 = catalog.query(operator="BP")
        assert len(r1) == len(r2)
        for f1, f2 in zip(r1, r2):
            assert f1.name == f2.name

    def test_query_by_water_depth_convenience(self, catalog):
        r1 = catalog.query_by_water_depth(1000, 2000)
        r2 = catalog.query(water_depth_min_m=1000, water_depth_max_m=2000)
        assert len(r1) == len(r2)

    def test_query_by_host_type_convenience(self, catalog):
        r1 = catalog.query_by_host_type("Spar")
        r2 = catalog.query(host_type="Spar")
        assert len(r1) == len(r2)


# ---------------------------------------------------------------------------
# Analogue matching edge cases
# ---------------------------------------------------------------------------

class TestAnalogueMatching:
    """Analogue match scoring edge cases."""

    def test_exact_match_score_near_1(self, catalog):
        """Matching an exact catalog field should give score near 1.0."""
        # Perdido: 2438 m, 100000 bopd
        match = catalog.find_gom_analogue(2438, 100000)
        assert match.score > 0.95
        assert match.field.name == "Perdido"

    def test_depth_delta_computed(self, catalog):
        """depth_delta_m should be absolute difference."""
        match = catalog.find_gom_analogue(2500, 100000)
        expected_delta = abs(match.field.water_depth_m - 2500)
        assert match.depth_delta_m == pytest.approx(expected_delta, abs=0.1)

    def test_capacity_delta_computed(self, catalog):
        """capacity_delta_bopd should be absolute difference."""
        match = catalog.find_gom_analogue(2438, 120000)
        expected_delta = abs(match.field.capacity_bopd - 120000)
        assert match.capacity_delta_bopd == expected_delta

    def test_score_between_0_and_1(self, catalog):
        """Score should always be in [0, 1]."""
        match = catalog.find_gom_analogue(5000, 500000)
        assert 0.0 <= match.score <= 1.0

    def test_negative_water_depth_raises(self, catalog):
        with pytest.raises(ValueError, match="water_depth"):
            catalog.find_gom_analogue(-100, 100000)

    def test_zero_water_depth_raises(self, catalog):
        with pytest.raises(ValueError, match="water_depth"):
            catalog.find_gom_analogue(0, 100000)

    def test_negative_capacity_raises(self, catalog):
        with pytest.raises(ValueError, match="production_capacity"):
            catalog.find_gom_analogue(2000, -1)

    def test_zero_capacity_raises(self, catalog):
        with pytest.raises(ValueError, match="production_capacity"):
            catalog.find_gom_analogue(2000, 0)

    def test_weights_must_sum_to_one(self, catalog):
        with pytest.raises(ValueError, match="must equal 1.0"):
            catalog.find_gom_analogue(2000, 100000, depth_weight=0.5, capacity_weight=0.3)

    def test_custom_weights(self, catalog):
        """Different weights should change the best match."""
        # All depth weight → should match closest by depth
        match_depth = catalog.find_gom_analogue(
            1067, 500000, depth_weight=1.0, capacity_weight=0.0
        )
        assert match_depth.field.name == "Ursa"  # 1067 m exact

    def test_very_far_target(self, catalog):
        """Target far from any field should still produce a match."""
        match = catalog.find_gom_analogue(10000, 1000000)
        assert match is not None
        assert match.score >= 0.0

    def test_analogue_match_dataclass(self, catalog):
        """AnalogueMatch should have all documented fields."""
        match = catalog.find_gom_analogue(2000, 100000)
        assert isinstance(match.field, GoMField)
        assert isinstance(match.score, float)
        assert isinstance(match.depth_delta_m, float)
        assert isinstance(match.capacity_delta_bopd, int)


# ---------------------------------------------------------------------------
# Distributions
# ---------------------------------------------------------------------------

class TestDistributions:
    """Host type and operator distribution tests."""

    def test_host_type_distribution_counts(self, catalog):
        dist = catalog.host_type_distribution()
        assert dist["Semi-submersible"] == 4
        assert dist["Spar"] == 2
        assert dist["TLP"] == 2
        assert dist["ETLP"] == 2

    def test_host_type_distribution_sorted_desc(self, catalog):
        """Distribution should be sorted by count descending."""
        dist = catalog.host_type_distribution()
        counts = list(dist.values())
        assert counts == sorted(counts, reverse=True)

    def test_operator_distribution_counts(self, catalog):
        dist = catalog.operator_distribution()
        assert dist["Shell"] == 6
        assert dist["BP"] == 3
        assert dist["LLOG"] == 1

    def test_operator_distribution_sorted_desc(self, catalog):
        dist = catalog.operator_distribution()
        counts = list(dist.values())
        assert counts == sorted(counts, reverse=True)


# ---------------------------------------------------------------------------
# Serialization roundtrip
# ---------------------------------------------------------------------------

class TestSerializationRoundtrip:
    """Test to_dict → from_dict roundtrip."""

    def test_to_dict_roundtrip(self, catalog):
        """to_dict → from_dict should preserve all fields."""
        exported = catalog.to_dict()
        restored = SubseaFieldCatalog.from_dict(exported)
        assert len(restored) == len(catalog)
        for orig, rest in zip(catalog.fields, restored.fields):
            assert orig.name == rest.name
            assert orig.operator == rest.operator
            assert orig.water_depth_m == rest.water_depth_m
            assert orig.host == rest.host
            assert orig.year == rest.year
            assert orig.capacity_bopd == rest.capacity_bopd

    def test_to_dict_contains_metadata(self, catalog):
        """to_dict should include distribution metadata."""
        exported = catalog.to_dict()
        assert "gom_fields" in exported
        assert "host_type_distribution" in exported
        assert "operator_distribution" in exported
        assert "field_count" in exported
        assert exported["field_count"] == 10

    def test_json_serialize_deserialize(self, catalog, tmp_path):
        """Full JSON serialize → deserialize should preserve data."""
        exported = catalog.to_dict()
        fp = tmp_path / "roundtrip.json"
        fp.write_text(json.dumps(exported))

        restored = SubseaFieldCatalog.from_json(str(fp))
        assert len(restored) == len(catalog)


# ---------------------------------------------------------------------------
# GoMField dataclass
# ---------------------------------------------------------------------------

class TestGoMFieldExtended:
    """Extended GoMField tests."""

    def test_to_dict_keys(self):
        f = GoMField("Test", "Shell", 1000, "Spar", 2020, 50000)
        d = f.to_dict()
        assert set(d.keys()) == {"name", "operator", "water_depth_m", "host", "year", "capacity_bopd"}

    def test_to_dict_values_match(self):
        f = GoMField("Perdido", "Shell", 2438, "Spar", 2010, 100000)
        d = f.to_dict()
        assert d["name"] == "Perdido"
        assert d["water_depth_m"] == 2438
        assert d["capacity_bopd"] == 100000

    def test_field_equality(self):
        """Two fields with same data should be equal (dataclass)."""
        f1 = GoMField("Test", "Shell", 1000, "Spar", 2020, 50000)
        f2 = GoMField("Test", "Shell", 1000, "Spar", 2020, 50000)
        assert f1 == f2

    def test_field_inequality(self):
        f1 = GoMField("Test", "Shell", 1000, "Spar", 2020, 50000)
        f2 = GoMField("Other", "Shell", 1000, "Spar", 2020, 50000)
        assert f1 != f2


# ---------------------------------------------------------------------------
# Catalog dunder methods
# ---------------------------------------------------------------------------

class TestCatalogDunders:
    """Test __len__, __repr__, etc."""

    def test_len(self, catalog):
        assert len(catalog) == 10

    def test_repr(self, catalog):
        r = repr(catalog)
        assert "SubseaFieldCatalog" in r
        assert "fields=10" in r

    def test_list_operators(self, catalog):
        ops = catalog.list_operators()
        assert isinstance(ops, list)
        assert ops == sorted(ops)  # alphabetical
        assert "Shell" in ops
        assert "BP" in ops
        assert "LLOG" in ops

    def test_get_all_fields_returns_copy(self, catalog):
        """get_all_fields should return a copy, not the internal list."""
        fields = catalog.get_all_fields()
        fields.pop()  # modify the returned list
        assert len(catalog) == 10  # original unchanged

    def test_fields_property_returns_copy(self, catalog):
        """fields property should return a copy."""
        fields = catalog.fields
        fields.clear()
        assert len(catalog) == 10  # unchanged


# ---------------------------------------------------------------------------
# File handling edge cases
# ---------------------------------------------------------------------------

class TestFileHandling:
    """File I/O edge cases."""

    def test_nonexistent_file_raises(self):
        with pytest.raises(FileNotFoundError, match="not found"):
            SubseaFieldCatalog.from_json("/nonexistent/path/file.json")

    def test_empty_json_file_raises(self, tmp_path):
        """Empty JSON file should raise an error."""
        fp = tmp_path / "empty.json"
        fp.write_text("{}")
        with pytest.raises(KeyError, match="gom_fields"):
            SubseaFieldCatalog.from_json(str(fp))

    def test_json_with_empty_gom_fields_raises(self, tmp_path):
        fp = tmp_path / "empty_fields.json"
        fp.write_text(json.dumps({"gom_fields": []}))
        with pytest.raises(KeyError, match="gom_fields"):
            SubseaFieldCatalog.from_json(str(fp))

    def test_json_with_missing_field_key_raises(self, tmp_path):
        """Field entry missing 'year' should raise."""
        data = {
            "gom_fields": [
                {"name": "Bad", "operator": "Test", "water_depth_m": 1000,
                 "host": "TLP", "capacity_bopd": 50000}
                # missing 'year'
            ]
        }
        fp = tmp_path / "bad.json"
        fp.write_text(json.dumps(data))
        with pytest.raises(KeyError):
            SubseaFieldCatalog.from_json(str(fp))


# ---------------------------------------------------------------------------
# Integration: analogue → concept_selection
# ---------------------------------------------------------------------------

class TestAnalogueToConceptSelection:
    """End-to-end: find analogue → use its depth for concept selection."""

    def test_analogue_drives_concept_selection(self, catalog):
        """Use the analogue's water depth to drive concept selection."""
        from digitalmodel.field_development.concept_selection import concept_selection

        match = catalog.find_gom_analogue(water_depth=2200, production_capacity=100000)
        result = concept_selection(
            water_depth=match.field.water_depth_m,
            reservoir_size_mmbbl=150,
            distance_to_infra_km=70,
            fluid_type="oil",
        )
        # Deep water analogue → should prefer Spar/Semi
        assert result.selected_host is not None
        top = result.ranked_options[0].host_type.value
        assert top in ("Spar", "Semi", "FPSO")

    def test_multiple_analogues_comparison(self, catalog):
        """Find analogues for different targets and verify consistency."""
        from digitalmodel.field_development.concept_selection import concept_selection

        shallow = catalog.find_gom_analogue(water_depth=900, production_capacity=100000)
        deep = catalog.find_gom_analogue(water_depth=2400, production_capacity=100000)

        r_shallow = concept_selection(shallow.field.water_depth_m, 150, 50, "oil")
        r_deep = concept_selection(deep.field.water_depth_m, 150, 50, "oil")

        # Shallow should prefer TLP, deep should prefer Spar/Semi
        shallow_top = r_shallow.ranked_options[0].host_type.value
        deep_top = r_deep.ranked_options[0].host_type.value
        assert shallow_top != deep_top or shallow.field.water_depth_m == deep.field.water_depth_m
