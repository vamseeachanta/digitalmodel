"""
ABOUTME: TDD tests for line_parser — parsing hull line definitions from
CSV, JSON, and YAML into HullLineDefinition data structures.

Test coverage:
- CSV station-offset parsing (multi-station file)
- JSON hull definition loading
- YAML hull definition loading
- Round-trip: load -> serialise -> re-load equality
- Validation errors: missing required fields, negative offsets
- Waterline and profile curve extraction
"""

from __future__ import annotations

import json
from pathlib import Path

import pytest


class TestHullLineDefinitionModel:
    """Tests for the HullLineDefinition pydantic model."""

    def test_construct_minimal_valid(self):
        """Minimal valid HullLineDefinition can be constructed."""
        from digitalmodel.hydrodynamics.hull_library.line_generator.line_parser import (
            HullLineDefinition,
            StationOffset,
        )

        stations = [
            StationOffset(x=0.0, offsets=[(0.0, 5.0), (3.0, 5.0)]),
            StationOffset(x=10.0, offsets=[(0.0, 5.0), (3.0, 5.0)]),
        ]
        defn = HullLineDefinition(
            name="test",
            hull_type="barge",
            length_bp=10.0,
            beam=10.0,
            draft=3.0,
            depth=4.0,
            source="test",
            stations=stations,
        )
        assert defn.name == "test"
        assert len(defn.stations) == 2

    def test_requires_at_least_two_stations(self):
        """Construction fails if fewer than two stations are provided."""
        from digitalmodel.hydrodynamics.hull_library.line_generator.line_parser import (
            HullLineDefinition,
            StationOffset,
        )

        with pytest.raises(ValueError, match="at least two"):
            HullLineDefinition(
                name="x",
                hull_type="barge",
                length_bp=10.0,
                beam=10.0,
                draft=3.0,
                depth=4.0,
                source="test",
                stations=[
                    StationOffset(x=0.0, offsets=[(0.0, 5.0)]),
                ],
            )

    def test_negative_half_breadth_rejected(self):
        """Negative half-breadth offset is rejected."""
        from digitalmodel.hydrodynamics.hull_library.line_generator.line_parser import (
            StationOffset,
        )

        with pytest.raises(ValueError, match="non-negative"):
            StationOffset(x=0.0, offsets=[(0.0, -1.0)])

    def test_negative_z_offset_rejected(self):
        """Negative z offset is rejected (keel-up convention, z >= 0)."""
        from digitalmodel.hydrodynamics.hull_library.line_generator.line_parser import (
            StationOffset,
        )

        with pytest.raises(ValueError, match="non-negative"):
            StationOffset(x=0.0, offsets=[(-1.0, 5.0)])

    def test_waterlines_default_empty(self, box_barge_json):
        """Waterlines list defaults to empty."""
        from digitalmodel.hydrodynamics.hull_library.line_generator.line_parser import (
            HullLineDefinition,
        )

        defn = HullLineDefinition(**box_barge_json)
        assert defn.waterlines == []

    def test_profile_default_empty(self, box_barge_json):
        """Profile list defaults to empty."""
        from digitalmodel.hydrodynamics.hull_library.line_generator.line_parser import (
            HullLineDefinition,
        )

        defn = HullLineDefinition(**box_barge_json)
        assert defn.profile == []


class TestLineParserCSV:
    """Tests for CSV parsing of hull station offsets."""

    def test_parse_csv_returns_definition(self, box_barge_csv_path):
        """parse_csv returns a HullLineDefinition from a valid CSV."""
        from digitalmodel.hydrodynamics.hull_library.line_generator.line_parser import (
            LineParser,
        )

        parser = LineParser()
        defn = parser.parse_csv(
            box_barge_csv_path,
            name="box_barge",
            hull_type="barge",
            length_bp=100.0,
            beam=20.0,
            draft=5.0,
            depth=6.0,
            source="test",
        )
        assert defn is not None
        assert defn.name == "box_barge"

    def test_parse_csv_station_count(self, box_barge_csv_path):
        """CSV with 3 distinct x-positions produces 3 stations."""
        from digitalmodel.hydrodynamics.hull_library.line_generator.line_parser import (
            LineParser,
        )

        parser = LineParser()
        defn = parser.parse_csv(
            box_barge_csv_path,
            name="box_barge",
            hull_type="barge",
            length_bp=100.0,
            beam=20.0,
            draft=5.0,
            depth=6.0,
            source="test",
        )
        assert len(defn.stations) == 3

    def test_parse_csv_offset_values(self, box_barge_csv_path):
        """CSV rows are correctly parsed into (z, y) offset pairs per station."""
        from digitalmodel.hydrodynamics.hull_library.line_generator.line_parser import (
            LineParser,
        )

        parser = LineParser()
        defn = parser.parse_csv(
            box_barge_csv_path,
            name="box_barge",
            hull_type="barge",
            length_bp=100.0,
            beam=20.0,
            draft=5.0,
            depth=6.0,
            source="test",
        )
        # First station should be at x=0.0 with two offsets
        first = next(s for s in defn.stations if s.x == 0.0)
        assert len(first.offsets) == 2
        # Each offset has z and y >= 0
        for z, y in first.offsets:
            assert z >= 0.0
            assert y >= 0.0

    def test_parse_csv_stations_sorted_by_x(self, box_barge_csv_path):
        """Stations from CSV are sorted in ascending x order."""
        from digitalmodel.hydrodynamics.hull_library.line_generator.line_parser import (
            LineParser,
        )

        parser = LineParser()
        defn = parser.parse_csv(
            box_barge_csv_path,
            name="box_barge",
            hull_type="barge",
            length_bp=100.0,
            beam=20.0,
            draft=5.0,
            depth=6.0,
            source="test",
        )
        xs = [s.x for s in defn.stations]
        assert xs == sorted(xs)


class TestLineParserJSON:
    """Tests for JSON parsing of hull definitions."""

    def test_parse_json_file_returns_definition(self, box_barge_json_path):
        """parse_json loads a valid JSON file to HullLineDefinition."""
        from digitalmodel.hydrodynamics.hull_library.line_generator.line_parser import (
            LineParser,
        )

        parser = LineParser()
        defn = parser.parse_json(box_barge_json_path)
        assert defn.name == "box_barge"

    def test_parse_json_dict_returns_definition(self, box_barge_json):
        """parse_json_dict builds HullLineDefinition from a plain dict."""
        from digitalmodel.hydrodynamics.hull_library.line_generator.line_parser import (
            LineParser,
        )

        parser = LineParser()
        defn = parser.parse_json_dict(box_barge_json)
        assert defn.hull_type == "barge"
        assert defn.length_bp == 100.0

    def test_parse_json_station_count(self, box_barge_json_path):
        """JSON with 3 station entries produces 3 stations."""
        from digitalmodel.hydrodynamics.hull_library.line_generator.line_parser import (
            LineParser,
        )

        parser = LineParser()
        defn = parser.parse_json(box_barge_json_path)
        assert len(defn.stations) == 3


class TestLineParserYAML:
    """Tests for YAML parsing of hull definitions."""

    def test_parse_yaml_returns_definition(self, box_barge_yaml_path):
        """parse_yaml loads a valid YAML file to HullLineDefinition."""
        from digitalmodel.hydrodynamics.hull_library.line_generator.line_parser import (
            LineParser,
        )

        parser = LineParser()
        defn = parser.parse_yaml(box_barge_yaml_path)
        assert defn.name == "box_barge"
        assert len(defn.stations) == 3

    def test_round_trip_yaml(self, box_barge_json, tmp_path):
        """load -> save_yaml -> load_yaml produces equal HullLineDefinition."""
        from digitalmodel.hydrodynamics.hull_library.line_generator.line_parser import (
            LineParser,
        )

        parser = LineParser()
        defn_a = parser.parse_json_dict(box_barge_json)

        out_path = tmp_path / "round_trip.yaml"
        defn_a.save_yaml(out_path)

        defn_b = parser.parse_yaml(out_path)
        assert defn_b.name == defn_a.name
        assert defn_b.length_bp == defn_a.length_bp
        assert len(defn_b.stations) == len(defn_a.stations)

    def test_round_trip_json(self, box_barge_json, tmp_path):
        """load -> save_json -> parse_json produces equal HullLineDefinition."""
        from digitalmodel.hydrodynamics.hull_library.line_generator.line_parser import (
            LineParser,
        )

        parser = LineParser()
        defn_a = parser.parse_json_dict(box_barge_json)

        out_path = tmp_path / "round_trip.json"
        defn_a.save_json(out_path)

        defn_b = parser.parse_json(out_path)
        assert defn_b.name == defn_a.name
        assert defn_b.draft == defn_a.draft


class TestLineParserToHullProfile:
    """Tests for converting HullLineDefinition to HullProfile."""

    def test_to_hull_profile_type(self, box_barge_json):
        """to_hull_profile returns a HullProfile instance."""
        from digitalmodel.hydrodynamics.hull_library.line_generator.line_parser import (
            LineParser,
        )
        from digitalmodel.hydrodynamics.hull_library.profile_schema import HullProfile

        parser = LineParser()
        defn = parser.parse_json_dict(box_barge_json)
        profile = defn.to_hull_profile()
        assert isinstance(profile, HullProfile)

    def test_to_hull_profile_dimensions(self, box_barge_json):
        """to_hull_profile preserves principal dimensions."""
        from digitalmodel.hydrodynamics.hull_library.line_generator.line_parser import (
            LineParser,
        )

        parser = LineParser()
        defn = parser.parse_json_dict(box_barge_json)
        profile = defn.to_hull_profile()
        assert profile.length_bp == defn.length_bp
        assert profile.beam == defn.beam
        assert profile.draft == defn.draft
        assert profile.depth == defn.depth

    def test_to_hull_profile_station_count(self, box_barge_json):
        """to_hull_profile produces correct station count."""
        from digitalmodel.hydrodynamics.hull_library.line_generator.line_parser import (
            LineParser,
        )

        parser = LineParser()
        defn = parser.parse_json_dict(box_barge_json)
        profile = defn.to_hull_profile()
        assert len(profile.stations) == len(defn.stations)
