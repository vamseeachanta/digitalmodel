"""Tests for hull profile schema -- line profile definitions for hull geometry."""

import pytest
import yaml
import tempfile
from pathlib import Path


class TestHullType:
    """Tests for HullType enum."""

    def test_hull_type_values(self):
        from digitalmodel.hydrodynamics.hull_library.profile_schema import HullType

        assert HullType.TANKER.value == "tanker"
        assert HullType.SEMI_PONTOON.value == "semi_pontoon"
        assert HullType.BARGE.value == "barge"
        assert HullType.SHIP.value == "ship"
        assert HullType.CUSTOM.value == "custom"


class TestHullStation:
    """Tests for HullStation model."""

    def test_valid_station(self):
        from digitalmodel.hydrodynamics.hull_library.profile_schema import (
            HullStation,
        )

        station = HullStation(
            x_position=10.0,
            waterline_offsets=[(0.0, 0.0), (5.0, 8.2), (10.0, 14.5)],
        )
        assert station.x_position == 10.0
        assert len(station.waterline_offsets) == 3

    def test_station_negative_x(self):
        """Stations can have negative x (aft of AP)."""
        from digitalmodel.hydrodynamics.hull_library.profile_schema import (
            HullStation,
        )

        station = HullStation(x_position=-2.0, waterline_offsets=[(0.0, 0.0)])
        assert station.x_position == -2.0

    def test_station_empty_offsets_rejected(self):
        """Must have at least one waterline offset."""
        from digitalmodel.hydrodynamics.hull_library.profile_schema import (
            HullStation,
        )

        with pytest.raises(ValueError):
            HullStation(x_position=0.0, waterline_offsets=[])

    def test_station_offsets_are_z_y_pairs(self):
        from digitalmodel.hydrodynamics.hull_library.profile_schema import (
            HullStation,
        )

        station = HullStation(
            x_position=0.0,
            waterline_offsets=[(0.0, 0.0), (5.0, 12.4)],
        )
        # First element of each pair is z (draft), second is y (half-breadth)
        assert station.waterline_offsets[1] == (5.0, 12.4)


class TestHullProfile:
    """Tests for HullProfile model."""

    def _make_box_profile(self):
        from digitalmodel.hydrodynamics.hull_library.profile_schema import (
            HullProfile,
            HullStation,
            HullType,
        )

        stations = [
            HullStation(
                x_position=0.0,
                waterline_offsets=[(0.0, 5.0), (4.0, 5.0)],
            ),
            HullStation(
                x_position=50.0,
                waterline_offsets=[(0.0, 5.0), (4.0, 5.0)],
            ),
        ]
        return HullProfile(
            name="test_box",
            hull_type=HullType.BARGE,
            stations=stations,
            length_bp=50.0,
            beam=10.0,
            draft=4.0,
            depth=6.0,
            source="test",
        )

    def test_valid_profile(self):
        profile = self._make_box_profile()
        assert profile.name == "test_box"
        assert profile.length_bp == 50.0

    def test_profile_requires_positive_length(self):
        from digitalmodel.hydrodynamics.hull_library.profile_schema import (
            HullProfile,
            HullStation,
            HullType,
        )

        with pytest.raises(ValueError):
            HullProfile(
                name="bad",
                hull_type=HullType.BARGE,
                stations=[
                    HullStation(
                        x_position=0.0,
                        waterline_offsets=[(0.0, 5.0)],
                    )
                ],
                length_bp=-10.0,
                beam=10.0,
                draft=4.0,
                depth=6.0,
                source="test",
            )

    def test_profile_requires_positive_beam(self):
        from digitalmodel.hydrodynamics.hull_library.profile_schema import (
            HullProfile,
            HullStation,
            HullType,
        )

        with pytest.raises(ValueError):
            HullProfile(
                name="bad",
                hull_type=HullType.BARGE,
                stations=[
                    HullStation(
                        x_position=0.0,
                        waterline_offsets=[(0.0, 5.0)],
                    )
                ],
                length_bp=50.0,
                beam=0.0,
                draft=4.0,
                depth=6.0,
                source="test",
            )

    def test_profile_requires_at_least_two_stations(self):
        from digitalmodel.hydrodynamics.hull_library.profile_schema import (
            HullProfile,
            HullStation,
            HullType,
        )

        with pytest.raises(ValueError):
            HullProfile(
                name="bad",
                hull_type=HullType.BARGE,
                stations=[
                    HullStation(
                        x_position=0.0,
                        waterline_offsets=[(0.0, 5.0)],
                    )
                ],
                length_bp=50.0,
                beam=10.0,
                draft=4.0,
                depth=6.0,
                source="test",
            )

    def test_optional_fields_default_none(self):
        profile = self._make_box_profile()
        assert profile.deck_profile is None
        assert profile.keel_profile is None
        assert profile.block_coefficient is None
        assert profile.displacement is None

    def test_block_coefficient_range(self):
        from digitalmodel.hydrodynamics.hull_library.profile_schema import (
            HullProfile,
            HullStation,
            HullType,
        )

        with pytest.raises(ValueError):
            HullProfile(
                name="bad",
                hull_type=HullType.TANKER,
                stations=[
                    HullStation(
                        x_position=0.0,
                        waterline_offsets=[(0.0, 5.0), (4.0, 5.0)],
                    ),
                    HullStation(
                        x_position=50.0,
                        waterline_offsets=[(0.0, 5.0), (4.0, 5.0)],
                    ),
                ],
                length_bp=50.0,
                beam=10.0,
                draft=4.0,
                depth=6.0,
                source="test",
                block_coefficient=1.5,
            )


class TestHullProfileYAML:
    """Tests for YAML serialization/deserialization."""

    def _make_box_profile(self):
        from digitalmodel.hydrodynamics.hull_library.profile_schema import (
            HullProfile,
            HullStation,
            HullType,
        )

        stations = [
            HullStation(
                x_position=0.0,
                waterline_offsets=[(0.0, 5.0), (4.0, 5.0)],
            ),
            HullStation(
                x_position=50.0,
                waterline_offsets=[(0.0, 5.0), (4.0, 5.0)],
            ),
        ]
        return HullProfile(
            name="test_box",
            hull_type=HullType.BARGE,
            stations=stations,
            length_bp=50.0,
            beam=10.0,
            draft=4.0,
            depth=6.0,
            source="test",
        )

    def test_to_yaml_dict(self):
        """Profile can be serialized to a YAML-friendly dict."""
        profile = self._make_box_profile()
        d = profile.to_yaml_dict()
        assert d["name"] == "test_box"
        assert d["hull_type"] == "barge"
        assert len(d["stations"]) == 2

    def test_round_trip_yaml(self):
        """Profile survives YAML round-trip."""
        from digitalmodel.hydrodynamics.hull_library.profile_schema import (
            HullProfile,
        )

        profile = self._make_box_profile()
        yaml_str = yaml.dump(profile.to_yaml_dict(), default_flow_style=False)
        data = yaml.safe_load(yaml_str)
        loaded = HullProfile.from_yaml_dict(data)
        assert loaded.name == profile.name
        assert loaded.length_bp == profile.length_bp
        assert len(loaded.stations) == len(profile.stations)

    def test_save_and_load_file(self):
        """Profile saves to file and loads back."""
        from digitalmodel.hydrodynamics.hull_library.profile_schema import (
            HullProfile,
        )

        profile = self._make_box_profile()
        with tempfile.TemporaryDirectory() as tmpdir:
            path = Path(tmpdir) / "test.yaml"
            profile.save_yaml(path)
            loaded = HullProfile.load_yaml(path)
            assert loaded.name == profile.name
            assert loaded.beam == profile.beam

    def test_load_nonexistent_file_raises(self):
        from digitalmodel.hydrodynamics.hull_library.profile_schema import (
            HullProfile,
        )

        with pytest.raises(FileNotFoundError):
            HullProfile.load_yaml(Path("/nonexistent/path.yaml"))
