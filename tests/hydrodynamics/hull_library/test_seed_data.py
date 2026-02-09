"""Tests for seed hull profile data files."""

import pytest
from pathlib import Path

DATA_DIR = Path(__file__).resolve().parents[3] / "data" / "hull_library" / "profiles"


class TestSeedProfiles:
    """Validate all seed YAML profiles load and are consistent."""

    @pytest.mark.parametrize(
        "filename",
        [
            "unit_box.yaml",
            "generic_barge.yaml",
            "generic_tanker.yaml",
        ],
    )
    def test_profile_loads(self, filename):
        from digitalmodel.hydrodynamics.hull_library.profile_schema import (
            HullProfile,
        )

        path = DATA_DIR / filename
        assert path.exists(), f"Seed profile not found: {path}"
        profile = HullProfile.load_yaml(path)
        assert profile.name
        assert profile.length_bp > 0
        assert len(profile.stations) >= 2

    def test_unit_box_is_rectangular(self):
        from digitalmodel.hydrodynamics.hull_library.profile_schema import (
            HullProfile,
        )

        profile = HullProfile.load_yaml(DATA_DIR / "unit_box.yaml")
        assert profile.block_coefficient == 1.0
        # All stations should have same half-breadth
        for station in profile.stations:
            for z, y in station.waterline_offsets:
                assert y == 10.0

    def test_tanker_has_varying_sections(self):
        from digitalmodel.hydrodynamics.hull_library.profile_schema import (
            HullProfile,
        )

        profile = HullProfile.load_yaml(DATA_DIR / "generic_tanker.yaml")
        # Midship should be wider than bow
        mid = [s for s in profile.stations if 80 < s.x_position < 150]
        bow = [s for s in profile.stations if s.x_position > 200]
        assert mid and bow
        mid_max_y = max(y for s in mid for _, y in s.waterline_offsets)
        bow_max_y = max(y for s in bow for _, y in s.waterline_offsets)
        assert mid_max_y > bow_max_y
