"""Tests for CRS (Coordinate Reference System) handling."""

from __future__ import annotations

import pytest

from digitalmodel.modules.gis.core.crs import CRSDefinition, CRSType, detect_utm_zone


# ------------------------------------------------------------------
# CRSType enum
# ------------------------------------------------------------------


class TestCRSType:
    """Verify the CRSType enum contains all expected members."""

    @pytest.mark.parametrize(
        "member_name, member_value",
        [
            ("WGS84", "wgs84"),
            ("UTM", "utm"),
            ("LOCAL", "local"),
            ("PROJECTED", "projected"),
            ("GEOGRAPHIC", "geographic"),
        ],
    )
    def test_crstype_member_exists_with_correct_value(
        self, member_name: str, member_value: str
    ) -> None:
        member = CRSType[member_name]
        assert member.value == member_value


# ------------------------------------------------------------------
# CRSDefinition factory methods
# ------------------------------------------------------------------


class TestCRSDefinitionWGS84:
    """Tests for the CRSDefinition.wgs84 factory."""

    def test_wgs84_returns_epsg_4326(self) -> None:
        crs = CRSDefinition.wgs84()
        assert crs.epsg_code == 4326

    def test_wgs84_returns_wgs84_type(self) -> None:
        crs = CRSDefinition.wgs84()
        assert crs.crs_type is CRSType.WGS84

    def test_wgs84_has_proj_string(self) -> None:
        crs = CRSDefinition.wgs84()
        assert crs.proj_string is not None
        assert "WGS84" in crs.proj_string


class TestCRSDefinitionUTMFromZone:
    """Tests for the CRSDefinition.utm_from_zone factory."""

    def test_utm_from_zone_northern_returns_correct_epsg(self) -> None:
        crs = CRSDefinition.utm_from_zone(16, "N")
        assert crs.epsg_code == 32616

    def test_utm_from_zone_northern_has_utm_type(self) -> None:
        crs = CRSDefinition.utm_from_zone(16, "N")
        assert crs.crs_type is CRSType.UTM

    def test_utm_from_zone_northern_stores_zone_and_hemisphere(self) -> None:
        crs = CRSDefinition.utm_from_zone(16, "N")
        assert crs.zone == 16
        assert crs.hemisphere == "N"

    def test_utm_from_zone_southern_returns_correct_epsg(self) -> None:
        crs = CRSDefinition.utm_from_zone(20, "S")
        assert crs.epsg_code == 32720

    def test_utm_from_zone_southern_stores_zone_and_hemisphere(self) -> None:
        crs = CRSDefinition.utm_from_zone(20, "S")
        assert crs.zone == 20
        assert crs.hemisphere == "S"


class TestCRSDefinitionUTMFromLongitude:
    """Tests for the CRSDefinition.utm_from_longitude factory."""

    def test_utm_from_longitude_gulf_of_mexico_detects_zone_16_north(self) -> None:
        # Longitude -89.0 falls in UTM zone 16 (covers -90 to -84)
        crs = CRSDefinition.utm_from_longitude(-89.0, 28.1)
        assert crs.zone == 16
        assert crs.hemisphere == "N"
        assert crs.epsg_code == 32616
        assert crs.crs_type is CRSType.UTM


class TestCRSDefinitionFromEPSG:
    """Tests for the CRSDefinition.from_epsg factory."""

    def test_from_epsg_utm_north_creates_correct_definition(self) -> None:
        crs = CRSDefinition.from_epsg(32616)
        assert crs.epsg_code == 32616
        assert crs.crs_type is CRSType.UTM
        assert crs.zone == 16
        assert crs.hemisphere == "N"

    def test_from_epsg_utm_south_creates_correct_definition(self) -> None:
        crs = CRSDefinition.from_epsg(32720)
        assert crs.epsg_code == 32720
        assert crs.crs_type is CRSType.UTM
        assert crs.zone == 20
        assert crs.hemisphere == "S"

    def test_from_epsg_wgs84_creates_correct_definition(self) -> None:
        crs = CRSDefinition.from_epsg(4326)
        assert crs.epsg_code == 4326
        assert crs.crs_type is CRSType.WGS84


# ------------------------------------------------------------------
# detect_utm_zone standalone function
# ------------------------------------------------------------------


class TestDetectUTMZone:
    """Tests for the detect_utm_zone helper function."""

    def test_detect_utm_zone_gulf_of_mexico_returns_zone_16_north(self) -> None:
        # Longitude -89.0 falls in UTM zone 16 (covers -90 to -84)
        zone, hemisphere = detect_utm_zone(-89.0, 28.1)
        assert zone == 16
        assert hemisphere == "N"

    def test_detect_utm_zone_south_atlantic_returns_correct_zone_south(self) -> None:
        zone, hemisphere = detect_utm_zone(10.0, -30.0)
        assert zone == 32
        assert hemisphere == "S"

    def test_detect_utm_zone_longitude_zero_returns_zone_31(self) -> None:
        zone, hemisphere = detect_utm_zone(0.0, 45.0)
        assert zone == 31
        assert hemisphere == "N"

    def test_detect_utm_zone_longitude_180_returns_zone_60(self) -> None:
        zone, hemisphere = detect_utm_zone(180.0, 45.0)
        assert zone == 60
        assert hemisphere == "N"

    def test_detect_utm_zone_equator_returns_north_hemisphere(self) -> None:
        zone, hemisphere = detect_utm_zone(15.0, 0.0)
        assert hemisphere == "N"
        assert zone == 33

    def test_detect_utm_zone_invalid_longitude_raises_value_error(self) -> None:
        with pytest.raises(ValueError, match="Longitude must be between"):
            detect_utm_zone(200.0, 0.0)

    def test_detect_utm_zone_invalid_latitude_raises_value_error(self) -> None:
        with pytest.raises(ValueError, match="Latitude must be between"):
            detect_utm_zone(0.0, 100.0)
