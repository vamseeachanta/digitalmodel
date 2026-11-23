"""CRS (Coordinate Reference System) handling for the GIS module.

Provides an enumeration of common CRS types, a ``CRSDefinition`` dataclass
for representing coordinate reference systems, and a standalone helper for
detecting UTM zones from geographic coordinates.

When *pyproj* is available the EPSG definitions are validated against its
database; otherwise the module operates in a lightweight mode with no
external dependencies beyond the standard library.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from enum import Enum

# ------------------------------------------------------------------
# Optional pyproj import
# ------------------------------------------------------------------

try:
    from pyproj import CRS as PyProjCRS

    HAS_PYPROJ = True
except ImportError:  # pragma: no cover
    HAS_PYPROJ = False


# ------------------------------------------------------------------
# CRS type enumeration
# ------------------------------------------------------------------


class CRSType(Enum):
    """Supported coordinate reference system families."""

    WGS84 = "wgs84"
    UTM = "utm"
    LOCAL = "local"
    PROJECTED = "projected"
    GEOGRAPHIC = "geographic"


# ------------------------------------------------------------------
# UTM zone detection
# ------------------------------------------------------------------


def detect_utm_zone(longitude: float, latitude: float) -> tuple[int, str]:
    """Determine the UTM zone number and hemisphere from geographic coordinates.

    Parameters
    ----------
    longitude:
        Longitude in decimal degrees (``-180`` to ``180``).
    latitude:
        Latitude in decimal degrees (``-90`` to ``90``).

    Returns
    -------
    tuple[int, str]
        A ``(zone_number, hemisphere_letter)`` pair where
        *hemisphere_letter* is ``'N'`` for the northern hemisphere or
        ``'S'`` for the southern hemisphere.

    Raises
    ------
    ValueError
        If *longitude* or *latitude* are outside their valid ranges.
    """
    if not (-180.0 <= longitude <= 180.0):
        raise ValueError(
            f"Longitude must be between -180 and 180, got {longitude}"
        )
    if not (-90.0 <= latitude <= 90.0):
        raise ValueError(
            f"Latitude must be between -90 and 90, got {latitude}"
        )

    zone_number = int(math.floor((longitude + 180.0) / 6.0)) + 1
    # Clamp to the valid 1..60 range (longitude == 180 yields 61)
    zone_number = min(zone_number, 60)

    hemisphere = "N" if latitude >= 0.0 else "S"
    return zone_number, hemisphere


# ------------------------------------------------------------------
# CRS definition dataclass
# ------------------------------------------------------------------


@dataclass
class CRSDefinition:
    """Immutable description of a coordinate reference system.

    Attributes
    ----------
    crs_type:
        The family of the CRS (WGS84, UTM, etc.).
    epsg_code:
        EPSG registry code, if known.
    proj_string:
        PROJ pipeline string for advanced use.
    zone:
        UTM zone number (1--60) when *crs_type* is ``CRSType.UTM``.
    hemisphere:
        ``'N'`` or ``'S'`` when *crs_type* is ``CRSType.UTM``.
    """

    crs_type: CRSType
    epsg_code: int | None = None
    proj_string: str | None = None
    zone: int | None = None
    hemisphere: str | None = None

    # ------------------------------------------------------------------
    # Validation
    # ------------------------------------------------------------------

    def __post_init__(self) -> None:
        if self.hemisphere is not None and self.hemisphere not in ("N", "S"):
            raise ValueError(
                f"Hemisphere must be 'N' or 'S', got {self.hemisphere!r}"
            )
        if self.zone is not None and not (1 <= self.zone <= 60):
            raise ValueError(
                f"UTM zone must be between 1 and 60, got {self.zone}"
            )

    # ------------------------------------------------------------------
    # Factory class methods
    # ------------------------------------------------------------------

    @classmethod
    def wgs84(cls) -> CRSDefinition:
        """Return a WGS 84 geographic CRS definition (EPSG:4326)."""
        return cls(
            crs_type=CRSType.WGS84,
            epsg_code=4326,
            proj_string="+proj=longlat +datum=WGS84 +no_defs",
        )

    @classmethod
    def utm_from_zone(
        cls, zone: int, hemisphere: str = "N"
    ) -> CRSDefinition:
        """Build a UTM CRS definition from an explicit zone and hemisphere.

        Parameters
        ----------
        zone:
            UTM zone number (1--60).
        hemisphere:
            ``'N'`` (default) or ``'S'``.

        Returns
        -------
        CRSDefinition
            A fully populated UTM definition with the computed EPSG code.
        """
        if hemisphere not in ("N", "S"):
            raise ValueError(
                f"Hemisphere must be 'N' or 'S', got {hemisphere!r}"
            )
        if not (1 <= zone <= 60):
            raise ValueError(
                f"UTM zone must be between 1 and 60, got {zone}"
            )

        epsg_code = (32600 + zone) if hemisphere == "N" else (32700 + zone)
        return cls(
            crs_type=CRSType.UTM,
            epsg_code=epsg_code,
            zone=zone,
            hemisphere=hemisphere,
        )

    @classmethod
    def utm_from_longitude(
        cls, longitude: float, latitude: float
    ) -> CRSDefinition:
        """Auto-detect the UTM zone from geographic coordinates.

        Parameters
        ----------
        longitude:
            Longitude in decimal degrees (``-180`` to ``180``).
        latitude:
            Latitude in decimal degrees (``-90`` to ``90``).

        Returns
        -------
        CRSDefinition
            A UTM definition whose zone and hemisphere match the supplied
            coordinates.
        """
        zone_number, hemisphere = detect_utm_zone(longitude, latitude)
        return cls.utm_from_zone(zone_number, hemisphere)

    @classmethod
    def from_epsg(cls, epsg_code: int) -> CRSDefinition:
        """Create a generic CRS definition from an EPSG code.

        When *pyproj* is available the code is validated against its
        database and the CRS type is inferred.  Without *pyproj* the
        definition is created with ``CRSType.PROJECTED`` as a sensible
        default.

        Parameters
        ----------
        epsg_code:
            A valid EPSG registry code (e.g. ``4326``, ``32615``).

        Returns
        -------
        CRSDefinition
            A definition with the appropriate type, EPSG code, and
            (where possible) zone / hemisphere metadata.
        """
        if HAS_PYPROJ:
            return cls._from_epsg_with_pyproj(epsg_code)
        return cls._from_epsg_without_pyproj(epsg_code)

    # ------------------------------------------------------------------
    # pyproj helpers
    # ------------------------------------------------------------------

    def to_pyproj(self) -> PyProjCRS:
        """Convert this definition to a ``pyproj.CRS`` object.

        Raises
        ------
        ImportError
            If *pyproj* is not installed.
        ValueError
            If neither *epsg_code* nor *proj_string* is available.
        """
        if not HAS_PYPROJ:
            raise ImportError(
                "pyproj is required for CRS conversion but is not installed"
            )
        if self.epsg_code is not None:
            return PyProjCRS.from_epsg(self.epsg_code)
        if self.proj_string is not None:
            return PyProjCRS.from_proj4(self.proj_string)
        raise ValueError(
            "Cannot create pyproj CRS: neither epsg_code nor proj_string is set"
        )

    # ------------------------------------------------------------------
    # Internal helpers
    # ------------------------------------------------------------------

    @classmethod
    def _from_epsg_with_pyproj(cls, epsg_code: int) -> CRSDefinition:
        """Build a definition using pyproj for validation and type inference."""
        pyproj_crs = PyProjCRS.from_epsg(epsg_code)

        if pyproj_crs.is_geographic:
            crs_type = CRSType.GEOGRAPHIC
        elif pyproj_crs.is_projected:
            crs_type = CRSType.PROJECTED
        else:
            crs_type = CRSType.PROJECTED

        zone: int | None = None
        hemisphere: str | None = None

        # Detect UTM specifics
        utm_info = pyproj_crs.utm_zone
        if utm_info is not None:
            crs_type = CRSType.UTM
            # pyproj returns e.g. "15N" or "32S"
            zone = int(utm_info[:-1])
            hemisphere = utm_info[-1]

        if epsg_code == 4326:
            crs_type = CRSType.WGS84

        return cls(
            crs_type=crs_type,
            epsg_code=epsg_code,
            zone=zone,
            hemisphere=hemisphere,
        )

    @classmethod
    def _from_epsg_without_pyproj(cls, epsg_code: int) -> CRSDefinition:
        """Build a definition without pyproj using well-known EPSG ranges."""
        # WGS 84
        if epsg_code == 4326:
            return cls.wgs84()

        # UTM North: EPSG 32601 .. 32660
        if 32601 <= epsg_code <= 32660:
            zone = epsg_code - 32600
            return cls(
                crs_type=CRSType.UTM,
                epsg_code=epsg_code,
                zone=zone,
                hemisphere="N",
            )

        # UTM South: EPSG 32701 .. 32760
        if 32701 <= epsg_code <= 32760:
            zone = epsg_code - 32700
            return cls(
                crs_type=CRSType.UTM,
                epsg_code=epsg_code,
                zone=zone,
                hemisphere="S",
            )

        # Fallback to a generic projected CRS
        return cls(
            crs_type=CRSType.PROJECTED,
            epsg_code=epsg_code,
        )
