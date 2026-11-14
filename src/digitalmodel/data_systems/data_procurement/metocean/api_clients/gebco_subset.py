"""Offline, digest-bound GEBCO_2025 ESRI ASCII subsets.

Only geographic, corner-registered grids are admitted. Elevations are metres,
positive up; rows run north to south. This loader does not qualify survey data
or establish fitness for navigation, safety at sea or a route calculation.
"""

from dataclasses import dataclass
import hashlib
import json
from math import isfinite
from pathlib import Path

import numpy as np

MAX_FILE_BYTES = 8 * 1024 * 1024
MAX_CELLS = 250_000
_FIELDS = {"ncols", "nrows", "xllcorner", "yllcorner", "cellsize", "nodata_value"}


@dataclass(frozen=True)
class GEBCOSubset:
    """Cell-centre axes in degrees and north-first elevation/mask arrays."""

    longitude: np.ndarray
    latitude: np.ndarray
    elevation_m: np.ndarray
    valid_mask: np.ndarray
    provenance: dict

    @property
    def depth_m(self) -> np.ndarray:
        """Positive underwater depth; land/zero stay zero, missing stays NaN."""
        return np.where(self.valid_mask, np.maximum(-self.elevation_m, 0), np.nan)


def _read_bounded(path: str | Path) -> bytes:
    with Path(path).open("rb") as stream:
        data = stream.read(MAX_FILE_BYTES + 1)
    if not data or len(data) > MAX_FILE_BYTES:
        raise ValueError("GEBCO input size must be between 1 byte and 8 MiB")
    return data


def load_gebco_2025_subset(path: str | Path, provenance_path: str | Path) -> GEBCOSubset:
    """Validate local subset bytes and provenance, then return coordinates/data.

    Both files are bounded to 8 MiB and the grid to 250,000 cells before a cell
    array is allocated. Provenance must declare edition GEBCO_2025, the exact
    subset SHA-256/byte count and its numeric subset_header. It records source
    lineage; a caller-supplied provenance record does not authenticate a source.
    Malformed input raises ValueError. No network or solver is used.
    """
    raw = _read_bounded(path)
    provenance = json.loads(_read_bounded(provenance_path))
    if not isinstance(provenance, dict) or provenance.get("edition") != "GEBCO_2025":
        raise ValueError("GEBCO provenance edition must be GEBCO_2025")
    if (provenance.get("subset_sha256") != hashlib.sha256(raw).hexdigest()
            or provenance.get("subset_byte_count") != len(raw)):
        raise ValueError("GEBCO subset digest or byte count mismatch")

    lines = raw.decode("ascii").splitlines()
    header = {}
    for line in lines[:6]:
        parts = line.split()
        if len(parts) != 2:
            raise ValueError("Malformed ESRI ASCII header")
        key, value = parts[0].lower(), parts[1]
        if key not in _FIELDS or key in header:
            raise ValueError("Only unique corner-registered ESRI ASCII header fields are supported")
        try:
            header[key] = int(value) if key in {"ncols", "nrows"} else float(value)
        except ValueError as error:
            raise ValueError("Invalid ESRI ASCII header value") from error
    if set(header) != _FIELDS or not all(
        isfinite(header[k]) for k in _FIELDS - {"ncols", "nrows"}
    ):
        raise ValueError("Missing or nonfinite ESRI ASCII header fields")
    ncols, nrows = header["ncols"], header["nrows"]
    if ncols <= 0 or nrows <= 0:
        raise ValueError("GEBCO dimensions must be positive integers")
    if ncols * nrows > MAX_CELLS:
        raise ValueError("GEBCO subset exceeds 250000 cells")
    step = header["cellsize"]
    xll, yll = header["xllcorner"], header["yllcorner"]
    # The source's rounded cellsize gives a 7.2e-9 degree polar edge excess.
    if (step <= 0 or not -180 <= xll < 180 or not -90 <= yll < 90
            or xll + ncols * step > 180 + 1e-7
            or yll + nrows * step > 90 + 1e-7):
        raise ValueError("Invalid or wrapped geographic extent")
    if provenance.get("subset_header") != header:
        raise ValueError("GEBCO provenance subset header mismatch")
    rows = [line.split() for line in lines[6:] if line.strip()]
    if len(rows) != nrows or any(len(row) != ncols for row in rows):
        raise ValueError("GEBCO cell rows do not match the declared shape")
    elevation = np.array(rows, dtype=float)
    if not np.isfinite(elevation).all():
        raise ValueError("GEBCO elevation values must be finite or the declared NODATA sentinel")
    valid = elevation != header["nodata_value"]
    elevation[~valid] = np.nan
    longitude = xll + (np.arange(ncols) + 0.5) * step
    latitude = yll + (nrows - np.arange(nrows) - 0.5) * step
    return GEBCOSubset(longitude, latitude, elevation, valid, provenance)
