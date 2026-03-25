"""
ABOUTME: RAO database for storing and querying Response Amplitude Operator
results indexed by hull variation parameters — Phase 3 of WRK-043.

Provides:
- RAODatabaseEntry: a single record pairing a variation ID, hull parameter
  dict, and RAOData with optional metadata.
- RAODatabase: an in-memory store with query-by-parameter-range, save/load
  via Parquet (pandas + pyarrow backend) for disk persistence.

Design notes
------------
RAOData contains three numpy arrays (frequencies, amplitudes, phases) plus
scalar fields.  Parquet is column-oriented so we serialise each numpy array
to bytes (via numpy.save) stored in a single binary column per array.  Hull
parameters and metadata are stored as JSON strings.  This avoids any
dependency on polars while remaining fully round-trippable.
"""

from __future__ import annotations

import io
import json
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Iterator

import numpy as np


# ---------------------------------------------------------------------------
# RAODatabaseEntry
# ---------------------------------------------------------------------------


@dataclass
class RAODatabaseEntry:
    """One record in the RAO database.

    Attributes
    ----------
    variation_id:
        Unique string key identifying the hull variation (matches the ID
        produced by HullParametricSpace.generate_profiles).
    hull_params:
        Flat dict of scaling / parametric values used to generate this
        hull variant (e.g. ``{"length_scale": 1.5, "beam_scale": 0.9}``).
    rao_data:
        The ``RAOData`` object containing frequencies, directions,
        amplitudes, and phases.
    metadata:
        Arbitrary extra key-value data (solver version, run timestamp,
        mesh resolution, etc.).
    """

    variation_id: str
    hull_params: dict[str, float]
    rao_data: Any  # RAOData — typed as Any to avoid heavy import at module level
    metadata: dict[str, Any] = field(default_factory=dict)


# ---------------------------------------------------------------------------
# RAODatabase
# ---------------------------------------------------------------------------


class RAODatabase:
    """In-memory store for RAO database entries with Parquet persistence.

    Usage
    -----
    .. code-block:: python

        db = RAODatabase()
        db.store(RAODatabaseEntry(variation_id="v001", hull_params={...},
                                  rao_data=rao))
        entry = db.get_by_id("v001")

        # Query by parameter range
        results = list(db.query({"length_scale": (1.0, 2.0)}))

        # Persist
        db.save_to_disk(Path("output/rao_db.parquet"))
        db2 = RAODatabase()
        db2.load_from_disk(Path("output/rao_db.parquet"))
    """

    def __init__(self) -> None:
        self._entries: dict[str, RAODatabaseEntry] = {}

    # ------------------------------------------------------------------
    # Core storage
    # ------------------------------------------------------------------

    def store(self, entry: RAODatabaseEntry) -> None:
        """Store an entry, overwriting any existing record with the same ID."""
        self._entries[entry.variation_id] = entry

    def get_by_id(self, variation_id: str) -> RAODatabaseEntry:
        """Retrieve an entry by its variation ID.

        Raises
        ------
        KeyError
            If *variation_id* is not in the database.
        """
        if variation_id not in self._entries:
            raise KeyError(
                f"RAO entry '{variation_id}' not found in database. "
                f"Available IDs: {sorted(self._entries.keys())[:10]}"
            )
        return self._entries[variation_id]

    # ------------------------------------------------------------------
    # Query
    # ------------------------------------------------------------------

    def query(
        self, ranges: dict[str, tuple[float, float]]
    ) -> Iterator[RAODatabaseEntry]:
        """Yield entries whose hull_params match the given parameter ranges.

        Parameters
        ----------
        ranges:
            A dict mapping parameter name to ``(min_value, max_value)``
            inclusive bounds.  An empty dict yields all entries.

        Yields
        ------
        RAODatabaseEntry
            All entries where every constrained parameter falls within its
            specified range.
        """
        for entry in self._entries.values():
            if _matches_ranges(entry.hull_params, ranges):
                yield entry

    # ------------------------------------------------------------------
    # Metadata inspection
    # ------------------------------------------------------------------

    def list_parameters(self) -> dict[str, list[float]]:
        """Return all unique values for each hull parameter across the database.

        Returns
        -------
        dict[str, list[float]]
            Mapping of parameter name → sorted list of unique values.
        """
        all_params: dict[str, set] = {}
        for entry in self._entries.values():
            for key, val in entry.hull_params.items():
                if key not in all_params:
                    all_params[key] = set()
                all_params[key].add(val)
        return {k: sorted(v) for k, v in all_params.items()}

    # ------------------------------------------------------------------
    # Disk persistence
    # ------------------------------------------------------------------

    def save_to_disk(self, path: str | Path) -> None:
        """Serialise the database to a Parquet file at *path*.

        Creates parent directories if they do not exist.  Uses pandas +
        pyarrow as the Parquet backend.

        The file schema is::

            variation_id   (string)
            hull_params    (string, JSON)
            metadata       (string, JSON)
            vessel_name    (string)
            frequencies    (bytes, numpy.save'd float64 array)
            directions     (bytes, numpy.save'd float64 array)
            amplitudes     (bytes, numpy.save'd float64 3-D array)
            phases         (bytes, numpy.save'd float64 3-D array)
        """
        import pandas as pd

        path = Path(path)
        path.parent.mkdir(parents=True, exist_ok=True)

        rows = []
        for entry in self._entries.values():
            rows.append(
                {
                    "variation_id": entry.variation_id,
                    "hull_params": json.dumps(entry.hull_params),
                    "metadata": json.dumps(entry.metadata),
                    "vessel_name": entry.rao_data.vessel_name,
                    "frequencies": _array_to_bytes(
                        entry.rao_data.frequencies
                    ),
                    "directions": _array_to_bytes(
                        entry.rao_data.directions
                    ),
                    "amplitudes": _array_to_bytes(
                        entry.rao_data.amplitudes
                    ),
                    "phases": _array_to_bytes(entry.rao_data.phases),
                }
            )

        df = pd.DataFrame(rows)
        df.to_parquet(path, index=False)

    def load_from_disk(self, path: str | Path) -> None:
        """Load entries from a Parquet file, merging with existing entries.

        Raises
        ------
        FileNotFoundError
            If *path* does not exist.
        """
        import pandas as pd
        from digitalmodel.hydrodynamics.models import RAOData

        path = Path(path)
        if not path.exists():
            raise FileNotFoundError(
                f"RAO database file not found: {path}"
            )

        df = pd.read_parquet(path)

        for _, row in df.iterrows():
            frequencies = _bytes_to_array(row["frequencies"])
            directions = _bytes_to_array(row["directions"])
            amplitudes = _bytes_to_array(row["amplitudes"])
            phases = _bytes_to_array(row["phases"])

            rao_data = RAOData(
                frequencies=frequencies,
                directions=directions,
                amplitudes=amplitudes,
                phases=phases,
                vessel_name=row["vessel_name"],
            )
            hull_params = json.loads(row["hull_params"])
            metadata = json.loads(row["metadata"])

            entry = RAODatabaseEntry(
                variation_id=row["variation_id"],
                hull_params=hull_params,
                rao_data=rao_data,
                metadata=metadata,
            )
            self.store(entry)


# ---------------------------------------------------------------------------
# Private helpers
# ---------------------------------------------------------------------------


def _matches_ranges(
    params: dict[str, float],
    ranges: dict[str, tuple[float, float]],
) -> bool:
    """Return True if all range constraints are satisfied by *params*."""
    for key, (lo, hi) in ranges.items():
        val = params.get(key)
        if val is None:
            return False
        if val < lo or val > hi:
            return False
    return True


def _array_to_bytes(arr: np.ndarray) -> bytes:
    """Serialise a numpy array to bytes using numpy.save format."""
    buf = io.BytesIO()
    np.save(buf, arr)
    return buf.getvalue()


def _bytes_to_array(data: bytes) -> np.ndarray:
    """Deserialise a numpy array from bytes produced by _array_to_bytes."""
    buf = io.BytesIO(data)
    return np.load(buf)


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

__all__ = [
    "RAODatabaseEntry",
    "RAODatabase",
]
