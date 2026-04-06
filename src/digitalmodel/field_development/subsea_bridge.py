# ABOUTME: SubseaIQ-to-field-development bridge — loads GoM JSON and provides queryable catalog.
# ABOUTME: Issue #1861 — SubseaIQ data bridge for GoM field analogues (concept selection).
"""
digitalmodel.field_development.subsea_bridge
============================================

Ingests the SubseaIQ overnight research JSON
(data/field-development/subseaiq-scan-latest.json) and maps the 10 GoM
reference fields into the field_development module's internal data
structures, providing a queryable catalog for concept-selection analogues.

Usage
-----
>>> from digitalmodel.field_development.subsea_bridge import SubseaFieldCatalog
>>> catalog = SubseaFieldCatalog.from_json("data/field-development/subseaiq-scan-latest.json")
>>> results = catalog.query(operator="Shell")
>>> analogue = catalog.find_gom_analogue(water_depth=2200, production_capacity=100000)
>>> dist = catalog.host_type_distribution()
"""

from __future__ import annotations

import json
import math
from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional


# ---------------------------------------------------------------------------
# Data classes
# ---------------------------------------------------------------------------

@dataclass
class GoMField:
    """A single GoM reference field loaded from the SubseaIQ scan.

    Attributes
    ----------
    name : str
        Field name (e.g. 'Perdido').
    operator : str
        Operating company (e.g. 'Shell', 'BP', 'LLOG').
    water_depth_m : float
        Water depth at the field location in metres.
    host : str
        Host facility type as reported in SubseaIQ
        (e.g. 'Spar', 'Semi-submersible', 'TLP', 'ETLP').
    year : int
        Year the field came on production.
    capacity_bopd : int
        Nameplate production capacity in barrels of oil per day.
    """

    name: str
    operator: str
    water_depth_m: float
    host: str
    year: int
    capacity_bopd: int

    def to_dict(self) -> dict:
        """Serialise to a plain dict (for downstream concept_selection use)."""
        return {
            "name": self.name,
            "operator": self.operator,
            "water_depth_m": self.water_depth_m,
            "host": self.host,
            "year": self.year,
            "capacity_bopd": self.capacity_bopd,
        }


@dataclass
class AnalogueMatch:
    """Result of a GoM analogue search.

    Attributes
    ----------
    field : GoMField
        The best-matching reference field.
    score : float
        Normalised similarity score 0.0–1.0 (1.0 = perfect match).
    depth_delta_m : float
        Absolute difference in water depth (metres).
    capacity_delta_bopd : int
        Absolute difference in production capacity (bopd).
    """

    field: GoMField
    score: float
    depth_delta_m: float
    capacity_delta_bopd: int


# ---------------------------------------------------------------------------
# Catalog
# ---------------------------------------------------------------------------

class SubseaFieldCatalog:
    """Queryable catalog of GoM reference fields sourced from SubseaIQ data.

    Build from JSON
    ---------------
    Use :meth:`from_json` to load from the overnight research file:

    >>> catalog = SubseaFieldCatalog.from_json(
    ...     "data/field-development/subseaiq-scan-latest.json"
    ... )

    Or construct directly with a list of :class:`GoMField` objects:

    >>> catalog = SubseaFieldCatalog(fields)
    """

    def __init__(self, fields: list[GoMField]) -> None:
        if not fields:
            raise ValueError(
                "SubseaFieldCatalog requires at least one field. "
                "Check that the JSON data file is not empty."
            )
        self._fields: list[GoMField] = list(fields)

    # ------------------------------------------------------------------
    # Constructors
    # ------------------------------------------------------------------

    @classmethod
    def from_json(cls, json_path: str | Path) -> "SubseaFieldCatalog":
        """Load GoM field catalog from the SubseaIQ research JSON file.

        Parameters
        ----------
        json_path : str or Path
            Path to ``subseaiq-scan-latest.json``.  Relative paths are
            resolved relative to the current working directory.

        Returns
        -------
        SubseaFieldCatalog

        Raises
        ------
        FileNotFoundError
            If the JSON file does not exist at the given path.
        KeyError
            If required fields are missing from the JSON structure.
        """
        path = Path(json_path)
        if not path.exists():
            raise FileNotFoundError(
                f"SubseaIQ data file not found: {path}. "
                "Ensure 'data/field-development/subseaiq-scan-latest.json' "
                "exists in the workspace."
            )
        with path.open("r", encoding="utf-8") as fh:
            raw = json.load(fh)

        gom_list = raw.get("gom_fields", [])
        if not gom_list:
            raise KeyError(
                "JSON file does not contain a 'gom_fields' array. "
                f"Keys found: {list(raw.keys())}"
            )

        fields = []
        for i, entry in enumerate(gom_list):
            try:
                fields.append(
                    GoMField(
                        name=str(entry["name"]),
                        operator=str(entry["operator"]),
                        water_depth_m=float(entry["water_depth_m"]),
                        host=str(entry["host"]),
                        year=int(entry["year"]),
                        capacity_bopd=int(entry["capacity_bopd"]),
                    )
                )
            except KeyError as exc:
                raise KeyError(
                    f"Field entry #{i} is missing required key {exc}. "
                    f"Entry: {entry}"
                ) from exc

        return cls(fields)

    @classmethod
    def from_dict(cls, data: dict) -> "SubseaFieldCatalog":
        """Build catalog from a raw dict (same schema as the JSON file).

        Useful for testing without touching the filesystem.

        Parameters
        ----------
        data : dict
            Dict with a ``'gom_fields'`` key mapping to a list of field dicts.
        """
        gom_list = data.get("gom_fields", [])
        if not gom_list:
            raise KeyError("Dict does not contain a 'gom_fields' array.")
        fields = [
            GoMField(
                name=str(e["name"]),
                operator=str(e["operator"]),
                water_depth_m=float(e["water_depth_m"]),
                host=str(e["host"]),
                year=int(e["year"]),
                capacity_bopd=int(e["capacity_bopd"]),
            )
            for e in gom_list
        ]
        return cls(fields)

    # ------------------------------------------------------------------
    # Query methods
    # ------------------------------------------------------------------

    def query(
        self,
        operator: Optional[str] = None,
        water_depth_min_m: Optional[float] = None,
        water_depth_max_m: Optional[float] = None,
        host_type: Optional[str] = None,
    ) -> list[GoMField]:
        """Filter the catalog by one or more criteria.

        All supplied criteria are ANDed together.  Comparisons are
        case-insensitive for string fields.

        Parameters
        ----------
        operator : str, optional
            Exact (case-insensitive) operator name, e.g. ``'Shell'``.
        water_depth_min_m : float, optional
            Minimum water depth (inclusive) in metres.
        water_depth_max_m : float, optional
            Maximum water depth (inclusive) in metres.
        host_type : str, optional
            Partial or full host type string, e.g. ``'Spar'``,
            ``'Semi'``, ``'TLP'``.  Matched case-insensitively as a
            substring of the stored host value.

        Returns
        -------
        list[GoMField]
            Fields satisfying all supplied criteria, sorted by
            water depth ascending.

        Raises
        ------
        ValueError
            If ``water_depth_min_m > water_depth_max_m``.

        Examples
        --------
        >>> catalog.query(operator="Shell")
        >>> catalog.query(water_depth_min_m=1500, water_depth_max_m=2500)
        >>> catalog.query(host_type="Semi")
        """
        if (
            water_depth_min_m is not None
            and water_depth_max_m is not None
            and water_depth_min_m > water_depth_max_m
        ):
            raise ValueError(
                f"water_depth_min_m ({water_depth_min_m}) must be <= "
                f"water_depth_max_m ({water_depth_max_m})."
            )

        results = []
        for f in self._fields:
            if operator is not None and f.operator.lower() != operator.lower():
                continue
            if water_depth_min_m is not None and f.water_depth_m < water_depth_min_m:
                continue
            if water_depth_max_m is not None and f.water_depth_m > water_depth_max_m:
                continue
            if host_type is not None and host_type.lower() not in f.host.lower():
                continue
            results.append(f)

        results.sort(key=lambda x: x.water_depth_m)
        return results

    def query_by_operator(self, operator: str) -> list[GoMField]:
        """Return all fields operated by ``operator`` (case-insensitive).

        Parameters
        ----------
        operator : str
            Operator name, e.g. ``'BP'``.

        Returns
        -------
        list[GoMField]
            Matching fields sorted by water depth ascending.
        """
        return self.query(operator=operator)

    def query_by_water_depth(
        self, min_m: float, max_m: float
    ) -> list[GoMField]:
        """Return fields within a water depth band.

        Parameters
        ----------
        min_m : float
            Minimum water depth in metres (inclusive).
        max_m : float
            Maximum water depth in metres (inclusive).

        Returns
        -------
        list[GoMField]
            Matching fields sorted by water depth ascending.
        """
        return self.query(water_depth_min_m=min_m, water_depth_max_m=max_m)

    def query_by_host_type(self, host_type: str) -> list[GoMField]:
        """Return fields with a given host facility type.

        The match is a case-insensitive substring search so that
        ``'Semi'`` matches ``'Semi-submersible'`` etc.

        Parameters
        ----------
        host_type : str
            Host type string or partial match, e.g. ``'TLP'``, ``'Spar'``,
            ``'Semi'``, ``'ETLP'``.

        Returns
        -------
        list[GoMField]
            Matching fields sorted by water depth ascending.
        """
        return self.query(host_type=host_type)

    # ------------------------------------------------------------------
    # GoM analogue matching
    # ------------------------------------------------------------------

    def find_gom_analogue(
        self,
        water_depth: float,
        production_capacity: float,
        depth_weight: float = 0.7,
        capacity_weight: float = 0.3,
    ) -> AnalogueMatch:
        """Find the closest GoM field analogue by water depth and production capacity.

        Computes a normalised distance score for each field using
        depth and capacity as weighted dimensions.  The reference
        ranges are derived from the catalog's own min/max values so
        the score is always 0–1.

        Parameters
        ----------
        water_depth : float
            Target water depth in metres (must be > 0).
        production_capacity : float
            Target production capacity in bopd (must be > 0).
        depth_weight : float
            Weight applied to the depth dimension (default 0.7).
            Must be in (0, 1).
        capacity_weight : float
            Weight applied to the capacity dimension (default 0.3).
            Must satisfy ``depth_weight + capacity_weight == 1.0``.

        Returns
        -------
        AnalogueMatch
            The best-matching field with similarity score and deltas.

        Raises
        ------
        ValueError
            If ``water_depth`` or ``production_capacity`` are not positive,
            or if the weights do not sum to 1.0.

        Examples
        --------
        >>> match = catalog.find_gom_analogue(
        ...     water_depth=2200, production_capacity=100000
        ... )
        >>> match.field.name
        'Whale'
        """
        if water_depth <= 0:
            raise ValueError(
                f"water_depth must be positive, got {water_depth!r}."
            )
        if production_capacity <= 0:
            raise ValueError(
                f"production_capacity must be positive, got {production_capacity!r}."
            )
        if not math.isclose(depth_weight + capacity_weight, 1.0, rel_tol=1e-6):
            raise ValueError(
                f"depth_weight ({depth_weight}) + capacity_weight ({capacity_weight}) "
                "must equal 1.0."
            )

        # Compute normalization ranges from catalog data
        depths = [f.water_depth_m for f in self._fields]
        caps = [f.capacity_bopd for f in self._fields]
        depth_range = max(depths) - min(depths) or 1.0
        cap_range = max(caps) - min(caps) or 1.0

        best_field: Optional[GoMField] = None
        best_score = float("-inf")

        for f in self._fields:
            depth_diff = abs(f.water_depth_m - water_depth) / depth_range
            cap_diff = abs(f.capacity_bopd - production_capacity) / cap_range
            # similarity = 1 - normalised weighted distance
            similarity = 1.0 - (
                depth_weight * depth_diff + capacity_weight * cap_diff
            )
            if similarity > best_score:
                best_score = similarity
                best_field = f

        assert best_field is not None  # catalog is non-empty (checked in __init__)

        return AnalogueMatch(
            field=best_field,
            score=round(max(0.0, best_score), 4),
            depth_delta_m=round(abs(best_field.water_depth_m - water_depth), 1),
            capacity_delta_bopd=abs(best_field.capacity_bopd - int(production_capacity)),
        )

    # ------------------------------------------------------------------
    # Distribution / summary
    # ------------------------------------------------------------------

    def host_type_distribution(self) -> dict[str, int]:
        """Count fields by host facility type.

        Returns
        -------
        dict[str, int]
            Mapping of host type string → count, sorted by count descending.

        Examples
        --------
        >>> catalog.host_type_distribution()
        {'Semi-submersible': 4, 'Spar': 2, 'TLP': 2, 'ETLP': 2}
        """
        counts: dict[str, int] = {}
        for f in self._fields:
            counts[f.host] = counts.get(f.host, 0) + 1
        return dict(sorted(counts.items(), key=lambda x: x[1], reverse=True))

    def operator_distribution(self) -> dict[str, int]:
        """Count fields by operator.

        Returns
        -------
        dict[str, int]
            Mapping of operator → count, sorted by count descending.
        """
        counts: dict[str, int] = {}
        for f in self._fields:
            counts[f.operator] = counts.get(f.operator, 0) + 1
        return dict(sorted(counts.items(), key=lambda x: x[1], reverse=True))

    # ------------------------------------------------------------------
    # Export
    # ------------------------------------------------------------------

    def to_dict(self) -> dict:
        """Export the full catalog as a plain dict for use by other modules.

        The returned structure mirrors the source JSON schema and is
        suitable for passing directly to concept_selection or other
        field_development helpers.

        Returns
        -------
        dict
            ``{"gom_fields": [<field dicts>], "host_type_distribution": {...},
            "operator_distribution": {...}, "field_count": N}``
        """
        return {
            "gom_fields": [f.to_dict() for f in self._fields],
            "host_type_distribution": self.host_type_distribution(),
            "operator_distribution": self.operator_distribution(),
            "field_count": len(self._fields),
        }

    def get_all_fields(self) -> list[GoMField]:
        """Return a copy of all fields in the catalog.

        Returns
        -------
        list[GoMField]
            All reference fields, in the order they were loaded.
        """
        return list(self._fields)

    # ------------------------------------------------------------------
    # Dunder helpers
    # ------------------------------------------------------------------

    def __len__(self) -> int:
        return len(self._fields)

    def __repr__(self) -> str:
        ops = set(f.operator for f in self._fields)
        return (
            f"SubseaFieldCatalog(fields={len(self._fields)}, "
            f"operators={sorted(ops)})"
        )
