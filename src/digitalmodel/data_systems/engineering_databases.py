# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Load explicit-unit engineering catalogues with mandatory row citations.

Catalogue directories are supplied by the caller; data is not bundled in the
wheel. Validation establishes structural consistency, not source authenticity,
redistribution permission or fitness for engineering use.
"""

from __future__ import annotations

import json
import math
import re
from ipaddress import IPv6Address
from pathlib import Path
from typing import Any
from urllib.parse import urlsplit

import yaml
from jsonschema import Draft202012Validator, FormatChecker
from jsonschema.exceptions import SchemaError, ValidationError

DATABASES = frozenset(
    {"line_pipe", "flexible_pipe", "structure_properties", "soil_stiffness"}
)
# Catalogue precision policy: seven significant digits in a derived value allow
# at most 5e-7 relative decimal rounding; 1e-6 leaves the same margin again.
# Derived values use stored operands; this is not a design tolerance.
# Basis: docs/plans/engineering-databases-plan-lite.md, Numerical validation basis;
# PEP 485 (2015), "Proposed Implementation", https://peps.python.org/pep-0485/.
CONSISTENCY_REL_TOL = 1e-6


class CatalogueValidationError(ValueError):
    """Indicate invalid catalogue data, units, provenance or relationships."""


def _unique_pairs(pairs: list[tuple[str, Any]]) -> dict[str, Any]:
    result: dict[str, Any] = {}
    for key, value in pairs:
        if key in result:
            raise CatalogueValidationError("duplicate mapping key")
        result[key] = value
    return result


class _UniqueSafeLoader(yaml.SafeLoader):
    """Reject duplicate keys before YAML parsing discards their evidence."""


def _yaml_mapping(loader: _UniqueSafeLoader, node: Any) -> dict[str, Any]:
    loader.flatten_mapping(node)
    return _unique_pairs(
        [(loader.construct_object(key), loader.construct_object(value))
         for key, value in node.value]
    )


_UniqueSafeLoader.add_constructor(
    yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG, _yaml_mapping
)


def _finite(value: Any) -> None:
    if isinstance(value, float) and not math.isfinite(value):
        raise CatalogueValidationError("numeric values must be finite")
    if isinstance(value, dict):
        for item in value.values():
            _finite(item)
    elif isinstance(value, list):
        for item in value:
            _finite(item)


def _engineering(database: str, row: dict[str, Any]) -> None:
    if database == "line_pipe":
        if 2 * row["wt_m"] >= row["od_m"]:
            raise CatalogueValidationError("wall must be less than half the OD")
        if row["smts_mpa"] < row["smys_mpa"]:
            raise CatalogueValidationError("SMTS must be at least SMYS")
    elif database == "flexible_pipe":
        if row["id_m"] >= row["od_m"]:
            raise CatalogueValidationError("ID must be less than OD")
    elif database == "structure_properties":
        if not math.isclose(
            row["dry_mass_kg_per_m"],
            row["area_m2"] * row["density_kg_per_m3"],
            rel_tol=CONSISTENCY_REL_TOL,
            abs_tol=0.0,
        ):
            raise CatalogueValidationError("dry mass must equal area times density")
    else:
        _soil_relationships(row)


def _soil_relationships(row: dict[str, Any]) -> None:
    displacement = row["displacement_m"]
    resistance = row["resistance_n_per_m"]
    if row["depth_top_m"] > row["depth_bottom_m"]:
        raise CatalogueValidationError("soil depth bounds must be ordered")
    if len(displacement) != len(resistance):
        raise CatalogueValidationError("soil curve arrays must have equal length")
    if displacement[0] != 0 or resistance[0] != 0:
        raise CatalogueValidationError("soil curve must start at the origin")
    if any(right <= left for left, right in zip(displacement, displacement[1:])):
        raise CatalogueValidationError("soil displacement must strictly increase")
    if not math.isclose(
        row["initial_secant_stiffness_n_per_m2"],
        resistance[1] / displacement[1],
        rel_tol=CONSISTENCY_REL_TOL,
        abs_tol=0.0,
    ):
        raise CatalogueValidationError("soil stiffness must match the first segment")


def _validate(schema: dict[str, Any], document: Any) -> None:
    Draft202012Validator(schema, format_checker=FormatChecker()).validate(document)


def _citation_address(url: str | None) -> None:
    """Check citation URI syntax without optional jsonschema format packages."""
    if url is None:
        return
    # RFC 3986 (2005), sections 2 and 3: escaped characters and URI components.
    # Citation policy excludes user information and scoped/future IP literals.
    reg_name = r"(?:[A-Za-z0-9._~!$&'()*+,;=-]|%[0-9A-Fa-f]{2})"
    authority = rf"(?:\[[0-9A-Fa-f:.]+\]|{reg_name}+)(?::[0-9]+)?"
    pchar = rf"(?:{reg_name}|[:@])"
    suffix = rf"(?:/{pchar}*)*(?:\?(?:{pchar}|[/?])*)?(?:#(?:{pchar}|[/?])*)?"
    try:
        parsed = urlsplit(url)
        valid = (
            bool(parsed.hostname)
            and re.fullmatch(rf"https?://{authority}{suffix}", url) is not None
        )
        # Explicit IP validation also covers older supported parser versions.
        if valid and parsed.netloc.startswith("["):
            IPv6Address(parsed.hostname or "")
        # Access validates numeric range.
        _ = parsed.port
    except ValueError:
        valid = False
    if not valid:
        raise CatalogueValidationError("citation URL must have valid HTTP(S) syntax and authority")


def load_database(
    database: str, directory: str | Path, *, allow_synthetic: bool = False
) -> list[dict[str, Any]]:
    """Return validated rows, each with its resolved ``citation`` mapping.

    Args:
        database: One of the four identifiers in ``DATABASES``.
        directory: Directory containing schema.json, data.json and sources.yml.
        allow_synthetic: Permit marked test fixtures; defaults to refusal.

    Returns:
        New row dictionaries, in file order, without unit conversion or defaults.

    Raises:
        CatalogueValidationError: Files, schema, units, citations or values fail
            validation. Source rights and authenticity need independent review.
    """
    if database not in DATABASES:
        raise CatalogueValidationError("unknown database")
    directory = Path(directory)
    try:
        schema = json.loads(
            (directory / "schema.json").read_text(encoding="utf-8"),
            object_pairs_hook=_unique_pairs,
        )
        Draft202012Validator.check_schema(schema)
        if schema["properties"]["database"]["const"] != database:
            raise CatalogueValidationError("schema database does not match request")
        data = json.loads(
            (directory / "data.json").read_text(encoding="utf-8"),
            object_pairs_hook=_unique_pairs,
        )
        sources = yaml.load(
            (directory / "sources.yml").read_text(encoding="utf-8"),
            Loader=_UniqueSafeLoader,
        )
        _finite(data)
        _validate(schema, data)
        _validate({"$ref": "#/$defs/sources", "$defs": schema["$defs"]}, sources)
        return _join_rows(database, data["records"], sources["citations"], allow_synthetic)
    except CatalogueValidationError:
        raise
    except ValidationError as exc:
        location = ".".join(str(part) for part in exc.absolute_path) or "document"
        # Values and file-system locations are deliberately omitted from errors.
        raise CatalogueValidationError(f"{location}: {exc.validator} validation failed") from None
    except (
        OSError, ValueError, TypeError, KeyError, OverflowError, RecursionError,
        yaml.YAMLError, SchemaError,
    ):
        raise CatalogueValidationError("cannot read catalogue files or schema") from None


def _join_rows(
    database: str,
    rows: list[dict[str, Any]],
    citations: dict[str, Any],
    allow_synthetic: bool,
) -> list[dict[str, Any]]:
    seen: set[str] = set()
    used: set[str] = set()
    result: list[dict[str, Any]] = []
    for row in rows:
        if row["id"] in seen:
            raise CatalogueValidationError("duplicate row id")
        seen.add(row["id"])
        citation_id = row["citation_id"]
        if citation_id not in citations:
            raise CatalogueValidationError("unresolved row citation")
        used.add(citation_id)
        citation = citations[citation_id]
        _citation_address(citation["url"])
        synthetic = citation["source_class"] == "synthetic_fixture"
        if synthetic != (row["status"] == "synthetic"):
            raise CatalogueValidationError("synthetic row and citation status disagree")
        if synthetic and not allow_synthetic:
            raise CatalogueValidationError("synthetic rows require explicit opt-in")
        try:
            _engineering(database, row)
        except OverflowError:
            raise CatalogueValidationError(
                "engineering numeric values exceed supported arithmetic"
            ) from None
        result.append({**row, "citation": citation})
    if used != set(citations):
        raise CatalogueValidationError("unreferenced citation")
    return result


def load_line_pipe(
    directory: str | Path, *, allow_synthetic: bool = False
) -> list[dict[str, Any]]:
    """Load the line-pipe catalogue using its explicit directory."""
    return load_database("line_pipe", directory, allow_synthetic=allow_synthetic)


def load_flexible_pipe(
    directory: str | Path, *, allow_synthetic: bool = False
) -> list[dict[str, Any]]:
    """Load the flexible-pipe catalogue using its explicit directory."""
    return load_database("flexible_pipe", directory, allow_synthetic=allow_synthetic)


def load_structure_properties(
    directory: str | Path, *, allow_synthetic: bool = False
) -> list[dict[str, Any]]:
    """Load structural cross-section properties using their explicit directory."""
    return load_database("structure_properties", directory, allow_synthetic=allow_synthetic)


def load_soil_stiffness(
    directory: str | Path, *, allow_synthetic: bool = False
) -> list[dict[str, Any]]:
    """Load pipe-soil stiffness curves using their explicit directory."""
    return load_database("soil_stiffness", directory, allow_synthetic=allow_synthetic)
