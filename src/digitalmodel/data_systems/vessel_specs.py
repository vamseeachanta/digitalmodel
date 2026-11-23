# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Offline checks for the contracts documented in assets/data/vessel_specs_README.md."""

import argparse
import json
import math
import re
from datetime import datetime, timezone
from pathlib import Path
from typing import Any
from urllib.parse import urlsplit

from jsonschema import Draft202012Validator, FormatChecker
from jsonschema.exceptions import ValidationError


def _unique_object(pairs: list[tuple[str, Any]]) -> dict[str, Any]:
    result: dict[str, Any] = {}
    for key, value in pairs:
        if key in result:
            raise ValueError(f"Duplicate JSON key: {key}")
        result[key] = value
    return result


def _reject_constant(value: str) -> None:
    raise ValueError(f"Non-finite JSON number: {value}")


def parse_json(text: str) -> Any:
    """Reject duplicate keys and non-standard numeric constants."""
    return json.loads(
        text, object_pairs_hook=_unique_object, parse_constant=_reject_constant
    )


def read_json(path: Path) -> Any:
    """Read JSON while rejecting duplicate keys and non-finite values."""
    return parse_json(path.read_text(encoding="utf-8"))


def _https_url(value: Any) -> None:
    if not isinstance(value, str) or any(c.isspace() for c in value):
        raise ValueError("Source URL must be a well-formed HTTPS URL")
    try:
        url = urlsplit(value)
        host = url.hostname or ""
        port = url.port
        valid = (
            url.scheme == "https"
            and url.netloc
            and not url.username
            and not url.password
            and "." in host
            and all(
                re.fullmatch(r"[A-Za-z0-9](?:[A-Za-z0-9-]*[A-Za-z0-9])?", p)
                for p in host.split(".")
            )
            and (port is None or 0 < port < 65536)
        )
    except ValueError as exc:
        raise ValueError("Malformed source URL") from exc
    if not valid:
        raise ValueError("Source URL must be a well-formed HTTPS URL")


def _utc_date(value: Any) -> datetime:
    if not isinstance(value, str) or not value.endswith(("Z", "+00:00")):
        raise ValueError("Retrieval date must be a UTC timestamp")
    if not FormatChecker().conforms(value, "date-time"):
        raise ValueError("Retrieval date must be an RFC3339 timestamp")
    try:
        parsed = datetime.fromisoformat(value.replace("Z", "+00:00"))
    except ValueError as exc:
        raise ValueError("Invalid retrieval date") from exc
    if parsed > datetime.now(timezone.utc):
        raise ValueError("Retrieval date cannot be in the future")
    return parsed


def _source_index(manifest: dict[str, Any]) -> dict[str, dict[str, Any]]:
    sources: dict[str, dict[str, Any]] = {}
    for source in manifest["sources"]:
        url = source["source_url"]
        _https_url(url)
        _utc_date(source["retrieved_utc"])
        if url in sources:
            raise ValueError("Duplicate provenance URL")
        if source["status"] == "retrieved":
            if not re.fullmatch(r"[a-f0-9]{64}", source.get("sha256", "")):
                raise ValueError("Retrieved source requires SHA-256 digest")
            size = source.get("size_bytes")
            if type(size) is not int or size <= 0:
                raise ValueError("Retrieved source requires positive byte count")
            if not source.get("licence_terms", "").strip():
                raise ValueError("Source terms must be recorded")
        sources[url] = source
    return sources


def _check_provenance(row: dict[str, Any], sources: dict[str, Any]) -> None:
    primary = row["source_url"]
    _https_url(primary)
    date = _utc_date(row["retrieved_utc"])
    if primary not in sources or sources[primary]["status"] != "retrieved":
        raise ValueError("Primary source was not retrieved")
    if date != _utc_date(sources[primary]["retrieved_utc"]):
        raise ValueError("Row retrieval date differs from primary source")
    facts = {
        k
        for k, v in row.items()
        if v is not None and k not in ("source_url", "retrieved_utc", "field_sources")
    }
    if facts != set(row["field_sources"]):
        raise ValueError("Per-field sources must cover exactly the non-null facts")
    for url in row["field_sources"].values():
        _https_url(url)
        if url not in sources or sources[url]["status"] != "retrieved":
            raise ValueError("Field source was not retrieved")


def _check_ranges(row: dict[str, Any]) -> None:
    for value in row.values():
        if isinstance(value, (int, float)) and (
            isinstance(value, bool) or not math.isfinite(value) or value <= 0
        ):
            raise ValueError("Numeric specifications must be finite and positive")
    lower, upper = row.get("water_depth_min_m"), row.get("water_depth_max_m")
    if lower is not None and upper is not None and lower > upper:
        raise ValueError("Water-depth range is reversed")
    capacity = row.get("crane_capacity_tonnes")
    if "crane_capacity_basis" in row and (
        (capacity is None) != (row["crane_capacity_basis"] is None)
    ):
        raise ValueError("Crane capacity and its basis must be known together")
    if (lower is not None or upper is not None) and row.get("lay_methods") is None:
        raise ValueError("Operating depth bounds require a known lay method")


def validate_rows(rows: Any, schema_path: Path, manifest: dict[str, Any]) -> None:
    """Validate structure, units encoded in fields, ranges and provenance links.

    Source authenticity and factual transcription require human readback; an
    offline validator cannot prove what a remote source actually published.
    Schemas restrict dp_class to IMO vocabulary; unclassified source notation
    may remain non-null and requires the same per-field provenance as other facts.
    """
    schema = read_json(schema_path)
    Draft202012Validator.check_schema(schema)
    try:
        Draft202012Validator(schema, format_checker=FormatChecker()).validate(rows)
    except ValidationError as exc:
        raise ValueError(f"Schema validation failed: {exc.message}") from exc
    sources = _source_index(manifest)
    names: set[str] = set()
    for row in rows:
        name = row["vessel_name"].strip().casefold()
        if name in names:
            raise ValueError(f"Duplicate vessel name: {row['vessel_name']}")
        names.add(name)
        _check_ranges(row)
        _check_provenance(row, sources)


def validate_dataset(data_dir: Path, kind: str) -> None:
    """Validate one dataset against its schema and provenance manifest."""
    if kind not in ("hlv", "pipelay"):
        raise ValueError("Dataset kind must be hlv or pipelay")
    validate_rows(
        read_json(data_dir / f"vessel_specs_{kind}.json"),
        data_dir / f"vessel_specs_{kind}.schema.json",
        read_json(data_dir / "vessel_specs_provenance.json"),
    )


def main() -> None:
    """Validate both reference datasets from the supplied directory."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("data_dir", type=Path)
    args = parser.parse_args()
    for kind in ("hlv", "pipelay"):
        validate_dataset(args.data_dir, kind)
        print(f"{kind}: valid")


if __name__ == "__main__":
    main()
