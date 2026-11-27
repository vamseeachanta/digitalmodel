"""Portable JSON metadata for the GTM example results stores."""

import json
from pathlib import Path


def _portable_path(value):
    """Re-anchor a stored demo path at its repository-relative examples directory."""
    if not isinstance(value, str):
        return value
    path = value.replace('\\', "/")
    marker = "/examples/demos/gtm/"
    if marker in path:
        return "examples/demos/gtm/" + path.rsplit(marker, 1)[1]
    return value


def write_manifest(path: Path, manifest: dict, *, sort_keys: bool) -> dict:
    """Write a run record and return the published paths for its derived indexes."""
    record = manifest.copy()
    for name in ("input_ref", "raw_source_path"):
        if name in record:
            record[name] = _portable_path(record[name])
    config = record["resolved_config"].copy()
    for name in ("source_path", "vessels_path", "mudmats_path", "results_root", "output_root"):
        if name in config:
            config[name] = _portable_path(config[name])
    record["resolved_config"] = config
    with path.open("w", encoding="utf-8") as output:
        json.dump(record, output, indent=2, sort_keys=sort_keys)
    return record
