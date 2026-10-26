# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Digest-bound matrix, token inventory and complete-campaign reader."""

from __future__ import annotations

import csv
import hashlib
import io
from pathlib import Path
from typing import Any, Iterable, TypedDict

from .validation import VERSION, load_json, load_result, parse_json

CASE_IDS = tuple(f"R{n:02}-W{w}-{configuration}"
                 for n in (8, 10, 12) for w in (1, 2) for configuration in ("SCR", "SLWR"))
SEA_STATE_IDS = ("F03", "F04", "F05")
_DATA = Path(__file__).with_name("data")
_MATRIX_SHA256 = "f95a98735eb3882a354857fc56975a82d1b67095d6a57cb8cf565229e9ddf2c1"
# The registry digest is pinned separately from its adjacent manifest.
_REGISTRY_SHA256 = "ba03ec892b938cfb55b65998510f1eaed12094a06840bd45f49675966c06714f"


class TokenRecord(TypedDict):
    """One exact output identity and the module that produces it."""

    id: str
    kind: str
    caption: str
    producing_module: str


def _checked_bytes(path: Path, expected: str) -> bytes:
    payload = path.read_bytes()
    if hashlib.sha256(payload).hexdigest() != expected:
        raise ValueError("catalog digest mismatch")
    return payload


def load_case_matrix(path: Path | None = None) -> list[dict[str, str]]:
    """Load the twelve frozen rows, retaining every TSV cell as written."""
    manifest = load_json(_DATA / "manifest-v1.json")
    if manifest["version"] != VERSION or manifest["matrix_sha256"] != _MATRIX_SHA256:
        raise ValueError("matrix manifest mismatch")
    raw = _checked_bytes(Path(path) if path is not None else _DATA / "case-matrix-v1.tsv",
                         _MATRIX_SHA256)
    rows = list(csv.DictReader(io.StringIO(raw.decode("utf-8")), delimiter="\t"))
    if len(rows) != 12 or {row["case_id"] for row in rows} != set(CASE_IDS):
        raise ValueError("matrix requires exactly twelve unique case ids")
    if any(row["fatigue_seas"].split(";") != list(SEA_STATE_IDS) for row in rows):
        raise ValueError("matrix requires exactly F03, F04, F05")
    return rows


def validate_matrix(rows: list[dict[str, str]]) -> None:
    """Require all frozen cells and cases; input row order has no significance."""
    expected = load_case_matrix()
    if (len(rows) != 12 or any(not isinstance(row, dict) for row in rows)
            or {row.get("case_id") for row in rows} != set(CASE_IDS)
            or sorted(rows, key=lambda r: r["case_id"]) != sorted(expected, key=lambda r: r["case_id"])):
        raise ValueError("matrix differs from frozen twelve-case data")


def load_token_registry(path: Path | None = None) -> list[TokenRecord]:
    """Read the versioned registry only after checking its exact payload digest."""
    manifest = load_json(_DATA / "manifest-v1.json")
    if manifest["version"] != VERSION or manifest["registry_sha256"] != _REGISTRY_SHA256:
        raise ValueError("registry manifest mismatch")
    source = Path(path) if path is not None else _DATA / "token-registry-v1.json"
    document = parse_json(_checked_bytes(source, _REGISTRY_SHA256))
    tokens = document["tokens"]
    if (document["version"] != VERSION or len(tokens) != 58
            or len({token["id"] for token in tokens}) != 58):
        raise ValueError("token registry inventory mismatch")
    return tokens


def assert_token_ids(producing_module: str, ids: Iterable[str]) -> None:
    """Reject missing, unexpected or duplicate output ids for the named module."""
    expected = {token["id"] for token in load_token_registry()
                if token["producing_module"] == producing_module}
    if not expected:
        raise ValueError("unknown producing module")
    actual = list(ids)
    if len(actual) != len(set(actual)) or set(actual) != expected:
        raise ValueError("produced token ids differ from registry")


def load_campaign(results: Path) -> dict[str, dict[str, Any]]:
    """Read all twelve strength exports and thirty-six fatigue exports.

    Each case contains strength.json and fatigue-F03/F04/F05.json. Extra files,
    omitted cases, and disagreement between paths and embedded ids are errors.
    This checks exchange completeness; no result is qualified or aggregated here.
    """
    results = Path(results)
    load_case_matrix()
    if not results.is_dir() or {p.name for p in results.iterdir()} != set(CASE_IDS):
        raise ValueError("campaign requires exactly twelve case directories")
    expected = {"strength.json", *(f"fatigue-{state}.json" for state in SEA_STATE_IDS)}
    records = {}
    for case in CASE_IDS:
        directory = results / case
        if not directory.is_dir() or {p.name for p in directory.iterdir()} != expected:
            raise ValueError("campaign requires strength and exactly three fatigue exports")
        strength = load_result(directory / "strength.json", "strength")
        fatigue = {state: load_result(directory / f"fatigue-{state}.json", "fatigue")
                   for state in SEA_STATE_IDS}
        if (strength["case_id"] != case
                or any(row["case_id"] != case or row["sea_state"]["id"] != state
                       for state, row in fatigue.items())):
            raise ValueError("campaign filename and record identity disagree")
        records[case] = {"strength": strength, "fatigue": fatigue}
    return records
