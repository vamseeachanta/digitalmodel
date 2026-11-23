#!/usr/bin/env python3
"""Schema authority for the protected-surface manifest (#1961, Stage 1).

Reasons-as-data, following the split that ``scripts/generated_html_ownership.py``
already uses: the JSON manifest carries the data, this module decides whether
that data is representable at all.

Two shapes are deliberately unrepresentable, because each is a way of making the
scanner certify a leak as clean:

* **A boundary rule.** A word boundary requires a word-character transition and
  ``_`` is a word character, so a bounded matcher never fires on a token
  immediately followed by an underscore -- the exact shape a public package
  export takes. Measured against a real historical tree, bounding suppressed one
  content-addressed false positive at the cost of thirteen genuine occurrences,
  the public export among them.
* **A whole-file exemption.** A file-level exempt is a backdoor: it removes a
  path from the comparison and proves nothing about it. Only per-line sentinels
  and field-scoped content-addressed declarations are expressible.

Every classification must carry a non-empty reason, as
``scripts/check_generated_html.py`` already requires of its page exclusions.
"""

from __future__ import annotations

import json
import re
from pathlib import Path
from typing import Any

SCHEMA_VERSION = 1

#: Matchers the schema admits. No bounded variant is listed, and none may be
#: added without deleting the measurement that removed it.
ALLOWED_MATCHERS = {
    "A": "substring_ci",
    "B": "co_occurrence_same_file",
}

#: Classification kinds. Every kind here scans the bytes; the content-addressed
#: kind narrows *which offsets count as findings*, never which files are read.
SCANNING_KINDS = {"scan_text"}
ALLOWED_KINDS = SCANNING_KINDS | {"content_addressed"}

#: Any rule key hinting at a boundary rule is refused outright, so a future edit
#: cannot reintroduce the contract by a different spelling.
_BOUNDARY_KEY_RE = re.compile(r"bound|boundar|\bword\b|wordwise", re.IGNORECASE)


class ManifestError(ValueError):
    """The manifest cannot be represented. Always fatal, never downgraded."""


def _require(condition: bool, message: str) -> None:
    if not condition:
        raise ManifestError(message)


def _validate_rule(rule_id: str, rule: Any) -> None:
    _require(isinstance(rule, dict), f"rule {rule_id}: not an object")
    for key in rule:
        _require(
            not _BOUNDARY_KEY_RE.search(str(key)),
            f"rule {rule_id}: boundary-shaped key {key!r} is not representable",
        )
    rule_class = rule.get("class")
    _require(rule_class in ALLOWED_MATCHERS, f"rule {rule_id}: unknown class {rule_class!r}")
    matcher = rule.get("matcher")
    _require(
        not _BOUNDARY_KEY_RE.search(str(matcher)),
        f"rule {rule_id}: boundary matcher {matcher!r} is not representable",
    )
    _require(
        matcher == ALLOWED_MATCHERS[rule_class],
        f"rule {rule_id}: class {rule_class} requires matcher "
        f"{ALLOWED_MATCHERS[rule_class]!r}, got {matcher!r}",
    )


def _validate_classification(entry: Any) -> None:
    _require(isinstance(entry, dict), "classification: not an object")
    entry_id = entry.get("id")
    _require(bool(entry_id), "classification: missing id")
    kind = entry.get("kind")
    _require(
        kind in ALLOWED_KINDS,
        f"classification {entry_id}: kind {kind!r} is not representable "
        f"(a whole-file exemption is a backdoor)",
    )
    reason = entry.get("reason")
    _require(
        isinstance(reason, str) and reason.strip() != "",
        f"classification {entry_id}: every classification must carry a non-empty reason",
    )
    paths = entry.get("paths")
    _require(isinstance(paths, list) and paths != [], f"classification {entry_id}: no paths")
    if kind == "content_addressed":
        patterns = entry.get("field_patterns")
        _require(
            isinstance(patterns, list) and patterns != [],
            f"classification {entry_id}: content-addressed entries must name their fields",
        )
        for pattern in patterns:
            try:
                re.compile(pattern)
            except re.error as exc:  # pragma: no cover - defensive
                raise ManifestError(f"classification {entry_id}: bad field pattern: {exc}")


def validate(manifest: Any) -> dict:
    """Return the manifest, or raise :class:`ManifestError`. Never repairs."""
    _require(isinstance(manifest, dict), "manifest: not an object")
    _require(
        manifest.get("schema_version") == SCHEMA_VERSION,
        f"manifest: schema_version must be {SCHEMA_VERSION}",
    )

    rules = manifest.get("rules")
    _require(isinstance(rules, dict) and rules != {}, "manifest: no rules declared")
    for rule_id, rule in rules.items():
        _validate_rule(rule_id, rule)

    classifications = manifest.get("classifications", [])
    _require(isinstance(classifications, list), "manifest: classifications must be a list")
    seen: set[str] = set()
    for entry in classifications:
        _validate_classification(entry)
        entry_id = entry["id"]
        _require(entry_id not in seen, f"classification {entry_id}: declared twice")
        seen.add(entry_id)

    if "default_classification" in manifest:
        default = manifest["default_classification"]
        _require(
            default in SCANNING_KINDS,
            f"manifest: default_classification {default!r} must be a scanning kind, "
            "so that an unanticipated path is scanned rather than skipped",
        )

    limits = manifest.get("limits", {})
    _require(isinstance(limits, dict), "manifest: limits must be an object")
    max_bytes = limits.get("max_file_bytes")
    _require(
        isinstance(max_bytes, int) and max_bytes > 0,
        "manifest: limits.max_file_bytes must be a positive integer",
    )

    sentinel = manifest.get("line_sentinel")
    _require(
        isinstance(sentinel, str) and sentinel.strip() != "",
        "manifest: line_sentinel must be a non-empty string",
    )
    return manifest


def load(path: Path) -> dict:
    """Read and validate a manifest file."""
    try:
        raw = json.loads(Path(path).read_text(encoding="utf-8"))
    except FileNotFoundError as exc:
        raise ManifestError(f"manifest not found: {path}") from exc
    except json.JSONDecodeError as exc:
        raise ManifestError(f"manifest is not valid JSON: {exc}") from exc
    return validate(raw)


def classify(manifest: dict, path: str) -> tuple[str, list[str]]:
    """Return ``(kind, field_patterns)`` for ``path``.

    Raises :class:`ManifestError` when no classification and no default apply --
    an unanticipated path fails the run rather than disappearing from it.
    """
    for entry in manifest.get("classifications", []):
        for candidate in entry["paths"]:
            if path == candidate or path.startswith(candidate.rstrip("*")) and candidate.endswith("*"):
                return entry["kind"], list(entry.get("field_patterns", []))
    default = manifest.get("default_classification")
    if default is None:
        raise ManifestError(f"unclassified tracked path: {path}")
    return default, []
