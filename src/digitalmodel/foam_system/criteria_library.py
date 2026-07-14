"""Starter cited criteria library for foam application rates (#1586).

Loads the YAML library shipped with the module
(``foam_system/data/foam_criteria_library.yml``) into validated
:class:`~digitalmodel.foam_system.sizing.FoamCriterion` objects so users
select commonly used, cited application rates by key instead of re-typing
cited values per job.

Contract:

- every entry carries the mandatory Citation fields (``standard``,
  ``edition``, ``clause``) — enforced by the ``Citation`` dataclass, so a
  library entry without a full citation cannot load;
- every entry states its edition explicitly; there are no uncited defaults;
- entries flagged ``verify_against_source: true`` must be confirmed against
  the purchased / governing edition before use in a deliverable. The loader
  appends a ``VERIFY AGAINST SOURCE`` marker to the citation note so the
  flag follows the value into every output CSV / report citation label.

Selection API::

    from digitalmodel.foam_system.criteria_library import (
        get_criterion, load_criteria_library,
    )

    criterion = get_criterion("fss_deck_foam_largest_tank_section")
    library = load_criteria_library()   # key -> LibraryEntry

The routed ``foam_system_sizing`` workflow accepts the same keys via the
``criteria_library`` config list (see ``foam_system/workflow.py``).
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Any

import yaml

from digitalmodel.foam_system.sizing import Citation, FoamCriterion

#: YAML criteria library shipped with the module.
LIBRARY_PATH = Path(__file__).resolve().parent / "data" / "foam_criteria_library.yml"

SCHEMA_VERSION = 1

#: Appended to the citation note of entries flagged ``verify_against_source``.
VERIFY_MARKER = "VERIFY AGAINST SOURCE before use in a deliverable"


@dataclass(frozen=True)
class LibraryEntry:
    """One validated library row: cited criterion + selection metadata."""

    key: str
    criterion: FoamCriterion
    application: str
    verify_against_source: bool


def load_criteria_library(path: Path | None = None) -> dict[str, LibraryEntry]:
    """Load and validate the criteria library (key -> :class:`LibraryEntry`).

    Every entry is validated through ``FoamCriterion`` / ``Citation``, so a
    successfully loaded library is guaranteed to satisfy the citation
    contract (standard, edition and clause all present and non-empty).
    """
    library_path = LIBRARY_PATH if path is None else Path(path)
    if not library_path.is_file():
        raise FileNotFoundError(f"foam criteria library not found: {library_path}")
    payload = yaml.safe_load(library_path.read_text(encoding="utf-8"))
    if not isinstance(payload, dict):
        raise ValueError(f"foam criteria library must be a mapping: {library_path}")
    version = payload.get("schema_version")
    if version != SCHEMA_VERSION:
        raise ValueError(
            f"foam criteria library {library_path}: schema_version must be "
            f"{SCHEMA_VERSION}, got {version!r}"
        )
    table = payload.get("criteria")
    if not isinstance(table, dict) or not table:
        raise ValueError(
            f"foam criteria library {library_path}: criteria must be a "
            "non-empty mapping"
        )
    entries: dict[str, LibraryEntry] = {}
    for key, item in table.items():
        entries[str(key)] = _entry(str(key), item, library_path)
    return entries


def get_criterion(key: str, path: Path | None = None) -> FoamCriterion:
    """Select one cited criterion from the library by key.

    Raises ``ValueError`` listing the known keys when ``key`` is absent, so
    a config typo surfaces with the available choices.
    """
    library = load_criteria_library(path)
    entry = library.get(key)
    if entry is None:
        raise ValueError(
            f"foam criteria library has no entry '{key}' "
            f"(known keys: {sorted(library)})"
        )
    return entry.criterion


def list_criteria(path: Path | None = None) -> tuple[LibraryEntry, ...]:
    """All library entries in key order (for docs / discovery)."""
    library = load_criteria_library(path)
    return tuple(library[key] for key in sorted(library))


def _entry(key: str, item: Any, library_path: Path) -> LibraryEntry:
    label = f"foam criteria library {library_path}: criteria['{key}']"
    if not isinstance(item, dict):
        raise ValueError(f"{label} must be a mapping")
    verify = item.get("verify_against_source")
    if not isinstance(verify, bool):
        raise ValueError(
            f"{label}.verify_against_source must be an explicit boolean"
        )
    citation_cfg = item.get("citation")
    if not isinstance(citation_cfg, dict):
        raise ValueError(
            f"{label}.citation is required (mapping with standard, edition, clause)"
        )
    note = _clean(citation_cfg.get("note", ""))
    if verify:
        note = f"{note} [{VERIFY_MARKER}]" if note else f"[{VERIFY_MARKER}]"
    try:
        citation = Citation(
            standard=_clean(citation_cfg.get("standard", "")),
            edition=_clean(citation_cfg.get("edition", "")),
            clause=_clean(citation_cfg.get("clause", "")),
            note=note,
        )
        criterion = FoamCriterion(
            key=key,
            application_rate_lpm_per_m2=float(
                _req(item, "application_rate_lpm_per_m2", label)
            ),
            discharge_time_min=float(_req(item, "discharge_time_min", label)),
            citation=citation,
        )
    except (TypeError, ValueError) as exc:
        raise ValueError(f"{label}: {exc}") from exc
    application = _clean(item.get("application", ""))
    if not application:
        raise ValueError(f"{label}.application (usage description) is required")
    return LibraryEntry(
        key=key,
        criterion=criterion,
        application=application,
        verify_against_source=verify,
    )


def _req(item: dict, name: str, label: str) -> Any:
    value = item.get(name)
    if value is None:
        raise ValueError(f"{label}.{name} is required")
    return value


def _clean(value: Any) -> str:
    """Collapse YAML folded-scalar whitespace to single spaces."""
    return " ".join(str(value).split())
