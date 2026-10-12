"""RAO registry -- tracks diffraction analysis results per hull.

ABOUTME: Provides registration, lookup, and persistence of RAO datasets
linked to hull panel meshes. Registry is stored as YAML index with actual
RAO data in per-hull subdirectories as JSON files. Content-addressed references
retain history; legacy references require independent verification before reuse.
See docs/domains/hull_library/rao-registry.html for the upgrade contract.
"""

from __future__ import annotations

import hashlib
import json
import logging
from datetime import datetime, timezone
from pathlib import Path
from typing import Optional

import numpy as np
import yaml

from digitalmodel.hydrodynamics.hull_library.panel_catalog import (
    PanelCatalog,
    RaoReference,
)

logger = logging.getLogger(__name__)

_DRAFT_TOLERANCE_M = 0.5


class RaoRegistry:
    """Registry of RAO datasets linked to hull library entries."""

    def __init__(self, raos_dir: Path) -> None:
        self._raos_dir = raos_dir
        self._index: dict[str, list[dict]] = {}
        self._load_index()

    @property
    def index_path(self) -> Path:
        return self._raos_dir / "registry.yaml"

    def _load_index(self) -> None:
        if self.index_path.exists():
            with open(self.index_path, encoding="utf-8") as f:
                data = yaml.safe_load(f) or {}
            self._index = data.get("hulls", {})
        else:
            self._index = {}

    def _save_index(self) -> None:
        self._raos_dir.mkdir(parents=True, exist_ok=True)
        data = {
            "version": "1.0",
            "updated": datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
            "hulls": self._index,
        }
        with open(self.index_path, "w", encoding="utf-8") as f:
            yaml.dump(data, f, default_flow_style=False, sort_keys=False)

    def register_rao(
        self,
        hull_id: str,
        solver: str,
        draft_m: float,
        headings_deg: list[float],
        frequencies: np.ndarray,
        amplitudes: np.ndarray,
        phases: np.ndarray,
        loading_condition: Optional[str] = None,
        benchmark_revision: Optional[str] = None,
    ) -> RaoReference:
        """Register content once; retain changed content as a new revision.

        Exact serialized values define identity, including condition and revision,
        excluding registration date. Identical repeats preserve the original
        reference and bytes. Lookup tolerance is never used to replace entries.
        """
        rao_data = {
            "hull_id": hull_id,
            "solver": solver,
            "draft_m": float(draft_m),
            "loading_condition": loading_condition,
            "headings_deg": [float(h) for h in headings_deg],
            "frequencies_rad_s": frequencies.tolist(),
            "amplitudes": amplitudes.tolist(),
            "phases_deg": phases.tolist(),
            "date": datetime.now(timezone.utc).strftime("%Y-%m-%d"),
            "benchmark_revision": benchmark_revision,
        }
        ref = self._store_content(rao_data)
        hull_entries = self._index.get(hull_id, [])
        if any(e.get("file_path") == ref.file_path for e in hull_entries):
            return ref
        self._index[hull_id] = [ref.to_dict(), *hull_entries]
        self._save_index()
        logger.info(
            "Registered RAO: %s/%s (draft=%.1fm, %d freqs)",
            hull_id,
            solver,
            draft_m,
            len(frequencies),
        )
        return ref

    def _store_content(self, data: dict) -> RaoReference:
        """Write a new digest path exclusively, or verify the existing content."""
        hull_id = data["hull_id"]
        if (
            not hull_id
            or hull_id in (".", "..")
            or any(c in hull_id for c in ("/", chr(92), ":", chr(0)))
        ):
            raise ValueError("hull_id must be a single directory name")
        path = self._raos_dir / hull_id / f"sha256_{_content_digest(data)}.json"
        relative = path.relative_to(self._raos_dir.parent.parent).as_posix()
        # Validate the destination before creating anything, including symlinks.
        self._resolve_reference_path(relative)
        for entry in self._index.get(hull_id, []):
            if entry.get("file_path") == relative:
                ref = RaoReference(**entry)
                self.load_rao_data(ref)
                return ref
        if path.exists():
            # Recover an unindexed write only after verifying its full content.
            stored = _read_payload(path)
            ref = _reference(stored, relative)
            self.load_rao_data(ref)
            return ref
        path.parent.mkdir(parents=True, exist_ok=True)
        with path.open("x", encoding="utf-8") as stream:
            json.dump(data, stream, indent=2, allow_nan=False)
        return _reference(data, relative)

    def get_raos(
        self,
        hull_id: str,
        solver: Optional[str] = None,
        draft_m: Optional[float] = None,
    ) -> list[RaoReference]:
        """Look up RAO references for a hull.

        Optionally filter by solver and/or draft (+-0.5m tolerance).
        Newly registered revisions come first; identical repeats do not reorder.
        Legacy references remain discoverable but fail integrity checks on load.
        """
        entries = self._index.get(hull_id, [])
        results = []
        for entry in entries:
            if solver and entry.get("solver") != solver:
                continue
            if draft_m is not None:
                entry_draft = entry.get("draft_m", 0)
                if abs(entry_draft - draft_m) > _DRAFT_TOLERANCE_M:
                    continue
            results.append(RaoReference(**entry))
        return results

    def list_hulls(self) -> list[str]:
        """Return hull IDs that have at least one RAO dataset."""
        return sorted(self._index.keys())

    def load_rao_data(self, ref: RaoReference) -> Optional[dict]:
        """Return verified data or raise ValueError for an integrity failure.

        The digest is carried by the reference's filename, not trusted from the
        mutable index or payload. Metadata must also match, including the date.
        Legacy paths have no original digest and cannot establish integrity.
        """
        file_path = self._resolve_reference_path(ref.file_path)
        if not file_path.name.startswith("sha256_"):
            raise ValueError(
                "RAO integrity check failed: legacy reference has no digest"
            )
        data = _read_payload(file_path)
        try:
            expected = f"sha256_{_content_digest(data)}.json"
            if file_path.name != expected or data["hull_id"] != file_path.parent.name:
                raise ValueError("content digest or hull mismatch")
            if _reference(data, ref.file_path) != ref:
                raise ValueError("reference metadata mismatch")
        except (KeyError, TypeError, ValueError) as exc:
            raise ValueError(f"RAO integrity check failed: {exc}") from exc
        return data

    def _resolve_reference_path(self, relative: str) -> Path:
        """Keep reference reads and writes inside this registry's directory."""
        path = self._raos_dir.parent.parent / relative
        if not path.resolve().is_relative_to(self._raos_dir.resolve()):
            raise ValueError("RAO integrity check failed: path outside registry")
        return path

    def link_to_catalog(self, catalog: PanelCatalog) -> int:
        """Link RAO references to matching PanelCatalog entries.

        Matches by hull_id. Returns count of entries updated.
        """
        updated = 0
        for entry in catalog.entries:
            raos = self.get_raos(entry.hull_id)
            if raos:
                entry.raos = raos
                updated += 1
        return updated


def _content_digest(data: dict) -> str:
    """Hash canonical JSON content, excluding the first-registration date."""
    content = {key: value for key, value in data.items() if key != "date"}
    encoded = json.dumps(
        content, sort_keys=True, separators=(",", ":"), allow_nan=False
    )
    return hashlib.sha256(encoded.encode("utf-8")).hexdigest()


def _reference(data: dict, relative: str) -> RaoReference:
    """Build the unchanged public reference schema from a stored payload."""
    return RaoReference(
        solver=data["solver"],
        draft_m=data["draft_m"],
        loading_condition=data["loading_condition"],
        headings_deg=data["headings_deg"],
        n_frequencies=len(data["frequencies_rad_s"]),
        date=data["date"],
        file_path=relative,
        benchmark_revision=data["benchmark_revision"],
    )


def _read_payload(path: Path) -> dict:
    """Read JSON with explicit integrity errors for absent or invalid payloads."""
    try:
        with path.open(encoding="utf-8") as stream:
            data = json.load(stream)
        if not isinstance(data, dict):
            raise ValueError("payload is not an object")
        return data
    except (OSError, ValueError) as exc:
        raise ValueError("RAO integrity check failed: unreadable payload") from exc


__all__ = ["RaoRegistry"]
