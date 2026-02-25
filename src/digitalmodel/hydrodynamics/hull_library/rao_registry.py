"""RAO registry -- tracks diffraction analysis results per hull.

ABOUTME: Provides registration, lookup, and persistence of RAO datasets
linked to hull panel meshes. Registry is stored as YAML index with actual
RAO data in per-hull subdirectories as JSON files.
"""

from __future__ import annotations

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
            with open(self.index_path) as f:
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
        with open(self.index_path, "w") as f:
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
        """Register a new RAO dataset for a hull.

        Saves RAO data to JSON and updates the registry index.
        Idempotent: re-registering same hull+solver+draft overwrites.
        """
        date_str = datetime.now(timezone.utc).strftime("%Y-%m-%d")
        draft_label = f"{draft_m:.1f}".replace(".", "p")
        filename = f"{solver}_{draft_label}m_{date_str}.json"

        # Create hull subdirectory
        hull_dir = self._raos_dir / hull_id
        hull_dir.mkdir(parents=True, exist_ok=True)
        file_path = hull_dir / filename

        # Save RAO data as JSON
        rao_data = {
            "hull_id": hull_id,
            "solver": solver,
            "draft_m": draft_m,
            "loading_condition": loading_condition,
            "headings_deg": [float(h) for h in headings_deg],
            "frequencies_rad_s": frequencies.tolist(),
            "amplitudes": amplitudes.tolist(),
            "phases_deg": phases.tolist(),
            "date": date_str,
            "benchmark_revision": benchmark_revision,
        }
        with open(file_path, "w") as f:
            json.dump(rao_data, f, indent=2)

        # Build reference
        ref = RaoReference(
            solver=solver,
            draft_m=draft_m,
            loading_condition=loading_condition,
            headings_deg=headings_deg,
            n_frequencies=len(frequencies),
            date=date_str,
            file_path=str(
                file_path.relative_to(self._raos_dir.parent.parent)
            ),
            benchmark_revision=benchmark_revision,
        )

        # Update index (idempotent: remove existing match)
        hull_entries = self._index.get(hull_id, [])
        hull_entries = [
            e
            for e in hull_entries
            if not (
                e.get("solver") == solver
                and abs(e.get("draft_m", 0) - draft_m) < _DRAFT_TOLERANCE_M
            )
        ]
        hull_entries.append(ref.to_dict())
        self._index[hull_id] = hull_entries
        self._save_index()

        logger.info(
            "Registered RAO: %s/%s (draft=%.1fm, %d freqs)",
            hull_id,
            solver,
            draft_m,
            len(frequencies),
        )
        return ref

    def get_raos(
        self,
        hull_id: str,
        solver: Optional[str] = None,
        draft_m: Optional[float] = None,
    ) -> list[RaoReference]:
        """Look up RAO references for a hull.

        Optionally filter by solver and/or draft (+-0.5m tolerance).
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
        """Load actual RAO data from the file referenced by an RaoReference."""
        file_path = self._raos_dir.parent.parent / ref.file_path
        if not file_path.exists():
            logger.warning("RAO file not found: %s", file_path)
            return None
        with open(file_path) as f:
            return json.load(f)

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


__all__ = ["RaoRegistry"]
