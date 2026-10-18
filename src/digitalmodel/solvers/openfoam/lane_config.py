"""Generic, environment-configurable CFD lane metadata."""

from __future__ import annotations

import os
from pathlib import Path
from typing import Any, Mapping

import yaml


DEFAULT_LANES_FILE = Path(__file__).with_name("data") / "lanes.example.yml"


def load_lanes(path: str | os.PathLike[str] | None = None) -> Mapping[str, Mapping[str, Any]]:
    """Load lane definitions without embedding infrastructure identities in code."""
    source = Path(path or os.environ.get("DM_CFD_LANES_FILE", DEFAULT_LANES_FILE))
    payload = yaml.safe_load(source.read_text(encoding="utf-8"))
    lanes = payload.get("lanes") if isinstance(payload, Mapping) else None
    if not isinstance(lanes, Mapping) or not lanes:
        raise ValueError(f"{source}: expected a non-empty 'lanes' mapping")
    if any(not isinstance(lane_id, str) or not isinstance(data, Mapping)
           for lane_id, data in lanes.items()):
        raise ValueError(f"{source}: every lane id and definition must be a mapping entry")
    return lanes
