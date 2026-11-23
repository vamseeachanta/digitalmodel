"""
ABOUTME: Shared synthetic fixtures for the arbitrary-hull case-construction
tests (#2023). The ingestion lane that produces a real ``hull_manifest.json``
is a separate lane; these fixtures are written against its published schema so
this lane is testable before that one lands.
"""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any, Dict

import pytest

#: The ingestion-lane contract, field for field. Values here are a plausible
#: model-scale container ship, deliberately NOT the KCS numbers: a fixture that
#: happened to be KCS could not distinguish a derived quantity from an
#: inherited one, which is the whole point of this test suite.
SYNTHETIC_MANIFEST: Dict[str, Any] = {
    "source_file": "synthetic_hull.stl",
    "source_sha256": "0" * 64,
    "units_in": "mm",
    "scale_to_m": 0.001,
    "orientation": {"x": "forward", "y": "port", "z": "up"},
    "origin": "aft_perpendicular_keel",
    "lpp_m": 6.0,
    "beam_m": 0.9,
    "draft_m": 0.30,
    "wetted_surface_m2": 7.2,
    "displacement_m3": 0.72,
    "watertight": True,
    "n_triangles": 120000,
    "bbox_min_m": [-0.05, -0.45, 0.0],
    "bbox_max_m": [6.15, 0.45, 0.55],
}


def scaled_manifest_dict(factor: float) -> Dict[str, Any]:
    """The synthetic manifest with every LENGTH scaled by ``factor``.

    Areas scale as factor^2 and volumes as factor^3, because a geometrically
    similar hull is what a scale-invariance test needs -- scaling the lengths
    while leaving the wetted surface alone would test nothing but arithmetic.
    """
    d = json.loads(json.dumps(SYNTHETIC_MANIFEST))
    for key in ("lpp_m", "beam_m", "draft_m"):
        d[key] = d[key] * factor
    d["wetted_surface_m2"] = d["wetted_surface_m2"] * factor**2
    d["displacement_m3"] = d["displacement_m3"] * factor**3
    d["bbox_min_m"] = [c * factor for c in d["bbox_min_m"]]
    d["bbox_max_m"] = [c * factor for c in d["bbox_max_m"]]
    return d


@pytest.fixture
def manifest_dict() -> Dict[str, Any]:
    return json.loads(json.dumps(SYNTHETIC_MANIFEST))


@pytest.fixture
def manifest_file(tmp_path: Path, manifest_dict: Dict[str, Any]) -> Path:
    path = tmp_path / "hull_manifest.json"
    path.write_text(json.dumps(manifest_dict, indent=2))
    return path


@pytest.fixture
def stl_file(tmp_path: Path) -> Path:
    """A minimal ASCII STL.

    The builder must copy the surface into ``constant/triSurface`` and name
    every dictionary that refers to it consistently. It never parses the STL,
    so a one-facet solid is a sufficient stand-in and keeps the suite fast.
    """
    path = tmp_path / "synthetic_hull.stl"
    path.write_text(
        "solid hull\n"
        "  facet normal 0 0 1\n"
        "    outer loop\n"
        "      vertex 0 0 0\n"
        "      vertex 1 0 0\n"
        "      vertex 0 1 0\n"
        "    endloop\n"
        "  endfacet\n"
        "endsolid hull\n"
    )
    return path
