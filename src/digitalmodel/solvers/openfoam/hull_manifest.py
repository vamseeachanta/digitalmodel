#!/usr/bin/env python3
"""
ABOUTME: The ingestion-lane contract for an arbitrary client hull (#2023).
``hull_manifest.json`` is the only channel through which a non-benchmark hull
reaches the resistance case builder, so this loader fails closed on every field
a downstream derivation depends on.

Every check here corresponds to a value that is silently PLAUSIBLE when wrong.
A hull whose x is aft still meshes, still converges and still reports a
resistance -- for a ship towed stern-first. A hull whose origin is not the keel
puts the free surface in the wrong place and the case runs anyway. None of
these surface as an error hours later; they surface as a number.
"""

from __future__ import annotations

import json
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, List, Mapping, Optional, Sequence, Tuple

__all__ = ["HullManifest", "HullManifestError", "load_hull_manifest"]


class HullManifestError(ValueError):
    """A manifest that cannot be trusted to build a case from."""


#: The orientation the builder is written against. Stated rather than inferred:
#: the bow is placed at the inlet on the strength of this field alone.
EXPECTED_ORIENTATION: Mapping[str, str] = {
    "x": "forward",
    "y": "port",
    "z": "up",
}

#: The origin the free-surface placement assumes. ``waterline_z = draft`` is
#: true only when the keel sits on z = 0.
EXPECTED_ORIGIN = "aft_perpendicular_keel"

_REQUIRED = (
    "source_file",
    "source_sha256",
    "units_in",
    "scale_to_m",
    "orientation",
    "origin",
    "lpp_m",
    "beam_m",
    "draft_m",
    "wetted_surface_m2",
    "displacement_m3",
    "watertight",
    "n_triangles",
    "bbox_min_m",
    "bbox_max_m",
)

_POSITIVE = ("lpp_m", "beam_m", "draft_m", "wetted_surface_m2", "displacement_m3")

#: Tolerance on the keel sitting at z = 0, as a fraction of the draft. Meshing
#: tolerances and STL rounding move it by microns; a real offset moves it by
#: percent.
_KEEL_TOLERANCE = 0.02


@dataclass(frozen=True)
class HullManifest:
    """A normalised hull, in metres, as published by the ingestion lane.

    The ``*_m`` fields are ALREADY metres. ``units_in`` and ``scale_to_m``
    describe the source file the ingestion lane consumed and are carried for
    provenance only -- re-applying ``scale_to_m`` here would shrink a 6 m hull
    to 6 mm, and every derived extent with it, and the case would still mesh.
    """

    source_file: str
    source_sha256: str
    units_in: str
    scale_to_m: float
    orientation: Mapping[str, str]
    origin: str
    lpp_m: float
    beam_m: float
    draft_m: float
    wetted_surface_m2: float
    displacement_m3: float
    watertight: bool
    n_triangles: int
    bbox_min_m: Tuple[float, float, float]
    bbox_max_m: Tuple[float, float, float]

    #: Multi-region block, present only when the ingestion lane emitted
    #: appendages beside the hull. Optional by design: a hull-only manifest
    #: predates this key and must keep loading unchanged.
    regions: Optional[Mapping[str, Any]] = None

    # -- regions ---------------------------------------------------------- #

    @property
    def appendage_regions(self) -> List[Mapping[str, Any]]:
        """Every region that is not the hull, in manifest order."""
        if not self.regions:
            return []
        return [
            region
            for region in self.regions.get("regions", [])
            if region.get("role") != "hull"
        ]

    @property
    def reference_wetted_surface_m2(self) -> float:
        """The area ``Aref`` must be built from.

        For a hull-only manifest this is ``wetted_surface_m2`` and nothing has
        changed. For a multi-region manifest it is the union's EXTERNAL wetted
        area, which is smaller than the sum of the parts: an appendage
        interpenetrates the hull, so part of each is inside the other and
        wetted by nothing. Summing would inflate Aref, and Aref is a
        denominator -- every reported coefficient would come back low by
        exactly that inflation, in a case that converged perfectly.
        """
        if not self.regions:
            return self.wetted_surface_m2
        union = self.regions.get("union", {})
        external = union.get("wetted_surface_external_m2")
        if external is None or float(external) <= 0.0:
            raise HullManifestError(
                "regions.union.wetted_surface_external_m2 is missing or "
                "non-positive. It is the only honest reference area for a "
                "multi-region hull; the naive sum of the per-region areas "
                "double-counts the interpenetration and is an upper bound."
            )
        return float(external)

    @property
    def wetted_surface_upper_bound_m2(self) -> float:
        """The naive sum, kept only so the two can be compared."""
        if not self.regions:
            return self.wetted_surface_m2
        return float(
            self.regions.get("union", {}).get(
                "wetted_surface_naive_sum_m2", self.wetted_surface_m2
            )
        )

    # -- derived geometry ------------------------------------------------- #

    @property
    def loa_m(self) -> float:
        """Overall length of the SURFACE, which is not Lpp."""
        return self.bbox_max_m[0] - self.bbox_min_m[0]

    @property
    def midship_x_m(self) -> float:
        """x of the bounding-box centre. The hull is translated by -this so
        the emitted domain is centred on midship, as ``DomainBuilder`` assumes.
        """
        return (self.bbox_min_m[0] + self.bbox_max_m[0]) / 2.0

    @property
    def hull_height_m(self) -> float:
        return self.bbox_max_m[2] - self.bbox_min_m[2]

    @property
    def half_beam_m(self) -> float:
        return self.beam_m / 2.0

    @property
    def block_coefficient(self) -> float:
        """Cb = V / (Lpp B T). Reported in the provenance; a Cb outside
        0.3-0.9 is a strong hint the units or the origin are wrong."""
        return self.displacement_m3 / (self.lpp_m * self.beam_m * self.draft_m)

    # -- construction ----------------------------------------------------- #

    @classmethod
    def from_dict(cls, data: Mapping[str, Any]) -> "HullManifest":
        _require_fields(data)
        _check_positive(data)
        _check_orientation(data)
        _check_origin(data)
        _check_watertight(data)
        bbox_min = _vec3(data, "bbox_min_m")
        bbox_max = _vec3(data, "bbox_max_m")
        _check_bbox(data, bbox_min, bbox_max)
        regions = data.get("regions")
        _check_regions(regions)

        return cls(
            regions=regions,
            source_file=str(data["source_file"]),
            source_sha256=str(data["source_sha256"]),
            units_in=str(data["units_in"]),
            scale_to_m=float(data["scale_to_m"]),
            orientation=dict(data["orientation"]),
            origin=str(data["origin"]),
            lpp_m=float(data["lpp_m"]),
            beam_m=float(data["beam_m"]),
            draft_m=float(data["draft_m"]),
            wetted_surface_m2=float(data["wetted_surface_m2"]),
            displacement_m3=float(data["displacement_m3"]),
            watertight=bool(data["watertight"]),
            n_triangles=int(data["n_triangles"]),
            bbox_min_m=bbox_min,
            bbox_max_m=bbox_max,
        )

    def to_provenance(self) -> Dict[str, Any]:
        """The subset a reader needs to know WHICH hull was solved."""
        return {
            "source_file": self.source_file,
            "source_sha256": self.source_sha256,
            "units_in": self.units_in,
            "scale_to_m": self.scale_to_m,
            "origin": self.origin,
            "orientation": dict(self.orientation),
            "lpp_m": self.lpp_m,
            "beam_m": self.beam_m,
            "draft_m": self.draft_m,
            "loa_m": self.loa_m,
            "wetted_surface_m2": self.wetted_surface_m2,
            "displacement_m3": self.displacement_m3,
            "block_coefficient": self.block_coefficient,
            "n_triangles": self.n_triangles,
            "watertight": self.watertight,
            "regions": self.regions,
        }


def load_hull_manifest(path: Path | str) -> HullManifest:
    """Read and validate ``hull_manifest.json``."""
    path = Path(path)
    if not path.is_file():
        raise HullManifestError(f"hull manifest not found: {path}")
    try:
        data = json.loads(path.read_text())
    except json.JSONDecodeError as exc:
        raise HullManifestError(f"{path} is not valid JSON: {exc}") from exc
    if not isinstance(data, dict):
        raise HullManifestError(f"{path} must contain a JSON object")
    return HullManifest.from_dict(data)


# --------------------------------------------------------------------------- #
#  Checks. One function each, so a failure names the field it failed on.
# --------------------------------------------------------------------------- #

def _require_fields(data: Mapping[str, Any]) -> None:
    missing = [f for f in _REQUIRED if f not in data or data[f] is None]
    if missing:
        raise HullManifestError(
            f"hull manifest is missing required field(s): {', '.join(missing)}"
        )


def _check_positive(data: Mapping[str, Any]) -> None:
    for field in _POSITIVE:
        try:
            value = float(data[field])
        except (TypeError, ValueError) as exc:
            raise HullManifestError(f"{field} is not a number: {data[field]!r}") from exc
        if value <= 0:
            raise HullManifestError(f"{field} must be positive, got {value}")


def _check_orientation(data: Mapping[str, Any]) -> None:
    orientation = data["orientation"]
    if not isinstance(orientation, Mapping):
        raise HullManifestError(f"orientation must be an object, got {orientation!r}")
    if dict(orientation) != dict(EXPECTED_ORIENTATION):
        raise HullManifestError(
            f"orientation {dict(orientation)} is not the expected "
            f"{dict(EXPECTED_ORIENTATION)}. The builder places the bow at the "
            f"inlet on the strength of this field; a hull whose +x is aft is "
            f"towed stern-first, and it solves."
        )


def _check_origin(data: Mapping[str, Any]) -> None:
    if str(data["origin"]) != EXPECTED_ORIGIN:
        raise HullManifestError(
            f"origin {data['origin']!r} is not {EXPECTED_ORIGIN!r}. The free "
            f"surface is placed at z = draft, which holds only keel-up."
        )


def _check_regions(regions: Any) -> None:
    """Every region individually closed, and the union area present.

    The closure check is per REGION and not on the whole set, because that is
    exactly the premise snappyHexMesh relies on: it tests each surface for
    inside/outside separately and forms the union from the results. One open
    appendage means the mesher keeps that appendage's interior as fluid, and
    the case still meshes, still solves and still reports a force.
    """
    if regions is None:
        return
    if not isinstance(regions, Mapping):
        raise HullManifestError(f"regions must be an object, got {regions!r}")
    listed = regions.get("regions")
    if not isinstance(listed, Sequence) or not listed:
        raise HullManifestError("regions.regions must be a non-empty list")
    for region in listed:
        if not bool(region.get("watertight")):
            name = region.get("name", "<unnamed>")
            raise HullManifestError(
                f"region {name!r} is not watertight. Regions are handed to "
                "snappyHexMesh as separate closed surfaces and the union is "
                "formed from per-surface inside/outside tests; on an open "
                "surface that test keeps the interior."
            )
    if len(listed) != len({region.get("name") for region in listed}):
        raise HullManifestError(
            f"regions.regions carries duplicate names: "
            f"{[r.get('name') for r in listed]}"
        )
    union = regions.get("union")
    if not isinstance(union, Mapping):
        raise HullManifestError("regions.union must be an object")


def _check_watertight(data: Mapping[str, Any]) -> None:
    if not bool(data["watertight"]):
        raise HullManifestError(
            "watertight is false. snappyHexMesh will happily mesh a leaking "
            "surface and lose the interior region to the outside."
        )


def _vec3(data: Mapping[str, Any], field: str) -> Tuple[float, float, float]:
    raw = data[field]
    if not isinstance(raw, Sequence) or isinstance(raw, (str, bytes)) or len(raw) != 3:
        raise HullManifestError(f"{field} must be three numbers, got {raw!r}")
    try:
        return (float(raw[0]), float(raw[1]), float(raw[2]))
    except (TypeError, ValueError) as exc:
        raise HullManifestError(f"{field} must be three numbers, got {raw!r}") from exc


def _check_bbox(
    data: Mapping[str, Any],
    bbox_min: Tuple[float, float, float],
    bbox_max: Tuple[float, float, float],
) -> None:
    if any(lo >= hi for lo, hi in zip(bbox_min, bbox_max)):
        raise HullManifestError(f"bbox is degenerate: {bbox_min} -> {bbox_max}")

    lpp = float(data["lpp_m"])
    loa = bbox_max[0] - bbox_min[0]
    if loa < lpp:
        raise HullManifestError(
            f"bbox x-extent {loa:.4g} m cannot contain the stated Lpp {lpp:.4g} m; "
            f"one of the two is wrong"
        )

    draft = float(data["draft_m"])
    if bbox_max[2] < draft:
        raise HullManifestError(
            f"draft {draft:.4g} m is above the hull's top z {bbox_max[2]:.4g} m"
        )

    if abs(bbox_min[2]) > _KEEL_TOLERANCE * draft:
        raise HullManifestError(
            f"keel is at z = {bbox_min[2]:.4g} m, not 0. The origin is declared "
            f"{EXPECTED_ORIGIN!r} and the free surface is placed at z = draft."
        )

    beam = float(data["beam_m"])
    bbox_beam = bbox_max[1] - bbox_min[1]
    if bbox_beam < beam * (1.0 - _KEEL_TOLERANCE):
        raise HullManifestError(
            f"bbox y-extent {bbox_beam:.4g} m is narrower than the stated beam "
            f"{beam:.4g} m"
        )
