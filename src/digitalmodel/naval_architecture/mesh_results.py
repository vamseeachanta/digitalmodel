"""Mesh results for the mesh hydrostatics adapter."""
from __future__ import annotations

import numpy as np
import math
from collections.abc import Mapping
from copy import deepcopy
from dataclasses import dataclass, field
from typing import Iterator, Optional
from .mesh_geometry import MeshContractError, SCHEMA_VERSION, _read_only


@dataclass(frozen=True, slots=True)
class Quantity:
    value: object
    units: str
    provenance: str  # "computed" | "declared" | "not_computed"
    input_hash: str
    reason: Optional[str] = None

    def __post_init__(self) -> None:
        value = object.__getattribute__(self, "value")
        snapshot = _read_only(value) if isinstance(value, np.ndarray) else deepcopy(value)
        object.__setattr__(self, "value", snapshot)

    def __getattribute__(self, name):
        value = object.__getattribute__(self, name)
        if name == "value":
            return value.view() if isinstance(value, np.ndarray) else deepcopy(value)
        return value

    def __reduce__(self):
        return type(self), (self.value, self.units, self.provenance, self.input_hash, self.reason)

    def to_dict(self) -> dict:
        v = self.value
        if isinstance(v, np.ndarray):
            v = v.tolist()
        return {"value": v, "units": self.units, "provenance": self.provenance,
                "input_hash": self.input_hash, "reason": self.reason}



@dataclass(frozen=True, slots=True)
class HydrostaticsResult(Mapping):
    quantities: dict
    input_hash: str
    conventions: dict = field(default_factory=dict)

    def __post_init__(self) -> None:
        object.__setattr__(self, "quantities", dict(self.quantities))
        object.__setattr__(self, "conventions", deepcopy(self.conventions))

    def __getattribute__(self, name):
        value = object.__getattribute__(self, name)
        if name == "quantities":
            return dict(value)
        if name == "conventions":
            return deepcopy(value)
        return value

    def __reduce__(self):
        return type(self), (dict(self.quantities), self.input_hash, dict(self.conventions))

    def __getitem__(self, key: str) -> Quantity:
        return self.quantities[key]

    def __iter__(self) -> Iterator[str]:
        return iter(self.quantities)

    def __len__(self) -> int:
        return len(self.quantities)

    def to_dict(self) -> dict:
        return {
            "schema": SCHEMA_VERSION,
            "input_hash": self.input_hash,
            "conventions": dict(self.conventions),
            "quantities": {k: q.to_dict() for k, q in self.quantities.items()},
        }



_CONVENTIONS = {
    "frame": "right-handed; x forward, y port, z up; baseline z = 0",
    "trim": "T_fwd - T_aft over l_pp, negative by the stern, about the midship waterline point",
    "LCB": "percent of L_wl, positive forward of midship, along the waterplane",
    "S": "physical (non-cap) submerged faces only",
    "extents": "L_wl, B_wl and half-breadths from physical faces only",
    "sections": "transverse planes normal to hull-frame x; end stations give the end face",
    "half_breadth": "max |y| of the physical submerged section at (x_i, height z_j above baseline)",
}



def _check_station(name: str, x: float, lo: np.ndarray, hi: np.ndarray, tol: float) -> float:
    x = float(x)
    if not math.isfinite(x) or not (lo[0] - tol <= x <= hi[0] + tol):
        raise MeshContractError(
            f"{name} station x = {x!r} m lies outside the hull [{lo[0]!r}, {hi[0]!r}]"
        )
    return min(max(x, float(lo[0])), float(hi[0]))


Quantity.__module__ = "digitalmodel.naval_architecture.mesh_hydrostatics"

HydrostaticsResult.__module__ = "digitalmodel.naval_architecture.mesh_hydrostatics"
