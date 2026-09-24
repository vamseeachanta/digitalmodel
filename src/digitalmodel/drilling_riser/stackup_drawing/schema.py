"""Riser stack-up drawing schema (#2152).

:class:`StackupDrawingSpec` is the single interchange object between the data
adapters and the renderer / reconciler. It carries no project, client or
document-layout fields: every value is either a number, ``None`` (not
applicable) or the sentinel :data:`NOT_FOUND` (applicable but not available in
any source). Every known value may record its source and basis in
``provenance``.

Conventions
-----------
* Elevations are metres relative to MSL, positive up.
* Components are ordered top (drill floor) to bottom (below mudline).
* ``label`` is a plain noun phrase WITHOUT digits; every number shown on the
  drawing is generated from a typed field so it can be reconciled.
"""

from __future__ import annotations

import json
import re
from dataclasses import asdict, dataclass, field
from enum import Enum
from typing import Any, Optional, Union

__all__ = [
    "BASES",
    "NESTED_TYPES",
    "NOT_FOUND",
    "SCHEMA_VERSION",
    "ComponentType",
    "Datums",
    "Num",
    "Provenance",
    "ReferenceValue",
    "StackupComponent",
    "StackupDrawingSpec",
    "TensionerSystem",
    "TitleBlock",
    "not_found_fields",
]

SCHEMA_VERSION = "riser-stackup-drawing/1"
NOT_FOUND = "NOT_FOUND"

#: A numeric field: a float, ``NOT_FOUND`` (missing) or ``None`` (not applicable).
Num = Union[float, str, None]

#: Allowed provenance bases.
BASES = ("workbook", "report table", "derived", "synthetic", "adapter")


class ComponentType(str, Enum):
    """Fixed vocabulary of drawable stack-up components."""

    DIVERTER = "diverter"
    UPPER_FLEX_JOINT = "upper_flex_joint"
    TELESCOPIC_JOINT_OUTER = "telescopic_joint_outer"
    TELESCOPIC_JOINT_INNER = "telescopic_joint_inner"
    TENSION_RING = "tension_ring"
    RISER_JOINT_BARE = "riser_joint_bare"
    RISER_JOINT_BUOYANT = "riser_joint_buoyant"
    PUP_JOINT = "pup_joint"
    TERMINATION_JOINT = "termination_joint"
    LMRP = "lmrp"
    LOWER_FLEX_JOINT = "lower_flex_joint"
    BOP = "bop"
    TREE = "tree"
    TUBING_HANGER_SPOOL = "tubing_hanger_spool"
    WELLHEAD = "wellhead"
    CONDUCTOR = "conductor"
    CASING = "casing"
    OTHER = "other"


#: Types outside the stacked load path: casing is nested inside the conductor
#: and is not part of the stacked length sum.
NESTED_TYPES = frozenset({ComponentType.CASING})


@dataclass
class Provenance:
    """Where a value came from and on what basis."""

    source: str
    basis: str = "workbook"

    def __post_init__(self) -> None:
        if self.basis not in BASES:
            raise ValueError(f"unknown provenance basis {self.basis!r}")


@dataclass
class StackupComponent:
    """One drawn row of the stack-up (a single item or a run of joints)."""

    id: str
    type: ComponentType
    label: str
    count: Union[int, str]
    joint_length_m: Num
    top_el_m: Num
    bottom_el_m: Num
    od_in: Num = None
    buoyancy_od_in: Num = None
    wall_thickness_in: Num = None
    envelope_width_in: Num = None
    drag_diameter_in: Num = None
    buoyancy_depth_rating_ft: Num = None
    source: str = NOT_FOUND
    provenance: dict[str, Provenance] = field(default_factory=dict)
    notes: list[str] = field(default_factory=list)

    def __post_init__(self) -> None:
        self.type = ComponentType(self.type)
        if re.search(r"\d", self.label):
            raise ValueError(
                f"{self.id}: label must not contain digits (numbers are drawn "
                f"from typed fields): {self.label!r}"
            )

    def known(self, name: str) -> bool:
        """True when field ``name`` holds a value (not ``None``/``NOT_FOUND``)."""
        value = getattr(self, name)
        return value is not None and value != NOT_FOUND

    @property
    def length_m(self) -> Num:
        """Elevation span (top - bottom) or ``NOT_FOUND``."""
        if self.known("top_el_m") and self.known("bottom_el_m"):
            return float(self.top_el_m) - float(self.bottom_el_m)
        return NOT_FOUND


@dataclass
class Datums:
    """Vertical datums, metres relative to MSL."""

    water_depth_m: Num
    drill_floor_el_m: Num
    air_gap_m: Num
    mudline_el_m: Num
    msl_el_m: float = 0.0
    provenance: dict[str, Provenance] = field(default_factory=dict)


@dataclass
class TensionerSystem:
    """Riser tensioner lines (drawn as two representative sheaves)."""

    count: Union[int, str]
    sheave_el_m: Num
    sheave_radius_m: Num
    provenance: dict[str, Provenance] = field(default_factory=dict)


@dataclass
class TitleBlock:
    """Drawing title block. Keep it generic in public fixtures."""

    title: str
    configuration: str
    vessel_label: str = "Drilling vessel (DP)"
    document_ref: str = ""
    notes: list[str] = field(default_factory=list)


@dataclass
class ReferenceValue:
    """A total stated by the source itself, used by the closure checks."""

    value: Num
    source: str
    basis: str = "workbook"
    description: str = ""


@dataclass
class StackupDrawingSpec:
    """Everything the renderer draws and the reconciler checks."""

    spec_id: str
    datums: Datums
    components: list[StackupComponent]
    title_block: TitleBlock
    tensioner_system: Optional[TensionerSystem] = None
    #: Totals stated by the source itself, used by the reconciliation checks.
    reference_totals: dict[str, ReferenceValue] = field(default_factory=dict)
    sources: list[dict[str, str]] = field(default_factory=list)
    data_conflicts: list[dict[str, Any]] = field(default_factory=list)
    gaps: list[dict[str, str]] = field(default_factory=list)
    schema_version: str = SCHEMA_VERSION

    # -- validation -------------------------------------------------------------
    def validate(self) -> list[str]:
        """Structural problems (empty list = valid)."""
        problems: list[str] = []
        ids = [c.id for c in self.components]
        if len(ids) != len(set(ids)):
            problems.append("duplicate component ids")
        prev_top = None
        for comp in self.components:
            if comp.type in NESTED_TYPES:
                continue
            if comp.known("top_el_m") and comp.known("bottom_el_m"):
                if float(comp.top_el_m) < float(comp.bottom_el_m) - 1e-9:
                    problems.append(f"{comp.id}: top below bottom")
                if prev_top is not None and float(comp.top_el_m) > prev_top + 1e-6:
                    problems.append(f"{comp.id}: out of top-to-bottom order")
                prev_top = float(comp.top_el_m)
        return problems

    def component(self, component_id: str) -> StackupComponent:
        """Component by id (``KeyError`` when absent)."""
        for comp in self.components:
            if comp.id == component_id:
                return comp
        raise KeyError(component_id)

    # -- serialisation ----------------------------------------------------------
    def to_dict(self) -> dict[str, Any]:
        """Plain JSON-ready dict (enums as their values)."""

        def convert(obj: Any) -> Any:
            if isinstance(obj, Enum):
                return obj.value
            if isinstance(obj, dict):
                return {k: convert(v) for k, v in obj.items()}
            if isinstance(obj, list):
                return [convert(v) for v in obj]
            return obj

        return convert(asdict(self))

    def to_json(self) -> str:
        """Deterministic, indented JSON text with a trailing newline."""
        return json.dumps(self.to_dict(), indent=2, ensure_ascii=False) + "\n"

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> "StackupDrawingSpec":
        """Inverse of :meth:`to_dict`."""

        def prov(d: Optional[dict[str, Any]]) -> dict[str, Provenance]:
            return {k: Provenance(**v) for k, v in (d or {}).items()}

        datums = dict(data["datums"])
        datums["provenance"] = prov(datums.get("provenance"))
        comps = []
        for c in data["components"]:
            c = dict(c)
            c["provenance"] = prov(c.get("provenance"))
            comps.append(StackupComponent(**c))
        tens = data.get("tensioner_system")
        if tens is not None:
            tens = dict(tens)
            tens["provenance"] = prov(tens.get("provenance"))
            tens = TensionerSystem(**tens)
        return cls(
            spec_id=data["spec_id"],
            datums=Datums(**datums),
            components=comps,
            title_block=TitleBlock(**data["title_block"]),
            tensioner_system=tens,
            reference_totals={
                k: ReferenceValue(**v)
                for k, v in data.get("reference_totals", {}).items()
            },
            sources=list(data.get("sources", [])),
            data_conflicts=list(data.get("data_conflicts", [])),
            gaps=list(data.get("gaps", [])),
            schema_version=data.get("schema_version", SCHEMA_VERSION),
        )

    @classmethod
    def from_json(cls, text: str) -> "StackupDrawingSpec":
        """Inverse of :meth:`to_json`."""
        return cls.from_dict(json.loads(text))


def not_found_fields(spec: StackupDrawingSpec) -> list[str]:
    """Dotted paths of every ``NOT_FOUND`` value in the spec."""
    out: list[str] = []

    def walk(obj: Any, path: str) -> None:
        if isinstance(obj, dict):
            for k, v in obj.items():
                if k in ("provenance", "notes", "gaps", "data_conflicts", "sources"):
                    continue
                walk(v, f"{path}.{k}" if path else k)
        elif isinstance(obj, list):
            for i, v in enumerate(obj):
                key = v.get("id", i) if isinstance(v, dict) else i
                walk(v, f"{path}[{key}]")
        elif obj == NOT_FOUND:
            out.append(path)

    walk(spec.to_dict(), "")
    return out
