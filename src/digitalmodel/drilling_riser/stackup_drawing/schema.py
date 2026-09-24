"""Riser stack-up drawing schema (#2152, #2158).

:class:`StackupDrawingSpec` is the single interchange object between the data
adapters and the renderer / reconciler. It carries no project, client or
document-layout fields: every value is either a number, ``None`` (not
applicable) or the sentinel :data:`NOT_FOUND` (applicable but not available in
any source). Every known value may record its source and basis in
``provenance``.

Design data (#2158, owner instruction 2026-09-24): the drawing is
self-contained within the final report. Every drawn value references an item
of the report's DESIGN DATA table (:class:`DesignDataItem`, ``D-nn``) through
``Provenance.design_data_id``; a design-data item cites PUBLIC references
(:class:`Reference`, ``R-n``) where they exist, or is marked assumed ("no
public data"), or records an owner decision. ``Provenance.source`` and
``basis`` stay for backward compatibility and internal traceability; the
drawing never prints them.

Conventions
-----------
* Elevations are metres relative to MSL, positive up.
* Components are ordered top (drill floor) to bottom (below mudline).
* ``label`` is a plain noun phrase WITHOUT digits; every number shown on the
  drawing is generated from a typed field so it can be reconciled.
"""

from __future__ import annotations

import datetime
import json
import math
import re
import urllib.parse
from dataclasses import asdict, dataclass, field
from enum import Enum
from typing import Any, Optional, Union

__all__ = [
    "BASES",
    "DESIGN_DATA_UNITS",
    "NESTED_TYPES",
    "NOT_FOUND",
    "OWNER_DECISION_RE",
    "REFERENCE_KEYS",
    "REVIEW_ROW_KEYS",
    "SCHEMA_VERSION",
    "SOURCE_CLASSES",
    "ComponentType",
    "Datums",
    "DesignDataItem",
    "Num",
    "Provenance",
    "Reference",
    "ReferenceValue",
    "StackupComponent",
    "StackupDrawingSpec",
    "TensionerSystem",
    "TitleBlock",
    "not_found_fields",
    "open_review_entries",
    "valid_basis",
]

SCHEMA_VERSION = "riser-stackup-drawing/1"
NOT_FOUND = "NOT_FOUND"

#: A numeric field: a float, ``NOT_FOUND`` (missing) or ``None`` (not applicable).
Num = Union[float, str, None]

#: Allowed provenance bases. An owner decision is recorded as
#: ``"owner decision <ID> (<YYYY-MM-DD>)"`` (see :data:`OWNER_DECISION_RE`);
#: ``"assumed"`` marks a value the drawing states but no source establishes.
BASES = ("workbook", "report table", "derived", "synthetic", "adapter", "assumed")
#: An owner basis decision, e.g. ``"owner decision K05 (2026-09-24)"`` (#2158).
OWNER_DECISION_RE = re.compile(r"owner decision [A-Z]\d{2} \(\d{4}-\d{2}-\d{2}\)")


def valid_basis(basis: Any) -> bool:
    """True for a listed basis or a well-formed owner decision."""
    return isinstance(basis, str) and (
        basis in BASES or OWNER_DECISION_RE.fullmatch(basis) is not None
    )


#: Totals and space-out values a source may state (#2158). Each may also be
#: carried as a ``<key>_workbook`` copy of the workbook's own figure.
_REFERENCE_BASE_KEYS = (
    "stackup_length_m",
    "stackup_length_ft",
    "rkb_node_above_mudline_m",
    "riser_length_ufj_lfj_m",
    "closure_residual_m",
    "closure_residual_geometric_m",
    "static_stretch_m",
    "tj_ib_geometric_m",
    "tj_ib_tensioned_m",
    "tj_ib_mid_stroke_m",
    "tj_offset_from_mid_m",
    "ufj_pivot_el_m",
    "lfj_pivot_el_m",
)
REFERENCE_KEYS = frozenset(
    _REFERENCE_BASE_KEYS + tuple(f"{k}_workbook" for k in _REFERENCE_BASE_KEYS)
)
#: Review-entry ``component_id`` values that key a non-component table row.
REVIEW_ROW_KEYS = ("datums", "tensioner_system")

#: Design-data source classes and the one-letter flag the drawing prints.
SOURCE_CLASSES = {"public": "P", "owner_decision": "D", "assumed": "A"}
#: Units a design-data item may state its value in.
DESIGN_DATA_UNITS = ("m", "ft", "in", "count", "-")
_DD_ID_RE = re.compile(r"D-\d{2,3}")
_REF_ID_RE = re.compile(r"R-\d{1,3}")
_DATE_RE = re.compile(r"\d{4}-\d{2}-\d{2}")


def _parse_date(text: Any) -> Optional[datetime.date]:
    """A real calendar date written YYYY-MM-DD, else ``None``."""
    if not isinstance(text, str) or not _DATE_RE.fullmatch(text):
        return None
    try:
        return datetime.date.fromisoformat(text)
    except ValueError:
        return None


def _usable_url(url: Any) -> bool:
    """http(s) URL whose host name is non-empty and dotted (no fetch)."""
    if not isinstance(url, str) or any(ch.isspace() for ch in url):
        return False
    try:
        parts = urllib.parse.urlsplit(url)
        host = parts.hostname
    except ValueError:
        return False
    return parts.scheme in ("http", "https") and bool(host) and "." in host.strip(".")


_DECISION_ID_RE = re.compile(r"\b[A-Z]\d{2}\b")
#: Exact label an assumed item's note starts with (owner decision DD01,
#: 2026-09-24; plain hyphen), followed by why no public data exists.
ASSUMED_LABEL = "ASSUMED - to be confirmed"
#: Phrase an assumed item's note must carry.
NO_PUBLIC_DATA = "no public data"


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
    #: The report design-data item (``D-nn``) that states this value.
    design_data_id: Optional[str] = None

    def __post_init__(self) -> None:
        if not valid_basis(self.basis):
            raise ValueError(f"unknown provenance basis {self.basis!r}")


@dataclass
class Reference:
    """A public reference cited by the report's design-data table."""

    id: str
    citation: str
    url: str
    retrieved: str
    #: ``"source"`` states the value; ``"context"`` only frames an assumption.
    role: str = "source"


@dataclass
class DesignDataItem:
    """One row of the report's DESIGN DATA table.

    ``value`` is the stated value in ``unit``. A composite item (e.g.
    "surface equipment dimensions") states one value per backed field in
    ``values``: ``{spec_path: {"value": v, "unit": u}}`` with paths such as
    ``components.c01-diverter.od_in``, ``datums.water_depth_m``,
    ``tensioner_system.count`` or ``reference_totals.stackup_length_m``. A
    field whose item states neither is not verifiable: the reconciler
    reports it ``not_established`` (review r1 finding 1).

    ``reference_ids`` cites references either as plain ids (the citation
    takes the reference's own ``role``) or as ``{"id": ..., "role":
    "source" | "context"}``. ``source_class`` is ``public`` (at least one
    resolved ``source`` citation), ``owner_decision`` (the note names the
    decision ID) or ``assumed`` (note starts with the DD01 label and says why
    there is no public data; context citations only).
    """

    id: str
    parameter: str
    value: Optional[float]
    unit: str
    source_class: str
    reference_ids: list[Any] = field(default_factory=list)
    note: str = ""
    values: dict[str, dict[str, Any]] = field(default_factory=dict)

    def citations(self, refs: dict[str, "Reference"]) -> list[tuple[Any, Any]]:
        """``(reference id, role)`` per citation; a plain id takes the reference's role."""
        out = []
        for cit in self.reference_ids:
            if isinstance(cit, dict):
                out.append((cit.get("id"), cit.get("role")))
            else:
                ref = refs.get(cit) if isinstance(cit, str) else None
                out.append((cit, ref.role if ref is not None else None))
        return out


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
    #: Explicit nesting relation: the id of a host component (#2158).
    #:
    #: * overlay - the span lies wholly inside the host span (e.g. a flex
    #:   joint inside the LMRP envelope): drawn after the host, not stacked
    #:   and not counted in the length sum;
    #: * landing - the component stays in the stacked chain and only its
    #:   overlap with the named host is excused (e.g. a wellhead housing
    #:   inside the conductor stick-up).
    nested_in: Optional[str] = None

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
    #: Internal (archive) document reference: kept for private provenance only,
    #: never printed on the drawing (#2158).
    document_ref: str = ""
    notes: list[str] = field(default_factory=list)
    #: The report's own document number, printed in the title block; ``None``
    #: prints "n/a" (#2158).
    report_document_no: Optional[str] = None


@dataclass
class ReferenceValue:
    """A total stated by the source itself, used by the closure checks."""

    value: Num
    source: str
    basis: str = "workbook"
    description: str = ""
    #: The report design-data item (``D-nn``) that states this value.
    design_data_id: Optional[str] = None

    def __post_init__(self) -> None:
        if not valid_basis(self.basis):
            raise ValueError(f"unknown reference basis {self.basis!r}")


_DATUM_FIELDS = (
    "water_depth_m",
    "drill_floor_el_m",
    "air_gap_m",
    "mudline_el_m",
    "msl_el_m",
)
_COMPONENT_FIELDS = (
    "count",
    "joint_length_m",
    "top_el_m",
    "bottom_el_m",
    "od_in",
    "buoyancy_od_in",
    "wall_thickness_in",
    "envelope_width_in",
    "drag_diameter_in",
    "buoyancy_depth_rating_ft",
)
_TENSIONER_FIELDS = ("count", "sheave_el_m", "sheave_radius_m")
#: Provenance keys a component may carry: its fields, the flex-joint pivot
#: and the nesting relation (#2158).
_COMPONENT_PROV_KEYS = frozenset(_COMPONENT_FIELDS + ("pivot_el_m", "nested_in"))


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
    gaps: list[dict[str, Any]] = field(default_factory=list)
    #: The report's design-data table and its public references (#2158).
    design_data: list[DesignDataItem] = field(default_factory=list)
    references: list[Reference] = field(default_factory=list)
    #: Date the spec's data stands at (YYYY-MM-DD); no reference may be
    #: retrieved after it. Optional.
    as_of: Optional[str] = None
    schema_version: str = SCHEMA_VERSION

    # -- nesting ----------------------------------------------------------------
    def is_overlay(self, comp: StackupComponent) -> bool:
        """True when ``comp.nested_in`` names a host whose span contains ``comp``."""
        if not comp.nested_in:
            return False
        try:
            host = self.component(comp.nested_in)
        except KeyError:
            return False
        if not all(
            x.known(f) for x in (comp, host) for f in ("top_el_m", "bottom_el_m")
        ):
            return False
        return (
            float(host.bottom_el_m) - 1e-9 <= float(comp.bottom_el_m)
            and float(comp.top_el_m) <= float(host.top_el_m) + 1e-9
        )

    def stacked(self) -> list[StackupComponent]:
        """The load-path chain: no NESTED_TYPES and no overlay components."""
        return [
            c
            for c in self.components
            if c.type not in NESTED_TYPES and not self.is_overlay(c)
        ]

    # -- validation -------------------------------------------------------------
    def validate(self) -> list[str]:
        """Structural problems (empty list = valid).

        Every numeric field must be a finite number, ``NOT_FOUND`` or ``None``;
        NaN and +-inf are rejected before any ordering arithmetic. Nesting
        hosts, provenance keys, reference keys and review-entry
        ``component_id`` values must name something that exists.
        """
        problems = self.non_finite_fields()
        if problems:
            return problems
        ids = [c.id for c in self.components]
        if len(ids) != len(set(ids)):
            problems.append("duplicate component ids")
        for comp in self.components:
            if comp.nested_in is not None and (
                comp.nested_in not in ids or comp.nested_in == comp.id
            ):
                problems.append(
                    f"{comp.id}: nested_in {comp.nested_in!r} is not another component"
                )
            for key in comp.provenance:
                if key not in _COMPONENT_PROV_KEYS:
                    problems.append(f"{comp.id}: unknown provenance key {key!r}")
        for key in self.datums.provenance:
            if key not in _DATUM_FIELDS:
                problems.append(f"datums: unknown provenance key {key!r}")
        if self.tensioner_system is not None:
            for key in self.tensioner_system.provenance:
                if key not in _TENSIONER_FIELDS:
                    problems.append(f"tensioner_system: unknown provenance key {key!r}")
        for key in self.reference_totals:
            if key not in REFERENCE_KEYS:
                problems.append(f"unknown reference_totals key {key!r}")
        # a review entry targets a row that exists: a component, the drawn
        # drill-floor datum, or the tensioner system when there is one;
        # drawing-level entries use component_id null (review r1 finding 4)
        row_keys = set(ids)
        if self.datums.drill_floor_el_m not in (None, NOT_FOUND):
            row_keys.add("datums")
        if self.tensioner_system is not None:
            row_keys.add("tensioner_system")
        for kind in ("data_conflicts", "gaps"):
            for i, entry in enumerate(getattr(self, kind)):
                if not isinstance(entry, dict):
                    problems.append(f"{kind}[{i}] is not an object")
                    continue
                cid = entry.get("component_id")
                if cid is not None and cid not in row_keys:
                    problems.append(
                        f"{kind}[{i}]: component_id {cid!r} names no table row "
                        "(use null for a drawing-level entry)"
                    )
        problems.extend(self.design_data_problems())
        if problems:
            return problems
        prev_top = None
        for comp in self.stacked():
            if comp.known("top_el_m") and comp.known("bottom_el_m"):
                if float(comp.top_el_m) < float(comp.bottom_el_m) - 1e-9:
                    problems.append(f"{comp.id}: top below bottom")
                if prev_top is not None and float(comp.top_el_m) > prev_top + 1e-6:
                    problems.append(f"{comp.id}: out of top-to-bottom order")
                prev_top = float(comp.top_el_m)
        return problems

    def design_data_problems(self) -> list[str]:
        """Register rules: ids, references, source classes and resolution."""
        out: list[str] = []
        refs = {}
        as_of = _parse_date(self.as_of) if self.as_of is not None else None
        if self.as_of is not None and as_of is None:
            out.append(f"as_of {self.as_of!r} is not a calendar date YYYY-MM-DD")
        for r in self.references:
            if not _REF_ID_RE.fullmatch(str(r.id)):
                out.append(f"reference id {r.id!r} is not R-<n>")
            if r.id in refs:
                out.append(f"duplicate reference id {r.id!r}")
            refs[r.id] = r
            # structure only: resolution of an id is not a check that the
            # reference is reachable, and URLs are never fetched
            if not _usable_url(r.url):
                out.append(
                    f"{r.id}: url {r.url!r} is not an http(s) URL with a host name"
                )
            retrieved = _parse_date(r.retrieved)
            if retrieved is None:
                out.append(
                    f"{r.id}: retrieved {r.retrieved!r} is not a calendar date YYYY-MM-DD"
                )
            elif as_of is not None and retrieved > as_of:
                out.append(
                    f"{r.id}: retrieved {r.retrieved} is after the spec date {self.as_of}"
                )
            if r.role not in ("source", "context"):
                out.append(f"{r.id}: role {r.role!r} is not source or context")
            if not str(r.citation).strip():
                out.append(f"{r.id}: empty citation")
        items: dict[str, DesignDataItem] = {}
        for it in self.design_data:
            if not _DD_ID_RE.fullmatch(str(it.id)):
                out.append(f"design data id {it.id!r} is not D-<nn>")
            if it.id in items:
                out.append(f"duplicate design data id {it.id!r}")
            items[it.id] = it
            if it.source_class not in SOURCE_CLASSES:
                out.append(
                    f"{it.id}: source_class {it.source_class!r} is not one of "
                    f"{sorted(SOURCE_CLASSES)}"
                )
            if it.unit not in DESIGN_DATA_UNITS:
                out.append(
                    f"{it.id}: unit {it.unit!r} is not one of {DESIGN_DATA_UNITS}"
                )
            v = it.value
            if v is not None and (
                isinstance(v, bool)
                or not isinstance(v, (int, float))
                or not math.isfinite(v)
            ):
                out.append(f"{it.id}: value {v!r} is not a finite number or null")
            if not str(it.parameter).strip():
                out.append(f"{it.id}: empty parameter")
            cites = it.citations(refs)
            for rid, role in cites:
                if not isinstance(rid, str) or rid not in refs:
                    out.append(f"{it.id}: reference {rid!r} does not resolve")
                elif role not in ("source", "context"):
                    out.append(
                        f"{it.id}: citation of {rid} has role {role!r}, not source "
                        "or context"
                    )
            resolved = [
                (rid, role)
                for rid, role in cites
                if isinstance(rid, str) and rid in refs
            ]
            note = str(it.note or "")
            if it.source_class == "public" and not any(
                role == "source" for _, role in resolved
            ):
                out.append(
                    f"{it.id}: a public item must cite at least one resolved source "
                    "reference (context citations do not state the value)"
                )
            elif it.source_class == "assumed":
                if not note.startswith(ASSUMED_LABEL):
                    out.append(
                        f"{it.id}: an assumed item's note must start with "
                        f"{ASSUMED_LABEL!r} (owner decision DD01)"
                    )
                if NO_PUBLIC_DATA not in note.lower():
                    out.append(
                        f"{it.id}: an assumed item's note must say why "
                        f"('{NO_PUBLIC_DATA}')"
                    )
                for rid, role in resolved:
                    if role != "context":
                        out.append(
                            f"{it.id}: an assumed item may cite context "
                            f"references only, not {rid} as {role!r}"
                        )
            elif it.source_class == "owner_decision" and not _DECISION_ID_RE.search(
                note
            ):
                out.append(
                    f"{it.id}: an owner_decision item's note must name the decision ID"
                )
        backs: dict[str, str] = {}
        for path, prov in self._provenance_entries():
            did = prov.design_data_id
            if did is not None and did not in items:
                out.append(f"{path}: design_data_id {did!r} does not resolve")
            elif did is not None:
                backs[path] = did
        for it in self.design_data:
            if not isinstance(it.values, dict):
                out.append(f"{it.id}: values is not an object")
                continue
            for path, entry in it.values.items():
                if backs.get(path) != it.id:
                    out.append(
                        f"{it.id}: values key {path!r} does not back a field of this item"
                    )
                ok = (
                    isinstance(entry, dict)
                    and set(entry) == {"value", "unit"}
                    and isinstance(entry["value"], (int, float))
                    and not isinstance(entry["value"], bool)
                    and math.isfinite(entry["value"])
                    and entry["unit"] in DESIGN_DATA_UNITS
                    and entry["unit"] != "-"
                )
                if not ok:
                    out.append(
                        f"{it.id}: values[{path!r}] is not {{value: <finite number>, "
                        f"unit: one of {DESIGN_DATA_UNITS[:-1]}}}"
                    )
        return out

    def _provenance_entries(self):
        """``(path, Provenance-like)`` for every entry that can carry a D-ID."""
        for key, p in self.datums.provenance.items():
            yield f"datums.{key}", p
        if self.tensioner_system is not None:
            for key, p in self.tensioner_system.provenance.items():
                yield f"tensioner_system.{key}", p
        for comp in self.components:
            for key, p in comp.provenance.items():
                yield f"components.{comp.id}.{key}", p
        for key, ref in self.reference_totals.items():
            yield f"reference_totals.{key}", ref

    def design_item(self, item_id: Optional[str]) -> Optional[DesignDataItem]:
        """Design-data item by id, or ``None``."""
        return next((it for it in self.design_data if it.id == item_id), None)

    def non_finite_fields(self) -> list[str]:
        """Numeric fields that are neither finite, ``NOT_FOUND`` nor ``None``."""
        out: list[str] = []

        def check(path: str, value: Any) -> None:
            if value is None or value == NOT_FOUND:
                return
            if isinstance(value, bool) or not isinstance(value, (int, float)):
                out.append(f"{path}={value!r} is not a number")
            elif not math.isfinite(value):
                out.append(f"{path}={value!r} is not finite")

        for name in _DATUM_FIELDS:
            check(f"datums.{name}", getattr(self.datums, name))
        for comp in self.components:
            for name in _COMPONENT_FIELDS:
                check(f"components[{comp.id}].{name}", getattr(comp, name))
        if self.tensioner_system is not None:
            for name in _TENSIONER_FIELDS:
                check(f"tensioner_system.{name}", getattr(self.tensioner_system, name))
        for key, ref in self.reference_totals.items():
            check(f"reference_totals.{key}.value", ref.value)
        return out

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
        ids = {c.id for c in comps} | set(REVIEW_ROW_KEYS)
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
            data_conflicts=_with_component_id(data.get("data_conflicts", []), ids),
            gaps=_with_component_id(data.get("gaps", []), ids),
            design_data=[DesignDataItem(**d) for d in data.get("design_data", [])],
            references=[Reference(**r) for r in data.get("references", [])],
            as_of=data.get("as_of"),
            schema_version=data.get("schema_version", SCHEMA_VERSION),
        )

    @classmethod
    def from_json(cls, text: str) -> "StackupDrawingSpec":
        """Inverse of :meth:`to_json`."""
        return cls.from_dict(json.loads(text))


def _with_component_id(entries: Any, ids: set[str]) -> list[Any]:
    """Compatibility read of review entries written before #2158.

    An entry without ``component_id`` gets one from the first word of its
    ``item`` when that word is a component id (or a row key such as
    ``tensioner_system``), else ``None`` (a drawing-level entry). Entries that
    carry the key are kept as written.
    """
    out: list[Any] = []
    for entry in entries or []:
        if isinstance(entry, dict) and "component_id" not in entry:
            words = str(entry.get("item", "")).split()
            first = words[0] if words else ""
            entry = {"component_id": first if first in ids else None, **entry}
        out.append(entry)
    return out


def open_review_entries(spec: StackupDrawingSpec) -> list[tuple[str, int, dict]]:
    """``(kind, index, entry)`` of every open conflict and gap, conflicts first.

    An entry is open unless its ``status`` says otherwise: a missing status,
    or one that starts with ``"open"``, is open.
    """
    out = []
    for kind in ("data_conflicts", "gaps"):
        for i, entry in enumerate(getattr(spec, kind)):
            if not isinstance(entry, dict):
                continue
            status = str(entry.get("status", "open")).strip().lower()
            if status.startswith("open"):
                out.append((kind, i, entry))
    return out


def not_found_fields(spec: StackupDrawingSpec) -> list[str]:
    """Dotted paths of every ``NOT_FOUND`` value in the spec."""
    out: list[str] = []

    def walk(obj: Any, path: str) -> None:
        if isinstance(obj, dict):
            for k, v in obj.items():
                if k in (
                    "provenance",
                    "notes",
                    "gaps",
                    "data_conflicts",
                    "sources",
                    "design_data",
                    "references",
                ):
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
