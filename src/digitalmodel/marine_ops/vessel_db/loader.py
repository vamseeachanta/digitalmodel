"""
ABOUTME: Load and normalise the curated vessel database (data/vessels/raw/*.json).

The raw datasets were collected by web research with a "flag, don't fake"
contract (see data/vessels/README.md): every field value is a cited number, an
``estimated:<basis>`` string, or the literal ``"gap"``. Field NAMES vary across
records (``loa_m`` vs ``length_m``), so this module normalises them to the
canonical keys in SCHEMA.yaml and parses values tolerantly.

Public API
----------
- ``vessels_dir()``                 -> Path to data/vessels
- ``load_dataset(scope, layer)``    -> dict (one raw JSON file)
- ``iter_records(scope, layer)``    -> list[Record]
- ``Record``                        -> normalised view of one vessel record
- ``validate_provenance(...)``      -> list[ProvenanceViolation]
- ``load_crane_curves()``           -> dict[name, CraneCurve]  (installation models)
"""

from __future__ import annotations

import json
import re
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Iterable, Optional

# Canonical dimension key -> set of accepted source aliases (exact, lowercased).
_ALIASES: dict[str, set[str]] = {
    "loa": {"loa", "loa_m", "length_overall_m", "length_oa_m", "length_m", "loa_length_m"},
    "lbp": {"lbp", "lbp_m", "length_bp_m", "lpp_m", "lpp", "length_between_perpendiculars_m", "lwl_m"},
    "beam": {"beam", "beam_m", "breadth_m", "beam_moulded_m", "beam_wl_m", "breadth"},
    "depth": {"depth", "depth_m", "hull_depth_m", "depth_moulded_m", "moulded_depth_m"},
    "draft": {"draft", "draft_m", "draft_operating_m", "draft_design_m", "operating_draft_m",
              "draught_m", "draft_max_m", "draft_operating_max_m", "design_draft_m"},
    "draft_transit": {"draft_transit_m", "transit_draft_m"},
    "displacement": {"displacement", "displacement_t", "displacement_tonnes", "displacement_max_t",
                     "displacement_te", "full_load_displacement_t"},
    "cb": {"cb", "block_coefficient", "block_coefficient_cb"},
    "lcg": {"lcg", "lcg_m"},
    "vcg": {"vcg", "vcg_m"},
    "tcg": {"tcg", "tcg_m"},
    "kxx": {"kxx", "kxx_roll", "kxx_roll_m", "kxx_m", "roll_radius_of_gyration_m"},
    "kyy": {"kyy", "kyy_pitch", "kyy_pitch_m", "kyy_m", "pitch_radius_of_gyration_m"},
    "kzz": {"kzz", "kzz_yaw", "kzz_yaw_m", "kzz_m", "yaw_radius_of_gyration_m"},
}

# Reverse map for O(1) lookup.
_ALIAS_TO_CANON: dict[str, str] = {a: c for c, aliases in _ALIASES.items() for a in aliases}

_GAP = "gap"


def vessels_dir() -> Path:
    """Locate the data/vessels directory by walking up from this file."""
    here = Path(__file__).resolve()
    for parent in here.parents:
        cand = parent / "data" / "vessels"
        if cand.is_dir():
            return cand
    raise FileNotFoundError(
        "data/vessels not found by walking up from "
        f"{here}; set it up per data/vessels/README.md"
    )


# Short unit tokens allowed to trail a numeric value (so "320.0 m" parses but
# "9 headings", "1:50", "6750-TEU", "~26 (typical)" stay descriptive text).
_UNIT_TOKENS = {
    "m", "t", "te", "tonnes", "tonne", "deg", "m2", "m3", "kn", "knots",
    "ms", "mt", "kg", "mm", "cm", "s", "hz", "mw",
}


def parse_value(value: Any) -> tuple[Optional[float], str]:
    """Parse a field value strictly.

    Returns ``(number, marker)`` where ``marker`` is one of:
    ``"number"`` (a clean numeric, optionally with a known unit suffix),
    ``"estimated"``, ``"gap"``, or ``"text"`` (anything descriptive). A string
    that merely STARTS with a digit but continues with words (``"9 headings"``,
    ``"1:50"``, ``"6750-TEU ..."``) is ``"text"``, not a number -- so it is never
    treated as an un-cited hard number for provenance.
    """
    if isinstance(value, bool):
        return None, "text"
    if isinstance(value, (int, float)):
        return float(value), "number"
    if not isinstance(value, str):
        return None, "text"
    s = value.strip()
    if s == _GAP:
        return None, "gap"
    if s.lower().startswith("estimated:"):
        return None, "estimated"
    # Whole string must be a number, optionally led by ~≈±<>= and trailed by a
    # single known unit token.
    m = re.match(r"^[~≈±<>=\s]*([0-9][0-9,]*\.?[0-9]*)\s*([A-Za-z0-9²/]*)\s*$", s)
    if m:
        unit = m.group(2).lower().replace("²", "2").replace("/", "")
        if unit == "" or unit in _UNIT_TOKENS:
            try:
                return float(m.group(1).replace(",", "")), "number"
            except ValueError:
                pass
    return None, "text"


@dataclass
class Record:
    """A normalised view of one vessel record."""

    name: str
    scope: str
    layer: str
    vessel_type: str = ""
    owner_operator: str = ""
    year_built: str = ""
    raw_fields: dict[str, Any] = field(default_factory=dict)
    citations: list[dict] = field(default_factory=list)
    gaps: list[str] = field(default_factory=list)

    def vessel_id(self) -> str:
        base = re.sub(r"[^a-z0-9]+", "_", self.name.lower()).strip("_")
        return f"{self.scope}_{base}"[:80]

    def cited_fields(self) -> set[str]:
        """Union of all field names any citation claims to cover.

        Special tokens ``all``/``geometry`` mean "every field in this record".
        """
        out: set[str] = set()
        for c in self.citations:
            for f in c.get("fields", []):
                out.add(f)
        return out

    def dimension(self, canon: str) -> tuple[Optional[float], str, Optional[str]]:
        """Return ``(value, marker, source_field)`` for a canonical dimension."""
        for raw_name, raw_val in self.raw_fields.items():
            if _ALIAS_TO_CANON.get(raw_name.lower()) == canon:
                num, marker = parse_value(raw_val)
                return num, marker, raw_name
        return None, "missing", None

    def canonical_dimensions(self) -> dict[str, Optional[float]]:
        """All canonical dimensions present, mapped to their numeric value."""
        out: dict[str, Optional[float]] = {}
        for canon in _ALIASES:
            num, marker, _ = self.dimension(canon)
            if marker == "number":
                out[canon] = num
        return out


def load_dataset(scope: str, layer: str, base: Optional[Path] = None) -> dict:
    base = base or vessels_dir()
    path = base / "raw" / f"{scope}__{layer}.json"
    with open(path) as f:
        return json.load(f)


def iter_records(scope: str, layer: str, base: Optional[Path] = None) -> list[Record]:
    d = load_dataset(scope, layer, base)
    recs: list[Record] = []
    for r in d.get("records", []):
        recs.append(
            Record(
                name=r.get("name", "?"),
                scope=scope,
                layer=layer,
                vessel_type=r.get("vessel_type", ""),
                owner_operator=r.get("owner_operator", ""),
                year_built=str(r.get("year_built", "")),
                raw_fields=r.get("fields", {}),
                citations=r.get("citations", []),
                gaps=r.get("gaps", []),
            )
        )
    return recs


def datasets(base: Optional[Path] = None) -> list[tuple[str, str]]:
    """Discover available ``(scope, layer)`` datasets from raw/ filenames."""
    base = base or vessels_dir()
    out = []
    for p in sorted((base / "raw").glob("*__*.json")):
        scope, layer = p.stem.split("__", 1)
        out.append((scope, layer))
    return out


# ---------------------------------------------------------------------------
# Provenance integrity
# ---------------------------------------------------------------------------

# Fields that are identifiers/labels, not engineering quantities -- a bare number
# here is not a provenance defect (e.g. a count, a build year).
_NON_ENGINEERING = {"cranes_count", "accommodation_persons", "panels", "wave_periods"}
_ALWAYS_OK_SUFFIX = ("_count", "_persons")


@dataclass
class ProvenanceViolation:
    scope: str
    layer: str
    record: str
    field_name: str
    value: Any
    reason: str


# Decoration tokens stripped when reducing a field name to its quantity "stem",
# so a citation that groups by quantity (e.g. "Hs_m_by_RP") is recognised as
# covering the per-return-period field "Hs_100yr_m".
_STEM_DROP = {
    "m", "t", "te", "ms", "s", "deg", "m2", "m3", "kn", "yr", "ratio", "tonnes",
    "max", "min", "mean", "avg", "of", "gyration", "radius", "by", "rp",
    "return", "period", "spectral", "peak", "10m", "1hr", "3s", "omni",
}


def _stem(name: str) -> frozenset[str]:
    """Reduce a field name to its quantity tokens (units/RP/stats removed)."""
    n = name.lower()
    n = re.sub(r"\d+\s*yr", "", n)          # 100yr -> ""
    n = re.sub(r"by[_\s]*rp", "", n)        # by_RP -> ""
    parts = [p for p in re.split(r"[_\s]+", n) if p]
    keep = [p for p in parts if not p.isdigit() and p not in _STEM_DROP]
    return frozenset(keep)


def _is_covered(fname: str, cited: set[str], blanket: bool) -> bool:
    if blanket or fname in cited:
        return True
    fs = _stem(fname)
    if not fs:
        return True  # name reduced to pure decoration -> not a hard quantity
    for c in cited:
        cs = _stem(c)
        if not cs:
            continue
        # Covered if one stem subsumes the other (same quantity family).
        if fs <= cs or cs <= fs:
            return True
    return False


def validate_provenance(base: Optional[Path] = None) -> list[ProvenanceViolation]:
    """Find hard numeric fields that are neither cited nor marked estimated/gap.

    Enforces the README "flag, don't fake" rule across the whole database.
    Citation coverage is matched by exact name, a blanket ``all``/``geometry``
    token, or a shared quantity stem (so quantity-grouped citations count).
    A clean database returns an empty list.
    """
    base = base or vessels_dir()
    violations: list[ProvenanceViolation] = []
    for scope, layer in datasets(base):
        for rec in iter_records(scope, layer, base):
            cited = rec.cited_fields()
            blanket = bool(cited & {"all", "geometry"})
            for fname, fval in rec.raw_fields.items():
                num, marker = parse_value(fval)
                if marker != "number":
                    continue  # estimated / gap / text are all fine
                if fname in _NON_ENGINEERING or fname.lower().endswith(_ALWAYS_OK_SUFFIX):
                    continue
                if _is_covered(fname, cited, blanket):
                    continue
                violations.append(
                    ProvenanceViolation(
                        scope, layer, rec.name, fname, fval,
                        "numeric value not covered by any citation and not marked estimated/gap",
                    )
                )
    return violations


# ---------------------------------------------------------------------------
# Installation wiring: crane curves
# ---------------------------------------------------------------------------

# Field-name fragments that carry a crane SWL [te] anchor value.
_SWL_HINTS = ("crane_swl", "main_crane_swl", "main_hoist_swl", "tandem_swl",
              "lift_capacity", "crane_capacity", "topsides_lift", "max_lift")
_RADIUS_HINTS = ("radius", "outreach", "reach")


def _extract_anchor_points(fields: dict[str, Any]) -> tuple[list[float], list[float], float]:
    """Best-effort (radius_m, swl_te) anchor points from heterogeneous fields.

    Public sources rarely publish a full SWL-vs-radius curve, so we extract the
    discrete anchor points that ARE published (e.g. "10000 t at 48 m"). Returns
    ``(radii, capacities, max_swl)``. ``radii``/``capacities`` may be empty when
    only a headline SWL is available; ``max_swl`` is the largest single-crane SWL.
    """
    radii: list[float] = []
    caps: list[float] = []
    max_swl = 0.0
    for fname, fval in fields.items():
        lname = fname.lower()
        if any(h in lname for h in _SWL_HINTS):
            num, marker = parse_value(fval)
            if marker == "number":
                max_swl = max(max_swl, num)
                # Look for an "at <radius> m" inside a sibling descriptive string.
                if isinstance(fval, str):
                    rm = re.search(r"at\s+([0-9]+(?:\.[0-9]+)?)\s*m", fval)
                    if rm:
                        radii.append(float(rm.group(1)))
                        caps.append(num)
    # Pair any standalone capacity@radius descriptive strings.
    for fname, fval in fields.items():
        if isinstance(fval, str) and "at_radius" in fname.lower():
            cm = re.search(r"([0-9][0-9,]*)\s*t", fval)
            rm = re.search(r"([0-9]+(?:\.[0-9]+)?)\s*m", fval)
            if cm and rm:
                caps.append(float(cm.group(1).replace(",", "")))
                radii.append(float(rm.group(1)))
                max_swl = max(max_swl, caps[-1])
    return radii, caps, max_swl


def load_crane_curves(base: Optional[Path] = None) -> dict[str, Any]:
    """Build ``CraneCurve`` objects for installation vessels that publish SWL.

    Returns a dict ``{vessel_name: CraneCurve}``. Curves come from discrete
    published anchor points; where only a single (or no) radius is published the
    curve is a single point and ``capacity_at_radius`` returns the headline SWL.
    Vessels with no usable SWL are omitted.
    """
    from digitalmodel.marine_ops.installation.models import CraneCurve
    import numpy as np

    out: dict[str, Any] = {}
    for rec in iter_records("install", "crane_deck", base):
        radii, caps, max_swl = _extract_anchor_points(rec.raw_fields)
        if max_swl <= 0:
            continue
        if not radii:
            # No published radius -> single flat point at the headline SWL.
            radii, caps = [0.0], [max_swl]
        # Sort by radius and dedup.
        pts = sorted(set(zip(radii, caps)))
        r = np.array([p[0] for p in pts], dtype=float)
        c = np.array([p[1] for p in pts], dtype=float)
        out[rec.name] = CraneCurve(radii_m=r, capacities_te=c, max_hook_load_te=float(max_swl))
    return out


def normalize_vessel_name(name: str) -> str:
    """Collapse a vessel name to a dedup key across data sources.

    Strips common type prefixes (SSCV/DCV/DLV/MV…) and non-alphanumerics so
    ``SSCV Sleipnir``, ``Sleipnir`` and ``SLEIPNIR`` collide.
    """
    n = name.upper()
    n = re.sub(r"\b(SSCV|SS CV|DCV|DLV|DSV|HLV|CSV|MV|M/V|MS|SS)\b", "", n)
    n = re.sub(r"[^A-Z0-9]+", "", n)
    return n


def installation_vessels(
    base: Optional[Path] = None,
    *,
    include_wed: bool = True,
    include_curated: bool = True,
) -> dict[str, dict]:
    """Real installation crane vessels with crane curve + deck metadata.

    Unions two sources, deduped by :func:`normalize_vessel_name`:
    - **worldenergydata** construction vessels (source of truth — real IMO,
      published crane reach), via the WED adapter (skipped if WED not checked
      out), and
    - the in-repo web-research curated ``install/crane_deck`` records.

    worldenergydata wins on a name collision. Returns ``{display_name: {...}}``
    with an installation ``CraneCurve`` plus deck/DP context.
    """
    merged: dict[str, dict] = {}  # norm_name -> entry (entry carries display name)

    if include_curated:
        curves = load_crane_curves(base)
        for rec in iter_records("install", "crane_deck", base):
            if rec.name not in curves:
                continue
            f = rec.raw_fields

            def _num(*names):
                for n in names:
                    if n in f:
                        val, marker = parse_value(f[n])
                        if marker == "number":
                            return val
                return None

            merged[normalize_vessel_name(rec.name)] = {
                "_display": rec.name,
                "vessel_type": rec.vessel_type,
                "owner_operator": rec.owner_operator,
                "crane_curve": curves[rec.name],
                "tandem_swl_te": _num("tandem_swl_t", "tandem_swl_te", "combined_swl_t"),
                "deck_area_m2": _num("deck_area_m2", "free_deck_area_m2"),
                "deck_strength_t_per_m2": _num("deck_strength_t_per_m2", "deck_load_t_per_m2"),
                "dp_class": f.get("dp_class", ""),
                "source": "curated",
                "n_citations": len(rec.citations),
            }

    if include_wed:
        try:
            from digitalmodel.marine_ops.vessel_db.wed_adapter import (
                construction_crane_vessels,
            )
            for name, info in construction_crane_vessels().items():
                entry = dict(info)
                entry["_display"] = name
                merged[normalize_vessel_name(name)] = entry  # WED wins on collision
        except Exception:  # pragma: no cover - WED optional / import-safe
            pass

    return {v.pop("_display"): v for v in merged.values()}
