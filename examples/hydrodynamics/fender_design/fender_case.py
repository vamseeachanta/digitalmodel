"""Load a fender design case (YAML) and compute every result the workbook,
charts and reports use, from :mod:`digitalmodel.hydrodynamics.fender_design`.

A case file holds the ship at its berthing condition, the berth, the design
choices (method, factors, fenders in contact, base fender), the sweep ranges
with their basis, and paths to a fender catalogue CSV and optional curves.
Every value should carry its basis in the YAML comments or ``basis`` keys.
"""

from __future__ import annotations

import csv
from dataclasses import dataclass, field
from pathlib import Path

import yaml

from digitalmodel.hydrodynamics import fender_design as fd
from digitalmodel.hydrodynamics.berthing_energy import (
    FenderRating,
    fender_size_sensitivity,
    load_fender_catalogue,
)


@dataclass
class CaseResult:
    case: dict
    ship: fd.Ship
    berth: fd.Berth
    methods: list
    skipped: dict
    selected: fd.MethodResult
    catalogue: list
    base_fender: FenderRating
    capacity_knm: float
    design: fd.DesignPerformance
    curve: fd.PerformanceCurve | None
    sweeps: list
    bars: list
    sizes: list = field(default_factory=list)
    fcase: fd.Case | None = None
    sweeps_skipped: dict = field(default_factory=dict)
    perf_basis: dict = field(default_factory=dict)

    @property
    def reaction_factor(self) -> float:
        """Design reaction / rated reaction of the base fender (1 + tolerance, x any factors)."""
        return self.design.reaction_kn / self.base_fender.reaction_kn

    @property
    def energy_per_fender_knm(self) -> float:
        return self.selected.design_energy_knm / self.case["design"].get("fenders_in_contact", 1)


def load_curves(path) -> dict[str, fd.PerformanceCurve]:
    """Curves CSV: curve, deflection_pct, reaction_pct, energy_pct, source."""
    rows: dict[str, list] = {}
    src: dict[str, str] = {}
    with Path(path).open(newline="", encoding="utf-8") as fh:
        for r in csv.DictReader(fh):
            rows.setdefault(r["curve"], []).append(
                (float(r["deflection_pct"]), float(r["reaction_pct"]), float(r["energy_pct"]))
            )
            src[r["curve"]] = r["source"]
    return {k: fd.PerformanceCurve(k, tuple(sorted(v)), src[k]) for k, v in rows.items()}


def catalogue_bases(path) -> dict[str, str]:
    """fender_id -> performance basis (RPD, CV, GEA) from a catalogue CSV's
    optional ``perf_basis`` column; empty when the column is absent."""
    with Path(path).open(newline="", encoding="utf-8") as fh:
        reader = csv.DictReader(fh)
        if "perf_basis" not in (reader.fieldnames or []):
            return {}
        return {r["fender_id"].strip(): (r["perf_basis"] or "").strip() for r in reader}


def check_one_basis(fenders, bases: dict[str, str]) -> None:
    """Refuse to compare rows rated on different performance bases.

    Within one fender type every row must carry the same ``perf_basis``
    (a blank counts as its own value): a CV row and an RPD row of the same
    size are different ratings of one fender, and comparing them would mix
    bases. Raises ValueError naming the types and bases found.
    """
    if not bases:
        return
    by_type: dict[str, set] = {}
    for f in fenders:
        by_type.setdefault(f.fender_type, set()).add(bases.get(f.fender_id, ""))
    mixed = {t: sorted(b) for t, b in by_type.items() if len(b) > 1}
    if mixed:
        raise ValueError(
            "catalogue mixes performance bases within a fender type: "
            + "; ".join(f"{t}: {', '.join(b or '(blank)' for b in bs)}" for t, bs in sorted(mixed.items()))
            + ". Export one basis per type (import_vendor_tables.py --perf-basis)."
        )


def _resolve(base: Path, p: str | None) -> Path | None:
    if not p:
        return None
    q = Path(p)
    return q if q.is_absolute() else (base / q).resolve()


def load_case(path) -> dict:
    path = Path(path)
    case = yaml.safe_load(path.read_text(encoding="utf-8"))
    case["_dir"] = str(path.parent.resolve())
    return case


def compute(case: dict, catalogue_path: str | Path | None = None) -> CaseResult:
    base_dir = Path(case.get("_dir", "."))
    s, b, d = case["ship"], case["berth"], case["design"]
    ship = fd.Ship(**s)
    berth = fd.Berth(**b)
    fcase = fd.Case(ship, berth, **case.get("conditions", {}))
    kw = dict(
        velocity_m_s=d.get("velocity_m_s"),
        factor=d.get("factor"),
        factor_position=d.get("factor_position", "smallest"),
    )
    # citation sidecar: mode record (default), strict or off; wiki_root optional
    cit = case.get("citations") or {}
    ckw = dict(citations=cit.get("mode", "record"), citation_root=_resolve(base_dir, cit.get("wiki_root")))
    methods = fd.compare_methods(fcase, methods=d.get("methods"), **kw, **ckw)
    skipped = fd.compare_methods_skipped(fcase, methods=d.get("methods"), **kw, citations="off")
    # the selected method uses its own velocity and factor unless the case overrides them
    selected = fd.energy_by_method(fcase, method=d["method"], **kw, **ckw)

    cat_path = Path(catalogue_path) if catalogue_path else _resolve(base_dir, case["database"]["catalogue"])
    catalogue = load_fender_catalogue(cat_path)
    types = case["database"].get("fender_types")
    if types:
        catalogue = [f for f in catalogue if f.fender_type in types]
    bases = catalogue_bases(cat_path)
    check_one_basis(catalogue, bases)
    by_id = {f.fender_id: f for f in catalogue}
    base_fender = by_id[d["base_fender_id"]]
    design = fd.design_performance(
        base_fender,
        energy_tolerance=d.get("energy_tolerance", 0.0),
        reaction_tolerance=d.get("reaction_tolerance", 0.10),
        angular_factor=d.get("angular_factor", 1.0),
    )
    curve = None
    curves_path = _resolve(base_dir, case["database"].get("curves"))
    if curves_path and d.get("curve"):
        curve = load_curves(curves_path)[d["curve"]]
    ranges = {k: fd.SweepRange(v["low"], v["high"], v["basis"]) for k, v in case["sweeps"].items()}
    capacity = design.energy_knm / selected.capacity_divisor
    sweeps = fd.standard_sweeps(
        fcase, d["method"], base_fender, ranges,
        fenders_in_contact=d.get("fenders_in_contact", 1), capacity_knm=capacity,
        curve=curve, reaction_limit_kn=d.get("reaction_limit_kn"),
        hull_pressure_limit_kpa=d.get("hull_pressure_limit_kpa"),
        reaction_factor=design.reaction_kn / base_fender.reaction_kn, **kw,
    )
    excluded = fd.inapplicable_sweeps(d["method"])
    sweeps_skipped = {k: excluded[k] for k in ranges if k in excluded}
    bars = fd.tornado(sweeps)
    sizes = []
    ids = d.get("neighbour_ids")
    if ids:
        per = selected.design_energy_knm / d.get("fenders_in_contact", 1)
        sizes = fender_size_sensitivity(catalogue, base_fender.fender_id, per, fender_ids=ids)
    return CaseResult(case, ship, berth, methods, skipped, selected, catalogue, base_fender,
                      capacity, design, curve, sweeps, bars, sizes, fcase, sweeps_skipped,
                      {f.fender_id: bases.get(f.fender_id, "") for f in catalogue} if bases else {})
