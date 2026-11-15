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
    methods = fd.compare_methods(fcase, methods=d.get("methods"), **kw)
    skipped = fd.compare_methods_skipped(fcase, methods=d.get("methods"), **kw)
    # the selected method uses its own velocity and factor unless the case overrides them
    selected = fd.energy_by_method(fcase, method=d["method"], **kw)

    cat_path = Path(catalogue_path) if catalogue_path else _resolve(base_dir, case["database"]["catalogue"])
    catalogue = load_fender_catalogue(cat_path)
    types = case["database"].get("fender_types")
    if types:
        catalogue = [f for f in catalogue if f.fender_type in types]
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
        hull_pressure_limit_kpa=d.get("hull_pressure_limit_kpa"), **kw,
    )
    bars = fd.tornado(sweeps)
    sizes = []
    ids = d.get("neighbour_ids")
    if ids:
        per = selected.design_energy_knm / d.get("fenders_in_contact", 1)
        sizes = fender_size_sensitivity(catalogue, base_fender.fender_id, per, fender_ids=ids)
    return CaseResult(case, ship, berth, methods, skipped, selected, catalogue, base_fender,
                      capacity, design, curve, sweeps, bars, sizes, fcase)
