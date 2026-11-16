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


@dataclass(frozen=True)
class SizeCheck:
    """One neighbouring size with the case's design adjustments applied.

    ``capacity_knm`` = rated energy x (1 - energy tolerance) x angular factor
    / the selected method's capacity divisor (WG211 gamma_m), the same basis
    as the base fender's capacity. ``reaction_kn`` and ``hull_pressure_kpa``
    are the rated-deflection values x (1 + reaction tolerance), compared with
    the limits as given. For a row of the case curve's fender type the
    reaction is also multiplied by ``reaction_peak_factor``
    (:func:`~digitalmodel.hydrodynamics.fender_design.rated_peak_factor`):
    where the curve exceeds the rated reaction before the rated point, the
    rated value is not the largest reaction reached. A check without a limit
    is None. A row without a
    hull pressure fails a set pressure limit (``meets_pressure`` False,
    :attr:`pressure_missing`), because compliance cannot be shown; this is
    how :func:`select_fenders` excludes the same row.
    """

    fender_id: str
    is_base: bool
    rated_energy_knm: float
    capacity_knm: float
    utilisation: float
    rated_reaction_kn: float
    reaction_kn: float
    hull_pressure_kpa: float | None
    meets_energy: bool
    meets_reaction: bool | None
    meets_pressure: bool | None
    reaction_peak_factor: float = 1.0

    @property
    def meets_all(self) -> bool:
        """Every check with a limit is met (a check without a limit is None and not counted)."""
        return self.meets_energy and self.meets_reaction is not False and self.meets_pressure is not False

    @property
    def pressure_missing(self) -> bool:
        """A hull-pressure limit is set but the row gives no hull pressure."""
        return self.meets_pressure is False and self.hull_pressure_kpa is None


def type_tolerances(design: dict, fender_type: str) -> tuple[float, float]:
    """(energy tolerance, reaction tolerance) for a fender of ``fender_type``.

    The design block's ``energy_tolerance`` and ``reaction_tolerance`` belong
    to the base fender's type; ``tolerances_by_type`` gives every other type
    in the catalogue (performance tolerances differ by type: TFADM p. 76
    gives moulded rubber +/-10 %, extruded +/-20 %, foam +/-10 %, pneumatic
    +/-10 % on reaction and energy; SFT p. 26 gives pneumatic 0 % on energy
    for guaranteed energy absorption, GEA). A type without an entry takes
    the base values; the base values default to 0.10 (energy) and 0.10
    (reaction), the pneumatic reading that gives the lower capacity;
    :func:`check_type_tolerances` refuses a catalogue in which a type other
    than the base fender's has none.
    """
    t = (design.get("tolerances_by_type") or {}).get(fender_type)
    if t is None:
        return design.get("energy_tolerance", 0.10), design.get("reaction_tolerance", 0.10)
    return t["energy_tolerance"], t["reaction_tolerance"]


def check_type_tolerances(fenders, base_type: str, design: dict) -> None:
    """Refuse fender types other than ``base_type`` without their own
    tolerances (``design["tolerances_by_type"]``), and an entry for the base
    type itself (the base type's tolerances are the design block's own, the
    workbook's live ETOL and RTOL). Raises ValueError naming the types."""
    by_type = design.get("tolerances_by_type") or {}
    if base_type in by_type:
        raise ValueError(f"tolerances_by_type names the base fender's type {base_type!r}; its tolerances are "
                         "design.energy_tolerance and design.reaction_tolerance")
    for t, v in by_type.items():
        if set(v) != {"energy_tolerance", "reaction_tolerance"}:
            raise ValueError(f"tolerances_by_type[{t!r}] needs energy_tolerance and reaction_tolerance only")
    missing = sorted({f.fender_type for f in fenders} - {base_type} - set(by_type))
    if missing:
        raise ValueError(
            f"no tolerances for fender type(s) {', '.join(missing)} (base type {base_type}): give "
            "design.tolerances_by_type for every other type, since manufacturing tolerances differ by type")


def size_checks(fenders, base_id: str, energy_per_fender_knm: float, design: dict,
                capacity_divisor: float = 1.0, curve: fd.PerformanceCurve | None = None,
                curve_type: str | None = None) -> list[SizeCheck]:
    """Check each fender in ``fenders`` against the case demand and limits,
    with the configured tolerances (by fender type, :func:`type_tolerances`),
    angular factor and capacity divisor (``design`` is the case's ``design``
    block). When the base row is among ``fenders``, a row of another type
    without its own tolerances is refused (:func:`check_type_tolerances`). ``curve`` is the normalised
    curve of fender type ``curve_type``; the reaction of a row of that type
    carries the curve's peak up to the rated point (``rated_peak_factor``)."""
    peak = fd.rated_peak_factor(curve)
    r_lim = fd.check_limit("reaction_limit_kn", design.get("reaction_limit_kn"))
    p_lim = fd.check_limit("hull_pressure_limit_kpa", design.get("hull_pressure_limit_kpa"))
    base = next((f for f in fenders if f.fender_id == base_id), None)
    if base is not None:
        check_type_tolerances(fenders, base.fender_type, design)
    out = []
    for f in fenders:
        et, rt = type_tolerances(design, f.fender_type)
        dp = fd.design_performance(
            f, energy_tolerance=et, reaction_tolerance=rt,
            angular_factor=design.get("angular_factor", 1.0),
        )
        cap = dp.energy_knm / capacity_divisor
        rf = dp.reaction_kn / f.reaction_kn
        k = peak if curve is not None and f.fender_type == curve_type else 1.0
        r = dp.reaction_kn * k
        # hull pressure stays the rated-deflection value x the tolerance (not scaled by the peak)
        p = None if f.hull_pressure_kpa is None else f.hull_pressure_kpa * rf
        out.append(SizeCheck(
            fender_id=f.fender_id, is_base=f.fender_id == base_id,
            rated_energy_knm=f.rated_energy_knm, capacity_knm=cap,
            utilisation=energy_per_fender_knm / cap,
            rated_reaction_kn=f.reaction_kn, reaction_kn=r, hull_pressure_kpa=p,
            meets_energy=cap >= energy_per_fender_knm,
            meets_reaction=None if r_lim is None else r <= r_lim,
            # a set limit with no datum is not met: compliance cannot be shown (as select_fenders)
            meets_pressure=None if p_lim is None else (p is not None and p <= p_lim),
            reaction_peak_factor=k,
        ))
    return out


def size_basis(res) -> str:
    """One sentence: the adjustments applied to every neighbouring size."""
    d = res.case["design"]
    div = res.selected.capacity_divisor
    k = res.reaction_peak_factor(res.base_fender)
    others = d.get("tolerances_by_type") or {}
    other = "".join(f"; {t} rows {v['energy_tolerance']:.2f}" for t, v in others.items())
    other_r = "".join(f"; {t} rows {v['reaction_tolerance']:.2f}" for t, v in others.items())
    return (f"Energy: rated x (1 - energy tolerance: {res.base_fender.fender_type} rows "
            f"{d.get('energy_tolerance', 0.10):.2f}{other}) x angular {d.get('angular_factor', 1.0):.2f}"
            + (f" / {div:.2f} ({res.selected.label} capacity divisor)" if div != 1.0 else "")
            + f". Reaction and hull pressure: rated-deflection value x (1 + reaction tolerance: "
              f"{res.base_fender.fender_type} rows {d.get('reaction_tolerance', 0.10):.2f}{other_r})."
            + (f" Reaction of {res.base_fender.fender_type} rows also x {k:.3f}, the curve's largest reaction "
               "before the rated point over the rated reaction." if k != 1.0 else ""))


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
    size_checks: list = field(default_factory=list)
    citation_mode: str = "strict"

    @property
    def reaction_factor(self) -> float:
        """Design reaction / rated reaction of the base fender (1 + tolerance, x any factors)."""
        return self.design.reaction_kn / self.base_fender.reaction_kn

    def reaction_peak_factor(self, fender: FenderRating) -> float:
        """Multiplier on the rated-deflection reaction of ``fender``: the case
        curve's peak up to the rated point over the rated reaction for a row of
        the base fender's type (the curve's type), else 1."""
        if self.curve is None or fender.fender_type != self.base_fender.fender_type:
            return 1.0
        return fd.rated_peak_factor(self.curve)

    @property
    def energy_per_fender_knm(self) -> float:
        return self.selected.design_energy_knm / self.case["design"].get("fenders_in_contact", 1)

    def method_capacity_knm(self, result: fd.MethodResult) -> float:
        """Design capacity of the base fender per fender under ``result``'s
        method: rated x (1 - energy tolerance) x angular factor / that
        method's capacity divisor (WG211 gamma_m; 1.0 elsewhere)."""
        return self.design.energy_knm / result.capacity_divisor

    def method_utilisation(self, result: fd.MethodResult) -> float:
        """Energy per fender of ``result`` over its own method's design capacity."""
        n = self.case["design"].get("fenders_in_contact", 1)
        return result.design_energy_knm / n / self.method_capacity_knm(result)


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


def compute(case: dict, catalogue_path: str | Path | None = None, citations: str | None = None) -> CaseResult:
    """Every result of a case.

    ``citations`` is the citation mode of the method results: ``strict``
    (default) raises when a cited wiki page is missing or no wiki is
    configured; ``record`` is the explicit opt-out that records each
    unresolved citation and flags the result. The argument overrides the
    case file's ``citations: {mode: ...}``; the mode used is kept in
    :attr:`CaseResult.citation_mode` for the report.
    """
    base_dir = Path(case.get("_dir", "."))
    s, b, d = case["ship"], case["berth"], case["design"]
    # design inputs checked before any result, whatever the sweep ranges hold
    fd.check_fenders_in_contact(d.get("fenders_in_contact", 1))
    fd.check_limit("reaction_limit_kn", d.get("reaction_limit_kn"))
    fd.check_limit("hull_pressure_limit_kpa", d.get("hull_pressure_limit_kpa"))
    ship = fd.Ship(**s)
    berth = fd.Berth(**b)
    fcase = fd.Case(ship, berth, **case.get("conditions", {}))
    kw = dict(
        velocity_m_s=d.get("velocity_m_s"),
        factor=d.get("factor"),
        factor_position=d.get("factor_position", "smallest"),
    )
    # citation sidecar: strict (default, fail-closed) or the explicit opt-out record
    cit = case.get("citations") or {}
    mode = citations or cit.get("mode", "strict")
    if mode not in ("strict", "record"):
        raise ValueError(f"case citation mode must be strict or record, not {mode!r}")
    ckw = dict(citations=mode, citation_root=_resolve(base_dir, cit.get("wiki_root")))
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
    check_type_tolerances(catalogue, base_fender.fender_type, d)
    design = fd.design_performance(
        base_fender,
        energy_tolerance=d.get("energy_tolerance", 0.10),
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
        base=selected, **ckw,
    )
    excluded = fd.inapplicable_sweeps(d["method"])
    sweeps_skipped = {k: excluded[k] for k in ranges if k in excluded}
    bars = fd.tornado(sweeps)
    sizes, checks = [], []
    ids = d.get("neighbour_ids")
    if ids:
        per = selected.design_energy_knm / d.get("fenders_in_contact", 1)
        sizes = fender_size_sensitivity(catalogue, base_fender.fender_id, per, fender_ids=ids)
        checks = size_checks([r.fender for r in sizes], base_fender.fender_id, per, d,
                             selected.capacity_divisor, curve=curve, curve_type=base_fender.fender_type)
    return CaseResult(case, ship, berth, methods, skipped, selected, catalogue, base_fender,
                      capacity, design, curve, sweeps, bars, sizes, fcase, sweeps_skipped,
                      {f.fender_id: bases.get(f.fender_id, "") for f in catalogue} if bases else {},
                      checks, mode)
