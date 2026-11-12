"""Roll damping benchmark data, acceptance criteria and method matrix.

Loads the public tables under ``data/roll_damping/``:

* ``benchmark_cases.csv`` - measured or fitted roll damping of barge-like hulls
  from the literature, one row per case and amplitude, with source page and
  verification level;
* ``sources.csv`` - the citation of each ``source_id``;
* ``acceptance_criteria.yaml`` - three-colour benchmark bands per quantity;
* ``method_matrix.csv`` - the recommended estimation route and screening
  damping per scenario;
* ``wiki_tables.csv`` - pointers (not values) to the copyrighted paper tables
  held in the private llm-wiki.

Contracts follow ``digitalmodel.riser_database.loader``: known file names
only, a size cap per file, ``yaml.safe_load``, and fail-closed errors.
:func:`verdict` grades a calculated value against a reference with the bands
of one quantity and never clamps an unknown quantity.
"""

from __future__ import annotations

import csv
import math
from dataclasses import dataclass
from pathlib import Path
from typing import Optional

import yaml

REPO_ROOT = Path(__file__).resolve().parents[3]
DEFAULT_DATA_ROOT = REPO_ROOT / "data" / "roll_damping"
MAX_TABLE_BYTES = 1024 * 1024

GREEN, AMBER, RED = "green", "amber", "red"


class RollDampingDataError(RuntimeError):
    """Fail-closed loader or lookup error."""

    def __init__(self, *, table: str, reason: str) -> None:
        self.table = table
        self.reason = reason
        super().__init__(f"roll_damping[{table}]: {reason}")


@dataclass(frozen=True)
class BenchmarkCase:
    case_id: str
    source_id: str
    hull: str
    b_over_t: Optional[float]
    bilge: str
    bilge_keel: str
    scale: str
    amplitude_deg: float
    damping_pct: float
    damping_basis: str
    source_page: str
    verification: str
    note: str


@dataclass(frozen=True)
class Band:
    quantity: str
    description: str
    green_max_pct: float
    amber_max_pct: float
    rationale: str
    abs_floor: Optional[float] = None


@dataclass(frozen=True)
class MethodRecommendation:
    scenario_id: str
    scenario: str
    primary_method: str
    digitalmodel_route: str
    cross_check: str
    screening_damping_pct: str
    basis: str
    avoid: str


def _read_text(root: Path, name: str) -> str:
    path = root / name
    if not path.is_file():
        raise RollDampingDataError(table=name, reason=f"missing file {path}")
    if path.stat().st_size > MAX_TABLE_BYTES:
        raise RollDampingDataError(table=name, reason="file exceeds the size cap")
    return path.read_text(encoding="utf-8")


def _rows(root: Path, name: str, required: tuple[str, ...]) -> list[dict[str, str]]:
    reader = csv.DictReader(_read_text(root, name).splitlines())
    missing = [c for c in required if c not in (reader.fieldnames or [])]
    if missing:
        raise RollDampingDataError(table=name, reason=f"missing columns {missing}")
    return list(reader)


def _float(value: str, *, table: str, column: str, optional: bool = False) -> Optional[float]:
    if value == "" and optional:
        return None
    try:
        out = float(value)
    except ValueError as exc:
        raise RollDampingDataError(table=table, reason=f"non-numeric {column}: {value!r}") from exc
    if not math.isfinite(out):
        raise RollDampingDataError(table=table, reason=f"non-finite {column}: {value!r}")
    return out


def load_sources(root: Path = DEFAULT_DATA_ROOT) -> dict[str, dict[str, str]]:
    rows = _rows(root, "sources.csv", ("source_id", "citation", "link"))
    return {r["source_id"]: r for r in rows}


def load_benchmark_cases(root: Path = DEFAULT_DATA_ROOT) -> list[BenchmarkCase]:
    """Benchmark rows; every ``source_id`` must resolve in ``sources.csv``."""
    fields = tuple(BenchmarkCase.__dataclass_fields__)
    rows = _rows(root, "benchmark_cases.csv", fields)
    sources = load_sources(root)
    out, seen = [], set()
    for r in rows:
        if r["case_id"] in seen:
            raise RollDampingDataError(table="benchmark_cases.csv", reason=f"duplicate case_id {r['case_id']}")
        seen.add(r["case_id"])
        if r["source_id"] not in sources:
            raise RollDampingDataError(table="benchmark_cases.csv", reason=f"unknown source_id {r['source_id']}")
        damping = _float(r["damping_pct"], table="benchmark_cases.csv", column="damping_pct")
        if damping <= 0.0 or damping >= 100.0:
            raise RollDampingDataError(table="benchmark_cases.csv", reason=f"damping_pct out of range in {r['case_id']}")
        amplitude = _float(r["amplitude_deg"], table="benchmark_cases.csv", column="amplitude_deg")
        if not 0.0 < amplitude < 90.0:
            raise RollDampingDataError(table="benchmark_cases.csv", reason=f"amplitude_deg out of range in {r['case_id']}")
        b_over_t = _float(r["b_over_t"], table="benchmark_cases.csv", column="b_over_t", optional=True)
        if b_over_t is not None and b_over_t <= 0.0:
            raise RollDampingDataError(table="benchmark_cases.csv", reason=f"b_over_t must be positive in {r['case_id']}")
        for column in ("source_page", "verification", "damping_basis"):
            if not r[column].strip():
                raise RollDampingDataError(table="benchmark_cases.csv", reason=f"empty {column} in {r['case_id']}")
        out.append(BenchmarkCase(
            case_id=r["case_id"], source_id=r["source_id"], hull=r["hull"],
            b_over_t=b_over_t,
            bilge=r["bilge"], bilge_keel=r["bilge_keel"], scale=r["scale"],
            amplitude_deg=amplitude,
            damping_pct=damping, damping_basis=r["damping_basis"], source_page=r["source_page"],
            verification=r["verification"], note=r["note"],
        ))
    return out


def _check_band(band: Band) -> Band:
    finite = math.isfinite(band.green_max_pct) and math.isfinite(band.amber_max_pct)
    if not finite or not 0.0 < band.green_max_pct < band.amber_max_pct:
        raise RollDampingDataError(table="acceptance_criteria.yaml",
                                   reason=f"bands not finite and increasing for {band.quantity}")
    if band.abs_floor is not None and not (math.isfinite(band.abs_floor) and band.abs_floor >= 0.0):
        raise RollDampingDataError(table="acceptance_criteria.yaml",
                                   reason=f"abs_floor must be finite and non-negative for {band.quantity}")
    return band


def load_acceptance_criteria(root: Path = DEFAULT_DATA_ROOT) -> dict[str, Band]:
    doc = yaml.safe_load(_read_text(root, "acceptance_criteria.yaml"))
    if not isinstance(doc, dict) or not isinstance(doc.get("quantities"), dict):
        raise RollDampingDataError(table="acceptance_criteria.yaml", reason="no quantities mapping")
    out = {}
    for key, q in doc["quantities"].items():
        try:
            band = Band(
                quantity=key, description=str(q["description"]),
                green_max_pct=float(q["green_max_pct"]), amber_max_pct=float(q["amber_max_pct"]),
                rationale=str(q["rationale"]),
                abs_floor=float(q["abs_floor"]) if "abs_floor" in q else None,
            )
        except (KeyError, TypeError, ValueError) as exc:
            raise RollDampingDataError(table="acceptance_criteria.yaml", reason=f"bad entry {key}: {exc}") from exc
        out[key] = _check_band(band)
    return out


def load_method_matrix(root: Path = DEFAULT_DATA_ROOT) -> list[MethodRecommendation]:
    fields = tuple(MethodRecommendation.__dataclass_fields__)
    return [MethodRecommendation(**{k: r[k] for k in fields}) for r in _rows(root, "method_matrix.csv", fields)]


def load_wiki_tables(root: Path = DEFAULT_DATA_ROOT) -> list[dict[str, str]]:
    return _rows(root, "wiki_tables.csv", ("table_id", "source_id", "wiki_path", "content"))


def verdict(
    quantity: str,
    calculated: float,
    reference: float,
    *,
    criteria: Optional[dict[str, Band]] = None,
) -> dict:
    """Grade ``calculated`` against ``reference`` with the bands of ``quantity``.

    Returns ``{"deviation_pct", "colour", "green_max_pct", "amber_max_pct"}``.
    The deviation is signed, ``100 (calculated / reference - 1)``; the colour
    uses its magnitude, unless ``|calculated - reference|`` is within the
    quantity's ``abs_floor``, which grades green. Band edges are inclusive,
    with a relative round-off tolerance of 1e-9. Every graded quantity is a
    positive magnitude (a damping, a coefficient, a period): a negative
    calculated value or a non-positive reference raises.
    """
    bands = criteria if criteria is not None else load_acceptance_criteria()
    if quantity not in bands:
        raise RollDampingDataError(table="acceptance_criteria.yaml", reason=f"unknown quantity {quantity!r}")
    band = _check_band(bands[quantity])
    if not (math.isfinite(calculated) and math.isfinite(reference)):
        raise RollDampingDataError(table="acceptance_criteria.yaml", reason="non-finite value")
    if reference <= 0.0 or calculated < 0.0:
        raise RollDampingDataError(table="acceptance_criteria.yaml",
                                   reason="graded quantities are positive: reference must be > 0, calculated >= 0")
    dev = 100.0 * (calculated / reference - 1.0)
    mag = abs(dev) * (1.0 - 1e-9)
    if band.abs_floor is not None and abs(calculated - reference) <= band.abs_floor * (1.0 + 1e-9):
        colour = GREEN
    elif mag <= band.green_max_pct:
        colour = GREEN
    elif mag <= band.amber_max_pct:
        colour = AMBER
    else:
        colour = RED
    return dict(deviation_pct=dev, colour=colour,
                green_max_pct=band.green_max_pct, amber_max_pct=band.amber_max_pct)
