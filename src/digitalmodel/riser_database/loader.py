"""Riser database loader — tabular slice (#1245 / #1199a).

Loads the three public reference tables under ``data/riser_database/``:

* ``config_catalog``      — riser configuration envelopes seeded from
  already-public repo files (``examples/riser_input/*.yml`` +
  ``config/pipe/*riser*.yml``). No ACE_SHARE rows in this slice (#1199d).
* ``standards_crosswalk`` — clause IDENTIFIERS + ``wiki_path`` only. Licensed
  DNV/API table VALUES never live in this public repo; they resolve through
  ``CitedValue`` getters against the private llm-wiki
  (see ``riser_database.getters``).
* ``material_sn_scf_dff`` — references into ``fatigue/sn_curves`` curve ids
  (the real riser fatigue consumer) + getter names. References, not values.

Contracts:

* **Bounded reads** — known filenames only, per-file size cap, ``safe_load``.
* **Fail-closed** — missing/malformed/tampered tables raise
  :class:`RiserDatabaseError`; there is no partial load.
* **Escalation** — an unknown key raises with a reason, mirroring
  ``parametric.atlas.Atlas.predict`` (never clamp, never fuzzy-match).
"""
from __future__ import annotations

import csv
import hashlib
from dataclasses import dataclass, fields
from pathlib import Path
from typing import Iterator, Mapping, Optional

import yaml

REPO_ROOT = Path(__file__).resolve().parents[3]
DEFAULT_DB_ROOT = REPO_ROOT / "data" / "riser_database"
#: Bounded-read cap per table file. These are small curated tables; anything
#: near this size indicates tampering or a generation defect.
MAX_TABLE_BYTES = 5 * 1024 * 1024


class RiserDatabaseError(RuntimeError):
    """Fail-closed loader/lookup error. Carries the offending table + reason."""

    def __init__(self, *, table: str, reason: str) -> None:
        self.table = table
        self.reason = reason
        super().__init__(f"riser_database[{table}]: {reason}")


def _opt_float(value: str, *, table: str, column: str) -> Optional[float]:
    if value == "":
        return None
    try:
        return float(value)
    except ValueError as exc:
        raise RiserDatabaseError(
            table=table, reason=f"non-numeric {column}: {value!r}"
        ) from exc


@dataclass(frozen=True)
class RiserConfigRow:
    config_id: str
    riser_type: str
    source_path: str
    water_depth_m: Optional[float]
    od_mm: Optional[float]
    wt_mm: Optional[float]
    material_grade: str
    analysis_kind: str
    notes: str


@dataclass(frozen=True)
class StandardsCrosswalkRow:
    code_id: str
    family_key: str
    publisher: str
    section: str
    topic: str
    wiki_path: str
    registry_revision: str
    note: str


@dataclass(frozen=True)
class MaterialSnScfDffRow:
    ref_id: str
    sn_curve_name: str
    environment: str
    scf_ref: str
    dff_ref: str
    code_id: str
    wiki_path: str
    note: str


@dataclass(frozen=True)
class RiserStackupRow:
    """Registry HANDLE + banded envelope (#1259): joint-by-joint values live
    wiki-side; ``water_depth_band`` is a coarse band, never an exact depth;
    ``rig_ref`` is the rig-residency join into ``rig_riser_interface``."""

    rsu_id: str
    topology_class: str
    stackup_type: str
    operation: str
    riser_od_in: Optional[float]
    water_depth_band: str
    rig_ref: str
    wiki_path: str
    note: str


@dataclass(frozen=True)
class RigRiserInterfaceRow:
    """Rig-class riser-handling capability (#1259). ``rig_ref`` is a generic
    technical-signature slug; ``wed_rig_name`` joins the worldenergydata
    2,268-rig identity space and is populated ONLY for rows sourced from
    public rig specifications (named-rig <-> engagement mapping stays
    wiki-side)."""

    rig_ref: str
    rig_type: str
    n_tensioners: Optional[float]
    tensioner_type: str
    tensioner_capacity_kips: Optional[float]
    tensioner_stroke_ft: Optional[float]
    tj_stroke_ft: Optional[float]
    n_tension_wires: Optional[float]
    moonpool_length_ft: Optional[float]
    moonpool_width_ft: Optional[float]
    drill_floor_elev_ft: Optional[float]
    #: Rated wellhead/conductor bending-moment capacity [kN.m] and flex-joint
    #: hardware angular rating [deg] (#1345). Public rows carry only public-spec
    #: rated values; client-/well-specific ratings stay wiki-side. Ship EMPTY in
    #: this slice — populated by the paired worldenergydata rig-spec sourcing.
    conductor_moment_capacity_kn_m: Optional[float]
    flexjoint_angle_rating_deg: Optional[float]
    wed_rig_name: str
    wiki_path: str
    note: str


_NUMERIC_CONFIG_COLUMNS = (
    "water_depth_m",
    "od_mm",
    "wt_mm",
    "riser_od_in",
    "n_tensioners",
    "tensioner_capacity_kips",
    "tensioner_stroke_ft",
    "tj_stroke_ft",
    "n_tension_wires",
    "moonpool_length_ft",
    "moonpool_width_ft",
    "drill_floor_elev_ft",
    "conductor_moment_capacity_kn_m",
    "flexjoint_angle_rating_deg",
)
#: Known tables: csv file, key column, row type. Anything else in the
#: manifest is rejected (bounded reads: no discovery, no globbing).
_TABLE_SPECS = {
    "config_catalog": ("config_catalog.csv", "config_id", RiserConfigRow),
    "standards_crosswalk": ("standards_crosswalk.csv", "code_id", StandardsCrosswalkRow),
    "material_sn_scf_dff": ("material_sn_scf_dff.csv", "ref_id", MaterialSnScfDffRow),
    "riser_stackup": ("riser_stackup.csv", "rsu_id", RiserStackupRow),
    "rig_riser_interface": ("rig_riser_interface.csv", "rig_ref", RigRiserInterfaceRow),
}


def _read_bounded(path: Path, *, table: str) -> bytes:
    if not path.is_file():
        raise RiserDatabaseError(table=table, reason=f"missing file: {path.name}")
    size = path.stat().st_size
    if size > MAX_TABLE_BYTES:
        raise RiserDatabaseError(
            table=table,
            reason=f"file size {size} exceeds bounded-read cap {MAX_TABLE_BYTES}",
        )
    return path.read_bytes()


def _load_table(root: Path, table: str, expected_sha256: str) -> dict[str, object]:
    file_name, key_column, row_type = _TABLE_SPECS[table]
    raw = _read_bounded(root / file_name, table=table)
    digest = hashlib.sha256(raw).hexdigest()
    if digest != expected_sha256:
        raise RiserDatabaseError(
            table=table,
            reason=f"fingerprint mismatch: manifest {expected_sha256[:12]}… "
            f"!= file {digest[:12]}…",
        )
    reader = csv.DictReader(raw.decode("utf-8").splitlines())
    expected_columns = [f.name for f in fields(row_type)]
    if list(reader.fieldnames or []) != expected_columns:
        raise RiserDatabaseError(
            table=table,
            reason=f"column mismatch: {reader.fieldnames} != {expected_columns}",
        )
    rows: dict[str, object] = {}
    for record in reader:
        if None in record or None in record.values():
            raise RiserDatabaseError(table=table, reason="ragged csv row")
        for column in _NUMERIC_CONFIG_COLUMNS:
            if column in record:
                record[column] = _opt_float(
                    record[column], table=table, column=column
                )
        row = row_type(**record)
        key = getattr(row, key_column)
        if key in rows:
            raise RiserDatabaseError(table=table, reason=f"duplicate key: {key!r}")
        rows[key] = row
    return rows


class RiserDatabase:
    """In-memory view of the riser reference tables (fail-closed load)."""

    def __init__(
        self,
        *,
        configs: Mapping[str, RiserConfigRow],
        crosswalk: Mapping[str, StandardsCrosswalkRow],
        materials: Mapping[str, MaterialSnScfDffRow],
        stackups: Mapping[str, RiserStackupRow],
        rigs: Mapping[str, RigRiserInterfaceRow],
    ) -> None:
        self._configs = dict(configs)
        self._crosswalk = dict(crosswalk)
        self._materials = dict(materials)
        self._stackups = dict(stackups)
        self._rigs = dict(rigs)

    @classmethod
    def load(cls, root: Path = DEFAULT_DB_ROOT) -> "RiserDatabase":
        root = Path(root)
        if not root.is_dir():
            raise RiserDatabaseError(
                table="manifest", reason=f"database root missing: {root}"
            )
        manifest_raw = _read_bounded(root / "manifest.yaml", table="manifest")
        manifest = yaml.safe_load(manifest_raw.decode("utf-8"))
        if not isinstance(manifest, dict) or "tables" not in manifest:
            raise RiserDatabaseError(table="manifest", reason="malformed manifest")
        listed = set(manifest["tables"])
        if listed != set(_TABLE_SPECS):
            raise RiserDatabaseError(
                table="manifest",
                reason=f"table set mismatch: {sorted(listed)} != "
                f"{sorted(_TABLE_SPECS)}",
            )
        loaded = {
            table: _load_table(root, table, manifest["tables"][table]["sha256"])
            for table in _TABLE_SPECS
        }
        return cls(
            configs=loaded["config_catalog"],
            crosswalk=loaded["standards_crosswalk"],
            materials=loaded["material_sn_scf_dff"],
            stackups=loaded["riser_stackup"],
            rigs=loaded["rig_riser_interface"],
        )

    # -- accessors: unknown key escalates (never defaults) -----------------------

    def config(self, config_id: str) -> RiserConfigRow:
        return self._lookup("config_catalog", self._configs, config_id)

    def crosswalk(self, code_id: str) -> StandardsCrosswalkRow:
        return self._lookup("standards_crosswalk", self._crosswalk, code_id)

    def material(self, ref_id: str) -> MaterialSnScfDffRow:
        return self._lookup("material_sn_scf_dff", self._materials, ref_id)

    def stackup(self, rsu_id: str) -> RiserStackupRow:
        return self._lookup("riser_stackup", self._stackups, rsu_id)

    def rig(self, rig_ref: str) -> RigRiserInterfaceRow:
        return self._lookup("rig_riser_interface", self._rigs, rig_ref)

    @staticmethod
    def _lookup(table: str, rows: Mapping[str, object], key: str):
        if key not in rows:
            raise RiserDatabaseError(
                table=table,
                reason=f"unknown key {key!r} — escalate (no fuzzy match, no default)",
            )
        return rows[key]

    @property
    def configs(self) -> tuple[RiserConfigRow, ...]:
        return tuple(self._configs.values())

    @property
    def crosswalk_rows(self) -> tuple[StandardsCrosswalkRow, ...]:
        return tuple(self._crosswalk.values())

    @property
    def materials(self) -> tuple[MaterialSnScfDffRow, ...]:
        return tuple(self._materials.values())

    @property
    def stackups(self) -> tuple[RiserStackupRow, ...]:
        return tuple(self._stackups.values())

    @property
    def rigs(self) -> tuple[RigRiserInterfaceRow, ...]:
        return tuple(self._rigs.values())

    def iter_public_rows(self) -> Iterator[tuple[str, str, str]]:
        """Yield ``(table, key, flattened-row-text)`` for every public-route
        row — the substrate the leak gate (suite 6) scans. Kept in lockstep
        with ``_TABLE_SPECS`` (a new table must appear here; the coverage
        test derives its expectation from ``_TABLE_SPECS``)."""
        for table, rows in (
            ("config_catalog", self._configs),
            ("standards_crosswalk", self._crosswalk),
            ("material_sn_scf_dff", self._materials),
            ("riser_stackup", self._stackups),
            ("rig_riser_interface", self._rigs),
        ):
            for key, row in rows.items():
                flat = " | ".join(
                    "" if value is None else str(value)
                    for value in (getattr(row, f.name) for f in fields(row))
                )
                yield table, key, flat
