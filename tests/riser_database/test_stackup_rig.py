"""#1259 suites: riser_stackup + rig_riser_interface — loader, controlled
vocabularies, the rig-residency join, wiki_path structural tripwire, de-id
tripwire, and RSU-registry reconciliation.

Governance (plan v2): public rows carry HANDLES + BANDED envelopes only.
No exact depths/masses/names anywhere in the public artifacts — the de-id
tripwire below enforces the shape, the provenance-token tripwire (separate
module) enforces the names.
"""
from __future__ import annotations

import os
import re
from pathlib import Path

import pytest

from digitalmodel.riser_database import loader as rdb_loader
from digitalmodel.riser_database.loader import (
    DEFAULT_DB_ROOT,
    RigRiserInterfaceRow,
    RiserDatabase,
    RiserDatabaseError,
    RiserStackupRow,
)

REGISTRY_WIKI_PATH = "wikis/riser-projects/wiki/datasets/stackup-registry.md"

TOPOLOGY_VOCAB = {
    "conventional-bop-on-wellhead",
    "completion-bop-on-ths",
    "intervention-bop-on-tree",
    "mpd-surface-stack",
    "production-scr-hangoff",
    # #1279 wave-2 classes (dm#1259 follow-on, RSU-0010..0070):
    "sbop",
    "riserless",
    "ttr",
    "slwr",
    "landing-string",
    "workover-surface-stack",
    "none",
}
STACKUP_TYPE_VOCAB = {
    "as-planned-stackup",
    "as-run-tally",
    "weight-table",
    "joint-library",
    "component-library",
    "stack-up-drawing",
}
OPERATION_VOCAB = {
    "drilling",
    "completion",
    "intervention",
    "mpd",
    "pa",
    "production",
    "workover",
    "",
}
WATER_DEPTH_BAND_VOCAB = {
    "0-500m",
    "500-1000m",
    "1000-1500m",
    "1500-2000m",
    "2000-3000m",
    "3000-4000m",
    "4000-5000m",
    "",
}

# Unit-bearing exact-value patterns (de-id tripwire). Bare numerics in typed
# numeric COLUMNS are fine (rig capability data); what must never appear is a
# number WITH a length/mass unit — the signature of exact depths/weights.
DEID_PATTERNS = (
    re.compile(r"\d{3,5}(\.\d+)?\s?(m|ft)\b", re.IGNORECASE),
    re.compile(r"\d{2,4}(\.\d+)?\s?(t|te|lb|lbs|kips)\b", re.IGNORECASE),
)


# -- loader ---------------------------------------------------------------------


def test_row_counts_pinned():
    db = RiserDatabase.load()
    assert len(db.stackups) == 70
    assert len(db.rigs) == 7
    assert all(isinstance(r, RiserStackupRow) for r in db.stackups)
    assert all(isinstance(r, RigRiserInterfaceRow) for r in db.rigs)


@pytest.mark.parametrize("accessor", ["stackup", "rig"])
def test_unknown_key_escalates(accessor):
    db = RiserDatabase.load()
    with pytest.raises(RiserDatabaseError) as exc:
        getattr(db, accessor)("no-such-key")
    assert "unknown" in exc.value.reason


def test_iter_public_rows_covers_five_tables():
    tables = {t for t, _k, _f in RiserDatabase.load().iter_public_rows()}
    assert tables == {
        "config_catalog",
        "standards_crosswalk",
        "material_sn_scf_dff",
        "riser_stackup",
        "rig_riser_interface",
    }


# -- controlled vocabularies -------------------------------------------------------


def test_stackup_vocabularies():
    for row in RiserDatabase.load().stackups:
        assert row.topology_class in TOPOLOGY_VOCAB, row.rsu_id
        assert row.stackup_type in STACKUP_TYPE_VOCAB, row.rsu_id
        assert row.operation in OPERATION_VOCAB, row.rsu_id
        assert row.water_depth_band in WATER_DEPTH_BAND_VOCAB, row.rsu_id
        assert re.fullmatch(r"RSU-\d{4}", row.rsu_id)


def test_topology_grain():
    """The depth-collapsed grain: every vocabulary class is exercised by at
    least one row (11 topology classes + none-sentinel rows)."""
    classes = {r.topology_class for r in RiserDatabase.load().stackups}
    assert classes == TOPOLOGY_VOCAB - {"none"} | {"none"}


def test_property_library_rows_use_none_sentinel():
    db = RiserDatabase.load()
    for rsu_id in ("RSU-0006", "RSU-0007"):
        assert db.stackup(rsu_id).topology_class == "none"


# -- rig-residency join --------------------------------------------------------------


def test_rig_ref_join_integrity():
    db = RiserDatabase.load()
    rig_keys = {r.rig_ref for r in db.rigs}
    for row in db.stackups:
        if row.rig_ref:
            assert row.rig_ref in rig_keys, (row.rsu_id, row.rig_ref)


def test_wed_rig_name_empty_for_engagement_rows():
    # This slice ships no public-spec-sourced rig rows -> all wed_rig_name empty.
    # Populating one requires a public rig-spec source documented in sources.yml.
    for rig in RiserDatabase.load().rigs:
        assert rig.wed_rig_name == "", rig.rig_ref


# -- wiki_path structural tripwire ---------------------------------------------------


def test_wiki_path_registry_page_only():
    db = RiserDatabase.load()
    forbidden = re.compile(r"/sources/|/entities/|\b\d{5}\b")
    for row in list(db.stackups) + list(db.rigs):
        assert row.wiki_path == REGISTRY_WIKI_PATH, row
        assert row.wiki_path.startswith("wikis/riser-projects/wiki/datasets/")
        assert not forbidden.search(row.wiki_path)


# -- de-id tripwire ------------------------------------------------------------------

_PUBLIC_FILES = (
    "riser_stackup.csv",
    "rig_riser_interface.csv",
    "manifest.yaml",
)


def test_no_exact_unit_bearing_values_in_rows():
    violations = []
    for table, key, flat in RiserDatabase.load().iter_public_rows():
        if table not in ("riser_stackup", "rig_riser_interface"):
            continue
        row = (
            RiserDatabase.load().stackup(key)
            if table == "riser_stackup"
            else RiserDatabase.load().rig(key)
        )
        for field_name in row.__dataclass_fields__:
            value = getattr(row, field_name)
            if field_name == "water_depth_band" or value is None:
                continue
            text = str(value)
            for pattern in DEID_PATTERNS:
                if pattern.search(text):
                    violations.append((table, key, field_name, text))
    assert not violations, violations


def test_no_exact_unit_bearing_values_in_raw_files():
    sources_spec = (
        Path(__file__).resolve().parents[2]
        / "scripts"
        / "riser_database"
        / "sources.yml"
    )
    targets = [DEFAULT_DB_ROOT / name for name in _PUBLIC_FILES] + [sources_spec]
    violations = []
    for path in targets:
        for line in path.read_text().splitlines():
            if "water_depth_band" in line or re.search(r"\b\d{1,4}-\d{3,4}m\b", line):
                continue  # band vocabulary lines (incl. 0-500m) asserted separately
            for pattern in DEID_PATTERNS:
                if pattern.search(line):
                    violations.append((path.name, line.strip()[:80]))
    assert not violations, violations


def test_registry_generic_names_not_republished():
    """The registry's generic-name strings embed exact WD/tonnage — they must
    not appear verbatim in any public row (in-context; loud skip standalone)."""
    registry = _registry_text()
    names = re.findall(r"\|\s*(2[01]?[\d.]*in [^|]{10,})\|", registry)
    flat_all = " ".join(f for _t, _k, f in RiserDatabase.load().iter_public_rows())
    for name in names:
        assert name.strip() not in flat_all


# -- RSU registry reconciliation (in-context) ---------------------------------------

_KNOWN_WIKI_CLONES = (
    Path("/mnt/local-analysis/llm-wiki"),  # abs-path-allowed
    Path.home() / "workspace-hub" / "llm-wiki",
)


def _registry_text() -> str:
    candidates = []
    env = os.environ.get("LLM_WIKI_PATH")
    if env:
        candidates.append(Path(env))
    candidates.extend(_KNOWN_WIKI_CLONES)
    for base in candidates:
        page = base / REGISTRY_WIKI_PATH
        if page.is_file():
            return page.read_text()
    pytest.skip(
        "llm-wiki stack-up registry not available (standalone mode) — this "
        "reconciliation test is part of the in-context merge gate"
    )


def test_registry_reconciliation_bidirectional():
    registry_ids = set(re.findall(r"RSU-\d{4}", _registry_text()))
    table_ids = {r.rsu_id for r in RiserDatabase.load().stackups}
    assert table_ids == registry_ids, (
        f"table-only: {sorted(table_ids - registry_ids)}; "
        f"registry-only (new RSU needs a conscious table update): "
        f"{sorted(registry_ids - table_ids)}"
    )
