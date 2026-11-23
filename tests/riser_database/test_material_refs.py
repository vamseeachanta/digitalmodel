"""Suite 4 (#1245): material_sn_scf_dff references reconcile with the REAL
riser S-N consumer — ``fatigue/sn_curves.get_sn_curve`` (plain detail-category
letters + environment strings), used by riser_fatigue workflow.py/touchdown.py.
NOT ``sn_library``/``sn_library_api`` (composite "DNV-RP-C203:D:air" ids).

Getter parity anchors #1246: the cited values must equal today's literals so
the calc-wiring slice inherits day-one numeric parity.
"""
from __future__ import annotations

from pathlib import Path

from digitalmodel.fatigue.sn_curves import get_sn_curve
from digitalmodel.riser_database import getters
from digitalmodel.riser_database.loader import RiserDatabase


def _fixture_repo_root() -> Path:
    return Path(__file__).resolve().parent.parent / "citations" / "fixtures"


def test_every_sn_reference_loads_via_the_real_consumer():
    failures = []
    for row in RiserDatabase.load().materials:
        try:
            curve = get_sn_curve(row.sn_curve_name, row.environment)
        except (KeyError, ValueError) as exc:
            failures.append((row.ref_id, repr(exc)))
            continue
        if curve is None:
            failures.append((row.ref_id, "get_sn_curve returned None"))
    assert not failures, failures


def test_every_getter_reference_names_a_real_getter():
    for row in RiserDatabase.load().materials:
        for ref in (row.scf_ref, row.dff_ref):
            assert callable(getattr(getters, ref, None)), (row.ref_id, ref)


def test_getter_values_match_live_riser_literals():
    # Parity anchors: riser_fatigue/touchdown.py defaults dff=10.0, scf=1.0
    # (and workflow.py wave scf default 1.0). #1246 wires calcs to these
    # getters; identical values = day-one parity.
    root = _fixture_repo_root()
    assert getters.get_riser_dff(repo_root=root).value == 10.0
    assert getters.get_riser_scf(repo_root=root).value == 1.0


def test_touchdown_default_row_present():
    row = RiserDatabase.load().material("riser_touchdown_default")
    assert row.sn_curve_name == "F1"
    assert row.environment == "seawater_cp"
    assert row.dff_ref == "get_riser_dff"
    assert row.scf_ref == "get_riser_scf"
