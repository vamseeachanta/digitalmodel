#!/usr/bin/env python3
# ABOUTME: Prepare a representative set of OrcaWave diffraction decks from the vessel DB.
# ABOUTME: Thin CLI over hydrodynamics.diffraction.vessel_deck_builder (#896).
"""
Prepare vessel diffraction decks
================================

Builds a representative cross-scope set of OrcaWave diffraction decks from the
vessel database — one per hull form (FPSO, drillship, tanker, semisub, monohull
crane vessel). Each deck = a parametric hull GDF (swappable) + an OrcaWave .yml
+ a manifest. Run the solver on a licensed OrcaWave/AQWA machine.

Usage:
    cd digitalmodel
    uv run python examples/demos/diffraction/prepare_vessel_decks.py
    uv run python examples/demos/diffraction/prepare_vessel_decks.py --out /tmp/decks
"""

from __future__ import annotations

import argparse
import json
from pathlib import Path

from digitalmodel.hydrodynamics.diffraction.vessel_deck_builder import (
    build_deck,
    select_form,
)
from digitalmodel.marine_ops.vessel_db.loader import datasets, iter_records

OUTPUT_DIR = Path(__file__).resolve().parent / "decks"

# Representative set: (label, scope filter or None, name/type keywords, required form).
# required form keeps the set varied (e.g. the tanker slot must be a 'full' ship
# form, the crane slot a monohull 'ship' — not another semisub).
_TARGETS = [
    ("FPSO", "floating", ("fpso",), "box"),
    ("drillship", "floating", ("drillship", "drill"), "ship"),
    ("tanker / gas carrier", "tanker", (), "full"),
    ("semisubmersible", None, ("semi", "sscv", "column"), "placeholder"),
    (
        "monohull crane / construction",
        "install",
        ("construction", "crane", "derrick", "dcv", "aegir", "balder", "monohull"),
        "ship",
    ),
]


def _particulars(scope_filter=None):
    recs = []
    for scope, layer in datasets():
        if layer != "particulars":
            continue
        if scope_filter and scope != scope_filter:
            continue
        recs.extend(iter_records(scope, layer))
    return recs


def _has_dims(r):
    d = r.canonical_dimensions()
    return (d.get("loa") or d.get("lbp")) and d.get("beam") and d.get("draft")


def select_representative():
    chosen, used = [], set()
    for label, scope_filter, keywords, req_form in _TARGETS:
        pick = None
        for r in _particulars(scope_filter):
            if r.name in used or not _has_dims(r):
                continue
            if select_form(r) != req_form:
                continue
            if keywords:
                vt, nm = (r.vessel_type or "").lower(), r.name.lower()
                if not any(k in vt or k in nm for k in keywords):
                    continue
            pick = r
            break
        if pick is not None:
            chosen.append((label, pick))
            used.add(pick.name)
    return chosen


def main() -> None:
    p = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    p.add_argument("--out", type=Path, default=OUTPUT_DIR)
    p.add_argument("--target-panels", type=int, default=1000)
    args = p.parse_args()

    args.out.mkdir(parents=True, exist_ok=True)
    chosen = select_representative()
    print("=" * 78)
    print("  PREPARING DIFFRACTION DECKS (parametric mesh + vessel_db mass props)")
    print("=" * 78)
    index = []
    for label, rec in chosen:
        res = build_deck(rec, args.out, target_panels=args.target_panels)
        if res is None:
            print(f"  [skip] {label}: {rec.name} (insufficient dimensions)")
            continue
        panels = f"{res.n_panels} panels" if res.n_panels else "mesh-to-supply"
        print(
            f"  [{res.form:11}] {rec.name[:32]:32} {res.mesh_kind:11} {panels:16} "
            f"mass={res.mass_basis}"
        )
        for n in res.notes:
            print(f"               · {n}")
        index.append(
            {
                "form_label": label,
                "vessel": rec.name,
                "form": res.form,
                "deck_dir": res.deck_path.parent.name,
                "deck": res.deck_path.name,
                "mesh_kind": res.mesh_kind,
                "n_panels": res.n_panels,
                "mass_basis": res.mass_basis,
            }
        )
    (args.out / "index.json").write_text(json.dumps(index, indent=2))
    print("-" * 78)
    print(f"  {len(index)} decks written under {args.out}/  (index.json)")
    print(
        "  Run each <vessel>.yml on a licensed OrcaWave machine; swap hull.gdf for a "
        "faired mesh\n  to upgrade from first-pass screening to design-grade."
    )


if __name__ == "__main__":
    main()
