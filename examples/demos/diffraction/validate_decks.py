#!/usr/bin/env python3
# ABOUTME: License-free hydrostatic validation of prepared diffraction meshes (Capytaine).
# ABOUTME: Confirms each parametric hull floats with outward normals before a licensed run.
"""
Validate prepared diffraction decks (license-free)
==================================================

Builds the representative deck set and hydrostatically validates each parametric
hull mesh with the open-source BEM solver Capytaine — no OrcaWave license needed.
A correct hull returns POSITIVE heave stiffness K33 ≈ ρg·A_wp; a negative K33
flags inverted panel normals.

Usage (Capytaine is optional — easiest via uv --with):
    cd digitalmodel
    uv run --with capytaine python examples/demos/diffraction/validate_decks.py
"""

from __future__ import annotations

import tempfile
from pathlib import Path

from digitalmodel.hydrodynamics.diffraction.deck_validation import validate_mesh
from digitalmodel.hydrodynamics.diffraction.vessel_deck_builder import (
    _FORM_TAPER,
    MESH_FILE,
    build_deck,
    select_form,
)
from digitalmodel.marine_ops.vessel_db.loader import datasets, iter_records

_FORMS = ("box", "full", "ship")  # parametric forms (placeholder/semisub skipped)


def _representative(limit_per_form=1):
    chosen, seen_forms = [], {}
    for scope, layer in datasets():
        if layer != "particulars":
            continue
        for r in iter_records(scope, layer):
            form = select_form(r)
            if form not in _FORMS:
                continue
            d = r.canonical_dimensions()
            if not (
                (d.get("loa") or d.get("lbp")) and d.get("beam") and d.get("draft")
            ):
                continue
            if seen_forms.get(form, 0) >= limit_per_form:
                continue
            seen_forms[form] = seen_forms.get(form, 0) + 1
            chosen.append(r)
    return chosen


def main() -> None:
    probe = validate_mesh("nonexistent.gdf")
    if not probe.get("available"):
        print(probe.get("hint"))
        return

    print("=" * 78)
    print("  LICENSE-FREE MESH VALIDATION (Capytaine hydrostatics)")
    print("=" * 78)
    print(f"  {'vessel':28} {'form':5} {'K33 (N/m)':>16} {'K33>0':>6} {'K33/ρgLB':>9}")
    print("-" * 78)
    with tempfile.TemporaryDirectory() as tmp:
        for rec in _representative():
            res = build_deck(rec, Path(tmp))
            if res is None or res.mesh_kind != "parametric":
                continue
            d = rec.canonical_dimensions()
            loa, beam = (d.get("loa") or d.get("lbp")), d.get("beam")
            taper = _FORM_TAPER.get(res.form, 0.8)
            v = validate_mesh(
                res.deck_path.parent / MESH_FILE,
                expected_waterplane_m2=loa * beam * taper,
            )
            flag = "yes" if v.get("k33_positive") else "NO"
            ratio = (
                v.get("k33", 0) / (1025.0 * 9.81 * loa * beam) if loa and beam else 0
            )
            print(
                f"  {rec.name[:28]:28} {res.form:5} {v.get('k33', 0):>16,.0f} "
                f"{flag:>6} {ratio:>9.3f}"
            )
    print("-" * 78)
    print("  K33>0 with ratio ~ plan-form fraction => mesh is watertight with outward")
    print(
        "  normals and floats at the right waterplane. Swap in a faired hull + run on"
    )
    print("  a licensed OrcaWave/AQWA machine for design-grade RAOs.")


if __name__ == "__main__":
    main()
