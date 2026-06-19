#!/usr/bin/env python3
# ABOUTME: Emit OrcaWave diffraction mass setup for a vessel from the vessel DB.
# ABOUTME: Wires vessel_db particulars + gyradii -> mass/inertia -> OrcaWave body block.
"""
Diffraction mass-setup from the vessel database
===============================================

Turns a vessel record (principal dims + displacement + radii of gyration) into
the rigid-body mass properties a diffraction run needs, and prints the OrcaWave
body-mass block (BodyMass / BodyCentreOfMass / BodyInertiaTensor*). This is the
diffraction-side consumer of the vessel DB (#853), parallel to the installation
go/no-go consumer. Feeds the diffraction domain module (#622).

Radii of gyration are cited where public, else estimated with a named relation
(gyradii.py); inertia = mass * k^2. Every field is tagged with its basis.

Usage:
    cd digitalmodel
    uv run python examples/demos/diffraction/diffraction_setup.py
    uv run python examples/demos/diffraction/diffraction_setup.py --scope floating --limit 5
"""

from __future__ import annotations

import argparse
import json

from digitalmodel.marine_ops.vessel_db.loader import iter_records
from digitalmodel.marine_ops.vessel_db.mass_properties import from_record


def setups_for_scope(scope: str, limit: int | None = None) -> list:
    out = []
    for rec in iter_records(scope, "particulars"):
        mp = from_record(rec)
        if mp is not None:
            out.append(mp)
        if limit and len(out) >= limit:
            break
    return out


def main() -> None:
    p = argparse.ArgumentParser(description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--scope", default="floating",
                   help="vessel scope (floating/install/tanker/support)")
    p.add_argument("--limit", type=int, default=3)
    p.add_argument("--json", action="store_true", help="emit OrcaWave blocks as JSON")
    args = p.parse_args()

    setups = setups_for_scope(args.scope, args.limit)
    if args.json:
        print(json.dumps([mp.orcawave_body_block() for mp in setups], indent=2))
        return

    print("=" * 78)
    print(f"  DIFFRACTION MASS SETUP — vessel DB scope '{args.scope}'")
    print("=" * 78)
    for mp in setups:
        blk = mp.orcawave_body_block()
        print(f"\n{mp.vessel_name}")
        print(f"  BodyMass: {blk['BodyMass']} te   CoG {blk['BodyCentreOfMass']} m")
        print(f"  gyradii (m): " + ", ".join(f"{k}={v:.2f}" for k, v in mp.gyradii_m.items()))
        print(f"  inertia (te·m²): Ixx={mp.inertia_te_m2.get('Ixx',0):.3e} "
              f"Iyy={mp.inertia_te_m2.get('Iyy',0):.3e} Izz={mp.inertia_te_m2.get('Izz',0):.3e}")
        prov = {k: v for k, v in mp.provenance.items() if v != "cited"}
        if prov:
            print("  basis: " + "; ".join(f"{k}={v}" for k, v in prov.items()))
        if mp.gaps:
            print(f"  gaps: {', '.join(mp.gaps)}")
    print("\n" + "-" * 78)
    print(f"  {len(setups)} diffraction setups generated. Off-diagonal inertia = 0 "
          "(principal-axis assumption).")


if __name__ == "__main__":
    main()
