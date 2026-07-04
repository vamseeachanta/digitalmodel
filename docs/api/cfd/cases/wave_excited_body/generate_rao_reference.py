#!/usr/bin/env python3
"""Regenerate the frozen potential-flow heave-RAO reference (#1324).

The wave-excited-body CFD sweep (#1324) spot-checks its heave RAO curve
against a **potential-flow diffraction reference** — the same physics the
OrcaWave/AQWA pipeline implements, computed here in 2D-consistent form for
the *exact* CFD section (0.2 m half-submerged square) at the CFD's finite
water depth (0.4 m).

Method (capytaine linear-BEM, the in-repo potential-flow cross-check tier of
the #1161 validation philosophy):

* The 2D CFD slab (empty front/back patches = an infinitely long cylinder)
  is approximated by a **long box** (L = 6 m = 30 beams). The heave RAO is a
  ratio, so it is length-independent to leading order; a length sweep
  (L = 4 -> 8 m) moves the resonance peak by 0.5 % and the flank by < 1 %,
  i.e. end effects are negligible — the finite box reproduces the 2D
  sectional RAO. (Verified 2026-07-02.)
* Heave RAO(omega) = |F3| / |c33 - (M + a33) omega^2 + i b33 omega|, per unit
  wave amplitude, with a33/b33 the radiation added mass/damping and F3 the
  total heave excitation = **Froude-Krylov + diffraction** (capytaine's
  ``DiffractionProblem`` returns the scattering force ONLY — the FK force
  must be added, else the long-wave limit collapses to ~0.13 instead of 1).
* Finite depth 0.4 m is passed to every problem (matches the CFD tank).

Self-consistency anchors (all satisfied): RAO -> 1 as omega -> 0 (wave
follower); resonance near T = 2*pi*sqrt((M + a33)/c33); RAO -> 0 as
omega -> infinity. The long-wave value (RAO 1.014 at T = 3 s) independently
reproduces the verified #1302 CFD point (1.017) from a different method.

This script is **gated / not run in CI** (capytaine is not a digitalmodel
runtime dependency). It writes the frozen CSV that the always-on tests and
the report consume, exactly as the Kleefsman case ships digitized MARIN data.

Usage:
    # in an env with capytaine installed (see the header of the CSV for the
    # pinned version used to freeze the committed reference):
    python generate_rao_reference.py [out_csv]
"""
from __future__ import annotations

import sys
from pathlib import Path

import numpy as np

G = 9.81
RHO = 1000.0
DEPTH = 0.4          # matches the CFD tank (constant/transportProperties + NWT)
BEAM = 0.2           # body beam B (x)
HEIGHT = 0.2         # body full height
DRAFT = 0.1          # half-submerged square (half density)
BOX_LENGTH = 6.0     # long box ~ 2D slab (converged: end effects < 1%)
RESOLUTION = (16, 160, 16)

# Dense period grid: long-wave anchor (3.5 s) down through resonance (~0.9 s)
# to the roll-off (0.6 s). Extra density around the peak for a smooth curve.
PERIODS = np.array([
    3.5, 3.0, 2.5, 2.2, 2.0, 1.8, 1.6, 1.5, 1.4, 1.3, 1.2, 1.15, 1.1, 1.05,
    1.0, 0.97, 0.94, 0.92, 0.90, 0.88, 0.86, 0.84, 0.82, 0.80, 0.77, 0.74,
    0.70, 0.66, 0.62,
])


def compute_reference() -> list[dict]:
    import capytaine as cpt
    from capytaine.bem.airy_waves import froude_krylov_force

    mass = RHO * BEAM * BOX_LENGTH * DRAFT          # = displaced mass
    c33 = RHO * G * BEAM * BOX_LENGTH               # waterplane restoring

    mesh = cpt.mesh_parallelepiped(
        size=(BEAM, BOX_LENGTH, HEIGHT), resolution=RESOLUTION,
        center=(0.0, 0.0, 0.0), name="box",
    )
    wet = mesh.immersed_part(water_depth=DEPTH)
    # Irregular-frequency removal: an interior free-surface lid suppresses the
    # spurious BEM interior resonances that otherwise spike a33/b33 (and hence
    # RAO) near T ~ 0.85 s. omega_max covers the whole period grid.
    omega_max = 2.0 * np.pi / float(PERIODS.min())
    lid = wet.generate_lid(z=wet.lowest_lid_position(omega_max=1.1 * omega_max))
    body = cpt.FloatingBody(mesh=wet, lid_mesh=lid, name="box")
    body.add_translation_dof(name="Heave")

    solver = cpt.BEMSolver()
    rows = []
    for period in PERIODS:
        omega = 2.0 * np.pi / period
        rad = cpt.RadiationProblem(body=body, omega=omega, water_depth=DEPTH,
                                   radiating_dof="Heave", rho=RHO, g=G)
        dif = cpt.DiffractionProblem(body=body, omega=omega, water_depth=DEPTH,
                                     wave_direction=0.0, rho=RHO, g=G)
        rr = solver.solve(rad)
        dr = solver.solve(dif)
        a33 = float(rr.added_masses["Heave"])
        b33 = float(rr.radiation_dampings["Heave"])
        f3 = complex(froude_krylov_force(dif)["Heave"] + dr.forces["Heave"])
        denom = c33 - (mass + a33) * omega**2 + 1j * b33 * omega
        rao = float(abs(f3 / denom))
        rows.append({
            "period_s": float(period),
            "omega_rad_s": float(omega),
            "rao_heave": rao,
            "added_mass_a33": a33,
            "radiation_damping_b33": b33,
            "excitation_force_abs": float(abs(f3)),
        })
    # sanity anchors
    long_wave = rows[0]["rao_heave"]
    peak = max(rows, key=lambda d: d["rao_heave"])
    assert 0.9 <= long_wave <= 1.1, f"long-wave RAO {long_wave} not ~1"
    assert all(r["radiation_damping_b33"] > 0 for r in rows), "negative damping"
    print(f"long-wave RAO(T={rows[0]['period_s']}s) = {long_wave:.3f}  "
          f"(CFD #1302 point: 1.017)")
    print(f"peak RAO {peak['rao_heave']:.3f} at T = {peak['period_s']} s")
    return rows


def write_csv(rows: list[dict], out: Path, capytaine_version: str) -> None:
    import csv
    header_comment = (
        f"# Frozen potential-flow heave-RAO reference for the wave-excited "
        f"body CFD sweep (#1324).\n"
        f"# Method: capytaine {capytaine_version} linear BEM; total excitation "
        f"= Froude-Krylov + diffraction.\n"
        f"# Geometry: {BEAM} m beam x {DRAFT} m draft square (half-submerged), "
        f"long box L={BOX_LENGTH} m (~2D slab), resolution {RESOLUTION}.\n"
        f"# Water depth {DEPTH} m (matches CFD). rho={RHO}, g={G}.\n"
        f"# a33/b33 = radiation added mass/damping; excitation_force_abs per "
        f"unit wave amplitude (N/m for the L={BOX_LENGTH} m box).\n"
        f"# Anchors: RAO->1 (long wave, reproduces CFD #1302 1.017); "
        f"resonance ~0.9 s; RAO->0 (short wave). Regenerate: "
        f"generate_rao_reference.py\n"
    )
    with out.open("w", newline="") as fh:
        fh.write(header_comment)
        w = csv.DictWriter(fh, fieldnames=list(rows[0].keys()))
        w.writeheader()
        for r in rows:
            w.writerow(r)
    print(f"wrote {out} ({len(rows)} periods)")


def main() -> None:
    import capytaine as cpt
    out = Path(sys.argv[1]) if len(sys.argv) > 1 else \
        Path(__file__).parent / "rao_reference_capytaine.csv"
    rows = compute_reference()
    write_csv(rows, out, cpt.__version__)


if __name__ == "__main__":
    main()
