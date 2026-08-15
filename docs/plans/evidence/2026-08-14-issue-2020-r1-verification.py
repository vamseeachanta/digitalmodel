#!/usr/bin/env python3
"""Reproduces every numeric claim in the #2020 adversarial review, round 1.

Run from the repository root:

    uv run --quiet python docs/plans/evidence/2026-08-14-issue-2020-r1-verification.py

Committed because the review asserted specific figures against the plan and
against Codex's independent run, and a review whose numbers cannot be
re-derived is an opinion. Every value printed here appears in the round-1
comment on the issue.

Two of these checks corrected the reviewer rather than the plan, which is the
reason the script exists in the repository instead of a scratch directory:

  * CHECK 4 shows the sub-unity form factor is NOT removed by correcting c12,
    contrary to the review's "C-1-corrected" qualifier. c12's shipped and
    published forms are identical everywhere except T/L > 0.05, and the
    minimum sits at T/L = 0.02.

  * CHECK 2 shows the floor argument is stronger than either the plan or the
    review stated: since a form factor is >= 1 by definition, Cf + Ca alone
    already exceeds the tanker fixture, with no Holtrop-Mennen regression
    involved at any point.
"""
from __future__ import annotations

import math
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[3] / "src"))

from digitalmodel.naval_architecture import holtrop_coefficients as hc  # noqa: E402
from digitalmodel.naval_architecture import holtrop_mennen as hm  # noqa: E402

SER = dict(lwl=121.92, beam=16.26, draft=6.5, cb=0.60, cm=0.977, cp=0.614,
           cwp=0.70, lcb_pct=-2.02, cstern=0, speed=7.72, abt=0.0, at=0.0,
           tf=6.5, fixture_ct=0.00385)
TNK = dict(lwl=205.0, beam=32.0, draft=12.0, cb=0.80, cm=0.995, cp=0.804,
           cwp=0.88, lcb_pct=-0.5, cstern=0, speed=7.72, abt=0.0, at=0.0,
           tf=12.0, fixture_ct=0.00175)


def c12_published(t_over_l: float) -> float:
    """Holtrop's piecewise c12. The shipped code returns the T/L < 0.02
    constant on the T/L > 0.05 branch as well; this is defect C-1."""
    if t_over_l > 0.05:
        return t_over_l ** 0.2228446
    if t_over_l < 0.02:
        return 0.479948
    return 48.20 * (t_over_l - 0.02) ** 2.078 + 0.479948


def form_factor(h: dict, c12fn=hc.coeff_c12) -> float:
    lr = hc.length_of_run(h["lwl"], h["cp"], h["lcb_pct"])
    return hc.coeff_c13(h["cstern"]) * (
        0.93 + c12fn(h["draft"] / h["lwl"]) * (h["beam"] / lr) ** 0.92497
        * (0.95 - h["cp"]) ** (-0.521448)
        * (1.0 - h["cp"] + 0.0225 * h["lcb_pct"]) ** 0.6906)


def friction(h: dict) -> float:
    rn = h["speed"] * h["lwl"] / hm.NU_SW
    return 0.075 / (math.log10(rn) - 2) ** 2


def banner(text: str) -> None:
    print(f"\n{'=' * 74}\n{text}\n{'=' * 74}")


# --------------------------------------------------------------------------- #
banner("CHECK 1 — the reported errors, before and after C-1 and C-2")
# --------------------------------------------------------------------------- #
for name, h in (("series60", SER), ("tanker", TNK)):
    vol = h["cb"] * h["lwl"] * h["beam"] * h["draft"]
    lr = hc.length_of_run(h["lwl"], h["cp"], h["lcb_pct"])
    S = hm.wetted_surface_holtrop(h["lwl"], h["beam"], h["draft"], h["cb"],
                                  h["cm"], h["cwp"], h["abt"])
    cf, ca = friction(h), hm.correlation_allowance(h["lwl"], h["cb"], h["draft"])
    q = 0.5 * hm.RHO_SW * h["speed"] ** 2 * S

    ie_now = hc.half_angle_of_entrance(h["lwl"], h["beam"], h["cwp"], h["cp"],
                                       h["lcb_pct"], lr, h["cb"])
    # C-2: the published term is 100*vol/L**3 (dimensionless), not 100*Cb/L.
    ie_fix = 1.0 + 89.0 * math.exp(
        -(h["lwl"] / h["beam"]) ** 0.80856
        * (1.0 - h["cwp"]) ** 0.30484
        * (1.0 - h["cp"] - 0.0225 * h["lcb_pct"]) ** 0.6367
        * (lr / h["beam"]) ** 0.34574
        * (100.0 * vol / h["lwl"] ** 3) ** 0.16302)

    cw_now = hm.wave_resistance(
        h["lwl"], h["beam"], h["draft"], h["cb"], h["cm"], h["cwp"], h["cp"],
        h["lcb_pct"], h["cstern"], h["speed"], h["abt"], h["tf"], 0.0,
        h["at"]) / q
    c7 = hc.coeff_c7(h["beam"] / h["lwl"])
    scale = ((90.0 - ie_fix) ** -1.37565) / ((90.0 - ie_now) ** -1.37565)
    _ = c7  # c7 cancels in the ratio; retained to show it was considered

    ct_now = cf * form_factor(h) + cw_now + ca
    ct_fix = cf * form_factor(h, c12_published) + cw_now * scale + ca
    print(f"  {name:9s} iE {ie_now:8.4f} -> {ie_fix:8.4f}    "
          f"(1+k1) {form_factor(h):.5f} -> {form_factor(h, c12_published):.5f}")
    print(f"  {'':9s} Ct {ct_now:.6e} -> {ct_fix:.6e}   vs fixture "
          f"{h['fixture_ct']}")
    print(f"  {'':9s} error {100 * (ct_now / h['fixture_ct'] - 1):+7.2f}% -> "
          f"{100 * (ct_fix / h['fixture_ct'] - 1):+7.2f}%")

# --------------------------------------------------------------------------- #
banner("CHECK 2 — the floor, in its strongest form (no regression involved)")
# --------------------------------------------------------------------------- #
cf, ca = friction(TNK), hm.correlation_allowance(TNK["lwl"], TNK["cb"],
                                                 TNK["draft"])
print(f"  Cf {cf:.6e} + Ca {ca:.6e} = {cf + ca:.6e}")
print(f"  fixture ct_approx = {TNK['fixture_ct']}")
print(f"  -> the floor exceeds the fixture by "
      f"{100 * ((cf + ca) / TNK['fixture_ct'] - 1):.2f}%, using only the "
      f"definition (1+k1) >= 1")
print(f"  equivalently, reaching {TNK['fixture_ct']} would require "
      f"(1+k1) <= {(TNK['fixture_ct'] - ca) / cf:.5f} — viscous resistance "
      f"below flat-plate friction")
assert cf + ca > TNK["fixture_ct"], "floor argument failed"

# --------------------------------------------------------------------------- #
banner("CHECK 3 — the third term of CA can never subtract")
# --------------------------------------------------------------------------- #
print("  CA's third term is +0.003*sqrt(L/7.5)*Cb**4*(0.04 - c4),")
print("  and c4 = min(T/L, 0.04), so (0.04 - c4) >= 0 for every hull.")
for t_over_l in (0.01, 0.02, 0.04, 0.06, 0.10):
    print(f"    T/L={t_over_l:<6} c4={hc.coeff_c4(t_over_l):.4f}  "
          f"(0.04 - c4)={0.04 - hc.coeff_c4(t_over_l):+.4f}")
    assert 0.04 - hc.coeff_c4(t_over_l) >= 0
print("  -> the negative-CA escape route is closed; the floor does not depend")
print("     on that term vanishing for these two hulls.")

# --------------------------------------------------------------------------- #
banner("CHECK 4 — sub-unity form factor, and why C-1 does not remove it")
# --------------------------------------------------------------------------- #
print("  c12 shipped vs published, by branch:")
for t in (0.02, 0.035, 0.05, 0.06):
    same = abs(hc.coeff_c12(t) - c12_published(t)) < 1e-12
    print(f"    T/L={t:<6} shipped {hc.coeff_c12(t):.6f}   published "
          f"{c12_published(t):.6f}   {'same' if same else 'DIFFER'}")
print("  They diverge only above T/L = 0.05, so a minimum at T/L = 0.02 is")
print("  unaffected by C-1 — the review's 'C-1-corrected' qualifier is wrong.")

worst = (9e9, None)
for cp100 in range(50, 90):
    for lob2 in range(8, 40):
        for tol in (0.02, 0.035, 0.05, 0.06, 0.08):
            for lcb in (-4.0, -2.0, 0.0, 2.0, 4.0):
                lwl, cp = 200.0, cp100 / 100
                h = dict(lwl=lwl, beam=lwl / (lob2 / 2), draft=tol * lwl,
                         cp=cp, lcb_pct=lcb, cstern=0)
                try:
                    if hc.length_of_run(lwl, cp, lcb) <= 0:
                        continue
                    k = form_factor(h, c12_published)
                except (ValueError, ZeroDivisionError):
                    continue
                if k < worst[0]:
                    worst = (k, (cp, lob2 / 2, tol, lcb))
print(f"  minimum (1+k1) over the sweep = {worst[0]:.5f} at "
      f"(Cp, L/B, T/L, lcb) = {worst[1]}")
print("  A form factor below 1 means viscous resistance below flat-plate")
print("  friction. Nothing in the module guards it. This is inventory row C-9.")

print("\nALL CHECKS REPRODUCED\n")
