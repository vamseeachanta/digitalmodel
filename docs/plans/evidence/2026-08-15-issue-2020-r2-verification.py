#!/usr/bin/env python3
"""Reproduces every numeric claim added to the #2020 plan at revision 3.

Run from the repository root:

    uv run --quiet --no-sources python docs/plans/evidence/2026-08-15-issue-2020-r2-verification.py

Note the ``--no-sources``. That flag is itself one of the findings: pyproject
pins ``assetutilities`` to a sibling checkout, so without it ``uv`` aborts
before collection in any clone of this repository alone. See CHECK 5.

Companion to ``2026-08-14-issue-2020-r1-verification.py``, which covers the
round-1 figures. This script covers the figures that answer adversarial review
round 2. Three of these checks correct a claim rather than confirm one, which
is why the script is committed rather than left in a scratch directory:

  * CHECK 1 refutes revision 2's justification for C-6's activating condition.
    Revision 2 stated that "ABT > 0 alone leaves c2 = 1". It does not: any
    positive ABT gives c3 > 0 and therefore c2 < 1. The condition survives for
    a different reason (T/L >= 0.04 zeroes the whole term), but the stated
    reason was backwards.

  * CHECK 2 shows two activating conditions the plan asserted were sufficient
    are not. C-4's defect is invisible when CM == CWP, and C-7's function
    returns early before the code under test whenever the bulb-immersion depth
    is non-positive.

  * CHECK 4 is the important one. It shows the sensitivity test the plan
    proposed CANNOT fail on today's engine. The near-identity this issue
    reports is an artifact of the two fixture hulls running at the same SPEED
    (7.72 m/s, hence different Froude numbers). The plan's own fixed-Froude
    convention removes it. Revision 2's claim that "any defensible threshold
    fails on today's code" is therefore false, and is withdrawn.

Every value printed here appears in the revision-3 plan or in the round-2
response comment on the issue.
"""
from __future__ import annotations

import inspect
import json
import math
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "src"))

from digitalmodel.naval_architecture import holtrop_coefficients as hc  # noqa: E402
from digitalmodel.naval_architecture.holtrop_mennen import (  # noqa: E402
    G,
    RHO_SW,
    form_factor_k1,
    total_resistance,
    wave_resistance,
    wetted_surface_holtrop,
)


def rule(title: str) -> None:
    print(f"\n{'=' * 72}\n{title}\n{'=' * 72}")


# ---------------------------------------------------------------------------
# CHECK 1 -- C-6's activating condition: the justification was backwards.
# ---------------------------------------------------------------------------
def check_c2_vs_abt() -> None:
    rule("CHECK 1  c2 as a function of ABT  (round 2 finding 4)")
    print("Geometry stated explicitly, because c3 depends on B, T and TF as well")
    print("as ABT -- round 2 reported 0.934505 at ABT=5.0 without naming its hull,")
    print("so the magnitude is not reproducible from the review text. The")
    print("DIRECTION is what the finding rests on, and it reproduces at any hull.\n")
    beam, draft, tf = 32.0, 20.5, 20.5
    print(f"  B={beam}  T={draft}  TF={tf}\n")
    for abt in (0.0, 5.0, 20.0):
        c3 = hc.coeff_c3(abt, beam, draft, tf)
        print(f"  ABT={abt:5.1f}   c3={c3:.6f}   c2={hc.coeff_c2(c3):.6f}")
    print("\n  -> ABT > 0 gives c3 > 0 and therefore c2 < 1.")
    print("     Revision 2 said 'ABT > 0 alone leaves c2 = 1'. Backwards.")
    print("     It is ZERO ABT that makes c2 exactly 1, so the omitted factor")
    print("     is the identity and C-6's term is correct by accident.")


# ---------------------------------------------------------------------------
# CHECK 2 -- two activating conditions that do not activate.
# ---------------------------------------------------------------------------
def check_defeatable_conditions() -> None:
    rule("CHECK 2  activating conditions that exercise nothing  (round 2 finding 4)")

    print("C-4  coeff_c5 substitutes CWP where the published form uses CM.")
    print("     AT > 0 alone is NOT sufficient: with CM == CWP the defective")
    print("     line executes and produces the published answer.\n")
    at, beam, draft = 30.0, 32.0, 20.5
    for cwp, cm in ((0.85, 0.85), (0.85, 0.98)):
        impl = hc.coeff_c5(at, beam, draft, cwp)
        published = 1.0 - 0.8 * at / (beam * draft * cm)
        print(
            f"  CWP={cwp}  CM={cm}   implemented={impl:.8f}  "
            f"published={published:.8f}   delta={impl - published:+.3e}"
        )
    print("\n  -> condition must be  AT > 0  AND  CM != CWP.")

    print("\nC-7  bulbous_bow_resistance returns early before fn_i is ever formed.")
    print("     holtrop_mennen.py:131 computes depth = tf - hb - 0.25*sqrt(abt);")
    print("     line 132 returns 0.0 when that is non-positive.\n")
    for tf, hb, abt in ((10.0, 4.0, 20.0), (10.0, 9.5, 20.0), (2.0, 0.5, 20.0)):
        depth = tf - hb - 0.25 * math.sqrt(abt)
        verdict = "reaches fn_i" if depth > 0.0 else "EARLY RETURN 0.0"
        print(f"  TF={tf:5.1f} hB={hb:4.1f} ABT={abt:5.1f}   depth={depth:+.4f}   {verdict}")
    print("\n  -> condition must add  TF - hB - 0.25*sqrt(ABT) > 0.")

    print("\nC-8  wave_resistance enforces no Froude range at all:")
    has_conditional = "if" in inspect.getsource(wave_resistance)
    print(f"  'if' appears in wave_resistance source: {has_conditional}")
    print("  -> nothing rejects Fn >= 0.4; the boundary Fn == 0.4 needs its own case.")


# ---------------------------------------------------------------------------
# CHECK 3 -- the explorer's true maximum Froude number.
# ---------------------------------------------------------------------------
def check_explorer_fn() -> None:
    rule("CHECK 3  explorer maximum Froude number  (round 2 NEW-6)")
    path = REPO_ROOT / "docs" / "api" / "structural" / "ship-resistance-explorer.json"
    if not path.exists():
        print(f"  SKIP -- artifact not found at {path.relative_to(REPO_ROOT)}")
        return
    data = json.loads(path.read_text())
    found: list[float] = []

    def walk(node: object) -> None:
        if isinstance(node, dict):
            for key, value in node.items():
                if key == "fn" and isinstance(value, (int, float)):
                    found.append(float(value))
                walk(value)
        elif isinstance(node, list):
            for value in node:
                walk(value)

    walk(data)
    print(f"  samples={len(found)}   min={min(found)}   max={max(found)}")
    print(f"  meta.standard = {data.get('meta', {}).get('standard')!r}")
    print("\n  -> revisions 1 and 2 said 'Fn = 0.224 at most'. The committed")
    print("     artifact carries 0.246. The conclusion survives (0.246 < 0.4),")
    print("     but 0.224 was the series60 fixture row's Fn carried across.")


# ---------------------------------------------------------------------------
# CHECK 4 -- the sensitivity test cannot fail today.
# ---------------------------------------------------------------------------
FIXTURE_PAIR = (
    dict(id="series60", lwl=121.92, beam=16.26, draft=6.50, cb=0.600,
         cm=0.977, cwp=0.700, cp=0.614, lcb=-2.02, cstern=0),
    dict(id="tanker", lwl=205.00, beam=32.00, draft=12.00, cb=0.800,
         cm=0.995, cwp=0.880, cp=0.804, lcb=-0.50, cstern=0),
)

# The four hulls the revision-3 plan declares for the sensitivity guard.
DECLARED_SET = (
    dict(id="A1-fine", lwl=200.0, beam=25.0, draft=10.0, cb=0.55,
         cm=0.98, cwp=0.72, cp=0.55 / 0.98, lcb=-1.0, cstern=0),
    dict(id="A2", lwl=180.0, beam=26.0, draft=10.5, cb=0.65,
         cm=0.98, cwp=0.78, cp=0.65 / 0.98, lcb=-1.0, cstern=0),
    dict(id="A3", lwl=170.0, beam=28.0, draft=11.0, cb=0.75,
         cm=0.99, cwp=0.84, cp=0.75 / 0.99, lcb=-1.0, cstern=0),
    dict(id="A4-full", lwl=160.0, beam=30.0, draft=11.5, cb=0.85,
         cm=0.99, cwp=0.90, cp=0.85 / 0.99, lcb=-1.0, cstern=0),
)


def ct_at_speed(hull: dict, speed_ms: float) -> float:
    """Total resistance coefficient at a given speed, bare hull."""
    surface = wetted_surface_holtrop(
        hull["lwl"], hull["beam"], hull["draft"],
        hull["cb"], hull["cm"], hull["cwp"], 0.0,
    )
    resistance = total_resistance(
        hull["lwl"], hull["beam"], hull["draft"],
        hull["cb"], hull["cm"], hull["cwp"], hull["cp"],
        hull["lcb"], hull["cstern"], speed_ms,
        0.0, hull["draft"], 0.0, 0.0,
    )
    return resistance / (0.5 * RHO_SW * speed_ms ** 2 * surface)


def ct_at_froude(hull: dict, fn: float) -> float:
    return ct_at_speed(hull, fn * math.sqrt(G * hull["lwl"]))


def check_sensitivity_cannot_fail() -> None:
    rule("CHECK 4  the sensitivity test cannot fail today  (round 2 finding 5)")

    print("4a. The shipped condition -- BOTH fixture hulls at the SAME SPEED.")
    print("    Note the Froude numbers differ. This is where the reported")
    print("    near-identity lives.\n")
    speed = 7.72
    shipped = []
    for hull in FIXTURE_PAIR:
        ct = ct_at_speed(hull, speed)
        shipped.append(ct)
        fn = speed / math.sqrt(G * hull["lwl"])
        print(f"  {hull['id']:10s} V={speed} m/s  Fn={fn:.6f}  Ct={ct:.6e}")
    sep = abs(shipped[1] - shipped[0]) / min(shipped) * 100.0
    print(f"\n  -> separation {sep:.5f}%   (the 4-parts-in-10^5 figure)")
    print("     These two Ct values also reproduce the plan's own component")
    print("     table exactly, which validates this harness before it is used")
    print("     for anything new.")

    print("\n4b. The SAME two hulls at a COMMON Froude number -- the convention")
    print("    the plan's own experimental design adopted.\n")
    for fn in (0.150, 0.172, 0.200, 0.224, 0.250):
        pair = [ct_at_froude(hull, fn) for hull in FIXTURE_PAIR]
        sep = abs(pair[1] - pair[0]) / min(pair) * 100.0
        print(
            f"  Fn={fn:.3f}   series60={pair[0]:.6e}   tanker={pair[1]:.6e}"
            f"   separation={sep:8.3f}%"
        )
    print("\n  -> the coincidence does not survive the change of convention.")
    print("     At the loosest point measured the hulls already separate by")
    print("     roughly 850x the reported near-identity.")

    print("\n4c. The four hulls the revision-3 plan declares, at fixed Froude.\n")
    for fn in (0.15, 0.20, 0.25):
        values = []
        for hull in DECLARED_SET:
            ct = ct_at_froude(hull, fn)
            k1 = form_factor_k1(
                hull["lwl"], hull["beam"], hull["draft"],
                hull["cp"], hull["lcb"], hull["cstern"],
            )
            values.append((hull["id"], ct, k1))
        print(f"  --- Fn = {fn} ---")
        for name, ct, k1 in values:
            print(f"    {name:10s} 1+k1={k1:.5f}   Ct={ct:.6e}")
        cts = [v[1] for v in values]
        ordered = sorted(cts)
        gaps = [(ordered[i + 1] - ordered[i]) / ordered[i] * 100.0
                for i in range(len(ordered) - 1)]
        spread = (max(cts) - min(cts)) / min(cts) * 100.0
        monotone = all(cts[i] < cts[i + 1] for i in range(len(cts) - 1))
        print(
            f"    spread={spread:8.3f}%   tightest adjacent pair={min(gaps):7.4f}%"
            f"   Ct monotone in CB: {monotone}"
        )
    print("\n  -> today's UNCORRECTED engine separates the declared set by")
    print("     47.6%-265.7%. The proposed test passes comfortably at any")
    print("     threshold that is not absurd.")
    print("\n  CONCLUSION: test_ct_separation_across_hull_forms_at_fixed_froude")
    print("  cannot be a red test. Revision 2's claim that 'any defensible")
    print("  threshold fails on today's code' took a figure measured on the")
    print("  fixture pair at common SPEED and attributed it to a four-hull set")
    print("  at fixed FROUDE. Different experiments, different answers. The")
    print("  test is reclassified as a guard with a mutation-based proof.")


# ---------------------------------------------------------------------------
# CHECK 5 -- the environment finding, reported rather than executed.
# ---------------------------------------------------------------------------
def check_standalone_clone() -> None:
    rule("CHECK 5  the canonical command in a standalone clone  (round 2 NEW-1)")
    sibling = REPO_ROOT.parent / "assetutilities"
    print(f"  sibling 'assetutilities' present beside this repo: {sibling.exists()}")
    print("""
  Measured on a clone with NO sibling present:

    uv run --with-editable '.[test]' python -m pytest
      -> error: Distribution not found at: file:///.../assetutilities
         (aborts BEFORE collection)

    uv run --no-sources --with-editable '.[test]' python -m pytest \\
        tests/naval_architecture/test_holtrop_mennen.py
      -> 20 passed

    uv run --no-sources --with-editable '.[test]' python -m pytest \\
        tests/workflows/test_durable_workflows.py
      -> 131 passed, 3 skipped        (the blocker-1 file)

    uv run --no-sources --with-editable '.[test]' python -m pytest --collect-only
      -> 21876 collected, 8 errors, all in tests/workflow_api/
         ModuleNotFoundError: No module named 'assetutilities.workflow_api'
         (the indexed release lacks the submodule the sibling provides)

  -> the plan's declared verification scope IS runnable standalone under
     --no-sources. The residual is bounded, unrelated to resistance, and is
     established as pre-existing by the AC 7 baseline run rather than by
     assertion. The pyproject path pin itself is filed as a follow-on.

  This check is reported rather than executed because reproducing it
  requires a checkout without the sibling, which this script cannot
  create for itself.""")


# ---------------------------------------------------------------------------
# CHECK 6 -- version claims across the repository.
# ---------------------------------------------------------------------------
def check_version_labels() -> None:
    rule("CHECK 6  version-bearing labels  (round 2 NEW-3)")
    targets = [
        Path("src/digitalmodel/naval_architecture/holtrop_mennen.py"),
        Path("src/digitalmodel/naval_architecture/holtrop_coefficients.py"),
        Path("tests/naval_architecture/test_holtrop_mennen.py"),
        Path("scripts/capabilities/build_ship_resistance_explorer.py"),
    ]
    total = 0
    for rel in targets:
        path = REPO_ROOT / rel
        if not path.exists():
            print(f"  SKIP {rel} -- not found")
            continue
        hits = [
            (n, line.strip())
            for n, line in enumerate(path.read_text().splitlines(), 1)
            if "1984" in line
        ]
        total += len(hits)
        print(f"\n  {rel}  ({len(hits)} claims)")
        for n, line in hits:
            print(f"    {n:>4}: {line[:96]}")
    print(f"\n  -> {total} version claims across {len(targets)} files.")
    print("     Round 2 reported three in holtrop_coefficients.py; there are six,")
    print("     and three of them cite a specific equation in a specific paper,")
    print("     which is a stronger claim than a file header.")


def main() -> int:
    print("#2020 revision-3 verification -- adversarial review round 2")
    print(f"repository root: {REPO_ROOT.name}")
    check_c2_vs_abt()
    check_defeatable_conditions()
    check_explorer_fn()
    check_sensitivity_cannot_fail()
    check_standalone_clone()
    check_version_labels()
    print("\nDone.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
