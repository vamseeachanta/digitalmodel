# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Compare MYSTRAN results with NAFEMS LE1 and the constant-stress patch test; fixed FE10 criteria.

Offline, standard library only. Fixed FE10 criteria: LE1 within 5 % on the
fine benchmark mesh (project default); patch test exact within 0.1 %. Differences use unrounded values.

LE1 result file (element centroid stresses at the mid-surface, i.e. the mean of the two fibre values):
    # program: MYSTRAN <version>
    # mesh: le1_32x96            (one of the decks in inputs/)
    eid,sigma_yy_MPa
    <one row per element; at least elements 1 and nt + 1, the two innermost elements of the y = 0 row>
sigma_yy at point D (2000, 0) is taken by linear extrapolation in x of the centroid values of those two
elements to x = 2000 mm (method proposed for maintainer acceptance); the innermost centroid
value is printed for information.

Patch result file:
    # program: MYSTRAN <version>
    eid,sigma_xx_MPa,sigma_yy_MPa,tau_xy_MPa
    <rows for elements 1 to 5>

Usage:  python -I compare.py --le1 results-le1_32x96.csv
        python -I compare.py --patch results-patch.csv
        python -I compare.py --selftest
Exit code 0 when every compared quantity passes, 1 otherwise, 2 on an unreadable input.
"""
import argparse
import csv
import io
import json
import math
import sys
from pathlib import Path
from typing import TextIO

HERE = Path(__file__).resolve().parent
EXP = json.loads((HERE / "expected.json").read_text(encoding="utf-8"))


def round_up(x: float, nd: int = 1) -> float:
    """Round a reported upper bound upward."""
    f = 10 ** nd
    return float(math.ceil(x * f - 1e-9) / f)


def read(text: str) -> tuple[dict[str, str], list[dict[str, str]]]:
    """Read metadata comments and the stress CSV body."""
    meta, body = {}, []
    for line in text.splitlines():
        s = line.strip()
        if s.startswith("#"):
            if ":" in s:
                k, v = s[1:].split(":", 1)
                meta[k.strip()] = v.strip()
        elif s:
            body.append(s)
    return meta, list(csv.DictReader(io.StringIO("\n".join(body))))


def centroids(deck: Path) -> dict[int, tuple[float, float]]:
    """Calculate the supplied deck's four-corner centroid positions."""
    grids, cen = {}, {}
    for l in deck.read_text(encoding="utf-8").splitlines():
        fl = l.split(",")
        if fl[0] == "GRID":
            grids[int(fl[1])] = (float(fl[3]), float(fl[4]))
        elif fl[0] == "CQUAD4":
            g = [grids[int(x)] for x in fl[3:7]]
            cen[int(fl[1])] = (sum(p[0] for p in g) / 4, sum(p[1] for p in g) / 4)
    return cen


def unique_rows(rows: list[dict[str, str]]) -> dict[int, dict[str, str]]:
    """Reject ambiguous element results before any row can be overwritten."""
    indexed: dict[int, dict[str, str]] = {}
    for row in rows:
        eid = int(row["eid"])
        if eid in indexed:
            raise ValueError("duplicate element ID %d" % eid)
        indexed[eid] = row
    return indexed


def le1(meta: dict[str, str], rows: list[dict[str, str]], out: TextIO = sys.stdout) -> str:
    """Compare the unchanged two-centroid extrapolation with the LE1 band."""
    mesh = meta.get("mesh", "")
    deck = HERE / "inputs" / (mesh + ".bdf")
    if not mesh.startswith("le1_") or not deck.exists():
        raise ValueError("'# mesh:' must name a deck in inputs/ (le1_8x24, le1_16x48 or le1_32x96)")
    nr, nt = (int(x) for x in mesh[4:].split("x"))
    cen = centroids(deck)
    s = {eid: float(row["sigma_yy_MPa"]) for eid, row in unique_rows(rows).items()}
    e1, e2 = 1, nt + 1
    if e1 not in s or e2 not in s:
        raise ValueError("elements %d and %d are required" % (e1, e2))
    (x1, _), (x2, _) = cen[e1], cen[e2]
    sd = s[e1] + (s[e2] - s[e1]) * (2000.0 - x1) / (x2 - x1)
    ref = EXP["le1"]["value_MPa"]
    band = EXP["le1"]["band_pct"]
    d = (sd - ref) / ref * 100
    ok = abs(sd - ref) <= band / 100 * abs(ref)
    print("program: %s; mesh %s (%d radial x %d hoop CQUAD4); FE10 band %.1f %% (fixed benchmark criteria)" % (meta.get("program", "not stated"), mesh, nr, nt, band), file=out)
    print("sigma_yy at D, extrapolated from elements %d (x = %.1f mm) and %d (x = %.1f mm): %.2f MPa; published %.1f MPa (%s); difference %.1f %%  %s"
          % (e1, x1, e2, x2, sd, ref, EXP["le1"]["cited_as"], d, "PASS" if ok else "FAIL"), file=out)
    print("information: innermost centroid value %.2f MPa (%.1f %% from the published value)" % (s[e1], (s[e1] - ref) / ref * 100), file=out)
    v = "PASS" if ok else "FAIL"
    print("SUMMARY %s: sigma_yy(D) within %.1f %% of the published value" % (v, round_up(abs(d))), file=out)
    return v


def patch(meta: dict[str, str], rows: list[dict[str, str]], out: TextIO = sys.stdout) -> str:
    """Grade each component of every patch element against its exact stress."""
    exp = EXP["patch"]["expected_MPa"]
    tol = EXP["patch"]["band_pct"]
    got = unique_rows(rows)
    n_ok = n_bad = 0
    worst = 0.0
    print("program: %s; patch test, FE10 band %.1f %% (fixed benchmark criteria)" % (meta.get("program", "not stated"), tol), file=out)
    for e in range(1, 6):
        for k in ("sigma_xx_MPa", "sigma_yy_MPa", "tau_xy_MPa"):
            if e not in got:
                n_bad += 1
                print("element %d %-14s missing  FAIL" % (e, k), file=out)
                continue
            c, x = float(got[e][k]), exp[k]
            d = (c - x) / x * 100
            ok = abs(c - x) <= tol / 100 * abs(x)
            worst = max(worst, abs(d))
            n_ok += ok
            n_bad += not ok
            print("element %d %-14s %10.4f expected %10.4f  diff %.1f %%  %s" % (e, k, c, x, d, "PASS" if ok else "FAIL"), file=out)
    v = "PASS" if n_bad == 0 else "FAIL"
    print("SUMMARY %s: %d compared, %d FAIL; all within %.1f %%" % (v, n_ok + n_bad, n_bad, round_up(worst)), file=out)
    return v


def selftest() -> int:
    """Exercise synthetic passing and failing comparisons."""
    sink = io.StringIO()
    exp = EXP["patch"]["expected_MPa"]

    def prow(scale: float) -> str:
        return "# program: synthetic self-test input (not a result)\neid,sigma_xx_MPa,sigma_yy_MPa,tau_xy_MPa\n" + "\n".join(
            "%d,%r,%r,%r" % (e, exp["sigma_xx_MPa"] * scale, exp["sigma_yy_MPa"] * scale, exp["tau_xy_MPa"] * scale) for e in range(1, 6))
    cen = centroids(HERE / "inputs" / "le1_32x96.bdf")
    x1, x2 = cen[1][0], cen[97][0]

    def lrow(sd: float, slope: float = -0.02) -> str:
        s1, s2 = sd + slope * (x1 - 2000.0), sd + slope * (x2 - 2000.0)
        return "# program: synthetic self-test input (not a result)\n# mesh: le1_32x96\neid,sigma_yy_MPa\n1,%r\n97,%r" % (s1, s2)
    ref = EXP["le1"]["value_MPa"]
    r = [patch(*read(prow(1.0)), out=sink), patch(*read(prow(1.002)), out=sink), patch(*read(prow(1.0009)), out=sink),
         le1(*read(lrow(ref * 1.049)), out=sink), le1(*read(lrow(ref * 1.06)), out=sink), le1(*read(lrow(ref)), out=sink)]
    want = ["PASS", "FAIL", "PASS", "PASS", "FAIL", "PASS"]
    print("selftest: patch exact %s, +0.2 %% %s, +0.09 %% %s; LE1 +4.9 %% %s, +6 %% %s, exact (extrapolated) %s (want %s) -> %s"
          % (*r, "/".join(want), "SELFTEST PASS" if r == want else "SELFTEST FAIL"))
    return 0 if r == want else 1


def main() -> None:
    """Run the requested comparison or synthetic selftest."""
    ap = argparse.ArgumentParser()
    ap.add_argument("--le1")
    ap.add_argument("--patch")
    ap.add_argument("--selftest", action="store_true")
    a = ap.parse_args()
    if a.selftest:
        sys.exit(selftest())
    try:
        if a.le1:
            v = le1(*read(Path(a.le1).read_text(encoding="utf-8").removeprefix("\ufeff")))
        elif a.patch:
            v = patch(*read(Path(a.patch).read_text(encoding="utf-8").removeprefix("\ufeff")))
        else:
            ap.error("--le1, --patch or --selftest")
    except (OSError, ValueError, KeyError) as err:
        print("unreadable result: %s" % err)
        sys.exit(2)
    sys.exit(0 if v == "PASS" else 1)


if __name__ == "__main__":
    main()
