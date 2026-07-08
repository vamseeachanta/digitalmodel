#!/usr/bin/env python
"""compare-cfd-benchmark.py — head-to-head of two 3D sloshing MPI benchmarks
(digitalmodel #1495). Reads two manifests written by run_sloshing_3d_benchmark.py
and prints a per-rank comparison + a verdict on which box is faster and scales
better, so the CFD-execution-box decision is data-backed.

    python scripts/setup/compare-cfd-benchmark.py \
        docs/api/cfd/sloshing-3d-benchmark.json \            # baseline (a-l-2)
        docs/api/cfd/sloshing-3d-benchmark-<newbox>.json     # candidate
"""
import json
import sys
from pathlib import Path


def load(p):
    m = json.loads(Path(p).read_text())
    box = m.get("meta", {}).get("box", Path(p).stem)
    cells = m.get("case", {}).get("cells")
    by = {r["ranks"]: r for r in m.get("scaling", [])
          if r.get("status") == "completed" and r.get("s_per_step")}
    return box, cells, by


def main() -> int:
    if len(sys.argv) != 3:
        print(__doc__)
        return 2
    a_box, a_cells, A = load(sys.argv[1])   # baseline
    b_box, b_cells, B = load(sys.argv[2])   # candidate
    print(f"Baseline : {a_box}  ({a_cells} cells)")
    print(f"Candidate: {b_box}  ({b_cells} cells)")
    if a_cells != b_cells:
        print(f"⚠️  cell counts differ ({a_cells} vs {b_cells}) — per-step times "
              f"are only comparable at matched mesh size; interpret with care.")
    print()
    print(f"{'cores':>5} | {'baseline s/step':>15} | {'candidate s/step':>16} | "
          f"{'speed-up':>9} | {'cand eff':>8}")
    print("-" * 66)
    ratios = []
    for n in sorted(set(A) | set(B)):
        a = A.get(n); b = B.get(n)
        asp = a["s_per_step"] if a else None
        bsp = b["s_per_step"] if b else None
        spd = (asp / bsp) if (asp and bsp) else None      # candidate speed-up vs baseline
        eff = b.get("parallel_efficiency") if b else None
        if spd:
            ratios.append((n, spd))
        print(f"{n:>5} | {('%.4f'%asp) if asp else '—':>15} | "
              f"{('%.4f'%bsp) if bsp else '—':>16} | "
              f"{('%.2fx'%spd) if spd else '—':>9} | {('%.2f'%eff) if eff else '—':>8}")
    print()
    if ratios:
        # single-core (or lowest common) raw-speed ratio, and best throughput
        lo = min(ratios, key=lambda x: x[0])
        best_b = min(((n, r["s_per_step"]) for n, r in B.items()), key=lambda x: x[1])
        best_a = min(((n, r["s_per_step"]) for n, r in A.items()), key=lambda x: x[1])
        print(f"Per-core raw speed (np={lo[0]}): candidate is {lo[1]:.2f}x the baseline.")
        print(f"Best throughput — baseline: {best_a[1]:.3f} s/step @np={best_a[0]}; "
              f"candidate: {best_b[1]:.3f} s/step @np={best_b[0]} "
              f"({best_a[1]/best_b[1]:.2f}x faster at each box's own best).")
        verdict = ("candidate wins" if best_b[1] < best_a[1] else
                   "baseline still faster" if best_b[1] > best_a[1] else "tie")
        print(f"VERDICT: {verdict}. "
              f"If the candidate is a dedicated (unshared) box, also weigh wall-clock "
              f"PREDICTABILITY, not just peak speed — that was a-l-2's real limitation.")
    else:
        print("No overlapping completed ranks to compare.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
