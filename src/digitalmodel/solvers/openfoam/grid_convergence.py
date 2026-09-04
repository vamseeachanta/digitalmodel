"""Grid convergence of a resistance coefficient from a systematic mesh triplet.

Implements the ASME V&V 20 / Roache grid convergence index (GCI) for three
meshes of representative cell size ``h1 < h2 < h3`` (fine, medium, coarse)
with refinement ratios ``r21 = h2/h1`` and ``r32 = h3/h2``, and the
Richardson-extrapolated value the sequence tends to. The observed order of
accuracy ``p`` is solved from the three values (iteratively when the ratios
are unequal), the extrapolated value from the fine pair, and the GCI on the
fine mesh with a factor of safety of 1.25 (three-mesh study).

Inputs are the settled coefficient per mesh, e.g. the mean of the final
window of ``forceCoeffs`` output, read with :func:`settled_mean`. The
representative size is the finest in-plane cell the builder records in
``case_provenance.json`` (``mesh.free_surface.in_plane_cell_size_m``), which
is what the wave resolution is set by; for a uniform refinement the ratio of
those is the ratio of every cell.

The 1 % target the B1552 programme accepts is a GCI on the MEDIUM mesh (the
one the production setting corresponds to), so :func:`gci_report` returns
both the fine-mesh GCI (the usual number) and the medium-mesh GCI.
"""

from __future__ import annotations

import json
import math
from dataclasses import dataclass, asdict
from pathlib import Path
from typing import Sequence

__all__ = ["GridConvergence", "grid_convergence", "settled_mean", "gci_report", "GridConvergenceError"]

FS_THREE_MESH = 1.25


class GridConvergenceError(ValueError):
    pass


@dataclass(frozen=True)
class GridConvergence:
    h: tuple[float, float, float]        # fine, medium, coarse representative size
    f: tuple[float, float, float]        # coefficient on fine, medium, coarse
    r21: float
    r32: float
    p: float                             # observed order of accuracy
    f_ext: float                         # Richardson extrapolation from the fine pair
    e21_pct: float                       # |f2 - f1| / |f1| in %
    e32_pct: float
    gci_fine_pct: float                  # GCI on the fine mesh (Fs 1.25)
    gci_medium_pct: float                # GCI on the medium mesh
    asymptotic_ratio: float              # gci_medium / (r21^p * gci_fine), ~1 in the asymptotic range
    monotone: bool                       # convergence is monotone (same sign of successive differences)
    oscillatory: bool

    def to_dict(self) -> dict:
        return asdict(self)


def grid_convergence(h: Sequence[float], f: Sequence[float], fs: float = FS_THREE_MESH,
                     max_iter: int = 100) -> GridConvergence:
    """Three-mesh Richardson / GCI. ``h`` and ``f`` ordered fine, medium, coarse."""
    if len(h) != 3 or len(f) != 3:
        raise GridConvergenceError("need exactly three meshes: fine, medium, coarse")
    h1, h2, h3 = (float(x) for x in h)
    f1, f2, f3 = (float(x) for x in f)
    if not (0 < h1 < h2 < h3):
        raise GridConvergenceError(f"sizes must increase fine -> coarse, got {h}")
    r21, r32 = h2 / h1, h3 / h2
    if min(r21, r32) < 1.1:
        raise GridConvergenceError(f"refinement ratios too close to 1 for a usable estimate: r21={r21:.3f}, r32={r32:.3f}")
    e21, e32 = f2 - f1, f3 - f2
    if e21 == 0 or e32 == 0:
        raise GridConvergenceError("two meshes gave identical values; the order cannot be observed")
    ratio = e32 / e21
    oscillatory = ratio < 0
    monotone = not oscillatory
    s = 1.0 if ratio > 0 else -1.0
    # p from  e32/e21 = (r21^p - 1)/(r32^p - 1) * r21^p ... solved by fixed point (Celik et al. 2008)
    p = abs(math.log(abs(ratio))) / math.log(r21)
    for _ in range(max_iter):
        q = math.log((r21 ** p - s) / (r32 ** p - s)) if r21 != r32 else 0.0
        p_new = abs(math.log(abs(ratio)) + q) / math.log(r21)
        if abs(p_new - p) < 1e-10:
            p = p_new
            break
        p = p_new
    if not math.isfinite(p) or p <= 0:
        raise GridConvergenceError(f"observed order is not positive/finite: p={p}")
    f_ext = (r21 ** p * f1 - f2) / (r21 ** p - 1.0)
    ea21 = abs(e21 / f1) if f1 else float("nan")
    ea32 = abs(e32 / f2) if f2 else float("nan")
    gci_fine = fs * ea21 / (r21 ** p - 1.0)
    gci_medium = fs * ea32 / (r32 ** p - 1.0)
    # Asymptotic-range check on ABSOLUTE errors: e32/(r32^p - 1) against
    # r21^p * e21/(r21^p - 1) is exactly 1 for a pure power law; the same
    # ratio of the relative GCIs is not, because they are normalised by
    # different f's.
    asym = (abs(e32) / (r32 ** p - 1.0)) / (r21 ** p * abs(e21) / (r21 ** p - 1.0))
    return GridConvergence(
        h=(h1, h2, h3), f=(f1, f2, f3), r21=r21, r32=r32, p=p, f_ext=f_ext,
        e21_pct=100 * ea21, e32_pct=100 * ea32,
        gci_fine_pct=100 * gci_fine, gci_medium_pct=100 * gci_medium,
        asymptotic_ratio=asym, monotone=monotone, oscillatory=oscillatory,
    )


def settled_mean(coefficient_dat: Path, column: str = "Cd", window: int = 400) -> tuple[float, float, int]:
    """Mean and drift of the last ``window`` rows of a forceCoeffs ``coefficient.dat``.

    Returns (mean over the last window, two-window drift in %, rows). The
    drift is |mean(last window) - mean(previous window)| / mean(last window):
    the settling check the free-surface gate applies (0.2 % per component).
    """
    lines = [l for l in Path(coefficient_dat).read_text().splitlines() if l.strip()]
    header = [l for l in lines if l.startswith("#") and column in l]
    if not header:
        raise GridConvergenceError(f"{coefficient_dat}: no header naming column {column!r}")
    names = header[-1].lstrip("#").split()
    col = names.index(column)
    rows = [l.split() for l in lines if not l.startswith("#")]
    if len(rows) < 2 * window:
        raise GridConvergenceError(f"{coefficient_dat}: {len(rows)} rows, need {2 * window} for two windows")
    vals = [float(r[col]) for r in rows]
    last = sum(vals[-window:]) / window
    prev = sum(vals[-2 * window:-window]) / window
    drift = abs(last - prev) / abs(last) * 100 if last else float("nan")
    return last, drift, len(rows)


def gci_report(cases: Sequence[Path], column: str = "Cd", window: int = 400,
               coeff_relpath: str = "postProcessing/forceCoeffs/0/coefficient.dat") -> dict:
    """Assemble a triplet report from three case directories (any order).

    Reads each case's in-plane cell from ``case_provenance.json`` and the
    settled coefficient from its forceCoeffs output; sorts fine -> coarse.
    """
    rows = []
    for case in cases:
        case = Path(case)
        prov = json.loads((case / "case_provenance.json").read_text())
        h = prov["mesh"]["free_surface"]["in_plane_cell_size_m"]
        cells = prov["mesh"].get("estimated_cells")
        mean, drift, n = settled_mean(case / coeff_relpath, column=column, window=window)
        rows.append(dict(case=case.name, h_m=h, cells_est=cells, value=mean, drift_pct=drift, rows=n,
                         cells_per_wavelength=prov["mesh"]["free_surface"].get("cells_per_wavelength")))
    rows.sort(key=lambda r: r["h_m"])
    gc = grid_convergence([r["h_m"] for r in rows], [r["value"] for r in rows])
    return {"column": column, "window": window, "meshes": rows, "gci": gc.to_dict()}
