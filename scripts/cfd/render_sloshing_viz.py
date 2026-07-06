#!/usr/bin/env python
"""Render free-surface (VOF) field visualizations from the sloshing CFD cases (#1437).

Turns the committed OpenFOAM outputs into authentic CFD imagery for the study page:
an animation of the resonant forced-roll tank (moving mesh — the tank visibly rolls
and the water sloshes), a phase-snapshot montage over a roll cycle, and a free-decay
standing-wave montage. Rendered directly from the ``alpha.water`` VOF field
(0 = air, 1 = water; interface at alpha = 0.5) on the actual (moved) mesh — nothing
mocked. Water is a single sequential blue (light air -> navy water), matching the
page's visual system.

    uv run python scripts/cfd/render_sloshing_viz.py --work-dir /mnt/local-analysis/sloshing_cfd_work

Outputs (small, optimised) under docs/api/cfd/viz/.
"""
from __future__ import annotations

import argparse
import io
import re
from pathlib import Path
from typing import List, Optional, Tuple

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.colors import LinearSegmentedColormap
from PIL import Image

_REPO = Path(__file__).resolve().parents[2]
_OUT = _REPO / "docs" / "api" / "cfd" / "viz"

# Sequential single-hue ramp: pale air -> navy water (page palette).
WATER = LinearSegmentedColormap.from_list("water", ["#eef3fa", "#9db8de", "#3f6bb0", "#0B3D91"])
WALL = "#13233f"
IFACE = "#0a2a5e"


# --------------------------------------------------------------------------- #
# OpenFOAM ascii parsers
# --------------------------------------------------------------------------- #


def _parse_scalar(path: Path, ncells: int) -> Optional[np.ndarray]:
    txt = path.read_text()
    i = txt.find("internalField")
    seg = txt[i:i + 60]
    if "nonuniform" not in seg:
        m = re.search(r"uniform\s+([-\d.eE]+)", seg)
        return np.full(ncells, float(m.group(1))) if m else None
    m = re.search(r"nonuniform[^\n]*\n\s*(\d+)\s*\n\(", txt[i:])
    if not m:
        return None
    n = int(m.group(1))
    after = txt[i + m.end():]
    end = after.find(")")
    vals = np.fromstring(after[:end].replace("\n", " "), sep=" ")
    return vals[:n] if vals.size >= n else None


def _parse_points(path: Path) -> np.ndarray:
    txt = path.read_text()
    m = re.search(r"\n\s*(\d+)\s*\n\(", txt)
    after = txt[m.end():]
    end = after.find("\n)")
    tup = re.findall(r"\(\s*([-\d.eE]+)\s+([-\d.eE]+)\s+([-\d.eE]+)\s*\)", after[:end])
    return np.asarray(tup, dtype=float)


def _grid(points_path: Path, nx: int, ny: int) -> Tuple[np.ndarray, np.ndarray]:
    """z=0 layer of a structured (nx x ny) blockMesh -> corner grids X,Y (ny+1,nx+1)."""
    pts = _parse_points(points_path)
    layer = pts[: (nx + 1) * (ny + 1)]              # first z-layer (x fastest, then y)
    X = layer[:, 0].reshape(ny + 1, nx + 1)
    Y = layer[:, 1].reshape(ny + 1, nx + 1)
    return X, Y


def _times(case: Path) -> List[float]:
    ts = []
    for d in case.iterdir():
        if d.is_dir() and re.fullmatch(r"\d+(\.\d+)?", d.name) and d.name != "0":
            ts.append(float(d.name))
    return sorted(ts)


def _tfmt(t: float) -> str:
    s = f"{t:.6f}".rstrip("0").rstrip(".")
    return s


# --------------------------------------------------------------------------- #
# Rendering
# --------------------------------------------------------------------------- #


def _frame(ax, case: Path, t: float, nx: int, ny: int, moved: bool,
           lim: Tuple[float, float, float, float], label: str = "") -> None:
    td = case / _tfmt(t)
    pts_path = (td / "polyMesh" / "points") if (moved and (td / "polyMesh" / "points").exists()) \
        else (case / "constant" / "polyMesh" / "points")
    X, Y = _grid(pts_path, nx, ny)
    alpha = _parse_scalar(td / "alpha.water", nx * ny).reshape(ny, nx)
    ax.clear()
    ax.pcolormesh(X, Y, alpha, cmap=WATER, vmin=0, vmax=1, shading="flat")
    Xc = 0.25 * (X[:-1, :-1] + X[1:, :-1] + X[:-1, 1:] + X[1:, 1:])
    Yc = 0.25 * (Y[:-1, :-1] + Y[1:, :-1] + Y[:-1, 1:] + Y[1:, 1:])
    ax.contour(Xc, Yc, alpha, levels=[0.5], colors=IFACE, linewidths=1.3)
    for ex, ey in ((X[0, :], Y[0, :]), (X[-1, :], Y[-1, :]), (X[:, 0], Y[:, 0]), (X[:, -1], Y[:, -1])):
        ax.plot(ex, ey, color=WALL, lw=2.2, solid_capstyle="round")
    ax.set_xlim(lim[0], lim[1]); ax.set_ylim(lim[2], lim[3])
    ax.set_aspect("equal"); ax.axis("off")
    if label:
        ax.text(0.03, 0.97, label, transform=ax.transAxes, va="top", ha="left",
                fontsize=10, color=WALL, fontweight="bold")


def _limits(case: Path, times: List[float], nx: int, ny: int, moved: bool) -> Tuple[float, float, float, float]:
    xs, ys = [], []
    for t in (times[0], times[len(times) // 2], times[-1]):
        td = case / _tfmt(t)
        p = (td / "polyMesh" / "points") if (moved and (td / "polyMesh" / "points").exists()) \
            else (case / "constant" / "polyMesh" / "points")
        X, Y = _grid(p, nx, ny)
        xs += [X.min(), X.max()]; ys += [Y.min(), Y.max()]
    mx = 0.05 * (max(xs) - min(xs))
    return (min(xs) - mx, max(xs) + mx, min(ys) - mx, max(ys) + mx)


def make_gif(case: Path, out: Path, nx: int, ny: int, *, moved: bool,
             t0: float, t1: float, period: float, fps: int = 12) -> None:
    times = [t for t in _times(case) if t0 - 1e-6 <= t <= t1 + 1e-6]
    lim = _limits(case, times, nx, ny, moved)
    fig, ax = plt.subplots(figsize=(4.2, 4.2), dpi=105)
    fig.subplots_adjust(0, 0, 1, 1)
    frames = []
    for t in times:
        ph = (t - t0) / period
        _frame(ax, case, t, nx, ny, moved, lim, label=f"t = {t:.2f} s   ({ph:.2f} roll cycles)")
        buf = io.BytesIO(); fig.savefig(buf, format="png"); buf.seek(0)
        frames.append(Image.open(buf).convert("P", palette=Image.ADAPTIVE, colors=128))
    plt.close(fig)
    out.parent.mkdir(parents=True, exist_ok=True)
    frames[0].save(out, save_all=True, append_images=frames[1:],
                   duration=int(1000 / fps), loop=0, optimize=True)
    print(f"wrote {out.relative_to(_REPO)} ({len(frames)} frames, {out.stat().st_size//1024} KB)")


def _snap(case: Path, t: float) -> float:
    """Nearest actually-written time directory to ``t``."""
    avail = _times(case)
    return min(avail, key=lambda a: abs(a - t))


def make_montage(case: Path, out: Path, nx: int, ny: int, *, moved: bool,
                 times: List[float], labels: List[str], title: str = "") -> None:
    times = [_snap(case, t) for t in times]
    lim = _limits(case, times, nx, ny, moved)
    n = len(times)
    fig, axes = plt.subplots(1, n, figsize=(2.5 * n, 2.7), dpi=115)
    for ax, t, lb in zip(axes, times, labels):
        _frame(ax, case, t, nx, ny, moved, lim, label=lb)
    fig.subplots_adjust(0.005, 0.02, 0.995, 0.98, wspace=0.04)
    out.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out, dpi=115)
    plt.close(fig)
    print(f"wrote {out.relative_to(_REPO)} ({out.stat().st_size//1024} KB)")


def make_effect_sidebyside(left: Path, right: Path, out_gif: Path, out_png: Path, *,
                           lnx: int, lny: int, rnx: int, rny: int,
                           t0: float, t1: float, period: float, fps: int = 12) -> None:
    """Side-by-side 'effect of roll': left = forced roll (sloshing), right = no roll
    (calm), same tank/fill. Writes an animation and a single peak-vs-calm still."""
    ltimes = [t for t in _times(left) if t0 - 1e-6 <= t <= t1 + 1e-6]
    llim = _limits(left, ltimes, lnx, lny, True)
    rlim = _limits(right, _times(right), rnx, rny, False)

    def draw(axL, axR, t):
        _frame(axL, left, t, lnx, lny, True, llim)
        axL.set_title("WITH ROLL", fontsize=11, color="#c0392b", fontweight="bold", pad=5)
        _frame(axR, right, _snap(right, t), rnx, rny, False, rlim)
        axR.set_title("WITHOUT ROLL", fontsize=11, color=WALL, fontweight="bold", pad=5)

    fig, (axL, axR) = plt.subplots(1, 2, figsize=(7.8, 4.1), dpi=105)
    fig.subplots_adjust(0.01, 0.02, 0.99, 0.90, wspace=0.06)
    frames = []
    for t in ltimes:
        draw(axL, axR, t)
        fig.suptitle(f"effect of forced roll  ·  same tank, h/L = 0.70  ·  t = {t:.2f} s",
                     fontsize=10, color="#5a6b7b", y=0.985)
        buf = io.BytesIO(); fig.savefig(buf, format="png"); buf.seek(0)
        frames.append(Image.open(buf).convert("P", palette=Image.ADAPTIVE, colors=128))
    out_gif.parent.mkdir(parents=True, exist_ok=True)
    frames[0].save(out_gif, save_all=True, append_images=frames[1:],
                   duration=int(1000 / fps), loop=0, optimize=True)
    print(f"wrote {out_gif.relative_to(_REPO)} ({len(frames)} frames, {out_gif.stat().st_size//1024} KB)")
    # still at the most-tilted phase (quarter period after t0)
    draw(axL, axR, _snap(left, t0 + 0.25 * period))
    fig.suptitle("effect of forced roll  ·  same tank, h/L = 0.70", fontsize=10, color="#5a6b7b", y=0.985)
    fig.savefig(out_png, dpi=115)
    plt.close(fig)
    print(f"wrote {out_png.relative_to(_REPO)} ({out_png.stat().st_size//1024} KB)")


def main(argv: List[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description="Render sloshing CFD field visualizations (#1437)")
    ap.add_argument("--work-dir", type=Path, default=Path("/mnt/local-analysis/sloshing_cfd_work"))
    args = ap.parse_args(argv)
    wd = args.work_dir

    # Resonant forced-roll case (h/L=0.70, r=1.0, 4 deg): the hero animation + cycle montage.
    hero = wd / "resp_fr_hl70_r100"
    T = 1.0872  # first-mode / drive period (s)
    if hero.exists():
        make_gif(hero, _OUT / "forced-roll-resonance.gif", 60, 60, moved=True,
                 t0=3.0, t1=3.0 + 2 * T, period=T, fps=12)
        c0 = 3.0
        make_montage(hero, _OUT / "roll-cycle-montage.png", 60, 60, moved=True,
                     times=[round(c0 + k * T / 4, 2) for k in range(5)],
                     labels=["0", "T/4", "T/2", "3T/4", "T"],
                     title="one roll cycle at resonance")
    # Free-decay standing wave (h/L=0.50): the "without forced roll" mode.
    fd = wd / "resp_fd_hl50"
    if fd.exists():
        # cells: cpb=80 free-decay -> nx=80, ny=round(0.9/ (0.9/80))=80
        make_montage(fd, _OUT / "free-decay-mode.png", 80, 80, moved=False,
                     times=[0.5, 1.0, 1.5, 2.0], labels=["t=0.5 s", "1.0 s", "1.5 s", "2.0 s"],
                     title="free-decay first mode")
    # Side-by-side effect of roll: left forced (h/L=0.70), right no roll (same fill).
    right = wd / "resp_fd_hl70"
    if hero.exists() and right.exists():
        make_effect_sidebyside(hero, right, _OUT / "effect-of-roll.gif", _OUT / "effect-of-roll.png",
                               lnx=60, lny=60, rnx=80, rny=80,
                               t0=3.0, t1=3.0 + 2 * T, period=T, fps=12)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
