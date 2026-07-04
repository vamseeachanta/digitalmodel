# ABOUTME: Digitize dynacard shapes from the private well-analysis PNG archive.
# ABOUTME: Writes _digitized_cards_stage.json used by build_dynacard_example_library.py.
"""Extract dynacard shapes from the well_analysis PNG archive.

Each PNG is a 640x480 matplotlib plot: blue card path + red markers.
Pipeline: detect red-marker pixel clusters -> centroids -> nearest-neighbour
chain (loop order) -> normalize to the marker bounding box (shape-only,
unitless). Phenomenon label = source folder. Well names are anonymized
deterministically and never written out.

The archive is private: its location comes from the uncommitted side file
``_measured_cards_manifest.json`` (key ``archive_root``) next to this
script — the path itself must never appear in the repo. The committed
library JSON already contains the digitized result, so running this again
is optional.
"""
import json
import math
from collections import defaultdict
from pathlib import Path

from PIL import Image

_MANIFEST = Path(__file__).with_name("_measured_cards_manifest.json")
SRC = Path(
    json.loads(_MANIFEST.read_text(encoding="utf-8"))["archive_root"]
) / "well_analysis"
FOLDERS = {  # folder -> (phenomenon key, max wells, frames per well)
    "butterflies": ("BUTTERFLY", 60, 2),
    "fillage": ("INCOMPLETE_FILLAGE", 45, 1),
    "fillage_collapse": ("FILLAGE_COLLAPSE", 45, 1),
    "full": ("FULL_CARD", 45, 1),
}
OUT = Path(__file__).with_name("_digitized_cards_stage.json")


def red_clusters(img):
    """Centroids of red marker blobs (tab:red ~ (214,39,40))."""
    px = img.load()
    w, h = img.size
    seen = [[False] * w for _ in range(h)]
    def is_red(p):
        return p[0] >= 170 and p[1] <= 120 and p[2] <= 120
    cents = []
    for y in range(h):
        for x in range(w):
            if seen[y][x]:
                continue
            p = px[x, y]
            if not is_red(p):
                seen[y][x] = True
                continue
            # BFS flood fill
            stack = [(x, y)]
            seen[y][x] = True
            xs = ys = n = 0
            while stack:
                cx, cy = stack.pop()
                xs += cx
                ys += cy
                n += 1
                for dx, dy in ((1, 0), (-1, 0), (0, 1), (0, -1)):
                    nx, ny = cx + dx, cy + dy
                    if 0 <= nx < w and 0 <= ny < h and not seen[ny][nx]:
                        if is_red(px[nx, ny]):
                            seen[ny][nx] = True
                            stack.append((nx, ny))
                        # don't mark non-red as seen here (cheap enough)
            if n >= 6:  # marker blobs are ~30-60 px; noise smaller
                cents.append((xs / n, ys / n))
    return cents


def chain(points):
    """Nearest-neighbour loop ordering starting from leftmost point."""
    pts = points[:]
    start = min(range(len(pts)), key=lambda i: pts[i][0])
    order = [pts.pop(start)]
    while pts:
        last = order[-1]
        j = min(range(len(pts)), key=lambda i: (pts[i][0] - last[0]) ** 2 + (pts[i][1] - last[1]) ** 2)
        order.append(pts.pop(j))
    return order


def chain_quality(order):
    """Max hop / median hop — large values mean the chain jumped across the loop."""
    hops = [math.dist(order[i], order[i + 1]) for i in range(len(order) - 1)]
    hops.sort()
    med = hops[len(hops) // 2] or 1.0
    return hops[-1] / med


def extract(path):
    img = Image.open(path).convert("RGB")
    cents = red_clusters(img)
    if len(cents) < 20:
        return None
    order = chain(cents)
    q = chain_quality(order)
    xs = [p[0] for p in order]
    ys = [p[1] for p in order]
    x0, x1 = min(xs), max(xs)
    y0, y1 = min(ys), max(ys)
    if x1 - x0 < 40 or y1 - y0 < 8:
        return None
    pos = [round((x - x0) / (x1 - x0), 4) for x in xs]
    # pixel y grows downward; invert so higher load = larger value
    load = [round((y1 - y) / (y1 - y0), 4) for y in ys]
    return dict(n_markers=len(cents), chain_q=round(q, 1), position=pos, load=load)


def main():
    cards = []
    anon = {}
    for folder, (phen, max_wells, frames_per_well) in FOLDERS.items():
        by_well = defaultdict(list)
        for f in sorted((SRC / folder).glob("*.png")):
            well = f.name.rsplit("_time_", 1)[0]
            t = int(f.name.rsplit("_time_", 1)[1].split(".")[0])
            by_well[well].append((t, f))
        wells = sorted(by_well)[:max_wells]
        for wi, well in enumerate(wells):
            frames = sorted(by_well[well])
            # median frame (+ first frame if 2 per well)
            picks = [frames[len(frames) // 2]]
            if frames_per_well > 1 and len(frames) > 1:
                picks.append(frames[0])
            anon.setdefault(well, f"{phen.lower()}-w{wi+1:02d}")
            for t, f in picks:
                rec = extract(f)
                if rec is None:
                    continue
                cards.append(dict(
                    id=f"{anon[well]}-t{t}",
                    phenomenon=phen,
                    source="field-archive-digitized",
                    units="normalized",
                    **rec,
                ))
        print(f"{folder}: {sum(1 for c in cards if c['phenomenon']==phen)} cards from {len(wells)} wells")
    OUT.write_text(json.dumps({"cards": cards}, separators=(",", ":")))
    bad = [c["id"] for c in cards if c["chain_q"] > 6]
    print(f"total {len(cards)} cards -> {OUT} ({OUT.stat().st_size/1024:.0f} KB); {len(bad)} high chain_q")


if __name__ == "__main__":
    main()
