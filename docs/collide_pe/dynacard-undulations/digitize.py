"""Digitize the Collide 'Dynamometer Discussions' surface dynacard.

Two overlapping zoom captures of the same plot (identical x-scale, y-offset by
one scroll tick) are stitched into a single frame, then the dark trace pixels
are extracted column by column into upstroke (upper) and downstroke (lower)
branches.

Frame convention: "top-frame" pixel rows. bot image row y  ==  top-frame row y+200.
"""
import numpy as np
from PIL import Image

top = np.array(Image.open("card_top.png").convert("RGB")).astype(float)
bot = np.array(Image.open("card_bot.png").convert("RGB")).astype(float)
DY = 200  # measured offset between the two captures (see axis-label rows)

# ---------------------------------------------------------------- calibration
# y-axis: label rows measured from the blue tick text in the right margin
Y_TOP = {12500: 206.0, 12000: 235.0, 11500: 264.0, 11000: 293.0,
         10500: 323.0, 10000: 352.0, 9500: 380.5}
ys = np.array(list(Y_TOP.values()))
ls = np.array(list(Y_TOP.keys()), dtype=float)
m_y, b_y = np.polyfit(ys, ls, 1)          # load = m_y * y_top + b_y
print(f"y-cal: load = {m_y:.4f} * y + {b_y:.1f}   ({-500/m_y:.2f} px per 500 lb)")


def x_gridlines(img, y0, y1):
    """Vertical gridline columns: light-gray, tall, inside the plot box."""
    g = img[y0:y1].mean(axis=2)
    score = ((g > 195) & (g < 246)).sum(axis=0)
    cols = np.where(score > 0.55 * (y1 - y0))[0]
    groups, cur = [], [cols[0]]
    for v in cols[1:]:
        if v - cur[-1] <= 2:
            cur.append(v)
        else:
            groups.append(cur)
            cur = [v]
    groups.append(cur)
    return np.array([np.mean(g) for g in groups])


gx = x_gridlines(bot, 10, 190)
print("vertical gridlines (bot px):", np.round(gx, 1))
# gridlines are every 2 inches, 0..40 -> 21 lines
step = np.median(np.diff(gx))
x0 = gx[0]
m_x = 2.0 / step                           # inches per pixel
print(f"x-cal: {step:.3f} px per 2 in, origin px {x0:.1f}, span "
      f"{(gx[-1]-gx[0])*m_x:.1f} in over {len(gx)} lines")


# ------------------------------------------------------------------ traces
def trace_pixels(img, y0, y1, dy):
    """Return {col: [top-frame rows]} of dark (trace) pixels."""
    sub = img[y0:y1]
    g = sub.mean(axis=2)
    sat = sub.max(axis=2) - sub.min(axis=2)
    mask = (g < 175) | ((g < 215) & (sat > 45))   # dark or saturated colour
    out = {}
    for c in range(452):   # stop before the right-margin tick labels
        rows = np.where(mask[:, c])[0]
        if len(rows):
            out[c] = rows + y0 + dy
    return out


# top image: plot interior rows 200..399. bot image: rows 0..11 are the black page
# banner above the chart, so start at 14; row 203 is the x-axis.
px_top = trace_pixels(top, 200, 400, 0)
px_bot = trace_pixels(bot, 14, 203, DY)

cols = sorted(set(px_top) | set(px_bot))
merged = {}
for c in cols:
    rows = np.concatenate([px_top.get(c, np.array([])), px_bot.get(c, np.array([]))])
    if len(rows):
        merged[c] = np.unique(np.round(rows).astype(int))

# split each column's pixels into contiguous runs; the highest run (smallest y)
# is the upstroke branch, the lowest run is the downstroke branch
rec = []
for c in sorted(merged):
    inch = (c - x0) * m_x
    if inch < -0.4 or inch > 41.2:
        continue
    rows = merged[c]
    runs, cur = [], [rows[0]]
    for v in rows[1:]:
        if v - cur[-1] <= 4:
            cur.append(v)
        else:
            runs.append(cur)
            cur = [v]
    runs.append(cur)
    up = m_y * np.mean(runs[0]) + b_y
    dn = m_y * np.mean(runs[-1]) + b_y
    rec.append((inch, up, dn, len(runs)))

rec = np.array([(a, b, c) for a, b, c, _ in rec])
print(f"\ndigitized {len(rec)} columns, x {rec[:,0].min():.2f}..{rec[:,0].max():.2f} in")
print(f"PPRL (max upper) = {rec[:,1].max():8.0f} lb at {rec[rec[:,1].argmax(),0]:5.1f} in")
print(f"MPRL (min lower) = {rec[:,2].min():8.0f} lb at {rec[rec[:,2].argmin(),0]:5.1f} in")

# resample onto a 0.5 in grid
grid = np.arange(0.5, 40.5, 0.5)
up = np.interp(grid, rec[:, 0], rec[:, 1])
dn = np.interp(grid, rec[:, 0], rec[:, 2])
np.savetxt("dynacard_digitized.csv",
           np.column_stack([grid, up, dn]),
           delimiter=",", header="position_in,upstroke_load_lb,downstroke_load_lb",
           comments="", fmt="%.2f")
print("wrote dynacard_digitized.csv")


# ------------------------------------------------- peak finding on the upstroke
def peaks(x, y, kind="max", w=6):
    idx = []
    for i in range(w, len(y) - w):
        win = y[i - w:i + w + 1]
        if (kind == "max" and y[i] == win.max()) or (kind == "min" and y[i] == win.min()):
            if not idx or i - idx[-1] > w:
                idx.append(i)
    return [(x[i], y[i]) for i in idx]


print("\nupstroke local maxima (position in, load lb):")
for p, l in peaks(grid, up, "max"):
    print(f"  {p:5.1f}  {l:8.0f}")
print("downstroke local minima:")
for p, l in peaks(grid, dn, "min"):
    print(f"  {p:5.1f}  {l:8.0f}")

# card area (work per stroke) by the shoelace/trapezoid rule
area = np.trapezoid(up - dn, grid)          # lb*in
print(f"\ncard area (net work) = {area:,.0f} lb-in = {area/12:,.0f} ft-lb per stroke")
print(f"                       = {area/12*6.4/33000:,.2f} hp (polished rod)")
