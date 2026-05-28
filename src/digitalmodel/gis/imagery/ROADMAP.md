# gis.imagery — Roadmap / Enhancements

Field findings from the first production run (FD30150 / 15645 Westpark Dr, Houston,
2026-05-27). Prioritized so a follow-up PR can pick them up.

## 1. Select the NAIP item that *contains* the property point (bug)

`renderer` currently chooses a representative STAC item that merely **intersects** the
search bbox. For a multi-tile neighborhood AOI this can pick a quarter-quad that does **not
cover the property point** (observed: picked `..._nw` while the site sat in `..._ne`),
producing frames centered off-property.

**Fix:** when selecting the per-year item, prefer the item whose `bbox` contains the
address point (`bbox[0] <= lon <= bbox[2] and bbox[1] <= lat <= bbox[3]`); query STAC with
`intersects: {Point}` rather than only `bbox`.

## 2. Actually render property-scale frames

`frames/property/` is empty by design — only neighborhood-scale `rendered_preview` frames
are produced, too zoomed to resolve a single building. Property scale is where the useful
detail lives (e.g., a perimeter vegetation line).

**Fix:** render a property-scale crop by stitching WebMercator XYZ tiles from the item's
tiler at z≈18 and cropping to the property bbox:
`/api/data/v1/item/tiles/WebMercatorQuad/{z}/{x}/{y}@2x?collection=naip&item=<id>&assets=image&asset_bidx=image|1,2,3`.
(Working reference implementation used for FD30150 stitched z18 `@2x` tiles per year.)

## 3. Append a current high-res "today" frame

NAIP lags 2–4 years (latest available was **2022** in May 2026), so the timelapse stops
short of the present and understates current condition. Append a recent-imagery frame as
the **final** frame for clarity on how the site looks *now*.

**Fix:** add an optional latest-frame source, in ToS-preference order:
current NAIP (if newer than cached) → **Esri World Imagery** / **USGS** XYZ tiles (open) →
Bing/Google Maps satellite (only with attention to their terms of service). Label it
`current (<source>, <year>)`. Same WebMercator stitch path as #2.

## 4. Nice-to-have

- Per-frame acquisition date in the frame label (already have year; add month/day).
- A `--latest-source` CLI flag wiring #3.
