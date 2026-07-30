# Issue #1915 renderer report

## Down-sampling and payload

The page keeps all nine design codes and applies regular index strides to the
three numeric axes:

- wall thickness: every second source value, including both endpoints
  (`41 -> 21`, or 1.0 mm display spacing);
- effective tension: every third source value (`31 -> 11`);
- bending moment: every third source value (`31 -> 11`).

That produces `9 x 21 x 11 x 11 = 22,869` embedded points, or 6.45% of the
354,609 source rows. Utilisation is packed as a fixed-point integer at
`1e-4` resolution, and governing checks are packed as indexes into one
categorical name table. Per-check utilisation columns are not embedded.

The generated artifact records an embedded JSON size of **172,085 bytes**
(172.1 kB decimal, about 168.1 KiB), below the hard 400,000-byte limit.
The builder measures the final UTF-8 JSON, includes that measurement in the
payload, and raises `ValueError` rather than writing a page at or above the
limit.

The page states this reduction and links
`docs/api/structural/wall-thickness-3d.json` for the full tidy study.

## WebGL fallback

The native `<canvas>` 2D heatmap is painted first from the same selected
code/wall slice. WebGL support is checked once with a real `webgl2`/`webgl`
context. Plotly 3.6.0 is loaded from an exact-version CDN URL:

- WebGL available: categorical `mesh3d` surface, double acceptance contour,
  and a translucent utilisation = 1 plane.
- WebGL unavailable: non-WebGL Plotly heatmap plus a double utilisation = 1
  contour.
- CDN/Plotly failure: the already-painted native canvas remains visible.
- WebGL context loss after startup: the page exposes the native canvas
  immediately and switches to the 2D Plotly view.

The design-code `<select>` and wall-thickness `<input type="range">` are
labelled native controls. At 640 px and below the control grid becomes a
single column; the page and plot shell both constrain overflow, so the
375 px layout does not require horizontal scrolling.

## Verification and uncertainty

Per instruction, I did not execute the builder, tests, linters, `uv`, or git
commands. The full JSON and an HTML artifact were generated concurrently in
the shared worktree; I read the artifact's payload measurement and patched
the static HTML to match the final renderer template. The added tests cover
packing order, all-code down-sampling, the payload ceiling, output writing,
native controls, pinned CDN use, fallback markup, and the default acceptance
marker, but remain unexecuted.

Browser rendering remains the verification gap. In particular, the page has
not been exercised here on a real no-WebGL browser or at a measured 375 px
viewport.
