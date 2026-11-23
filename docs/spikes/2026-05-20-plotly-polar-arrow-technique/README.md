# Spike: Plotly polar arrow-rendering technique

> **Issue:** [digitalmodel#616](https://github.com/vamseeachanta/digitalmodel/issues/616)
> **Plan:** [`docs/plans/2026-05-20-issue-616-ocimf-polar-vessel-force-overlay.md`](../../plans/2026-05-20-issue-616-ocimf-polar-vessel-force-overlay.md) §Risks > "Plotly polar arrow rendering"
> **Plan gate:** acceptance criterion "r1 m6 + r2 n4 enforcement: Plotly arrow technique pre-spike artifact committed under `docs/spikes/2026-05-20-plotly-polar-arrow-technique/` …"
> **Date:** 2026-05-20

## Question being answered

Plotly's `Scatterpolar` trace type does not provide native arrowhead markers (`layout.annotations` arrowheads are designed for Cartesian axes). The plan §Risks called out three candidate techniques and required this spike to validate one before any module code lands. The question: which technique renders a directionally-correct arrow on a polar axis with the least Cartesian conversion overhead?

## Candidates considered

| Technique | How it works | Why rejected (or chosen) |
|---|---|---|
| (a) `layout.annotations` with explicit Cartesian conversion | Compute `(x,y) = (r·cos(θ_rad - π/2), r·sin(θ_rad - π/2))` per arrow, use annotation `xref='x'`/`yref='y'` with `arrowhead`/`arrowsize` | **Rejected** — every angle requires a per-point Cartesian conversion. For 12 sampled headings × 6 components = 72 conversions per polar diagram, each with its own coordinate-system bookkeeping. Error-prone. |
| (b) `mode='lines+markers'` with `symbol='arrow'` on a Cartesian projection | Switch from `Scatterpolar` to `Scatter` with polar projection emulation | **Rejected** — abandons polar projection. The entire chart's heading-axis semantics (ticks "0° bow / 90° stbd / 180° stern / 270° port") would have to be reimplemented manually. Defeats the purpose. |
| (c) Custom SVG injection via `fig.add_shape` with `type='path'` | Construct SVG path strings for each arrow | **Rejected** — robust but heavy. Path strings are opaque; debugging arrow direction errors requires reading SVG path data. Not maintainable. |
| **(d) Two `Scatterpolar` traces — line for shaft + marker for head** | One trace draws the line from origin to tip; second trace draws a `symbol='triangle-up'` marker at the tip with `angle=θ` to rotate the triangle to point outward | **CHOSEN** — stays in native polar coords; arrow direction is whatever the line+marker pair encodes; no Cartesian conversion; the `marker.angle` rotation behaves predictably (verified by this spike). Two traces per arrow scales linearly with arrow count. |

## What this spike rendered

[`prototype.py`](./prototype.py) constructs a Plotly figure with:
- One Scatterpolar **line** trace from `(r=0, θ=90)` to `(r=0.5, θ=90)` — the arrow shaft pointing outward at the starboard-beam direction (θ=90° in our `direction='clockwise', rotation=90` convention).
- One Scatterpolar **marker** trace at `(r=0.5, θ=90)` with `symbol='triangle-up'`, `angle=90` — the arrow head, rotated 90° to point along the +Y body-fixed direction.

[`rendered.html`](./rendered.html) (8.5 KB) is the rendered output. Open it locally and you should see a bow-up polar with a single arrow pointing rightward (toward the "90° stbd" tick), confirming:
1. The technique produces a visible arrow on a polar projection.
2. The arrow direction (outward, toward starboard at the starboard-beam heading) matches the convention the implementing agent expects.
3. The angular-axis ticks remain correctly oriented (bow up, starboard right, stern down, port left).

## Known limitations / open questions for implementation phase

- **Arrow head rotation reference frame:** `marker.angle` is documented as rotating the marker symbol relative to its default upright. On Scatterpolar, "default upright" appears to align with the polar projection's local tangent — but this needs confirmed verification when arrows are placed at θ ≠ 90°. The implementation phase MUST test arrows at θ ∈ {0, 45, 90, 135, 180, 225, 270, 315} to confirm consistent visual outward-pointing.
- **Hover behavior:** the marker trace will register hover events. The module should set `hoverinfo='skip'` on the head-marker trace to avoid duplicate hovers.
- **Two-trace overhead:** each arrow becomes two traces in `fig.data`. The no-regression test #13 compares trace counts against the pre-refactor baseline — the test must account for this expansion (or compare the union-of-shaft-plus-head as a single conceptual arrow).
- **Marker symbol choice:** `triangle-up` was chosen for clarity. `arrow-bar-up` may also work and looks more like a conventional arrow; defer to implementation-phase A/B comparison.
- **Color encoding for sign:** the module needs to render positive forces vs negative forces in distinct colors. The line + marker color attributes must agree (driven from the same source-of-truth in the implementation).

## What this spike does NOT validate

- Does not pin the OCIMF MEG3/MEG4 Annex A convention (that's the citation gate at TDD #5/#6).
- Does not validate vessel silhouette polygon rendering (separate spike could be added; the plan's silhouette work is small enough to TDD without a spike).
- Does not test arrow density / visual clutter at high `arrow_sample_step_deg` settings.

## Decision

Implementation phase will use **technique (d): two-trace Scatterpolar (shaft + head)**. The arrow renderer in `polar_force_overlay.py` will encapsulate this as a private helper, e.g.:

```python
def _add_force_arrow(fig, r_origin, r_tip, theta_deg, color):
    fig.add_trace(go.Scatterpolar(
        r=[r_origin, r_tip], theta=[theta_deg, theta_deg],
        mode="lines", line=dict(color=color, width=2),
        hoverinfo="skip", showlegend=False,
    ))
    fig.add_trace(go.Scatterpolar(
        r=[r_tip], theta=[theta_deg],
        mode="markers",
        marker=dict(symbol="triangle-up", size=12, color=color, angle=theta_deg),
        hoverinfo="skip", showlegend=False,
    ))
```

(Final signature subject to refinement during TDD.)
