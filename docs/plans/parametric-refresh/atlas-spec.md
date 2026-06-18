# Parametric Atlas — specification

> Slice: [digitalmodel#795](https://github.com/vamseeachanta/digitalmodel/issues/795)
> Epic: [digitalmodel#794](https://github.com/vamseeachanta/digitalmodel/issues/794) — Parametric Refresh
> Status: **draft — pending architectural sign-off** (HITL slice)

## Purpose

Enhance the **existing** digitalmodel workflows with a pre-computed **parametric atlas**:
a parameter grid plus a per-physics surrogate, stored as a provenance-stamped artifact.
The Deckhand bot serves instant **interpolated** answers from an atlas via its `EXECUTE`
path; out-of-range or low-confidence queries fall back to the existing 24h-SLA custom
on-demand run, which remains the deliverable of record.

This document fixes the four contracts every downstream slice inherits:

1. the `parametric:` registry block,
2. the atlas artifact format,
3. the query input/output schema,
4. the interpolation taxonomy and the out-of-range / confidence contract.

It is worked end-to-end against `mooring_fatigue` (the Phase-1 pilot,
[digitalmodel#796](https://github.com/vamseeachanta/digitalmodel/issues/796)).

## Where the value is (and isn't)

The atlas earns its keep differently across the cost tiers:

- **Fast (<1 s) workflows** (mooring/spectral fatigue, code checks, geotechnical): a fresh
  run is already instant, so the atlas is **not** about speed. Its value is a queryable,
  bounded **response surface** — instant what-if / sensitivity sweeps across a continuum
  without constructing a full input file, plus a uniform interface the bot can interpolate
  and bound with confidence.
- **Licensed / slow workflows** (AQWA/OrcaWave diffraction, OrcaFlex): here the atlas **is**
  about speed — a 5–30 min licensed solve becomes a sub-second lookup. These get sparse
  canonical libraries with explicit coverage maps
  ([digitalmodel#801](https://github.com/vamseeachanta/digitalmodel/issues/801)), not dense grids.

In all tiers the interpolated number is a **screening estimate, not a certified
deliverable**. The full run stays the document of record.

---

## Decision 1 — the `parametric:` registry block

A new **optional** block on a `docs/registry/workflows.yaml` row declares the atlas for that
workflow. A workflow without the block simply has no atlas (current behaviour unchanged).

Worked example — `mooring-fatigue`:

```yaml
- id: mooring-fatigue
  basename: mooring_fatigue
  title: Metallic mooring-line fatigue damage from tension-range bins
  input: examples/workflows/mooring-fatigue/input.yml
  # ...existing fields unchanged...
  runtime: offline
  parametric:
    physics: log_log                  # linear | log_log | utilization_threshold
    response: damage                  # field the atlas predicts (see "fatigue target" below)
    axes:
      - name: tension_range_kN
        path: "mooring_fatigue.lines[].tension_range_bins[].tension_range_kN"
        grid: [100, 150, 200, 250, 300, 350, 400, 450]
        scale: log
      - name: n_cycles
        path: "mooring_fatigue.lines[].tension_range_bins[].n_cycles"
        grid: [1.0e4, 3.0e4, 1.0e5, 3.0e5, 1.0e6]
        scale: log
      - name: area_mm2
        path: "mooring_fatigue.lines[].area_mm2"
        grid: [3000, 5000, 7000, 9000]
        scale: linear
      - name: sn_curve
        path: "mooring_fatigue.sn_curve.curve"
        values: [B1, B2, C, C1, C2, D, E, F]     # categorical → one atlas slice per value
    tolerance:
      metric: max_rel_error
      threshold: 0.10                 # densify the grid if held-out error exceeds this
```

Rules:

- **Axis path syntax** reuses the existing `parametric_run` mapping grammar
  (dot-separated, `[]` for "every element of this list").
- **Continuous axes** carry a `grid` (explicit knot values) and a `scale`
  (`linear` | `log`) that fixes how the axis is spaced and interpolated.
- **Categorical axes** carry `values` instead of `grid`; they are **not** interpolated —
  each value selects an independent atlas slice.
- `tolerance` is the publish gate: an atlas whose realized held-out error exceeds
  `threshold` is not publishable until its grid is densified.

### The fatigue target (`response: damage`, not `fatigue_life_years`)

For Miner-rule fatigue the atlas predicts **damage** for a single
`(tension_range_kN, n_cycles)` cell within a `(area_mm2, sn_curve)` slice. Total damage is
the **additive** Miner sum over the actual bins, computed at query time from per-cell
atlas lookups; fatigue life is then derived analytically from total damage, `dff`, and
`design_life_years`.

This is preferred over predicting `fatigue_life_years` directly because:

- Miner damage is linear-additive, so summing interpolated per-cell damage is **exact** for
  the recombination step — only the per-cell `damage(range, n_cycles)` surface is interpolated.
- It keeps the interpolated grid **2-D** per slice (range × cycles) instead of baking in a
  fixed bin spectrum, so arbitrary bin histograms are answerable without re-gridding.
- `dff` and `design_life_years` stay analytic scalars applied after the sum — they are not
  atlas axes at all.

---

## Decision 2 — atlas artifact format

One provenance-stamped directory per workflow + atlas configuration:

```
atlases/<basename>/<atlas_id>/
  manifest.yaml      # axes, physics, response, tolerance, validation summary, PROVENANCE
  grid.parquet       # one row per grid point: axis columns + response column
  surrogate.json     # interpolator spec: method, per-axis scale flags, categorical slices, knots
  validation.json    # held-out points: predicted vs actual, realized max_rel_error
```

`manifest.yaml` provenance block (the hook that makes refresh,
[digitalmodel#799](https://github.com/vamseeachanta/digitalmodel/issues/799), detectable):

```yaml
provenance:
  basename: mooring_fatigue
  code_version: <digitalmodel git sha at generation>
  standards:
    - {id: DNV-RP-C203, edition: "2021-09"}     # cited S-N curve source
  input_template_sha256: <hash of the base input.yml the grid was rendered from>
  generated_by: <tool/version>
  atlas_id: <stable hash of axes + grid + code_version + standards>
```

A query against an atlas whose `code_version` or any `standards[].edition` is stale relative
to the current workflow is flagged as **stale** — it must not silently serve an answer
computed against a superseded standard.

---

## Decision 3 — query input / output schema

Input — a normal workflow input (`uv run python -m digitalmodel <query.yml>`):

```yaml
basename: parametric_query
parametric_query:
  atlas: mooring_fatigue              # workflow basename, or an explicit atlas_id
  point:
    tension_range_kN: 275
    n_cycles: 120000
    area_mm2: 8000
    sn_curve: D
    dff: 3.0
    design_life_years: 25
  policy:
    on_out_of_range: escalate         # escalate | error   (clamp / extrapolate are NOT allowed)
    confidence: required
  output_dir: results
```

Output:

```yaml
result:
  response: fatigue_life_years
  value: 41.2
  confidence:
    band: [37.8, 45.1]
    basis: "holdout max_rel_error = 0.08"
  in_range: true
  stale: false
  disclaimer: "Screening estimate from a pre-computed atlas — not a certified deliverable."
  provenance: {atlas_id: ..., code_version: ..., standards: [...]}
```

For a multi-bin fatigue query the `point` may carry a `tension_range_bins` list instead of a
single `tension_range_kN` / `n_cycles` pair; the engine looks up per-cell damage and
Miner-sums per Decision 1.

---

## Decision 4 — interpolation taxonomy and the out-of-range / confidence contract

| Physics class | Interpolator | Rule |
|---|---|---|
| `linear` | regular-grid linear / spline in native axis scale | RAO, catenary, superposable responses |
| `log_log` | interpolate `log10(response)` against `log10(axis)` on `scale: log` axes | fatigue, creep — response spans decades; linear-space interpolation is wrong by orders of magnitude |
| `utilization_threshold` | interpolate the **continuous utilization ratio**, then apply the code threshold | code checks, buckling, VIV lock-in — never store or interpolate the boolean verdict |

**Out-of-range contract (hard rule).** A `point` outside the per-axis grid box (below the
min or above the max knot of any continuous axis), or a categorical value not in the declared
set, returns `in_range: false` and is routed to escalation per `policy.on_out_of_range`.
The engine **never clamps to the boundary and never extrapolates** — `policy.on_out_of_range`
has no `clamp`/`extrapolate` option by construction.

**Confidence contract.** Every in-range answer carries a `confidence.band`. The band is
derived from the atlas's held-out `max_rel_error` (a global bound; a later slice may refine to
a local residual). For `utilization_threshold`, if the band straddles the threshold
(e.g. utilization point estimate 0.97 with band [0.92, 1.03]) the verdict is **not** decidable
from the atlas → the query downgrades to escalate even though the point is in-range.

**Staleness.** An in-range, in-tolerance answer from a `stale` atlas (Decision 2) is also
routed to escalate until the atlas is refreshed.

---

## What this spec does not decide

- The concrete interpolation library / implementation (slice #796 picks it, constrained by
  this taxonomy).
- Grid densities beyond the pilot example (each workflow's manifest, validated against its
  own `tolerance`).
- Sparse-library coverage maps for licensed solvers
  ([digitalmodel#801](https://github.com/vamseeachanta/digitalmodel/issues/801)).
- The Deckhand routing/voice wiring
  ([deckhand#437](https://github.com/vamseeachanta/deckhand/issues/437)).
