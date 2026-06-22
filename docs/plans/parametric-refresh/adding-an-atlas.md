# Adding an analytical parametric atlas — authoring guide

> Operational companion to [`atlas-spec.md`](./atlas-spec.md) (which fixes the
> contracts). This is the **recipe**: how to turn an existing offline,
> closed-form digitalmodel workflow into a committed, drift-gated atlas the
> Deckhand bot serves instantly via `parametric_query`.

## When this applies

The workflow must be **analytical / closed-form and offline** (no licensed
solver), so a response value can be computed for any grid point in CI. Its
response must reduce to a scalar over a few **scalar** axes. If the live workflow
consumes an array (a stress histogram, an Hs time series, a scatter diagram),
**bake a fixed reference array into the response function** and sweep scalar
multipliers around it (see `weather_window` / `riser_fatigue`). An array is never
an atlas axis.

Licensed-solver workflows (OrcaWave/OrcaFlex/AQWA/ANSYS) do **not** use this
recipe — their atlases are sparse libraries refreshed by an operator run (see
`licensed-libraries.md`); P1 of `parametrics-preparedness.md`.

## The recipe (8 edits)

Worked examples: `fowt_mooring` (#979), `lifting_lug`, `esp_pump_hydraulics`,
`inspection_planning` (#983, plain value), `weather_window` (#987, baked
hindcast), `riser_fatigue` (#991, annual damage). All follow this exactly.

1. **Response function** — `src/digitalmodel/parametric/generate.py`. Add
   `_<name>(point: dict) -> float` that computes the scalar response for ONE grid
   point, calling the workflow's own math (fix every non-swept input at the
   example values so the atlas reproduces a real run). Register it in
   `RESPONSE_FUNCS[<basename>]`.
2. **Drift basis** — `src/digitalmodel/parametric/refresh.py`. Add
   `SOURCE_FILES[<basename>]` (the response-relevant source module(s)) and, if a
   code/standard governs the formula, `STANDARDS[<basename>]`. These feed
   `content_fingerprint`, so the atlas reads stale when its basis moves.
3. **Registry `parametric:` block** — `docs/registry/workflows.yaml`, on the
   workflow's existing row: `physics`, `response` (a name), `tolerance`
   (`{metric: max_rel_error, threshold: 0.10}`), `axes` (each `{name, scale,
   grid}` with ~5 knots), `atlas: atlases/<basename>`.
4. **Query row** — also in `workflows.yaml`: a `<id>-atlas-query` row with
   `basename: parametric_query`, `input: examples/workflows/<id>-atlas-query/input.yml`,
   the result output, and the durable test id.
5. **Handler** — `src/digitalmodel/parametric/query.py`. Add
   `_HANDLERS[<basename>] = <handler>` (see the table below).
6. **Build the atlas** —
   `build_atlas_from_registry("<id>")` (it generates the grid, validates against
   held-out interior midpoints, stamps the fingerprint, and saves under
   `atlases/<basename>/<atlas_id>/`). **`max_rel_error` must be < the tolerance**;
   if not, densify the grid or pick a `physics`/`scale` that linearises the
   response (see gotchas). Commit the saved `grid.parquet` + `*.json` +
   `manifest.yaml` + `default.txt`.
7. **Query example** — `examples/workflows/<id>-atlas-query/input.yml`: an
   in-range `point` (plus any non-axis kwargs the handler reads, e.g.
   `design_life_years` / `dff`), `policy.on_out_of_range: escalate`.
8. **Durable assertion** — `tests/workflows/test_durable_workflows.py`: an
   `elif workflow["id"] == "<id>-atlas-query"` block asserting the keys the chosen
   handler actually returns (every registered workflow MUST have a block, or the
   test raises `Missing workflow assertion`).

### Verify (all must pass)
```
uv run python -m digitalmodel.parametric.refresh        # shows [ok] <id> (current)
uv run python -m pytest \
  "tests/workflows/test_durable_workflows.py::test_workflow_registry[<id>]" \
  "tests/workflows/test_durable_workflows.py::test_workflow_registry[<id>-atlas-query]" -q
uv run --with ruff ruff check <edited .py files>
```

## Handler classes (`query.py` `_HANDLERS`)

| Response | Handler | Returns | Use for |
|---|---|---|---|
| Utilisation (UC, threshold 1.0) | `_handle_utilisation` | `value`, `screening_status` pass/fail | `fowt_mooring`, `lifting_lug`, `esp_pump_hydraulics`, `mudmat_bearing_capacity`, `span_rectification`, `code_check`, `free_span` |
| Plain scalar value | `_handle_value` | `value`, confidence band (no verdict) | `inspection_planning`, `weather_window`, `fpso_mooring_full`, `viv_analysis` |
| Annual fatigue damage | `_handle_annual_damage` | `fatigue_life_years`, `annual_damage`, `dff_margin`, pass/fail (reads `design_life_years`/`dff` from the point) | `riser_fatigue`, `spectral_fatigue` |
| Capacity vs demand | `_handle_capacity_demand` | capacity, UC vs query-time load | `pile_capacity`, `anchor_capacity` |

## Gotchas

- **Full-factorial completeness.** The grid must be a complete Cartesian product
  of the axis knots — `generate_atlas` validates and the interpolator reshapes to
  the grid box. No holes.
- **Tolerance.** Holdout error must be `< threshold` (0.10). If a response is a
  steep power law (e.g. fatigue `∝ stress^m`), use `physics: log_log` so it
  interpolates near-exactly; otherwise add knots. `utilization_threshold` /
  `linear` keep the response untransformed.
- **Baked reference for array inputs.** Hold the hindcast/histogram/scatter fixed
  inside the response function (a per-case property, like `spectral_fatigue`'s
  stress gain) and sweep scalar multipliers. Make it deterministic (no RNG) so a
  refresh rebuilds an identical atlas.
- **Query-time kwargs.** Axes are interpolation dimensions; values a handler needs
  but should NOT grid on (`design_life_years`, `dff`, an applied load) are passed
  in the query `point` and read by the handler — keep them OUT of `axes`.
- **Out-of-range is a hard gate.** Outside any axis box → `in_range: false`,
  escalate; never clamp or extrapolate.
- **Drift fingerprint** = the `parametric:` block + the `input` file + `STANDARDS`
  + `SOURCE_FILES` for the basename. Change any → the committed atlas reads stale
  and must be rebuilt (step 6).

## Shared-file note (parallel authoring)

Every atlas edits the same five shared files (`generate.py`, `refresh.py`,
`query.py`, `workflows.yaml`, the durable test). If several atlases are authored
in parallel (e.g. an agent team), they conflict pairwise on additive hunks —
build in parallel but **merge sequentially**, resolving the trivial keep-both
conflicts. Do not attempt a parallel merge.

## Served atlases (live index)

For the current list + grid summaries: `uv run python -m digitalmodel.parametric.catalog`.
As of this writing, 20 atlases span marine structures (`fowt_mooring`,
`lifting_lug`, `fpso_mooring_full`), wells (`esp_pump_hydraulics`), geotech
(`mudmat_bearing_capacity`, `pile_capacity`, `anchor_capacity`), pipeline
(`free_span`, `span_rectification`, `code_check`), asset integrity
(`inspection_planning`), marine ops (`weather_window`), hydrodynamics
(`rao_tabulation`), and fatigue (`riser_fatigue`, `spectral_fatigue`,
`mooring_fatigue`, `synthetic_rope_mooring_fatigue`, `viv_analysis`), plus the
licensed-solver stub libraries (`diffraction_library`, `orcaflex_library`).
