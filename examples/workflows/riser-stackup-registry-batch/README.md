# Riser Stack-up Registry Batch Run (API RP 16Q)

Batch sweep of the riser stack-up registry (#1453/#1458, epic #1279): every
`riser_stackup` row with `stackup_type = as-planned-stackup` and
`operation = drilling` is checked for a wiki-side `16q-min-top-tension` calc
contract (llm-wiki#826 — registry page + per-dataset source pages) and, where
the chain inputs exist, the API RP 16Q minimum vertical top tension is
computed through `digitalmodel.drilling_riser.assembly.minimum_top_tension_16q`.
Rows without a golden chain contract but WITH a text-extractable wiki joint
schedule + joint library run the #1458 schedule-assembly path instead
(`digitalmodel.drilling_riser.schedule_assembly`): the string weight is
assembled from the schedule, the 16Q minimum is computed at the heaviest
documented mud weight, and the row reports the validation delta band against
the wave-3 `16q-min-tension-endpoints` contract (llm-wiki#828).
Rows with a shallow-water source workbook carrying a top-tension-to-rotate
result are emitted as `runnable-source`; the runner does not recast those
values as an API RP 16Q chain.

## Run

```bash
LLM_WIKI_PATH=/path/to/llm-wiki \
    uv run python examples/workflows/riser-stackup-registry-batch/run.py
```

Prints the batch table and writes `results/registry_batch_16q.csv` with, per
RSU: `rsu_id`, `water_depth_band`, `topology_class`, string dry/submerged
weight and buoyancy uplift/net-lift (where the source decomposes them), the
computed `Tmin_vert`, its unit, and `status` (`runnable` |
`runnable-schedule` | `runnable-source` | `missing-inputs`) with a
`fields_unknown` reason.

## Output is NOT committed (by design)

`results/` is gitignored. Computed tensions/weights are wiki-side VALUES
derived from private project workbooks: dm carries only key names, the
assembly engine, and this runner (the #1280 calc-contract discipline). The
private llm-wiki registry keeps the values until a dataset passes the dm
de-id/leak gate.

## Reading the reasons

- `runnable` — a golden `16q-min-top-tension` contract exists; the engine
  reproduces its documented Tmin (asserted by
  `tests/drilling_riser/test_assembly_golden.py`).
- `runnable-schedule` — no golden chain contract, but the source page carries
  a text-extractable joint schedule + joint library: the string is assembled
  bottom-up (#1458) and validated against the `16q-min-tension-endpoints`
  contract. The reason column carries the per-RSU delta band and a
  PASS (<= 3 %) / FINDING verdict — findings are characterized (delta bands
  pinned) in `tests/drilling_riser/test_schedule_assembly.py`, never
  tolerance-masked. The modelling assumptions (net-weight basis, mud in main
  bore + C&K + boost, 1.25 top-tension factor, pup/termination joints at the
  slick per-foot rate, ...) are documented in
  `digitalmodel.drilling_riser.schedule_assembly`.
- `runnable-source` — no golden 16Q chain is claimed; the source page carries
  a machine-readable top-tension result in another workbook dialect and the
  batch reports that source-backed governing tension.
- `fields_unknown: 16Q endpoint contract exists but the joint schedule is
  not text-extractable` — e.g. RSU-0038, whose per-class joint counts are
  raster-only in the source document.
- `fields_unknown: wiki carries result-dialect contracts only` — the source
  page documents results (`id:`/`expected:` entries) but no machine-golden
  16Q chain yet: candidate wave-3 contracts.
- `fields_unknown: no calc contract wiki-side` — the dataset is registered
  (stack-up schedule exists) but no reproduction contract has been extracted.

## Chain conventions

The wave-2 workbook chains apply the bare `N/(N-n)` tensioner-failure
allowance (reduction factor Rf = 1.0, no separate fleet-angle factor); a
contract that carries explicit `efficiency` / `fleet_angle_factor` keys
(RSU-0007 style) is honoured. See `minimum_top_tension_16q` docstring for
the Rf-split note.
