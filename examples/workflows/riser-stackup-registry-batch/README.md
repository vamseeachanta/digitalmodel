# Riser Stack-up Registry Batch Run (API RP 16Q)

First batch sweep of the riser stack-up registry (#1453, epic #1279): every
`riser_stackup` row with `stackup_type = as-planned-stackup` and
`operation = drilling` is checked for a wiki-side `16q-min-top-tension` calc
contract (llm-wiki#826 — registry page + per-dataset source pages) and, where
the chain inputs exist, the API RP 16Q minimum vertical top tension is
computed through `digitalmodel.drilling_riser.assembly.minimum_top_tension_16q`.

## Run

```bash
LLM_WIKI_PATH=/path/to/llm-wiki \
    uv run python examples/workflows/riser-stackup-registry-batch/run.py
```

Prints the batch table and writes `results/registry_batch_16q.csv` with, per
RSU: `rsu_id`, `water_depth_band`, `topology_class`, string dry/submerged
weight and buoyancy uplift (where the contract decomposes them), the computed
`Tmin_vert`, its unit, and `status` (`runnable` | `missing-inputs`) with a
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
