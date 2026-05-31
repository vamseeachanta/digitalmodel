# demo_01 Runbook — DNV Freespan / VIV Screening

Operator runbook for the demo_01 parametric deliverable. Terms (**Sweep Config**, **Run**,
**Baseline Run**, **Results Store**, **Screening**, **Prospect Intake**) are defined in
[`CONTEXT.md`](../CONTEXT.md); the storage/latency decision is [ADR-0003](adr/0003-results-store-sqlite-fast-read-async-write.md).

All commands run from the `digitalmodel` repo root with this prefix:

```bash
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_01_dnv_freespan_viv.py
```

(Substitute `uv run python` with `.venv/bin/python` on machines without `uv`.)

---

## 1. Verified Baseline Run

Reproduces the committed 680-case golden matrix exactly (the **Baseline Run**). Writes the legacy
report `output/demo_01_freespan_report.html`, the JSON `results/demo_01_freespan_results.json`, and
seeds the `baseline` run into the **Results Store**.

```bash
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_01_dnv_freespan_viv.py
```

The cases[] payload of the JSON is byte-identical to
`tests/fixtures/golden/demo_01_baseline_results.json` (only the volatile `metadata.timestamp`
changes between runs). The golden regression test enforces this:

```bash
PYTHONPATH=examples/demos/gtm:src .venv/bin/python -m pytest \
    examples/demos/gtm/tests/test_demo_01_baseline_golden.py -q
```

## 2. Client Sweep Run

Run a client's own **Sweep Config** under a named `run_id`. Writes a per-run report and store
partition WITHOUT touching the Baseline artifacts.

```bash
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_01_dnv_freespan_viv.py \
    --input inputs/client.yml --run-id acme_q2
```

Outputs for a named run:

| Artifact | Path |
|----------|------|
| Per-run report (client-facing) | `output/parametric/demo_01/acme_q2/report.html` |
| Per-run cases (text source of truth) | `results/parametric/demo_01/acme_q2/cases.csv` |
| Per-run manifest (resolved config + provenance) | `results/parametric/demo_01/acme_q2/manifest.json` |
| Run row in the Results Store cache | `results/parametric/demo_01/results.db` |

Re-running the same `--run-id` replaces that run in place (delete-then-insert) — no duplicates.

## 3. Report-only (from cache)

Regenerate the Baseline report/JSON from the cached results without recomputing the sweep (no
**Sweep Config** needed). The Results Store is NOT rewritten on this path (there is no resolved
config to snapshot).

```bash
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_01_dnv_freespan_viv.py --from-cache
```

## 4. `lookup` — operator ad-hoc case lookup

Read-only filtered query over the Results Store cache for off-chart questions during a live demo.
Never shown to a client (raw SQL stays internal — see ADR-0003). The db is opened read-only and is
NEVER created by `lookup`; run a sweep or `rebuild-db` first if it is missing.

Filters (all optional, AND-combined; each is canonicalized to the stored form before binding):

| Flag | Meaning | Notes |
|------|---------|-------|
| `--run-id` | Run to query | default `baseline` |
| `--size` | `nominal_size` | **include the `in` suffix**, e.g. `12in`; jumpers are `8in-jumper` |
| `--current` | current velocity m/s | float, e.g. `0.6` |
| `--gap` | gap ratio e/D | `1.0` (finite) or `inf` (jumper mid-water) |
| `--span` | span length m | integer |
| `--status` | screening status | `PASS` / `INLINE_ONLY` / `FAIL_CF` / `FAIL_LOCKIN` |
| `--base-dir` | Store base dir | default the demo's `results/` dir |

```bash
# Pipeline 12in @ 0.6 m/s, gap e/D = 1.0 — all 8 spans
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_01_dnv_freespan_viv.py lookup \
    --run-id baseline --size 12in --current 0.6 --gap 1.0

# Jumper mid-water cases (gap = infinity)
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_01_dnv_freespan_viv.py lookup \
    --run-id baseline --gap inf

# Only the cross-flow VIV failures for a size
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_01_dnv_freespan_viv.py lookup \
    --run-id baseline --size 16in --status FAIL_CF
```

Messages are distinct: an unknown `--run-id` lists the available runs; a filter that matches
nothing prints `0 cases match the given filters.`; a missing db prints a run-a-sweep hint.

> **Note (deferred, D3):** `lookup` and `rebuild-db` currently require the full `uv run` env
> (they import `results_store`, which transitively loads the demo module's `plotly` dependency).
> Lazy-importing the chart deps so the read-only subcommands run in a minimal env is a future
> follow-up; for now run them under the same `uv run` env as the sweep.

## 5. `rebuild-db` — regenerate the cache from text

The Results Store SQLite file is a **rebuildable, gitignored cache** (ADR-0003). Regenerate it (and
`index.csv`) byte-identically from the per-run `cases.csv` + `manifest.json` text source of truth —
the recovery path after a partial write or a fresh checkout.

```bash
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_01_dnv_freespan_viv.py rebuild-db
```

Prints the db path plus per-run row counts.

---

## 6. Reusable agent prompt (adapt the issue #632 §5 prompt)

To produce a client deliverable for a new prospect, hand an agent this prompt (fill the bracketed
fields):

> You are producing a demo_01 DNV-RP-F105 freespan/VIV **Screening** deliverable for **[client]**.
> 1. Author a **Sweep Config** at `inputs/[client].yml` from their design point — pipe sizes, span
>    lengths, current velocities, gap ratios (use `inf` *as a float* for jumper mid-water), content
>    density, boundary condition, wt selection. Each axis is a **list**; length-1 freezes the axis,
>    length-N sweeps it (the engine takes the full cross-product — ADR-0004).
> 2. Run it: `... --input inputs/[client].yml --run-id [client_run]`.
> 3. Verify the run landed: `... lookup --run-id [client_run] --size [size] --current [v] --gap [e/D]`
>    returns the expected rows; confirm `output/parametric/demo_01/[client_run]/report.html` exists.
> 4. Report back with the generated `report.html` path and a one-line status summary. This is a
>    **screening** (flags cases warranting detailed analysis), NOT an acceptance check — say so.
>
> Do NOT modify the Baseline Run, the golden fixture, or the 15 frozen JSON keys. The golden
> regression suite must stay green.

## 7. Execution model — manual command, no scheduler

Per [ADR-0003](adr/0003-results-store-sqlite-fast-read-async-write.md), a client rerun is invoked
as a **manual command** ("run it and come back"); the report-back artifact is the generated
`report.html`. The ~1hr-to-next-day latency is compute time, not a scheduling requirement, so a
queue/cron is **deliberately not built** (YAGNI). Promoting the manual command to a cron payload is
a small later step if rerun volume ever justifies unattended runs.
