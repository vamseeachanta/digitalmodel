# demo_04 Runbook — Shallow Water Pipeline Installation Analysis

Operator runbook for the demo_04 parametric deliverable. Terms (**Sweep Config**, **Run**,
**Baseline Run**, **Results Store**) carry the same meaning as in the demo_01 / demo_02 / demo_03
runbooks and `CONTEXT.md`; the storage/latency decision is shared with the other demos (ADR-0003 —
SQLite fast-read, rebuildable cache).

All commands run from the `digitalmodel` repo root with this prefix:

```bash
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_04_shallow_water_pipelay.py
```

(Substitute `uv run python` with `.venv/bin/python` on machines without `uv`.)

---

## 1. Verified Baseline Run

Reproduces the committed 60-case golden matrix exactly (2 vessels × 5 pipe sizes × 6 water depths =
60). Writes the legacy report `output/demo_04_shallow_pipelay_report.html`, the JSON
`results/demo_04_shallow_pipelay_results.json`, and seeds the `baseline` run into the **Results
Store**.

```bash
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_04_shallow_water_pipelay.py
```

The `cases[]` payload of the JSON is byte-identical to
`tests/fixtures/golden/demo_04_baseline_results.json`. The golden regression test enforces this:

```bash
PYTHONPATH=examples/demos/gtm:src .venv/bin/python -m pytest \
    examples/demos/gtm/tests/test_demo_04_baseline_golden.py -q
```

The baseline status split is **50 GO / 5 MARGINAL / 5 NO_GO** (the 5 NO_GO are the Large PLV at
7 m — a depth-envelope rejection).

## 2. Client Sweep Run

Run under a named `run_id` with a client **Sweep Config** (writes a per-run report + store partition
WITHOUT touching the Baseline artifacts). Use `--results-dir` to point the legacy JSON + Results
Store at a client directory.

```bash
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_04_shallow_water_pipelay.py \
    --config inputs/acme.yml --run-id acme --results-dir /path/to/acme/results
```

Outputs for a named run:

| Artifact | Path |
|----------|------|
| Per-run report (client-facing) | `output/parametric/demo_04/acme/report.html` |
| Per-run cases (text source of truth) | `<results-dir>/parametric/demo_04/acme/cases.csv` |
| Per-run manifest (resolved config + provenance) | `<results-dir>/parametric/demo_04/acme/manifest.json` |
| Run row in the Results Store cache | `<results-dir>/parametric/demo_04/results.db` |

Re-running the same `--run-id` replaces that run in place (delete-then-insert) — no duplicates.
A `--run-id` is validated as a single safe path segment (`[A-Za-z0-9._-]+`, never `''`/`.`/`..`),
so traversal values like `../../evil` are rejected non-zero before any directory is created. The
manifest stores all config paths repo-relative (no machine-specific absolute paths land in the
committed artifact).

## 3. Report-only (from cache)

Regenerate the Baseline report/JSON from the cached results without re-running the sweep. The
Results Store is NOT rewritten on this path.

```bash
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_04_shallow_water_pipelay.py --from-cache
```

> `--from-cache` is only supported with the committed baseline config (a non-default `--config`
> cannot be proven to match the cache and is refused). The cache always holds the Baseline 60
> cases. Passing `--from-cache --run-id <name>` emits a warning and writes the report to the
> **baseline** path (not a named-run subdir) so a cached run is never mislabeled. To compute a
> genuine named run, drop `--from-cache`.

## 4. `lookup` — operator ad-hoc case lookup

Read-only filtered query over the Results Store cache for off-chart questions during a live demo.
The db is opened read-only (`file:...?mode=ro`) and is NEVER created by `lookup`; run a sweep or
`rebuild-db` first if it is missing.

Filters (all optional, AND-combined; each binds verbatim to the stored value):

| Flag | Meaning | Notes |
|------|---------|-------|
| `--run-id` | Run to query | default `baseline` |
| `--vessel-id` | `vessel_id` | the **SHORT id** `PLV-001` / `PLV-002` (NOT the display name) |
| `--pipe-size` | `pipe_size` | nominal size token: `8in` / `12in` / `16in` / `20in` / `24in` |
| `--water-depth` | `water_depth_m` | **integer** metres, e.g. `30` |
| `--status` | `overall_status` | one of `GO` / `MARGINAL` / `NO_GO` / `ERROR` |
| `--base-dir` | Store base dir | default the demo's `results/` dir |

> **Identifiers are the SHORT ids.** `--vessel-id` matches the catalog SHORT ids
> (`PLV-001`/`PLV-002`), **not** the display names. The valid `--status` tokens are exactly
> `GO` / `MARGINAL` / `NO_GO` / `ERROR` (note the underscore in `NO_GO`).

The displayed columns are `vessel_id`, `pipe_size`, `water_depth_m`, `overall_status`,
`governing_check`, `max_utilisation`, plus the three per-check utilisations (`sagbend_util`,
`overbend_util`, `tension_util`).

```bash
# All cases for one vessel in the baseline run (30 of 60)
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_04_shallow_water_pipelay.py lookup \
    --run-id baseline --vessel-id PLV-001

# All cases for one pipe size (12 of 60 — 2 vessels x 6 depths)
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_04_shallow_water_pipelay.py lookup \
    --run-id baseline --pipe-size 24in

# Only the marginal cases (5 of 60)
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_04_shallow_water_pipelay.py lookup \
    --run-id baseline --status MARGINAL

# A single water depth — integer metres (10 of 60)
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_04_shallow_water_pipelay.py lookup \
    --run-id baseline --water-depth 30

# Combine: the Large PLV in the deepest water — the 5 GO->MARGINAL/sagbend cases
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_04_shallow_water_pipelay.py lookup \
    --run-id baseline --vessel-id PLV-001 --water-depth 30
```

Messages are distinct: an unknown `--status` lists the valid tokens and exits non-zero (no query
run); an unknown `--run-id` lists the available runs; a filter matching nothing prints
`0 cases match.`; a missing db prints a run-a-sweep hint and exits non-zero without creating the db.

## 5. `rebuild-db` — regenerate the cache from text

The Results Store SQLite file is a **rebuildable, gitignored cache**. Regenerate it (and
`index.csv`) byte-identically from the per-run `cases.csv` + `manifest.json` text source of truth —
the recovery path after a partial write or a fresh checkout.

```bash
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_04_shallow_water_pipelay.py rebuild-db
```

Prints the db path plus per-run row counts.

---

## 6. Reusable agent prompt

To produce a client deliverable for a new prospect, hand an agent this prompt (fill the bracketed
fields):

> You are producing a demo_04 shallow water pipeline installation deliverable for **[client]**.
> 1. Author/adapt a **Sweep Config** at `inputs/[client].yml` from their installation point —
>    vessels, pipe sizes (nominal size -> target WT), water depths (integers), the material grade
>    selecting SMYS/SMTS, plus the engineering constants and criteria (stress limit factor, tension
>    margin, go/no-go bands, sagbend stress basis). Each axis is a list/map; length-1 freezes the
>    axis, length-N sweeps it (full cross-product).
> 2. Run it: `... --config inputs/[client].yml --run-id [client_run] --results-dir [client_results_dir]`.
> 3. Verify the run landed: `... lookup --run-id [client_run] --status MARGINAL` (and a
>    `--vessel-id PLV-001` / `--pipe-size 24in` spot check) returns the expected rows; confirm
>    `output/parametric/demo_04/[client_run]/report.html` exists. Remember the SHORT ids
>    (`PLV-001`/`PLV-002`) and the status tokens `GO`/`MARGINAL`/`NO_GO`/`ERROR`.
> 4. Report back with the generated `report.html` path and a one-line status summary
>    (e.g. "N GO / M MARGINAL / K NO_GO of 60").
>
> Do NOT modify the Baseline Run, the golden fixture, or the frozen JSON case keys. The golden
> regression suite must stay green.

## 7. Execution model — manual command, no scheduler

A client rerun is invoked as a **manual command** ("run it and come back"); the report-back artifact
is the generated `report.html`. The compute latency is not a scheduling requirement, so a queue/cron
is **deliberately not built** (YAGNI). Promoting the manual command to a cron payload is a small
later step if rerun volume ever justifies unattended runs.
