# demo_02 Runbook — Pipeline Wall Thickness Multi-Code Comparison

Operator runbook for the demo_02 parametric deliverable. Terms (**Sweep Config**, **Run**,
**Baseline Run**, **Results Store**) carry the same meaning as in the demo_01 runbook and
`CONTEXT.md`; the storage/latency decision is shared with demo_01 (ADR-0003 — SQLite fast-read,
rebuildable cache).

All commands run from the `digitalmodel` repo root with this prefix:

```bash
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_02_wall_thickness_multicode.py
```

(Substitute `uv run python` with `.venv/bin/python` on machines without `uv`.)

---

## 1. Verified Baseline Run

Reproduces the committed 72-case golden matrix exactly (6 pipe sizes × 3 design codes × 4 internal
pressures). Writes the legacy report `output/demo_02_wall_thickness_report.html`, the JSON
`results/demo_02_wall_thickness_results.json`, and seeds the `baseline` run into the **Results
Store**.

```bash
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_02_wall_thickness_multicode.py
```

The `results[]` payload of the JSON is byte-identical to
`tests/fixtures/golden/demo_02_baseline_results.json`. The golden regression test enforces this:

```bash
PYTHONPATH=examples/demos/gtm:src .venv/bin/python -m pytest \
    examples/demos/gtm/tests/test_demo_02_baseline_golden.py -q
```

## 2. Client Sweep Run

Run under a named `run_id` (writes a per-run report + store partition WITHOUT touching the Baseline
artifacts). Use `--results-dir` to point the legacy JSON + Results Store at a client directory.

```bash
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_02_wall_thickness_multicode.py \
    --run-id acme --results-dir /path/to/acme/results
```

Outputs for a named run:

| Artifact | Path |
|----------|------|
| Per-run report (client-facing) | `output/parametric/demo_02/acme/report.html` |
| Per-run cases (text source of truth) | `<results-dir>/parametric/demo_02/acme/cases.csv` |
| Per-run manifest (resolved config + provenance) | `<results-dir>/parametric/demo_02/acme/manifest.json` |
| Run row in the Results Store cache | `<results-dir>/parametric/demo_02/results.db` |

Re-running the same `--run-id` replaces that run in place (delete-then-insert) — no duplicates.
A `--run-id` is validated as a single safe path segment (`[A-Za-z0-9._-]+`, never `''`/`.`/`..`),
so traversal values like `../../evil` are rejected non-zero before any directory is created.

## 3. Report-only (from cache)

Regenerate the Baseline report/JSON from the cached results without re-running the sweep (no
engineering modules needed). The Results Store is NOT rewritten on this path.

```bash
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_02_wall_thickness_multicode.py --from-cache
```

> The cache always holds the Baseline 72 cases. Passing `--from-cache --run-id <name>` emits a
> warning and writes the report to the **baseline** path (not a named-run subdir) so a cached run is
> never mislabeled. To compute a genuine named run, drop `--from-cache`.

## 4. `lookup` — operator ad-hoc case lookup

Read-only filtered query over the Results Store cache for off-chart questions during a live demo.
The db is opened read-only and is NEVER created by `lookup`; run a sweep or `rebuild-db` first if it
is missing.

Filters (all optional, AND-combined; each binds verbatim to the stored value):

| Flag | Meaning | Notes |
|------|---------|-------|
| `--run-id` | Run to query | default `baseline` |
| `--pipe-size` | `pipe_size` | **includes the literal `"`** — shell-quote it, e.g. `'12"'` |
| `--code` | design code | the **SPACED display string** (`DNV-ST-F101`, `API RP 1111`, `PD 8010-2`); shell-quote `"API RP 1111"` |
| `--pressure` | internal pressure MPa | integer, e.g. `20` |
| `--safe` / `--unsafe` | filter on `is_safe` | mutually exclusive; `--safe` → passing cases, `--unsafe` → failing |
| `--base-dir` | Store base dir | default the demo's `results/` dir |

```bash
# All passing cases in the baseline run (48 of 72)
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_02_wall_thickness_multicode.py lookup \
    --run-id baseline --safe

# All failing cases (24 of 72)
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_02_wall_thickness_multicode.py lookup \
    --run-id baseline --unsafe

# A single design code — note the SPACED display string, shell-quoted
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_02_wall_thickness_multicode.py lookup \
    --run-id baseline --code "API RP 1111"

# A single pipe size — the literal '"' must be shell-quoted
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_02_wall_thickness_multicode.py lookup \
    --run-id baseline --pipe-size '12"'

# Combine: 20 MPa, PD 8010-2, only failures
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_02_wall_thickness_multicode.py lookup \
    --run-id baseline --pressure 20 --code "PD 8010-2" --unsafe
```

> **Shell-quoting reminders.** `pipe_size` contains a literal double-quote (`12"`), so wrap the value
> in single quotes: `--pipe-size '12"'`. `--code` is matched against the **spaced** display string
> (not a hyphenated/slug form), so `--code "API RP 1111"` is correct and `--code API-RP-1111` is an
> unknown-code error.

Messages are distinct: an unknown `--code` lists the valid display strings; an unknown `--run-id`
lists the available runs; a filter matching nothing prints `0 cases match.`; a missing db prints a
run-a-sweep hint and exits non-zero without creating the db.

## 5. `rebuild-db` — regenerate the cache from text

The Results Store SQLite file is a **rebuildable, gitignored cache**. Regenerate it (and
`index.csv`) byte-identically from the per-run `cases.csv` + `manifest.json` text source of truth —
the recovery path after a partial write or a fresh checkout.

```bash
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_02_wall_thickness_multicode.py rebuild-db
```

Prints the db path plus per-run row counts.

---

## 6. Reusable agent prompt

To produce a client deliverable for a new prospect, hand an agent this prompt (fill the bracketed
fields):

> You are producing a demo_02 wall-thickness multi-code comparison deliverable for **[client]**.
> 1. Author/adapt a **Sweep Config** at `inputs/[client].yml` from their design point — pipe sizes,
>    design codes (the spaced display strings `DNV-ST-F101` / `API RP 1111` / `PD 8010-2`), internal
>    pressures, grade, water depth. Each axis is a list; length-1 freezes the axis, length-N sweeps
>    it (full cross-product).
> 2. Run it: `... --run-id [client_run] --results-dir [client_results_dir]`.
> 3. Verify the run landed: `... lookup --run-id [client_run] --code "API RP 1111"` (and
>    `--safe`/`--unsafe`) returns the expected rows; confirm
>    `output/parametric/demo_02/[client_run]/report.html` exists.
> 4. Report back with the generated `report.html` path and a one-line status summary
>    (e.g. "N of M cases safe").
>
> Do NOT modify the Baseline Run, the golden fixture, or the 14 frozen JSON keys. The golden
> regression suite must stay green. Remember the shell-quoting: `--pipe-size '12"'` and
> `--code "API RP 1111"` (spaced display string).

## 7. Execution model — manual command, no scheduler

A client rerun is invoked as a **manual command** ("run it and come back"); the report-back artifact
is the generated `report.html`. The compute latency is not a scheduling requirement, so a queue/cron
is **deliberately not built** (YAGNI). Promoting the manual command to a cron payload is a small
later step if rerun volume ever justifies unattended runs.
```
