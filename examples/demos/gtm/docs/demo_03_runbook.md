# demo_03 Runbook — Deepwater Mudmat Installation Analysis

Operator runbook for the demo_03 parametric deliverable. Terms (**Sweep Config**, **Run**,
**Baseline Run**, **Results Store**) carry the same meaning as in the demo_01 / demo_02 runbooks and
`CONTEXT.md`; the storage/latency decision is shared with the other demos (ADR-0003 — SQLite
fast-read, rebuildable cache).

All commands run from the `digitalmodel` repo root with this prefix:

```bash
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_03_deepwater_mudmat_installation.py
```

(Substitute `uv run python` with `.venv/bin/python` on machines without `uv`.)

---

## 1. Verified Baseline Run

Reproduces the committed 180-case golden matrix exactly (2 vessels × 6 water depths × 3 mudmat
structures × 5 sea states = 180). Writes the legacy report
`output/demo_03_mudmat_installation_report.html`, the JSON
`results/demo_03_mudmat_installation_results.json`, and seeds the `baseline` run into the **Results
Store**.

```bash
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_03_deepwater_mudmat_installation.py
```

The `cases[]` payload of the JSON is byte-identical to
`tests/fixtures/golden/demo_03_baseline_results.json`. The golden regression test enforces this:

```bash
PYTHONPATH=examples/demos/gtm:src .venv/bin/python -m pytest \
    examples/demos/gtm/tests/test_demo_03_baseline_golden.py -q
```

The baseline status split is **174 GO / 6 MARGINAL / 0 NO_GO**.

## 2. Client Sweep Run

Run under a named `run_id` with a client **Sweep Config** (writes a per-run report + store partition
WITHOUT touching the Baseline artifacts). Use `--results-dir` to point the legacy JSON + Results
Store at a client directory.

```bash
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_03_deepwater_mudmat_installation.py \
    --config inputs/acme.yml --run-id acme --results-dir /path/to/acme/results
```

Outputs for a named run:

| Artifact | Path |
|----------|------|
| Per-run report (client-facing) | `output/parametric/demo_03/acme/report.html` |
| Per-run cases (text source of truth) | `<results-dir>/parametric/demo_03/acme/cases.csv` |
| Per-run manifest (resolved config + provenance) | `<results-dir>/parametric/demo_03/acme/manifest.json` |
| Run row in the Results Store cache | `<results-dir>/parametric/demo_03/results.db` |

Re-running the same `--run-id` replaces that run in place (delete-then-insert) — no duplicates.
A `--run-id` is validated as a single safe path segment (`[A-Za-z0-9._-]+`, never `''`/`.`/`..`),
so traversal values like `../../evil` are rejected non-zero before any directory is created.

## 3. Report-only (from cache)

Regenerate the Baseline report/JSON from the cached results without re-running the sweep. The
Results Store is NOT rewritten on this path.

```bash
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_03_deepwater_mudmat_installation.py --from-cache
```

> `--from-cache` is only supported with the committed baseline config (a non-default `--config`
> cannot be proven to match the cache and is refused). The cache always holds the Baseline 180
> cases. Passing `--from-cache --run-id <name>` emits a warning and writes the report to the
> **baseline** path (not a named-run subdir) so a cached run is never mislabeled. To compute a
> genuine named run, drop `--from-cache`.

## 4. `lookup` — operator ad-hoc case lookup

Read-only filtered query over the Results Store cache for off-chart questions during a live demo.
The db is opened read-only and is NEVER created by `lookup`; run a sweep or `rebuild-db` first if it
is missing.

Filters (all optional, AND-combined; each binds verbatim to the stored value):

| Flag | Meaning | Notes |
|------|---------|-------|
| `--run-id` | Run to query | default `baseline` |
| `--vessel-id` | `vessel_id` | the **SHORT id** `CSV-001` / `CSV-002` (NOT the display name) |
| `--structure-id` | `structure_id` | the **SHORT id** `MUD-S` / `MUD-M` / `MUD-L` (NOT the display name) |
| `--water-depth` | `water_depth_m` | **integer** metres, e.g. `2000` |
| `--hs` | `hs_m` | **float** metres, e.g. `2.0` |
| `--status` | `overall_status` | one of `GO` / `MARGINAL` / `NO_GO` / `ERROR` |
| `--base-dir` | Store base dir | default the demo's `results/` dir |

> **N3 — identifiers are the SHORT ids.** `--vessel-id` / `--structure-id` match the catalog SHORT
> ids (`CSV-001`/`CSV-002` for vessels; `MUD-S`/`MUD-M`/`MUD-L` for structures), **not** the display
> names (e.g. "Large CSV", "Mudmat-L-200te"). The valid `--status` tokens are exactly
> `GO` / `MARGINAL` / `NO_GO` / `ERROR` (note the underscore in `NO_GO`).

The displayed columns are `vessel_id`, `structure_id`, `water_depth_m`, `hs_m`, `overall_status`,
`governing_phase`, `max_utilisation`, plus the five per-phase utilisations
(`lift-off_utilisation`, `in-air_utilisation`, `splash_zone_utilisation`, `lowering_utilisation`,
`landing_utilisation`). The per-phase columns are hyphen-prefixed, so the SELECT quotes every
identifier — they resolve cleanly rather than being arithmetic-parsed.

```bash
# All cases for one vessel in the baseline run (90 of 180)
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_03_deepwater_mudmat_installation.py lookup \
    --run-id baseline --vessel-id CSV-001

# All cases for the large mudmat (60 of 180)
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_03_deepwater_mudmat_installation.py lookup \
    --run-id baseline --structure-id MUD-L

# Only the marginal cases (6 of 180)
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_03_deepwater_mudmat_installation.py lookup \
    --run-id baseline --status MARGINAL

# A single water depth — integer metres (30 of 180)
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_03_deepwater_mudmat_installation.py lookup \
    --run-id baseline --water-depth 2000

# A single sea state — float metres (36 of 180)
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_03_deepwater_mudmat_installation.py lookup \
    --run-id baseline --hs 2.0

# Combine: the smaller CSV on the large mudmat in the deepest water at the worst sea state —
# the governing marginal case (1 of the 6)
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_03_deepwater_mudmat_installation.py lookup \
    --run-id baseline --vessel-id CSV-002 --structure-id MUD-L --water-depth 3000 --hs 3.0 --status MARGINAL
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
    examples/demos/gtm/demo_03_deepwater_mudmat_installation.py rebuild-db
```

Prints the db path plus per-run row counts.

---

## 6. Reusable agent prompt

To produce a client deliverable for a new prospect, hand an agent this prompt (fill the bracketed
fields):

> You are producing a demo_03 deepwater mudmat installation deliverable for **[client]**.
> 1. Author/adapt a **Sweep Config** at `inputs/[client].yml` from their installation point —
>    vessels, water depths (integers), mudmat structures, and sea states (Hs, floats), plus the
>    engineering constants (DAFs, wire MBL safety factor, tilt/operating-radius limits). Each axis is
>    a list; length-1 freezes the axis, length-N sweeps it (full cross-product).
> 2. Run it: `... --config inputs/[client].yml --run-id [client_run] --results-dir [client_results_dir]`.
> 3. Verify the run landed: `... lookup --run-id [client_run] --status MARGINAL` (and a
>    `--vessel-id CSV-001` / `--structure-id MUD-L` spot check) returns the expected rows; confirm
>    `output/parametric/demo_03/[client_run]/report.html` exists. Remember the SHORT ids
>    (`CSV-001`/`CSV-002`, `MUD-S`/`MUD-M`/`MUD-L`) and the status tokens `GO`/`MARGINAL`/`NO_GO`/`ERROR`.
> 4. Report back with the generated `report.html` path and a one-line status summary
>    (e.g. "N GO / M MARGINAL / K NO_GO of 180").
>
> Do NOT modify the Baseline Run, the golden fixture, or the 11 frozen JSON case keys. The golden
> regression suite must stay green.

## 7. Execution model — manual command, no scheduler

A client rerun is invoked as a **manual command** ("run it and come back"); the report-back artifact
is the generated `report.html`. The compute latency is not a scheduling requirement, so a queue/cron
is **deliberately not built** (YAGNI). Promoting the manual command to a cron payload is a small
later step if rerun volume ever justifies unattended runs.
