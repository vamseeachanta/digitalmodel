# demo_05 Runbook — Deepwater Rigid Jumper Installation Analysis

Operator runbook for the demo_05 parametric deliverable. Terms (**Sweep Config**, **Run**,
**Baseline Run**, **Results Store**) carry the same meaning as in the demo_01 / demo_02 / demo_03 /
demo_04 runbooks and `CONTEXT.md`; the storage/latency decision is shared with the other demos
(ADR-0003 — SQLite fast-read, rebuildable cache).

All commands run from the `digitalmodel` repo root with this prefix:

```bash
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_05_deepwater_rigid_jumper_installation.py
```

(Substitute `uv run python` with `.venv/bin/python` on machines without `uv`.)

---

## 1. Verified Baseline Run

Reproduces the committed 300-case golden matrix exactly (2 vessels × 5 jumper lengths × 6 water
depths × 5 significant wave heights = 300). Writes the legacy report
`output/demo_05_jumper_installation_report.html`, the JSON
`results/demo_05_jumper_installation_results.json`, and seeds the `baseline` run into the **Results
Store**.

```bash
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_05_deepwater_rigid_jumper_installation.py
```

The `results[]` payload of the JSON is byte-identical to the `cases[]` payload of
`tests/fixtures/golden/demo_05_baseline_results.json`. The golden regression test enforces this:

```bash
PYTHONPATH=examples/demos/gtm:src .venv/bin/python -m pytest \
    examples/demos/gtm/tests/test_demo_05_baseline_golden.py -q
```

The baseline status split is **240 GO / 60 NO_GO** (no MARGINAL, no ERROR). In-air lift bending
governs all 300 cases (utilisation scales as span² ≈ length²); only the longest jumper (`JMP-100`,
75 m horizontal span) exceeds the allowable → NO_GO. That is 60 cases = 1 length × 2 vessels ×
6 depths × 5 Hs. The weather axes (depth, Hs) drive only the splash/lowering phases, which stay far
below the lift-bending utilisation, so **jumper length is the dominant discriminator**.

## 2. Client Sweep Run

Run under a named `run_id` with a client **Sweep Config** (writes a per-run report + store partition
WITHOUT touching the Baseline artifacts). Use `--results-dir` to point the legacy JSON + Results
Store at a client directory.

```bash
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_05_deepwater_rigid_jumper_installation.py \
    --config inputs/acme.yml --run-id acme --results-dir /path/to/acme/results
```

Outputs for a named run:

| Artifact | Path |
|----------|------|
| Per-run report (client-facing) | `output/parametric/demo_05/acme/report.html` |
| Per-run cases (text source of truth) | `<results-dir>/parametric/demo_05/acme/cases.csv` |
| Per-run manifest (resolved config + provenance) | `<results-dir>/parametric/demo_05/acme/manifest.json` |
| Run row in the Results Store cache | `<results-dir>/parametric/demo_05/results.db` |

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
    examples/demos/gtm/demo_05_deepwater_rigid_jumper_installation.py --from-cache
```

> `--from-cache` is only supported with the committed baseline config (a non-default `--config`
> cannot be proven to match the cache and is refused). The cache always holds the Baseline 300
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
| `--vessel-id` | `vessel_id` | the **SHORT id** `CSV-001` / `CSV-002` (NOT the display name) |
| `--jumper-id` | `jumper_id` | jumper id token: `JMP-20` / `JMP-40` / `JMP-60` / `JMP-80` / `JMP-100` |
| `--water-depth` | `water_depth_m` | **integer** metres, e.g. `500` |
| `--hs` | `hs_m` | **float** metres (significant wave height), e.g. `2.0` |
| `--status` | `overall_status` | one of `GO` / `MARGINAL` / `NO_GO` / `ERROR` |
| `--base-dir` | Store base dir | default the demo's `results/` dir |

> **Identifiers are the SHORT ids.** `--vessel-id` matches the catalog SHORT ids
> (`CSV-001`/`CSV-002`), **not** the display names (`Large CSV`/`Medium CSV`). The valid
> `--status` tokens are exactly `GO` / `MARGINAL` / `NO_GO` / `ERROR` (note the underscore in
> `NO_GO`).

The displayed columns are `vessel_id`, `jumper_id`, `length_m`, `water_depth_m`, `hs_m`,
`overall_status`, `governing_phase`, and `max_utilisation`.

```bash
# All cases for one vessel in the baseline run (150 of 300)
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_05_deepwater_rigid_jumper_installation.py lookup \
    --run-id baseline --vessel-id CSV-001

# All cases for one jumper (60 of 300 — 2 vessels x 6 depths x 5 Hs)
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_05_deepwater_rigid_jumper_installation.py lookup \
    --run-id baseline --jumper-id JMP-100

# Only the no-go cases (60 of 300 — all JMP-100, lift bending governs)
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_05_deepwater_rigid_jumper_installation.py lookup \
    --run-id baseline --status NO_GO

# A single water depth — integer metres (50 of 300)
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_05_deepwater_rigid_jumper_installation.py lookup \
    --run-id baseline --water-depth 500

# A single sea state — float metres (60 of 300)
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_05_deepwater_rigid_jumper_installation.py lookup \
    --run-id baseline --hs 2.0

# Combine: the Large CSV in the shallowest water — 25 cases (5 jumpers x 5 Hs)
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_05_deepwater_rigid_jumper_installation.py lookup \
    --run-id baseline --vessel-id CSV-001 --water-depth 500
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
    examples/demos/gtm/demo_05_deepwater_rigid_jumper_installation.py rebuild-db
```

Prints the db path plus per-run row counts.

---

## 6. Reusable agent prompt

To produce a client deliverable for a new prospect, hand an agent this prompt (fill the bracketed
fields):

> You are producing a demo_05 deepwater rigid jumper installation deliverable for **[client]**.
> 1. Author/adapt a **Sweep Config** at `inputs/[client].yml` from their installation point —
>    vessels, jumper lengths (floats, matching the catalog `length_m`), water depths (integers),
>    significant wave heights (floats), plus the engineering constants and criteria (DAFs,
>    hydrodynamic coefficients, velocities, rigging mass, wire/bending allowables, tie-in tolerance,
>    Tp coefficient, go/no-go bands, and the Phase-2 lift-span / Phase-5 tie-in-span model
>    parameters). Each axis is a list; length-1 freezes the axis, length-N sweeps it (full
>    cross-product).
> 2. Run it: `... --config inputs/[client].yml --run-id [client_run] --results-dir [client_results_dir]`.
> 3. Verify the run landed: `... lookup --run-id [client_run] --status NO_GO` (and a
>    `--vessel-id CSV-001` / `--jumper-id JMP-100` spot check) returns the expected rows; confirm
>    `output/parametric/demo_05/[client_run]/report.html` exists. Remember the SHORT ids
>    (`CSV-001`/`CSV-002`), the jumper ids (`JMP-20`…`JMP-100`), and the status tokens
>    `GO`/`MARGINAL`/`NO_GO`/`ERROR`.
> 4. Report back with the generated `report.html` path and a one-line status summary
>    (e.g. "N GO / M MARGINAL / K NO_GO of 300").
>
> Do NOT modify the Baseline Run, the golden fixture, or the frozen JSON case keys. The golden
> regression suite must stay green.

## 7. Execution model — manual command, no scheduler

A client rerun is invoked as a **manual command** ("run it and come back"); the report-back artifact
is the generated `report.html`. The compute latency is not a scheduling requirement, so a queue/cron
is **deliberately not built** (YAGNI). Promoting the manual command to a cron payload is a small
later step if rerun volume ever justifies unattended runs.
