---
status: accepted
---

# Results Store is a queryable SQLite cache: fast read path for demos, async write path for client reruns

The demo deliverable must let an operator **look up any case's result instantly** during a live
client demo, while fresh client parametric reruns can tolerate a 1-hour-to-next-day turnaround.
That is a fast read path and a slow write path with different requirements.

We decided the canonical fast-lookup layer is a **SQLite Results Store** (`sqlite3`, Python stdlib —
no server, no new dependency, portable to a client laptop). Tables: `runs` (run_id, UTC timestamp,
resolved yaml snapshot, git SHA, total_cases, summary counts) and `cases` (run_id FK + every input
parameter and every output per row). Demo lookups are `SELECT … WHERE` queries.

The persisted input is the **resolved** Sweep Config (post-defaults) — authoritative, so a Run stays
reproducible even if the source `inputs/*.yml` is later edited — plus a **reference to the raw source
path** for traceability. The `cases` table stores resolved per-case parameters alongside outputs.

To preserve ADR-0002 (yaml is source of truth) and avoid binary-in-git hazards, the SQLite file is a
**rebuildable cache and is gitignored**; the committed source of truth remains the yaml inputs plus
per-run `cases.csv` / `manifest.json`. A `--rebuild-db` step repopulates SQLite from those text
artifacts.

## Consequences

- **Read path:** the `baseline` run and any precomputed client sweeps are seeded into SQLite before
  a demo, so lookups are instant.
- **Write path:** a new client rerun (`--input client.yml --run-id …`) appends to CSV + SQLite. It
  is invoked as a **manual command** ("run it and come back"); the report-back artifact is the
  generated `report.html`. The ~1hr/next-day latency is compute time, not a scheduling requirement —
  so a queue/cron is **deliberately not built** (YAGNI). Promoting the manual command to a cron
  payload is a small later step if rerun volume ever justifies unattended runs.
- **Git holds text only** (yaml + CSV + manifest); the DB is derived. No binary churn, no merge
  conflicts on the store.
- CSV remains the portable client export; parquet/DuckDB is the documented scale-up path beyond
  ~10^5 rows (not built now — SQLite covers the demo's scale).
- **Presentation surfaces:** the branded HTML report (5 Plotly charts) is the *client-facing*
  artifact, regenerated per Run; a small `lookup` CLI over SQLite is the *operator's* ad-hoc
  answer tool for off-chart questions; raw SQL is never shown to a client.
- #633–#636 inherit the Results-Store + fast-read/async-write template.
