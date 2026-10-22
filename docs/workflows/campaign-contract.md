# Persisted campaign contract

`digitalmodel.workflows.campaign_contract.Campaign` connects the solver-neutral
helpers in `campaign_state` to a persistent checkpoint and checked consumer
views. A stopped campaign can resume untouched authorized cases; a successful
exit requires native readback before it counts as native completed. Acceptance
remains a separate, explicitly supplied decision.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-22 | Add persisted campaign evidence and consumer views to prevent stale criteria and liveness from being reported as results. |

## Source and numerical basis

The software requirements are issue #2298 (2026), proposed contract items 1–5,
and its parent issue #3973 (2026), recommendations on continuation, progress,
results-first status, versioned criteria and retention.
Existing continuation implementations are described in pull requests #2299 and
#2301; the sampling helpers were extended in #2935. This interface reuses those
helpers. It introduces no physical equation, engineering coefficient or default
acceptance limit.

Progress age is `observation_time - last_counter_advance_time`. The explicit
`stall_after` input uses the same clock units. A first sample, a counter reset,
or a repeated poll supplies no new evidence of advancement. A live process is
SOLVING only while a recorded counter increase is within that limit. A stale
counter is STALLED; an exited process needs successful native readback and an
explicit acceptance decision to be COMPLETE. These are software states under
issue #2298 item 2, rather than engineering acceptance criteria.

## Lifecycle and consumers

1. Call `Campaign.launch` with unique case ids, criteria id/version/source,
   explicit continuation authority, output-age limit and retention manifest.
   Retention declares inputs, plotted arrays, compact metrics, SHA-256 and an
   archive replica. The declaration is required before any case is observed.
2. Save the launch checkpoint, then call `observe` with a native iteration,
   simulation-time or log-byte counter and independent process-alive evidence.
   Save after each observation. Polling time never substitutes for counter
   advancement. The caller uses a consistent nondecreasing clock across restarts.
3. Call `collect` with the result's criteria id/version, exit code, native
   readback flag and acceptance decision. `True` qualifies, `False` withholds,
   and `None` records NOT_EVALUATED. Save after collection. Native completion
   counts successful readback independently of the acceptance decision.
4. Call `stop` and save on each stop path. The checkpoint records failed and
   untouched ids, disposition, blocker and clearance condition. `next_cases`
   returns untouched ids only for an isolated stop covered by explicit
   authority. Policy, diagnosis and shared-failure stops do not resume. Active
   cases block continuation; failed cases require a separate retry campaign.
5. Collectors use `result_rows`; monitors use `status`; workbook import uses
   `workbook_csv`; reports use `report`. Every entry point requires explicit
   governing criteria and rejects mismatches. Every export retains all rows,
   including failed and pending rows, with the criteria id/version/source.
   `mark_reported` records native completions included at the current criteria.
   The caller supplies the next action to status and report output.

`run_untouched(execute, checkpoint=...)` provides a sequential adapter runner.
It saves the launch and each attempted case before invoking the adapter, then
collects and saves each result and stop disposition. The callback receives the
case id and current criteria; it returns collection evidence plus an optional
`stop_reason`. Isolated failures continue to untouched authorized cases.
Explicit policy/diagnosis blockers prevent further calls. Exceptions and invalid
results save a diagnosis stop and propagate; abrupt termination leaves an active
case that cannot be replayed automatically. The caller retains all native
solver-specific guards, including detection of shared failures. This interface
adds no retry loop or failure-fraction threshold.

`save` replaces the checkpoint atomically in its existing directory; `load`
checks the schema, result stamps and terminal evidence. No work is launched by
loading a checkpoint. A single caller serializes checkpoint writes. The JSON
layout is `schema_version: v1`; this software layout version is independent of
the engineering criteria version.

## Changing criteria

Use `preview_criteria_change` with a new criteria identity or version, its source
and explicit re-evaluation decisions for exactly the native completed rows.
The preview names every row whose stamp changes, every changed qualification,
and every previously qualified row now withheld. Pending and failed rows also
receive the new criteria stamp; they cannot become qualified by migration.

Pass the unchanged preview to `apply_criteria_change` with an `emit` callback
that prints or retains the preview. Announcement precedes mutation; a failed
announcement or stale/modified preview prevents application. Application stores
the migration history, updates row stamps and invalidates all previous report
counts. Save immediately. A subsequent collector, monitor, workbook or report
request using the old version raises `CriteriaVersionMismatch`, including when
no qualification changed. Report counts recover only when regenerated rows are
explicitly marked as reported.

## Worked example: inputs, method and answer

Run the solver-free example with an explicitly selected output directory:

```bash
PYTHONPATH=src uv run --no-project python examples/workflows/campaign_contract.py --output "$OUTPUT_DIR"
```

The example uses three synthetic cases, a supplied age limit of 10 clock units,
and counter readings 4 at time 10 and 5 at time 20. A repeated reading at time 31
has age `31 - 20 = 11`, exceeding the input limit, so its health is STALLED.
These numbers are illustrative inputs, not recommended solver settings.

Case a completes with successful readback and passes the supplied criterion;
b fails; c remains pending. An isolated stop permits only c to resume, and its
supplied result completes and passes. Thus three cases were attempted, two
completed natively and two qualified. A revised criteria input withholds a and
retains c. The preview lists a, b and c as affected and a as newly withheld.
The revised workbook and report retain all three rows at version v2.

The final answer, after regenerating the report for a and c, is:

```text
native 3/3 attempted, 2 completed, 1 qualified; report 2/2 complete; next: Review withheld a and failed b
```

The example writes `campaign.json`, `criteria-preview.json`, `workbook.csv` and
`report.json`. Test docstrings independently derive the counts and elapsed age.

## Limits and further adoption

Retention is a launch declaration, not proof of archive replication or digest
verification. Native readback and acceptance decisions are evidence supplied by
an adapter; the interface does not parse native files or verify that evidence.
Missing engineering limits remain caller inputs or NOT_EVALUATED decisions.
Clock epochs and counter units must remain consistent across restarts.
Checkpoint replacement provides atomic visibility, not concurrent writer
coordination or a guarantee against storage failure.

Existing solver controllers are not automatically migrated by this opt-in
interface. The adapter runner demonstrates collection and continuation using
the same persisted contract without importing a licensed solver.
Private controllers and deferred solver-specific monitoring remain separate
work. The workbook output is CSV for import, not a formatted spreadsheet file.
