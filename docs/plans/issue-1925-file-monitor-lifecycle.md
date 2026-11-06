# File monitor lifecycle plan

The file monitor change addresses startup ordering, observer event delivery and
resource ownership. The implementation and acceptance checks remain limited to
the existing monitor API and text-file analysis tests.

## Plan-lite

### Scope

Keep the consumer alive when initialization yields, and deliver observer events
on the event loop captured by startup. Make shutdown and failed startup account
for consumers, polling, rescans, observer threads and I/O executor workers.

### Implementation steps

1. Set the running state and capture the event loop before starting consumers or
   awaiting the initial scan.
2. Schedule observer deliveries through `call_soon_threadsafe`. Reject stale
   deliveries after shutdown or restart.
3. Serialize lifecycle operations, await owned tasks during cleanup, clear loop
   references and recreate the I/O executor on restart.
4. Exercise startup, observer delivery, polling, failed startup, cancellation,
   restart and callback-initiated shutdown with deterministic regression tests.

### Acceptance checks

- `test_start_keeps_consumer_alive` yields during initialization, then confirms
  that an event reaches the consumer.
- `test_observer_thread_event_reaches_consumer` delivers an event from a real
  thread and confirms exactly one callback on the captured event loop.
- Cleanup assertions run before asynchronous runner cleanup and check that owned
  tasks, observer threads and executor workers have finished.
- Run all 17 lifecycle tests and the complete results-analysis test directory;
  report collected, passed, failed and skipped counts.
- Check changed text for whitespace, conflict markers, publication identifiers
  and secrets, and run the repository structure and engineering-register checks.
- Inspect completed hosted checks and distinguish failures caused by this change
  from failures in unchanged files on the base branch.

## Limits

Observer setup uses a deterministic substitute with a real delivery thread.
Native filesystem notifications are not exercised. The tests use text files and
require no simulation files or licensed solver. Shutdown waits for synchronous
I/O and observer completion, so slow filesystem operations can delay it.
