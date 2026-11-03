# MPI placement receipts for continuation planning

## Summary

The offline `mpi_placement` module preserves explicitly validated placement
flags, checks restart provenance and compares supplied effective affinity.
It retains timing observations without setting a throughput acceptance limit.
It does not launch or inspect a running solver. Production continuation scripts
are unchanged; integration remains a later step for issue #2109.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-19 | Add offline placement evidence contracts to prevent unrecorded continuation flag changes. |

## Scope and sources

The implementation is
`src/digitalmodel/solvers/openfoam/mpi_placement.py`; its independent synthetic
examples are in `tests/solvers/openfoam/test_mpi_placement.py`.

The existing `scripts/cfd/continue_case.sh` accepts optional flags through
`DM_CFD_MPIRUN_FLAGS`. The receipt API supplies a future integration contract;
it does not change that script or the batch runner's decomposition validation.

- [Open MPI v5.0.6, mpirun, Mapping, ranking, and binding options](https://docs.open-mpi.org/en/v5.0.6/man-openmpi/man1/mpirun.1.html#mapping-ranking-and-binding-options)
  describes separate placement controls. Tokens are retained in order and no
  mapping policy is selected by this module.
- [Microsoft Learn, mpiexec (2018), affinity options](https://learn.microsoft.com/en-us/powershell/high-performance-computing/mpiexec)
  describes different option syntax. These tokens are retained without
  translation to Open MPI syntax.
- [Python 3.11 Library Reference, time.monotonic](https://docs.python.org/3.11/library/time.html#time.monotonic)
  specifies that differences between clock readings are valid, in seconds.
  Interval mean seconds per iteration is defined here as
  `(elapsed_end - elapsed_start) / (iteration_end - iteration_start)`.
  This is an arithmetic definition; no fitted coefficient or engineering
  acceptance limit is used.

These public pages were retrieved successfully on 2026-10-19. Tests use
synthetic observations and independent calculations written in their docstrings;
they do not establish solver performance.

## Receipt contract

`Launcher` records the caller-observed backend, executable and version.
`PlacementReceipt` records its exact ordered flags, rank count, case digest,
checkpoint digest, topology digest, rank CPU/socket observations, optional
external validation reference and optional cumulative timing samples.
`to_json()` and `from_json()` retain this evidence. Unknown or duplicate JSON
fields and malformed identifiers are rejected.

The producer supplies manifests for the digest inputs:

- `case_sha256`: mesh and numerical dictionaries, excluding intentionally
  changed continuation controls such as the requested end time.
- `checkpoint_sha256`: the complete restart state associated with the receipt.
- `topology_sha256`: one execution domain, its topology and CPU/socket numbering.
  A topology shape alone is insufficient because identical CPU identifiers can
  describe different execution domains. Multi-node observations are unsupported.

These digests are assertions about external manifests; the module does not read
or independently authenticate their content. The producer retains and verifies
the manifests and their evidence. `validation_reference` locates external
placement/performance validation; its presence is a caller attestation and does
not authenticate approval or establish throughput. Unknown observations and
topology use `None`, rather than fabricated identifiers.

Each available observation has a unique nonnegative integer rank and nonempty
sets of allowed CPU and socket identifiers. CPU/socket identifiers are scoped
to the recorded execution domain. The observer supplies the CPU-to-socket
relationship; it is not inferred or checked against hardware here. Observations
represent allowed affinity, not instantaneous CPU occupancy.

## Continuation planning and effective affinity

`plan_continuation` requires complete baseline affinity, known topology and
external validation evidence. Requested launcher identity, rank count, case,
checkpoint and topology digests must match the baseline exactly. These checks
apply to explicit overrides as well as defaults.

An omitted `flags` argument reuses the baseline tokens in their recorded order.
An explicit tuple, including an empty tuple that clears optional flags,
requires a nonempty `override_reason`. A reason without an override is rejected.
The returned plan records the choice, the reason, the baseline receipt SHA-256
and the restart checkpoint SHA-256. Changed flags require new external
validation; they do not inherit the baseline's validation claim.

`check_effective_placement` compares supplied post-launch evidence against the
receipt by rank and by CPU/socket sets, independent of observation ordering:

| Status | Meaning |
| --- | --- |
| `PASS` | Complete observations match within the same recorded execution domain and topology. |
| `FAIL` | Complete observations differ within that execution domain and topology. |
| `NOT_EVALUATED` | Baseline or current affinity is incomplete, or topology is unavailable or different. |

Duplicate or out-of-range ranks are malformed evidence and raise `ValueError`.
`PASS` denotes affinity equality only. It does not validate throughput, flag
semantics, external attestation or engineering convergence. Callers can compare
an unattested receipt, but cannot use it for default continuation planning.

## Iteration timing

`timing_samples` retains `(iteration, cumulative_elapsed_seconds)` pairs from one
monotonic clock within one run segment. `iteration_timing` returns interval
means separately; gaps in iteration numbers produce an interval mean, not
invented individual timings. The first sample anchors the differences and is
not assigned a duration. A restarted clock needs a separate receipt/segment.
Missing observations or a single anchor return `NOT_EVALUATED`.

For the independent synthetic example, elapsed seconds increase from 5 to 9
while the iteration count increases from 10 to 12. The mean is
`(9 - 5) / (12 - 10) = 2` seconds per iteration. The next observation at
iteration 13 and elapsed 12 gives `(12 - 9) / (13 - 12) = 3` seconds per iteration.
Timing `PASS` means intervals were calculated from valid samples; no performance
criterion is evaluated.

## Later integration

Production receipt capture, observed-affinity collection, portable launch
execution, manifest generation and checkpoint verification remain outstanding.
An integration must retain the receipt, verify the checkpoint/manifests before
mutation, pass flags as argv tokens, collect effective per-rank affinity after
launch, record unavailable evidence explicitly and obtain external validation
for any deliberate placement change. No universal binding rule is implied.
