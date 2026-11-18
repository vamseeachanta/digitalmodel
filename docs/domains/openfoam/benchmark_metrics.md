# Benchmark metric reduction

`bench_run.sh` reduces each completed command's log with
`scripts/cfd/lib/bench_metrics.py`. The reducer reads files and runs no solver.
The TSV column names and order remain unchanged. Optional measurements that
cannot be determined appear as `nan`, with a `NOT_EVALUATED` reason on standard
error. Invalid timing prevents the benchmark completion marker from being
written.

## Calculation contract

For cumulative CPU measurements `E[1]` through `E[n]`, the reported
`s_per_it` is `E[n] / n`. A completed step has one `Time` record followed by
one `ExecutionTime` record. Time values must increase and cumulative CPU values
must be finite, nonnegative and nondecreasing. Missing, repeated, reversed or
incomplete records are refused. A solver can finish successfully without
providing enough valid timing data to produce a benchmark result.

For window length `w`, set `k = min(w, n)` and `E[0] = 0`. The first-window
mean is `E[k] / k`; the last-window mean is `(E[n] - E[n-k]) / k`. This
counts exactly `k` completed steps in each window, including a single-step
run. Initialization CPU time before the first measurement is included in the
first window and overall mean. These are CPU timings from `ExecutionTime`,
not wall timings from `ClockTime` or aggregate CPU consumption across ranks.
The existing window length of 50 is a reporting convention inherited from
`bench_run.sh` at revision `6476218e`, not a convergence or acceptance limit.
The first-window calculation now includes the first completed step; the
previous implementation divided the difference between its first and last
samples by one fewer interval.

The cell count comes from the serial `constant/polyMesh/owner` header when
present. Otherwise it is the sum of positive cell counts from every requested
processor rank. A partial decomposition is unavailable. Additional stale
processor directories are ignored. Serial and decomposed copies are never
added together. An unreadable serial header is unavailable rather than silently
substituted with another mesh. Header inspection follows the existing
`mesh_store.py::mesh_cells` contract.

`us_per_cell_iteration = s_per_it * 10^6 / cell_count`. The conversion follows
the SI prefix micro, which denotes `10^-6` ([NIST SI prefixes](https://physics.nist.gov/cuu/Units/prefixes.html)).

Force and coefficient summaries select the last quarter of data rows by sample
count, preserving the prior reporting convention. With `n` rows and tail
fraction `f`, rows from zero-based index `floor(n * (1-f))` onward are used.
This is an arithmetic sample mean, not a time-weighted integral. The window
length and tail fraction are required inputs to the standalone reducer.
No physical acceptance threshold is supplied.

Force columns are selected by the `total_x`, `pressure_x` and `viscous_x`
headers. Flat and parenthesized numeric vectors are accepted. Each component's
share is `100 * mean(component_x) / mean(total_x)`. A zero total leaves both
shares unavailable. Porous forces, when present, contribute to total force;
pressure and viscous shares therefore need not add to 100 percent. The output
axis is the function object's x component; no drag-direction projection is
inferred. The source for this layout is OpenFOAM v2312 (2023),
[`forces.C`, integrated-data header and writer methods](https://api.openfoam.com/2312/forces_8C_source.html).

`Cd_last_quarter` uses the `Cd` header, since active coefficient columns may
vary. The source is OpenFOAM v2312 (2023),
[`forceCoeffs.C`, integrated-data header and writer methods](https://api.openfoam.com/2312/forceCoeffs_8C_source.html).
The final `p_rgh` initial residual is taken from the log's last matching solver
record, following the existing benchmark script convention. An invalid final
residual is unavailable; an older valid residual is not substituted.

## Standalone use

```sh
python3 scripts/cfd/lib/bench_metrics.py \
  --case benchmark/variants/base --log benchmark/variants/base/log.synthetic \
  --ranks 2 --window 50 --tail-fraction 0.25
```

Standard output contains the nine numeric TSV fields after `variant` and
`ranks`. Diagnostics go to standard error. Exit status is nonzero for invalid
options, an unreadable log or invalid timing. Missing optional cell, force,
coefficient or residual data does not invalidate the timing measurements.

## Limits

The reducer supports one run and a unique `force.dat` and `coefficient.dat`
below `postProcessing`. Missing headers, repeated headers, duplicate columns,
nonfinite or malformed rows, non-increasing sample times, and multiple matching
files leave the affected optional measurement unavailable. Restart segments
are not merged and a legacy column order is not guessed. Optional sample
histories are not aligned to the timing log or checked for convergence.
The reducer checks the representation, not the physical validity of a solve.
No OpenFOAM execution or performance measurement is required for its tests.
