# Envelope plotting error handling

## Plan-lite

Scope: preserve errors raised while initializing or using optional envelope
plotting, and close the figure created by each plotting call. The implementation
and its test routing form one scope.

Implementation steps:

1. Validate envelope points before loading plotting dependencies.
2. Return `None` only when the initial import cannot find the top-level
   `matplotlib` package. Propagate backend, transitive import, rendering and
   saving errors with their original exception identity and cause.
3. Close the created figure in a `finally` block while preserving figures
   belonging to other callers.
4. Route the plotting regression file through the existing marine-operations
   test domain.

Validation: run the 16 plotting cases and the 8 original operation-envelope
cases, then check the concrete domain route and gaps at unavailable periods.
The plotting cases include optional-package absence, initialization failures,
validation before import, an Agg PNG control and cleanup after failures.

## Numerical scope

The 99% bounded response-moment coverage gate is unchanged; heading conventions
and out-of-band response sensitivity remain outside this change.

Basis B4 in
[the envelope calculation basis](../../src/digitalmodel/marine_ops/docs/envelope-basis.md)
defines coverage using the response moment, which includes squared response
amplitude weighting. Wave-energy coverage alone does not describe that gate.
