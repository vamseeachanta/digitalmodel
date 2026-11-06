# OrcaFlex Parallel Helper Configuration

The optimized parallel helper selects its worker count dynamically when
`num_threads=None`. A fixed optimal worker count is **UNVERIFIED**; an
independent benchmark is not supplied by this guide. Performance qualification
is **NOT_EVALUATED**. See the [helper evidence](docs/helper-evidence.html) for
source boundaries and solver-free checks.

## Source basis

The implementation basis is repository revision
`fb8a214bdb622ed04f0b6473a9311695c9a5a314`:

- [OrcaFlexOptimizedParallelAnalysis.__init__](orcaflex_optimized_parallel_v2.py)
  accepts `num_threads=None` and `use_processes=True`. The constructor retains
  the supplied count and sets `dynamic_threads` from whether the count is absent.
- `process_files_parallel` in the same file returns early for an empty batch.
  Otherwise it calls `SimpleResourceManager.calculate_optimal_threads`; the
  returned count is adopted only for dynamic allocation. An explicit count is
  retained, although the recommendation is still calculated for logging.
- `process_directory` in the same file accepts `directory`, `pattern` and
  `config` and delegates matching paths. The default `*.dat` pattern searches
  only the supplied directory; a caller pattern containing `**` recurses.
- [ResourceManager.calculate_optimal_threads](performance_monitor.py) is a
  separate implementation. The optimized parallel helper uses
  `SimpleResourceManager`, not this `ResourceManager` class. Their size
  branches differ; they are not interchangeable performance evidence.

The source establishes configuration behavior, not measured throughput. The
size bands and memory-per-worker assumptions in either manager are implementation
heuristics without independent benchmark qualification here. No engineering
acceptance limit or empirical resource recommendation is introduced by this guide.

## Configuration examples

These examples describe the interface; they are not solver-free execution
commands. Calling processing methods with matching native model files can load
and run the licensed interface. An unavailable interface can instead return
implicit mock output; that is not native completion evidence (tracked separately
in issue #2521). `num_threads` configures executor workers, not the native solver's
internal thread count.

```python
from digitalmodel.solvers.orcaflex.orcaflex_optimized_parallel_v2 import OrcaFlexOptimizedParallelAnalysis

# Allocation will be calculated when a nonempty batch is processed.
analysis = OrcaFlexOptimizedParallelAnalysis(num_threads=None)
```

With `model_directory` and `processing_config` supplied by the caller, the
directory interface will pass matching paths and configuration to processing:

```python
results = analysis.process_directory(
    directory=model_directory, pattern="*.dat", config=processing_config
)
```

An explicit worker count is a required caller decision for reproducible resource
allocation. It should be recorded with the input identities and resource limits;
this guide supplies no optimal value.

```python
def prepare_analysis(requested_workers: int):
    # requested_workers is supplied by the caller, not inferred from this guide.
    return OrcaFlexOptimizedParallelAnalysis(num_threads=requested_workers)
```

There is no `auto_optimize` constructor argument. Dynamic allocation is selected
by omitting `num_threads` or passing `None`.

## Benchmark evidence required

The earlier timing and percentage claims are UNVERIFIED and are withdrawn as
recommendations. Neither a repeated measurement record nor an independently
reviewable input/result manifest accompanies them. They cannot establish an
optimal count, an I/O bottleneck, a speedup or a memory allowance for other files.

A future benchmark should record input digests and sizes, implementation revision,
interface version, explicit worker counts, executor mode, solver thread count,
hardware and storage conditions, repeated elapsed-time measurements, failures and
native output readback. The comparator and acceptance criterion should be declared
before measurement. No benchmark or licensed-interface run accompanies this change.

## Validation limits

The linked evidence page classifies the focused tests as offline source-contract
checks. Those checks isolate selected class methods without importing the module
or its licensed interface. They establish constructor state, early returns and
filesystem delegation only. Worker execution, dynamic resource probing, Windows
process setup and native simulation results remain NOT_EVALUATED.
