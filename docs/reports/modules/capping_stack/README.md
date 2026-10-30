# Capping-stack scalar module analysis

The [module report](index.html) inventories the public API and presents synthetic
pressure boundaries and a buoyancy force ratio. Physical and operational
qualification remain `NOT_EVALUATED`. The existing public-source worked document
and its history are preserved.

Rebuild with normal source imports and caller-selected external scratch:

```sh
PYTHONPATH=src uv run --no-project --with pytest python scripts/reports/build_capping_stack_module_report.py --scratch "$REPORT_SCRATCH" --output-dir docs/reports/modules/capping_stack
```

Set `REPORT_SCRATCH` to a writable directory outside the checkout. Both command
arguments are required. All intermediate files belong there; only the five
small delivery files are written to the output directory. The builder requires
the standard library and the existing public package, without a solver.

The [results](results.json) retain unrounded inputs and public API outputs.
The [manifest](manifest.json) binds all other deliverable bytes and the exact
source, builder, styles and regression test; it excludes itself.
The [change history](changes.json) matches the report revision table.

Run `tests/reporting/test_capping_stack_module_report.py` with source imports
and an external pytest base. Its independent hand calculation is distinct from
API replay and byte-rebuild checks. Coherently regenerated wrong values fail
the numerical oracle, even when all regenerated manifest digests match.
