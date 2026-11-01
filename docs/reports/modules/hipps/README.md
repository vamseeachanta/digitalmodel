# HIPPS module analysis

Open [the self-contained report](index.html) for synthetic reliability integrals,
valve closure kinematics, the public API inventory and NOT_EVALUATED limits.
The calculation does not establish achieved system integrity or design acceptance.

With source imports enabled, rebuild from the repository root:

```sh
PYTHONPATH=src python scripts/reports/build_hipps_module_report.py \
  --scratch ../hipps-calculation --output-dir docs/reports/modules/hipps
```

Scratch must be outside the checkout. Both paths are required. The calculation
uses the standard library and normal package imports; tests additionally need
pytest. No solver is required. All intermediate evidence stays in scratch.

[Results](results.json) retain unrounded API values, input units and basis,
contracts and sampled conductance values. [Change history](changes.json) matches
the report. The [manifest](manifest.json) records SHA-256 over exact artifact
bytes, generator bytes and source bytes; it excludes itself. The reviewed
package revision is recorded separately from the generator/test digests.

Run `tests/reporting/test_hipps_module_report.py` with source imports and an
external pytest base directory. Hand-calculation targets and mutation tests
check arithmetic independently; replay and byte rebuilds check freshness only.
Standards, architecture, failure-mode and transient qualification remain open.
