# OrcaFlex Reporting Fixture Artifacts

This directory stores stable, reviewable artifacts used by fixture-backed OrcaFlex reporting tests.

## Purpose

These files support the proof-standard workflow defined by:
- workspace-hub issue `#1652` — OrcaFlex forward native-fidelity proof
- workspace-hub issue `#1788` — snapshot-testing follow-on
- roadmap anchor `#1572`

The goal is to make OrcaFlex reporting validation reproducible on both:
- licensed environments with OrcFxAPI available
- non-licensed environments running bounded regression checks from committed artifacts

## Authoritative fixture

Primary fixture inputs live outside this directory:
- `digitalmodel/tests/fixtures/minimal_test.sim`
- `digitalmodel/tests/fixtures/minimal_test.dat`

These artifacts are the reporting-facing derivatives of that fixture.

## Files expected here

### 1. `minimal_test.metadata.json`

Stable machine-readable metadata extracted from the `.sim` fixture on a licensed machine.

This file should contain only fields that are:
- deterministic
- reviewable in git
- useful for regression tests and report generation

Typical contents:
- fixture identity
- OrcaFlex version and extraction provenance
- model state
- object counts by type
- key object names
- stable environment/general summary
- report-facing summary fields

### 2. `minimal_test.report.snapshot.html`

Normalized HTML snapshot baseline for regression tests.

Important:
- this file is **not** intended to be a byte-for-byte dump of every raw generated report
- it should represent the normalized snapshot surface used by tests
- volatile content must be excluded or normalized before comparison

Examples of volatile content to normalize or exclude:
- timestamps
- machine-local paths
- unstable IDs
- incidental formatting churn

## Regeneration workflow

### Metadata baseline regeneration

Run on a machine with OrcFxAPI available, using the extraction script planned for:
- `digitalmodel/scripts/orcaflex/extract_fixture_metadata.py`

Expected workflow:
1. Load `minimal_test.sim`
2. Extract stable metadata fields only
3. Write/update `minimal_test.metadata.json`
4. Review git diff before commit

### Snapshot baseline regeneration

Run after metadata-driven report generation is stable.

Expected workflow:
1. Generate report from committed metadata baseline
2. Normalize report HTML using snapshot helper logic
3. Write/update `minimal_test.report.snapshot.html`
4. Review git diff before commit

## Contract rules

### Allowed to change
- fields intentionally added to the stable proof surface
- normalization rules when explicitly documented
- snapshot content when reporting output changes intentionally and review confirms the change

### Not allowed without review
- adding unstable/internal/volatile fields to metadata baseline
- changing snapshot scope from normalized structural content to brittle full-output comparison
- silently replacing fixture provenance or model identity

## Review checklist

When these files change, review for:
- stable fixture identity preserved
- no timestamps or machine-specific paths added
- no unnecessary large raw dumps or arrays
- snapshot remains readable and purpose-specific
- provenance remains explicit

## Notes

If a second fixture is introduced later (for example `mooring_with_raos.sim`), create separate files rather than overloading the `minimal_test` baseline:
- `mooring_with_raos.metadata.json`
- `mooring_with_raos.report.snapshot.html`

Keep `minimal_test` as the smallest proof-standard path.
