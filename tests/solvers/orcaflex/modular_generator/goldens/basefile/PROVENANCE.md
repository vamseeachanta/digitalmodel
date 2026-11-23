# BaseFile goldens — provenance

## Producer

| Field | Value |
|---|---|
| Module | `digitalmodel.solvers.orcaflex.template_generator` |
| Entry point | `TemplateGenerator.generate(base_file, variation_file, output_file, as_reference=True)` |
| Function under capture | `TemplateGenerator._generate_reference` |
| Source location | `src/digitalmodel/solvers/orcaflex/template_generator.py:380-418` |
| Repository commit at capture | `b2b462d53c0829414bb29ae0ba4cb1ba93f93b20` |
| Branch at capture | `feat/orcaflex-3843-basefile-merge` |
| Date of capture | 2026-09-11 |
| Capture script | `capture.py`, beside this record |
| Interpreter | `.venv/Scripts/python.exe` (Windows, CPython) |

## Comparator class

**`cross-solver`** — per `.claude/rules/reproducibility-is-not-correctness.md`.

`_generate_reference` is a second in-tree implementation of the same
vendor-documented OrcaFlex variation-model format. It is not the consumer of that
format. Agreement between `modular_generator.writers.basefile` and
`template_generator._generate_reference` establishes that two independent
implementations of the relative-path reference agree with each other; it does not
establish that either agrees with OrcaFlex.

The class is **not** `archived-run`: the goldens are not a prior run of the code
under test, and they do not drift with it. They do drift with
`template_generator.py`, and re-capture is required if that module changes.

A licensed equivalence check — loading a written variation model through
OrcFxAPI and comparing the resolved object set against the flat `IncludeFile`
composition — remains a follow-on. It is not performed here and is not claimed.

## What the goldens cover, and what they do not

| Emitted key | Comparator class | Note |
|---|---|---|
| `BaseFile` | `cross-solver` | The relative-path arithmetic and the POSIX separator normalisation are asserted against `_generate_reference` |
| Override sections (`LineTypes`, `Vessels`, `Lines`, …) | `none` | `_generate_reference` does not emit them; see the divergence below |

The override half of the writer's output has **no comparator**. Its section
ordering is asserted against `BuilderRegistry`, and its anchor-free encoding is
asserted structurally. Both are inside the producing system, and neither is
evidence of agreement with OrcaFlex.

The ordering authority was corrected after the first capture. The plan named
`post_validator._OBJECT_SECTIONS`, which lists `6DBuoys`, `3DBuoys`,
`Constraints`, `Links` and `Winches` **after** `Lines`; ordering by it emits a
line that references a buoy the document has not yet declared. That module
documents the tuple as the SET of sections defining named objects, not as an
order, and it is now used for membership only. The order is read off
`BuilderRegistry`, where `BuoysBuilder` is registered at 80 against
`LinesBuilder` at 90 — the same order the committed
`docs/domains/orcaflex/templates/mooring_systems/calm_buoy/master.yml` loads its
include files in, under the comment "Buoys must be before Lines since lines
connect to buoys". Sections no builder claims are placed by
`generic_builder._SECTION_ORDER`, which is documented as derived from a
monolithic model's own `SaveData()` output order. None of this changes the
goldens: `_generate_reference` emits no override sections, so the capture is
unaffected.

## Divergence from the plan's description of the comparator

The plan
(`workspace-hub/docs/plans/2026-09-11-issue-3843-generator-consolidation.md`,
"The comparator problem") describes `_generate_reference` as an independent
implementation of the format the new writer emits. The two formats overlap only
partially.

`_generate_reference` returns exactly two keys:

```
{'BaseFile': <variation base, relative to the output file>,
 'IncludeFile': <variation file, relative to the output file>}
```

It composes a variation model by **referencing a separate variation file**. The
writer specified by the plan composes one by **inlining the override sections**
into the same document, and emits no `IncludeFile` key at all. Both are valid
OrcaFlex variation models and the two are not interchangeable: the inline form
carries its overrides in one file, the reference form defers them to a second.

The consequence is recorded above and is load-bearing: the goldens constrain the
`BaseFile` key only. The plan's TDD row 13, "writer output matches the committed
golden", is implemented as an assertion on that key. A claim that the writer's
full output is golden-backed would be false.

## Relative-path behaviour reproduced

`_generate_reference` resolves the base reference in two branches, and both are
represented in the case set:

1. `Path.relative_to(output_file.parent)` — lexical, no resolution, succeeds only
   when the base sits at or below the output directory. Case
   `base_below_output`.
2. `os.path.relpath(base.resolve(), output_file.parent.resolve())` — the
   fallback, which produces `..` traversal. Cases `calm_buoy_deep_water`,
   `spread_mooring_twelve_leg`, `salm_wire_rope`, `turret_external`,
   `dat_base_sibling`.

Both branches normalise `\` to `/` in the emitted string.

## Case set

Inputs are recorded exactly in `manifest.yml`. Four cases are driven from the
committed hybrid template sets under `docs/domains/orcaflex/templates/`; two are
driven from synthetic fixtures under `fixtures/` that exist to exercise the
`.dat` base extension and the `relative_to` branch.

| Case | Base file | Captured `BaseFile` |
|---|---|---|
| `calm_buoy_deep_water` | `calm_buoy_hybrid/base/calm_buoy_base.yml` | `../base/calm_buoy_base.yml` |
| `spread_mooring_twelve_leg` | `spread_mooring_hybrid/base/spread_mooring_base.yml` | `../base/spread_mooring_base.yml` |
| `salm_wire_rope` | `salm_hybrid/base/salm_base.yml` | `../base/salm_base.yml` |
| `turret_external` | `turret_mooring_hybrid/base/turret_mooring_base.yml` | `../base/turret_mooring_base.yml` |
| `dat_base_sibling` | `fixtures/base_model.dat` | `../base_model.dat` |
| `base_below_output` | `fixtures/sub/base_model.yml` | `sub/base_model.yml` |

Table 1 — captured cases and the `BaseFile` value each produced.

The capture wrote to `<template>/cases/case_*_golden.yml` inside the committed
template sets and removed those files afterwards, so that the committed case
files were not overwritten. The output directory, not the output filename,
determines the relative path, so the recorded values are the values those
template sets already carry: `calm_buoy_hybrid/cases/case_deep_water.yml` holds
`BaseFile: ../base/calm_buoy_base.yml`, which case `calm_buoy_deep_water`
reproduces independently.

## Re-capture

```
.venv/Scripts/python.exe tests/solvers/orcaflex/modular_generator/goldens/basefile/capture.py
```

Re-capture is required when `template_generator._generate_reference` changes.
A golden that no longer matches a changed comparator is a signal to establish
which of the two implementations is wrong, not to refresh the golden.
