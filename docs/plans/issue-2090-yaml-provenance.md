# YAML validator provenance audit

## Summary

Refs #2090 and its linked #716 repair. This bounded audit retains diagnostic
occurrences within a source line and separates YAML composition from construction
failures. Offline findings do not establish native solver acceptance.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-19 | Inventory adjacent readers and plan occurrence and failure-phase regressions. |

## Plan-lite

- Slice 1: retain distinct flow-mapping diagnostic occurrences by line and column.
- Files: `yaml_timestep_validation.py`, `yaml_validator.py`, and a focused test
  module under `tests/solvers/orcaflex/`; update the existing syntax-error
  assertion for the explicit composition phase. This plan records the audit boundary.
- Slice 2: separate composition and construction failures, retaining source findings.
- Slice 3: contain scalar-constructor conversion failures after source inspection.
- Equations and engineering acceptance limits: none; no engineering values change.
- Position convention: PyYAML Mark uses zero-based line and column; diagnostics
  add one to each, following the error-position example in its documentation.
- Expected results: manually enumerate key starts in literal YAML fixtures; retain
  both same-line keys, one source key reached through aliases, and earlier findings
  when a later unknown or malformed scalar tag prevents construction.
- Existing context tests must retain sequence edges, setter order, duplicate-key
  ambiguity, inherited-context warnings, cycle rejection and large flat tables.
- Later slices: source-aware include extraction and extractor overwrite semantics;
  no blanket loader replacement or new graph limit is included.
- Integration: existing legacy-library validation records bind both source
  modules by digest. Re-run their validator with its external resumable cache,
  then refresh `validators.json` and its manifest binding; retain other records.

## Sources

- [YAML 1.2.2 (2021), sections 3.2.1.1, 3.2.2.2 and 3.3](https://yaml.org/spec/1.2.2/):
  collection edges, alias identity and loading failure stages. Duplicate mapping
  keys are not endorsed; findings must survive before dictionary construction
  discards them. The existing PyYAML resolver dialect remains unchanged.
- [PyYAML documentation, Mark, compose and safe_load](https://pyyaml.org/wiki/PyYAMLDocumentation):
  position conventions, representation graphs and object construction.
- Existing repository regressions in `test_yaml_validator_timestep.py` supply the
  ordered-context contract from #716. New fixtures use synthetic strings and
  source-position counting, with no physical coefficients or acceptance limits.

Both public documentation pages were opened successfully before implementation.

## Inspected modules

Paths below are relative to `src/digitalmodel/`.

| Module | Inspection and disposition |
| --- | --- |
| `solvers/orcaflex/yaml_timestep_validation.py` | Preserves mapping and sequence paths, setter order and duplicate-key ambiguity. Emits only line locations; distinct flow keys can share a line. Repair occurrence locations. |
| `solvers/orcaflex/yaml_validator.py` | Deduplicates by property, message and line. Composition and construction share a catch block; scalar conversion exceptions can escape. Reproduce before repair. |
| `solvers/orcaflex/modular_input_validation/level_1_yaml.py` | Uses constructed dictionaries for sections and includes; source occurrences unavailable. Deferred. |
| `solvers/orcaflex/modular_input_validation/utils.py` | `safe_load` precedes recursive include traversal; duplicate directives lose source order and cyclic aliases need separate investigation. Deferred. |
| `solvers/orcaflex/modular_generator/post_validator.py` | Include extraction traverses constructed objects; include-file repeat warnings are distinct from YAML alias traversal. Deferred source-aware include audit. |
| `solvers/orcaflex/modular_generator/extractor.py` | Merges constructed documents using dictionary updates. Duplicate keys and earlier sections lose identity; extraction semantics need separate tests before change. Deferred. |
| `infrastructure/validation/template_validator.py` | Loads YAML before rule evaluation; diagnostics are field strings without node positions. Deferred. |
| `infrastructure/validation/data_validator.py` | YAML supplies validation configuration; checks operate on tabular data, not YAML setter context. No change. |
| `data_systems/data_scraping/validators/data_validator.py` | Same configuration-reader boundary; no source-context validation claim. No change. |
| `infrastructure/templates/template_validator.py` | Re-exports the infrastructure validator; no independent YAML walker. No change. |

This is a bounded inventory of the inspected readers, not a package-wide audit.
No licensed solver was imported, queried or started.

## Plan adversarial review

Local review before implementation identified these constraints:

- Line identity is insufficient for flow mappings. Add column to occurrence
  identity and test both duplicate keys and different nested objects on one line.
- Alias traversal can revisit one key. Deduplicate identical source locations,
  without inventing alias-use locations that PyYAML composition does not retain.
- Construction failure must retain source findings, but composition failure has
  no complete representation graph. Test these outcomes separately.
- Constructor conversion exceptions must be caught only at the construction
  boundary; do not conceal unrelated validator programming errors.
- Append optional diagnostic fields to preserve positional constructor callers.
  Keep the existing printed diagnostic representation compatible.
- Existing validation evidence cannot retain an earlier source digest after code
  changes. Recompute validation records rather than editing their digests alone.

Independent provider review is pending and is required before integration. No
other model or assistant command was called for this bounded implementation.

## Implementation review and evidence

- Before source changes, the focused suite reproduced same-line occurrence loss,
  missing loading-phase distinctions and escaping scalar conversion exceptions.
- The diagnostic column is optional and follows all existing positional fields.
  Printed diagnostic formatting is unchanged. The sole repository consumer of
  source-inspection tuples now reads the appended column.
- Exception handling is confined to the loader call; later validation code is
  outside that catch boundary. Source findings remain available after a failed
  construction, including malformed numeric and timestamp tags.
- Existing sequence-context, setter-order, inherited-context, duplicate-key,
  alias-cycle and large-table regressions pass. No graph limit was changed.
- The existing malformed-syntax assertion now expects a composition error;
  this is the one existing assertion that encoded the replaced message.
- The existing preparation functions rechecked retained input evidence and
  validated 514 inputs across 94 models, using an external resumable cache.
  Every model result matches the prior record. Only the validator source digests,
  validation start time and dependent manifest binding changed.
- Validator tests: 126 passed, including 15 new regressions. Source-bound record
  consumer tests: 46 passed. All ran against the uncommitted implementation.
- Repository structure, engineering register and diff whitespace checks pass.

### Baseline

Ruff on all touched Python files reports E713 at the unchanged extension check
in `yaml_validator.py` (line 177 on `origin/main`) and F401 at the unchanged
`yaml` import in `test_yaml_validator.py` (line 13 on `origin/main`). Both were
confirmed by reading those files on `origin/main`; no new lint finding appeared.

### Hooks that could not run

None. The runner executes the hook framework after handoff; direct file checks
were run here.

### Follow-up

Independent provider review remains pending. Source-aware include extraction,
alias-use locations and extractor overwrite semantics remain outside this repair.
