# Plan for #1571: Fix AQWA GDF triangles, frequency ordering, and v261 discovery

> **Status:** adversarial-reviewed — user approval checkpoint pending
> **Complexity:** T2
> **Date:** 2026-07-13
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/1571
> **Client:** N/A — tests use synthetic meshes only; private Noble data stays outside this repository
> **Lane:** lane:codex
> **Execution mode:** single-lane

---

## Resource Intelligence Summary

### Existing repo code

- `src/digitalmodel/hydrodynamics/diffraction/aqwa_backend.py:281-300` returns `None` for a missing mesh but also catches every parser exception, so malformed existing files are silently downgraded to a warning card.
- `src/digitalmodel/hydrodynamics/diffraction/aqwa_backend.py:343-378` parses the four coordinate records required by GDF and deduplicates equal coordinates into equal node IDs, but silently drops truncated or non-numeric panels without verifying that the declared panel count was read.
- `src/digitalmodel/hydrodynamics/diffraction/aqwa_backend.py:525-540` incorrectly emits every parsed panel as four-node `QPPL DIFF`, including GDF triangles whose fourth coordinate repeats the third.
- `src/digitalmodel/hydrodynamics/diffraction/aqwa_backend.py:773-793` preserves the source order after period-to-frequency conversion; ascending periods therefore become descending frequencies.
- `src/digitalmodel/hydrodynamics/diffraction/aqwa_runner.py:189-201` preserves `AQWA_PATH` precedence but its Windows standard-path list stops at v252.
- `src/digitalmodel/hydrodynamics/diffraction/gmsh_mesh_builder.py:125-143` is a second AQWA DAT exporter with the same invalid repeated-node `QPPL` triangle behavior and stale documentation.
- `src/digitalmodel/hydrodynamics/diffraction/input_schemas.py:369-386` silently filters nonpositive period values during conversion, preventing the backend from detecting that requested source values disappeared.

### Reference evidence

- Issue #1571 records the production symptom and the privacy boundary.
- A private licensed-run listing reported repeated-node quadrilateral-card errors and a separate frequency/direction ordering error. This plan intentionally records only the error classes; no connectivity, client geometry, or native-result excerpt is included.
- The client's original AQWA deck, inspected locally, uses three-node `TPPL DIFF` cards for triangular panels and four-node `QPPL DIFF` cards for quadrilaterals. This establishes the required AQWA card distinction.
- `tests/hydrodynamics/diffraction/test_aqwa_runner.py:462-489` already tests executable discovery and environment precedence, providing the extension point for v261.
- `tests/hydrodynamics/diffraction/test_orcawave_backend.py:444-455` demonstrates the repository convention of explicitly testing solver-required ordering.

### Standards and external documents

- No external engineering standard governs this serialization defect. The authoritative evidence is accepted AQWA deck syntax plus the fail-closed v261 solver diagnostics.
- No drive-file search is needed: private project files supplied only runtime reproduction evidence and are prohibited from the public synthetic regression fixtures.

### Gaps identified

- No backend test distinguishes triangular GDF panels from true quadrilaterals at AQWA card emission.
- No AQWA backend test asserts ascending generated frequency cards for period input.
- No discovery test covers the installed v261 standard path.
- No parser test requires the number of successfully parsed panels to equal the GDF-declared panel count, and `_load_mesh` currently hides malformed-file exceptions.
- No backend validation rejects duplicate, nonfinite, or nonpositive converted frequencies before assigning AQWA indices.
- The platform-gated `_STANDARD_PATHS` shape prevents deterministic cross-platform testing of Windows candidate ordering.

### Reproduction proof

The private licensed v261 run reached AQWA and failed on two input-validation classes: repeated-node quadrilateral cards and non-ascending frequency/direction cards. The same worker's synthetic license canary completed `rc=0`, isolating these failures from licensing. Exact client connectivity and verbatim native-result text are deliberately omitted from this public plan.

Distinct sources: issue #1571; `aqwa_backend.py`; `aqwa_runner.py`; existing backend/runner tests; accepted original AQWA syntax.

---

## Deliverable

AQWA deck generation that emits valid triangle/quad cards, ascending frequencies, and discovers ANSYS v261, with synthetic regression tests and no client data.

## Design and Pseudocode

```text
for each parsed four-slot GDF panel:
    unique_nodes = preserve_order(panel node IDs)
    if panel shape is exactly [n1, n2, n3, n3] with n1/n2/n3 distinct:
        emit TPPL DIFF with the first three node IDs
    elif exactly 4 unique nodes:
        emit QPPL DIFF with four node IDs
    else:
        fail closed with a descriptive invalid-panel error

while parsing GDF:
    require a valid positive declared panel count
    require exactly four valid coordinate records per declared panel
    require parsed panel count == declared panel count
    require each panel is a canonical triangle or four-unique-node quad
    raise a descriptive mesh-format error containing mesh path,
        1-based panel number, and vertex slot on any violation

when loading mesh:
    return None only when the resolved path does not exist
    propagate malformed existing-file errors so deck generation fails closed

frequencies_hz = convert_to_hz(spec frequencies)
conversion must preserve source cardinality; FrequencySpec raises on
    nonfinite or nonpositive period/frequency inputs instead of filtering
reject nonfinite, nonpositive, or duplicate converted values
frequencies_hz = sorted(frequencies_hz)
emit indexed HRTZ cards in ascending numeric order

unconditional WINDOWS_AQWA_CANDIDATES = [v261, v252, existing descending versions]
active standard paths = candidates only on Windows
AQWA_PATH remains checked before standard paths

gmsh DAT helper:
    emit TPPL DIFF for three-node input
    emit QPPL DIFF for four-unique-node input
    reject all other cardinality/duplicate patterns
```

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Modify | `tests/hydrodynamics/diffraction/test_aqwa_backend.py` | Add triangle/quad, malformed-panel, and ascending-frequency regressions before code changes |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/aqwa_backend.py` | Emit TPPL/QPPL correctly and sort frequency cards |
| Modify | `tests/hydrodynamics/diffraction/test_aqwa_runner.py` | Add v261 discovery and retain environment-precedence coverage |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/aqwa_runner.py` | Add v261 to standard discovery paths |
| Modify | `tests/hydrodynamics/diffraction/test_gmsh_mesh_builder.py` | Replace the stale repeated-node-QPPL expectation with TPPL/quad/invalid-shape tests |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/gmsh_mesh_builder.py` | Make the second AQWA DAT exporter use valid TPPL syntax and correct its documentation |
| Modify | `tests/hydrodynamics/diffraction/test_input_schemas.py` | Prove invalid period/frequency values fail instead of disappearing during conversion |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/input_schemas.py` | Preserve frequency-source cardinality by rejecting invalid values before conversion |

## TDD Test List

| Test | Verification |
|---|---|
| `test_deck2_emits_tppl_for_repeated_fourth_gdf_vertex` | A synthetic triangular GDF panel produces one three-node `TPPL DIFF` card |
| `test_deck2_preserves_qppl_for_four_unique_vertices` | A true quad remains a four-node `QPPL DIFF` card |
| `test_parse_gdf_rejects_truncated_declared_panel` | Missing coordinate records fail closed instead of silently dropping a panel |
| `test_parse_gdf_rejects_non_numeric_panel_coordinate` | An existing malformed file raises a descriptive format error |
| `test_parse_gdf_rejects_invalid_declared_panel_count` (parameterized) | Non-integer, zero, and negative NPAN values fail closed |
| `test_load_mesh_propagates_malformed_existing_file` | Parser errors are not converted into warning-only missing meshes |
| `test_load_mesh_missing_path_retains_missing_behavior` | A genuinely absent path still returns `None` for the existing caller contract |
| `test_deck2_rejects_noncanonical_three_unique_duplicates` (parameterized) | Fourth-repeats-first, adjacent duplicates, and fewer than three unique nodes fail closed |
| `test_deck2_tppl_and_qppl_have_exact_node_field_counts` | TPPL has three node fields, QPPL has four, and no repeated-node QPPL is emitted |
| `test_deck6_sorts_period_derived_frequencies_with_exact_values_and_indices` | Exact Hz conversions are ascending and HRTZ indices are sequential |
| `test_deck6_sorts_unsorted_explicit_frequencies_and_reindexes` | Explicit unsorted inputs receive deterministic solver order and indices |
| `test_deck6_rejects_invalid_or_duplicate_converted_frequencies` (parameterized) | Duplicate, nonfinite, zero, and negative solver frequencies fail closed |
| `test_frequency_conversion_rejects_nonpositive_or_nonfinite_source_values` (parameterized) | Period/frequency conversion never silently drops requested values |
| `test_windows_candidates_put_v261_before_v252` | Candidate ordering is deterministic on every test platform |
| `test_detect_executable_checks_v261_standard_path` | Selective existence mocking discovers v261 with environment state explicitly cleared |
| `test_explicit_executable_path_wins_over_env_and_standard_candidates` | Caller configuration has highest precedence with all alternatives available |
| `test_aqwa_path_env_wins_over_standard_candidates` | Agent environment wins when v261 and older installs also exist |
| `test_v261_wins_over_older_existing_standard_candidates` | Standard discovery selects the newest installed supported version |
| `test_dat_panel_triangle_uses_tppl_and_quad_uses_qppl` | The gmsh DAT helper follows the same valid card contract |
| `test_dat_panel_rejects_invalid_duplicate_shapes` | The gmsh DAT helper fails closed on noncanonical connectivity |

## Acceptance Criteria

- [ ] New focused tests fail before implementation and pass afterward.
- [ ] `uv run pytest tests/hydrodynamics/diffraction/test_aqwa_backend.py tests/hydrodynamics/diffraction/test_aqwa_runner.py tests/hydrodynamics/diffraction/test_gmsh_mesh_builder.py tests/hydrodynamics/diffraction/test_input_schemas.py -v` passes.
- [ ] Existing AQWA/diffraction tests show no regression.
- [ ] A dry-generated Noble deck contains `TPPL` for all repeated-vertex triangles, no repeated-node `QPPL`, and ascending HRTZ and DIRN values (private verification only; no connectivity or native output is copied into the public repository).
- [ ] The private licensed-run retry gets past Deck 2 and Deck 6 input validation.
- [ ] No private input or native result is committed to `digitalmodel`.

## Adversarial Review Summary

First review verdict: **MAJOR**. The reviewer found private excerpts, silent parser/load failures, insufficient negative triangle/frequency tests, platform-dependent discovery testing, and a second invalid DAT exporter. This revision removes all private excerpts; adds explicit parser cardinality and contextual errors; preserves only missing-file behavior; defines canonical triangle and frequency validity contracts; exposes testable Windows candidates; and includes `gmsh_mesh_builder.py`.

Independent privacy/scope review verdict: **MINOR**. Privacy and scope are clean. The plan now adds the requested invalid-NPAN test, direct three-level executable-precedence tests, and private ascending-DIRN acceptance. Final test-design review remains pending before promotion.

Independent test-design review verdict: **APPROVE**, with one implementation caution folded into the plan: `FrequencySpec` currently filters invalid periods, so source validation/cardinality preservation is now explicit and tested before backend ordering.

Original review second-pass verdict: **APPROVE**. All prior MAJOR findings are resolved; no new blocking or minor defect was found.

## Risks and Open Questions

- GDF permits triangles by repeated coordinates; detection must be based on parsed node identity without reordering valid quadrilaterals. Only the conventional repeated third/fourth slot is accepted as a triangle; other duplicate patterns fail closed.
- Sorting frequencies changes output indexing but is required by AQWA; downstream extraction must use the emitted solver order.
- Frequency sorting changes solver indices; extraction and result ordering must follow the emitted sequence, so exact-value/index tests are required.

## Complexity: T2

Two small production fixes plus executable discovery, but correctness depends on solver-specific serialization and regression coverage.
