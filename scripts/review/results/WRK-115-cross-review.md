# WRK-115 Cross-Review: RAO-Hull Linking in Hull Library Catalog

**Commit**: `77690c76a` (digitalmodel)
**Date**: 2026-02-13
**Files reviewed**:
- `src/digitalmodel/hydrodynamics/hull_library/panel_catalog.py` (214 lines)
- `src/digitalmodel/hydrodynamics/hull_library/rao_registry.py` (187 lines)
- `src/digitalmodel/hydrodynamics/hull_library/__init__.py` (47 lines)
- `tests/hydrodynamics/hull_library/test_rao_registry.py` (353 lines)
- `data/hull_library/raos/registry.yaml` (3 lines)
- `pyproject.toml` (1-line fix)
**Total**: 6 files changed, 586 insertions(+), 4 deletions(-)

---

## Claude Review
**Verdict**: MINOR

### 1. Schema Correctness (PASS)

`RaoReference` covers the essential RAO metadata fields:
- `solver` (str, required) -- diffraction solver identity (orcawave, aqwa, bemrosetta)
- `draft_m` (float, required) -- analysis draft in metres
- `loading_condition` (Optional[str]) -- ballast, full_load, etc.
- `headings_deg` (Optional[list[float]]) -- wave headings analysed
- `n_frequencies` (Optional[int]) -- frequency bin count
- `date` (Optional[str]) -- ISO 8601 date string
- `file_path` (str, default="") -- relative path to RAO data JSON
- `benchmark_revision` (Optional[str]) -- revision tracking (r1, r4, etc.)

Good design decisions:
- Pydantic `BaseModel` with `Field` descriptions provides self-documenting schema
- `to_dict()` uses `exclude_none=True` for clean YAML serialization
- `file_path` defaults to empty string rather than None, avoiding None-path concatenation bugs
- `raos` on `PanelCatalogEntry` is `Optional[list[RaoReference]]` -- backward compatible

**P3 finding**: Consider adding a `frequency_range_rad_s` tuple field (min, max) to allow quick filtering without loading the full JSON. Currently requires `load_rao_data()` to inspect frequency coverage.

### 2. Registry Operations (PASS)

- **Idempotent register**: Lines 116-126 correctly filter existing entries by solver + draft tolerance before appending. The draft tolerance of 0.5m is appropriate for most hull analysis contexts.
- **Draft-tolerance lookup**: `get_raos()` (lines 148-158) properly applies `_DRAFT_TOLERANCE_M` for draft filtering and supports optional solver filtering.
- **YAML persistence**: `_save_index()` writes structured YAML with `version`, `updated` (UTC timestamp), and `hulls` dict. `_load_index()` handles empty/missing files gracefully with `or {}` fallback.
- **Catalog linking**: `link_to_catalog()` (lines 173-184) iterates catalog entries, matches by `hull_id`, and injects `RaoReference` objects. Returns update count for reporting.

**P2 finding**: Idempotency uses draft tolerance (0.5m) for dedup, meaning registering drafts 10.0m and 10.4m for the same solver will overwrite. The docstring says "same hull+solver+draft overwrites" but the tolerance makes this imprecise. Consider either exact match for idempotency or documenting the tolerance-based behavior explicitly.

**P2 finding**: `file_path.relative_to(self._raos_dir.parent.parent)` on line 110 assumes the raos directory is exactly 2 levels below the data root (e.g., `data/hull_library/raos/`). This is fragile -- if `raos_dir` is constructed differently, `relative_to()` will raise `ValueError`. Consider accepting a `base_dir` parameter or computing the relative path from the registry's root.

**P3 finding**: `load_rao_data()` on line 166 reconstructs the absolute path as `self._raos_dir.parent.parent / ref.file_path`. This mirrors the 2-level assumption and should be documented or made configurable.

### 3. Serialization (PASS)

- **YAML round-trip**: `_entry_to_yaml_dict()` correctly serializes `raos` via `r.to_dict()`. `from_yaml()` correctly deserializes `raos` with list comprehension and `RaoReference(**r)` constructor. Tested in `TestPanelCatalogRaoRoundTrip.test_yaml_round_trip_with_raos`.
- **CSV export**: `to_csv()` correctly adds `rao_count` column (line 174), computed as `len(entry.raos) if entry.raos else 0`.
- **Backward compatibility**: Entries without RAOs serialize/deserialize correctly (`test_yaml_round_trip_without_raos`).
- **Registry YAML**: `_save_index()` persists hull entries as nested dicts under `hulls` key with UTC timestamp.

### 4. Test Quality (PASS with notes)

18 tests across 4 classes:
- `TestRaoReference` (3 tests): creation, serialization, optional fields
- `TestRaoRegistry` (11 tests): file creation, YAML persistence, lookup by hull/solver/draft, draft tolerance edge cases, idempotency, list_hulls, load_rao_data, empty lookup, persistence across instances, benchmark_revision
- `TestCatalogLinking` (1 test): link_to_catalog with matched/unmatched entries
- `TestPanelCatalogRaoRoundTrip` (3 tests): YAML round-trip with/without RAOs, CSV export

Good practices:
- `tmp_path` fixtures ensure test isolation
- `sample_rao_data` fixture provides consistent test data
- Draft tolerance tested at both boundaries (0.3m within, 0.6m outside)
- Persistence test creates a second `RaoRegistry` instance to verify disk round-trip

**P2 finding**: CSV test (`test_csv_export_with_raos`, line 334) only checks that `csv_test` appears in content, but does not verify the `rao_count` column value is `1`. Should parse CSV and assert `rao_count == 1`.

**P3 finding**: No negative test for `load_rao_data` with a missing file (the code handles it with `logger.warning` and returns `None`, but this path is untested).

**P3 finding**: No test for idempotency within tolerance range (e.g., register at 10.0m, then at 10.3m for same solver -- does it correctly replace?).

### 5. Security and Compliance (PASS)

- **No client references**: Scanned all 6 files against digitalmodel `.legal-deny-list.yaml` patterns. Zero matches.
- **No secrets**: No API keys, tokens, credentials, or hardcoded sensitive values.
- **No denied terms**: Hull IDs in tests are generic (`barge_100x20x10`, `test_hull`, `hull_a`, etc.).
- **Path safety**: Uses `pathlib` throughout (no string concatenation for paths). However, `hull_id` and `solver` are used directly in directory/filename construction without sanitization.

**P2 finding (security)**: `hull_id` and `solver` are used directly in path construction (lines 81, 78). While this is an internal-only API (not exposed to untrusted input), a Pydantic validator on `RaoReference.solver` and a hull_id sanitizer in `register_rao()` would be defensive best practice. At minimum, reject values containing `/`, `..`, or null bytes.

### pyproject.toml Fix (PASS)

The rename from `[tool.pytest.benchmark]` to `[tool.pytest-benchmark]` is correct. The `pytest-benchmark` plugin reads from `[tool.pytest-benchmark]` per its documentation. The old key was being parsed as a sub-table of `[tool.pytest]` which conflicts with `[tool.pytest.ini_options]`.

### Summary of Findings

| Priority | Finding | File | Line |
|----------|---------|------|------|
| P2 | Idempotency uses tolerance, can overwrite distinct drafts | rao_registry.py | 122 |
| P2 | Hardcoded 2-level parent assumption for relative_to() | rao_registry.py | 110 |
| P2 | CSV test does not verify rao_count value | test_rao_registry.py | 334 |
| P2 | No input sanitization on hull_id/solver for path construction | rao_registry.py | 78-83 |
| P3 | Consider adding frequency_range_rad_s to schema | panel_catalog.py | 34 |
| P3 | Missing negative test for load_rao_data with absent file | test_rao_registry.py | - |
| P3 | Missing idempotency test within tolerance range | test_rao_registry.py | - |

---

## Codex Review
**Verdict**: MINOR

Codex CLI (gpt-5.3-codex) performed a thorough static analysis (could not execute tests due to sandbox temp dir restrictions). Key findings:

1. **High: Path traversal / arbitrary file write** -- `hull_id` and `solver` used directly in filesystem paths without sanitization (lines 78, 81, 83, 98). Inputs like `hull_id="../../.."` could escape the RAO directory.

2. **High: Path traversal / arbitrary file read** -- `ref.file_path` trusted and joined directly in `load_rao_data` (line 166). Absolute paths or `..` segments could bypass the base directory.

3. **Medium: Hardcoded 2-level ancestor** -- `file_path.relative_to(self._raos_dir.parent.parent)` assumes fixed directory structure (line 109). Non-standard `raos_dir` will throw `ValueError`.

4. **Medium: Idempotency inconsistency** -- Dedup removes entries within `< 0.5m` tolerance, but docstring says "same hull+solver+draft overwrites". Can silently overwrite distinct drafts (10.0m vs 10.4m).

5. **Medium: Missing RAO shape validation** -- No checks that `headings_deg` length matches array direction axis, or that `frequencies`, `amplitudes`, `phases` have consistent dimensions.

6. **Low: Test coverage gaps** -- No path traversal tests, no bad-shape input tests, CSV test does not verify `rao_count`, idempotency test only checks count not record content.

Open questions raised:
- Is `RaoRegistry` intended for untrusted callers or internal-only?
- Should idempotency be exact or tolerance-based?
- Is `file_path` workspace-relative or RAO-dir-relative?

---

## Gemini Review
**Verdict**: APPROVE

Gemini CLI reviewed the full commit diff and provided the following assessment:

1. **Schema Correctness**: `RaoReference` schema is well-defined with appropriate Pydantic types and descriptions. Integration into `PanelCatalogEntry` via optional list is logical and supports multi-RAO linking.

2. **Registry Operations**: Comprehensive functionality with sound design -- YAML index for metadata, JSON files for detailed data. Relative path handling is consistently applied during registration and loading.

3. **Serialization Round-Trip**: `PanelCatalog` correctly updated for YAML serialization/deserialization of `RaoReference` objects. CSV `rao_count` column demonstrates careful export consideration.

4. **Test Quality**: Thorough and well-structured test suite covering reference instantiation, registry core functions, catalog linking, and YAML/CSV round-trip. Good use of `tmp_path` fixtures.

5. **Security**: No obvious vulnerabilities. Path manipulation uses `pathlib` for safer operations.

No issues raised.

---

## Summary

| Reviewer | Verdict | Findings |
|----------|---------|----------|
| Claude (Opus 4.6) | MINOR | 4x P2 (idempotency tolerance, hardcoded parent, CSV test gap, path sanitization), 3x P3 (schema enhancement, missing negative tests) |
| Codex (gpt-5.3-codex) | MINOR | 2x High (path traversal write/read), 2x Medium (hardcoded ancestor, idempotency), 1x Medium (shape validation), 1x Low (test gaps) |
| Gemini CLI | APPROVE | No issues raised. Schema, operations, serialization, tests, and security all satisfactory. |

**Overall assessment**: APPROVE with MINOR findings. The implementation is well-structured, correctly implements the RAO-hull linking feature, and has solid test coverage (18 tests). The core architecture -- YAML registry index + per-hull JSON data files + Pydantic schema -- is sound. The main recurring concern across Claude and Codex is path sanitization on `hull_id`/`solver` inputs (defensive hardening, not a live vulnerability since callers are internal). The idempotency tolerance semantics and hardcoded 2-level parent assumption are worth documenting or addressing in a follow-up. None of the findings are blocking.

**Reviewers attempted**: 3 -- minimum met.
