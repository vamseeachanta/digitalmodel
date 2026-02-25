# WRK-116 Cross-Review: Hull Panel Mesh Scaler

**Commit**: `14e1503a8` (digitalmodel)
**Date**: 2026-02-13
**Files reviewed**:
- `src/digitalmodel/hydrodynamics/hull_library/mesh_scaler.py` (312 lines)
- `tests/hydrodynamics/hull_library/test_mesh_scaler.py` (502 lines)
- `src/digitalmodel/hydrodynamics/hull_library/__init__.py` (+17 lines)

**Total**: 3 files, 831 insertions, 34 tests across 6 test classes

---

## Claude Review (Claude Opus 4.6)
**Verdict**: APPROVE (with 2 minor observations)

### 1. Scaling Correctness
- **Uniform scaling** (`scale_mesh_uniform`): Correctly multiplies all vertex coordinates and the reference point by a single factor. Broadcasting via `mesh.vertices * factor` is clean and numerically sound.
- **Parametric scaling** (`scale_mesh_parametric`): Constructs a `[sx, sy, sz]` array and applies element-wise multiplication. Each axis is independently validated. Reference point is correctly scaled per-axis (lines 112-116).
- **Target-dimension scaling** (`scale_mesh_to_target`): Ratio computation `target / source` on lines 152-154 is correct. Delegates to `scale_mesh_parametric` for the actual transform, which is good separation of concerns.
- **No issues found.**

### 2. Validation (validate_scaled_mesh)
- **Bounding box check**: 1% relative tolerance (`tol = 0.01`) on lines 207-222 is appropriate for engineering meshes. The symmetry plane handling on line 202 correctly doubles `max_y` when Y-symmetry is present.
- **Degenerate panels**: Uses `mesh.panel_areas` against a `1e-4 m^2` threshold (line 225-228). Sensible minimum for offshore-scale panels.
- **Aspect ratios**: `_compute_aspect_ratios` (lines 256-281) iterates panels, computes edge lengths, and returns `max/min` ratio. Correctly handles degenerate zero-length edges by filtering with `> 1e-12` tolerance and assigning `inf` when fewer than 2 edges survive. Threshold of 10.0 for "high aspect" is standard.
- **Normal consistency**: Dot product of normals with radial vectors from mesh centroid (lines 236-239). The `-1e-6` tolerance accommodates floating-point noise. Note: this heuristic assumes a convex-ish hull; for concave hull forms with re-entrant waterline sections, some panels could legitimately have inward-pointing radials even with correct outward normals. This is acceptable for the current hull library scope (barges, semi-subs, ships) but worth documenting as a known limitation.
- **P2 observation**: The `if mesh.symmetry_plane and "y" in (mesh.symmetry_plane or "").lower()` on line 202 has a redundant `or ""` since the truthiness check already excludes `None`. Cosmetic only, no functional impact.

### 3. Topology Preservation
- Both `scale_mesh_uniform` and `scale_mesh_parametric` call `mesh.panels.copy()` (lines 76, 120), ensuring the panel connectivity array is preserved but decoupled from the source mesh. Vertex count and panel count are unchanged. The PanelMesh constructor presumably recomputes normals, areas, and centers from the new vertices, so derived quantities update correctly.
- Tests verify: `scaled.n_panels == original.n_panels`, `scaled.n_vertices == original.n_vertices`, and `np.testing.assert_array_equal(scaled.panels, original.panels)`.
- **No issues found.**

### 4. GDF Export Round-Trip
- `export_scaled_gdf` (lines 284-301) uses a lazy import of `GDFHandler` to avoid circular imports, which is a good pattern.
- Tests verify: file creation, file readability via `GDFHandler.read()`, and geometric preservation by comparing sorted panel centers with `atol=1e-4`. The atol accounts for GDF's limited floating-point precision in the file format.
- The comment in the test (line 471-472) correctly explains why panel centers are compared rather than raw vertices (GDF vertex deduplication may reorder).
- **No issues found.**

### 5. Edge Cases
- **Identity scale**: `test_uniform_identity_scale` verifies factor=1.0 produces identical vertices (`atol=1e-12`).
- **Fractional scale**: `test_uniform_fractional_scale` verifies factor=0.5 halves dimensions.
- **Zero rejection**: Both `scale_mesh_uniform(mesh, 0.0)` and `scale_mesh_parametric(mesh, 0.0, 1.0, 1.0)` raise `ValueError`. `ScaleDimensions` rejects zero and negative values in `__post_init__`.
- **Negative rejection**: Tested for uniform and parametric.
- **Scale round-trip**: `test_scale_round_trip` scales up 2x then down 0.5x and asserts `atol=1e-10` on vertex recovery.
- **P3 observation**: No test for very large scale factors (e.g., 1000x) or very small (e.g., 0.001x) which could expose floating-point precision issues. Low priority since numpy float64 handles these ranges well.

### 6. Security / Legal Compliance
- No hardcoded secrets, API keys, or credentials.
- No client references, project codenames, or denied terms.
- No file system access outside of the explicit `output_path` parameter in `export_scaled_gdf`.
- No network calls, no subprocess invocations.
- Logging uses standard `logger.info` with no sensitive data exposure.
- **Clean.**

### Summary of Findings
| # | Severity | Finding |
|---|----------|---------|
| 1 | P2 | Line 202: redundant `or ""` in symmetry check (cosmetic) |
| 2 | P3 | Normal consistency heuristic assumes convex-ish hull; document limitation |
| 3 | P3 | No extreme scale factor tests (1000x, 0.001x) — low risk with float64 |

---

## Codex Review
**Verdict**: NO_OUTPUT

Codex CLI exited with code 2. The `review --commit` subcommand failed, consistent with previous behavior on large diffs (831 lines). No review output produced.

---

## Gemini Review (Gemini CLI)
**Verdict**: APPROVE

### Findings
Gemini performed a thorough review covering all six criteria:

- **Scaling correctness**: Confirmed all three scaling methods (uniform, parametric, target-dimension) are mathematically sound with correct dimension handling.
- **Validation**: Comprehensive coverage of bounding box checks, degenerate panel detection, aspect ratio computation, and normal consistency verification.
- **Topology preservation**: Confirmed panel connectivity is preserved through `.copy()` on panel arrays.
- **GDF round-trip**: Export and re-import verified with geometry preservation through panel center comparison.
- **Edge cases**: Noted explicit tests for identity, fractional, zero, and negative scale factors. `ScaleDimensions` validation covers invalid inputs.
- **Security**: No issues identified.
- **Test quality**: Praised the fixture design (`unit_box_mesh`, `barge_mesh`), the Arrange-Act-Assert structure, and the use of `np.testing.assert_allclose` with appropriate tolerances.

No defects or concerns raised.

---

## Summary

| Reviewer | Verdict | Findings |
|----------|---------|----------|
| Claude Opus 4.6 | APPROVE | 1x P2 cosmetic (redundant `or ""`), 2x P3 suggestions (document convex assumption, extreme-factor tests) |
| Codex CLI | NO_OUTPUT | Exit code 2, consistent with known large-diff limitation |
| Gemini CLI | APPROVE | Clean approval, no defects found |

**Overall assessment**: APPROVE. The implementation is well-structured, mathematically correct, and thoroughly tested. The 34 tests cover all six review criteria with appropriate tolerances. Two minor cosmetic/documentation suggestions noted by Claude (P2/P3 severity, non-blocking). No security or legal compliance issues. No client references or denied terms detected.

**Reviewers attempted**: 3 -- minimum met (2 produced verdicts, 1 NO_OUTPUT).
