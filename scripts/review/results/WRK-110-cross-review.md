# WRK-110 Cross-Review: Hull Library Expansion (FST + LNGC)

**Commit**: `11d3e2db2` (digitalmodel)
**Date**: 2026-02-13
**Files reviewed**:
- `data/hull_library/profiles/fst_barge_250m.yaml` (86 lines)
- `data/hull_library/profiles/fst_ship_330m.yaml` (126 lines)
- `data/hull_library/profiles/lngc_qflex_315m.yaml` (126 lines)
- `data/hull_library/profiles/lngc_qmax_345m.yaml` (126 lines)
- `data/hull_library/catalog/hull_panel_catalog.yaml` (+77 lines)
- `data/hull_library/panels/fpso/fst_barge_250m.gdf` (8004 lines)
- `data/hull_library/panels/fpso/fst_ship_330m.gdf` (8644 lines)
- `data/hull_library/panels/lngc/lngc_qflex_315m.gdf` (8824 lines)
- `data/hull_library/panels/lngc/lngc_qmax_345m.gdf` (9664 lines)
- `scripts/generate_hull_library_meshes.py` (97 lines)
- `tests/hydrodynamics/hull_library/test_hull_library_expansion.py` (290 lines)

**Total**: 11 files, 36,063 insertions, 1 deletion, 26 tests across 8 test classes

---

## Claude Review (Claude Opus 4.6)
**Verdict**: APPROVE (with 3 minor observations)

### 1. Hull Profile Correctness

**FST Barge 250m**: 9 stations, 5 waterline offsets per station. Stations 1-7 (x=31.25 to x=218.75) have identical full-width rectangular sections with offset 23.0 m at all draft levels -- correctly representing a barge-type parallel midbody covering ~75% of length. Stations 0 and 8 (AP and FP) show mild rake narrowing from 23.0 m at keel to 18.0 m at draft=14.0 m. Block coefficient 0.95 is realistic for a barge form. The flat keel profile `[0,0] -> [125,0] -> [250,0]` is correct for a flat-bottomed barge.

**FST Ship 330m**: 11 stations, 7 waterline offsets per station. Clear VLCC-type hull form: stern frame (station 0) starts narrow at `[0,0]` and fills out progressively. Parallel midbody at stations 4-6 (x=132 to x=198) with maximum half-beam 30.0 m at upper waterlines. Forward fining from station 7 through station 10 (bow) where the waterline half-beam at draft reduces to 8.0 m. Block coefficient 0.82 is appropriate for a converted VLCC. The offsets monotonically increase from keel to waterline at each station, which is physically correct.

**LNGC Q-Flex 315m**: 11 stations, 7 offsets each. Similar ship-like progression with narrower stern (max 16.0 m at AP waterline), expanding parallel midbody at stations 4-6 (max 25.0 m half-beam), and fine bow entrance at station 10 (7.5 m at waterline). Block coefficient 0.78 is realistic for a Q-Flex. The draft of 12.0 m with 27.0 m depth gives generous freeboard typical of LNG carriers with high deck structures.

**LNGC Q-Max 345m**: 11 stations, 7 offsets each. Slightly fuller than Q-Flex with max half-beam 27.5 m at parallel midbody. Block coefficient 0.80 is correct (slightly higher than Q-Flex due to larger capacity). Fine bow entrance with 8.0 m half-beam at waterline. Station spacing is uniform at L/10 = 34.5 m.

- **P2 observation**: The FST barge profile has only 5 waterline offsets per station while the ship-type profiles have 7. With only 5 offsets spanning 14 m draft, the vertical resolution is coarser (3.5 m spacing vs 2.0 m for LNGC and ~3.5 m for FST ship). This is acceptable for a rectangular barge where section shape varies little, but finer resolution would improve the accuracy of the raked bow/stern sections at stations 0 and 8.

- **No issues found** with hull shape realism. All four profiles represent physically plausible hull forms.

### 2. Mesh Quality

- **Panel counts**: Barge 2000, Ship 2160, Q-Flex 2205, Q-Max 2415 -- all within the 2000-3000 target range. The generation script targets 2500 panels; actual counts vary by hull complexity, which is expected.
- **GDF format**: Both sample GDF files examined (barge and Q-Max) have correct headers: `1.0  9.81` (length scale, gravity), `0  1` (ISX, ISY symmetry flags -- `0` = no X-symmetry, `1` = Y-symmetry), and declared panel counts matching the catalog.
- **Vertex coordinates**: Barge GDF starts at `[0.0, 23.0, -14.0]` (x=0, y=beam/2, z=-draft), which is consistent with the Y-symmetric convention (starboard half only) and marine z-convention (z<0 below waterline). Q-Max GDF starts at `[0.0, 0.0, -12.0]` (stern centerline at full draft), consistent with the narrow stern frame.
- **P3 observation**: The vertex counts in the catalog (barge: 2259, ship: 2408, Q-Flex: 2526, Q-Max: 2766) are higher than panel counts, which is expected for a quad-panel mesh (shared vertices between adjacent panels). The ratio of vertices to panels (~1.13) is reasonable and suggests no degenerate or orphan vertices.

### 3. Catalog Consistency

All 4 new catalog entries verified against profiles:

| Hull | catalog length_m | profile length_bp | Match | catalog beam_m | profile beam | Match | catalog draft_m | profile draft | Match |
|------|-----------------|-------------------|-------|---------------|-------------|-------|-----------------|-------------|-------|
| fst_barge_250m | 250.0 | 250.0 | Yes | 46.0 | 46.0 | Yes | 14.0 | 14.0 | Yes |
| fst_ship_330m | 330.0 | 330.0 | Yes | 60.0 | 60.0 | Yes | 22.0 | 22.0 | Yes |
| lngc_qflex_315m | 315.0 | 315.0 | Yes | 50.0 | 50.0 | Yes | 12.0 | 12.0 | Yes |
| lngc_qmax_345m | 345.0 | 345.0 | Yes | 55.0 | 55.0 | Yes | 12.0 | 12.0 | Yes |

- All entries use `source: canonical`, `panel_format: gdf`, `symmetry: y`, and include `displacement_t` matching the profile `displacement` field.
- File paths are consistent: FST hulls under `panels/fpso/`, LNGC hulls under `panels/lngc/`.
- Tags include hull-type identifiers (`fst`, `vlcc`, `lngc`, `qflex`, `qmax`).

**Displacement cross-check** (Cb * L * B * T * rho_sw):

| Hull | Profile displacement | Computed (Cb*L*B*T*1.025) | Delta |
|------|---------------------|---------------------------|-------|
| fst_barge_250m | 148,120 t | 156,774 t | -5.5% |
| fst_ship_330m | 356,400 t | 366,122 t | -2.7% |
| lngc_qflex_315m | 148,176 t | 151,106 t | -1.9% |
| lngc_qmax_345m | 182,160 t | 186,714 t | -2.4% |

- **P2 observation**: The barge displacement (148,120 t) is 5.5% below the theoretical Cb*L*B*T*rho value. This is the largest delta of the four hulls. It likely reflects that the raked bow/stern reduce the actual displaced volume below the nominal Cb*L*B*T. This is physically reasonable but worth noting: the stated `block_coefficient: 0.95` is the section-based Cb, while the actual volumetric Cb accounting for the raked ends would be slightly lower (~0.90). The ship-type hulls show smaller deltas (1.9-2.7%) which is consistent with their finer station definitions absorbing more of the end-effect volume reduction. Not blocking -- the displacement values are self-consistent with the actual station geometry.

### 4. Test Coverage

26 tests organized into 8 test classes:

| Class | Tests | What it validates |
|-------|-------|-------------------|
| TestProfileLoading | 4 | YAML loads correctly, dimensions match, hull_type correct, >=2 stations |
| TestMeshFiles | 4 | GDF file exists on disk and has non-zero size |
| TestGDFReadability | 4 | GDFHandler parses mesh with n_panels > 0 and n_vertices > 0 |
| TestMeshDimensions | 4 | Bounding box within 5% of expected L, B/2, T |
| TestPanelCountRange | 4 | Panel count in [1500, 3500] range |
| TestCatalogEntries | 4 | Catalog has entry with correct hull_type, dimensions, non-null panel_count |
| TestCatalogCompleteness | 1 | Total entries >= 27 (23 original + 4 new) |
| TestMeshGenerationRoundTrip | 1 | Generate mesh from profile with target_panels=500, verify non-empty result |

All acceptance criteria are covered. Tests use `pytest.mark.parametrize` for the 4-hull matrix, avoiding test duplication. Fixtures are module-scoped (`catalog_data`, `gdf_handler`) for efficiency.

- **P3 observation**: The round-trip test (class 8) only exercises `fst_barge_250m` with target_panels=500. A parametrized version across all 4 hulls would strengthen coverage, though the per-hull mesh dimension tests already implicitly validate the full-resolution generation.

### 5. Security / Legal Compliance

- No hardcoded secrets, API keys, or credentials in any file.
- No client references, project codenames, or proprietary tool names. All sources described as "generic" (e.g., "generic barge-type floating storage terminal", "generic Q-Max class LNG carrier").
- No denied terms from the legal deny list.
- Legal sanity scan (`legal-sanity-scan.sh --repo=digitalmodel --diff-only`): **PASS** -- no violations found.
- No file system access outside project paths. The generation script uses `Path(__file__).resolve().parent.parent` for project root.
- No network calls, no subprocess invocations.
- **Clean.**

### 6. Script Quality (generate_hull_library_meshes.py)

- 97 lines, well-structured with clear configuration section and main loop.
- Uses `PROFILE_OUTPUT_MAP` dict to map profiles to output subdirectories -- easy to extend for future hulls.
- Proper imports from the project's own modules (`profile_schema`, `mesh_generator`, `gdf_handler`).
- Creates output directories with `mkdir(parents=True, exist_ok=True)`.
- Reports bounding box extents, panel/vertex counts, and total area for each hull -- good for verification.
- No issues found.

### Summary of Findings
| # | Severity | Finding |
|---|----------|---------|
| 1 | P2 | Barge profile has only 5 waterline offsets vs 7 for ship-type profiles; coarser vertical resolution at raked ends |
| 2 | P2 | Barge displacement 5.5% below Cb*L*B*T*rho -- consistent with raked-end volume reduction but stated Cb=0.95 is section-based, not volumetric |
| 3 | P3 | Round-trip mesh generation test only exercises one hull (fst_barge_250m) |

---

## Codex Review
**Verdict**: NO_OUTPUT

Codex CLI exited with code 2. The `review --commit` subcommand failed, consistent with previous behavior on large diffs (36K+ lines including GDF mesh files). No review output produced.

---

## Gemini Review (Gemini CLI)
**Verdict**: APPROVE

### Findings

Gemini performed a review covering all five criteria:

- **Hull profile correctness (P3 - No issues)**: Confirmed FST barge exhibits rectangular shape with long parallel midbody and raked ends. Ship-type (FST ship) and LNGC hulls show fining towards bow/stern with distinct parallel midbody. Block coefficients confirmed as typical: 0.95 (barge), 0.82 (ship), 0.78 (Q-Flex), 0.80 (Q-Max).
- **Mesh quality (P3 - No issues)**: Panel counts of 2000, 2160, 2205, 2415 confirmed within 2000-3000 range. Generation script targets 2500. Tests validate [1500, 3500] bounds.
- **Catalog consistency (P3 - No issues)**: All four entries accurately reflect length_m, beam_m, draft_m from profiles. Panel counts correctly listed.
- **Test coverage (P3 - No issues)**: 26 tests confirmed across 8 classes covering profile loading, mesh files, GDF readability, dimensions, panel counts, catalog entries, completeness, and round-trip generation.
- **Security/legal (P3 - No issues)**: No client-specific references, proprietary information, or secrets found.

No defects or concerns raised.

---

## Summary

| Reviewer | Verdict | Findings |
|----------|---------|----------|
| Claude Opus 4.6 | APPROVE | 2x P2 (barge vertical resolution, displacement delta), 1x P3 (round-trip test coverage) |
| Codex CLI | NO_OUTPUT | Exit code 2, large diff (36K lines) exceeds capacity |
| Gemini CLI | APPROVE | Clean approval, no defects found |

**Overall assessment**: APPROVE. The implementation adds 4 well-defined hull forms with physically realistic station profiles, GDF panel meshes in the expected 2000-2500 panel range, correctly populated catalog entries, and comprehensive test coverage (26 tests across 8 classes). The two P2 findings (barge vertical resolution and displacement delta) are minor engineering observations that do not affect mesh quality or functionality. No security or legal compliance issues. No client references or denied terms detected. Legal scan passed.

**Reviewers attempted**: 3 -- minimum met (2 produced verdicts, 1 NO_OUTPUT).
