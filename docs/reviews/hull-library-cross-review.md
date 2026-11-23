# Hull Library Cross-Review Report

| Field | Value |
|-------|-------|
| **Module** | `src/digitalmodel/hydrodynamics/hull_library/` |
| **Reviewer** | Claude Opus 4.6 (code-review agent) |
| **Date** | 2026-02-08 |
| **Verdict** | **REQUEST_CHANGES** |
| **Files reviewed** | 5 source, 6 test, 3 seed YAML, 3 dependency modules |
| **Total lines** | 1946 source, 1612 test |

---

## Summary

The hull library is a well-structured module that provides a complete pipeline from hull geometry definition through panel mesh generation to spectral motion analysis and point acceleration calculation. The architecture is clean, the public API is well-designed, and the integration with existing `PanelMesh`, `MeshPipeline`, `WaveSpectra`, and `RAOData` interfaces is handled correctly.

However, there are several issues that must be addressed before merge: one mathematical correctness issue in the rigid-body transfer function (P0), a file exceeding the project size limit (P0), sign convention inconsistencies in the acceleration kinematics (P1), and a silent failure mode in catalog loading (P1). The test suite is solid but has coverage gaps around error paths and boundary conditions.

---

## Findings

### P0 -- Blocking Issues

#### P0-1. Incorrect rigid-body acceleration transfer signs (catalog.py:327-337)

**File:** `src/digitalmodel/hydrodynamics/hull_library/catalog.py`, lines 327-337

The rigid-body transfer from COG to an arbitrary point uses the linearised small-angle formulation. The current implementation is:

```python
acc_rao_x = (surge_rao + pitch_rao * dz + yaw_rao * dy) * omega_sq
acc_rao_y = (sway_rao + roll_rao * dz + yaw_rao * dx) * omega_sq
acc_rao_z = (heave_rao + pitch_rao * dx + roll_rao * dy) * omega_sq
```

The correct linearised rigid-body transfer for **acceleration** at a point offset (dx, dy, dz) from the COG, expressed via angular accelerations, is:

```
a_x = surge_accel - yaw_accel * dy + pitch_accel * dz
a_y = sway_accel  + yaw_accel * dx - roll_accel * dz
a_z = heave_accel - pitch_accel * dx + roll_accel * dy
```

This comes from the cross-product **omega_dot x r** where positive rotations follow the right-hand rule. The sign on `yaw_rao * dy` in the longitudinal equation should be negative (minus), and the sign on `roll_rao * dz` in the lateral equation should also be negative (minus), and the sign on `pitch_rao * dx` in the vertical equation should be negative (minus).

The current code has:
- Longitudinal: `+ yaw_rao * dy` (should be `- yaw_rao * dy`)
- Lateral: `+ roll_rao * dz` (should be `- roll_rao * dz`)
- Vertical: `+ pitch_rao * dx` (should be `- pitch_rao * dx`)

All three cross-product terms have incorrect signs. This will produce wrong acceleration magnitudes at off-centerline points and at points with large vertical lever arms. For on-centerline points (dy=0) with small dz relative to dx, the vertical acceleration error is most significant.

**Impact:** Numerically incorrect acceleration outputs -- fundamental to the stated purpose of the module.

**Fix:** Correct the signs per the standard rigid-body kinematics cross-product formulation (DNV-RP-C205 or any naval architecture reference for rigid-body transfer).

---

#### P0-2. mesh_generator.py exceeds 500-line project limit (721 lines)

**File:** `src/digitalmodel/hydrodynamics/hull_library/mesh_generator.py` -- 721 lines

The project CLAUDE.md states "Files under 500 lines (modular design)". At 721 lines, `mesh_generator.py` exceeds this by 44%. The `coarsen_mesh` function (lines 499-667) and its helper `_compute_feature_subdivision` (lines 670-714) are logically independent from the `HullMeshGenerator` class and should be extracted to a separate `mesh_coarsener.py` module. This would bring both files well within the limit.

**Impact:** Violates project coding standards. Makes the file harder to navigate and review.

**Fix:** Extract `coarsen_mesh()` and `_compute_feature_subdivision()` into `mesh_coarsener.py`. Update `__init__.py` imports accordingly.

---

### P1 -- Should Fix

#### P1-1. Silent exception swallowing in catalog profile loading (catalog.py:363-365)

**File:** `src/digitalmodel/hydrodynamics/hull_library/catalog.py`, lines 363-365

```python
except Exception:
    # Skip files that cannot be parsed
    continue
```

This bare `except Exception` with a silent `continue` swallows all errors during profile loading, including `PermissionError`, `MemoryError`, or bugs in the schema code. A malformed YAML file or a regression in `HullProfile.from_yaml_dict` would be invisible. At minimum, this should log the exception. Better: catch only `(yaml.YAMLError, ValueError, KeyError)` and log at WARNING level.

**Impact:** Debugging failures during catalog loading will be extremely difficult. Regression bugs in the schema parser will go undetected.

---

#### P1-2. `mesh_below_waterline_only` config flag is declared but never used (mesh_generator.py:48)

**File:** `src/digitalmodel/hydrodynamics/hull_library/mesh_generator.py`, line 48

`MeshGeneratorConfig` defines `mesh_below_waterline_only: bool = True` but the `HullMeshGenerator.generate()` method never references it. The z-grid is always built from `-draft` to `0.0` (lines 85-86), which inherently meshes only below the waterline. If the intent is to also support meshing above the waterline (up to `depth`), the flag should control the z-range upper bound. If the flag is not needed, it should be removed to avoid misleading users.

**Impact:** Dead code that misleads API users into thinking they can control above-waterline meshing.

---

#### P1-3. Waterline offsets use keel-up z-convention but this is not validated or documented in the schema (profile_schema.py:48-60)

**File:** `src/digitalmodel/hydrodynamics/hull_library/profile_schema.py`, lines 48-60

The `HullStation.waterline_offsets` are documented as "(z, y) pairs: z = draft, y = half-breadth" but the actual convention -- that z is measured from keel upward (0 at keel, draft at waterline) -- is only implicitly revealed in `mesh_generator.py` line 280 where `z_marine = z_keel - draft` is computed. The YAML seed data confirms this (z ranges from 0 to the draft/depth value).

There is no validation that:
- z values in waterline_offsets are non-negative
- z values do not exceed `depth`
- y (half-breadth) values are non-negative

Negative half-breadth values would produce geometrically invalid hulls. The mesh generator silently clamps `y_grid` to zero (line 310), masking the problem.

**Impact:** Invalid input data can produce subtly wrong geometry without any error or warning. The coordinate convention is a common source of confusion in naval architecture software.

---

#### P1-4. `_interpolate_hull_surface` uses `fill_value` extrapolation that can produce wrong geometry (mesh_generator.py:288-293, 300-307)

**File:** `src/digitalmodel/hydrodynamics/hull_library/mesh_generator.py`, lines 288-293 and 300-307

Both z-interpolation and x-interpolation use `bounds_error=False` with `fill_value` set to the endpoint values. This means:
- For z-interpolation: if the z-grid extends beyond the station's waterline offset range, the half-breadth is extrapolated as constant. For stations where offsets do not go all the way to the waterline (z=0 in marine convention), this fills in the wrong value.
- For x-interpolation: if `x_values` extends beyond the station x-range (which happens by design since `x_values` goes from 0 to `length_bp`), the half-breadth is held constant at the end station values rather than tapering to zero.

For hull forms where the first station is not at x=0 or the last station is not at x=`length_bp`, this produces flat "shelves" in the hull surface rather than a natural taper.

**Impact:** Geometric inaccuracy at hull extremities for profiles where stations do not cover the full length range.

---

#### P1-5. `_adaptive_x_grid` evaluates half-breadth at keel-up draft, not marine draft (mesh_generator.py:196-211)

**File:** `src/digitalmodel/hydrodynamics/hull_library/mesh_generator.py`, lines 196-211

In `_compute_adaptive_x_grid`, the station half-breadth is evaluated at `draft` (line 211: `interp_z(draft)`), but the station waterline offsets use keel-up coordinates. This means `draft` here is the z-value in keel-up coordinates corresponding to the waterline -- which is correct. However, the z-interpolation is performed on `z_keel` coordinates without the marine convention transform (unlike `_interpolate_hull_surface` which does `z_marine = z_keel - draft`). This is actually consistent within the adaptive function, but the inconsistency in approach between the two methods is fragile and confusing. If the keel-up convention is ever changed, one function would break silently.

**Impact:** Maintenance risk. Both methods process the same station data with different coordinate handling. A refactor that changes one will silently break the other.

---

#### P1-6. No `model_validator` cross-checking stations against principal dimensions (profile_schema.py)

**File:** `src/digitalmodel/hydrodynamics/hull_library/profile_schema.py`

The schema validates individual fields (positive length, beam, draft) but never cross-validates that station data is consistent with principal dimensions. For example:
- Station x-positions could be outside `[0, length_bp]`
- Station half-breadths could exceed `beam / 2`
- Station z-offsets could exceed `depth`

These inconsistencies would produce incorrect meshes and misleading schematics.

**Impact:** No protection against inconsistent hull definitions that would produce wrong geometry.

---

#### P1-7. Test helper duplication across test files

**Files:**
- `tests/hydrodynamics/hull_library/test_profile_schema.py`, lines 71-97 (`_make_box_profile`)
- `tests/hydrodynamics/hull_library/test_mesh_generator.py`, lines 35-69 (`_make_box_profile`)
- `tests/hydrodynamics/hull_library/test_schematic_generator.py`, lines 11-41 (`_make_box_profile`)
- `tests/hydrodynamics/hull_library/test_catalog.py`, lines 66-84 (inline profile creation)

The `_make_box_profile` helper is duplicated four times with slight variations. This violates DRY and means that changes to the profile schema require updates in multiple test files.

**Impact:** Maintenance burden. Inconsistent test fixtures across files.

**Fix:** Create a `conftest.py` in the test directory with shared fixtures (`box_profile`, `ship_profile`, `simple_rao_data`).

---

### P2 -- Nice to Have

#### P2-1. `coarsen_mesh` vertex clustering uses Python loops instead of vectorized operations (mesh_generator.py:607-610, 614-637)

**File:** `src/digitalmodel/hydrodynamics/hull_library/mesh_generator.py`, lines 607-610 and 614-637

The centroid accumulation loop (`for vi in range(len(vertices))`) and the panel remapping loop (`for pi in range(len(panels))`) iterate in Python over potentially large arrays. For meshes with thousands of panels, this will be slow. Both loops can be replaced with `np.add.at` for centroid accumulation and vectorized indexing for panel remapping.

**Impact:** Performance -- not blocking but will matter for production-scale meshes (>5000 panels).

---

#### P2-2. Schematic generator does not sanitize profile name for SVG text content (schematic_generator.py:267, 335, 401)

**File:** `src/digitalmodel/hydrodynamics/hull_library/schematic_generator.py`, lines 267, 335, 401

The `profile.name` is embedded directly into SVG `<text>` elements without XML entity escaping. If the profile name contains characters like `<`, `>`, `&`, or `"`, the SVG output will be malformed. While unlikely in practice (names come from YAML files), this is an injection vector if SVGs are served in a web context.

**Impact:** Minor security/robustness concern. SVG output corruption with special characters.

**Fix:** Apply `xml.sax.saxutils.escape()` to text content in `_svg_text()`.

---

#### P2-3. `_deduplicate_vertices` uses a Python `set` loop instead of fully vectorized approach (mesh_generator.py:443-449)

**File:** `src/digitalmodel/hydrodynamics/hull_library/mesh_generator.py`, lines 443-449

The first-occurrence tracking loop iterates over all vertices with a Python set. This can be vectorized using `np.unique` with `return_index=True` and then sorting.

**Impact:** Minor performance improvement for large meshes.

---

#### P2-4. `_remove_degenerate_panels` uses a Python loop (mesh_generator.py:466-474)

**File:** `src/digitalmodel/hydrodynamics/hull_library/mesh_generator.py`, lines 466-474

Converting each panel to a set in a Python loop is O(n) with high constant factor. A vectorized approach using `np.unique` per row would be faster.

**Impact:** Minor performance.

---

#### P2-5. `_orient_normals_outward` uses centroid-based heuristic that can fail for non-convex hulls (mesh_generator.py:476-496)

**File:** `src/digitalmodel/hydrodynamics/hull_library/mesh_generator.py`, lines 476-496

The outward normal orientation uses the mesh centroid as the "inside" reference point. For non-convex hull forms (e.g., semi-submersible pontoons with column cutouts), the centroid may not be inside the hull, causing normals to be flipped incorrectly for some panels.

**Impact:** Incorrect for non-convex hulls. Acceptable for the currently supported hull types (tanker, barge, ship) but limits future extensibility.

---

#### P2-6. Missing `__repr__` and `__str__` on key classes for debugging

**Files:** `catalog.py` (HullCatalog, HullCatalogEntry), `mesh_generator.py` (HullMeshGenerator)

These classes lack string representations, making debugging harder in interactive sessions and logs.

**Impact:** Developer experience.

---

#### P2-7. `schematic_generator.py` line 67 exceeds 100-character line limit

**File:** `src/digitalmodel/hydrodynamics/hull_library/schematic_generator.py`, line 67

```python
return f'<line x1="{x1:.1f}" y1="{y1:.1f}" x2="{x2:.1f}" y2="{y2:.1f}" {style}/>\n'
```

This line is 85 characters so it fits under the hard limit, but several other lines in the SVG helper functions are close. No actual violations found, but worth monitoring.

**Impact:** Cosmetic.

---

#### P2-8. Test coverage gaps

**Tests missing:**
- No test for `HullProfile.from_yaml_dict` with missing required keys (should raise `KeyError`)
- No test for `HullProfile.from_yaml_dict` with malformed data (wrong types)
- No test for `coarsen_mesh` when `target_panels >= mesh.n_panels` (should return original mesh)
- No test for `compute_motions` with mismatched RAO frequency range (RAOs outside spectrum range)
- No test for `compute_accelerations` with point at COG (should give pure translational accelerations)
- No negative test for `SeaStateDefinition` with invalid values (e.g. negative Hs)
- No test for `SchematicGenerator` with custom width/height/margin parameters
- The `test_adaptive_clusters_panels_at_ends` test has a conditional guard (`if np.sum(mid_mask) > 0 and np.sum(bow_mask) > 0`) that can silently pass without testing anything

**Impact:** Test gaps reduce confidence in edge-case behavior.

---

#### P2-9. `HullVariation` model is defined but never used (catalog.py:71-80)

**File:** `src/digitalmodel/hydrodynamics/hull_library/catalog.py`, lines 71-80

`HullVariation` is defined and exported but never instantiated or used by any method in the catalog. This appears to be forward-declared for future use.

**Impact:** Dead code. Should either be implemented or removed per YAGNI.

---

## Strengths

1. **Clean public API**: The `__init__.py` exports are well-organized and the `__all__` declarations in each module are consistent. Users get a clear, flat namespace.

2. **Pydantic validation**: The schema module makes good use of Pydantic v2 `field_validator` for invariant checking (non-empty offsets, minimum stations, block coefficient range, positive dimensions). This catches many data errors at construction time.

3. **YAML round-trip**: The `to_yaml_dict` / `from_yaml_dict` pair is well-designed, handling enum serialization and tuple/list conversion correctly. The `save_yaml` / `load_yaml` convenience methods are clean.

4. **Mesh generation algorithm**: The two-pass interpolation (z-axis then x-axis) with `scipy.interpolate.interp1d` is a standard and correct approach. The coordinate transform from keel-up to marine convention is handled correctly in `_interpolate_hull_surface`.

5. **Adaptive density**: The curvature-based inverse-CDF sampling in `_compute_adaptive_x_grid` is a solid algorithm choice. Using `|d^2y/dx^2|` as a curvature proxy and concentrating panels at high-curvature regions (bow/stern) is correct for hydrodynamic analysis where radiation and diffraction effects are strongest at hull discontinuities.

6. **Coarsening with feature preservation**: The `coarsen_mesh` function uses per-cell normal variance to identify high-curvature regions and applies finer subdivision there. This is a practical and effective approach for mesh LOD generation.

7. **Degenerate panel handling**: The mesh generator correctly identifies and removes degenerate panels after vertex deduplication (lines 116-117, 454-474), which is essential for bow/stern tip regions where half-breadths collapse to zero.

8. **Integration test coverage**: The `test_integration.py` file tests the full chain from YAML load through mesh generation, GDF/DAT export, reload, and motion calculation. This gives high confidence in interface compatibility.

9. **SVG generation**: The schematic generator produces clean SVG with proper coordinate mapping, Y-axis inversion, grid lines, and axis labels. The three-view convention (profile, plan, body plan) follows standard naval architecture practice. The body plan correctly places forward stations on the right and aft stations on the left.

10. **Spectral analysis chain**: The `compute_motions` method correctly implements the standard spectral response analysis: `S_response = |RAO|^2 * S_wave`, `m0 = integral(S_response)`, `sig = 2*sqrt(m0)`. The numpy compatibility shim `_trapz` (line 38-42) is a thoughtful detail for supporting both old and new numpy APIs.

11. **Good use of TYPE_CHECKING**: The mesh generator uses `TYPE_CHECKING` to avoid circular imports with the profile schema (lines 36-39).

12. **Seed data quality**: The three YAML profiles cover key hull forms (box for verification, barge for shallow draft, tanker for complex curves) with realistic dimensions and appropriate station density. The tanker profile with 11 stations and 7 waterline offsets per station is sufficiently detailed for the target panel counts.

---

## File-by-File Summary

| File | Lines | Verdict | Key Issues |
|------|-------|---------|------------|
| `profile_schema.py` | 263 | COMMENT | P1-3, P1-6: missing validation and cross-checks |
| `mesh_generator.py` | 721 | REQUEST_CHANGES | P0-2: exceeds line limit; P1-2, P1-4, P1-5 |
| `schematic_generator.py` | 519 | APPROVE | P2-2: minor XSS concern |
| `catalog.py` | 406 | REQUEST_CHANGES | P0-1: wrong kinematics signs; P1-1: silent exception |
| `__init__.py` | 37 | APPROVE | Clean |
| `test_profile_schema.py` | 287 | APPROVE | Solid coverage |
| `test_mesh_generator.py` | 656 | APPROVE | Thorough, good parametric checks |
| `test_schematic_generator.py` | 145 | COMMENT | P2-8: could test custom dimensions |
| `test_catalog.py` | 230 | COMMENT | P2-8: coverage gaps on error paths |
| `test_seed_data.py` | 57 | APPROVE | Good parametric validation |
| `test_integration.py` | 243 | APPROVE | Excellent end-to-end coverage |
| `unit_box.yaml` | 33 | APPROVE | Correct box geometry |
| `generic_barge.yaml` | 54 | APPROVE | Realistic barge with raked ends |
| `generic_tanker.yaml` | 127 | APPROVE | Well-detailed tanker hull form |

---

## Action Items

- [ ] **P0-1**: Fix rigid-body acceleration transfer signs in `catalog.py:327-337`
- [ ] **P0-2**: Split `mesh_generator.py` to bring it under 500 lines
- [ ] **P1-1**: Replace bare `except Exception` with specific exceptions and logging in `catalog.py:363-365`
- [ ] **P1-2**: Either implement or remove `mesh_below_waterline_only` flag
- [ ] **P1-3**: Add z and y validation to `HullStation.waterline_offsets`; document keel-up convention
- [ ] **P1-4**: Consider tapering half-breadth to zero outside station x-range instead of extrapolating constant
- [ ] **P1-5**: Refactor station offset interpolation into a shared helper to avoid convention divergence
- [ ] **P1-6**: Add a `model_validator` to `HullProfile` that cross-checks station geometry against principal dimensions
- [ ] **P1-7**: Create `tests/hydrodynamics/hull_library/conftest.py` with shared fixtures
- [ ] **P2-8**: Add missing edge-case tests (see list above)
- [ ] **P2-9**: Remove `HullVariation` or implement variation support

---

*Report generated by Claude Opus 4.6 code-review agent on 2026-02-08.*
