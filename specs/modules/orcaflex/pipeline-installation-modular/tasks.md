# WRK-032 Implementation Tasks

> Phase 2 artifact | Generated: 2026-02-03 | Rev: 2 (post iteration-1 review)
> Dependency-ordered, TDD-first

## Changes from Rev 1

- Added T00: `ModularModelGenerator.from_spec()` prerequisite (Gemini C1, #1)
- T05: addresses `BuoysBuilder.should_generate()` for S-lay roller case (Codex #13; Gemini A6, #6)
- T05: adds roller geometry formula documentation and `roller_buoy_names` context field (Codex #5, #6, #7, #14, #15)
- T06: eliminated `SectionRegistry`; extends `ModularModelGenerator` directly (Gemini #2, #3)
- T07: uses streaming generation, unified CLI subcommand, `--force`/`--resume` flags (Gemini #10, #11, #12)
- T04: adds `tensions` cross-validation, output_naming coverage validation (Gemini #5, #18)
- T08: scoped to `create_model_for_water_depth()` only (Codex #9)
- T09: adds current-profile depth-scaling test (Gemini D2)

## Task Dependency Graph

```
T00 ──→ T01 ──→ T02 ──→ T03 ──→ T04
                          ↓       ↓
                         T05 ──→ T06 ──→ T07
                                          ↓
                                         T08 ──→ T09
```

## Tasks

### T00: Add `ModularModelGenerator.from_spec()` classmethod

**File**: `src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py`
**Test file**: `tests/solvers/orcaflex/modular_generator/test_generator_from_spec.py`
**Blocked by**: None
**Complexity**: Low

**Steps (TDD)**:
1. Write test: `ModularModelGenerator.from_spec(spec)` accepts a `ProjectInputSpec` object
2. Write test: `from_spec()` produces same output as file-based constructor for same spec
3. Refactor constructor to separate YAML loading from validation
4. Add `from_spec()` classmethod that skips file loading, uses provided spec directly
5. Verify existing CLI still works

**Acceptance criteria**:
- [x] `from_spec(spec)` generates identical output to file-based path
- [x] Existing CLI and file-based usage unaffected

---

### T01: Add `RollerType` enum and `RollerStation` model

**File**: `src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py`, `_enums.py`
**Test file**: `tests/solvers/orcaflex/modular_generator/schema/test_roller_arrangement.py`
**Blocked by**: T00
**Complexity**: Low

**Steps (TDD)**:
1. Write tests for `RollerType` enum values (v_roller, flat, cradle)
2. Write tests for `RollerStation` validation:
   - Position must have 3 components (Field + validator)
   - v_angle: `None` allowed for flat/cradle; range [30, 180] when set
   - friction_coefficient range [0, 1]
   - diameter range (0.05, 5.0]
   - support_count range [1, 20]
   - Defaults: support_count=4, v_angle=None, diameter=0.5, friction=0.1
3. Implement `RollerType` enum in `_enums.py`
4. Implement `RollerStation` model with `Field()` constraints in `equipment.py`

**Acceptance criteria**:
- [x] All Field constraints use `Field(ge=..., le=...)` matching codebase convention
- [x] Invalid values raise `ValidationError` with descriptive messages
- [x] `v_angle=None` accepted for flat and cradle types

---

### T02: Add `RollerArrangement` model with `uniform()` factory

**File**: `src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py`
**Test file**: `tests/solvers/orcaflex/modular_generator/schema/test_roller_arrangement.py`
**Blocked by**: T01
**Complexity**: Low

**Steps (TDD)**:
1. Write tests for `RollerArrangement`:
   - Requires at least 1 station
   - `validate_v_angle_for_type` defaults v_angle=120 for V_ROLLER stations where None
   - Flat/cradle types leave v_angle as None
2. Write tests for `uniform()` factory:
   - Creates correct number of stations spaced along X-axis
   - Docstring documents X-axis limitation
   - `**station_kwargs` forwarded to each station
3. Implement model and factory

**Acceptance criteria**:
- [x] V_ROLLER type auto-fills v_angle=120 on stations missing it
- [x] `uniform(V_ROLLER, 3, [5,0,-2], 10)` creates 3 stations at x=5,15,25
- [x] Empty stations list raises `ValidationError`

---

### T03: Extend `Equipment` with `roller_arrangement` and backward compat

**File**: `src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py`
**Test file**: `tests/solvers/orcaflex/modular_generator/schema/test_equipment_compat.py`
**Blocked by**: T02
**Complexity**: Medium

**Steps (TDD)**:
1. Write tests:
   - Legacy `rollers` field still parses
   - New `roller_arrangement` field parses
   - Auto-conversion: legacy → single-station RollerArrangement
   - `roller_arrangement` takes precedence when both set
   - Neither set → `get_effective_rollers()` returns None
   - **Load existing floating spec** (`docs/modules/orcaflex/pipeline/installation/floating/30in_pipeline/spec.yml`) — must parse without error
   - **Load existing S-lay spec** (`docs/modules/orcaflex/pipeline/installation/s-lay/SB-SA/spec.yml`) — must parse without error
2. Add `roller_arrangement` field to `Equipment`
3. Add `resolve_roller_compat` model validator
4. Add `get_effective_rollers()` method
5. Export new models from `schema/__init__.py`

**Acceptance criteria**:
- [x] All existing tests pass
- [x] Both existing spec examples load and validate
- [x] Legacy `rollers` auto-converts to single-station arrangement

---

### T04: Add campaign models (`CampaignMatrix`, `CampaignSpec`)

**File**: `src/digitalmodel/solvers/orcaflex/modular_generator/schema/campaign.py` (new)
**Test file**: `tests/solvers/orcaflex/modular_generator/schema/test_campaign.py`
**Blocked by**: T03
**Complexity**: Medium

**Steps (TDD)**:
1. Write tests for `EnvironmentVariation`, `SoilVariation` (with optional slope)
2. Write tests for `CampaignMatrix.combinations()`:
   - Return type: `list[dict[str, float | str]]` with documented keys
   - 3 depths x 2 environments = 6 dicts
   - Each dict has "water_depth" key (always) + optional keys
3. Write tests for `CampaignSpec` validators:
   - `tensions` without S-lay base raises `ValueError`
   - `output_naming` missing a varying param raises `ValueError`
   - `max_runs` limits combination count
4. Write tests for `generate_run_specs()`:
   - Returns iterator (not list)
   - Uses `model_copy(deep=True)` (verify by checking spec is independent)
   - Overrides applied correctly per mapping table
   - Current profile depth-scaled when water_depth changes
   - Route length delta applied to last segment only
5. Implement all models and `_apply_overrides()` function

**Acceptance criteria**:
- [x] `tensions` on floating model raises `ValueError`
- [x] Output naming validated for parameter coverage
- [x] Iterator pattern for memory efficiency
- [x] Current profile depths scaled proportionally with water depth change
- [x] Route length adjusts last segment; error if result <= 0

---

### T05: Update `BuoysBuilder` for `RollerArrangement`

**File**: `src/digitalmodel/solvers/orcaflex/modular_generator/builders/buoys_builder.py`
**Test file**: `tests/solvers/orcaflex/modular_generator/builders/test_buoys_builder.py`
**Blocked by**: T03
**Complexity**: Medium

**Steps (TDD)**:
1. Add `roller_buoy_names: list[str]` field to `BuilderContext`
2. Write tests:
   - Single-station arrangement generates one roller buoy
   - Multi-station generates numbered buoys (Roller_1, Roller_2, ...)
   - V-roller geometry: supports at `±(diameter/2) * sin(half_angle)` lateral offset, `-(diameter/2) * cos(half_angle)` vertical offset from station position
   - Flat roller geometry: supports in horizontal row
   - Context registers all names in `roller_buoy_names`
   - Legacy `rollers` field still works (via auto-conversion)
3. Fix `should_generate()`: return `True` if `equipment.get_effective_rollers() is not None` even for S-lay (route rollers are separate from stinger rollers)
4. Add `_build_roller_arrangement()` method
5. Add `get_support_geometry()` static method with documented formula:

   **V-roller geometry** (per support pair):
   ```
   half_angle = v_angle / 2
   r = diameter / 2
   lateral_offset = r * sin(radians(half_angle))
   vertical_offset = -r * cos(radians(half_angle))
   ```
   Support positions relative to station origin:
   - `[0, +lateral_offset, vertical_offset + height_offset]`
   - `[0, -lateral_offset, vertical_offset + height_offset]`
   For `support_count > 2`, additional supports are distributed along X-axis at small offsets.

6. Update `build()` to use `get_effective_rollers()` and register `roller_buoy_names` in context
7. Update `GroupsBuilder` and any builders referencing old `"Rollers"` name to use `context.roller_buoy_names`

**Acceptance criteria**:
- [x] Multi-station generates correct numbered 6D buoys
- [x] Support positions reflect v_angle and diameter mathematically
- [x] S-lay models with `roller_arrangement` generate roller buoys
- [x] `roller_buoy_names` registered in context for downstream builders
- [x] GroupsBuilder uses `roller_buoy_names` instead of hardcoded `"Rollers"`
- [x] Existing single-roller tests still pass

---

### T06: Add `VariableResolver` and extend `ModularModelGenerator`

**File**: `src/digitalmodel/solvers/orcaflex/modular_generator/sections.py` (new — VariableResolver only)
**File**: `src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py` (extend generator)
**Test file**: `tests/solvers/orcaflex/modular_generator/test_sections.py`
**Blocked by**: T04, T05
**Complexity**: Medium

**Steps (TDD)**:
1. Write tests for `VariableResolver`:
   - `${water_depth}` replaced with value
   - `${water_depth:.0f}` formats as integer
   - Whole-number floats auto-format as int
   - Unresolved `${...}` raises `ValueError` with token name
   - Post-substitution YAML validation catches invalid structure
   - No `${...}` tokens → string returned unchanged
2. Write tests for `ModularModelGenerator.generate_with_overrides()`:
   - Default (no overrides) → same output as `generate()`
   - Section with custom template → loads template, applies variables, writes
   - Disabled section → skipped in output
   - Template paths resolved relative to provided base directory
   - Master YAML lists only generated include files
   - **If custom template replaces a builder, context entries for that builder are unavailable** — test that downstream builders handle missing context gracefully
3. Implement `VariableResolver` in `sections.py`
4. Add `generate_with_overrides(output_dir, sections, variables, template_base_dir)` to `ModularModelGenerator`
5. This method reuses the existing builder loop but intercepts sections with custom templates

**Acceptance criteria**:
- [x] Variable resolution with format specs works
- [x] YAML validation catches broken substitutions
- [x] Custom templates override builder output correctly
- [x] Master YAML generated with correct `includefile` entries
- [x] No new `SectionRegistry` or `SectionComposer` classes (eliminated per review)

---

### T07: Implement `CampaignGenerator`

**File**: `src/digitalmodel/solvers/orcaflex/modular_generator/campaign.py`
**Test file**: `tests/solvers/orcaflex/modular_generator/test_campaign_generator.py`
**Blocked by**: T06
**Complexity**: High

**Steps (TDD)**:
1. Write tests:
   - Loads valid campaign YAML
   - `preview()` returns correct combinations
   - `validate()` catches: negative depths, duplicate names, tensions on floating
   - `generate()` creates expected directory structure
   - `--force` overwrites; default skips existing with warning
   - `--resume` skips directories containing `master.yml`
   - `max_runs` limits generation
   - Streaming: memory usage constant regardless of matrix size
   - Each run directory has `includes/` + `master.yml`
   - Overrides applied correctly per mapping table
2. Implement `CampaignGenerator`:
   - Constructor: load YAML, validate against `CampaignSpec`
   - `preview()`: delegates to `CampaignMatrix.combinations()`
   - `validate()`: comprehensive consistency checks
   - `generate(output_dir, force=False, resume=False)`: streaming loop over `generate_run_specs()`, calls `ModularModelGenerator.from_spec(spec).generate_with_overrides()`
3. Add `campaign` subcommand to existing `cli.py` (not a separate `__main__.py`):
   - `campaign --preview campaign.yml`
   - `campaign --output ./out/ campaign.yml`
   - `campaign --output ./out/ --force campaign.yml`
   - `campaign --output ./out/ --resume campaign.yml`
   - `--verbose` / `--quiet` log level control
4. Use Python `logging` module (not `print()`)

**Acceptance criteria**:
- [x] Campaign with 3 depths x 2 envs generates 6 directories
- [x] Each directory contains valid OrcaFlex include files
- [x] `--preview` shows matrix without file generation
- [x] `--force` and `--resume` work correctly
- [x] Structured logging via `logging` module
- [x] Unified CLI subcommand (no separate entry point)

---

### T08: Absorb legacy `OrcInstallation.create_model_for_water_depth()`

**File**: `src/digitalmodel/solvers/orcaflex/modular_generator/campaign.py`
**Test file**: `tests/solvers/orcaflex/modular_generator/test_legacy_compat.py`
**Blocked by**: T07
**Complexity**: Medium

**Scope**: `create_model_for_water_depth()` only. The other two methods (`create_installation_depth_model`, `create_installation_depth_model2`) handle subsea crane installation — out of scope for WRK-032.

**Steps (TDD)**:
1. Write tests:
   - Legacy `delta_elevations` config converted to campaign `water_depths`
   - `from_legacy_config()` maps reference files to base spec
   - Generated models match legacy output for 6DBuoy/3DBuoy InitialZ adjustments
   - Generated models match legacy output for line length adjustments
   - Output file naming matches legacy convention
2. Add `CampaignSpec.from_legacy_config(cfg: dict)` classmethod
3. Verify legacy test cases produce equivalent output (within float tolerance)
4. Add deprecation warning to `OrcInstallation` class docstring
5. Do NOT delete `OrcInstallation` — keep as deprecated for existing callers

**Acceptance criteria**:
- [x] `from_legacy_config()` converts elevation-based config to campaign spec
- [x] Generated models match legacy output within 0.01m tolerance
- [x] Deprecation warning on `OrcInstallation` class
- [x] `assetutilities` dependency NOT introduced into new code

---

### T09: Integration tests and example campaign files

**File**: `tests/solvers/orcaflex/modular_generator/integration/test_campaign_integration.py`
**Blocked by**: T08
**Complexity**: Medium

**Steps**:
1. Integration test: floating installation campaign
   - Load example campaign YAML
   - Generate all runs
   - Verify directory structure
   - Spot-check water depth in environment section
   - Verify current profile depth-scaled correctly
2. Integration test: S-lay installation campaign
   - Verify tension override in tensioner section
   - Verify vessel/stinger sections present
3. Integration test: backward compatibility
   - Load existing single-run floating + S-lay spec files
   - Verify they generate correctly through `ModularModelGenerator`
4. Create example campaign files:
   - `docs/modules/orcaflex/pipeline/installation/floating/30in_pipeline/campaign.yml`
   - `docs/modules/orcaflex/pipeline/installation/s-lay/SB-SA/campaign.yml`

**Acceptance criteria**:
- [x] Floating campaign generates correct output for all matrix combinations
- [x] S-lay campaign with tension variations generates correctly
- [x] Current profile depth-scaling verified in generated files
- [x] All existing modular generator tests still pass
- [x] Example campaign files parse and generate successfully

## Summary

| Task | Description | Blocked By | Complexity | New/Modified Files |
|------|-------------|------------|------------|-------------------|
| T00 | `ModularModelGenerator.from_spec()` | - | Low | `__init__.py` |
| T01 | `RollerStation` + `RollerType` | T00 | Low | `equipment.py`, `_enums.py` |
| T02 | `RollerArrangement` + uniform() | T01 | Low | `equipment.py` |
| T03 | `Equipment` extension + compat | T02 | Medium | `equipment.py`, `__init__.py` |
| T04 | Campaign models | T03 | Medium | `campaign.py` (new) |
| T05 | BuoysBuilder roller arrangement | T03 | Medium | `buoys_builder.py`, `context.py` |
| T06 | VariableResolver + generator extension | T04, T05 | Medium | `sections.py` (new), `__init__.py` |
| T07 | CampaignGenerator + CLI | T06 | High | `campaign.py`, `cli.py` |
| T08 | Legacy absorption (scoped) | T07 | Medium | `campaign.py` |
| T09 | Integration tests + examples | T08 | Medium | tests/, docs/ |

**Parallelisable**: T04 and T05 (both depend on T03, independent of each other).

**New files**: 2 (`schema/campaign.py`, `sections.py`)
**Modified files**: 6 (`equipment.py`, `_enums.py`, `__init__.py`, `buoys_builder.py`, `context.py`, `cli.py`)
**Test files**: 7 new test modules
