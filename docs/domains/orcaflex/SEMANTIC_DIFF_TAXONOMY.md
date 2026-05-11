# OrcaFlex YAML Semantic-Diff Taxonomy

**Date:** 2026-04-11
**Adopted by:** [#515](https://github.com/vamseeachanta/digitalmodel/issues/515) on **2026-05-11** as the ratified classification policy for the OrcaFlex YAML semantic-equivalence claim boundary (`SEMANTIC_EQUIVALENCE_CLAIM_BOUNDARY.md`). The four Open Questions (OQ-1..OQ-4) in §7 close inline under #515 Approach A across iterations 4–6 of the implementation loop; see `SEMANTIC_EQUIVALENCE_CLAIM_BOUNDARY.md` §5.1 for per-OQ disposition.
**Issue:** #517 (parent: #515) — taxonomy authoring; #515 ratifies and closes the Open Questions.
**Scope:** Classification policy for differences between OrcaFlex strict YAML representations

## Purpose

This document defines the categories, decision rules, and comparison policy for
classifying differences between:

- **monolithic** OrcaFlex strict YAML (exported from `.dat` via OrcFxAPI)
- **generated** OrcaFlex strict YAML (produced by the modular generator from `spec.yml`)
- **extracted spec** YAML (reverse-extracted from monolithic via format converter)

Without this taxonomy, comparisons are ad hoc and the repo risks over-claiming
semantic equivalence. Every observed difference must map to exactly one category.

---

## 1. Taxonomy Categories

Six mutually exclusive impact categories, ordered from lowest to highest concern.

| # | Category | Code Label | Impact on Analysis | Action Required |
|---|----------|------------|-------------------|-----------------|
| C1 | **UI / Cosmetic** | `ui_cosmetic` | None | Document; exclude from significant-diff counts |
| C2 | **Normalization** | `normalization` | None (representational only) | Document; verify equivalence claim per property |
| C3 | **Known Intentional Omission** | `intentional_omission` | None (property is dormant or inapplicable) | Document reason; test that omission is deliberate |
| C4 | **Reference Resolution** | `reference_resolution` | None if resolved correctly; breaks model if wrong | Test cross-references; flag unresolved |
| C5 | **Loadability** | `loadability` | Model may fail to load in OrcaFlex | Fix in generator or document as known limitation |
| C6 | **Physics-Significant** | `physics_significant` | Analysis results change | Fix; block equivalence claim until resolved |

### Decision Rules

To classify an observed difference, apply these rules **in order** (first match wins):

```
1. Is the property in COSMETIC_PROPS?
   → C1: UI / Cosmetic

2. Is the difference only in representation (bool↔Yes/No, int↔float,
   None↔~, whitespace, string casing)?
   → C2: Normalization

3. Is the property in SKIP_KEYS and documented as dormant/invalid?
   → C3: Known Intentional Omission

4. Does the property define a cross-reference (connection name,
   object name, arc-length target)?
   → C4: Reference Resolution

5. Would the difference cause OrcaFlex to reject the YAML at load time
   ("Change not allowed", missing required key, schema violation)?
   → C5: Loadability

6. Otherwise (the property affects solver behavior):
   → C6: Physics-Significant
```

---

## 2. Category Definitions and Evidence

### C1: UI / Cosmetic (`ui_cosmetic`)

**Definition:** Properties that control only the OrcaFlex GUI display, 3D view
state, node rendering, or group tree appearance. They have zero effect on model
loading or solver computation.

**Decision test:** Changing the property value does not alter any OrcaFlex
calculation result and does not cause a load error.

**Canonical property list** (from `ALLOWED_DIFF_PROPS` in `scripts/semantic_validate.py`
and `_SKIP_GENERAL_KEYS` in `generic_builder.py`):

| Sub-group | Properties | Source |
|-----------|-----------|--------|
| View defaults | `DefaultViewAngle1`, `DefaultViewAngle2`, `DefaultViewCentre`, `DefaultViewSize`, `DefaultViewOrientation`, `DefaultViewResetWhenConnectedObjectMoved`, `DefaultViewDistortionX`, `DefaultViewDistortionY`, `DefaultViewDistortionZ`, `DefaultViewAzimuth`, `DefaultViewElevation`, `DefaultViewMode`, `DefaultShadedFillMode`, `DefaultShadedProjectionMode` | `_SKIP_GENERAL_KEYS` |
| Background | `BackgroundColour`, `WireframeMode` | `_SKIP_GENERAL_KEYS` |
| Sea/seabed rendering | `SeaSurfaceTranslucency`, `SeabedTranslucency`, `SeaSurfaceGridDensity`, `SeabedGridDensity`, `SeaSurfacePen` | `_SKIP_GENERAL_KEYS`, `ALLOWED_DIFF_PROPS` |
| Node/shaded drawing | `DrawNodes`, `DrawNodesSize`, `DrawNodesAsDiscs`, `DrawShaded`, `DrawShadedDiameter`, `DrawShadedSections`, `DrawShadedNodesAsSpheres`, `DrawNodeSymbol`, `DrawNodeSize`, `DrawShadedModel`, `ShadedDrawingCullingMode`, `DrawShadedSmoothShading`, `DrawShadedOnSeabed`, `DrawAxialColour`, `DrawShadedColour`, `DrawShadedFillColour`, `DrawShadedWallThickness`, `NodeColour`, `Colour`, `ShowDeflectedShape`, `PenWidth` | `ALLOWED_DIFF_PROPS` |
| Contact cosmetics | `ContactPen`, `ContactVisualisation` | `ALLOWED_DIFF_PROPS` |
| Group GUI state | `State` (collapsed/expanded), `Structure` (group membership) | `ALLOWED_DIFF_PROPS` |
| Model bookkeeping | `ModelState` | `_SKIP_GENERAL_KEYS`, `ALLOWED_DIFF_PROPS` |

**Observed examples:**
- `General.DefaultViewAzimuth`: 270 in monolithic, 0 in generated → C1
- `General.DefaultViewSize`: 400 in monolithic, absent in generated → C1
- `Groups.State`: `{Collapsed: []}` in monolithic, absent in generated → C1

---

### C2: Normalization (`normalization`)

**Definition:** The monolithic and generated values are semantically identical but
differ in YAML representation. No behavioral change.

**Decision test:** Both values, when loaded by OrcaFlex, produce the same
in-memory model state.

**Known normalization patterns:**

| Pattern | Monolithic | Generated | Mechanism |
|---------|-----------|-----------|-----------|
| Boolean encoding | `Yes` / `No` | `true` / `false` (or vice versa) | `yaml_utils.py:_represent_bool` emits `Yes`/`No`; raw YAML loaders may emit `true`/`false` |
| Int → float | `100` | `100.0` | Python/YAML numeric coercion |
| Null encoding | `~` | `null` or absent | `yaml_utils.py:_represent_none` emits `~` |
| String whitespace | `"Dean Stream "` | `"Dean Stream"` | Trailing whitespace stripped |
| Temperature units | `°C` | different encoding of `°` | Unicode vs ASCII representation (`TemperatureUnits`) |

**Policy:** Normalization differences are not counted as failures. The comparison
engine (`values_equal` in `semantic_validate.py`) already handles int↔float via
numeric tolerance and whitespace via `strip()`. Bool↔Yes/No should be handled
explicitly (see follow-up in #515 Phase 2).

**Observed examples:**
- `Environment.MultipleCurrentDataCanBeDefined`: `No` vs `false` → C2
- `Environment.WaterDepth`: `100` vs `100.0` → C2

---

### C3: Known Intentional Omission (`intentional_omission`)

**Definition:** A property present in monolithic YAML is deliberately not emitted
by the generator because:
- it is a **dormant property** (mode-dependent, not applicable to the current model
  configuration, and setting it causes "Change not allowed" at load time), or
- it is an **invalid/deprecated property** that OrcaFlex's YAML-strict schema
  rejects, or
- it is a **generator-policy exclusion** (e.g., Groups section omitted for
  generic-track models).

**Decision test:** The omission is present in a documented skip-list or policy,
and the reason is recorded.

**Canonical skip-lists:**

| Skip-List | Location | Count | Reason |
|-----------|----------|-------|--------|
| `_SKIP_GENERAL_KEYS` | `generic_builder.py:115-149` | 34 | View/display/dormant keys that cause "Change not allowed" |
| `_SKIP_OBJECT_KEYS` | `generic_builder.py:160-165` | 2 | Mode-dependent dormant properties on all object types |
| `_WIND_SPEED_DORMANT` | `environment_builder.py:160` | 1 | `WindSpeed` dormant when `WindType` = `Full field` |
| GroupsBuilder policy | `groups_builder.py:27-29` | — | Groups not generated for `is_generic()` models |

**Specific documented omissions:**

| Property | Section | Reason | First Documented |
|----------|---------|--------|-----------------|
| `ImplicitVariableMaxTimeStep` | General | OrcFxAPI internal; not in YAML-strict schema | SECTION_FIDELITY_ANALYSIS.md |
| `ApplySeabedContactLoadsAtCentreline` | (objects) | Dormant mode-dependent property | generic_builder.py |
| `SeabedDamping` | (objects) | Dormant mode-dependent property | generic_builder.py |
| `Groups` section (generic track) | Groups | Not generated for generic-track models — see C3 Groups policy below | groups_builder.py |
| `VerticalWindVariationFactor` | Environment | Conditional C3 — only emitted under spectrum-class WindTypes (API/NPD/ESDU); absent for `Constant` and `Full field` | environment_builder.py `_WIND_TYPE_PROPS` |

#### C3 sub-policy: `VerticalWindVariationFactor` (OQ-1 closure)

This property's emission is **WindType-conditional**, not unconditional. Per
`environment_builder._WIND_TYPE_PROPS`:

| `WindType` value | `VerticalWindVariationFactor` emitted? | Classification |
|---|---|---|
| `API spectrum` | yes | not a diff |
| `NPD spectrum` | yes | not a diff |
| `ESDU spectrum` | yes | not a diff |
| `Constant` | no | **C3** — property is dormant when wind is constant |
| `Full field` | no | **C3** — property is dormant when wind is from a full-field file |

If a monolithic model has `WindType: Constant` and emits
`VerticalWindVariationFactor`, the generated model legitimately drops it as
C3. If a monolithic model has `WindType: API spectrum` and the generator does
NOT emit `VerticalWindVariationFactor`, that is **C6** — a real generator
defect, not C3. The classification depends on the model's WindType context.

#### C3 sub-policy: `Groups` section (OQ-2 closure)

Generation is **builder-track-conditional** per `GroupsBuilder.should_generate()`:

| Builder track | `Groups` emitted? | Classification |
|---|---|---|
| Generic (`spec.is_generic()`) | no | **C3** — generic-track policy: Groups are a UI-organization construct without spec-side authoring; omitting them is intentional |
| Pipeline (`spec.is_pipeline()`) | yes (derived from context entity names) | partial — derived Groups may not match user-authored monolithic Groups; track via registry `known_diffs` |
| Riser (`spec.is_riser()`) | yes (derived from context entity names) | partial — same as pipeline track |

Generic-track Groups absence is a C3 intentional omission. Pipeline / riser
derivation is a partial match by construction — the generator builds Groups
from the spec's known entities, which can never reconstruct a user's hand-
authored grouping (e.g. logical clusters spanning unrelated objects). Per-
family registry entries MUST list residual Groups differences under
`known_diffs` when the model is on the pipeline or riser track and the
monolithic Groups carry user-authored content.

**Observed examples:**
- `General.ImplicitVariableMaxTimeStep`: present in monolithic, rejected by
  YAML-strict validator → intentionally skipped → C3
- `Groups` section: present in monolithic, absent in generated (generic track) → C3

---

### C4: Reference Resolution (`reference_resolution`)

**Definition:** A property that names another object in the model (connection
endpoint, line type reference, vessel reference, constraint reference). If the
reference is resolved correctly, there is no behavioral difference. If wrong or
missing, the model may fail to load or produce incorrect results.

**Decision test:** The property value must match a `Name` field of an object
defined elsewhere in the model. Validated by the post-validator's object registry.

**Known reference types:**

| Property Pattern | Example | Validator Check |
|-----------------|---------|-----------------|
| Connection names | `EndAConnection: "FPSO"` | `_BUILTIN_CONNECTIONS` + object registry |
| LineType references | `LineType: "10in_product"` | LineTypes name list |
| Constraint connections | `Connection: "10\" riser S clamp"` | Constraint name registry |
| OutFrameConnection | `OutFrameConnection: "Line1 arc-length"` | Object + arc-length pattern |

**Observed failure:** Post-validator's `_BUILTIN_CONNECTIONS` did not include
constraint names, causing 34 false-positive errors across 7 specs
(SECTION_FIDELITY_ANALYSIS.md, Post-Validation Failures section).

**Policy:** Reference differences are always significant. They must either match
exactly or be validated against the full object registry. No tolerance or
normalization applies.

---

### C5: Loadability (`loadability`)

**Definition:** A difference that causes OrcaFlex to reject the YAML file at load
time. The model cannot be instantiated.

**Decision test:** `OrcFxAPI.Model().LoadData(path)` raises an error.

**Known loadability hazards:**

| Hazard | Mechanism | Mitigation |
|--------|-----------|-----------|
| Dormant property set | Setting a mode-dependent property when mode doesn't allow it → "Change not allowed" | `_SKIP_GENERAL_KEYS`, `_SKIP_OBJECT_KEYS` |
| Deprecated property | `ImplicitVariableMaxTimeStep` → rejected by YAML-strict | `_SKIP_GENERAL_KEYS` |
| Missing required property | Omitting a property OrcaFlex expects for the current mode | Builder typed-field coverage |
| Schema validation | Pydantic field constraint failure (e.g., `shear: ~` when `ge=0` required) | Schema defaults (SECTION_FIDELITY_ANALYSIS.md fix) |

**Observed examples:**
- `ImplicitVariableMaxTimeStep` emitted → YAML-strict rejection → C5 (now fixed via skip-list → C3)
- `equipment.ramp` vs `equipment.ramps` attribute error → generation failure → C5 (now fixed)

**Policy:** Any loadability difference is a blocking defect. It must be fixed in
the generator or documented as a known unsupported model configuration.

---

### C6: Physics-Significant (`physics_significant`)

**Definition:** A difference that changes the physical behavior of the model:
solver results (tensions, bending moments, displacements, etc.) will differ beyond
engineering tolerance.

**Decision test:** Running statics/dynamics on both models produces results that
differ beyond the benchmark tolerance:
- Tension: 5% relative OR 10 kN absolute
- Bending moment: 15% relative OR 5 kN.m absolute

**Known physics-significant property families:**

| Section | Property Family | Impact |
|---------|----------------|--------|
| Environment | `WaterDepth`, `WaveHeight`, `WavePeriod`, `CurrentSpeed`, `CurrentProfile` | Hydrodynamic loads |
| Environment | `SeabedStiffness`, `SeabedModel` | Soil-structure interaction |
| Environment | `VerticalWindVariationFactor` | Wind load distribution |
| LineTypes | `OD`, `ID`, `MassPerUnitLength`, `EA`, `EI`, `GJ`, `Cd`, `Ca` | Structural/hydro response |
| Lines | `Length`, `SegmentLength`, `EndAConnection`, `EndBConnection` | Geometry and boundary conditions |
| Vessels | `Position`, `Orientation`, `PrimaryMotion` | Excitation definition |
| General | `StageDuration`, `ImplicitConstantTimeStep` | Solver time-stepping |

**Observed examples:**
- `Environment.VerticalWindVariationFactor`: present in monolithic, missing in
  generated → potentially C6 (needs classification: is the default identical to
  the monolithic value?)

**Policy:** Physics-significant differences block any equivalence claim for the
affected model. They must be resolved by fixing the generator or updating the spec.

---

## 3. Diff Mechanism vs Impact Category (Cross-Reference)

The comparison engine (`semantic_validate.py`) detects differences by *mechanism*.
The taxonomy classifies them by *impact*. This table maps between the two:

| Diff Mechanism (code) | Possible Impact Categories | Resolution |
|-----------------------|---------------------------|------------|
| `MATCH` | — | No action |
| `COSMETIC` (< 0.01% numeric) | C1 or C2 | Check if property is in `COSMETIC_PROPS`; if not, classify by property family |
| `MINOR` (0.01–1% numeric) | C2 or C6 | Review: is the difference from int↔float coercion (C2) or a real value change (C6)? |
| `SIGNIFICANT` (> 1% numeric) | C5 or C6 | Always investigate |
| `TYPE_MISMATCH` | C2 or C6 | Check if bool↔string normalization (C2) or genuine type error (C6) |
| `MISSING` (in monolithic, not in generated) | C1, C3, or C6 | Check skip-lists (C3), cosmetic list (C1); otherwise C6 |
| `EXTRA` (in generated, not in monolithic) | C2 or C5 | Generator emitting a default not in original; usually harmless but verify |

---

## 4. Comparison Policy for Tests and Reports

### 4.1 Equivalence Claim Levels

The repo supports three levels of equivalence claim, from weakest to strongest:

| Level | Name | Requirement |
|-------|------|------------|
| L1 | **Loadable** | Generated YAML loads without error in OrcaFlex |
| L2 | **Behaviorally equivalent** | Statics/dynamics results match within benchmark tolerance (no C6 diffs) |
| L3 | **Semantically identical** | No differences except C1 (cosmetic) and C2 (normalization) |

**Current repo status:** Most validated models achieve **L2**. No model has been
proven to achieve L3 across all sections. The repo should claim L2 where
benchmarked and explicitly disclaim L3 unless proven per-model.

### 4.2 Test Assertions

Tests should use this decision table:

| Category | Test Behavior |
|----------|--------------|
| C1: UI/Cosmetic | Excluded from `has_significant_diffs`; reported but not failed |
| C2: Normalization | Excluded from `has_significant_diffs`; reported but not failed |
| C3: Intentional Omission | Excluded from `has_significant_diffs` **only if** the property is in a documented skip-list; otherwise fails |
| C4: Reference Resolution | Always checked; any mismatch fails the test |
| C5: Loadability | Fails the test; blocks model generation |
| C6: Physics-Significant | Fails the test; blocks equivalence claim |

### 4.3 Reporting Format

The `semantic_validate.py` script should report each section's diff count
broken down by impact category:

```
Section: Environment
  Total properties: 45
  Matches: 40
  C1 (cosmetic): 0
  C2 (normalization): 3  [WaterDepth: 100→100.0, ...]
  C3 (intentional omission): 1  [VerticalWindVariationFactor]
  C6 (physics-significant): 1  [WaveHeight: 6.0→5.8]
  Verdict: SIGNIFICANT DIFFS (1 C6)
```

---

## 5. Property Classification Registry

This section provides the machine-usable mapping. The canonical source for each
set is the Python constant referenced; this table is a human-readable summary.

### 5.1 Cosmetic Properties (`COSMETIC_PROPS`)

**Canonical source:** `scripts/semantic_validate.py::ALLOWED_DIFF_PROPS`

All 50+ properties listed in `ALLOWED_DIFF_PROPS` are classified as **C1**.
The set should be importable by tests and reporting code.

### 5.2 Intentional Omission Keys

**Canonical sources:**
- `generic_builder.py::_SKIP_GENERAL_KEYS` (34 keys)
- `generic_builder.py::_SKIP_OBJECT_KEYS` (2 keys)
- `environment_builder.py::_WIND_SPEED_DORMANT` (1 key)

Properties in these sets are classified as **C3** when they appear in monolithic
YAML but not in generated YAML.

### 5.3 Normalization Patterns

**Canonical source:** `yaml_utils.py` (bool and null representers)

These are detected by `values_equal()` tolerance logic, not by property name.
Classification rule: if two values differ only by representation and the
tolerance check passes, the diff is **C2**.

### 5.4 Unclassified Properties

Any property not in the above sets that shows a difference defaults to **C6**
(physics-significant) until explicitly classified otherwise. This is the
conservative default.

---

## 6. Current Observed Diffs by Category

Based on the `a01_catenary_riser` comparison evidence (issue #515):

| Section | Property | Monolithic | Generated | Category | Notes |
|---------|----------|-----------|-----------|----------|-------|
| General | `DefaultViewMode` | `Single` | absent | C1 | View state |
| General | `DefaultShadedFillMode` | `Fill and wireframe` | absent | C1 | View state |
| General | `DefaultShadedProjectionMode` | `Perspective` | absent | C1 | View state |
| General | `DefaultViewSize` | `400` | absent | C1 | View state |
| General | `DefaultViewCentre` | `[0, -48, -50]` | absent | C1 | View state |
| General | `DefaultViewAzimuth` | `270` | absent | C1 | View state |
| General | `DefaultViewElevation` | `10` | absent | C1 | View state |
| Environment | bool fields | `Yes`/`No` | `true`/`false` | C2 | Bool encoding |
| Environment | numeric fields | int | float | C2 | Numeric coercion |
| Environment | `VerticalWindVariationFactor` | present | absent | **C6 or C3** | **Needs classification** (see open question OQ-1) |
| Groups | entire section | present | absent | C3 | Generic-track policy |

---

## 7. Open Questions

**OQ-1: `VerticalWindVariationFactor` classification.** — **RESOLVED 2026-05-11 (#515 iter 5/7)**
Classified as **conditional C3** per the §C3 sub-policy table above:
- Emitted when `WindType ∈ {API spectrum, NPD spectrum, ESDU spectrum}` → not a diff
- Omitted when `WindType ∈ {Constant, Full field}` → C3 intentional omission
- If `WindType` is spectrum-class AND property is missing in generated → C6 defect
Enforced by `tests/solvers/orcaflex/test_oq1_oq2_classifications.py::TestOQ1WindFactorClassification`.

**OQ-2: `Groups` section for pipeline/riser tracks.** — **RESOLVED 2026-05-11 (#515 iter 5/7)**
Classified as **builder-track-conditional C3** per the §C3 sub-policy table above:
- Generic track → omitted → C3 intentional (UI-organization construct)
- Pipeline / riser tracks → derived from context entity names → may not match
  user-authored monolithic Groups; residual differences tracked per-family in
  `MODEL_CLAIM_REGISTRY.yaml::known_diffs` rather than as taxonomy-level diffs.
Enforced by `tests/solvers/orcaflex/test_oq1_oq2_classifications.py::TestOQ2GroupsPolicy`.

**OQ-3: Environment builder hardcoded defaults.**
`environment_builder.py` defines 21 defaults (e.g., `KinematicViscosity: 1.35e-06`,
`SeaTemperature: 10`). Are these identical to OrcaFlex's own defaults? If a
monolithic model has different values for these properties, the generated model
will silently substitute the hardcoded default.
**Follow-up:** #515 Phase 3.

**OQ-4: Bool↔Yes/No normalization in comparison engine.**
The `values_equal()` function does not currently equate `true`↔`Yes` or
`false`↔`No`. This causes false-positive SIGNIFICANT diffs for C2 normalization.
**Follow-up:** #515 Phase 2 (semantic diff tests).

---

## 8. References

| Artifact | Path | Relationship |
|----------|------|-------------|
| Section fidelity analysis | `docs/domains/orcaflex/SECTION_FIDELITY_ANALYSIS.md` | Builder coverage matrix |
| Modular generator methods | `docs/domains/orcaflex/MODULAR_GENERATOR_METHODS.md` | Architecture and data flow |
| Semantic validator | `scripts/semantic_validate.py` | Comparison engine implementing this taxonomy |
| Generic builder skip-lists | `src/.../builders/generic_builder.py` | `_SKIP_GENERAL_KEYS`, `_SKIP_OBJECT_KEYS` |
| Environment builder | `src/.../builders/environment_builder.py` | `_DEFAULTS`, `_SAFE_RAW_OVERLAY_KEYS` |
| Groups builder | `src/.../builders/groups_builder.py` | Groups generation policy |
| YAML utilities | `src/.../orcaflex/yaml_utils.py` | Bool/null normalization |
| Semantic roundtrip tests | `tests/.../test_semantic_roundtrip.py` | `TestSemanticExclusionList` |
| Roundtrip fidelity tests | `tests/.../test_roundtrip_fidelity.py` | Property merge verification |
| Parent issue | #515 | Semantic-equivalence gap closure |
| This issue | #517 | Taxonomy definition |
