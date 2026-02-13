---
title: "WRK-128: Property Routing from Engineering Specs to OrcaFlex Objects"
description: "Phase 1 (Object Inventory) + Phase 2 (Routing Documentation + Mooring Router Prototype)"
version: "1.0"
module: orcaflex/property_routing
session:
  id: "20260212_wrk128"
  agent: "claude-opus-4-6"
review: pending
---

# WRK-128: Property Routing — Phases 1 & 2

## Context

The modular generator has 22 builders that convert spec.yml → OrcaFlex YAML, but the property routing logic is **implicit** — scattered across builders with no central inventory or documented mapping. Engineers cannot easily answer "what properties does a mooring line need?" or "how does coating thickness become OD?". Additionally, mooring models currently only work as generic pass-through (no domain-specific schema or transformation).

This plan delivers:
1. A machine-readable **object type inventory** cataloging all OrcaFlex properties with metadata
2. **Routing documentation** for 3 key paths (pipeline, mooring, vessel)
3. A **working mooring router** with Pydantic schema and unit tests

## Scope Boundaries

**In scope**: Inventory YAML, routing docs, MooringSpec schema, MooringRouter with unit tests
**Out of scope**: Solver integration tests, CLI integration, VesselRouter implementation, changes to existing builders

---

## Phase 1: Object Type Inventory

### Deliverable
`docs/modules/orcaflex/property_routing/object_inventory.yaml`

### Approach
Auto-generate from existing code registries, then validate against actual spec.yml usage.

### Step 1.1: Inventory Generation Script

**File**: `scripts/extract_property_inventory.py` (~250 lines)

Sources to parse (all in `src/digitalmodel/solvers/orcaflex/modular_generator/`):
- `schema/generic.py` → `SECTION_REGISTRY` (26 object types), `FIELD_TO_SECTION` (26 mappings), `TYPED_FIELD_MAP` (22 mappings), `SINGLETON_SECTIONS` (7 singletons)
- `builders/generic_builder.py` → `_PRIORITY_KEYS` (13), `_SKIP_GENERAL_KEYS` (15), `_SKIP_OBJECT_KEYS` (2), `_SECTION_ORDER` (33 sections)
- `builders/environment_builder.py` → `_SAFE_RAW_OVERLAY_KEYS`, `_WIND_TYPE_PROPS`, `_WIND_SPEED_DORMANT`, current mode gates, wave type gates
- `schema/riser.py` → `RiserLineType` fields (17 typed fields) — domain routing example

Script logic:
1. Import the registries directly (not regex parsing)
2. For each section in `SECTION_REGISTRY`: extract schema class fields + types
3. Cross-reference with skip/priority lists to classify properties
4. Walk all 63 spec.yml files → collect actual property usage per object type
5. Output structured YAML

Output schema:
```yaml
object_types:
  LineTypes:
    section_type: list
    schema_class: GenericLineType
    builders: [LineTypeBuilder, RiserLineTypeBuilder, GenericModelBuilder]
    typed_fields:
      name: {orcaflex_key: Name, type: str, required: true}
      outer_diameter: {orcaflex_key: OD, type: "float|str", required: false}
      # ...
    priority_keys: [Name, Category]
    skip_keys: [ApplySeabedContactLoadsAtCentreline]
    usage:
      spec_count: 58
      common_properties: [OD, EA, EI, MassPerUnitLength, Cd, Ca]
dormancy_rules:
  WaveGamma: {dormant_when: "WaveJONSWAPParameters == Automatic"}
  WindSpeed: {dormant_when: "WindType == Full field"}
  CurrentExponent: {dormant_when: "VerticalCurrentVariationMethod != Power law"}
```

### Step 1.2: Tests (TDD)

**File**: `tests/scripts/test_extract_property_inventory.py` (~100 lines, 8 tests)

```
test_inventory_covers_all_section_registry_types
test_priority_keys_marked_as_mode_setting
test_skip_general_keys_marked_as_dormant
test_typed_field_map_entries_have_schema_reference
test_dormancy_rules_include_wind_wave_current
test_usage_stats_from_spec_files
test_output_yaml_is_valid_and_loadable
test_singleton_sections_included
```

### Step 1.3: Manual Curation

Review auto-generated YAML:
- Add human-readable descriptions for top-20 properties
- Group into functional categories (geometry, structural, hydrodynamic, connection, mode-setting)
- Verify dormancy rules against known gotchas from `memory/orcaflex-gotchas.md`

---

## Phase 2A: Routing Documentation

### Deliverable
`docs/modules/orcaflex/property_routing/routing_map.md`

### Content (3 sections)

**Route A — Pipeline** (existing, document only):
- Source: `schema/pipeline.py` → `Pipeline.dimensions`, `Pipeline.coatings`, `Pipeline.material`
- Transform: `LineTypeBuilder` calculates OD from coating stack, EA/EI from material properties
- Output: `LineTypes`, `Lines`, `Environment` OrcaFlex sections
- Ref: `builders/linetype_builder.py`, `builders/lines_builder.py`

**Route B — Mooring Line** (new, prototype in Phase 2B):
- Source: `schema/mooring.py` (new) → chain/wire segment composition, anchor/fairlead positions, pretension
- Transform: `MooringRouter` looks up chain/wire material database → OrcaFlex properties
- Output: `generic` field → `GenericModelBuilder` → `LineTypes`, `Lines`, `Winches`

**Route C — Vessel Hydrodynamics** (future, document path only):
- Source: `hull_panel_catalog.yaml` hull_id + `DiffractionResults` from solver
- Transform: `VesselRouter` (future) links hull dims + RAO files → VesselType
- Output: `generic` field → `GenericModelBuilder` → `VesselTypes`, `Vessels`
- Existing: `hydrodynamics/diffraction/orcaflex_exporter.py` already exports VesselType YAML

**Architecture diagram** showing 3 routes converging on ProjectInputSpec.generic → GenericModelBuilder → OrcaFlex YAML.

---

## Phase 2B: Mooring Router Prototype

### Design Decision

Routers sit **upstream** of builders. They transform engineering specs into `ProjectInputSpec.generic`-compatible dicts, which flow through the existing `GenericModelBuilder`. This avoids duplicating builder logic.

```
MooringSpec → MooringRouter.route() → dict (generic-compatible)
                                        ↓
                           ProjectInputSpec(generic=GenericModel(**dict))
                                        ↓
                           GenericModelBuilder.build() → OrcaFlex YAML
```

### Files to Create

#### 1. Schema: `src/digitalmodel/solvers/orcaflex/modular_generator/schema/mooring.py` (~120 lines)

Following the `riser.py` pattern with domain-specific Pydantic models:

```python
class MooringSegmentType(str, Enum):
    CHAIN = "chain"
    WIRE_ROPE = "wire_rope"
    POLYESTER = "polyester"

class ChainGrade(str, Enum):
    R3 = "R3"
    R3S = "R3S"
    R4 = "R4"
    R4S = "R4S"
    R5 = "R5"

class MooringSegment(BaseModel):
    type: MooringSegmentType
    diameter: float  # m
    length: float    # m
    grade: ChainGrade | None = None        # for chain
    breaking_load: float | None = None     # kN (for wire/polyester)
    axial_stiffness: float | None = None   # kN (override if known)
    segment_length: float = 5.0            # FE mesh size (m)

class MooringEndpoint(BaseModel):
    type: Literal["anchor", "fairlead", "fixed"]
    position: list[float]  # [x, y, z] m
    vessel: str | None = None  # vessel name for fairlead

class MooringLine(BaseModel):
    name: str
    segments: list[MooringSegment]  # ordered from anchor to fairlead
    anchor: MooringEndpoint
    fairlead: MooringEndpoint
    pretension: float | None = None  # kN
    lay_azimuth: float | None = None # deg

class MooringSystem(BaseModel):
    lines: list[MooringLine]
```

#### 2. Router: `src/digitalmodel/solvers/orcaflex/modular_generator/routers/__init__.py` (empty)

#### 3. Router: `src/digitalmodel/solvers/orcaflex/modular_generator/routers/base_router.py` (~30 lines)

```python
class BaseRouter(ABC):
    @abstractmethod
    def route(self, spec: Any) -> dict[str, Any]:
        """Transform domain spec into generic-model-compatible dict."""
```

#### 4. Router: `src/digitalmodel/solvers/orcaflex/modular_generator/routers/mooring_router.py` (~200 lines)

```python
class MooringRouter(BaseRouter):
    def route(self, mooring: MooringSystem) -> dict[str, Any]:
        """Convert MooringSystem → GenericModel-compatible dict."""
        line_types = []
        lines = []
        winches = []

        for ml in mooring.lines:
            # Deduplicate line types by (type, diameter, grade)
            for seg in ml.segments:
                lt = self._segment_to_linetype(seg)
                if lt["name"] not in seen:
                    line_types.append(lt)

            # Build multi-section line
            line = self._build_line(ml)
            lines.append(line)

            # Pretension winch if specified
            if ml.pretension:
                winches.append(self._build_winch(ml))

        return {
            "line_types": line_types,
            "lines": lines,
            "winches": winches,
        }

    def _segment_to_linetype(self, seg: MooringSegment) -> dict
    def _chain_properties(self, diameter: float, grade: ChainGrade) -> dict
    def _wire_properties(self, diameter: float, breaking_load: float) -> dict
    def _build_line(self, ml: MooringLine) -> dict
    def _build_winch(self, ml: MooringLine) -> dict
```

Material lookup: Hardcoded `CHAIN_DATABASE` dict with 5 entries (R3/R3S/R4/R4S/R5 at common diameters). Follows DNV-OS-E302 / API 2SK simplified values. No external YAML file for now.

#### 5. Tests: `tests/solvers/orcaflex/modular_generator/routers/__init__.py` (empty)

#### 6. Tests: `tests/solvers/orcaflex/modular_generator/routers/test_mooring_router.py` (~180 lines, 10 tests)

```
test_single_chain_segment_produces_one_linetype
test_chain_wire_chain_produces_three_sections
test_linetype_deduplication_across_lines
test_chain_grade_r3_properties_from_database
test_wire_rope_properties_from_diameter_and_mbl
test_anchor_connection_maps_to_end_a_fixed
test_fairlead_connection_maps_to_vessel_reference
test_pretension_creates_winch
test_no_pretension_no_winch
test_output_dict_compatible_with_generic_model_schema
```

All tests are unit tests — no OrcFxAPI, no solver, dict assertions only.

---

## Execution Order

| Step | What | Files | Tests First |
|------|------|-------|-------------|
| 1 | Mooring schema | `schema/mooring.py` | Schema validation tests |
| 2 | Router base class | `routers/base_router.py` | N/A (abstract) |
| 3 | Mooring router | `routers/mooring_router.py` | 10 router tests |
| 4 | Inventory script | `scripts/extract_property_inventory.py` | 8 inventory tests |
| 5 | Inventory YAML | `docs/.../object_inventory.yaml` | Generated output |
| 6 | Routing docs | `docs/.../routing_map.md` | N/A (documentation) |

## Verification

1. `uv run python -m pytest tests/solvers/orcaflex/modular_generator/routers/ -v` — all 10 pass
2. `uv run python -m pytest tests/scripts/test_extract_property_inventory.py -v` — all 8 pass
3. `uv run python scripts/extract_property_inventory.py` — generates valid YAML
4. Existing tests unaffected: `uv run python -m pytest tests/solvers/orcaflex/modular_generator/ -v` — no regressions

## Critical Files Reference

**Read (existing)**:
- `src/digitalmodel/solvers/orcaflex/modular_generator/schema/generic.py` — SECTION_REGISTRY, FIELD_TO_SECTION, TYPED_FIELD_MAP
- `src/digitalmodel/solvers/orcaflex/modular_generator/schema/riser.py` — domain schema pattern to follow
- `src/digitalmodel/solvers/orcaflex/modular_generator/builders/generic_builder.py` — _PRIORITY_KEYS, _SKIP_*, _SECTION_ORDER
- `src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py` — dormancy rules
- `src/digitalmodel/solvers/orcaflex/modular_generator/builders/riser_linetype_builder.py` — domain builder pattern

**Create (new)**:
- `src/digitalmodel/solvers/orcaflex/modular_generator/schema/mooring.py`
- `src/digitalmodel/solvers/orcaflex/modular_generator/routers/__init__.py`
- `src/digitalmodel/solvers/orcaflex/modular_generator/routers/base_router.py`
- `src/digitalmodel/solvers/orcaflex/modular_generator/routers/mooring_router.py`
- `tests/solvers/orcaflex/modular_generator/routers/__init__.py`
- `tests/solvers/orcaflex/modular_generator/routers/test_mooring_router.py`
- `scripts/extract_property_inventory.py`
- `tests/scripts/test_extract_property_inventory.py`
- `docs/modules/orcaflex/property_routing/object_inventory.yaml`
- `docs/modules/orcaflex/property_routing/routing_map.md`
