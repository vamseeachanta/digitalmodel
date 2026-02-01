# S-Lay Pipeline Installation: Schema Extension + Spec Files

> Extend OrcaFlex modular generator schema for S-lay installations; create spec.yml files for SB-SA and RS8-ID pipelines

## Objective

Extend the `ProjectInputSpec` Pydantic schema to support S-lay (stinger-lay) pipeline installation alongside existing floating installation. Create two spec.yml input files for the PRPP project pipelines using data extracted from client reports.

## Pipeline Data (Both Pipelines)

| Parameter | Value | Source |
|-----------|-------|--------|
| OD | 273.1 mm (10-inch) | Concrete crush check PDF |
| Wall thickness | 14.3 mm | Concrete crush check PDF |
| Material | X-60 (Fy=415 MPa) | Concrete crush check PDF |
| Corrosion coating | Asphalt | Concrete crush check PDF |
| Concrete coating | 40mm+ | Concrete crush check PDF |
| Submerged weight | 1705.6 N/m | Route radius check PDF |
| Soil friction | 0.5 | Route radius check PDF |
| Stinger radius | 150 m | Concrete crush check PDF |

| | SB-SA | RS8-ID |
|---|---|---|
| Touchdown tension | 585.4 kN | 566.9 kN |
| Route radius | 1500 m | 1100 m |

**Eclipse Vessel**: LOA=130m, Beam=28m, Draft=4.0m, GMT=14.7m, COG=(68.6, 0, 9.6), Gyration=(10.3, 37.8, 37.8), 8-point mooring, 50MT tensioner.

**Missing (TBD)**: Water depth, seabed stiffness/slope, coating densities, pipeline lengths, current profiles, wind, exact roller positions, vessel position.

## Implementation Steps

### Step 1: Schema Extensions (equipment.py)

Add 7 new Pydantic models to `schema/equipment.py`:

```
VesselProperties  - loa, beam, depth, draft, displacement, gmt, cog, gyration_radii, rao_reference
VesselMooring     - drums, winch_capacity, pattern
Vessel            - name, properties, mooring, position, orientation
StingerSection    - length, bend_radius
StingerRoller     - arc_length, support_type, z_offset
Stinger           - radius, sections, rollers, origin_position, origin_orientation
Tensioner         - name, capacity_kn, stiffness, damping, position_on_vessel, tension_value
```

Extend `Equipment` class with 3 new optional fields:
```python
vessel: Vessel | None = Field(default=None)
stinger: Stinger | None = Field(default=None)
tensioner: Tensioner | None = Field(default=None)
```

All existing fields unchanged. Full backward compatibility.

### Step 2: Schema Cross-Validation (root.py)

Add to `validate_consistency`:
- Stinger requires vessel
- Tensioner requires vessel

Add helper methods:
- `is_s_lay() -> bool` (vessel is not None)
- `is_floating() -> bool` (tugs is not None)

### Step 3: Add X-60 Material (linetype_builder.py)

Add to `MATERIAL_PROPERTIES`:
```python
"X60": {"MaterialDensity": 7.85, "E": 207e6, "PoissonRatio": 0.293, "DNVSTF101Fy": 415e3}
```

### Step 4: Schema Exports (schema/__init__.py)

Export new models: `Vessel`, `VesselProperties`, `VesselMooring`, `Stinger`, `StingerSection`, `StingerRoller`, `Tensioner`.

### Step 5: New Builders (3 files)

**`builders/vessel_type_builder.py`** - Generates `VesselTypes` section:
- `should_generate()` returns False for floating models
- Builds vessel type with mass, inertia, RAO reference
- Registers `vessel_type_name` in context

**`builders/vessel_builder.py`** - Generates `Vessels` section:
- Builds vessel with support path (stinger geometry)
- Maps stinger rollers to support positions
- Registers `vessel_name` in context

**`builders/winch_builder.py`** - Generates `Winches` section:
- Builds tensioner as winch connecting vessel to pipeline End A
- Stage-controlled tension

### Step 6: Update Existing Builders

**`builders/context.py`** - Add fields: `vessel_type_names`, `vessel_names`, `main_vessel_name`, `winch_names`

**`builders/lines_builder.py`** - Conditional connection logic:
- Floating: End A Fixed, End B buoy
- S-lay: End A Free (on vessel), End B Anchored

**`builders/buoys_builder.py`** - Add `should_generate()` to skip tug/BM buoys for S-lay

**`builders/groups_builder.py`** - Include vessel and winch names in structure groups

**`builders/__init__.py`** - Import new builders

### Step 7: Create Spec Files

**`docs/modules/orcaflex/pipeline/installation/s-lay/SB-SA/spec.yml`**:
```yaml
metadata:
  name: "prpp_sb_sa_10in"
  description: "10-inch SB-SA pipeline S-lay installation from Eclipse"
  structure: pipeline
  operation: installation/s-lay
  project: "PRPP"

environment:
  water:
    depth: 20                    # TBD
    density: 1.025
  seabed:
    slope: 0                     # TBD
    stiffness: {normal: 100, shear: 10}  # TBD
  waves:
    type: airy
    height: 0                    # TBD - base case (0), dynamics use 0.25-1.75m
    period: 8
    direction: 0
  current:
    speed: 1.0
    direction: 0
    profile: [[0, 1.0]]         # TBD
  wind:
    speed: 0                     # TBD
    direction: 0

pipeline:
  name: "SB-SA Pipeline"
  material: X60
  dimensions:
    outer_diameter: 0.2731
    wall_thickness: 0.0143
  coatings:
    corrosion: {thickness: 0.005, density: 1.3}  # TBD exact
    weight:
      - {name: CWC40, thickness: 0.04, density: 3.04}  # TBD exact
  segments:
    - {type: X60+coating+CWC40, length: 1000, segment_length: 2.0}  # TBD length

equipment:
  vessel:
    name: "Eclipse"
    properties:
      loa: 130
      beam: 28
      depth: 9.5
      draft: 4.0
      gmt: 14.7
      cog: [68.6, 0.0, 9.6]
      gyration_radii: [10.3, 37.8, 37.8]
      rao_reference: "TBD"
    mooring: {drums: 8, winch_capacity: 735.75}
    position: [0, 0, 0]         # TBD
  stinger:
    radius: 150
    sections: [{length: 40, bend_radius: 150}]  # TBD
    rollers: []                  # TBD - exact positions from OrcaFlex model
    origin_position: [-30, 0, 7]  # TBD
    origin_orientation: [180, 0, 0]
  tensioner:
    capacity_kn: 490
    tension_value: 585.4         # From touchdown tension

simulation:
  time_step: 0.1
  stages: [8, 200]
  north_direction: 0            # TBD
```

**`docs/modules/orcaflex/pipeline/installation/s-lay/RS8-ID/spec.yml`** - Same pipeline spec, different metadata name and tensioner tension (566.9 kN).

### Step 8: Tests

**`tests/modules/orcaflex/modular_generator/test_slay_schema.py`**:
- Test all new Pydantic models validate correctly
- Test cross-validation (stinger requires vessel, tensioner requires vessel)
- Test backward compatibility (existing floating spec unchanged)
- Test `is_s_lay()` / `is_floating()` detection
- Test spec files load and validate

**`tests/modules/orcaflex/modular_generator/test_slay_builders.py`**:
- Test `should_generate()` for floating vs S-lay
- Test VesselTypeBuilder output structure
- Test VesselBuilder with stinger support path
- Test WinchBuilder tensioner connection
- Test LinesBuilder S-lay vs floating connection logic

## Critical Files

| Action | File |
|--------|------|
| Modify | `src/.../modular_generator/schema/equipment.py` |
| Modify | `src/.../modular_generator/schema/root.py` |
| Modify | `src/.../modular_generator/schema/__init__.py` |
| Modify | `src/.../modular_generator/builders/context.py` |
| Modify | `src/.../modular_generator/builders/linetype_builder.py` |
| Modify | `src/.../modular_generator/builders/lines_builder.py` |
| Modify | `src/.../modular_generator/builders/buoys_builder.py` |
| Modify | `src/.../modular_generator/builders/groups_builder.py` |
| Modify | `src/.../modular_generator/builders/__init__.py` |
| Create | `src/.../modular_generator/builders/vessel_type_builder.py` |
| Create | `src/.../modular_generator/builders/vessel_builder.py` |
| Create | `src/.../modular_generator/builders/winch_builder.py` |
| Create | `docs/.../s-lay/SB-SA/spec.yml` |
| Create | `docs/.../s-lay/RS8-ID/spec.yml` |
| Create | `tests/.../modular_generator/test_slay_schema.py` |
| Create | `tests/.../modular_generator/test_slay_builders.py` |

## Reference

Existing S-lay OrcaFlex models for structure reference:
- `docs/modules/orcaflex/pipeline/installation/s-lay/bigfoot1/WD_18m.yml`

## Verification

```bash
# Run all modular generator tests (existing + new)
cd /mnt/github/workspace-hub/digitalmodel
uv run pytest tests/modules/orcaflex/modular_generator/ -v

# Validate spec files load
uv run python -c "
from digitalmodel.modules.orcaflex.modular_generator.schema import ProjectInputSpec
import yaml
for name in ['SB-SA', 'RS8-ID']:
    path = f'docs/modules/orcaflex/pipeline/installation/s-lay/{name}/spec.yml'
    with open(path) as f:
        data = yaml.safe_load(f)
    spec = ProjectInputSpec(**data)
    print(f'{name}: {spec.metadata.name} - is_s_lay={spec.is_s_lay()}')
"

# Generate modular model from spec
uv run python -m digitalmodel.modules.orcaflex.modular_generator generate \
    --input docs/modules/orcaflex/pipeline/installation/s-lay/SB-SA/spec.yml \
    --output /tmp/slay_test/

# Verify existing floating spec still works
uv run python -m digitalmodel.modules.orcaflex.modular_generator generate \
    --input docs/modules/orcaflex/pipeline/installation/floating/30in_pipeline/spec.yml \
    --output /tmp/floating_test/
```
