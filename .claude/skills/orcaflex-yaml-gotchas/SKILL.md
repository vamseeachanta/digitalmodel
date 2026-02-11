---
name: orcaflex-yaml-gotchas
description: Production-proven OrcaFlex YAML traps and solutions covering dormant properties, boolean mismatches, section ordering, Pydantic integration, and section name aliases.
version: 1.0.0
updated: 2026-02-10
category: offshore-engineering
triggers:
- OrcaFlex YAML error
- Change not allowed OrcaFlex
- OrcaFlex property error
- dormant property
- OrcaFlex section order
- Pydantic OrcaFlex
- OrcaFlex YAML gotcha
- OrcaFlex SaveData
---
# OrcaFlex YAML Gotchas

Production-proven traps encountered when programmatically generating, parsing, and round-tripping OrcaFlex YAML files. Each gotcha includes the symptom, root cause, and verified fix.

## Critical: Dormant Properties from SaveData()

### Symptom
```
Error: Change not allowed for property 'SeabedDamping'
Error: Change not allowed for property 'WaveGamma'
```

### Root Cause
`OrcFxAPI.Model.SaveData()` exports ALL properties, including **dormant** ones that are only valid in certain modes. When re-loaded in YAML, properties may be set in a different order than the mode-setting property, causing "Change not allowed" errors.

Mode-setting properties that gate other properties:
- `MultipleCurrentDataCanBeDefined` → gates current data properties
- `CurrentModel` → gates current interpolation properties
- `WindType` → gates wind profile properties
- `SeabedModel` → gates seabed damping properties
- `WaveType` → gates WaveGamma (only valid for JONSWAP)

### Fix
**Never blindly pass through raw SaveData() properties.** Use hardcoded safe defaults as the base layer, then overlay only the specific properties you need:

```python
_DEFAULTS = {
    "SeabedModel": "Elastic",          # Safe default
    "MultipleCurrentDataCanBeDefined": False,
    "CurrentModel": "Variation scheme",
    "WindType": "Constant",
    # ... other safe defaults
}

def build(self):
    environment = dict(self._DEFAULTS)
    environment["Density"] = spec.water.density  # Overlay specific values
    return {"Environment": environment}
```

## Boolean Type Mismatch

### Symptom
Semantic validation reports type_mismatch: `True` vs `"Yes"`, `False` vs `"No"`.

### Root Cause
OrcaFlex `SaveData()` exports Python booleans (`True`/`False`). Builder code may use OrcaFlex string equivalents (`"Yes"`/`"No"`). Both work in OrcaFlex, but they serialize differently in YAML and fail semantic comparison.

### Fix
Use Python booleans in builder defaults, not OrcaFlex strings:

```python
# WRONG
"IncludeVesselWindLoads": "Yes",
"CurrentRamped": "No",

# CORRECT
"IncludeVesselWindLoads": True,
"CurrentRamped": False,
```

## Section Name Aliases

### Symptom
Extractor doesn't find sections that exist in monolithic YAML (e.g., Groups, FrictionCoefficients).

### Root Cause
OrcaFlex uses different section names in YAML exports vs the API:

| YAML Export (SaveData) | API/Internal Name |
|------------------------|-------------------|
| `Groups` | `BrowserGroups` |
| `FrictionCoefficients` | `SolidFrictionCoefficients` |

### Fix
Use the YAML export name as the canonical key and add fallback aliases in the extractor:

```python
_SECTION_ALIASES = {
    "FrictionCoefficients": ["SolidFrictionCoefficients"],
    "Groups": ["BrowserGroups"],
}

def _extract_singleton(self, section_key):
    data = self._raw.get(section_key)
    if not data:
        for alias in _SECTION_ALIASES.get(section_key, []):
            data = self._raw.get(alias)
            if data:
                break
    ...
```

## Section Dependency Order

### Symptom
```
Error: 'Line Type1' is not a valid LineType name
```

### Root Cause
OrcaFlex YAML loading validates references sequentially. If a Line references a LineType that hasn't been defined yet, it fails.

### Fix
Emit sections in strict dependency order:

```
General → VariableData → ExpansionTables
→ RayleighDampingCoefficients → FrictionCoefficients → LineContactData
→ LineTypes → VesselTypes → ClumpTypes → StiffenerTypes → SupportTypes
→ Vessels → Lines → Shapes → 6DBuoys → 3DBuoys
→ Constraints → Links → Winches → FlexJoints
→ MultibodyGroups → BrowserGroups → Groups
```

In the builder, use `_SECTION_ORDER` list and `_order_sections()` to enforce this.

## Priority Keys Within Objects

### Symptom
```
Error: 'MaterialDensity' is not valid for Category 'General'
```

### Root Cause
Some OrcaFlex properties are only valid after a mode-setting property is set. If YAML keys are in wrong order, the mode isn't set when the dependent property is encountered.

### Fix
Emit mode-setting properties first within each object dict:

```python
_PRIORITY_KEYS = [
    "Name",
    "Category",        # LineTypes: General vs Homogeneous pipe
    "ShapeType",       # Shapes: Drawing, Elastic solid
    "Shape",           # Shapes: Block, Cylinder
    "BuoyType",        # 6DBuoys: Spar buoy, Lumped buoy
    "Connection",      # Various: affects sub-properties
    "LinkType",        # Links: Tether, Spring/damper
    "DegreesOfFreedomInStatics",  # 6DBuoys: must precede stiffness
]
```

## Pydantic v2 Silent Field Drop

### Symptom
OrcaFlex property uses its default value instead of the value from the spec/monolithic source. No error or warning.

### Root Cause
Pydantic v2 with `extra='ignore'` (default) silently drops fields that aren't declared on the model class. If `GenericBuoy6D` lacks a `mass` field, `mass=0.47` from the spec is silently dropped and OrcaFlex uses its default (30.0).

### Fix
**Every OrcaFlex property that goes through `TYPED_FIELD_MAP` must have a matching typed field on the relevant GenericObject subclass.** Common fields that need explicit declaration:

```python
class GenericBuoy6D(GenericObject):
    mass: float | None = Field(default=None)
    volume: float | None = Field(default=None)

class GenericVessel(GenericObject):
    length: float | None = Field(default=None)

class GenericShape(GenericObject):
    length: float | None = Field(default=None)
    mass: float | None = Field(default=None)
    volume: float | None = Field(default=None)
```

### Detection
Compare monolithic values against generated output. Any property that has the OrcaFlex default value instead of the expected value is a candidate for silent drop.

## model_fields_set for Explicitly-Set None

### Symptom
Monolithic has `Length: null` but modular output omits `Length` entirely. Semantic validator flags as "missing".

### Root Cause
Builder's `_merge_object()` skips typed fields with `None` value. But `None` was explicitly set by the extractor (the monolithic had `Length: null`). Default `None` (never set) and explicit `None` (set to null) are indistinguishable by value alone.

### Fix
Use Pydantic's `model_fields_set` to distinguish explicitly-set fields:

```python
explicitly_set = getattr(obj, "model_fields_set", set())

for py_field, ofx_key in TYPED_FIELD_MAP.items():
    value = getattr(obj, py_field, None)
    if value is not None:
        merged[ofx_key] = value
    elif py_field in explicitly_set:
        merged[ofx_key] = value  # Emit explicit None
```

## YAML Aliases Rejected

### Symptom
```
Error: Invalid YAML: unexpected alias
```

### Root Cause
When multiple objects share identical data (e.g., buoy vertex arrays), PyYAML default dumper uses YAML anchors (`&id001`) and aliases (`*id001`). OrcFxAPI's YAML parser doesn't support these.

### Fix
Use a custom dumper that disables aliases:

```python
class _NoAliasDumper(yaml.Dumper):
    def ignore_aliases(self, data):
        return True

yaml.dump(data, f, Dumper=_NoAliasDumper)
```

## VariableData Section Format

### Symptom
```
Error: 'VariableDataSources' is not recognized
```

### Root Cause
OrcaFlex expects `VariableData` (not `VariableDataSources`) with nested sub-categories:

```yaml
VariableData:
  Dragcoefficient:
    - Name: "Generic Drag"
      ...
  Linetypediameter:
    - Name: "Diameter Variation"
      ...
```

### Fix
Group variable data entries by `data_type` into nested categories.

## CurrentProfile Minimum 2 Levels

### Symptom
```
Error: NumberOfCurrentLevels must be >= 2
```

### Root Cause
OrcFxAPI requires at least 2 current depth levels, even if the profile is uniform.

### Fix
Pad single-point profiles with a second point:

```python
if len(profile) < 2:
    seabed_depth = spec.environment.water.depth
    last_factor = profile[0][1] if profile else 1.0
    profile.append([seabed_depth, last_factor, 0])
```

## Current Profile Factors > 2.0

### Symptom
Pydantic validation error: `CurrentFactor must be <= 2.0`.

### Root Cause
OrcaFlex allows current factors > 2.0 for current amplification scenarios. Overly restrictive validators reject valid configurations.

### Fix
Don't cap current factor validators at 2.0. OrcaFlex accepts any positive value.

## Case-Insensitive Name Collisions

### Symptom
```
Error: Name 'Tether' already exists
```

### Root Cause
OrcaFlex object names are case-insensitive. A LineType named "tether" and a Link named "Tether" collide.

### Fix
Use suffixes to disambiguate: LineType `"tether_LT"`, Link `"Tether"`.

## Wave-Type-Aware Property Emission

### Symptom
```
Error: Change not allowed for property 'WaveHeight'
```

### Root Cause
Different wave types use different property names:
- **Deterministic** (Dean stream, Airy, Stokes'): `WaveHeight` + `WavePeriod`
- **Spectral** (JONSWAP, Pierson-Moskowitz): `WaveHs` + `WaveTz`
- **Other** (User defined, No waves): no height/period

Setting `WaveHeight` on a spectral wave type causes "Change not allowed".

### Fix
Check wave type before emitting height/period:

```python
if wave_type in _DETERMINISTIC_WAVE_TYPES:
    wave_train["WaveHeight"] = waves.height
    wave_train["WavePeriod"] = waves.period
elif wave_type in _SPECTRAL_WAVE_TYPES:
    wave_train["WaveHs"] = waves.height
    wave_train["WaveTz"] = waves.period
```

## Category-Dependent Properties

### Symptom
```
Error: 'MaterialDensity' is not valid for this category
```

### Root Cause
LineType properties depend on `Category`:
- `Category: Homogeneous pipe` → uses `MaterialDensity`, `OD`, `ID`
- `Category: General` → uses `MassPerUnitLength`, `EI`, `EA`

### Fix
Set `Category` FIRST in the dict (via `_PRIORITY_KEYS`), then only emit category-appropriate properties.

## ClumpTypes vs 6DBuoys for Inline Attachments

### Symptom
Singular Jacobian error with many 6DBuoy attachments on a riser.

### Root Cause
6DBuoys are 6-DOF rigid bodies with their own equation of motion. Many 6DBuoys on a single line can cause solver instability. ClumpTypes are inline attachments that share the line's DOFs.

### Fix
Use ClumpTypes for buoyancy modules and distributed weights. Reserve 6DBuoys for independent floating bodies.

## NorthDirection Default Emission

### Symptom
Semantic validator reports `NorthDirection: 0` as "extra" in modular output.

### Root Cause
Builder unconditionally emits `NorthDirection: 0` (default). Monolithic doesn't include it (OrcaFlex assumes 0).

### Fix
Only emit when non-zero:

```python
if sim.north_direction:
    general["NorthDirection"] = sim.north_direction
```

## Related Skills

- [orcaflex-monolithic-to-modular](../orcaflex-monolithic-to-modular/SKILL.md) - Extraction pipeline
- [orcaflex-model-generator](../orcaflex-model-generator/SKILL.md) - Builder architecture
- [orcaflex-environment-config](../orcaflex-environment-config/SKILL.md) - Environment setup

## References

- Source: `src/digitalmodel/solvers/orcaflex/modular_generator/`
- Memory: `memory/semantic-validation.md`
- OrcaFlex YAML documentation

---

## Version History

- **1.0.0** (2026-02-10): Initial release consolidating all production-proven OrcaFlex YAML gotchas from modular generator development (10+ fixes across 5 models).
