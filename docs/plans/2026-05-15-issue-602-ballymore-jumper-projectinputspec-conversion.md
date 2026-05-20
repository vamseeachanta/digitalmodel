# Plan: digitalmodel #602 - Convert Ballymore jumper specs into generator-ready ProjectInputSpec models

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/602
**Status:** plan-review
**Tier:** T3
**Date:** 2026-05-15
**Parent/story:** #471
**Related:** #478, #506, #601, #603, #604

## Scope

Implement a conversion layer that reads the Ballymore jumper installation domain specs and builds a
valid `ProjectInputSpec` data structure for `ModularModelGenerator`. This is a focused slice of the
broader #478 integration goal and should align with #601's calculation/output key contract, but it
does not consume #601's persisted `line_sections.yml`, does not run licensed OrcaFlex, and does not
require OrcFxAPI on Linux.

The conversion should target the existing `generic:` model path, because the current
`ProjectInputSpec` root model accepts only one of `pipeline`, `riser`, `mooring`, or `generic`.

## Resource Intelligence Summary

### Live issue state

Verified from GitHub issue #602 on 2026-05-15:

- State: open
- Labels: `cat:engineering`
- Acceptance criteria require `ProjectInputSpec(**data)` validation and generator output for both
  Ballymore specs.

### Sources consulted

- `AGENTS.md` - repository test command convention is `PYTHONPATH=src uv run python -m pytest`.
- `docs/plans/2026-05-05-issue-478-orcaflex-modular-generator-integration.md` - broad integration
  intent, but it suggests top-level `vessels`/`analysis` fields that are not accepted by the current
  `ProjectInputSpec` root model.
- `docs/plans/2026-05-05-issue-506-passing-ship-jumper-installation-schemas.md` - adjacent domain
  schema plan; `src/digitalmodel/marine_ops/installation/jumper_schema.py` is not present at HEAD.
- `src/digitalmodel/solvers/orcaflex/modular_generator/schema/root.py:67-80` - root fields are
  `metadata`, `environment`, optional `pipeline`/`riser`/`mooring`/`generic`, `equipment`, and
  `simulation`.
- `src/digitalmodel/solvers/orcaflex/modular_generator/schema/root.py:82-118` - exactly one model
  type must be defined.
- `src/digitalmodel/solvers/orcaflex/modular_generator/schema/generic.py:395-480` - generic model
  supports `line_types`, `vessels`, `lines`, `constraints`, `winches`, and other pass-through
  OrcaFlex sections.
- `src/digitalmodel/solvers/orcaflex/modular_generator/schema/generic.py:338-367` maps
  `variable_data_sources` to OrcaFlex `VariableData`.
- `src/digitalmodel/solvers/orcaflex/modular_generator/schema/generic.py:469` defines
  `variable_data_sources: list[GenericVariableData]`.
- `src/digitalmodel/solvers/orcaflex/modular_generator/builders/generic_builder.py:218-223` emits
  `VariableData` when `generic.variable_data_sources` is populated.
- `docs/domains/orcaflex/library/templates/jumper_rigid_subsea/spec.yml:443-460` - the template's
  buoyancy and strake line types reference named coating/lining variable data sources
  `Insulation+Buoyancy` and `Insulation+Strakes`; a converter must emit those names too.
- `src/digitalmodel/solvers/orcaflex/modular_generator/schema/environment.py:71-301` - when metocean
  data is absent, schema defaults are explicit calm values: wave height 0 m, period 8 s, direction
  0 deg; current speed 0 m/s with profile `[[0, 1.0]]`; wind speed 0 m/s at 10 m.
- `src/digitalmodel/solvers/orcaflex/modular_generator/builders/generic_builder.py:168-247` -
  generic builder emits generic sections into `20_generic_objects.yml`.
- `src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py:73-86` - `from_spec()` accepts an
  in-memory `ProjectInputSpec`.
- `src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py:140-180` - `generate()` writes
  include files, `inputs/parameters.yml`, and `master.yml`.
- `docs/domains/orcaflex/library/templates/jumper_rigid_subsea/spec.yml:56-420` - working generic
  template for jumper line types and lines.
- `docs/domains/orcaflex/subsea/jumper/installation/ballymore_mf_plet/spec.yml`
- `docs/domains/orcaflex/subsea/jumper/installation/ballymore_plet_plem/spec.yml`
- `tests/solvers/orcaflex/modular_generator/test_jumper_plet_to_plem_semantic.py` - existing
  generator semantic proof for jumper generic output.
- `docs/plans/README.md` - absent in this repository; follow the existing standalone plan-file
  convention.

### Reproduction Evidence

Command run on 2026-05-15 America/Chicago from `/mnt/local-analysis/workspace-hub/digitalmodel`:

```bash
uv run python - <<'PY'
from pathlib import Path
import shutil
import yaml
from pydantic import ValidationError
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec
from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
from digitalmodel.marine_ops.installation.jumper_installation import run_pipeline

spec_paths = [
    Path('docs/domains/orcaflex/subsea/jumper/installation/ballymore_mf_plet/spec.yml'),
    Path('docs/domains/orcaflex/subsea/jumper/installation/ballymore_plet_plem/spec.yml'),
]

for spec_path in spec_paths:
    data = yaml.safe_load(spec_path.read_text())
    try:
        ProjectInputSpec(**data)
        print(f'{spec_path}: ProjectInputSpec PASS')
    except ValidationError as exc:
        first_error = exc.errors()[0]
        print(f'{spec_path}: ProjectInputSpec FAIL: {first_error["msg"]}')

ref = Path('docs/domains/orcaflex/library/templates/jumper_rigid_subsea/spec.yml')
ref_spec = ProjectInputSpec(**yaml.safe_load(ref.read_text()))
print(f'{ref}: ProjectInputSpec PASS generic={ref_spec.is_generic()} line_types={len(ref_spec.generic.line_types)} lines={len(ref_spec.generic.lines)}')
out = Path('/tmp/jumper-reference-template-generate')
if out.exists():
    shutil.rmtree(out)
ModularModelGenerator.from_spec(ref_spec).generate(out)
includes = sorted(p.name for p in (out / 'includes').glob('*.yml'))
print(f'{ref}: generate PASS master_exists={(out / "master.yml").exists()} includes={includes}')
PY
```

Tail of output:

```text
docs/domains/orcaflex/subsea/jumper/installation/ballymore_mf_plet/spec.yml: ProjectInputSpec FAIL: Value error, Either 'pipeline', 'riser', 'mooring', or 'generic' must be defined in spec
docs/domains/orcaflex/subsea/jumper/installation/ballymore_plet_plem/spec.yml: ProjectInputSpec FAIL: Value error, Either 'pipeline', 'riser', 'mooring', or 'generic' must be defined in spec
docs/domains/orcaflex/library/templates/jumper_rigid_subsea/spec.yml: ProjectInputSpec PASS generic=True line_types=4 lines=3
docs/domains/orcaflex/library/templates/jumper_rigid_subsea/spec.yml: generate PASS master_exists=True includes=['01_general.yml', '03_environment.yml', '20_generic_objects.yml']
```

## Deliverable

A new `jumper_to_modular_spec` conversion module produces an in-memory dict accepted by
`ProjectInputSpec(**data)` for both Ballymore jumper specs, and `ModularModelGenerator.from_spec()`
can generate `master.yml`, `inputs/parameters.yml`, and the expected include files from it.

## Artifact Map

| Artifact | Path |
|---|---|
| Plan | `docs/plans/2026-05-15-issue-602-ballymore-jumper-projectinputspec-conversion.md` |
| Plan index | N/A - `digitalmodel/docs/plans/README.md` does not exist |
| Review artifacts | `scripts/review/results/2026-05-15-plan-602-{claude,codex,gemini}.md` |
| Conversion module | `src/digitalmodel/marine_ops/installation/jumper_to_modular_spec.py` |
| Focused tests | `tests/solvers/orcaflex/modular_generator/test_jumper_modular_spec.py` |
| Optional documentation | `docs/domains/orcaflex/subsea/jumper/installation/README.md` or module docstring |

## Plan

1. Add RED tests before creating the conversion module:
   - `test_build_modular_spec_validates_for_ballymore_specs`
   - `test_build_modular_spec_preserves_27_line_sections`
   - `test_modular_generator_writes_master_and_includes_for_ballymore_specs`
   - `test_ballymore_specs_are_distinct_or_baseline_geometry_is_explicitly_marked`
2. Create `src/digitalmodel/marine_ops/installation/jumper_to_modular_spec.py` with:
   - `build_modular_spec(spec_path: str | Path) -> dict`
   - `build_conversion_provenance(spec_path: str | Path, modular_data: dict) -> dict`
   - internal helpers for environment mapping, simulation defaults, line-type definitions, and
     line-section row conversion.
   - no OrcFxAPI imports.
3. Load the domain spec through existing `load_spec()` until #506 lands. Use `parse_jumper_config()`
   only for the `jumper` geometry fields it actually reads; explicitly consume the source spec's
   `pipe`, `buoyancy_modules`, `strake_modules`, `rigging`, and `crane_configuration` sections when
   deriving line-type properties, connector/clamp metadata, and conversion provenance. Do not require
   the missing `JumperInstallationSpec` schema at implementation time.
4. Build the root dict with only schema-accepted top-level keys:

   ```python
   {
       "metadata": {...},
       "environment": {...},
       "simulation": {...},
       "generic": {
           "line_types": [...],
           "lines": [...],
           "variable_data_sources": [...],
       },
   }
   ```

5. Derive line types using the working generic template as the contract:
   - `10.75"Jumper_wCoat`
   - `10.75"Jumper_wCoat_wBuoy`
   - `10.75"Jumper_wCoat_wStrake`
   - `OCS 200-V`
   - include the named `variable_data_sources` required by the buoyancy and strake `CoatingThickness`
     values; the buoyancy line type must set `CoatingThickness: "Insulation+Buoyancy"` and the strake
     line type must set `CoatingThickness: "Insulation+Strakes"`. Do not replace those references
     with numeric coating properties in this issue.
6. Convert the computed 27 sections into the compact OrcaFlex multi-column line table inside a
   `generic.lines` entry. The current computed section dicts contain only `name`, `line_type`, and
   `length_m`, so the converter must derive the additional OrcaFlex row fields explicitly:
   - `TargetSegmentLength`: deterministic placeholder meshing rule, tested by section category:
     0.2 m for bend rows, 0.4 m for connector rows, 1.0 m for buoy/strake rows, and 0.5 m for other
     short straight/transition rows unless a future source value is added. This is a generator
     completeness assumption, not licensed OrcaFlex mesh validation.
   - `PreBendCurvaturex`: 0 for all current Ballymore rows unless source data adds out-of-plane
     curvature.
   - `PreBendCurvaturey`: nonzero for bend rows using `1 / bend_radius_m` with this provisional
     Ballymore sign map, derived from the existing rigid-jumper template pattern and recorded in
     provenance as an assumption until workbook-derived route geometry is available:
     `A-B bend=-k`, `B-C bend=-k`, `C-D bend=+k`, `D-E bend=+k`, `E-F bend=-k`, `F-G bend=-k`,
     where `k = 1 / bend_radius_m`. Unknown bend names or non-positive bend radius must raise a
     clear `ValueError` rather than silently emitting straight spans.
   - Section names must match the exact `stage_2_calculate()`/`compute_orcaflex_sections()` names:
     `Connector-start`, `A-straight`, `A-B bend`, `B-straight`, `B-C bend`, `C-bare-start`,
     `C-strake-1`, `C-bare-mid`, `C-buoy`, `C-bare-remaining`, `C-strake-2`, `C-bare-end`,
     `C-D bend`, `D-bare-1`, `D-buoy-10a`, `D-bare-centre`, `D-buoy-10b`, `D-bare-2`,
     `D-E bend`, `E-bare-start`, `E-buoy`, `E-bare-end`, `E-F bend`, `F-straight`, `F-G bend`,
     `G-straight`, `Connector-end`. Do not normalize alternate spellings in this issue.
   Tests must fail if bend rows are emitted as a fully straight line.
   The converter must get those 27 sections by loading the domain spec with
   `stage_1_load_spec(str(spec_path))`, running `stage_2_calculate(spec)`, and consuming
   `results["orcaflex_sections"]`; it must not depend on #601's persisted `line_sections.yml` file.
   If the key is missing or the count is not 27, raise `ValueError` naming the spec path and observed
   count/key state.
7. Map Ballymore environment data into `ProjectInputSpec.environment`:
   - water depth/density from `environment.water`.
   - seabed stiffness where available.
   - if the domain spec stores metocean under `environment.metocean`, explicitly translate wave
     `significant`/`period`/`direction` and wind `design_speed` to the schema's `waves` and `wind`
     fields; use calm current defaults unless source current data exists.
   - if metocean is absent, set and assert explicit calm defaults rather than relying on implicit
    Pydantic defaults: wave height 0 m, period 8 s, direction 0 deg; current speed 0 m/s with
    profile `[[0, 1.0]]`; wind speed 0 m/s, direction 0 deg, reference height 10 m.
8. Add generator smoke tests that instantiate `ProjectInputSpec(**data)`, call
   `ModularModelGenerator.from_spec(spec).generate(tmp_path)`, then parse `master.yml`,
   `inputs/parameters.yml`, and every generated include with `yaml.safe_load()`.
   Also parse `master.yml` to assert every listed include exists, and inspect
   `includes/20_generic_objects.yml` to assert named coating/lining references resolve to generated
   `VariableData` entries. The generated `Lines` table must be under `Lines[].LineType, Length,
   TargetSegmentLength, PreBendCurvaturex, PreBendCurvaturey`; tests must assert each of the 27 rows
   has exactly five columns and that every first-column line type exists in generated `LineTypes`.
9. Add mutation tests for source sections outside `jumper` so the converter cannot silently ignore
   the domain spec. The mutation tests must copy each source `spec.yml` to `tmp_path`, modify the
   temporary YAML with `yaml.safe_load()`/`yaml.safe_dump()`, then call `build_modular_spec(tmp_spec)`;
   they must not modify the repository fixtures in place:
   - changing `pipe.outer_diameter` or insulation density changes corresponding line-type fields.
   - changing `buoyancy_modules`/`strake_modules` layer values changes the named variable data table.
   - changing `pipe.bend_radius` changes bend-row `PreBendCurvaturey` magnitudes.
   - changing `crane_configuration` is either reflected in generated metadata/provenance or explicitly
     documented as not used by the current generic model.
10. Document the conversion field map in a module docstring or nearby domain README. Include the fact
   that `.dat`/`.sim` load proof remains #604.

## Required Schema Input Shape

The converter must build this ProjectInputSpec input shape, not OrcaFlex output YAML directly:

```python
{
    "metadata": {
        "name": "ballymore_mf_plet_jumper",
        "description": "Ballymore Manifold-to-PLET Jumper Installation Analysis",
        "structure": "installation",
        "operation": "installation/lift",
        "project": "Ballymore",
        "version": "1.0",
    },
    # Example for Ballymore specs with seabed data present. If a future spec omits the whole seabed
    # block, omit seabed stiffness and record seabed_source="absent" in provenance.
    "environment": {
        "water": {"depth": 1400, "density": 1.025},
        "seabed": {"type": "flat", "stiffness": {"normal": 1000, "shear": 0}},
        "waves": {"trains": [{"height": 0, "period": 8, "direction": 0}]},
        "current": {"speed": 0, "profile": [[0, 1.0]]},
        "wind": {"speed": 0, "direction": 0, "reference_height": 10},
    },
    "simulation": {"stages": [10, 300], "time_step": 0.1},
    "generic": {
        "line_types": [
            {
                "name": "10.75\"Jumper_wCoat",
                "category": "Homogeneous pipe",
                "outer_diameter": 0.27305,
                "inner_diameter": 0.182118,
                "properties": {
                    "CoatingThickness": 0.0762,
                    "CoatingMaterialDensity": 0.9787,  # te/m3; source kg/m3 / 1000
                },
            },
            {
                "name": "10.75\"Jumper_wCoat_wBuoy",
                "category": "Homogeneous pipe",
                "outer_diameter": 0.27305,
                "inner_diameter": 0.182118,
                "properties": {"CoatingThickness": "Insulation+Buoyancy"},
            },
            {
                "name": "10.75\"Jumper_wCoat_wStrake",
                "category": "Homogeneous pipe",
                "outer_diameter": 0.27305,
                "inner_diameter": 0.182118,
                "properties": {"CoatingThickness": "Insulation+Strakes"},
            },
            {
                "name": "OCS 200-V",
                "category": "General",
                "outer_diameter": 1.8,
                "inner_diameter": 1.5,
                "properties": {"MassPerUnitLength": 0},
            }
        ],
        "lines": [
            {
                "name": "JumperLine",
                "properties": {
                    "LineType, Length, TargetSegmentLength, PreBendCurvaturex, PreBendCurvaturey": [
                        ["10.75\"Jumper_wCoat", 8.5344, 0.5, 0, 0],
                        ["10.75\"Jumper_wCoat", 1.9949, 0.2, 0, -0.787401575],
                    ]
                },
            }
        ],
        "variable_data_sources": [
            {
                "name": "Insulation+Buoyancy",
                "data_type": "Coatingorlining",
                "properties": {
                    "LayerThickness, LayerMaterialDensity": [
                        [0.0762, 0.9787],
                        [0.557425, 0.6943],
                    ]
                },
            },
            {
                "name": "Insulation+Strakes",
                "data_type": "Coatingorlining",
                "properties": {
                    "LayerThickness, LayerMaterialDensity": [
                        [0.0762, 0.9787],
                        [0.005, 1.128],
                    ]
                },
            }
        ],
    },
}
```

The generated `includes/20_generic_objects.yml` must contain `VariableData` grouped by
`Coatingorlining`, and the line types must reference those variable-data names through
`CoatingThickness`. The review/implementation should use
`docs/domains/orcaflex/library/templates/jumper_rigid_subsea/spec.yml:75-122` and `:443-460` as the
schema-compatible example: homogeneous pipe densities in OrcaFlex line type/coating fields are
`te/m3`, while the Ballymore source insulation density is `kg/m3` and must be divided by 1000.
Tests must inspect both shapes:
- pre-generator `ProjectInputSpec` input: `data["generic"]["lines"][0]["properties"][compact_key]`.
  The table lives under `properties` because `GenericObject.properties` is the pass-through input
  field for non-typed OrcaFlex keys.
- generated OrcaFlex YAML: `includes/20_generic_objects.yml` section `Lines[0][compact_key]`.
  The generic builder flattens `properties` into the OrcaFlex object, so the same compact key appears
  directly under `Lines[0]` after generation.

`build_conversion_provenance()` must return a plain dict with concrete keys used by tests and
reporting:

```python
{
    "source_spec": "docs/domains/orcaflex/subsea/jumper/installation/ballymore_mf_plet/spec.yml",
    "config_name": "ballymore_mf_plet",
    "section_source": "stage_1_load_spec + stage_2_calculate -> orcaflex_sections",
    "section_count": 27,
    "baseline_geometry": False,
    "assumptions": {
        "target_segment_length_rule": "bend=0.2m, connector=0.4m, buoy_or_strake=1.0m, short_straight=0.5m",
        "prebend_sign_source": "jumper_rigid_subsea template pattern",
        "crane_configuration_use": "recorded in provenance only; not used by generic line model",
    },
    "source_values": {
        "pipe.outer_diameter_m": 0.27305,
        "pipe.wall_thickness_m": 0.045466,
        "pipe.insulation.density_kg_m3": 978.7,
        "crane_configuration.SZ.radius_m": 18.0,
    },
}
```

For `ballymore_plet_plem`, `baseline_geometry` must be derived from source text/comments rather than
hardcoded by spec name. The converter should treat the file as baseline geometry only while the
source spec still contains the explicit notes `Segment lengths need verification` and/or `Using
MF-PLET baseline until workbook is converted`; tests should fail if those source notes disappear but
the converter still reports `baseline_geometry=True`.

Required source fields used for physical output (`pipe.outer_diameter`, `pipe.wall_thickness`,
`pipe.insulation.thickness`, `pipe.insulation.density`, `pipe.bend_radius`,
`environment.water.depth`, `environment.water.density`, and `environment.seabed.stiffness.normal`
when a `seabed` block is present) must raise `ValueError` with the missing field path when absent.
If the whole `seabed` block is absent, omit seabed stiffness and record `seabed_source="absent"` in
provenance rather than inventing physical stiffness. Missing seabed shear defaults to 0 only when
normal stiffness is present. Provenance-only optional fields, including alternate crane IDs or
optional strake/buoyancy bookkeeping values, must be represented as `None` plus an `assumptions`
entry rather than silently defaulted to a physical value.

Baseline-geometry status for PLET-PLEM is intentionally raw-text based because the current source
markers are YAML comments. `build_conversion_provenance()` must read the source file text as well as
the parsed YAML. Tests must copy the PLET-PLEM spec to `tmp_path`, remove the comment lines containing
`Segment lengths need verification` and `Using MF-PLET baseline until workbook is converted`, and
assert the converter no longer reports `baseline_geometry=True` unless a future parseable field is
added.

## Required Field Map

| Source field | Output field | Notes/tests |
|---|---|---|
| `pipe.outer_diameter` | `generic.line_types[*].outer_diameter` for the three jumper pipe variants | Source unit m; mutation test changes OD and asserts emitted `OD` changes |
| `pipe.wall_thickness` | `generic.line_types[*].inner_diameter = outer_diameter - 2 * wall_thickness` | Source unit m; mutation test changes wall thickness and asserts `ID` changes |
| `pipe.insulation.thickness` | coated line-type `CoatingThickness` and first `Insulation+*` variable-data layer thickness | Assert exact numeric payload |
| `pipe.insulation.density` | coated line-type `CoatingMaterialDensity` and first `Insulation+*` variable-data layer density in te/m3 | Source kg/m3; divide by 1000; template line-type and variable-data examples use te/m3 |
| `buoyancy_modules.od_drag_m` and `pipe.insulated_od` | second `Insulation+Buoyancy` layer thickness `(od_drag_m - insulated_od) / 2` | Raise if negative; mutation changes payload |
| `buoyancy_modules.dry_weight_lbs` / `wet_weight_lbs` | converter provenance metadata until a validated material-density formula is added | Convert to kg in provenance using 0.45359237; generated buoyancy density remains a template-derived assumption and must be labeled provisional until #604 licensed validation |
| `strake_modules.od_drag_m` and `pipe.insulated_od` | second `Insulation+Strakes` layer thickness `(od_drag_m - insulated_od) / 2` | Raise if negative; mutation changes generated `VariableData`; strake density remains the template value 1.128 te/m3 until a source density exists and this assumption is recorded |
| `strake_modules.length_m`, `base_dry_lbs`, `wt_lbs_per_joint`, `num_strakes` | converter provenance metadata | Source m/lb/count; pounds converted to kg in provenance using 0.45359237 |
| `pipe.bend_radius` | bend-row `PreBendCurvaturey = sign / bend_radius_m` | Mutation test changes bend radius and asserts curvature magnitudes change |
| `rigging` connector/clamp fields | converter provenance metadata only in this generic-line issue | Required provenance keys include masterlink WLL, connector weight/length, clamp weight, and clamp WLL where present |
| `crane_configuration` | converter provenance metadata only in this generic-line issue | Mutation test asserts changed crane radius/SWL is recorded, not silently ignored |
| `environment.water.depth` / `density` | `environment.water.depth` / `density` | Density remains te/m3 |
| `environment.seabed.stiffness.normal` / `shear` | `environment.seabed.stiffness.normal` / `shear` | Missing shear defaults to 0 and is asserted |
| `environment.metocean.wave.significant` / `period` / `direction` | `environment.waves.trains[0].height` / `period` / `direction` | MF-PLET maps source metocean |
| `environment.metocean.wind.design_speed` | `environment.wind.speed` | Direction defaults to 0 unless source adds one |
| missing `environment.metocean` | explicit calm defaults | PLET-PLEM defaults asserted exactly |

Quote-containing line-type names must be asserted after YAML generation by loading
`includes/20_generic_objects.yml` and verifying both `LineTypes[].Name` and every `Lines` table first
column preserve the exact strings, including `10.75"Jumper_wCoat`.

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Add | `src/digitalmodel/marine_ops/installation/jumper_to_modular_spec.py` | Conversion layer from domain spec/results to `ProjectInputSpec` data |
| Add | `tests/solvers/orcaflex/modular_generator/test_jumper_modular_spec.py` | Validation and generator regression tests |
| Modify optional | `src/digitalmodel/marine_ops/installation/__init__.py` | Export helper only if local package convention does this |
| Modify optional | `docs/domains/orcaflex/subsea/jumper/installation/README.md` | Field-map documentation if a README exists or is useful |

## TDD Test List

| Test | Expected RED | Expected GREEN |
|---|---|---|
| `test_build_modular_spec_validates_for_ballymore_specs` | import/function missing | `ProjectInputSpec(**data)` succeeds for both specs |
| `test_build_modular_spec_preserves_27_line_sections` | missing conversion | one generated line contains 27 section rows |
| `test_modular_generator_writes_master_and_includes_for_ballymore_specs` | generator cannot run | `master.yml`, `inputs/parameters.yml`, includes exist and parse |
| `test_line_type_references_resolve` | unresolved refs possible | every line-table line type is defined in `generic.line_types` |
| `test_section_rows_include_mesh_and_prebend_fields` | generated line can be straight/under-specified | section rows include target mesh length and bend curvature fields |
| `test_bend_rows_have_signed_prebend_curvature` | bend geometry can be lost | bend rows use nonzero `PreBendCurvaturey` from bend radius |
| `test_bend_radius_mutation_changes_prebend_curvature` | curvature can be hardcoded | changed source bend radius changes generated curvature magnitudes |
| `test_named_coating_variable_data_references_resolve` | buoy/strake coating refs may be missing | `Insulation+Buoyancy` and `Insulation+Strakes` resolve to `variable_data_sources` with `data_type: Coatingorlining` and `LayerThickness, LayerMaterialDensity` rows |
| `test_source_pipe_buoyancy_and_strake_sections_affect_output` | converter can ignore domain sections | mutations to `pipe`, `buoyancy_modules`, and `strake_modules` alter generated line types/variable data |
| `test_insulation_thickness_mutation_changes_all_coating_layers` | insulation thickness can be partially wired | changed source insulation thickness changes coated line type and first variable-data layer |
| `test_required_linetypes_include_connector_type` | connector rows can reference undefined type | `OCS 200-V` exists and connector rows reference a defined line type |
| `test_rigging_connector_and_crane_values_are_in_provenance` | non-jumper sections can be silently ignored | rigging, connector/clamp, and crane fields appear in provenance with units |
| `test_quoted_linetype_names_roundtrip_exactly` | quote-containing names can drift | generated YAML preserves exact line-type names and line references |
| `test_environment_defaults_are_explicit_for_missing_metocean` | implicit schema defaults unguarded | PLET-PLEM calm defaults are set and asserted |
| `test_ballymore_specs_are_distinct_or_baseline_geometry_is_explicitly_marked` | current ambiguity not asserted | differences are asserted, or baseline reuse is documented in metadata |

## Acceptance Criteria

- [ ] `build_modular_spec()` or equivalent returns dicts accepted by `ProjectInputSpec(**data)` for
  both Ballymore specs.
- [ ] `ModularModelGenerator.from_spec(spec).generate(out_dir)` writes `master.yml`,
  `inputs/parameters.yml`, and includes for both specs.
- [ ] Generated model includes the 27 jumper sections in OrcaFlex line-section form.
- [ ] Generated line rows include explicit target segment length and pre-bend curvature fields; bend
  rows are not silently emitted as straight spans.
- [ ] Tests assert distinct configuration behavior or explicitly document current PLET-PLEM baseline
  geometry reuse.
- [ ] Generated YAML parses with `yaml.safe_load()`.
- [ ] Generated `master.yml` includes all listed include files, and named `CoatingThickness` variable
  data references resolve with the correct `Coatingorlining` payload.
- [ ] Tests prove source `pipe`, `buoyancy_modules`, and `strake_modules` sections affect generated
  output.
- [ ] Tests prove conversion provenance reports source path, section count, baseline-geometry status,
  provisional mesh/prebend assumptions, crane/rigging/connector values, and source-unit conversions.
- [ ] If PLET-PLEM still uses MF-PLET baseline geometry, the generated metadata and tests explicitly
  mark that status; this is allowed because issue #602 acceptance criteria explicitly permit
  documenting baseline geometry reuse until workbook conversion lands.
- [ ] Missing metocean data is mapped to explicit, tested calm defaults.
- [ ] No Linux CI test imports or requires OrcFxAPI.

## Risks and Constraints

- #506 may later add `JumperInstallationSpec`; this issue should not block on that absent schema, but
  the converter should be easy to migrate once it exists.
- #478 is broader and may already be approved. If #478 executes first, this issue can be merged into
  that implementation slice, but the tests here should still be used as acceptance coverage.
- The old #478 plan mentions top-level `vessels` and `analysis`, but the current root schema rejects
  arbitrary model-type-free data. Keep top-level keys schema-compatible.
- The generic template includes connectors and attachments in addition to the main line. This issue's
  minimum acceptance is the 27-section jumper line plus valid generator output; licensed OrcaFlex
  fidelity validation remains #604.

## Verification Commands

```bash
PYTHONPATH=src uv run python -m pytest tests/solvers/orcaflex/modular_generator/test_jumper_modular_spec.py -q
PYTHONPATH=src uv run python - <<'PY'
from pathlib import Path
import shutil
import yaml
from digitalmodel.marine_ops.installation.jumper_to_modular_spec import build_modular_spec
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec
from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator

for spec_path in [
    Path('docs/domains/orcaflex/subsea/jumper/installation/ballymore_mf_plet/spec.yml'),
    Path('docs/domains/orcaflex/subsea/jumper/installation/ballymore_plet_plem/spec.yml'),
]:
    data = build_modular_spec(spec_path)
    spec = ProjectInputSpec(**data)
    out = Path('/tmp/ballymore-modular') / spec_path.parent.name
    if out.exists():
        shutil.rmtree(out)
    ModularModelGenerator.from_spec(spec).generate(out)
    assert (out / 'master.yml').exists()
    assert (out / 'inputs' / 'parameters.yml').exists()
    assert yaml.safe_load((out / 'master.yml').read_text())
    objects = yaml.safe_load((out / 'includes' / '20_generic_objects.yml').read_text())
    assert len(objects['Lines'][0]['LineType, Length, TargetSegmentLength, PreBendCurvaturex, PreBendCurvaturey']) == 27
    assert 'VariableData' in objects
    print(spec_path, 'OK')
PY
```

## Adversarial Review Summary

| Provider | Verdict | Artifact | Notes |
|---|---|---|---|
| Codex | MINOR | `scripts/review/results/2026-05-15-plan-602-codex.md` | Final remaining minor notes patched after review |
| Gemini | UNAVAILABLE | `scripts/review/results/2026-05-15-plan-602-gemini.md` | Gemini quota exhausted; stderr saved in `.err` artifact |

**Overall result:** PLAN-REVIEW - no MAJOR blockers remain in latest available review evidence;
Gemini was unavailable due quota. Await user approval before implementation.
