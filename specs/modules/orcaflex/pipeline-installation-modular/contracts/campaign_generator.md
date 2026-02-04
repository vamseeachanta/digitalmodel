# Contract: CampaignGenerator

> API contract for the campaign generation engine
> Rev: 2 (post iteration-2 review — streaming API, --force/--resume, max_runs, model_copy)

## Module

`digitalmodel.solvers.orcaflex.modular_generator.campaign`

## Class: `CampaignGenerator`

### Constructor

```python
CampaignGenerator(campaign_file: Path)
```

- Loads YAML from `campaign_file`
- Validates against `CampaignSpec` schema
- Raises `ValidationError` if invalid

### Methods

#### `generate(output_dir: Path, force: bool = False, resume: bool = False) -> CampaignResult`

Streams through campaign matrix and generates OrcaFlex model files one at a time.

**Arguments:**
- `output_dir`: Root directory for generated files
- `force`: If True, overwrites existing run directories. Default: skip with warning.
- `resume`: If True, skips directories that already contain `master.yml`. Useful for resuming interrupted generation.

**Returns:** `CampaignResult` with aggregated results.

**Behavior:**
1. Calls `campaign_spec.generate_run_specs()` → iterator of `(name, spec)` tuples
2. For each `(name, spec)` yielded (streaming — one at a time):
   a. Spec already has overrides applied (`model_copy(deep=True)` + `_apply_overrides()`)
   b. Formats output directory name from `output_naming` template
   c. If directory exists:
      - `force=True` → overwrite
      - `resume=True` and `master.yml` exists → skip
      - Otherwise → log warning, skip
   d. Creates subdirectory under `output_dir`
   e. Delegates to `ModularModelGenerator.from_spec(spec).generate_with_overrides()`
   f. Applies section overrides and variable substitution if campaign has `sections`
3. Respects `max_runs` limit (stops after N runs if set)
4. Returns aggregate `CampaignResult`

**Logging:** Uses Python `logging` module (not `print()`). Supports `--verbose`/`--quiet` levels.

**Error handling:**
- Invalid parameter override → raises `ValueError` with parameter name and value
- File write failure → logs error, continues to next combination, records in `errors`

#### `preview() -> list[dict[str, float | str]]`

Dry-run: returns the list of parameter combinations without generating files. Delegates to `CampaignMatrix.combinations()`.

Return dict keys:
- `"water_depth"`: float (always present)
- `"route_length"`: float (if route_lengths specified)
- `"tension"`: float (if tensions specified)
- `"environment"`: str (name, if environments specified)
- `"soil"`: str (name, if soils specified)

#### `validate() -> list[str]`

Validates campaign spec consistency without generating. Returns list of warnings.

Checks:
- All water depths > 0
- Route lengths > 0 (if specified)
- Tensions > 0 (if specified, and S-lay model)
- `tensions` only specified for S-lay models (cross-validated)
- No duplicate combination names after template formatting
- Environment and soil variations have unique names
- `output_naming` template covers all varying parameters

## Class: `VariableResolver`

Defined in `sections.py` — see `section_composition.md` contract.

## Dataclass: `CampaignResult`

```python
@dataclass
class CampaignResult:
    run_count: int                  # Number of runs generated
    skipped_count: int              # Number of runs skipped (existing/resume)
    run_dirs: list[Path]            # Paths to each generated run directory
    errors: list[str]               # Non-fatal errors encountered
    matrix_summary: dict[str, list] # Parameter name → values used
```

## CLI Subcommand

Campaign is a subcommand of the existing `cli.py`, not a separate entry point.

```bash
# Preview matrix
uv run python -m digitalmodel.solvers.orcaflex.modular_generator.cli \
    campaign --preview campaign_30in.yml

# Generate all runs
uv run python -m digitalmodel.solvers.orcaflex.modular_generator.cli \
    campaign --output output/30in_campaign/ campaign_30in.yml

# Force overwrite
uv run python -m digitalmodel.solvers.orcaflex.modular_generator.cli \
    campaign --output output/ --force campaign_30in.yml

# Resume interrupted generation
uv run python -m digitalmodel.solvers.orcaflex.modular_generator.cli \
    campaign --output output/ --resume campaign_30in.yml

# Verbose logging
uv run python -m digitalmodel.solvers.orcaflex.modular_generator.cli \
    campaign --output output/ --verbose campaign_30in.yml
```

## Parameter Override Mapping

| Campaign Parameter | Target Field on `ProjectInputSpec` | Notes |
|--------------------|-------------------------------------|-------|
| `water_depth` | `environment.water.depth` | Current profile depths scaled proportionally |
| `route_length` | Last `pipeline.segments[-1].length` adjusted | Delta applied to last segment only |
| `tension` | `equipment.tensioner.tension_value` | S-lay only; cross-validated |
| `environment` (name) | `environment.waves`, `.current`, `.wind` | All three replaced from `EnvironmentVariation` |
| `soil` (name) | `environment.seabed.stiffness`, `.friction_coefficient`, optionally `.slope` | From `SoilVariation` |
